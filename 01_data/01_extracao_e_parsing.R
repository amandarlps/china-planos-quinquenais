# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 01_extracao_e_parsing.R
# FUNÇÃO: Processamento de PDF, Parser Hierárquico Universal e Construção do Corpus
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(pdftools)
  library(stringr)
  library(dplyr)
  library(readr)
  library(quanteda)
})

message("--> [SCRIPT 01] Executando Extração Avançada e Parsing Hierárquico das Seções...")

NUMS_EXTENSO <- "One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Eleven|Twelve|Thirteen|Fourteen|Fifteen|Sixteen|Seventeen|Eighteen|Nineteen|Twenty"

# 1. Extração Preservando Páginas
extrair_pdf_estruturado <- function(caminho_pdf, plano_id) {
  if (!file.exists(caminho_pdf)) {
    stop(paste("Arquivo não encontrado:", caminho_pdf))
  }
  
  message(paste("--> Lendo e extraindo páginas de:", plano_id))
  paginas_raw <- pdf_text(caminho_pdf)
  
  tibble(
    plano = plano_id,
    pagina = seq_along(paginas_raw),
    texto_bruto = paginas_raw
  )
}

# 2. Filtro Anti-Sumário (TOC) e Limpeza Textual
limpar_e_filtrar_frontmatter <- function(df_paginas, padroes_cabecalho = NULL) {
  regex_marcador_sumario <- sprintf("(?:CHAPTER|PART|ARTICLE)\\s+(?:[0-9IVXLCDM]+|[0-9]+|%s)", NUMS_EXTENSO)
  
  df_paginas %>%
    mutate(
      contagem_titulos = str_count(texto_bruto, regex(regex_marcador_sumario, ignore_case = TRUE))
    ) %>%
    filter(contagem_titulos < 3) %>%
    select(-contagem_titulos) %>%
    mutate(
      texto_limpo = texto_bruto,
      texto_limpo = str_replace_all(texto_limpo, "\r\n", "\n"),
      texto_limpo = str_replace_all(texto_limpo, "([a-zA-Z])-\\s\n\\s([a-zA-Z])", "\\1\\2"),
      texto_limpo = str_replace_all(texto_limpo, "\n\\s*\\d+\\s*\n", "\n"),
      texto_limpo = str_replace_all(texto_limpo, "\n\\s*Page\\s+\\d+\\s*\n", "\n")
    ) %>%
    {
      df_temp <- .
      if (!is.null(padroes_cabecalho)) {
        for (p_pat in padroes_cabecalho) {
          df_temp$texto_limpo <- str_replace_all(df_temp$texto_limpo, regex(p_pat, ignore_case = TRUE), " ")
        }
      }
      df_temp
    } %>%
    mutate(
      texto_limpo = str_replace_all(texto_limpo, "[\t ]+", " "),
      texto_limpo = str_replace_all(texto_limpo, "\n{3,}", "\n\n"),
      texto_limpo = str_trim(texto_limpo)
    )
}

# 3. Parser Hierárquico com Herança de Páginas
segmentar_por_estrutura <- function(df_limpo, min_palavras = 50) {
  texto_consolidado <- df_limpo %>%
    mutate(texto_pag = paste0("\n[PAGE_", pagina, "]\n", texto_limpo)) %>%
    pull(texto_pag) %>%
    paste(collapse = "\n")
  
  padrao_secao <- sprintf("(?i)(?=\n\\s*(?:CHAPTER|PART|ARTICLE)\\s+(?:[0-9IVXLCDM]+|[0-9]+|%s)\\b)", NUMS_EXTENSO)
  blocos <- str_split(texto_consolidado, padrao_secao)[[1]]
  
  registros <- list()
  cap_num <- 0
  ultima_pagina_vista <- 1
  
  for (i in seq_along(blocos)) {
    bloco_txt <- str_trim(blocos[i])
    
    # Extração de páginas usando Raw String r"(...)" para evitar erros de barra invertida
    pags_no_bloco <- as.numeric(str_extract_all(bloco_txt, r"(?<=\[PAGE_)\d+(?=\])")[[1]])
    
    if (length(pags_no_bloco) > 0) {
      pag_ini_bloco <- pags_no_bloco[1]
      pag_fim_bloco <- pags_no_bloco[length(pags_no_bloco)]
      ultima_pagina_vista <- pag_fim_bloco
    } else {
      pag_ini_bloco <- ultima_pagina_vista
      pag_fim_bloco <- ultima_pagina_vista
    }
    
    # Remoção das tags de página
    bloco_sem_tags <- str_replace_all(bloco_txt, r"(\[PAGE_\d+\])", " ")
    bloco_limpo_espacos <- str_replace_all(bloco_sem_tags, "\\s+", " ") %>% str_trim()
    palavras_bloco <- str_count(bloco_limpo_espacos, "\\w+")
    
    if (palavras_bloco < min_palavras) next
    
    linhas <- str_split(str_trim(bloco_sem_tags), "\n")[[1]]
    header_linha <- str_trim(linhas[1])
    cap_num <- cap_num + 1
    
    registros[[length(registros) + 1]] <- tibble(
      capitulo_index = cap_num,
      capitulo_header = str_sub(header_linha, 1, 140),
      pagina_inicio = pag_ini_bloco,
      pagina_fim = pag_fim_bloco,
      texto_secao = bloco_limpo_espacos,
      n_palavras = palavras_bloco
    )
  }
  bind_rows(registros)
}

# 4. Execução do Pipeline nos Arquivos Mapeados
arquivos_pq <- list(
  PQ13 = list(nomes_possiveis = c("13PQ_en.pdf"), periodo = "2016-2020", tipo = "Texto Integral"),
  PQ14 = list(nomes_possiveis = c("14PQ_en.pdf"), periodo = "2021-2025", tipo = "Texto Integral"),
  PQ15 = list(nomes_possiveis = c("15PQ_en.pdf"), periodo = "2026-2030", tipo = "Outline/Proposta")
)

padroes_cabecalho_pq15 <- c("Outline of the 15th Five-Year Plan", "Xinhua News Agency")
base_processada_lista <- list()

for (pq_key in names(arquivos_pq)) {
  info <- arquivos_pq[[pq_key]]
  caminho_encontrado <- NULL
  
  for (nome in info$nomes_possiveis) {
    p_sub <- file.path("01_data/01_raw_pdfs", nome)
    p_root <- nome
    if (file.exists(p_sub)) { caminho_encontrado <- p_sub; break }
    if (file.exists(p_root)) { caminho_encontrado <- p_root; break }
  }
  
  if (!is.null(caminho_encontrado)) {
    message(paste0("--> Processando [", pq_key, "] a partir de: ", caminho_encontrado))
    raw_df <- extrair_pdf_estruturado(caminho_encontrado, pq_key)
    write_csv(raw_df, paste0("01_data/02_intermediate/", pq_key, "_paginas_brutas.csv"))
    
    pats <- if (pq_key == "PQ15") padroes_cabecalho_pq15 else NULL
    clean_df <- limpar_e_filtrar_frontmatter(raw_df, padroes_cabecalho = pats)
    
    parsed_df <- segmentar_por_estrutura(clean_df, min_palavras = 50) %>%
      mutate(plano = pq_key, periodo = info$periodo, tipo_doc = info$tipo)
    
    base_processada_lista[[pq_key]] <- parsed_df
  } else {
    warning(paste("[ATENÇÃO] PDF não localizado para:", pq_key))
  }
}

if (length(base_processada_lista) == 0) {
  stop("\n[ERRO CRÍTICO]: Nenhum PDF foi localizado em '01_data/01_raw_pdfs/'.")
}

tabela_master_secoes <- bind_rows(base_processada_lista) %>%
  mutate(doc_id = sprintf("%s_Cap_%02d", plano, capitulo_index)) %>%
  select(doc_id, plano, periodo, tipo_doc, capitulo_index, capitulo_header, pagina_inicio, pagina_fim, n_palavras, texto_secao)

write_csv(tabela_master_secoes, "01_data/03_clean_csv/planos_quinquenais_secoes_limpas.csv")

# 5. Montagem do Corpus no Quanteda com Metadados
corpus_pq_estruturado <- corpus(
  tabela_master_secoes,
  docid_field = "doc_id",
  text_field = "texto_secao"
)

docvars(corpus_pq_estruturado, "plano")               <- factor(tabela_master_secoes$plano, levels = c("PQ13", "PQ14", "PQ15"))
docvars(corpus_pq_estruturado, "periodo")             <- factor(tabela_master_secoes$periodo)
docvars(corpus_pq_estruturado, "capitulo_num")        <- as.integer(tabela_master_secoes$capitulo_index)
docvars(corpus_pq_estruturado, "capitulo_nome")       <- tabela_master_secoes$capitulo_header
docvars(corpus_pq_estruturado, "pagina_inicio")       <- as.integer(tabela_master_secoes$pagina_inicio)
docvars(corpus_pq_estruturado, "pagina_fim")          <- as.integer(tabela_master_secoes$pagina_fim)
docvars(corpus_pq_estruturado, "data_processamento")  <- as.character(Sys.Date())

saveRDS(corpus_pq_estruturado, "01_data/04_processed_corpus/corpus_pq_estruturado.rds")
message("--> [SCRIPT 01] Corpus salvo com sucesso em '01_data/04_processed_corpus/corpus_pq_estruturado.rds'!")
