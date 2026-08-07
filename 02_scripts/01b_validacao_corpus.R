# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 01b_validacao_corpus.R
# FUNÇÃO: Auditoria Diagnóstica, Volumetria e Integridade Estrutural
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(quanteda)
})

message("--> [SCRIPT 01b] Executando Diagnóstico de Integridade do Corpus...")

caminho_csv <- "01_data/03_clean_csv/planos_quinquenais_secoes_limpas.csv"
caminho_corpus <- "01_data/04_processed_corpus/corpus_pq_estruturado.rds"

if (!file.exists(caminho_csv) || !file.exists(caminho_corpus)) {
  stop("[ERRO CRÍTICO]: Arquivos para validação não foram encontrados.")
}

tabela_master <- read_csv(caminho_csv, show_col_types = FALSE)
corpus_pq <- readRDS(caminho_corpus)

message("\n=== 1. DISTRIBUIÇÃO DE DOCUMENTOS E PALAVRAS POR PLANO ===")
print(
  tabela_master %>%
    group_by(plano, periodo) %>%
    summarise(
      total_secoes = n(),
      total_palavras = sum(n_palavras),
      mediana_palavras = median(n_palavras),
      .groups = "drop"
    )
)

message("\n=== 2. INSPEÇÃO DE SEÇÕES ANÔMALAS (< 100 PALAVRAS) ===")
secoes_curtas <- tabela_master %>% filter(n_palavras < 100)
if (nrow(secoes_curtas) > 0) {
  print(secoes_curtas %>% select(doc_id, plano, capitulo_header, n_palavras))
} else {
  message("[OK] Nenhuma seção anômala extremamente curta foi encontrada.")
}

message("\n=== 3. AUDITORIA DE TAGS RESIDUAIS DE PÁGINA ===")
tags_residuais <- sum(stringr::str_detect(tabela_master$texto_secao, r"(\[PAGE_\d+\])"))
if (tags_residuais == 0) {
  message("[OK] Nenhuma tag [PAGE_X] residual detectada.")
} else {
  warning(paste("[ATENÇÃO]", tags_residuais, "documentos contêm tags residuais!"))
}

message("\n=== 4. INTEGRIDADE DOS METADADOS (DOCVARS) ===")
df_vars <- docvars(corpus_pq)
nas_detectados <- sum(is.na(df_vars))
if (nas_detectados == 0) {
  message("[OK] Todos os metadados (docvars) do Quanteda estão preenchidos sem NAs.\n")
} else {
  warning(paste("[ATENÇÃO] Encontrados", nas_detectados, "valores NA nos metadados."))
}
