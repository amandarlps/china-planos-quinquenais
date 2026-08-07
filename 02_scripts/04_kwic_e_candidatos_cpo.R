# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 04_kwic_e_candidatos_cpo.R
# FUNÇÃO: Extração Contextual KWIC (Keywords in Context) para Process Tracing (CPOs)
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(quanteda)
})

message("--> [SCRIPT 04] Iniciando Extração Contextual KWIC para Process Tracing...")

caminho_corpus <- "01_data/04_processed_corpus/corpus_pq_estruturado.rds"
caminho_dict   <- "01_data/04_processed_corpus/dicionario_racionalidade.rds"

if (!file.exists(caminho_corpus) || !file.exists(caminho_dict)) {
  stop("[ERRO CRÍTICO]: Corpus ou Dicionário não localizados em '01_data/04_processed_corpus/'.")
}

corpus_pq <- readRDS(caminho_corpus)
dicionario_racionalidade <- readRDS(caminho_dict)

# Tokenização preservando estrutura para busca KWIC
toks_kwic <- tokens(
  corpus_pq,
  remove_punct = FALSE,
  remove_symbols = FALSE,
  remove_numbers = FALSE
)

# Unifica termos do dicionário para amostragem KWIC
termos_chave_busca <- unlist(dicionario_racionalidade, use.names = FALSE) %>%
  str_replace_all("_", " ") %>%
  unique()

message("--> Extraindo janelas de contexto (KWIC) com janela de 20 palavras...")

kwic_resultado <- kwic(
  toks_kwic,
  pattern = phrase(termos_chave_busca),
  window = 20,
  case_insensitive = TRUE
) %>%
  as_tibble()

# Reorganização e vínculo com metadados do Corpus
df_metadados <- docvars(corpus_pq) %>%
  as_tibble() %>%
  mutate(doc_id = docnames(corpus_pq))

kwic_processado <- kwic_resultado %>%
  rename(
    doc_id = docname,
    contexto_anterior = pre,
    termo_busca = keyword,
    contexto_posterior = post
  ) %>%
  left_join(df_metadados, by = "doc_id") %>%
  select(doc_id, plano, periodo, capitulo_nome, pagina_inicio, pagina_fim, contexto_anterior, termo_busca, contexto_posterior)

dir.create("04_apendice", showWarnings = FALSE, recursive = TRUE)
write_csv(kwic_processado, "04_apendice/kwic_candidatos_cpo.csv")

message(sprintf("--> [SCRIPT 04] SUCESSO: %d ocorrências contextuais de CPOs exportadas para '04_apendice/kwic_candidatos_cpo.csv'!", nrow(kwic_processado)))
