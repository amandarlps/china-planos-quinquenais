# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 00_setup_e_diretorios.R
# FUNÇÃO: Instalação, Carregamento de Pacotes e Infraestrutura de Pastas
# =============================================================================

options(scipen = 999, stringsAsFactors = FALSE)

# Lista completa de dependências do projeto
pacotes <- c(
  "pdftools", "stringr", "dplyr", "tidyr", "purrr", "readr",
  "quanteda", "quanteda.textstats", "quanteda.textplots",
  "stopwords", "ggplot2", "scales", "gridExtra", "ggrepel", "tidytext"
)

# Carregamento silencioso das bibliotecas
suppressPackageStartupMessages(lapply(pacotes, library, character.only = TRUE))

# Infraestrutura Padronizada de Pastas do Projeto
diretorios_projeto <- c(
  "01_data/01_raw_pdfs",
  "01_data/02_intermediate",
  "01_data/03_clean_csv",
  "01_data/04_processed_corpus",
  "02_scripts",
  "03_outputs/graficos",
  "03_outputs/tabelas",
  "03_outputs/relatorios",
  "04_apendice"
)

purrr::walk(diretorios_projeto, ~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

message("--> [SCRIPT 00] Infraestrutura de diretórios e pacotes configurada com sucesso!")
