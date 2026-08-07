# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 05_matriz_cpo_exportacao.R
# FUNÇÃO: Consolidação da Matriz de CPOs, Tabelas de Síntese e Visualizações
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(ggrepel)
})

message("--> [SCRIPT 05] Consolidação da Matriz de CPOs e Relatórios de Process Tracing...")

dir_figuras   <- "03_outputs/graficos"
dir_relatorios <- "03_outputs/relatorios"
dir_apendice   <- "04_apendice"

purrr::walk(c(dir_figuras, dir_relatorios, dir_apendice), function(d) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
})

caminho_kwic        <- file.path(dir_apendice, "kwic_candidatos_cpo.csv")
caminho_corpus      <- "01_data/04_processed_corpus/corpus_pq_estruturado.rds"
caminho_resumo_dict <- "01_data/03_clean_csv/dicionario_resumo_por_plano.csv"

if (!file.exists(caminho_kwic)) {
  stop("[ERRO]: O arquivo 'kwic_candidatos_cpo.csv' não foi localizado em '04_apendice/'. Execute o Script 04 primeiro.")
}

kwic_cpos   <- read_csv(caminho_kwic, show_col_types = FALSE)
corpus_pq   <- if (file.exists(caminho_corpus)) readRDS(caminho_corpus) else NULL
resumo_dict <- if (file.exists(caminho_resumo_dict)) read_csv(caminho_resumo_dict, show_col_types = FALSE) else NULL

if (!"periodo" %in% names(kwic_cpos)) {
  kwic_cpos <- kwic_cpos %>%
    mutate(periodo = case_when(
      str_detect(plano, "PQ13") ~ "2016-2020",
      str_detect(plano, "PQ14") ~ "2021-2025",
      str_detect(plano, "PQ15") ~ "2026-2030",
      TRUE ~ "Período Não Identificado"
    ))
}

if (!"doc_id" %in% names(kwic_cpos)) kwic_cpos$doc_id <- paste0(kwic_cpos$plano, "_Doc")
if (!"capitulo_nome" %in% names(kwic_cpos)) kwic_cpos$capitulo_nome <- "Seção sem Nome"
if (!"pagina_inicio" %in% names(kwic_cpos)) kwic_cpos$pagina_inicio <- 1
if (!"pagina_fim" %in% names(kwic_cpos)) kwic_cpos$pagina_fim <- 1

# Matriz Formal de CPOs
matriz_cpo <- kwic_cpos %>%
  mutate(
    evidencia_texto = paste0(
      coalesce(contexto_anterior, ""), " [",
      coalesce(termo_busca, ""), "] ",
      coalesce(contexto_posterior, "")
    ),
    localizacao_precisa = sprintf("%s, %s (pp. %s-%s)", plano, capitulo_nome, as.character(pagina_inicio), as.character(pagina_fim)),
    mecanismo_hipotetico = case_when(
      str_detect(termo_busca, "(?i)security|defense|risk|military|sovereign|coordinate_development|cybersecurity|emergency") ~ "M1: Securitização Holística e Gestão de Riscos",
      str_detect(termo_busca, "(?i)development|technology|science|innovation|digital|quality|productive|research|resilience|tech") ~ "M2: Autonomia Tecno-Relacional e Modernização",
      str_detect(termo_busca, "(?i)global|belt|road|community|shared_future|gsi|gdi|gci|dual_circulation|multilateral|opening") ~ "M3: Governança Global e Projeção Normativa",
      str_detect(termo_busca, "(?i)party|leadership|centralized|xi_jinping|rigorous|self_governance") ~ "M4: Liderança Estratégica e Capacidade Central",
      str_detect(termo_busca, "(?i)ecological|carbon|green|civilization|beautiful_china|redline") ~ "M5: Civilização Ecológica e Transição Verde",
      TRUE ~ "M6: Teleologia Nacional e Fins Estruturais"
    )
  )

write_csv(matriz_cpo, file.path(dir_apendice, "matriz_cpo_process_tracing.csv"))

tabela_sintese_cpo <- matriz_cpo %>%
  group_by(plano, periodo, mecanismo_hipotetico) %>%
  summarise(frequencia_cpo = n(), .groups = "drop") %>%
  group_by(plano) %>%
  mutate(proporcao_pct = round((frequencia_cpo / sum(frequencia_cpo)) * 100, 2)) %>%
  ungroup()

write_csv(tabela_sintese_cpo, file.path(dir_apendice, "tabela_sintese_cpo_mecanismos.csv"))

tabela_densidade_capitulos <- matriz_cpo %>%
  group_by(plano, capitulo_nome, mecanismo_hipotetico) %>%
  summarise(total_cpos = n(), .groups = "drop") %>%
  arrange(plano, desc(total_cpos))

write_csv(tabela_densidade_capitulos, file.path(dir_apendice, "tabela_cpo_densidade_capitulos.csv"))

# GERAR RELATÓRIO DE SÍNTESE DO PIPELINE EM TEXTO
relatorio_txt <- file.path(dir_relatorios, "relatorio_sintese_pipeline.txt")
sink(relatorio_txt)
cat("===\n")
cat(" RELATÓRIO DE SÍNTESE DO PIPELINE DE PESQUISA (PROCESS TRACING)\n")
cat(sprintf(" Data de Geração: %s\n", Sys.time()))
cat("===\n\n")
cat(sprintf("Total de Evidências CPO Mapeadas: %d\n\n", nrow(matriz_cpo)))
cat("Distribuição de CPOs por Mecanismo Causal:\n")
print(as.data.frame(tabela_sintese_cpo))
sink()

# VISUALIZAÇÕES GRÁFICAS DE CPOs
paleta_mecanismos <- c(
  "M1: Securitização Holística e Gestão de Riscos"    = "#D95F02",
  "M2: Autonomia Tecno-Relacional e Modernização"     = "#1B9E77",
  "M3: Governança Global e Projeção Normativa"        = "#E7298A",
  "M4: Liderança Estratégica e Capacidade Central"    = "#7570B3",
  "M5: Civilização Ecológica e Transição Verde"       = "#66A61E",
  "M6: Teleologia Nacional e Fins Estruturais"         = "#B22222"
)

tema_cpo <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#1A1A1A"),
      plot.subtitle = element_text(size = 9.5, color = "#4A4A4A", margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5),
      axis.title = element_text(face = "bold", size = 9.5),
      axis.text = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

grafico_cpo_dist <- ggplot(tabela_sintese_cpo, aes(x = plano, y = frequencia_cpo, fill = mecanismo_hipotetico)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9, width = 0.75) +
  geom_text(
    aes(label = sprintf("%d\n(%.1f%%)", frequencia_cpo, proporcao_pct)),
    position = position_dodge(width = 0.75),
    vjust = -0.25, size = 2.7, fontface = "bold"
  ) +
  scale_fill_manual(values = paleta_mecanismos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Distribuição Temporal das Observações do Processo Causal (CPOs)",
    subtitle = "Evolução da densidade empírica dos 6 mecanismos estratégicos nos Planos Quinquenais",
    x = "Plano Quinquenal / Ciclo Estratégico",
    y = "Frequência Absoluta de CPOs Identificadas"
  ) +
  tema_cpo() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(dir_figuras, "figura4_distribuicao_mecanismos_cpo.png"), plot = grafico_cpo_dist, width = 10.5, height = 6.5, dpi = 300)

grafico_cpo_composicao_relativa <- ggplot(tabela_sintese_cpo, aes(x = plano, y = proporcao_pct, fill = mecanismo_hipotetico)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, alpha = 0.95) +
  geom_text(
    aes(label = ifelse(proporcao_pct >= 3, sprintf("%.1f%%", proporcao_pct), "")),
    position = position_fill(vjust = 0.5),
    size = 2.9, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = paleta_mecanismos) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Composição Relativa dos 6 Mecanismos Causais por Plano Quinquenal",
    subtitle = "Participação proporcional relativa (%) de cada mecanismo causal no total de CPOs",
    x = "Plano Quinquenal / Ciclo Estratégico",
    y = "Participação Proporcional Relativa (%)",
    fill = "Mecanismo Causal:"
  ) +
  tema_cpo() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(dir_figuras, "figura9_composicao_relativa_mecanismos_cpo.png"), plot = grafico_cpo_composicao_relativa, width = 10.5, height = 6.5, dpi = 300)
ggsave(file.path(dir_figuras, "figura9_composicao_relativa_mecanismos_cpo.pdf"), plot = grafico_cpo_composicao_relativa, width = 10.5, height = 6.5)

message("--> [SCRIPT 05] Processamento concluído. Matriz de CPOs, relatório e gráficos gerados com sucesso.")
