# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 03_visualizacao_graficos.R
# FUNÇÃO: Visualização Lexicográfica, Trajetórias Longitudinais, Matriz Dialética,
#         Análise de Keyness, MWEs, Redes de Co-ocorrência e Process Tracing (300 DPI)
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(quanteda)
  library(quanteda.textplots)
  library(quanteda.textstats)
  library(ggrepel)
  library(scales)
  library(tidytext)
})

message("--> [SCRIPT 03] Iniciando Módulo de Visualização e Process Tracing...")

dir_figuras <- "03_outputs/graficos"
dir_tabelas <- "03_outputs/tabelas"

if (!dir.exists(dir_figuras)) dir.create(dir_figuras, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_tabelas)) dir.create(dir_tabelas, recursive = TRUE, showWarnings = FALSE)

caminho_resumo  <- "01_data/03_clean_csv/dicionario_resumo_por_plano.csv"
caminho_comp    <- "01_data/03_clean_csv/dicionario_composicao_relativa_por_plano.csv"
caminho_dfm_raw <- "01_data/04_processed_corpus/dfm_bruta.rds"
caminho_toks    <- "01_data/04_processed_corpus/tokens_limpos.rds"

if (!file.exists(caminho_resumo) || !file.exists(caminho_dfm_raw)) {
  stop("[ERRO CRÍTICO]: Arquivos de dados corrigidos necessários não encontrados.")
}

df_resumo  <- read_csv(caminho_resumo, show_col_types = FALSE)
df_comp    <- if (file.exists(caminho_comp)) read_csv(caminho_comp, show_col_types = FALSE) else df_resumo
dfm_raw    <- readRDS(caminho_dfm_raw)
toks_clean <- if (file.exists(caminho_toks)) readRDS(caminho_toks) else NULL

if (!"plano" %in% names(docvars(dfm_raw))) {
  docvars(dfm_raw, "plano") <- stringr::str_extract(docnames(dfm_raw), "PQ\\d+")
}

tema_academico <- function() {
  theme_minimal(base_size = 11) +
    theme(
      text = element_text(family = "sans", color = "#222222"),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 6)),
      plot.subtitle = element_text(size = 9, color = "#555555", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#777777", margin = margin(t = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8.5),
      strip.text = element_text(face = "bold", size = 9.5)
    )
}

# -----------------------------------------------------------------------------
# 1. TABELA DE FREQUÊNCIA E PROCESS TRACING
# -----------------------------------------------------------------------------
vetor_planos <- docvars(dfm_raw, "plano")

freq_por_plano <- textstat_frequency(dfm_raw, groups = vetor_planos) %>%
  as_tibble() %>%
  rename(plano = group, termo = feature, frequencia_absoluta = frequency) %>%
  group_by(plano) %>%
  mutate(
    total_termos_plano = sum(frequencia_absoluta),
    proporcao_pct = (frequencia_absoluta / total_termos_plano) * 100,
    rank = row_number()
  ) %>%
  ungroup()

tabela_process_tracing <- freq_por_plano %>%
  filter(rank <= 25) %>%
  select(plano, rank, termo, frequencia_absoluta, proporcao_pct) %>%
  mutate(proporcao_pct = round(proporcao_pct, 3))

write_csv(tabela_process_tracing, file.path(dir_tabelas, "tabela_top_termos_process_tracing.csv"))

# -----------------------------------------------------------------------------
# 1B. GRÁFICOS DOS TOP TERMOS (INDIVIDUAIS E AGREGADO)
# -----------------------------------------------------------------------------
message("--> Gerando visualizações gráficas dos Top Termos por Plano Quinquenal...")
top_15_por_plano <- tabela_process_tracing %>%
  filter(rank <= 15)

list_planos <- unique(top_15_por_plano$plano)

for (p in list_planos) {
  df_sub <- top_15_por_plano %>% filter(plano == p)
  
  fig_indiv <- ggplot(df_sub, aes(x = reorder(termo, proporcao_pct), y = proporcao_pct)) +
    geom_col(fill = "#1F4E79", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = sprintf("%.2f%%", proporcao_pct)), hjust = -0.15, size = 3, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = function(x) paste0(x, "%")) +
    labs(
      title = paste("Top 15 Termos Mais Frequentes -", p),
      subtitle = "Proporção percentual das palavras-chave em relação ao total do plano",
      x = "Termo / Vocábulo",
      y = "Proporção no Corpus do Plano (%)"
    ) +
    tema_academico()
  
  ggsave(file.path(dir_figuras, paste0("figura_top_termos_", p, ".png")), plot = fig_indiv, width = 8, height = 5.5, dpi = 300)
  ggsave(file.path(dir_figuras, paste0("figura_top_termos_", p, ".pdf")), plot = fig_indiv, width = 8, height = 5.5)
}

fig_agregada <- ggplot(top_15_por_plano, aes(x = reorder_within(termo, proporcao_pct, plano), y = proporcao_pct, fill = plano)) +
  geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", proporcao_pct)), hjust = -0.1, size = 2.5, fontface = "bold") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ plano, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Comparativo dos Top 15 Termos Mais Frequentes nos Ciclos Estratégicos",
    subtitle = "Análise lexicográfica da densidade relativa por Plano Quinquenal (13º ao 15º PQ)",
    x = "Termo / Vocábulo",
    y = "Proporção de Ocorrência no Corpus (%)"
  ) +
  tema_academico() +
  theme(axis.text.y = element_text(size = 8))

ggsave(file.path(dir_figuras, "figura_top_termos_agregado.png"), plot = fig_agregada, width = 11.5, height = 6.5, dpi = 300)
ggsave(file.path(dir_figuras, "figura_top_termos_agregado.pdf"), plot = fig_agregada, width = 11.5, height = 6.5)

# -----------------------------------------------------------------------------
# 1C. RANKING DAS TOP 10 MWEs DOUTRINÁRIAS (POR PLANO E GERAL)
# -----------------------------------------------------------------------------
message("--> Extraindo e gerando visualizações do Ranking das Top 10 MWEs Doutrinárias...")

freq_mwes <- freq_por_plano %>%
  filter(str_detect(termo, "_")) %>%
  mutate(termo_formatado = str_replace_all(termo, "_", " "))

top10_mwes_por_plano <- freq_mwes %>%
  group_by(plano) %>%
  slice_max(order_by = frequencia_absoluta, n = 10, with_ties = FALSE) %>%
  mutate(rank_mwe = row_number()) %>%
  ungroup()

top10_mwes_geral <- textstat_frequency(dfm_raw) %>%
  as_tibble() %>%
  rename(termo = feature, frequencia_absoluta = frequency) %>%
  filter(str_detect(termo, "_")) %>%
  mutate(termo_formatado = str_replace_all(termo, "_", " ")) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

write_csv(top10_mwes_por_plano, file.path(dir_tabelas, "tabela_top10_mwes_por_plano.csv"))
write_csv(top10_mwes_geral, file.path(dir_tabelas, "tabela_top10_mwes_geral.csv"))

fig_top10_mwes <- ggplot(top10_mwes_por_plano, aes(x = reorder_within(termo_formatado, frequencia_absoluta, plano), y = frequencia_absoluta, fill = plano)) +
  geom_col(show.legend = FALSE, alpha = 0.9, width = 0.7) +
  geom_text(aes(label = frequencia_absoluta), hjust = -0.15, size = 2.8, fontface = "bold") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ plano, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Ranking das Top 10 MWEs Doutrinárias por Plano Quinquenal",
    subtitle = "Frequência absoluta das expressões multipalavras (MWEs) conceituais mais utilizadas",
    x = "Expressão Doutrinária (MWE)",
    y = "Frequência Absoluta"
  ) +
  tema_academico() +
  theme(axis.text.y = element_text(size = 8))

ggsave(file.path(dir_figuras, "figura_top10_mwes_doutrinarias.png"), plot = fig_top10_mwes, width = 12, height = 6.5, dpi = 300)
ggsave(file.path(dir_figuras, "figura_top10_mwes_doutrinarias.pdf"), plot = fig_top10_mwes, width = 12, height = 6.5)

# -----------------------------------------------------------------------------
# 2. NUVEM DE PALAVRAS COMPARATIVA
# -----------------------------------------------------------------------------
message("--> Gerando Nuvem de Palavras Comparativa...")
dfm_agrupada_pq <- dfm_group(dfm_raw, groups = vetor_planos)

png(file.path(dir_figuras, "nuvem_palavras_comparativa.png"), width = 3000, height = 2200, res = 300)
textplot_wordcloud(
  dfm_agrupada_pq,
  comparison = TRUE,
  max_words = 80,
  color = c("#1B9E77", "#D95F02", "#7570B3"),
  min_size = 0.6,
  max_size = 2.8,
  labelsize = 0.9
)
dev.off()

df_long <- df_resumo %>%
  pivot_longer(
    cols = c(
      Modernizacao_e_Inovacao_Tecnologica,
      Concepcao_Holistica_de_Seguranca_Nacional,
      Lideranca_Estrategica_e_Capacidade,
      Governanca_Global_e_Integracao,
      Civilizacao_Ecologica_e_Sustentabilidade,
      Fins_Estruturais_e_Objetivos_Nacionais
    ),
    names_to = "Dimensao_Raw",
    values_to = "Percentual"
  ) %>%
  mutate(
    Dimensao = case_when(
      Dimensao_Raw == "Modernizacao_e_Inovacao_Tecnologica"       ~ "Modernização & Inovação",
      Dimensao_Raw == "Concepcao_Holistica_de_Seguranca_Nacional" ~ "Segurança Holística",
      Dimensao_Raw == "Lideranca_Estrategica_e_Capacidade"        ~ "Liderança Estratégica",
      Dimensao_Raw == "Governanca_Global_e_Integracao"            ~ "Governança Global",
      Dimensao_Raw == "Civilizacao_Ecologica_e_Sustentabilidade" ~ "Civilização Ecológica",
      Dimensao_Raw == "Fins_Estruturais_e_Objetivos_Nacionais"   ~ "Fins Estruturais"
    )
  )

# -----------------------------------------------------------------------------
# 3. FIGURA 1: BARRAS AGRUPADAS (DENSIDADE REAIS NO CORPUS)
# -----------------------------------------------------------------------------
figura1 <- ggplot(df_long, aes(x = plano, y = Percentual, fill = Dimensao)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f%%", Percentual)), position = position_dodge(width = 0.75), vjust = -0.4, size = 2.6, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Evolução da Atenção Discursiva por Dimensão Estratégica",
    subtitle = "Densidade percentual das categorias em relação ao total de palavras de cada Plano Quinquenal",
    x = "Ciclo Estratégico (Plano Quinquenal)",
    y = "Densidade Discursiva no Texto (%)",
    fill = "Dimensão Teórica:"
  ) +
  tema_academico() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(dir_figuras, "figura1_evolucao_dimensoes_barras.png"), plot = figura1, width = 10, height = 6, dpi = 300)
ggsave(file.path(dir_figuras, "figura1_evolucao_dimensoes_barras.pdf"), plot = figura1, width = 10, height = 6)

# -----------------------------------------------------------------------------
# 4. FIGURA 2: LINHAS FACETADAS DE TENDÊNCIA INDIVIDUAL
# -----------------------------------------------------------------------------
figura2 <- ggplot(df_long, aes(x = plano, y = Percentual, group = Dimensao, color = Dimensao)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = sprintf("%.2f%%", Percentual)), size = 3, fontface = "bold", vjust = -0.5) +
  facet_wrap(~ Dimensao, scales = "free_y", ncol = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Trajetória Longitudinal das Categorias Teóricas",
    subtitle = "Variação individual da densidade textual por ciclo estratégico",
    x = "Plano Quinquenal",
    y = "Densidade (%)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#EFEFEF"),
    strip.text = element_text(face = "bold", size = 8.5)
  )

ggsave(file.path(dir_figuras, "figura2_linhas_tendencia_facetadas.png"), plot = figura2, width = 10, height = 6, dpi = 300)
ggsave(file.path(dir_figuras, "figura2_linhas_tendencia_facetadas.pdf"), plot = figura2, width = 10, height = 6)

# -----------------------------------------------------------------------------
# 5. FIGURA 3: MATRIZ DIALÉTICA (DESENVOLVIMENTO VS. SEGURANÇA)
# -----------------------------------------------------------------------------
df_dialetica <- df_resumo %>%
  select(plano, Modernizacao_e_Inovacao_Tecnologica, Concepcao_Holistica_de_Seguranca_Nacional) %>%
  rename(Desenvolvimento = Modernizacao_e_Inovacao_Tecnologica, Seguranca = Concepcao_Holistica_de_Seguranca_Nacional)

figura3 <- ggplot(df_dialetica, aes(x = Desenvolvimento, y = Seguranca, label = plano)) +
  geom_path(color = "#444444", linetype = "dashed", linewidth = 0.8, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_point(color = "#B22222", size = 5) +
  geom_text_repel(fontface = "bold", size = 4, box.padding = 0.6) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Matriz Dialética: Desenvolvimento vs. Segurança Holística",
    subtitle = "Trajetória de coexistência e co-evolução dos dois eixos estruturantes do Partido-Estado",
    x = "Densidade de Modernização & Inovação (%)",
    y = "Densidade de Segurança Holística (%)"
  ) +
  tema_academico()

ggsave(file.path(dir_figuras, "figura3_dialetica_desenvolvimento_seguranca.png"), plot = figura3, width = 8, height = 5.5, dpi = 300)
ggsave(file.path(dir_figuras, "figura3_dialetica_desenvolvimento_seguranca.pdf"), plot = figura3, width = 8, height = 5.5)

# -----------------------------------------------------------------------------
# 6. FIGURA 4: TRAJETÓRIA COM MARCOS ESTATAIS
# -----------------------------------------------------------------------------
df_trajetoria_marcos <- df_resumo %>%
  mutate(
    Ano_Inicio = case_when(plano == "PQ13" ~ 2016, plano == "PQ14" ~ 2021, plano == "PQ15" ~ 2026),
    Indice_Convergencia = Modernizacao_e_Inovacao_Tecnologica + Concepcao_Holistica_de_Seguranca_Nacional,
    Rotulo_Caixa = case_when(
      plano == "PQ13" ~ "13º PQ (2016)\nMade in China 2025 &\nSegurança Holística Inicial",
      plano == "PQ14" ~ "14º PQ (2021)\nCirculação Dual &\nAutonomia Tecnológica",
      plano == "PQ15" ~ "15º PQ (2026)\nNovas Forças Produtivas &\nResiliência Sistêmica"
    )
  )

p_marcos <- ggplot(df_trajetoria_marcos, aes(x = Ano_Inicio, y = Indice_Convergencia)) +
  geom_line(color = "#1F4E79", linewidth = 1.3) +
  geom_vline(aes(xintercept = Ano_Inicio), linetype = "dotted", color = "#666666", linewidth = 0.8) +
  geom_point(color = "#C00000", size = 4.5) +
  geom_label_repel(
    aes(label = Rotulo_Caixa),
    size = 3.2, fontface = "bold", fill = "white", color = "#111111",
    box.padding = 0.8, point.padding = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2016, 2021, 2026),
    labels = c("2016\n(13º PQ)", "2021\n(14º PQ)", "2026\n(15º PQ)"),
    limits = c(2014, 2028)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Trajetória da Racionalidade Estratégica e Planos Estatais",
    subtitle = "Evolução do Índice Agregado Tecno-Securitário (Inovação + Segurança)",
    x = "Ano de Lançamento do Ciclo Estratégico",
    y = "Densidade Tecno-Securitária Agregada (%)",
    caption = "Nota: Soma das frequências relativas das categorias 'Modernização & Inovação' e 'Segurança Holística'."
  ) +
  tema_academico()

ggsave(file.path(dir_figuras, "figura4_trajetoria_marcos_planos.png"), plot = p_marcos, width = 10, height = 6, dpi = 300)
ggsave(file.path(dir_figuras, "figura4_trajetoria_marcos_planos.pdf"), plot = p_marcos, width = 10, height = 6)

# -----------------------------------------------------------------------------
# 7. FIGURA 5: HEATMAP DE DENSIDADE CONCEITUAL
# -----------------------------------------------------------------------------
p_heatmap <- ggplot(df_long, aes(x = factor(plano), y = Dimensao, fill = Percentual)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f%%", Percentual)), color = "black", fontface = "bold", size = 3.8) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Densidade (%)") +
  labs(
    title = "Matriz de Intensidade Discursiva por Plano Quinquenal",
    subtitle = "Mapeamento em grade de calor do peso relativo das dimensões estratégicas",
    x = "Plano Quinquenal",
    y = "Dimensão Estratégica"
  ) +
  tema_academico() +
  theme(panel.grid = element_blank())

ggsave(file.path(dir_figuras, "figura5_heatmap_intensidade.png"), plot = p_heatmap, width = 9, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# 8. FIGURA 6: ANÁLISE DE KEYNESS LEXICAL (13º PQ vs. 15º PQ)
# -----------------------------------------------------------------------------
if ("PQ13" %in% docnames(dfm_agrupada_pq) && "PQ15" %in% docnames(dfm_agrupada_pq)) {
  dfm_sub <- dfm_subset(dfm_agrupada_pq, docnames(dfm_agrupada_pq) %in% c("PQ13", "PQ15"))
  keyness_obj <- textstat_keyness(dfm_sub, target = "PQ15")
  
  figura6 <- textplot_keyness(keyness_obj, n = 15, color = c("#0073C2FF", "#EFC000FF")) +
    labs(
      title = "Assinatura Lexical Diferencial: 15º PQ em Comparação ao 13º PQ",
      subtitle = "Termos estatisticamente mais sobressalentes no 15º PQ (Estatística G² de Keyness)"
    ) +
    tema_academico()
  
  ggsave(file.path(dir_figuras, "figura6_keyness_diferencial_pq15_vs_pq13.png"), plot = figura6, width = 10, height = 6, dpi = 300)
}

# -----------------------------------------------------------------------------
# 9. FIGURA 7: REDE DE CO-OCORRÊNCIA DE TERMOS (FCM)
# -----------------------------------------------------------------------------
if (!is.null(toks_clean)) {
  termos_chave_fcm <- c(
    "innovation", "technology", "security", "data", "ai", "resilience",
    "supply_chain", "autonomy", "national_security", "high-quality_development",
    "modernization", "governance", "sovereignty"
  )
  
  toks_fcm <- tokens_keep(toks_clean, pattern = phrase(termos_chave_fcm), window = 5)
  if (sum(ntoken(toks_fcm)) > 0) {
    fcm_matrix <- fcm(toks_fcm, context = "window", window = 5, count = "frequency")
    top_fcm <- fcm_select(fcm_matrix, pattern = termos_chave_fcm)
    
    png(file.path(dir_figuras, "figura7_rede_coocorrencia_tecno_seguranca.png"), width = 2600, height = 2000, res = 300)
    textplot_network(
      top_fcm,
      min_freq = 0.5,
      edge_color = "#1F4E79",
      edge_alpha = 0.6,
      edge_size = 2,
      vertex_color = "#C00000",
      vertex_labelsize = 4
    )
    dev.off()
  }
}

# -----------------------------------------------------------------------------
# 10. FIGURA 8: COMPOSIÇÃO RELATIVA DAS PRIORIDADES (100% STACKED BAR)
# -----------------------------------------------------------------------------
df_comp_long <- df_comp %>%
  pivot_longer(
    cols = c(
      Modernizacao_e_Inovacao_Tecnologica,
      Concepcao_Holistica_de_Seguranca_Nacional,
      Lideranca_Estrategica_e_Capacidade,
      Governanca_Global_e_Integracao,
      Civilizacao_Ecologica_e_Sustentabilidade,
      Fins_Estruturais_e_Objetivos_Nacionais
    ),
    names_to = "Dimensao_Raw",
    values_to = "Proporcao_Relativa"
  ) %>%
  mutate(
    Dimensao = case_when(
      Dimensao_Raw == "Modernizacao_e_Inovacao_Tecnologica"       ~ "Modernização & Inovação",
      Dimensao_Raw == "Concepcao_Holistica_de_Seguranca_Nacional" ~ "Segurança Holística",
      Dimensao_Raw == "Lideranca_Estrategica_e_Capacidade"        ~ "Liderança Estratégica",
      Dimensao_Raw == "Governanca_Global_e_Integracao"            ~ "Governança Global",
      Dimensao_Raw == "Civilizacao_Ecologica_e_Sustentabilidade" ~ "Civilização Ecológica",
      Dimensao_Raw == "Fins_Estruturais_e_Objetivos_Nacionais"   ~ "Fins Estruturais"
    )
  )

figura8 <- ggplot(df_comp_long, aes(x = plano, y = Proporcao_Relativa, fill = Dimensao)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, alpha = 0.95) +
  geom_text(aes(label = sprintf("%.1f%%", Proporcao_Relativa)), position = position_fill(vjust = 0.5), size = 2.8, color = "white", fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Redistribuição do 'Orçamento de Atenção' do Partido-Estado",
    subtitle = "Participação proporcional de cada dimensão no total de menções do dicionário estratégico",
    x = "Plano Quinquenal",
    y = "Participação Proporcional Relativa (%)",
    fill = "Dimensão Teórica:"
  ) +
  tema_academico() +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(dir_figuras, "figura8_composicao_relativa_prioridades.png"), plot = figura8, width = 10, height = 6.5, dpi = 300)
ggsave(file.path(dir_figuras, "figura8_composicao_relativa_prioridades.pdf"), plot = figura8, width = 10, height = 6.5)

message("\n=================================================================")
message("  SCRIPT 03 UNIFICADO, COMPLETO E EXECUTADO COM SUCESSO!        ")
message("=================================================================")
