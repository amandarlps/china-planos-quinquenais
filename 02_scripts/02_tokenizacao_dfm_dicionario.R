# =============================================================================
# PROJETO: A Evolução da Racionalidade Estratégica do Partido-Estado Chinês
# SCRIPT: 02_tokenizacao_dfm_dicionario.R
# FUNÇÃO: Tokenização, MWEs, DFM e Cálculo Correto de Densidade do Dicionário
# =============================================================================

if (file.exists("00_setup_e_diretorios.R")) {
  source("00_setup_e_diretorios.R")
} else if (file.exists("02_scripts/00_setup_e_diretorios.R")) {
  source("02_scripts/00_setup_e_diretorios.R")
}

suppressPackageStartupMessages({
  library(quanteda)
  library(tidyverse)
})

message("--> [SCRIPT 02] Iniciando Processamento Lexicográfico e Dicionário Teórico...")

# 1. CARREGAMENTO DO CORPUS
caminho_corpus <- "01_data/04_processed_corpus/corpus_pq_estruturado.rds"
if (!file.exists(caminho_corpus)) {
  stop("[ERRO CRÍTICO]: O arquivo 'corpus_pq_estruturado.rds' não foi localizado.")
}
corpus_pq <- readRDS(caminho_corpus)

# 2. TOKENIZAÇÃO E MULTI-WORD EXPRESSIONS (MWEs)
toks_raw <- tokens(
  corpus_pq,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  split_hyphens = FALSE
) %>% tokens_tolower()

mwe_china_strategy <- c(
  # 1. Modernização e Inovação Tecnológica
  "high-quality development", "higher-quality development", "new quality productive forces",
  "innovation-driven development", "technological self-reliance and self-strengthening",
  "technological sovereignty", "technological autonomy", "science and technology",
  "st innovation", "s and t innovation", "modern industrial system", "manufacturing powerhouse",
  "digital economy", "ai plus", "digital-intelligent", "integrated circuits",
  "upgrading of the industrial chain", "total factor productivity", "supply-side structural reform",
  "real economy", "new development stage", "basic research", "core technologies in key fields",
  "industrial chain resilience", "smart manufacturing", "strategic emerging industries",
  "future industries", "data elements", "commercial spaceflight", "low-altitude economy",
  "quantum information", "biomanufacturing", "embodied ai", "frontier technologies",
  "digital transformation", "science and technology self-reliance",
  
  # 2. Concepção Holística de Segurança Nacional
  "comprehensive national security concept", "holistic view of state security",
  "overall national security concept", "overall national security", "political security",
  "regime security", "coordinate development and security", "security and development",
  "economic security", "food and energy security", "food security", "energy security",
  "civil-military synergy", "civil-military integration", "civil-military fusion",
  "global security initiative", "bottom-line thinking", "national security system",
  "national security governance", "security governance system", "strategic stability",
  "national defense and armed forces", "cybersecurity", "information security",
  "data security", "financial security", "biosafety", "biosecurity", "nuclear security",
  "outer space security", "deep sea security", "risk prevention and control",
  "risk defusion", "emergency management system", "armed forces modernization",
  
  # 3. Liderança Estratégica e Capacidade
  "party leadership", "ccp leadership", "communist party", "centralized and unified leadership",
  "political leadership efficiency", "xi jinping thought", "socialism with chinese characteristics for a new era",
  "two establishments", "two safeguards", "social stability", "party discipline", "party building",
  "governance capacity", "modernization of governance capacity", "state-owned enterprises",
  "central committee", "targeted poverty reduction", "full and rigorous party self-governance",
  "comprehensive strict party governance", "self-reform of the party", "political ecosystem",
  "intra-party democracy", "socialist rule of law", "law-based governance", "rule of law",
  "anti-corruption", "modernization of state governance",
  
  # 4. Governança Global e Integração
  "community with a shared future for mankind", "community of shared future", "common destiny",
  "global development initiative", "global civilization initiative", "belt and road initiative",
  "high-quality joint construction of the belt and road", "major-country diplomacy",
  "major power with chinese characteristics", "global south", "partnership network",
  "high-standard opening up", "institutional opening up", "dual circulation",
  "new development pattern", "great changes unseen in a century", "win-win cooperation",
  "peaceful coexistence", "peaceful development", "global governance system",
  "true multilateralism", "south-south cooperation", "free trade zones", "high-standard free trade",
  
  # 5. Civilização Ecológica e Sustentabilidade
  "ecological civilization", "green development", "carbon neutrality", "carbon peaking",
  "beautiful china", "lucid waters and lush mountains", "low-carbon transition",
  "clean energy", "non-fossil energy", "sustainable development", "harmony between man and nature",
  "harmony between humanity and nature", "ecological environment", "environmental protection",
  "environmental governance", "pollution control", "dual carbon goals", "green and low-carbon development",
  "green transformation", "ecological protection redline", "biodiversity conservation",
  "circular economy", "green finance", "ecological restoration",
  
  # 6. Fins Estruturais e Objetivos Nacionais
  "chinese path to modernization", "chinese style modernization", "chinese-style modernization",
  "socialist modernization", "national rejuvenation", "rejuvenation of the chinese nation",
  "great rejuvenation of the chinese nation", "chinese dream", "first centenary goal",
  "second centenary goal", "two centenaries", "great modern socialist country",
  "modern socialist country", "modern socialist powerhouse", "moderately prosperous society in all respects",
  "moderately prosperous society", "xiaokang society", "common prosperity",
  "whole-process people democracy", "whole-process peoples democracy",
  "standing tall among the nations of the world", "standing tall among the nations",
  "people-centered development", "people-centered philosophy", "material and cultural-ethical advancement",
  "cultural confidence", "two-step strategic plan", "modernization of a huge population"
)

toks_compounds <- tokens_compound(toks_raw, pattern = phrase(mwe_china_strategy), concatenator = "_")

# 3. FILTRAGEM DE STOPWORDS E RUÍDOS INSTITUCIONAIS
lista_stopwords_en <- if (requireNamespace("stopwords", quietly = TRUE)) {
  stopwords::stopwords("en")
} else {
  c("a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can", "did", "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have", "having", "he", "her", "here", "hers", "herself", "him", "himself", "his", "how", "i", "if", "in", "into", "is", "it", "its", "itself", "just", "me", "more", "most", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "should", "so", "some", "such", "than", "that", "the", "their", "theirs", "them", "themselves", "then", "there", "these", "they", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we", "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "you", "your", "yours")
}

stopwords_dominio <- c(
  lista_stopwords_en,
  "chapter", "part", "article", "section", "table", "box", "figure", "note",
  "th", "st", "nd", "rd", "china", "chinese", "plan", "year", "five", "five-year",
  "xinhua", "news", "agency", "cset", "translation", "editor", "translator",
  "source", "http", "view", "paragraph", "annex", "appendix", "index", "column",
  "updated", "relevant", "departments", "sectors", "etc", "various", "general"
)

toks_clean <- tokens_select(toks_compounds, pattern = stopwords_dominio, selection = "remove", padding = FALSE)

saveRDS(toks_clean, "01_data/04_processed_corpus/tokens_limpos.rds")

# 4. CONSTRUÇÃO DA DFM
dfm_raw <- dfm(toks_clean)
dfm_tfidf <- dfm_tfidf(dfm_raw)

saveRDS(dfm_raw, "01_data/04_processed_corpus/dfm_bruta.rds")
saveRDS(dfm_tfidf, "01_data/04_processed_corpus/dfm_tfidf.rds")

# 5. DICIONÁRIO TEÓRICO EXPANDIDO
dicionario_racionalidade <- dictionary(list(
  Modernizacao_e_Inovacao_Tecnologica = c(
    "high-quality_development", "higher-quality_development", "new_quality_productive_forces",
    "innovation-driven_development", "technological_self-reliance_and_self-strengthening",
    "technological_sovereignty", "technological_autonomy", "science_and_technology",
    "st_innovation", "s_and_t_innovation", "modern_industrial_system", "digital_economy",
    "total_factor_productivity", "real_economy", "manufacturing_powerhouse", "ai_plus",
    "digital-intelligent", "integrated_circuits", "semicond*", "quantum*", "new_development_stage",
    "upgrading_of_the_industrial_chain", "supply-side_structural_reform", "basic_research",
    "core_technologies_in_key_fields", "industrial_chain_resilience", "smart_manufacturing",
    "strategic_emerging_industries", "future_industries", "data_elements", "commercial_spaceflight",
    "low-altitude_economy", "quantum_information", "biomanufacturing", "embodied_ai",
    "frontier_technologies", "digital_transformation", "science_and_technology_self-reliance"
  ),
  Concepcao_Holistica_de_Seguranca_Nacional = c(
    "comprehensive_national_security_concept", "holistic_view_of_state_security",
    "overall_national_security_concept", "overall_national_security", "political_security",
    "regime_security", "national_security", "security_and_development",
    "coordinate_development_and_security", "national_security_system",
    "national_security_governance", "security_governance_system", "economic_security",
    "food_and_energy_security", "food_security", "energy_security", "civil-military_synergy",
    "civil-military_integration", "civil-military_fusion", "global_security_initiative",
    "gsi", "dual-use", "risk_prevention", "bottom-line_thinking",
    "national_defense_and_armed_forces", "strategic_stability", "cybersecurity",
    "information_security", "data_security", "financial_security", "biosafety",
    "biosecurity", "nuclear_security", "outer_space_security", "deep_sea_security",
    "risk_prevention_and_control", "risk_defusion", "emergency_management_system",
    "armed_forces_modernization"
  ),
  Lideranca_Estrategica_e_Capacidade = c(
    "party_leadership", "ccp_leadership", "communist_party", "centralized_and_unified_leadership",
    "political_leadership_efficiency", "xi_jinping_thought*",
    "socialism_with_chinese_characteristics_for_a_new_era", "two_establishments",
    "two_safeguards", "party_discipline", "party_building", "targeted_poverty_reduction",
    "state-owned_enterprises", "central_committee", "governance_capacity",
    "modernization_of_governance_capacity", "social_stability",
    "full_and_rigorous_party_self-governance", "comprehensive_strict_party_governance",
    "self-reform_of_the_party", "political_ecosystem", "intra-party_democracy",
    "socialist_rule_of_law", "law-based_governance", "rule_of_law", "anti-corruption",
    "modernization_of_state_governance"
  ),
  Governanca_Global_e_Integracao = c(
    "community_with_a_shared_future_for_mankind", "community_of_shared_future", "common_destiny",
    "global_development_initiative", "gdi", "global_civilization_initiative", "gci",
    "belt_and_road_initiative", "high-quality_joint_construction_of_the_belt_and_road",
    "major-country_diplomacy", "major_power_with_chinese_characteristics",
    "great_changes_unseen_in_a_century", "high-standard_opening_up", "institutional_opening_up",
    "dual_circulation", "new_development_pattern", "peaceful_development", "global_south",
    "partnership_network", "win-win_cooperation", "peaceful_coexistence", "global_governance",
    "global_governance_system", "multilateral*", "true_multilateralism", "south-south_cooperation",
    "free_trade_zones", "high-standard_free_trade"
  ),
  Civilizacao_Ecologica_e_Sustentabilidade = c(
    "ecological_civilization", "green_development", "carbon_neutrality", "carbon_peaking",
    "beautiful_china", "lucid_waters_and_lush_mountains", "low-carbon_transition",
    "clean_energy", "non-fossil_energy", "sustainable_development", "harmony_between_man_and_nature",
    "harmony_between_humanity_and_nature", "ecological_environment", "environmental_protection",
    "environmental_governance", "pollution_control", "pollution", "emission*", "decarbon*",
    "dual_carbon_goals", "green_and_low-carbon_development", "green_transformation",
    "ecological_protection_redline", "biodiversity_conservation", "circular_economy",
    "green_finance", "ecological_restoration"
  ),
  Fins_Estruturais_e_Objetivos_Nacionais = c(
    "national_rejuvenation", "rejuvenation_of_the_chinese_nation",
    "great_rejuvenation_of_the_chinese_nation", "chinese_dream", "chinese_path_to_modernization",
    "chinese_style_modernization", "chinese-style_modernization", "socialist_modernization",
    "first_centenary_goal", "second_centenary_goal", "two_centenaries",
    "great_modern_socialist_country", "modern_socialist_country", "modern_socialist_powerhouse",
    "moderately_prosperous_society", "moderately_prosperous_society_in_all_respects",
    "xiaokang_society", "common_prosperity", "whole-process_people_democracy",
    "whole-process_peoples_democracy", "standing_tall_among_the_nations",
    "standing_tall_among_the_nations_of_the_world", "people-centered_development",
    "people-centered_philosophy", "material_and_cultural-ethical_advancement",
    "cultural_confidence", "two-step_strategic_plan", "modernization_of_a_huge_population"
  )
))

saveRDS(dicionario_racionalidade, "01_data/04_processed_corpus/dicionario_racionalidade.rds")

# 6. EXTRAÇÃO EXPLÍCITA DO VETOR DO PLANO E AGRUPAMENTO SEGURO
if ("plano" %in% names(docvars(dfm_raw)) && length(docvars(dfm_raw, "plano")) == ndoc(dfm_raw)) {
  vetor_plano <- as.character(docvars(dfm_raw, "plano"))
} else {
  vetor_plano <- stringr::str_extract(docnames(dfm_raw), "PQ\\d+")
}

if (any(is.na(vetor_plano))) {
  vetor_plano[is.na(vetor_plano)] <- "PQ_Desconhecido"
}

# Aplicação do Dicionário
dfm_dict <- dfm_lookup(dfm_raw, dictionary = dicionario_racionalidade)

# Agrupamento explícito via vetor indexado
dfm_dict_plan <- dfm_group(dfm_dict, groups = vetor_plano)
dfm_raw_plan <- dfm_group(dfm_raw, groups = vetor_plano)

# Conversão segura independentemente da versão do quanteda
df_counts_plano <- convert(dfm_dict_plan, to = "data.frame", docid_field = "plano")
if (names(df_counts_plano)[1] != "plano") {
  names(df_counts_plano)[1] <- "plano"
}

# Total de termos reais por plano na DFM limpa
tokens_totais_plano <- tibble(
  plano = docnames(dfm_raw_plan),
  total_tokens = as.numeric(rowSums(dfm_raw_plan))
)

categorias_dict <- names(dicionario_racionalidade)

# Cálculo de Densidade Discursiva Real (% em relação ao total de palavras do Plano)
resumo_dicionario_plano <- df_counts_plano %>%
  left_join(tokens_totais_plano, by = "plano") %>%
  mutate(across(all_of(categorias_dict), ~ (.x / total_tokens) * 100)) %>%
  select(-total_tokens)

# Cálculo de Composição Relativa do Dicionário (% em relação ao total de hits do dicionário)
resumo_dicionario_composicao <- df_counts_plano %>%
  mutate(total_dict_hits = rowSums(pick(all_of(categorias_dict)))) %>%
  mutate(across(all_of(categorias_dict), ~ ifelse(total_dict_hits > 0, (.x / total_dict_hits) * 100, 0))) %>%
  select(-total_dict_hits)

# 7. EXPORTAÇÃO DOS DADOS CORRIGIDOS
dir.create("01_data/03_clean_csv", recursive = TRUE, showWarnings = FALSE)

df_export_dict <- convert(dfm_dict, to = "data.frame", docid_field = "doc_id")
write_csv(df_export_dict, "01_data/03_clean_csv/frequencias_dimensoes_dicionario.csv")
write_csv(resumo_dicionario_plano, "01_data/03_clean_csv/dicionario_resumo_por_plano.csv")
write_csv(resumo_dicionario_composicao, "01_data/03_clean_csv/dicionario_composicao_relativa_por_plano.csv")

message("\n=================================================================")
message(" SCRIPT 02 EXECUTADO E CORRIGIDO COM SUCESSO! ")
message("=================================================================")
print(resumo_dicionario_plano %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
