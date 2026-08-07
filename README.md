# A Evolução da Racionalidade Estratégica do Partido-Estado Chinês

> **Análise Lexicográfica, Trajetórias Longitudinais e Process Tracing nos Planos Quinquenais (13º ao 15º PQ)**

Este repositório contém o código-fonte, os dados e os pipelines de Processamento de Linguagem Natural (PLN/NLP) desenvolvidos para analisar a evolução dos discursos e diretrizes estratégicas dos Planos Quinquenais (13º, 14º e 15º PQ) da República Popular da China.

---

## Sobre o Trabalho

O objetivo central desta pesquisa é mapear quantitativa e qualitativamente a transição da racionalidade estratégica do Partido-Estado chinês, investigando a hipótese de reconfiguração de prioridades: a passagem de um foco prioritário no **desenvolvimento econômico e modernização tecnológica** para uma integração simbiótica com a **Segurança Holística Nacional** e a **resiliência sistêmica**.

### Fundamentação Teórica e Metodológica

* **Enquadramento Teórico:** Apoiando-se na teoria relacional de Qin Yaqing (2014; 2016), investiga-se como o Partido-Estado respondeu à rivalidade tecno-estratégica e às sanções externas segundo a lógica da *“mudança na continuidade”* — reconfigurando os meios operacionais (da integração em cadeias globais para a autossuficiência tecnológica) sem alterar os objetivos estruturais de fortalecimento estatal e rejuvenescimento nacional.
* **Metodologia Triangulada:**
1. Mineração computacional de texto em R (*Text-as-Data*).
2. Análise qualitativa de discursos estratégicos de Xi Jinping.
3. Rastreamento processual (*Process Tracing*) baseado em *Causal Process Observations* (CPOs).



---

## Metodologia e Implementação

O pipeline analítico do projeto está estruturado em quatro etapas principais:

1. **Pré-processamento e Construção do Corpus:**
* Tokenização e limpeza de ruídos textuais com o pacote `quanteda`.
* Identificação, extração e mapeamento de **Expressões Multipalavras (MWEs - *Multi-Word Expressions*)** conceituais (ex.: `national_security`, `high-quality_development`, `dual_circulation`).


2. **Análise Baseada em Dicionário Estratégico:**
* Categorização do corpus em **6 Dimensões Teóricas**:
1. Modernização e Inovação Tecnológica
2. Concepção Holística de Segurança Nacional
3. Liderança Estratégica e Capacidade
4. Governança Global e Integração
5. Civilização Ecológica e Sustentabilidade
6. Fins Estruturais e Objetivos Nacionais


* Cálculo da **Densidade Discursiva Relativa (%)** e distribuição proporcional do "Orçamento de Atenção" (*Attention Budget*) do Estado.


3. **Análises Avançadas de Texto:**
* **Process Tracing Lexicográfico:** Mapeamento dos termos mais frequentes por ciclo e variação de rankings.
* **Análise de Sobressalência (Keyness — $G^2$):** Identificação da assinatura lexical diferencial entre o 13º e o 15º PQ.
* **Matriz de Co-ocorrência (FCM):** Redes de co-ocorrência conceitual contextual para mapear o adensamento da relação tecno-securitária.
* **Matriz Dialética:** Análise bivariada da trajetória espacial e temporal entre Desenvolvimento e Segurança.


4. **Visualização de Dados e Exportação:**
* Automação de gráficos e mapas conceituais gerados via `ggplot2` e `quanteda.textplots` em alta resolução (300 DPI — PNG e PDF vetorial).



---

## 📂 Estrutura do Repositório

```text
├── 01_data/
│   └── 01_raw_docs/                          # Processamento de PDF, Parser Hierárquico Universal e Construção do Corpus
│   └── PQ13_limpo.txt/                       # 13º Plano Quinquenal após limpeza e validação
│   └── PQ14_limpo.txt/                       # 14º Plano Quinquenal após limpeza e validação
│   └── PQ15_limpo.txt/                       # 15º Plano Quinquenal após limpeza e validação
│
├── 02_scripts/
│   ├── 00_setup_e_diretorios.R                       # Configuração de ambiente, pacotes e pastas
│   ├── 01_extracao_e_parsing.R                        # Leitura e parsing inicial dos documentos brutos
│   ├── 01b_validacao_corpus.R                         # Testes de integridade e validação do corpus
│   ├── 02_tokenizacao_dfm_dicionario.R                # Tokenização, matriz DFM e aplicação do dicionário
│   ├── 03_visualizacao_graficos.R                      # Visualização Lexicográfica, geração de gráficos e figuras e análise de Keyness
│   ├── 04_kwic_e_candidatos_cpo.R                      # Análise KWIC e seleção de candidatos a CPOs
│   ├── 05_matriz_cpo_exportacao.R                      # Construção e exportação das matrizes de CPOs
│   └── artigo_planos_quinquenais_SCRIPTS_COMPLETOS.R    # Script unificado contendo o pipeline completo
│
├── 03_outputs/
│   ├── graficos/                    # Gráficos exportados (PNG 300 DPI / PDF)
│   └── tabelas/                     # Tabelas CSV do Process Tracing e MWEs
│
└── README.md                        # Documentação do projeto

---

## 🚀 Como Executar o Projeto

### Pré-requisitos

Certifique-se de ter o **R** (versão 4.0 ou superior) e o **RStudio** instalados.

### Instalação de Pacotes

Execute o comando abaixo no console do R para instalar as dependências necessárias:

```r
install.packages(c(
  "tidyverse", 
  "quanteda", 
  "quanteda.textplots", 
  "quanteda.textstats", 
  "ggrepel", 
  "scales", 
  "tidytext"
))

```

### Ordem de Execução dos Scripts

Para reproduzir integralmente os resultados e gráficos, execute os scripts em `02_scripts/` na seguinte ordem:

1. `00_setup_e_diretorios.R` – Prepara o ambiente e valida as pastas.
2. `01_processamento_corpus.R` – Processa o corpus e constrói as estruturas do `quanteda`.
3. `02_analise_dicionario.R` – Aplica o dicionário conceitual e exporta as métricas.
4. `03_visualizacao_graficos.R` – Gera a suíte completa de gráficos e tabelas de resultados.

---

## 📊 Principais Saídas e Produtos Gerados

* **`tabela_top_termos_process_tracing.csv`**: Relação dos 25 termos mais frequentes por ciclo para evidência de *process tracing*.
* **`tabela_top10_mwes_por_plano.csv`**: Ranking de conceitos compostos doutrinários (ex.: *high-quality development*).
* **`figura3_dialetica_desenvolvimento_seguranca.png`**: Trajetória vetorial da coexistência entre Desenvolvimento e Segurança.
* **`figura4_trajetoria_marcos_planos.png`**: Curva de evolução do Índice Agregado Tecno-Securitário.
* **`figura6_keyness_diferencial_pq15_vs_pq13.png`**: Termos com maior ganho de peso relativo no plano mais recente.
* **`figura8_composicao_relativa_prioridades.png`**: Distribuição 100% empilhada do "orçamento de atenção" do Estado.

---

## 📝 Autoria e Citação

* **Autor:** Amanda Ribeiro Lopes
* **Afiliação:** Universidade do Estado do Rio de Janeiro (UERJ)
* **Contato:** amanda.lopes@acad.ufsm.br
* **Programa:** Programa de Pós-Graduação em Relações Internacionais

### Declaração do Uso de Inteligência Artificial
> **Nota de Desenvolvimento:** Os scripts deste repositório foram refatorados e otimizados com o auxílio do modelo de Inteligência Artificial (Gemini). A IA foi utilizada exclusivamente como ferramenta auxiliar para otimização de desempenho, estruturação visual e depuração de sintaxe de código, sob supervisão, instrução, validação e testes contínuos da autora, responsável integral pela concepção teórica, metodológica e analítica da pesquisa.

### Como Citar Este Repositório

```bibtex
@misc{LOPES_2026,
  author       = {Amanda Ribeiro Lopes},
  title        = {A Evolução da Racionalidade Estratégica do Partido-Estado Chinês (13º ao 15º PQ)},
  year         = {2026},
  publisher    = {GitHub},
  journal      = {Repositório GitHub},
  howpublished = {\url{https://github.com/[seu-usuario]/[seu-repositorio]}}
}

```

---
