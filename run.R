# =============================================================
# run.R - ponto de entrada do pipeline
#
# Rode com o working directory na RAIZ do projeto.
# Pre-requisito: data/raw/ com os microdados do Censo Escolar
# INEP (2015-2025), ver README.md para onde baixar cada ano.
# =============================================================

# Passo 1 - pipeline principal do Censo Escolar: le os microdados
# brutos, tipa, empilha 2015-2025, monta as 3 tabelas analiticas
# (tab_municipios, tab_capitais, tab_saopaulo), valida celula a
# celula contra o CSV bruto do INEP (GATE) e exporta:
#   outputs/tabelas_analiticas_2015_2025.xlsx
#   outputs/codebook_censo_lemann_cem.xlsx
# Script monolitico (blocos 00-07 no proprio arquivo).
source("leman_final.R")

# Passo 2 - coleta de fontes externas (populacao IBGE, PNAD Continua
# creche/pre-escola nas capitais, Bolsa Familia por distrito de SP).
# Roda separado porque depende de rede liberada para IBGE/SIDRA e
# para dados.prefeitura.sp.gov.br. Produz PNUD/outputs/.
# source("PNUD/coleta_pnadc_pop_bolsafamilia.R")

# Passo 3 - integra o resultado do Passo 2 no banco final do Passo 1
# e gera o BANCO DEFINITIVO, sem sobrescrever os arquivos do Passo 1:
#   outputs/tabelas_analiticas_2015_2025_v2.xlsx
#   outputs/codebook_censo_lemann_cem_v2.xlsx
# source("integrar_banco_final.R")
