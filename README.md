# Censo Escolar INEP 2015-2025 — Municípios, Capitais e Distritos de São Paulo

Pipeline em R que organiza os microdados do Censo Escolar (INEP, 2015-2025) em três tabelas analíticas — municípios, capitais e distritos do município de São Paulo —, complementadas com população (IBGE), frequência em creche/pré-escola (PNAD Contínua) e cobertura do Programa Bolsa Família em creches/pré-escolas de São Paulo (Prefeitura de SP).

## Estrutura do projeto

```
.
├── run.R                        # ponto de entrada, documenta a ordem de execução
├── leman_final.R                # pipeline principal (monolítico): INEP bruto -> 3 tabelas analíticas + codebook
├── integrar_banco_final.R       # integra população/PNAD-C/Bolsa Família e gera o BANCO DEFINITIVO (v2)
├── data/
│   ├── raw/                     # microdados INEP brutos (não versionado, ver "Como rodar" abaixo)
│   └── processed/               # .rds intermediários do pipeline (não versionado, regenerável)
├── outputs/
│   ├── tabelas_analiticas_2015_2025.xlsx      # Passo 1: só Censo Escolar INEP (abas municipios / capitais / saopaulo)
│   ├── codebook_censo_lemann_cem.xlsx         # codebook do Passo 1
│   ├── tabelas_analiticas_2015_2025_v2.xlsx   # BANCO DEFINITIVO: Passo 1 + população + PNAD-C + Bolsa Família
│   └── codebook_censo_lemann_cem_v2.xlsx      # codebook do banco definitivo
├── PNUD/                         # coleta de população, PNAD Contínua e Bolsa Família
│   ├── coleta_pnadc_pop_bolsafamilia.R
│   └── outputs/
│       ├── conferencia_pnadc_pop_bolsafamilia.rds
│       └── bf_distrito_painel_2015_2026.csv
└── archive/                      # versões anteriores/soltas, mantidas só como referência
    ├── censo_escolar_2015_2025.R
    ├── codbook.R
    └── relatorio_tecnico_v1/
```

## As três tabelas

- **`tab_municipios`**: um registro por município brasileiro (~5.570) por ano (2015-2025), com contagens de escolas e matrículas por rede (direta/indireta municipal/indireta municipal-estadual) e recorte, mais população total e por faixa etária (0-3, 4-5 anos) e, para as capitais, a taxa de frequência em creche/pré-escola da PNAD Contínua.
- **`tab_capitais`**: mesmo schema de `tab_municipios`, filtrado às 27 capitais.
- **`tab_saopaulo`**: mesmo schema, por distrito do município de São Paulo (96 distritos), mais população do município (repetida por distrito, ver caveat abaixo) e cobertura do Bolsa Família por distrito.

Estrutura completa de colunas, tipos e percentual de preenchimento: aba `dicionario_municipios` / `dicionario_capitais` / `dicionario_saopaulo` do codebook.

## Como rodar

1. Baixe os microdados do Censo Escolar INEP (2015-2025) em <https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar> e coloque em `data/raw/` (nomes esperados: `microdados_ed_basica_AAAA.csv` para 2015-2024, e os arquivos separados de 2025 — `Tabela_Escola_2025.csv`, `Tabela_Matricula_2025.csv`, `Tabela_Docente_2025.csv`, `Tabela_Turma_2025.csv`, `Tabela_Curso_Tecnico_2025.csv`, `Tabela_Gestor_Escolar_2025.csv` —, já que o INEP mudou o formato de distribuição em 2025).
2. Com o working directory na **raiz do projeto**, rode `source("run.R")`. O Passo 1 (`leman_final.R`) gera `outputs/tabelas_analiticas_2015_2025.xlsx` e o codebook correspondente, validando célula a célula contra o CSV bruto do INEP (GATE — interrompe se houver divergência).
3. Para o **banco definitivo** (com população/PNAD-C/Bolsa Família): rode `PNUD/coleta_pnadc_pop_bolsafamilia.R` (precisa de rede liberada para IBGE/SIDRA e para dados.prefeitura.sp.gov.br) e depois `integrar_banco_final.R`, que grava `outputs/tabelas_analiticas_2015_2025_v2.xlsx` e `outputs/codebook_censo_lemann_cem_v2.xlsx` **sem sobrescrever** os arquivos do Passo 1.

> Os scripts localizam a raiz do projeto sozinhos (subindo diretórios) ou exigem que o working directory já seja a raiz — não há mais `setwd()` com caminho fixo de máquina.

## Fontes de dados

| Fonte | Uso | Nível geográfico |
|---|---|---|
| Censo Escolar INEP | escolas, matrículas, docentes | escola → agregado a município/capital/distrito |
| IBGE (`brpop`, SIDRA 9514) | população total e por idade (0-3, 4-5 anos) | município (2022 exato via Censo Demográfico; demais anos estimados) |
| PNAD Contínua (SIDRA 7138) | taxa de frequência em creche/pré-escola | só as 27 capitais (restrição da amostra) |
| Prefeitura de SP (dados.prefeitura.sp.gov.br) | beneficiários do Bolsa Família em creche/pré-escola | distrito, 2015-2026 (duas fontes: descontinuada até 2023, atual desde 2024) |

## Metodologia e limitações conhecidas

O codebook documenta os princípios metodológicos do pipeline na aba `notas_metodologicas` (P1-P9), a descrição de cada variável em `glossario`, e a validação célula a célula contra o CSV bruto do INEP em `validacao`. Pontos que merecem atenção especial:

- **Bolsa Família não é população.** As colunas `qtd_bf_*` medem apenas crianças beneficiárias do programa matriculadas, não a população total de 0-3/4-5 anos do distrito — não usar como denominador de taxa de cobertura populacional.
- **População em `tab_saopaulo` é do município, repetida por distrito** (colunas com sufixo `_mun`). População geral por distrito de São Paulo não está disponível em nenhuma fonte com API ou exportação viável (SEADE sem exportação automatizada, Censo 2022 por setor censitário descartado por custo-benefício).
- **PNAD Contínua só cobre as 27 capitais**, confirmado empiricamente via teste de controle (município não-capital retorna `NA` em todos os anos). No banco definitivo, `taxa_creche_0a3` fica preenchida em 208 das 216 células capital × ano — 8 cruzamentos retornam `NA` na própria SIDRA.
- **`tab_saopaulo` no banco definitivo**: 5 linhas (um distrito nos anos 2015-2019) ficam sem dado de Bolsa Família porque o dataset antigo da Prefeitura não reporta esse distrito nesses anos.
- Fora do escopo por decisão do projeto: Região Metropolitana de São Paulo (SIDRA 7139, cor/raça) e cor/raça por subprefeitura.

## Notas de manutenção

- `leman_final.R` é monolítico por opção; os blocos 00-07 estão comentados dentro do próprio arquivo.
- Os xlsx de saída podem gerar um aviso de reparo ao abrir no Excel (relação de `drawing`/`vmlDrawing` do `openxlsx::freezePane()`); não afeta os dados. O banco definitivo (`integrar_banco_final.R`) é gravado sem `freezePane` justamente para evitar isso.
