# ============================================================
# PIPELINE CENSO ESCOLAR DA EDUCAÇÃO BÁSICA — 2015–2025
# VERSÃO FINAL CORRIGIDA — Abr/2026
# LEMANN-CEM 
# Analista: ANDERSON HENRIQUE
# RODADO EM 23 DE ABRIL SE 2026
# ============================================================

rm(list = ls())
gc()

setwd("~/Desktop/leman_cem")

# ── Pacotes ───────────────────────────────────────────────────────────────────
pacotes <- c("educabR", "dplyr", "readr", "janitor",
             "purrr", "stringr", "tidyr", "openxlsx")
invisible(lapply(pacotes, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}))

library(educabR); library(dplyr); library(readr)
library(janitor);  library(purrr); library(stringr)
library(tidyr);    library(openxlsx)

# ============================================================
# VARIÁVEIS DE INTERESSE — 66 variáveis canônicas
# ============================================================

variaveis_interesse <- c(
  "NU_ANO_CENSO", "SG_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
  "NO_MICRORREGIAO", "CO_MICRORREGIAO", "NO_DISTRITO", "CO_DISTRITO",
  "CO_ENTIDADE", "DS_ENDERECO", "NU_ENDERECO", "DS_COMPLEMENTO", "CO_CEP",
  "TP_DEPENDENCIA", "TP_CATEGORIA_ESCOLA_PRIVADA", "TP_LOCALIZACAO",
  "TP_LOCALIZACAO_DIFERENCIADA", "TP_SITUACAO_FUNCIONAMENTO",
  "IN_PODER_PUBLICO_PARCERIA",
  "TP_PODER_PUBLICO_PARCERIA",        # nome canônico INEP
  "IN_MANT_ESCOLA_PRIVADA_EMP", "IN_MANT_ESCOLA_PRIV_ONG_OSCIP",
  "IN_MANT_ESCOLA_PRIVADA_SIND", "IN_ALIMENTACAO",
  "IN_INF", "IN_INF_CRE", "IN_INF_PRE",
  "QT_MAT_BAS", "QT_MAT_INF_CRE", "QT_MAT_INF_PRE",
  "QT_MAT_FUND_AI", "QT_MAT_FUND_AF", "QT_MAT_MED", "QT_MAT_PROF",
  "QT_MAT_EJA", "QT_MAT_EJA_FUND_AI", "QT_MAT_EJA_FUND_AF", "QT_MAT_EJA_MED",
  "QT_MAT_ESP", "QT_MAT_ESP_CC", "QT_MAT_ESP_CE",
  "QT_MAT_BAS_FEM", "QT_MAT_BAS_MASC", "QT_MAT_BAS_ND",
  "QT_MAT_BAS_BRANCA", "QT_MAT_BAS_PRETA", "QT_MAT_BAS_PARDA",
  "QT_MAT_BAS_AMARELA", "QT_MAT_BAS_INDIGENA",
  "QT_MAT_INF_INT", "QT_MAT_INF_CRE_INT", "QT_MAT_INF_PRE_INT",
  "QT_MAT_FUND_INT", "QT_MAT_FUND_AI_INT", "QT_MAT_FUND_AF_INT",
  "QT_MAT_MED_INT", "QT_MAT_ZR_URB", "QT_MAT_ZR_RUR",
  "QT_TRANSP_PUBLICO",
  "QT_DOC_INF_CRE", "QT_DOC_INF_PRE",
  "QT_TUR_INF_CRE", "QT_TUR_INF_PRE", "QT_TUR_INF_INT",
  "QT_TUR_INF_CRE_INT", "QT_TUR_INF_PRE_INT"
)

# ============================================================
# BLOCO 1 — CARREGAMENTO 2015–2024
# ============================================================

# ── Função de padronização ────────────────────────────────────────────────────
padronizar_df <- function(df, ano) {
  
  names(df) <- toupper(names(df))
  
  # [1] IN_CONVENIADA_PP → IN_PODER_PUBLICO_PARCERIA (renomeio histórico)
  if ("IN_CONVENIADA_PP" %in% names(df) &&
      !"IN_PODER_PUBLICO_PARCERIA" %in% names(df)) {
    df <- rename(df, IN_PODER_PUBLICO_PARCERIA = IN_CONVENIADA_PP)
    message("  [renomeado] IN_CONVENIADA_PP → IN_PODER_PUBLICO_PARCERIA")
  }
  
  # [2] TP_CONVENIO_PODER_PUBLICO → TP_PODER_PUBLICO_PARCERIA
  #     Ocorre em 2015–2021 via educabR e em 2022 (coluna era 100% NA nesse ano;
  #     o nome correto do dicionário INEP é TP_PODER_PUBLICO_PARCERIA)
  if ("TP_CONVENIO_PODER_PUBLICO" %in% names(df) &&
      !"TP_PODER_PUBLICO_PARCERIA" %in% names(df)) {
    df <- rename(df, TP_PODER_PUBLICO_PARCERIA = TP_CONVENIO_PODER_PUBLICO)
    message("  [renomeado] TP_CONVENIO_PODER_PUBLICO → TP_PODER_PUBLICO_PARCERIA")
  }
  
  # Garante NU_ANO_CENSO
  df <- mutate(df, NU_ANO_CENSO = as.integer(ano))
  
  # Identifica ausentes e preenche com NA estrutural
  disponiveis <- intersect(variaveis_interesse, names(df))
  ausentes    <- setdiff(variaveis_interesse, names(df))
  
  if (length(ausentes) > 0)
    message("  [NA estrutural] Ausentes em ", ano, ": ",
            paste(ausentes, collapse = ", "))
  
  df <- select(df, all_of(disponiveis))
  for (col in ausentes) df[[col]] <- NA
  
  df <- select(df, all_of(variaveis_interesse))
  
  message("  [ok] ", format(nrow(df), big.mark = "."),
          " escolas | ", ncol(df), " colunas")
  df
}

# ── educabR: 2015–2021 e 2023–2024 ───────────────────────────────────────────
carregar_ano <- function(ano) {
  message("\n>>> Carregando ano: ", ano)
  df <- tryCatch(
    educabR::get_censo_escolar(ano, quiet = FALSE),
    error = function(e) { warning("Erro ", ano, ": ", e$message); NULL }
  )
  if (is.null(df)) return(NULL)
  padronizar_df(df, ano)
}

anos_educabr <- c(2015:2021, 2023:2024)
lista_anos   <- map(anos_educabr, carregar_ano) |>
  setNames(as.character(anos_educabr)) |>
  Filter(Negate(is.null), x = _)

message("\n>>> Anos carregados via educabR: ",
        paste(names(lista_anos), collapse = ", "))

# ── CSV manual: 2022 ──────────────────────────────────────────────────────────
message("\n>>> Carregando 2022 via CSV local...")

censo_2022_bruto <- read_delim(
  "microdados_ed_basica_2022.csv",
  delim          = ";",
  locale         = locale(encoding = "latin1", decimal_mark = ",",
                          grouping_mark = "."),
  col_types      = cols(.default = col_character()),
  progress       = TRUE,
  show_col_types = FALSE
)

message("  Bruto 2022: ", format(nrow(censo_2022_bruto), big.mark = "."),
        " linhas | ", ncol(censo_2022_bruto), " colunas")

censo_2022 <- padronizar_df(censo_2022_bruto, 2022)

# ── Empilhamento 2015–2024 ────────────────────────────────────────────────────
lista_chr      <- map(lista_anos, ~ mutate(.x, across(everything(), as.character)))
censo_2022_chr <- mutate(censo_2022, across(everything(), as.character))

# unname() remove os nomes da lista antes do bind_rows
censo_2015_2024 <- bind_rows(unname(lista_chr), censo_2022_chr) |>
  arrange(NU_ANO_CENSO)

message(">>> BANCO 2015–2024: ",
        format(nrow(censo_2015_2024), big.mark = ","), " linhas | ",
        ncol(censo_2015_2024), " colunas")

count(censo_2015_2024, NU_ANO_CENSO, name = "n_escolas") |> print(n = Inf)

message("\n>>> BANCO 2015–2024: ",
        format(nrow(censo_2015_2024), big.mark = ","), " linhas | ",
        ncol(censo_2015_2024), " colunas")

count(censo_2015_2024, NU_ANO_CENSO, name = "n_escolas") |> print(n = Inf)

# ============================================================
# BLOCO 2 — CARREGAMENTO 2025
# ============================================================

message("\n>>> Lendo arquivos CSV de 2025...")

ler_csv_inep <- function(arquivo) {
  message("  Lendo: ", arquivo)
  df <- read_delim(
    arquivo,
    delim          = ";",
    locale         = locale(encoding = "latin1", decimal_mark = ",",
                            grouping_mark = "."),
    col_types      = cols(.default = col_character()),
    progress       = TRUE,
    show_col_types = FALSE
  )
  names(df) <- toupper(names(df))
  message("  [ok] ", format(nrow(df), big.mark = ","),
          " linhas | ", ncol(df), " colunas")
  df
}

tab_escola    <- ler_csv_inep("Tabela_Escola_2025.csv")
tab_matricula <- ler_csv_inep("Tabela_Matricula_2025.csv")
tab_docente   <- ler_csv_inep("Tabela_Docente_2025.csv")
tab_turma     <- ler_csv_inep("Tabela_Turma_2025.csv")

# ── Variáveis esperadas por tabela ────────────────────────────────────────────
# NOTA: IN_INF / IN_INF_CRE / IN_INF_PRE AUSENTES nos microdados 2025 —
#       NA estrutural confirmado (mudança de instrumento INEP)
vars_escola_2025 <- c(
  "NU_ANO_CENSO", "SG_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
  "NO_MICRORREGIAO", "CO_MICRORREGIAO", "NO_DISTRITO", "CO_DISTRITO",
  "CO_ENTIDADE", "DS_ENDERECO", "NU_ENDERECO", "DS_COMPLEMENTO", "CO_CEP",
  "TP_DEPENDENCIA", "TP_CATEGORIA_ESCOLA_PRIVADA", "TP_LOCALIZACAO",
  "TP_LOCALIZACAO_DIFERENCIADA", "TP_SITUACAO_FUNCIONAMENTO",
  "IN_PODER_PUBLICO_PARCERIA", "TP_PODER_PUBLICO_PARCERIA",
  "IN_MANT_ESCOLA_PRIVADA_EMP", "IN_MANT_ESCOLA_PRIV_ONG_OSCIP",
  "IN_MANT_ESCOLA_PRIVADA_SIND", "IN_ALIMENTACAO",
  # IN_INF / IN_INF_CRE / IN_INF_PRE ausentes em 2025
  "QT_TRANSP_PUBLICO"
)

vars_matricula_2025 <- c(
  "CO_ENTIDADE",
  "QT_MAT_BAS", "QT_MAT_INF_CRE", "QT_MAT_INF_PRE",
  "QT_MAT_FUND_AI", "QT_MAT_FUND_AF", "QT_MAT_MED", "QT_MAT_PROF",
  "QT_MAT_EJA", "QT_MAT_EJA_FUND_AI", "QT_MAT_EJA_FUND_AF", "QT_MAT_EJA_MED",
  "QT_MAT_ESP", "QT_MAT_ESP_CC", "QT_MAT_ESP_CE",
  "QT_MAT_BAS_FEM", "QT_MAT_BAS_MASC", "QT_MAT_BAS_ND",
  "QT_MAT_BAS_BRANCA", "QT_MAT_BAS_PRETA", "QT_MAT_BAS_PARDA",
  "QT_MAT_BAS_AMARELA", "QT_MAT_BAS_INDIGENA",
  "QT_MAT_INF_INT", "QT_MAT_INF_CRE_INT", "QT_MAT_INF_PRE_INT",
  "QT_MAT_FUND_INT", "QT_MAT_FUND_AI_INT", "QT_MAT_FUND_AF_INT",
  "QT_MAT_MED_INT", "QT_MAT_ZR_URB", "QT_MAT_ZR_RUR",
  "QT_TRANSP_PUBLICO"
)

vars_docente_2025 <- c("CO_ENTIDADE", "QT_DOC_INF_CRE", "QT_DOC_INF_PRE")

vars_turma_2025 <- c(
  "CO_ENTIDADE",
  "QT_TUR_INF_CRE", "QT_TUR_INF_PRE", "QT_TUR_INF_INT",
  "QT_TUR_INF_CRE_INT", "QT_TUR_INF_PRE_INT"
)

# ── Agregação por CO_ENTIDADE ─────────────────────────────────────────────────
agregar_por_escola <- function(df, vars_ok, nome) {
  vars_num <- setdiff(vars_ok, "CO_ENTIDADE")
  resultado <- df |>
    select(all_of(intersect(vars_ok, names(df)))) |>
    mutate(across(-CO_ENTIDADE,
                  ~ as.numeric(str_replace_all(., ",", ".")))) |>
    group_by(CO_ENTIDADE) |>
    summarise(across(all_of(intersect(vars_num, names(df))),
                     ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")
  message("  [", nome, "] ",
          format(nrow(resultado), big.mark = ","), " escolas únicas")
  resultado
}

mat_ag <- agregar_por_escola(tab_matricula, vars_matricula_2025, "Matricula")
doc_ag <- agregar_por_escola(tab_docente,   vars_docente_2025,   "Docente")
tur_ag <- agregar_por_escola(tab_turma,     vars_turma_2025,     "Turma")

# ── Join ──────────────────────────────────────────────────────────────────────
base_escola_2025 <- tab_escola |>
  select(all_of(intersect(vars_escola_2025, names(tab_escola))))

censo_2025_raw <- base_escola_2025 |>
  left_join(mat_ag, by = "CO_ENTIDADE") |>
  left_join(doc_ag, by = "CO_ENTIDADE") |>
  left_join(tur_ag, by = "CO_ENTIDADE") |>
  mutate(NU_ANO_CENSO = "2025")

message("  Após joins: ", format(nrow(censo_2025_raw), big.mark = ","),
        " linhas | ", ncol(censo_2025_raw), " colunas")

if (nrow(censo_2025_raw) != nrow(base_escola_2025))
  warning("[ALERTA] Join gerou duplicação de linhas!")

# ── Preenche colunas ausentes em 2025 com NA estrutural ───────────────────────
ausentes_2025 <- setdiff(variaveis_interesse, names(censo_2025_raw))
if (length(ausentes_2025) > 0) {
  message("  [NA estrutural 2025]: ", paste(ausentes_2025, collapse = ", "))
  for (col in ausentes_2025) censo_2025_raw[[col]] <- NA
}

censo_2025 <- censo_2025_raw |>
  mutate(across(everything(), as.character)) |>
  select(all_of(variaveis_interesse))

message("  [ok] 2025 final: ", format(nrow(censo_2025), big.mark = ","),
        " escolas | ", ncol(censo_2025), " colunas")

# ============================================================
# BLOCO 3 — CONSOLIDAÇÃO E PADRONIZAÇÕES SEMÂNTICAS
# ============================================================

message("\n>>> Consolidando banco completo 2015–2025...")

censo_completo <- bind_rows(censo_2015_2024, censo_2025) |>
  arrange(NU_ANO_CENSO)

message(">>> BANCO COMPLETO: ",
        format(nrow(censo_completo), big.mark = ","), " linhas | ",
        ncol(censo_completo), " colunas")

print(table(censo_completo$NU_ANO_CENSO))

# ── 3.1 Conversão de tipos ────────────────────────────────────────────────────
vars_numericas <- c(
  "CO_MUNICIPIO", "CO_MICRORREGIAO", "CO_DISTRITO",
  "CO_ENTIDADE", "CO_CEP", "NU_ENDERECO", "NU_ANO_CENSO",
  "QT_MAT_BAS", "QT_MAT_INF_CRE", "QT_MAT_INF_PRE",
  "QT_MAT_FUND_AI", "QT_MAT_FUND_AF", "QT_MAT_MED", "QT_MAT_PROF",
  "QT_MAT_EJA", "QT_MAT_EJA_FUND_AI", "QT_MAT_EJA_FUND_AF", "QT_MAT_EJA_MED",
  "QT_MAT_ESP", "QT_MAT_ESP_CC", "QT_MAT_ESP_CE",
  "QT_MAT_BAS_FEM", "QT_MAT_BAS_MASC", "QT_MAT_BAS_ND",
  "QT_MAT_BAS_BRANCA", "QT_MAT_BAS_PRETA", "QT_MAT_BAS_PARDA",
  "QT_MAT_BAS_AMARELA", "QT_MAT_BAS_INDIGENA",
  "QT_MAT_INF_INT", "QT_MAT_INF_CRE_INT", "QT_MAT_INF_PRE_INT",
  "QT_MAT_FUND_INT", "QT_MAT_FUND_AI_INT", "QT_MAT_FUND_AF_INT",
  "QT_MAT_MED_INT", "QT_MAT_ZR_URB", "QT_MAT_ZR_RUR",
  "QT_TRANSP_PUBLICO",
  "QT_DOC_INF_CRE", "QT_DOC_INF_PRE",
  "QT_TUR_INF_CRE", "QT_TUR_INF_PRE", "QT_TUR_INF_INT",
  "QT_TUR_INF_CRE_INT", "QT_TUR_INF_PRE_INT"
)

censo_completo <- censo_completo |>
  mutate(across(all_of(vars_numericas),
                ~ as.numeric(str_replace_all(.x, ",", "."))))

# ── 3.2 TP_PODER_PUBLICO_PARCERIA: NA semântico → 0 a partir de 2022 ─────────
#
# LÓGICA:
#   2015–2021: valores 0, 1, 2, 3 — 0% NA
#              (0 = sem convênio registrado explicitamente)
#   2022–2025: valores 1, 2, 3   — ~95–96% NA
#              (INEP mudou a lógica: campo só preenchido para conveniadas;
#               escolas sem convênio ficam com NA — semanticamente = 0)
#   DECISÃO: substituir NA por 0 a partir de 2022 para
#            manter comparabilidade longitudinal
#
# VALIDAÇÃO EMPÍRICA: NA em 2022–2025 são ~100% escolas públicas
# (TP_DEPENDENCIA 1, 2 ou 3) — escola pública não tem convênio
# com o poder público por definição. Confirma a substituição.

message("\n>>> Corrigindo NA semântico em TP_PODER_PUBLICO_PARCERIA...")

censo_completo <- censo_completo |>
  mutate(
    TP_PODER_PUBLICO_PARCERIA = case_when(
      NU_ANO_CENSO >= 2022 & is.na(TP_PODER_PUBLICO_PARCERIA) ~ 0L,
      TRUE ~ as.integer(TP_PODER_PUBLICO_PARCERIA)
    ),
    # Variável auxiliar binária para uso analítico direto
    IN_CONVENIO = if_else(TP_PODER_PUBLICO_PARCERIA > 0, 1L, 0L)
  )

message("  Distribuição TP_PODER_PUBLICO_PARCERIA pós-correção:")
censo_completo |>
  group_by(NU_ANO_CENSO) |>
  summarise(
    pct_na       = round(mean(is.na(TP_PODER_PUBLICO_PARCERIA)) * 100, 1),
    pct_convenio = round(mean(TP_PODER_PUBLICO_PARCERIA > 0, na.rm = TRUE) * 100, 1),
    valores      = paste(sort(unique(na.omit(as.character(TP_PODER_PUBLICO_PARCERIA)))),
                         collapse = " | ")
  ) |>
  print(n = Inf, width = Inf)

# ── 3.3 TP_LOCALIZACAO_DIFERENCIADA: replicação retroativa ───────────────────
# Referência: ano mais recente disponível (2025)
# Lógica: classificação estrutural da escola — estável no período

message("\n>>> Replicando TP_LOCALIZACAO_DIFERENCIADA por CO_ENTIDADE...")

ref_localizacao <- censo_completo |>
  filter(NU_ANO_CENSO == 2025, !is.na(TP_LOCALIZACAO_DIFERENCIADA)) |>
  group_by(CO_ENTIDADE) |>
  slice_max(NU_ANO_CENSO, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(CO_ENTIDADE, TP_LOCALIZACAO_DIFERENCIADA_REF = TP_LOCALIZACAO_DIFERENCIADA)

censo_completo <- censo_completo |>
  left_join(ref_localizacao, by = "CO_ENTIDADE") |>
  mutate(
    TP_LOCALIZACAO_DIFERENCIADA = case_when(
      !is.na(TP_LOCALIZACAO_DIFERENCIADA_REF) ~ TP_LOCALIZACAO_DIFERENCIADA_REF,
      !is.na(TP_LOCALIZACAO_DIFERENCIADA)     ~ TP_LOCALIZACAO_DIFERENCIADA,
      TRUE ~ NA_character_
    )
  ) |>
  select(-TP_LOCALIZACAO_DIFERENCIADA_REF)

# ── 3.4 Labels semânticos ─────────────────────────────────────────────────────

censo_completo <- censo_completo |>
  mutate(
    TP_LOCALIZACAO_DIFERENCIADA = case_when(
      TP_LOCALIZACAO_DIFERENCIADA == "0" ~ "Escola fora de área diferenciada",
      TP_LOCALIZACAO_DIFERENCIADA == "1" ~ "Área de assentamento",
      TP_LOCALIZACAO_DIFERENCIADA == "2" ~ "Terra indígena",
      TP_LOCALIZACAO_DIFERENCIADA == "3" ~ "Área com comunidade remanescente de quilombos",
      TP_LOCALIZACAO_DIFERENCIADA == "8" ~ "Área com povos e comunidades tradicionais",
      TRUE ~ NA_character_
    ),
    TP_CATEGORIA_ESCOLA_PRIVADA = case_when(
      TP_CATEGORIA_ESCOLA_PRIVADA == "1" ~ "Particular",
      TP_CATEGORIA_ESCOLA_PRIVADA == "2" ~ "Comunitária",
      TP_CATEGORIA_ESCOLA_PRIVADA == "3" ~ "Confessional",
      TP_CATEGORIA_ESCOLA_PRIVADA == "4" ~ "Filantrópica",
      TRUE ~ NA_character_
    )
  )

# ── 3.5 Checagem final ────────────────────────────────────────────────────────

message("\n=== CHECAGEM FINAL ===")
message("Dimensões: ",
        format(nrow(censo_completo), big.mark = ","), " linhas | ",
        ncol(censo_completo), " colunas")

message("\nNA geral — variáveis com algum NA:")
censo_completo |>
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "pct_na") |>
  filter(pct_na > 0) |>
  arrange(desc(pct_na)) |>
  mutate(status = case_when(
    pct_na == 100 ~ "AUSENTE",
    pct_na < 10   ~ "baixo",
    pct_na < 50   ~ "moderado",
    pct_na < 100  ~ "alto"
  )) |>
  print(n = Inf)

message("\nNA por variável × ano — só onde há problema:")
censo_completo |>
  group_by(NU_ANO_CENSO) |>
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1)),
            .groups = "drop") |>
  pivot_longer(-NU_ANO_CENSO, names_to = "variavel", values_to = "pct_na") |>
  pivot_wider(names_from = NU_ANO_CENSO, values_from = pct_na) |>
  filter(if_any(-variavel, ~ . > 0)) |>
  print(n = Inf, width = Inf)

# ── 3.6 Validação dos NAs por grupo ──────────────────────────────────────────

message("\n=== GRUPO A: NAs estruturais (introdução em 2022) × TP_SITUACAO ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c(2023, 2024)) |>
  mutate(na_grupo_a = is.na(QT_MAT_ZR_URB)) |>
  count(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO, na_grupo_a) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = NU_ANO_CENSO) |>
  print(n = Inf)

message("\n=== GRUPO B: NAs em privadas × TP_DEPENDENCIA ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c(2022, 2023, 2024)) |>
  mutate(na_privada = is.na(TP_CATEGORIA_ESCOLA_PRIVADA)) |>
  count(NU_ANO_CENSO, TP_DEPENDENCIA, na_privada) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = c(NU_ANO_CENSO, TP_DEPENDENCIA)) |>
  print(n = Inf)

message("\n=== GRUPO B2: TP_PODER_PUBLICO_PARCERIA — quem preenche? ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c(2022, 2023, 2024),
         TP_PODER_PUBLICO_PARCERIA > 0) |>
  count(NU_ANO_CENSO, TP_DEPENDENCIA, TP_PODER_PUBLICO_PARCERIA) |>
  print(n = Inf)

message("\n=== GRUPO C: NAs em QT_MAT_BAS × TP_SITUACAO ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c(2022, 2023, 2024)) |>
  mutate(na_mat = is.na(QT_MAT_BAS)) |>
  count(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO, na_mat) |>
  mutate(pct = round(n / sum(n) * 100, 1), .by = c(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO)) |>
  print(n = Inf)

# ── 3.7 Subsets ───────────────────────────────────────────────────────────────

capitais_co <- c(
  1100205, 1302603, 1200401, 1600303, 1721000, 1400100,
  1501402, 2111300, 2211001, 2304400, 2408102, 2507507,
  2611606, 2704302, 2800308, 2927408, 3106200, 3205309,
  3304557, 3550308, 4106902, 4205407, 4314902, 5002704,
  5103403, 5208707, 5300108
)

censo_capitais  <- filter(censo_completo, CO_MUNICIPIO %in% capitais_co)
censo_saopaulo  <- filter(censo_completo, CO_MUNICIPIO == 3550308)

message("\nSubset capitais: ", format(nrow(censo_capitais), big.mark = ","),
        " linhas | ", n_distinct(censo_capitais$CO_MUNICIPIO), " municípios")
message("Subset São Paulo: ", format(nrow(censo_saopaulo), big.mark = ","), " linhas")

# ── 3.8 Salvar RDS ────────────────────────────────────────────────────────────

message("\n>>> Salvando .RDS...")
saveRDS(censo_completo, "censo_completo_2015_2025.rds")
saveRDS(censo_capitais, "censo_capitais_2015_2025.rds")
saveRDS(censo_saopaulo, "censo_saopaulo_2015_2025.rds")

fs <- file.size(c("censo_completo_2015_2025.rds",
                  "censo_capitais_2015_2025.rds",
                  "censo_saopaulo_2015_2025.rds"))
names(fs) <- c("completo", "capitais", "sao_paulo")
print(round(fs / 1024^2, 1))











### Parte 2 - Adicao de variaveis de estrutura #####

# ============================================================
# PATCH CIRÚRGICO — Injetar 4 variáveis de infraestrutura
# no censo_completo já construído
# ============================================================

novas_vars <- c("IN_AREA_VERDE", "IN_BANHEIRO_EI",
                "IN_MATERIAL_PED_JOGOS", "IN_MATERIAL_PED_ARTISTICAS")

# ── Função auxiliar: extrai chave + novas vars de um df bruto ────────────────
extrair_infra <- function(df, ano) {
  names(df) <- toupper(names(df))
  df <- mutate(df, NU_ANO_CENSO = as.character(ano))
  
  presentes <- intersect(novas_vars, names(df))
  ausentes  <- setdiff(novas_vars, names(df))
  
  if (length(ausentes) > 0)
    message("  [NA estrutural] Ausentes em ", ano, ": ",
            paste(ausentes, collapse = ", "))
  
  df <- select(df, CO_ENTIDADE, NU_ANO_CENSO, all_of(presentes))
  for (col in ausentes) df[[col]] <- NA_character_
  
  # Garante tipos character para o bind_rows
  mutate(df, across(everything(), as.character))
}

# ── 1. educabR: 2015–2021 e 2023–2024 ───────────────────────────────────────
message(">>> Extraindo infraestrutura via educabR...")

anos_educabr <- c(2015:2021, 2023:2024)

lista_infra <- map(anos_educabr, function(ano) {
  message("  ano: ", ano)
  df <- tryCatch(
    educabR::get_censo_escolar(ano, quiet = TRUE),
    error = function(e) { warning("Erro ", ano, ": ", e$message); NULL }
  )
  if (is.null(df)) return(NULL)
  extrair_infra(df, ano)
}) |>
  setNames(as.character(anos_educabr)) |>
  Filter(Negate(is.null), x = _)

# ── 2. CSV manual: 2022 ──────────────────────────────────────────────────────
message(">>> Extraindo infraestrutura do CSV 2022...")

infra_2022 <- read_delim(
  "microdados_ed_basica_2022.csv",
  delim          = ";",
  locale         = locale(encoding = "latin1"),
  col_select     = c("CO_ENTIDADE", all_of(novas_vars)),
  col_types      = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  extrair_infra(2022)

# ── 3. CSV 2025 ──────────────────────────────────────────────────────────────
message(">>> Extraindo infraestrutura do CSV 2025...")

infra_2025 <- read_delim(
  "Tabela_Escola_2025.csv",
  delim          = ";",
  locale         = locale(encoding = "latin1"),
  col_select     = c("CO_ENTIDADE", all_of(novas_vars)),
  col_types      = cols(.default = col_character()),
  show_col_types = FALSE
) |>
  extrair_infra(2025)

# ── 4. Empilhar todas as fontes ───────────────────────────────────────────────
infra_completo <- bind_rows(unname(lista_infra), infra_2022, infra_2025) |>
  arrange(NU_ANO_CENSO)

message(">>> infra_completo: ",
        format(nrow(infra_completo), big.mark = ","), " linhas | ",
        ncol(infra_completo), " colunas")

# Sanidade: sem duplicatas chave
dupl <- infra_completo |>
  count(CO_ENTIDADE, NU_ANO_CENSO) |>
  filter(n > 1)

if (nrow(dupl) > 0) {
  warning("[ALERTA] ", nrow(dupl), " chaves duplicadas em infra_completo!")
  print(dupl)
} else {
  message("  [ok] Sem duplicatas de chave CO_ENTIDADE × NU_ANO_CENSO")
}

# ── 5. Join no censo_completo ────────────────────────────────────────────────
message(">>> Injetando variáveis no censo_completo...")

# Garante que a chave no censo_completo também é character
censo_completo <- censo_completo |>
  mutate(CO_ENTIDADE   = as.character(CO_ENTIDADE),
         NU_ANO_CENSO  = as.character(NU_ANO_CENSO))

n_antes <- nrow(censo_completo)

censo_completo <- censo_completo |>
  left_join(infra_completo,
            by = c("CO_ENTIDADE", "NU_ANO_CENSO"))

stopifnot(nrow(censo_completo) == n_antes)  # join não pode duplicar linhas
message("  [ok] Linhas antes: ", format(n_antes, big.mark = ","),
        " | depois: ",           format(nrow(censo_completo), big.mark = ","))

# ── 6. Validação de cobertura ────────────────────────────────────────────────
message("\n=== COBERTURA DAS VARIÁVEIS DE INFRAESTRUTURA ===")
censo_completo |>
  group_by(NU_ANO_CENSO) |>
  summarise(
    across(all_of(novas_vars),
           ~ round(mean(!is.na(.)) * 100, 1),
           .names = "cob_{.col}"),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)

# ── 7. Re-salvar RDS ─────────────────────────────────────────────────────────
message(">>> Salvando RDS atualizado...")

# Atualiza subsets também
censo_capitais <- filter(censo_completo, CO_MUNICIPIO %in% capitais_co)
censo_saopaulo <- filter(censo_completo, CO_MUNICIPIO == "3550308")

saveRDS(censo_completo, "censo_completo_2015_2025.rds")
saveRDS(censo_capitais, "censo_capitais_2015_2025.rds")
saveRDS(censo_saopaulo, "censo_saopaulo_2015_2025.rds")

fs <- file.size(c("censo_completo_2015_2025.rds",
                  "censo_capitais_2015_2025.rds",
                  "censo_saopaulo_2015_2025.rds"))
names(fs) <- c("completo", "capitais", "sao_paulo")
print(round(fs / 1024^2, 1))




# ── Diagnóstico: quem são os NAs em 2022–2025? ──────────────────────────────

message("=== NA em IN_AREA_VERDE × TP_SITUACAO_FUNCIONAMENTO (2022–2025) ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c("2022", "2023", "2024", "2025")) |>
  mutate(na_infra = is.na(IN_AREA_VERDE)) |>
  count(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO, na_infra) |>
  mutate(pct = round(n / sum(n) * 100, 1),
         .by = c(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO)) |>
  arrange(NU_ANO_CENSO, TP_SITUACAO_FUNCIONAMENTO, na_infra) |>
  print(n = Inf)

message("\n=== NA × TP_DEPENDENCIA (2022–2025) ===")
censo_completo |>
  filter(NU_ANO_CENSO %in% c("2022", "2023", "2024", "2025")) |>
  mutate(na_infra = is.na(IN_AREA_VERDE)) |>
  count(NU_ANO_CENSO, TP_DEPENDENCIA, na_infra) |>
  mutate(pct = round(n / sum(n) * 100, 1),
         .by = c(NU_ANO_CENSO, TP_DEPENDENCIA)) |>
  arrange(NU_ANO_CENSO, TP_DEPENDENCIA, na_infra) |>
  print(n = Inf)





# ── Salvar com variáveis de infraestrutura incorporadas ──────────────────────

message(">>> Atualizando subsets e salvando RDS...")

censo_capitais <- filter(censo_completo, CO_MUNICIPIO %in% as.character(capitais_co))
censo_saopaulo <- filter(censo_completo, CO_MUNICIPIO == "3550308")

saveRDS(censo_completo, "censo_completo_2015_2025.rds")
saveRDS(censo_capitais, "censo_capitais_2015_2025.rds")
saveRDS(censo_saopaulo, "censo_saopaulo_2015_2025.rds")

fs <- file.size(c("censo_completo_2015_2025.rds",
                  "censo_capitais_2015_2025.rds",
                  "censo_saopaulo_2015_2025.rds"))
names(fs) <- c("completo", "capitais", "sao_paulo")
message("Tamanhos (MB):"); print(round(fs / 1024^2, 1))

# ── Confirmação final das dimensões ─────────────────────────────────────────
message("\nDimensões finais:")
message("  censo_completo : ", format(nrow(censo_completo), big.mark=","),
        " × ", ncol(censo_completo))
message("  censo_capitais : ", format(nrow(censo_capitais), big.mark=","),
        " × ", ncol(censo_capitais))
message("  censo_saopaulo : ", format(nrow(censo_saopaulo), big.mark=","),
        " × ", ncol(censo_saopaulo))

message("\n>>> Colunas novas presentes: ",
        paste(intersect(novas_vars, names(censo_completo)), collapse = ", "))




### Parte 3 - Criação de novas variaveis #####


# ============================================================
# BLOCO 4 — CONSTRUÇÃO DAS TABELAS ANALÍTICAS
# Unidades: município × ano  /  distrito × ano (São Paulo)
# ============================================================
# [N1] TP_CONVENIO_PODER_PUBLICO (doc) = TP_PODER_PUBLICO_PARCERIA (banco, integer)
# [N2] TP_LOCALIZACAO_DIFERENCIADA = labels textuais (Bloco 3.4)
# [N3] Tipos pós-patch: NU_ANO_CENSO, CO_ENTIDADE = character;
#      QT_/CO_MUNICIPIO = numeric; TP_PODER_PUBLICO_PARCERIA = integer; IN_/TP_ = character
# [N4] Infraestrutura (branco no doc): sum(EI_CRE + EI_PRE) | IN_x == "1"
# [N5] Correção: mat_pré por localização → QT_MAT_INF_PRE (era INF_CRE)
# [N6] Correção: mat_bas rede indireta por loc_dif → TP_DEPENDENCIA "4" (era "3")
# [N7] doc/tur: sum(doc)/sum(tur) — evita viés de médias de razões individuais
# [N8] mat_ei = sum(CRE) + sum(PRE) separados (evita NA propagation de x+y)
# ============================================================

# ── Filtros reutilizáveis ─────────────────────────────────────────────────────

f_dir         <- function(df) filter(df, TP_DEPENDENCIA == "3")

f_ind_mun     <- function(df)      # só municipal (TP_PPP = 1)  [N1]
  filter(df, TP_DEPENDENCIA            == "4",
         IN_PODER_PUBLICO_PARCERIA == "1",
         TP_PODER_PUBLICO_PARCERIA == 1L)

f_ind_mun_est <- function(df)      # municipal + estadual (TP_PPP = 3)  [N1]
  filter(df, TP_DEPENDENCIA            == "4",
         IN_PODER_PUBLICO_PARCERIA == "1",
         TP_PODER_PUBLICO_PARCERIA == 3L)

# ── Rótulos curtos para pivot_wider ──────────────────────────────────────────

loc_map <- c("1" = "urb", "2" = "rur")

loc_dif_map <- c(
  "Escola fora de área diferenciada"              = "fora",
  "Área de assentamento"                          = "asset",
  "Terra indígena"                                = "indig",
  "Área com comunidade remanescente de quilombos" = "quilom",
  "Área com povos e comunidades tradicionais"     = "trad"
)

# ── Função construtora principal ──────────────────────────────────────────────

construir_tabela <- function(base, g_vars) {
  
  message(">>> Construindo: ", paste(g_vars, collapse = " × "))
  
  dir <- f_dir(base)
  ind <- f_ind_mun(base)
  tot <- bind_rows(dir, ind)        # dir + ind_mun (disjuntos em TP_DEPENDENCIA)
  
  # helpers internos que fecham sobre g_vars
  agg <- function(df)
    group_by(df, across(all_of(g_vars)))
  
  agg_loc <- function(df, qt_col, pref) {
    df |>
      filter(!is.na(TP_LOCALIZACAO)) |>
      mutate(loc = loc_map[TP_LOCALIZACAO]) |>
      group_by(across(all_of(c(g_vars, "loc")))) |>
      summarise(mat = sum(.data[[qt_col]], na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = loc, values_from = mat,
                  names_prefix = paste0(pref, "_"), values_fill = 0)
  }
  
  agg_locdif <- function(df, qt_col, pref) {
    df |>
      filter(!is.na(TP_LOCALIZACAO_DIFERENCIADA)) |>
      mutate(ld = loc_dif_map[TP_LOCALIZACAO_DIFERENCIADA]) |>
      filter(!is.na(ld)) |>
      group_by(across(all_of(c(g_vars, "ld")))) |>
      summarise(mat = sum(.data[[qt_col]], na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = ld, values_from = mat,
                  names_prefix = paste0(pref, "_"), values_fill = 0)
  }
  
  # ── A. Contagem de escolas ────────────────────────────────────────────────
  message("  [A] Escolas...")
  
  bloco_a <- agg(dir) |>
    summarise(n_esc_dir = n_distinct(CO_ENTIDADE), .groups = "drop") |>
    left_join(
      agg(ind |> filter(TP_SITUACAO_FUNCIONAMENTO == "1",
                        !is.na(QT_MAT_BAS), QT_MAT_BAS != 0)) |>
        summarise(n_esc_ind_mun = n_distinct(CO_ENTIDADE), .groups = "drop"),
      by = g_vars
    ) |>
    left_join(
      f_ind_mun_est(base) |>
        filter(TP_SITUACAO_FUNCIONAMENTO == "1",
               !is.na(QT_MAT_BAS), QT_MAT_BAS != 0) |>
        group_by(across(all_of(g_vars))) |>
        summarise(n_esc_ind_mun_est = n_distinct(CO_ENTIDADE), .groups = "drop"),
      by = g_vars
    )
  
  # ── B. Matrículas educação básica ─────────────────────────────────────────
  message("  [B] Matrículas EB...")
  
  bloco_b <- agg(dir) |>
    summarise(mat_bas_dir = sum(QT_MAT_BAS, na.rm = TRUE), .groups = "drop") |>
    left_join(
      agg(ind) |>
        summarise(mat_bas_ind = sum(QT_MAT_BAS, na.rm = TRUE), .groups = "drop"),
      by = g_vars
    ) |>
    mutate(mat_bas_tot = mat_bas_dir + replace_na(mat_bas_ind, 0))
  
  # ── C. Matrículas educação infantil ──────────────────────────────────────
  message("  [C] Matrículas EI...")
  
  ei_seg <- function(df, suf) {             # [N8] soma separada evita NA propagation
    agg(df) |>
      summarise(
        !!paste0("mat_ei_dir",    "")[1] := NULL,   # placeholder para renomear dinamicamente
        .groups = "drop"
      )
    agg(df) |>
      summarise(
        "{suf}_ei"      := sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
        "{suf}_ei_ti"   := sum(QT_MAT_INF_INT,             na.rm=TRUE),
        "{suf}_cre"     := sum(QT_MAT_INF_CRE,             na.rm=TRUE),
        "{suf}_cre_ti"  := sum(QT_MAT_INF_CRE_INT,         na.rm=TRUE),
        "{suf}_pre"     := sum(QT_MAT_INF_PRE,             na.rm=TRUE),
        "{suf}_pre_ti"  := sum(QT_MAT_INF_PRE_INT,         na.rm=TRUE),
        .groups = "drop"
      )
  }
  
  # Abordagem direta (mais legível que glue dentro de summarise):
  c_dir <- agg(dir) |>
    summarise(
      mat_ei_dir     = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
      mat_ei_ti_dir  = sum(QT_MAT_INF_INT,     na.rm=TRUE),
      mat_cre_dir    = sum(QT_MAT_INF_CRE,     na.rm=TRUE),
      mat_cre_ti_dir = sum(QT_MAT_INF_CRE_INT, na.rm=TRUE),
      mat_pre_dir    = sum(QT_MAT_INF_PRE,     na.rm=TRUE),
      mat_pre_ti_dir = sum(QT_MAT_INF_PRE_INT, na.rm=TRUE),
      .groups = "drop"
    )
  
  c_ind <- agg(ind) |>
    summarise(
      mat_ei_ind     = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
      mat_ei_ti_ind  = sum(QT_MAT_INF_INT,     na.rm=TRUE),
      mat_cre_ind    = sum(QT_MAT_INF_CRE,     na.rm=TRUE),
      mat_cre_ti_ind = sum(QT_MAT_INF_CRE_INT, na.rm=TRUE),
      mat_pre_ind    = sum(QT_MAT_INF_PRE,     na.rm=TRUE),
      mat_pre_ti_ind = sum(QT_MAT_INF_PRE_INT, na.rm=TRUE),
      .groups = "drop"
    )
  
  bloco_c <- left_join(c_dir, c_ind, by = g_vars) |>
    mutate(
      mat_ei_tot    = mat_ei_dir    + replace_na(mat_ei_ind,    0),
      mat_ei_ti_tot = mat_ei_ti_dir + replace_na(mat_ei_ti_ind, 0),
      mat_cre_tot   = mat_cre_dir   + replace_na(mat_cre_ind,   0),
      mat_pre_tot   = mat_pre_dir   + replace_na(mat_pre_ind,   0)
    )
  
  # ── D. EI por localização (urb / rur) ────────────────────────────────────
  message("  [D] EI por localização...")
  
  bloco_d <-
    agg_loc(dir, "QT_MAT_INF_CRE", "mat_cre_dir_loc") |>
    left_join(agg_loc(ind, "QT_MAT_INF_CRE", "mat_cre_ind_loc"), by = g_vars) |>
    left_join(agg_loc(dir, "QT_MAT_INF_PRE", "mat_pre_dir_loc"), by = g_vars) |>  # [N5]
    left_join(agg_loc(ind, "QT_MAT_INF_PRE", "mat_pre_ind_loc"), by = g_vars)     # [N5]
  
  # ── E. EI por localização diferenciada (total dir + ind) ─────────────────
  message("  [E] EI por loc. diferenciada...")
  
  bloco_e <-
    agg_locdif(tot, "QT_MAT_INF_CRE", "mat_cre_tot_ld") |>
    left_join(agg_locdif(tot, "QT_MAT_INF_PRE", "mat_pre_tot_ld"), by = g_vars)
  
  # ── F. Docentes por turma  [N7] ───────────────────────────────────────────
  message("  [F] Docentes/turma...")
  
  doc_tur <- function(df, suf) {
    agg(df) |>
      summarise(
        sd_cre = sum(QT_DOC_INF_CRE, na.rm = TRUE),
        st_cre = sum(QT_TUR_INF_CRE, na.rm = TRUE),
        sd_pre = sum(QT_DOC_INF_PRE, na.rm = TRUE),
        st_pre = sum(QT_TUR_INF_PRE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        !!paste0("doc_tur_cre_", suf) := if_else(st_cre > 0, sd_cre / st_cre, NA_real_),
        !!paste0("doc_tur_pre_", suf) := if_else(st_pre > 0, sd_pre / st_pre, NA_real_)
      ) |>
      select(-sd_cre, -st_cre, -sd_pre, -st_pre)
  }
  
  bloco_f <- doc_tur(dir, "dir") |> left_join(doc_tur(ind, "ind"), by = g_vars)
  
  # ── G. Matrículas EI por instituição (IN_INF = NA em 2025 → razão NA)  ───
  message("  [G] Matrículas EI por escola...")
  
  mat_esc <- function(df, suf) {
    agg(df) |>
      summarise(
        s_ei  = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
        n_inf = sum(as.integer(!is.na(IN_INF) & IN_INF == "1")),
        .groups = "drop"
      ) |>
      mutate(!!paste0("mat_ei_esc_", suf) :=
               if_else(n_inf > 0, s_ei / n_inf, NA_real_)) |>
      select(-s_ei, -n_inf)
  }
  
  bloco_g <- mat_esc(dir, "dir") |> left_join(mat_esc(ind, "ind"), by = g_vars)
  
  # ── H. Infraestrutura — matrículas EI  [N4] ──────────────────────────────
  message("  [H] Infraestrutura EI...")
  
  infra <- function(df, suf) {
    df |>
      mutate(
        # [N8] rowSums trata NA de CRE ou PRE como 0 (school parcial)
        mat_ei = rowSums(cbind(QT_MAT_INF_CRE, QT_MAT_INF_PRE), na.rm = TRUE),
        i_av   = !is.na(IN_AREA_VERDE)              & IN_AREA_VERDE              == "1",
        i_ban  = !is.na(IN_BANHEIRO_EI)             & IN_BANHEIRO_EI             == "1",
        i_jog  = !is.na(IN_MATERIAL_PED_JOGOS)      & IN_MATERIAL_PED_JOGOS      == "1",
        i_art  = !is.na(IN_MATERIAL_PED_ARTISTICAS) & IN_MATERIAL_PED_ARTISTICAS == "1",
        i_4    = i_av & i_ban & i_jog & i_art,          # todos os 4
        i_s4   = !i_av & !i_ban & !i_jog & !i_art       # nenhum dos 4
      ) |>
      group_by(across(all_of(g_vars))) |>
      summarise(
        !!paste0("mat_ei_av_",  suf) := sum(mat_ei * i_av,  na.rm = TRUE),
        !!paste0("mat_ei_ban_", suf) := sum(mat_ei * i_ban, na.rm = TRUE),
        !!paste0("mat_ei_jog_", suf) := sum(mat_ei * i_jog, na.rm = TRUE),
        !!paste0("mat_ei_art_", suf) := sum(mat_ei * i_art, na.rm = TRUE),
        !!paste0("mat_ei_4_",   suf) := sum(mat_ei * i_4,   na.rm = TRUE),
        !!paste0("mat_ei_s4_",  suf) := sum(mat_ei * i_s4,  na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  bloco_h <- infra(dir, "dir") |> left_join(infra(ind, "ind"), by = g_vars)
  
  # ── I. Outros segmentos (EF / EM / EJA / EP / EE) ────────────────────────
  message("  [I] Fund/EM/EJA/EP/EE...")
  
  bloco_i <- agg(dir) |>
    summarise(
      mat_fund_ai_dir = sum(QT_MAT_FUND_AI, na.rm = TRUE),
      mat_fund_af_dir = sum(QT_MAT_FUND_AF, na.rm = TRUE),
      mat_med_dir     = sum(QT_MAT_MED,     na.rm = TRUE),
      mat_eja_dir     = sum(QT_MAT_EJA,     na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(agg_locdif(tot, "QT_MAT_PROF", "mat_prof_tot_ld"), by = g_vars) |>
    left_join(agg_locdif(tot, "QT_MAT_ESP",  "mat_esp_tot_ld"),  by = g_vars)
  
  # ── J. Cor / raça ─────────────────────────────────────────────────────────
  message("  [J] Cor/raça...")
  
  raca_cols <- c("QT_MAT_BAS_ND",   "QT_MAT_BAS_BRANCA", "QT_MAT_BAS_PRETA",
                 "QT_MAT_BAS_PARDA","QT_MAT_BAS_AMARELA","QT_MAT_BAS_INDIGENA")
  raca_suf  <- c("nd","branca","preta","parda","amarela","indigena")
  
  raca <- function(df, seg) {
    agg(df) |>
      summarise(across(all_of(raca_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") |>
      rename_with(~ paste0("mat_bas_", raca_suf, "_", seg), all_of(raca_cols))
  }
  
  bloco_j <- raca(dir, "dir") |> left_join(raca(ind, "ind"), by = g_vars)
  
  # ── K. EB por localização diferenciada  [N6] ─────────────────────────────
  message("  [K] EB por loc. diferenciada...")
  
  bloco_k <-
    agg_locdif(dir, "QT_MAT_BAS", "mat_bas_dir_ld") |>
    left_join(agg_locdif(ind, "QT_MAT_BAS", "mat_bas_ind_ld"), by = g_vars)  # [N6]
  
  # ── JOIN FINAL ────────────────────────────────────────────────────────────
  message("  [JOIN] Montando...")
  
  tabela <- list(bloco_a, bloco_b, bloco_c, bloco_d, bloco_e,
                 bloco_f, bloco_g, bloco_h, bloco_i, bloco_j, bloco_k) |>
    reduce(left_join, by = g_vars)
  
  message("  [ok] ", format(nrow(tabela), big.mark=","),
          " linhas | ", ncol(tabela), " colunas")
  tabela
}

# ── Execução para os três bancos ──────────────────────────────────────────────

g_mun  <- c("NU_ANO_CENSO", "SG_UF", "NO_MUNICIPIO", "CO_MUNICIPIO")
g_dist <- c("NU_ANO_CENSO", "SG_UF", "NO_MUNICIPIO", "CO_MUNICIPIO",
            "NO_MICRORREGIAO", "CO_MICRORREGIAO", "NO_DISTRITO", "CO_DISTRITO")

message("\n>>> TABELA: Todos os municípios")
tab_municipios <- construir_tabela(censo_completo, g_mun)

message("\n>>> TABELA: Capitais")
tab_capitais   <- construir_tabela(censo_capitais, g_mun)

message("\n>>> TABELA: São Paulo — por distrito")
tab_saopaulo   <- construir_tabela(censo_saopaulo, g_dist)

# ── Suplemento SP: creche / pré-escola por escola-tipo (IN_INF_CRE / PRE) ───
# IN_INF_CRE e IN_INF_PRE são NA estrutural em 2025 (mudança de instrumento)

message("\n>>> Suplemento SP: escola-tipo por distrito...")

sp_tipo <- local({
  dir <- f_dir(censo_saopaulo)
  ind <- f_ind_mun(censo_saopaulo)
  
  list(
    dir |>
      filter(!is.na(IN_INF_CRE), IN_INF_CRE == "1",
             TP_SITUACAO_FUNCIONAMENTO == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_cre_dir = n_distinct(CO_ENTIDADE), .groups = "drop"),
    
    ind |>
      filter(!is.na(IN_INF_CRE), IN_INF_CRE == "1",
             TP_SITUACAO_FUNCIONAMENTO == "1",
             !is.na(QT_MAT_BAS), QT_MAT_BAS != 0) |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_cre_ind = n_distinct(CO_ENTIDADE), .groups = "drop"),
    
    dir |>
      filter(!is.na(IN_INF_PRE), IN_INF_PRE == "1",
             TP_SITUACAO_FUNCIONAMENTO == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_pre_dir = n_distinct(CO_ENTIDADE), .groups = "drop"),
    
    ind |>
      filter(!is.na(IN_INF_PRE), IN_INF_PRE == "1",
             TP_SITUACAO_FUNCIONAMENTO == "1",
             !is.na(QT_MAT_BAS), QT_MAT_BAS != 0) |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_pre_ind = n_distinct(CO_ENTIDADE), .groups = "drop")
  ) |>
    reduce(left_join, by = g_dist)
})

tab_saopaulo <- left_join(tab_saopaulo, sp_tipo, by = g_dist)

# ── Validação cruzada ─────────────────────────────────────────────────────────
message("\n=== DIMENSÕES ===")
message("tab_municipios : ",
        format(nrow(tab_municipios), big.mark=","), " × ", ncol(tab_municipios))
message("tab_capitais   : ",
        format(nrow(tab_capitais),   big.mark=","), " × ", ncol(tab_capitais))
message("tab_saopaulo   : ",
        format(nrow(tab_saopaulo),   big.mark=","), " × ", ncol(tab_saopaulo))

# Check: soma de mat_bas_dir por ano deve ser idêntica no tab e no censo_completo
message("\n=== CHECK mat_bas_dir: tab_municipios × censo_completo ===")
check_ref <- censo_completo |>
  filter(TP_DEPENDENCIA == "3") |>
  group_by(NU_ANO_CENSO) |>
  summarise(ref = sum(QT_MAT_BAS, na.rm = TRUE), .groups = "drop")

check_tab <- tab_municipios |>
  group_by(NU_ANO_CENSO) |>
  summarise(tab = sum(mat_bas_dir, na.rm = TRUE), .groups = "drop")

left_join(check_ref, check_tab, by = "NU_ANO_CENSO") |>
  mutate(dif = ref - tab, ok = dif == 0) |>
  print(n = Inf)



# Garante que as 3 tabelas tenham as mesmas colunas (preenche ausentes com 0 ou NA)
completar_colunas <- function(tabela, referencia) {
  cols_ausentes <- setdiff(names(referencia), names(tabela))
  if (length(cols_ausentes) == 0) return(tabela)
  message("  Completando ", length(cols_ausentes), " colunas ausentes: ",
          paste(cols_ausentes, collapse = ", "))
  for (col in cols_ausentes) tabela[[col]] <- 0
  select(tabela, all_of(names(referencia)))  # reordena igual à referência
}

tab_capitais  <- completar_colunas(tab_capitais,  tab_municipios)
tab_saopaulo  <- completar_colunas(tab_saopaulo,  tab_municipios)

message("tab_capitais  pós-completar: ", ncol(tab_capitais),  " colunas")
message("tab_saopaulo  pós-completar: ", ncol(tab_saopaulo),  " colunas")


# Quais capitais esperadas não estão no banco?
capitais_esperadas <- tibble(
  CO_MUNICIPIO = c(
    1100205, 1302603, 1200401, 1600303, 1721000, 1400100,
    1501402, 2111300, 2211001, 2304400, 2408102, 2507507,
    2611606, 2704302, 2800308, 2927408, 3106200, 3205309,
    3304557, 3550308, 4106902, 4205407, 4314902, 5002704,
    5103403, 5208707, 5300108
  )
)

tab_capitais |>
  distinct(CO_MUNICIPIO) |>
  anti_join(capitais_esperadas, by = "CO_MUNICIPIO")

capitais_esperadas |>
  anti_join(distinct(tab_capitais, CO_MUNICIPIO), by = "CO_MUNICIPIO")




# ============================================================
# BLOCO 4 — TABELAS ANALÍTICAS (versão final corrigida)
# + VERIFICAÇÃO ALEATÓRIA contra banco bruto
# Pré-requisito: censo_completo, censo_capitais, censo_saopaulo em memória
# ============================================================

library(purrr); library(dplyr); library(tidyr); library(stringr)

# ── Mapas de rótulos ──────────────────────────────────────────────────────────
loc_map <- c("1" = "urb", "2" = "rur")

loc_dif_map <- c(
  "Escola fora de área diferenciada"              = "fora",
  "Área de assentamento"                          = "asset",
  "Terra indígena"                                = "indig",
  "Área com comunidade remanescente de quilombos" = "quilom",
  "Área com povos e comunidades tradicionais"     = "trad"
)

# ── Filtros base (versão corrigida com TP_SITUACAO_FUNCIONAMENTO == "1") ───────

f_dir <- function(df)
  filter(df,
         TP_DEPENDENCIA            == "3",
         TP_SITUACAO_FUNCIONAMENTO == "1")

f_ind_mun <- function(df)
  filter(df,
         TP_DEPENDENCIA            == "4",
         TP_SITUACAO_FUNCIONAMENTO == "1",
         TP_PODER_PUBLICO_PARCERIA == 1L)

f_ind_mun_est <- function(df)
  filter(df,
         TP_DEPENDENCIA            == "4",
         TP_SITUACAO_FUNCIONAMENTO == "1",
         TP_PODER_PUBLICO_PARCERIA == 3L)

# DF: rede estadual equiparada à rede direta municipal
f_dir_capitais <- function(df)
  filter(df,
         TP_SITUACAO_FUNCIONAMENTO == "1",
         TP_DEPENDENCIA == "3" |
           (TP_DEPENDENCIA == "2" & CO_MUNICIPIO == 5300108))

# ── Utilitário: completar colunas ausentes ────────────────────────────────────
completar_colunas <- function(tabela, referencia) {
  cols_ausentes <- setdiff(names(referencia), names(tabela))
  cols_extras   <- setdiff(names(tabela),     names(referencia))
  if (length(cols_ausentes) > 0) {
    message("  Completando ", length(cols_ausentes), " colunas: ",
            paste(cols_ausentes, collapse = ", "))
    for (col in cols_ausentes) tabela[[col]] <- 0
  }
  if (length(cols_extras) > 0)
    message("  Preservando ", length(cols_extras),
            " extras: ", paste(cols_extras, collapse = ", "))
  select(tabela, all_of(c(names(referencia), cols_extras)))
}

# ── Função construtora principal ──────────────────────────────────────────────
construir_tabela <- function(base, g_vars,
                             filtro_dir = function(df)
                               filter(df,
                                      TP_DEPENDENCIA            == "3",
                                      TP_SITUACAO_FUNCIONAMENTO == "1")) {
  
  message(">>> Construindo: ", paste(g_vars, collapse = " × "))
  
  dir <- filtro_dir(base)
  ind <- f_ind_mun(base)
  tot <- bind_rows(dir, ind)
  
  agg <- function(df) group_by(df, across(all_of(g_vars)))
  
  agg_loc <- function(df, qt_col, pref) {
    df |>
      filter(!is.na(TP_LOCALIZACAO)) |>
      mutate(loc = loc_map[TP_LOCALIZACAO]) |>
      group_by(across(all_of(c(g_vars, "loc")))) |>
      summarise(mat = sum(.data[[qt_col]], na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = loc, values_from = mat,
                  names_prefix = paste0(pref, "_"), values_fill = 0)
  }
  
  agg_locdif <- function(df, qt_col, pref) {
    df |>
      filter(!is.na(TP_LOCALIZACAO_DIFERENCIADA)) |>
      mutate(ld = loc_dif_map[TP_LOCALIZACAO_DIFERENCIADA]) |>
      filter(!is.na(ld)) |>
      group_by(across(all_of(c(g_vars, "ld")))) |>
      summarise(mat = sum(.data[[qt_col]], na.rm = TRUE), .groups = "drop") |>
      pivot_wider(names_from = ld, values_from = mat,
                  names_prefix = paste0(pref, "_"), values_fill = 0)
  }
  
  # A. Escolas
  message("  [A] Escolas...")
  bloco_a <- agg(dir) |>
    summarise(n_esc_dir = n_distinct(CO_ENTIDADE), .groups = "drop") |>
    left_join(
      agg(ind) |>
        summarise(n_esc_ind_mun = n_distinct(CO_ENTIDADE), .groups = "drop"),
      by = g_vars
    ) |>
    left_join(
      f_ind_mun_est(base) |>
        group_by(across(all_of(g_vars))) |>
        summarise(n_esc_ind_mun_est = n_distinct(CO_ENTIDADE), .groups = "drop"),
      by = g_vars
    )
  
  # B. Matrículas EB
  message("  [B] Matrículas EB...")
  bloco_b <- agg(dir) |>
    summarise(mat_bas_dir = sum(QT_MAT_BAS, na.rm = TRUE), .groups = "drop") |>
    left_join(
      agg(ind) |>
        summarise(mat_bas_ind = sum(QT_MAT_BAS, na.rm = TRUE), .groups = "drop"),
      by = g_vars
    ) |>
    mutate(mat_bas_tot = mat_bas_dir + replace_na(mat_bas_ind, 0))
  
  # C. Matrículas EI
  message("  [C] Matrículas EI...")
  c_dir <- agg(dir) |>
    summarise(
      mat_ei_dir     = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
      mat_ei_ti_dir  = sum(QT_MAT_INF_INT,     na.rm=TRUE),
      mat_cre_dir    = sum(QT_MAT_INF_CRE,     na.rm=TRUE),
      mat_cre_ti_dir = sum(QT_MAT_INF_CRE_INT, na.rm=TRUE),
      mat_pre_dir    = sum(QT_MAT_INF_PRE,     na.rm=TRUE),
      mat_pre_ti_dir = sum(QT_MAT_INF_PRE_INT, na.rm=TRUE),
      .groups = "drop")
  
  c_ind <- agg(ind) |>
    summarise(
      mat_ei_ind     = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
      mat_ei_ti_ind  = sum(QT_MAT_INF_INT,     na.rm=TRUE),
      mat_cre_ind    = sum(QT_MAT_INF_CRE,     na.rm=TRUE),
      mat_cre_ti_ind = sum(QT_MAT_INF_CRE_INT, na.rm=TRUE),
      mat_pre_ind    = sum(QT_MAT_INF_PRE,     na.rm=TRUE),
      mat_pre_ti_ind = sum(QT_MAT_INF_PRE_INT, na.rm=TRUE),
      .groups = "drop")
  
  bloco_c <- left_join(c_dir, c_ind, by = g_vars) |>
    mutate(
      mat_ei_tot    = mat_ei_dir    + replace_na(mat_ei_ind,    0),
      mat_ei_ti_tot = mat_ei_ti_dir + replace_na(mat_ei_ti_ind, 0),
      mat_cre_tot   = mat_cre_dir   + replace_na(mat_cre_ind,   0),
      mat_pre_tot   = mat_pre_dir   + replace_na(mat_pre_ind,   0))
  
  # D. EI por localização
  message("  [D] EI por localização...")
  bloco_d <-
    agg_loc(dir, "QT_MAT_INF_CRE", "mat_cre_dir_loc") |>
    left_join(agg_loc(ind, "QT_MAT_INF_CRE", "mat_cre_ind_loc"), by = g_vars) |>
    left_join(agg_loc(dir, "QT_MAT_INF_PRE", "mat_pre_dir_loc"), by = g_vars) |>
    left_join(agg_loc(ind, "QT_MAT_INF_PRE", "mat_pre_ind_loc"), by = g_vars)
  
  # E. EI por loc. diferenciada
  message("  [E] EI por loc. diferenciada...")
  bloco_e <-
    agg_locdif(tot, "QT_MAT_INF_CRE", "mat_cre_tot_ld") |>
    left_join(agg_locdif(tot, "QT_MAT_INF_PRE", "mat_pre_tot_ld"), by = g_vars)
  
  # F. Docentes/turma
  message("  [F] Docentes/turma...")
  doc_tur <- function(df, suf) {
    agg(df) |>
      summarise(
        sd_cre = sum(QT_DOC_INF_CRE, na.rm = TRUE),
        st_cre = sum(QT_TUR_INF_CRE, na.rm = TRUE),
        sd_pre = sum(QT_DOC_INF_PRE, na.rm = TRUE),
        st_pre = sum(QT_TUR_INF_PRE, na.rm = TRUE),
        .groups = "drop") |>
      mutate(
        !!paste0("doc_tur_cre_", suf) := if_else(st_cre > 0, sd_cre/st_cre, NA_real_),
        !!paste0("doc_tur_pre_", suf) := if_else(st_pre > 0, sd_pre/st_pre, NA_real_)) |>
      select(-sd_cre, -st_cre, -sd_pre, -st_pre)
  }
  bloco_f <- doc_tur(dir, "dir") |> left_join(doc_tur(ind, "ind"), by = g_vars)
  
  # G. Mat. EI por escola
  message("  [G] Matrículas EI por escola...")
  mat_esc <- function(df, suf) {
    agg(df) |>
      summarise(
        s_ei  = sum(QT_MAT_INF_CRE, na.rm=TRUE) + sum(QT_MAT_INF_PRE, na.rm=TRUE),
        n_inf = sum(as.integer(!is.na(IN_INF) & IN_INF == "1")),
        .groups = "drop") |>
      mutate(!!paste0("mat_ei_esc_", suf) :=
               if_else(n_inf > 0, s_ei/n_inf, NA_real_)) |>
      select(-s_ei, -n_inf)
  }
  bloco_g <- mat_esc(dir, "dir") |> left_join(mat_esc(ind, "ind"), by = g_vars)
  
  # H. Infraestrutura
  message("  [H] Infraestrutura EI...")
  infra <- function(df, suf) {
    df |>
      mutate(
        mat_ei = rowSums(cbind(QT_MAT_INF_CRE, QT_MAT_INF_PRE), na.rm = TRUE),
        i_av   = !is.na(IN_AREA_VERDE)              & IN_AREA_VERDE              == "1",
        i_ban  = !is.na(IN_BANHEIRO_EI)             & IN_BANHEIRO_EI             == "1",
        i_jog  = !is.na(IN_MATERIAL_PED_JOGOS)      & IN_MATERIAL_PED_JOGOS      == "1",
        i_art  = !is.na(IN_MATERIAL_PED_ARTISTICAS) & IN_MATERIAL_PED_ARTISTICAS == "1",
        i_4    = i_av & i_ban & i_jog & i_art,
        i_s4   = !i_av & !i_ban & !i_jog & !i_art) |>
      group_by(across(all_of(g_vars))) |>
      summarise(
        !!paste0("mat_ei_av_",  suf) := sum(mat_ei * i_av,  na.rm = TRUE),
        !!paste0("mat_ei_ban_", suf) := sum(mat_ei * i_ban, na.rm = TRUE),
        !!paste0("mat_ei_jog_", suf) := sum(mat_ei * i_jog, na.rm = TRUE),
        !!paste0("mat_ei_art_", suf) := sum(mat_ei * i_art, na.rm = TRUE),
        !!paste0("mat_ei_4_",   suf) := sum(mat_ei * i_4,   na.rm = TRUE),
        !!paste0("mat_ei_s4_",  suf) := sum(mat_ei * i_s4,  na.rm = TRUE),
        .groups = "drop")
  }
  bloco_h <- infra(dir, "dir") |> left_join(infra(ind, "ind"), by = g_vars)
  
  # I. Outros segmentos
  message("  [I] Fund/EM/EJA/EP/EE...")
  bloco_i <- agg(dir) |>
    summarise(
      mat_fund_ai_dir = sum(QT_MAT_FUND_AI, na.rm = TRUE),
      mat_fund_af_dir = sum(QT_MAT_FUND_AF, na.rm = TRUE),
      mat_med_dir     = sum(QT_MAT_MED,     na.rm = TRUE),
      mat_eja_dir     = sum(QT_MAT_EJA,     na.rm = TRUE),
      .groups = "drop") |>
    left_join(agg_locdif(tot, "QT_MAT_PROF", "mat_prof_tot_ld"), by = g_vars) |>
    left_join(agg_locdif(tot, "QT_MAT_ESP",  "mat_esp_tot_ld"),  by = g_vars)
  
  # J. Cor/raça
  message("  [J] Cor/raça...")
  raca_cols <- c("QT_MAT_BAS_ND","QT_MAT_BAS_BRANCA","QT_MAT_BAS_PRETA",
                 "QT_MAT_BAS_PARDA","QT_MAT_BAS_AMARELA","QT_MAT_BAS_INDIGENA")
  raca_suf  <- c("nd","branca","preta","parda","amarela","indigena")
  raca <- function(df, seg) {
    agg(df) |>
      summarise(across(all_of(raca_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
      rename_with(~ paste0("mat_bas_", raca_suf, "_", seg), all_of(raca_cols))
  }
  bloco_j <- raca(dir, "dir") |> left_join(raca(ind, "ind"), by = g_vars)
  
  # K. EB por loc. diferenciada
  message("  [K] EB por loc. diferenciada...")
  bloco_k <-
    agg_locdif(dir, "QT_MAT_BAS", "mat_bas_dir_ld") |>
    left_join(agg_locdif(ind, "QT_MAT_BAS", "mat_bas_ind_ld"), by = g_vars)
  
  # JOIN
  message("  [JOIN] Montando...")
  tabela <- list(bloco_a, bloco_b, bloco_c, bloco_d, bloco_e,
                 bloco_f, bloco_g, bloco_h, bloco_i, bloco_j, bloco_k) |>
    reduce(left_join, by = g_vars)
  
  message("  [ok] ", format(nrow(tabela), big.mark=","),
          " linhas | ", ncol(tabela), " colunas")
  tabela
}

# ── Chaves de agrupamento ─────────────────────────────────────────────────────
g_mun  <- c("NU_ANO_CENSO","SG_UF","NO_MUNICIPIO","CO_MUNICIPIO")
g_dist <- c("NU_ANO_CENSO","SG_UF","NO_MUNICIPIO","CO_MUNICIPIO",
            "NO_MICRORREGIAO","CO_MICRORREGIAO","NO_DISTRITO","CO_DISTRITO")

# ── Construção das tabelas ────────────────────────────────────────────────────
message("\n>>> TABELA: Todos os municípios")
tab_municipios <- construir_tabela(censo_completo, g_mun)

message("\n>>> TABELA: Capitais")
tab_capitais   <- construir_tabela(censo_capitais, g_mun,
                                   filtro_dir = f_dir_capitais)

message("\n>>> TABELA: São Paulo — por distrito")
tab_saopaulo   <- construir_tabela(censo_saopaulo, g_dist)

# ── Suplemento SP ─────────────────────────────────────────────────────────────
message("\n>>> Suplemento SP...")
sp_tipo <- local({
  dir_sp <- f_dir(censo_saopaulo)
  ind_sp <- f_ind_mun(censo_saopaulo)
  list(
    dir_sp |> filter(!is.na(IN_INF_CRE), IN_INF_CRE == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_cre_dir = n_distinct(CO_ENTIDADE), .groups = "drop"),
    ind_sp |> filter(!is.na(IN_INF_CRE), IN_INF_CRE == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_cre_ind = n_distinct(CO_ENTIDADE), .groups = "drop"),
    dir_sp |> filter(!is.na(IN_INF_PRE), IN_INF_PRE == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_pre_dir = n_distinct(CO_ENTIDADE), .groups = "drop"),
    ind_sp |> filter(!is.na(IN_INF_PRE), IN_INF_PRE == "1") |>
      group_by(across(all_of(g_dist))) |>
      summarise(n_pre_ind = n_distinct(CO_ENTIDADE), .groups = "drop")
  ) |> reduce(left_join, by = g_dist)
})
tab_saopaulo <- left_join(tab_saopaulo, sp_tipo, by = g_dist)

# ── Completar + reordenar SP ──────────────────────────────────────────────────
tab_capitais <- completar_colunas(tab_capitais, tab_municipios)
tab_saopaulo <- completar_colunas(tab_saopaulo, tab_municipios)
tab_saopaulo <- tab_saopaulo |> select(all_of(g_dist), everything())

# ── Zerar NAs em contagens/_ind (exceto razões) ───────────────────────────────
cols_zerar <- tab_municipios |>
  select(matches("_ind")) |>
  select(-matches("doc_tur_|mat_ei_esc_")) |>
  names()

for (tbl_nm in c("tab_municipios","tab_capitais","tab_saopaulo")) {
  tbl <- get(tbl_nm)
  cols <- intersect(cols_zerar, names(tbl))
  assign(tbl_nm, mutate(tbl, across(all_of(cols), ~ replace_na(., 0))))
}

# ── Salvar ────────────────────────────────────────────────────────────────────
message("\n>>> Salvando...")
saveRDS(tab_municipios, "tab_municipios_2015_2025.rds")
saveRDS(tab_capitais,   "tab_capitais_2015_2025.rds")
saveRDS(tab_saopaulo,   "tab_saopaulo_2015_2025.rds")

fs <- file.size(c("tab_municipios_2015_2025.rds",
                  "tab_capitais_2015_2025.rds",
                  "tab_saopaulo_2015_2025.rds"))
names(fs) <- c("municipios","capitais","sao_paulo")
message("Tamanhos (MB):"); print(round(fs/1024^2, 1))

# ============================================================
# BLOCO 5 — VERIFICAÇÃO ALEATÓRIA
# Compara banco bruto (censo_completo) vs. tabelas analíticas
# ============================================================

set.seed(42)  # reprodutibilidade

verificar_amostra <- function(n_municipios = 10, anos = c(2019, 2022, 2024)) {
  
  # Sortear municípios (exceto DF — tratamento especial)
  municipios_pool <- tab_municipios |>
    filter(NU_ANO_CENSO %in% as.character(anos),
           CO_MUNICIPIO != 5300108) |>
    distinct(CO_MUNICIPIO, NO_MUNICIPIO) |>
    slice_sample(n = n_municipios)
  
  message("Municípios sorteados: ",
          paste(municipios_pool$NO_MUNICIPIO, collapse = " | "))
  
  # Construir referência do banco bruto
  ref <- censo_completo |>
    filter(CO_MUNICIPIO %in% municipios_pool$CO_MUNICIPIO,
           NU_ANO_CENSO  %in% anos) |>
    group_by(NU_ANO_CENSO, CO_MUNICIPIO, NO_MUNICIPIO) |>
    summarise(
      # Escolas
      ref_n_esc_dir     = sum(TP_DEPENDENCIA == "3" &
                                TP_SITUACAO_FUNCIONAMENTO == "1"),
      ref_n_esc_ind     = sum(TP_DEPENDENCIA == "4" &
                                TP_SITUACAO_FUNCIONAMENTO == "1" &
                                TP_PODER_PUBLICO_PARCERIA == 1L),
      # Matrículas EB
      ref_mat_bas_dir   = sum(QT_MAT_BAS[TP_DEPENDENCIA == "3" &
                                           TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
      ref_mat_bas_ind   = sum(QT_MAT_BAS[TP_DEPENDENCIA == "4" &
                                           TP_SITUACAO_FUNCIONAMENTO == "1" &
                                           TP_PODER_PUBLICO_PARCERIA == 1L], na.rm=TRUE),
      # Matrículas creche
      ref_mat_cre_dir   = sum(QT_MAT_INF_CRE[TP_DEPENDENCIA == "3" &
                                               TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
      ref_mat_cre_ind   = sum(QT_MAT_INF_CRE[TP_DEPENDENCIA == "4" &
                                               TP_SITUACAO_FUNCIONAMENTO == "1" &
                                               TP_PODER_PUBLICO_PARCERIA == 1L], na.rm=TRUE),
      # Matrículas pré-escola
      ref_mat_pre_dir   = sum(QT_MAT_INF_PRE[TP_DEPENDENCIA == "3" &
                                               TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
      # Matrículas EF anos iniciais
      ref_mat_fund_ai   = sum(QT_MAT_FUND_AI[TP_DEPENDENCIA == "3" &
                                               TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
      # Matrículas EJA
      ref_mat_eja       = sum(QT_MAT_EJA[TP_DEPENDENCIA == "3" &
                                           TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
      .groups = "drop"
    ) |>
    mutate(NU_ANO_CENSO = as.character(NU_ANO_CENSO))
  
  # Extrair valores das tabelas analíticas
  tab <- tab_municipios |>
    filter(CO_MUNICIPIO %in% municipios_pool$CO_MUNICIPIO,
           NU_ANO_CENSO  %in% as.character(anos)) |>
    select(NU_ANO_CENSO, CO_MUNICIPIO, NO_MUNICIPIO,
           tab_n_esc_dir     = n_esc_dir,
           tab_n_esc_ind     = n_esc_ind_mun,
           tab_mat_bas_dir   = mat_bas_dir,
           tab_mat_bas_ind   = mat_bas_ind,
           tab_mat_cre_dir   = mat_cre_dir,
           tab_mat_cre_ind   = mat_cre_ind,
           tab_mat_pre_dir   = mat_pre_dir,
           tab_mat_fund_ai   = mat_fund_ai_dir,
           tab_mat_eja       = mat_eja_dir)
  
  # Comparar
  resultado <- ref |>
    left_join(tab, by = c("NU_ANO_CENSO","CO_MUNICIPIO","NO_MUNICIPIO")) |>
    mutate(
      ok_n_esc_dir   = ref_n_esc_dir   == tab_n_esc_dir,
      ok_n_esc_ind   = ref_n_esc_ind   == replace_na(tab_n_esc_ind, 0),
      ok_mat_bas_dir = ref_mat_bas_dir  == tab_mat_bas_dir,
      ok_mat_bas_ind = ref_mat_bas_ind  == replace_na(tab_mat_bas_ind, 0),
      ok_mat_cre_dir = ref_mat_cre_dir  == tab_mat_cre_dir,
      ok_mat_cre_ind = ref_mat_cre_ind  == replace_na(tab_mat_cre_ind, 0),
      ok_mat_pre_dir = ref_mat_pre_dir  == tab_mat_pre_dir,
      ok_mat_fund_ai = ref_mat_fund_ai  == tab_mat_fund_ai,
      ok_mat_eja     = ref_mat_eja      == tab_mat_eja,
      tudo_ok = ok_n_esc_dir & ok_n_esc_ind & ok_mat_bas_dir & ok_mat_bas_ind &
        ok_mat_cre_dir & ok_mat_cre_ind & ok_mat_pre_dir &
        ok_mat_fund_ai & ok_mat_eja
    )
  
  resultado
}

# ── Rodar verificação ─────────────────────────────────────────────────────────
message("\n=== VERIFICAÇÃO ALEATÓRIA — tab_municipios ===")
verif <- verificar_amostra(n_municipios = 15, anos = c(2015, 2019, 2022, 2024))

# Resumo por linha (município × ano)
message("\n--- Resultado por município × ano ---")
verif |>
  select(NU_ANO_CENSO, NO_MUNICIPIO,
         ok_n_esc_dir, ok_n_esc_ind,
         ok_mat_bas_dir, ok_mat_bas_ind,
         ok_mat_cre_dir, ok_mat_fund_ai,
         ok_mat_eja, tudo_ok) |>
  print(n = Inf, width = Inf)

# Resumo agregado
message("\n--- Taxa de acerto por indicador ---")
verif |>
  summarise(across(starts_with("ok_"), ~ paste0(sum(.), "/", n()))) |>
  pivot_longer(everything(), names_to = "indicador", values_to = "acertos") |>
  print(n = Inf)

# Falhas (se houver)
falhas <- verif |> filter(!tudo_ok)
if (nrow(falhas) == 0) {
  message("\n✅ Verificação concluída: TODOS os ", nrow(verif),
          " pares município × ano batem com o banco bruto.")
} else {
  message("\n⚠️  ", nrow(falhas), " divergências encontradas:")
  falhas |>
    select(NU_ANO_CENSO, NO_MUNICIPIO, starts_with("ok_")) |>
    print(n = Inf, width = Inf)
}

# ── Verificação adicional: São Paulo por distrito (2019 e 2024) ───────────────
message("\n=== VERIFICAÇÃO — São Paulo por distrito (anos: 2019, 2024) ===")

set.seed(99)
distritos_amostra <- tab_saopaulo |>
  filter(NU_ANO_CENSO %in% c("2019","2024")) |>
  distinct(CO_DISTRITO, NO_DISTRITO) |>
  slice_sample(n = 8)

message("Distritos sorteados: ",
        paste(distritos_amostra$NO_DISTRITO, collapse = " | "))

ref_sp <- censo_saopaulo |>
  filter(CO_DISTRITO  %in% distritos_amostra$CO_DISTRITO,
         NU_ANO_CENSO %in% c(2019, 2024)) |>
  group_by(NU_ANO_CENSO, CO_DISTRITO) |>
  summarise(
    ref_n_esc_dir   = sum(TP_DEPENDENCIA == "3" & TP_SITUACAO_FUNCIONAMENTO == "1"),
    ref_n_esc_ind   = sum(TP_DEPENDENCIA == "4" & TP_SITUACAO_FUNCIONAMENTO == "1" &
                            TP_PODER_PUBLICO_PARCERIA == 1L),
    ref_mat_bas_dir = sum(QT_MAT_BAS[TP_DEPENDENCIA == "3" &
                                       TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
    ref_mat_cre_dir = sum(QT_MAT_INF_CRE[TP_DEPENDENCIA == "3" &
                                           TP_SITUACAO_FUNCIONAMENTO == "1"], na.rm=TRUE),
    .groups = "drop"
  ) |>
  mutate(NU_ANO_CENSO = as.character(NU_ANO_CENSO))

tab_sp <- tab_saopaulo |>
  filter(CO_DISTRITO  %in% distritos_amostra$CO_DISTRITO,
         NU_ANO_CENSO %in% c("2019","2024")) |>
  select(NU_ANO_CENSO, CO_DISTRITO, NO_DISTRITO,
         tab_n_esc_dir   = n_esc_dir,
         tab_n_esc_ind   = n_esc_ind_mun,
         tab_mat_bas_dir = mat_bas_dir,
         tab_mat_cre_dir = mat_cre_dir)

ref_sp |>
  left_join(tab_sp, by = c("NU_ANO_CENSO","CO_DISTRITO")) |>
  mutate(
    ok_n_dir   = ref_n_esc_dir   == tab_n_esc_dir,
    ok_n_ind   = ref_n_esc_ind   == replace_na(tab_n_esc_ind, 0),
    ok_mat_dir = ref_mat_bas_dir == tab_mat_bas_dir,
    ok_cre_dir = ref_mat_cre_dir == tab_mat_cre_dir,
    tudo_ok    = ok_n_dir & ok_n_ind & ok_mat_dir & ok_cre_dir
  ) |>
  select(NU_ANO_CENSO, NO_DISTRITO,
         ref_n_esc_dir, tab_n_esc_dir, ok_n_dir,
         ref_n_esc_ind, tab_n_esc_ind, ok_n_ind,
         ref_mat_bas_dir, tab_mat_bas_dir, ok_mat_dir,
         ok_cre_dir, tudo_ok) |>
  print(n = Inf, width = Inf)