# ============================================================
# LIVRO DE CÓDIGO — CENSO ESCOLAR LEMANN-CEM 2015–2025
# Versão: Mai/2026 — pós-correção scaffold 96 distritos SP / 27 capitais
# ============================================================

library(openxlsx); library(dplyr); library(tidyr); library(stringr)

# ── Cores nomeadas R ──────────────────────────────────────────────────────────
C_H1    <- "midnightblue"
C_H2    <- "steelblue"
C_LBLUE <- "aliceblue"
C_LBLU2 <- "lavender"
C_GRN   <- "honeydew"
C_YEL   <- "lemonchiffon"
C_WHT   <- "white"
C_BLK   <- "black"

# ── Fábrica de estilos ────────────────────────────────────────────────────────
est <- function(bg = C_WHT, fc = C_BLK, bold = FALSE, center = FALSE,
                numFmt = "GENERAL") {
  args <- list(
    fontName = "Arial", fontSize = 10,
    fontColour = fc, fgFill = bg,
    halign = if (center) "CENTER" else "LEFT",
    valign = "center", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "gray",
    numFmt = numFmt
  )
  if (bold) args$textDecoration <- "bold"
  do.call(createStyle, args)
}

st_h1  <- est(C_H1,  "white", bold = TRUE, center = TRUE)
st_h2  <- est(C_H2,  "white", bold = TRUE)
st_sec <- est(C_H2,  "white", bold = TRUE)

# ── Workbook ──────────────────────────────────────────────────────────────────
wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

# ============================================================
# ABA 1 — RESUMO
# ============================================================
addWorksheet(wb, "Resumo", gridLines = FALSE)
setColWidths(wb, "Resumo", cols = 1:5, widths = c(28, 42, 14, 12, 18))

mergeCells(wb, "Resumo", cols = 1:5, rows = 1)
writeData(wb, "Resumo",
          "CENSO ESCOLAR DA EDUCACAO BASICA — BANCO DE DADOS LEMANN-CEM", startRow = 1)
addStyle(wb, "Resumo", st_h1, rows = 1, cols = 1:5, gridExpand = TRUE)
setRowHeights(wb, "Resumo", rows = 1, heights = 30)

mergeCells(wb, "Resumo", cols = 1:5, rows = 2)
writeData(wb, "Resumo",
          "Tabelas analiticas municipais | 2015 a 2025 | v. Mai/2026", startRow = 2)
addStyle(wb, "Resumo", est(C_H2, "white", center = TRUE),
         rows = 2, cols = 1:5, gridExpand = TRUE)
setRowHeights(wb, "Resumo", rows = 2, heights = 22)

mergeCells(wb, "Resumo", cols = 1:5, rows = 4)
writeData(wb, "Resumo", "OBJETOS ANALITICOS", startRow = 4)
addStyle(wb, "Resumo", st_h2, rows = 4, cols = 1:5, gridExpand = TRUE)
setRowHeights(wb, "Resumo", rows = 4, heights = 20)

writeData(wb, "Resumo",
          data.frame(A="Objeto",B="Unidade de analise",C="Linhas",D="Colunas",E="Periodo"),
          startRow = 5, colNames = FALSE)
addStyle(wb, "Resumo", st_h1, rows = 5, cols = 1:5, gridExpand = TRUE)
setRowHeights(wb, "Resumo", rows = 5, heights = 22)

objs <- data.frame(
  Objeto  = c("tab_municipios","tab_capitais","tab_saopaulo"),
  Unidade = c("Municipio x Ano (todos os municipios BR com rede direta)",
              "Municipio x Ano (27 capitais x 11 anos = 297 linhas garantidas)",
              "Distrito x Ano (96 distritos IBGE x 11 anos = 1.056 linhas)"),
  Linhas  = c(format(nrow(tab_municipios),big.mark=","),
              format(nrow(tab_capitais),  big.mark=","),
              format(nrow(tab_saopaulo),  big.mark=",")),
  Colunas = c(ncol(tab_municipios), ncol(tab_capitais), ncol(tab_saopaulo)),
  Periodo = "2015-2025", stringsAsFactors = FALSE
)
writeData(wb, "Resumo", objs, startRow = 6, colNames = FALSE)
for (i in 1:3) {
  bg <- if (i %% 2 == 1) C_LBLUE else C_LBLU2
  addStyle(wb,"Resumo",est(bg,bold=TRUE),  rows=5+i,cols=1)
  addStyle(wb,"Resumo",est(bg),            rows=5+i,cols=2)
  addStyle(wb,"Resumo",est(bg,center=TRUE),rows=5+i,cols=3:5,gridExpand=TRUE)
  setRowHeights(wb,"Resumo",rows=5+i,heights=22)
}

mergeCells(wb, "Resumo", cols=1:5, rows=10)
writeData(wb, "Resumo", "NOTAS GERAIS", startRow=10)
addStyle(wb, "Resumo", st_h2, rows=10, cols=1:5, gridExpand=TRUE)
setRowHeights(wb, "Resumo", rows=10, heights=20)

notas_r <- data.frame(
  Chave = c("Fonte primaria","Rede analisada","Brasilia/DF",
            "Scaffold capitais","Scaffold Sao Paulo",
            "NAs em _ind","NAs em razoes","Infraestrutura","2025","Validacao"),
  Valor = c(
    "INEP - Microdados do Censo Escolar da Educacao Basica (2015-2025)",
    "Rede direta municipal (TP_DEPENDENCIA=3) e rede indireta conveniada municipal (TP_DEPENDENCIA=4 + IN_PODER_PUBLICO_PARCERIA=1 + TP_PODER_PUBLICO_PARCERIA=1)",
    "DF sem governo municipal: escolas com TP_DEPENDENCIA=2 e CO_MUNICIPIO=5300108 equiparadas a rede direta em tab_capitais",
    "tab_capitais usa scaffold (27 capitais x 11 anos = 297 linhas garantidas). Capitais sem rede direta em algum ano aparecem com 0s, nao ausentes.",
    "tab_saopaulo usa scaffold da API IBGE (96 distritos oficiais x 11 anos = 1.056 linhas). Garante representacao completa do municipio mesmo em distritos sem oferta da rede municipal direta.",
    "Variaveis _ind de contagem e matricula: NA substituido por 0 (municipio/distrito-ano sem rede indireta = 0 escolas/matriculas). Aplicado tambem a contagens e somas da rede direta (n_esc_dir, mat_*_dir) introduzidas pelo scaffold.",
    "doc_tur_*_dir, doc_tur_*_ind, mat_ei_esc_dir, mat_ei_esc_ind: mantidos como NA. Razao indefinida sem turmas ou sem escolas com IN_INF=1 no denominador.",
    "IN_AREA_VERDE, IN_BANHEIRO_EI, IN_MATERIAL_PED_JOGOS/ARTISTICAS: NA em escolas inativas (TP_SITUACAO!=1) — ~16-20% em 2022-2025, 0% em 2015-2021",
    "IN_INF, IN_INF_CRE, IN_INF_PRE ausentes nos microdados 2025 — mudanca de instrumento INEP",
    "CHECK mat_bas_dir: dif=0 em todos os 11 anos (tab_municipios vs censo_completo). Scaffold nao altera somatorios."
  ), stringsAsFactors = FALSE
)
for (i in seq_len(nrow(notas_r))) {
  r  <- 10 + i
  bg <- if (i %% 2 == 1) C_YEL else C_WHT
  mergeCells(wb,"Resumo",cols=2:5,rows=r)
  writeData(wb,"Resumo",notas_r$Chave[i],startRow=r,startCol=1)
  writeData(wb,"Resumo",notas_r$Valor[i],startRow=r,startCol=2)
  addStyle(wb,"Resumo",est(bg,bold=TRUE),rows=r,cols=1)
  addStyle(wb,"Resumo",est(bg),          rows=r,cols=2)
  setRowHeights(wb,"Resumo",rows=r,heights=44)
}

# ============================================================
# ABA 2 — DICIONÁRIO (15 colunas)
# ============================================================
addWorksheet(wb, "Dicionario", gridLines = FALSE)
freezePane(wb, "Dicionario", firstActiveRow = 3)
setColWidths(wb, "Dicionario", cols = 1:15,
             widths = c(4, 27, 44, 17, 9, 10, 34, 10, 22, 9, 9, 9, 9, 9, 9))

mergeCells(wb,"Dicionario",cols=1:15,rows=1)
writeData(wb,"Dicionario",
          "DICIONARIO DE VARIAVEIS — TABELAS ANALITICAS CENSO ESCOLAR LEMANN-CEM 2015-2025",
          startRow=1)
addStyle(wb,"Dicionario",est(C_H1,"white",bold=TRUE,center=TRUE),
         rows=1,cols=1:15,gridExpand=TRUE)
setRowHeights(wb,"Dicionario",rows=1,heights=24)

hdrs_d <- c("N","Variavel","Descricao","Grupo","Tipo","Unidade",
            "Categorias/Valores","Presente","Anos Disponiveis",
            "NA%\nMunic","NA%\nCap","NA%\nSP","Min","Max","Media")
writeData(wb,"Dicionario",t(as.data.frame(hdrs_d)),startRow=2,colNames=FALSE)
addStyle(wb,"Dicionario",st_h1,rows=2,cols=1:15,gridExpand=TRUE)
setRowHeights(wb,"Dicionario",rows=2,heights=36)

# ── Definições de variáveis ───────────────────────────────────────────────────
defs <- tribble(
  ~variavel,                ~descricao,                                              ~grupo,
  # IDENTIFICACAO
  "NU_ANO_CENSO",           "Ano do censo escolar",                                  "Identificacao",
  "SG_UF",                  "Sigla da UF",                                           "Identificacao",
  "NO_MUNICIPIO",           "Nome do municipio",                                     "Identificacao",
  "CO_MUNICIPIO",           "Codigo IBGE do municipio (7 digitos)",                  "Identificacao",
  "NO_MICRORREGIAO",        "Nome da microrregiao IBGE (somente SP)",                "Identificacao",
  "CO_MICRORREGIAO",        "Codigo da microrregiao IBGE (somente SP)",              "Identificacao",
  "NO_DISTRITO",            "Nome do distrito IBGE (somente SP — fonte: API IBGE)",  "Identificacao",
  "CO_DISTRITO",            "Codigo IBGE do distrito 9 digitos (somente SP)",        "Identificacao",
  # ESCOLAS
  "n_esc_dir",              "N escolas — rede direta municipal",                     "Escolas",
  "n_esc_ind_mun",          "N escolas privadas conveniadas — so municipal",         "Escolas",
  "n_esc_ind_mun_est",      "N escolas privadas conveniadas — mun. e estadual",      "Escolas",
  "n_cre_dir",              "N creches rede direta por distrito (somente SP)",       "Escolas",
  "n_cre_ind",              "N creches privadas conveniadas por distrito (SP)",      "Escolas",
  "n_pre_dir",              "N pre-escolas rede direta por distrito (somente SP)",   "Escolas",
  "n_pre_ind",              "N pre-escolas privadas conveniadas por distr. (SP)",    "Escolas",
  # MATRICULA EB
  "mat_bas_dir",            "Matriculas na EB — rede direta",                        "Matricula EB",
  "mat_bas_ind",            "Matriculas na EB — rede indireta municipal",            "Matricula EB",
  "mat_bas_tot",            "Matriculas na EB — total (dir + ind)",                  "Matricula EB",
  # MATRICULA EI
  "mat_ei_dir",             "Matriculas na EI — rede direta",                        "Matricula EI",
  "mat_ei_ind",             "Matriculas na EI — rede indireta",                      "Matricula EI",
  "mat_ei_tot",             "Matriculas na EI — total",                              "Matricula EI",
  "mat_ei_ti_dir",          "Matriculas na EI tempo integral — direta",              "Matricula EI",
  "mat_ei_ti_ind",          "Matriculas na EI tempo integral — indireta",            "Matricula EI",
  "mat_ei_ti_tot",          "Matriculas na EI tempo integral — total",               "Matricula EI",
  "mat_cre_dir",            "Matriculas na creche — rede direta",                    "Matricula EI",
  "mat_cre_ind",            "Matriculas na creche — rede indireta",                  "Matricula EI",
  "mat_cre_tot",            "Matriculas na creche — total",                          "Matricula EI",
  "mat_cre_ti_dir",         "Matriculas na creche tempo integral — direta",          "Matricula EI",
  "mat_cre_ti_ind",         "Matriculas na creche tempo integral — indireta",        "Matricula EI",
  "mat_pre_dir",            "Matriculas na pre-escola — rede direta",                "Matricula EI",
  "mat_pre_ind",            "Matriculas na pre-escola — rede indireta",              "Matricula EI",
  "mat_pre_tot",            "Matriculas na pre-escola — total",                      "Matricula EI",
  "mat_pre_ti_dir",         "Matriculas na pre-escola TI — direta",                  "Matricula EI",
  "mat_pre_ti_ind",         "Matriculas na pre-escola TI — indireta",                "Matricula EI",
  # EI LOCALIZACAO
  "mat_cre_dir_loc_urb",    "Matriculas creche direta — area urbana",                "EI Localizacao",
  "mat_cre_dir_loc_rur",    "Matriculas creche direta — area rural",                 "EI Localizacao",
  "mat_cre_ind_loc_urb",    "Matriculas creche indireta — area urbana",              "EI Localizacao",
  "mat_cre_ind_loc_rur",    "Matriculas creche indireta — area rural",               "EI Localizacao",
  "mat_pre_dir_loc_urb",    "Matriculas pre-escola direta — area urbana",            "EI Localizacao",
  "mat_pre_dir_loc_rur",    "Matriculas pre-escola direta — area rural",             "EI Localizacao",
  "mat_pre_ind_loc_urb",    "Matriculas pre-escola indireta — area urbana",          "EI Localizacao",
  "mat_pre_ind_loc_rur",    "Matriculas pre-escola indireta — area rural",           "EI Localizacao",
  # EI LOC DIFERENCIADA
  "mat_cre_tot_ld_fora",    "Mat. creche total — fora area diferenciada",            "EI Loc Dif",
  "mat_cre_tot_ld_asset",   "Mat. creche total — assentamento",                      "EI Loc Dif",
  "mat_cre_tot_ld_indig",   "Mat. creche total — terra indigena",                    "EI Loc Dif",
  "mat_cre_tot_ld_quilom",  "Mat. creche total — quilombos",                         "EI Loc Dif",
  "mat_cre_tot_ld_trad",    "Mat. creche total — povos tradicionais",                "EI Loc Dif",
  "mat_pre_tot_ld_fora",    "Mat. pre-escola total — fora area diferenciada",        "EI Loc Dif",
  "mat_pre_tot_ld_asset",   "Mat. pre-escola total — assentamento",                  "EI Loc Dif",
  "mat_pre_tot_ld_indig",   "Mat. pre-escola total — terra indigena",                "EI Loc Dif",
  "mat_pre_tot_ld_quilom",  "Mat. pre-escola total — quilombos",                     "EI Loc Dif",
  "mat_pre_tot_ld_trad",    "Mat. pre-escola total — povos tradicionais",            "EI Loc Dif",
  # DOCENTES TURMA
  "doc_tur_cre_dir",        "Razao docentes/turma creche — rede direta",             "Docentes Turma",
  "doc_tur_cre_ind",        "Razao docentes/turma creche — rede indireta",           "Docentes Turma",
  "doc_tur_pre_dir",        "Razao docentes/turma pre-escola — rede direta",         "Docentes Turma",
  "doc_tur_pre_ind",        "Razao docentes/turma pre-escola — rede indireta",       "Docentes Turma",
  # RAZOES
  "mat_ei_esc_dir",         "Media mat. EI por escola — rede direta",                "Razoes",
  "mat_ei_esc_ind",         "Media mat. EI por escola — rede indireta",              "Razoes",
  # INFRAESTRUTURA EI
  "mat_ei_av_dir",          "Mat. EI em escolas com area verde — direta",            "Infraestrutura EI",
  "mat_ei_av_ind",          "Mat. EI em escolas com area verde — indireta",          "Infraestrutura EI",
  "mat_ei_ban_dir",         "Mat. EI em escolas com banheiro EI — direta",           "Infraestrutura EI",
  "mat_ei_ban_ind",         "Mat. EI em escolas com banheiro EI — indireta",         "Infraestrutura EI",
  "mat_ei_jog_dir",         "Mat. EI com mat. pedagogico jogos — direta",            "Infraestrutura EI",
  "mat_ei_jog_ind",         "Mat. EI com mat. pedagogico jogos — indireta",          "Infraestrutura EI",
  "mat_ei_art_dir",         "Mat. EI com mat. pedagogico artistico — direta",        "Infraestrutura EI",
  "mat_ei_art_ind",         "Mat. EI com mat. pedagogico artistico — indireta",      "Infraestrutura EI",
  "mat_ei_4_dir",           "Mat. EI escolas com todos 4 itens infra — direta",      "Infraestrutura EI",
  "mat_ei_4_ind",           "Mat. EI escolas com todos 4 itens infra — indireta",    "Infraestrutura EI",
  "mat_ei_s4_dir",          "Mat. EI escolas sem nenhum item infra — direta",        "Infraestrutura EI",
  "mat_ei_s4_ind",          "Mat. EI escolas sem nenhum item infra — indireta",      "Infraestrutura EI",
  # OUTROS SEGMENTOS
  "mat_fund_ai_dir",        "Matriculas EF anos iniciais — rede direta",             "Outros Segmentos",
  "mat_fund_af_dir",        "Matriculas EF anos finais — rede direta",               "Outros Segmentos",
  "mat_med_dir",            "Matriculas ensino medio — rede direta",                 "Outros Segmentos",
  "mat_eja_dir",            "Matriculas EJA — rede direta",                          "Outros Segmentos",
  # ED PROFISSIONAL
  "mat_prof_tot_ld_fora",   "Mat. ed. profissional total — fora area dif.",          "Ed Profissional",
  "mat_prof_tot_ld_asset",  "Mat. ed. profissional total — assentamento",            "Ed Profissional",
  "mat_prof_tot_ld_indig",  "Mat. ed. profissional total — terra indigena",          "Ed Profissional",
  "mat_prof_tot_ld_quilom", "Mat. ed. profissional total — quilombos",               "Ed Profissional",
  "mat_prof_tot_ld_trad",   "Mat. ed. profissional total — povos tradicionais",      "Ed Profissional",
  # ED ESPECIAL
  "mat_esp_tot_ld_fora",    "Mat. ed. especial total — fora area dif.",              "Ed Especial",
  "mat_esp_tot_ld_asset",   "Mat. ed. especial total — assentamento",               "Ed Especial",
  "mat_esp_tot_ld_indig",   "Mat. ed. especial total — terra indigena",             "Ed Especial",
  "mat_esp_tot_ld_quilom",  "Mat. ed. especial total — quilombos",                  "Ed Especial",
  "mat_esp_tot_ld_trad",    "Mat. ed. especial total — povos tradicionais",         "Ed Especial",
  # COR RACA
  "mat_bas_nd_dir",         "Mat. EB cor/raca nao declarado — direta",              "Cor Raca",
  "mat_bas_branca_dir",     "Mat. EB cor/raca branca — direta",                     "Cor Raca",
  "mat_bas_preta_dir",      "Mat. EB cor/raca preta — direta",                      "Cor Raca",
  "mat_bas_parda_dir",      "Mat. EB cor/raca parda — direta",                      "Cor Raca",
  "mat_bas_amarela_dir",    "Mat. EB cor/raca amarela — direta",                    "Cor Raca",
  "mat_bas_indigena_dir",   "Mat. EB cor/raca indigena — direta",                   "Cor Raca",
  "mat_bas_nd_ind",         "Mat. EB cor/raca nao declarado — indireta",            "Cor Raca",
  "mat_bas_branca_ind",     "Mat. EB cor/raca branca — indireta",                   "Cor Raca",
  "mat_bas_preta_ind",      "Mat. EB cor/raca preta — indireta",                    "Cor Raca",
  "mat_bas_parda_ind",      "Mat. EB cor/raca parda — indireta",                    "Cor Raca",
  "mat_bas_amarela_ind",    "Mat. EB cor/raca amarela — indireta",                  "Cor Raca",
  "mat_bas_indigena_ind",   "Mat. EB cor/raca indigena — indireta",                 "Cor Raca",
  # EB LOC DIF
  "mat_bas_dir_ld_fora",    "Mat. EB direta — fora area dif.",                      "EB Loc Dif",
  "mat_bas_dir_ld_asset",   "Mat. EB direta — assentamento",                        "EB Loc Dif",
  "mat_bas_dir_ld_indig",   "Mat. EB direta — terra indigena",                      "EB Loc Dif",
  "mat_bas_dir_ld_quilom",  "Mat. EB direta — quilombos",                           "EB Loc Dif",
  "mat_bas_dir_ld_trad",    "Mat. EB direta — povos tradicionais",                  "EB Loc Dif",
  "mat_bas_ind_ld_fora",    "Mat. EB indireta — fora area dif.",                    "EB Loc Dif",
  "mat_bas_ind_ld_asset",   "Mat. EB indireta — assentamento",                      "EB Loc Dif",
  "mat_bas_ind_ld_indig",   "Mat. EB indireta — terra indigena",                    "EB Loc Dif",
  "mat_bas_ind_ld_quilom",  "Mat. EB indireta — quilombos",                         "EB Loc Dif",
  "mat_bas_ind_ld_trad",    "Mat. EB indireta — povos tradicionais",                "EB Loc Dif"
) |>
  mutate(
    tipo = case_when(
      variavel %in% c("NU_ANO_CENSO","SG_UF","NO_MUNICIPIO","CO_MUNICIPIO",
                      "NO_MICRORREGIAO","CO_MICRORREGIAO","NO_DISTRITO","CO_DISTRITO") ~ "chr/num",
      str_starts(variavel, "n_")                 ~ "int",
      str_detect(variavel, "doc_tur|mat_ei_esc") ~ "num (razao)",
      TRUE                                       ~ "num"
    ),
    unidade = case_when(
      str_detect(variavel, "doc_tur")    ~ "doc/turma",
      str_detect(variavel, "mat_ei_esc") ~ "mat/escola",
      str_starts(variavel, "n_")         ~ "escola",
      str_starts(variavel, "mat_")       ~ "matricula",
      TRUE                               ~ "—"
    ),
    # ── Categorias / descrição de valores ────────────────────────────────────
    categorias = case_when(
      variavel == "NU_ANO_CENSO" ~
        "2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 | 2025",
      variavel == "SG_UF" ~
        "AC AL AM AP BA CE DF ES GO MA MG MS MT PA PB PE PI PR RJ RN RO RR RS SC SE SP TO (27 UFs)",
      variavel %in% c("NO_MUNICIPIO","NO_MICRORREGIAO","NO_DISTRITO") ~
        "Texto livre — nome oficial IBGE (NO_DISTRITO em SP: fonte API IBGE/localidades)",
      variavel == "CO_MUNICIPIO" ~
        "Codigo IBGE 7 digitos (ex: 3550308 = Sao Paulo / 5300108 = Brasilia/DF)",
      variavel == "CO_MICRORREGIAO" ~
        "Codigo IBGE microrregiao (5 digitos)",
      variavel == "CO_DISTRITO" ~
        "Codigo IBGE distrito (9 digitos; ex: 355030801 = Agua Rasa/SP)",
      variavel == "n_esc_dir" ~
        "Inteiro >= 0 | Inclui todas as situacoes de funcionamento | DF: inclui TP_DEP=2 | 0 = municipio/distrito-ano sem rede direta (presenca garantida via scaffold em tab_capitais e tab_saopaulo)",
      variavel == "n_esc_ind_mun" ~
        "Inteiro >= 0 | Filtros: TP_DEP=4 + IN_PPP=1 + TP_PPP=1 + ativo + QT_MAT_BAS>0 | 0 = sem rede indireta exclusivamente municipal",
      variavel == "n_esc_ind_mun_est" ~
        "Inteiro >= 0 | TP_PODER_PUBLICO_PARCERIA=3 (convenio municipal E estadual) | 0 = sem convenio combinado",
      variavel %in% c("n_cre_dir","n_cre_ind") ~
        "Inteiro >= 0 | Filtro: IN_INF_CRE=1 + TP_SITUACAO=1 | NA em 2025 (IN_INF_CRE ausente) | 0 = distrito-ano sem creche dessa rede",
      variavel %in% c("n_pre_dir","n_pre_ind") ~
        "Inteiro >= 0 | Filtro: IN_INF_PRE=1 + TP_SITUACAO=1 | NA em 2025 (IN_INF_PRE ausente) | 0 = distrito-ano sem pre-escola dessa rede",
      variavel %in% c("mat_bas_dir","mat_bas_ind","mat_bas_tot") ~
        "Inteiro >= 0 | Inclui: EI + EF + EM + EJA + EP + EE | 0 = sem matriculas naquela rede no municipio/distrito-ano",
      grupo == "Matricula EI" & str_detect(variavel,"_ti_") ~
        "Inteiro >= 0 | Tempo integral = permanencia >= 7h/dia | 0 = sem TI",
      grupo == "Matricula EI" ~
        "Inteiro >= 0 | 0 = sem matriculas nessa etapa e rede",
      grupo == "EI Localizacao" ~
        "Inteiro >= 0 | TP_LOCALIZACAO: 1=Urbana 2=Rural | 0 = sem mat. nessa zona",
      grupo == "EI Loc Dif" ~
        "Inteiro >= 0 | 0 = sem mat. nessa loc. diferenciada | Dir + Ind somados",
      grupo == "Docentes Turma" ~
        "Real > 0 | Formula: sum(QT_DOC)/sum(QT_TUR) por unidade geografica | Tipico: 1.0-3.0 | NA = sem turmas (denominador=0) ou sem oferta da rede",
      grupo == "Razoes" ~
        "Real > 0 | Formula: sum(mat_EI)/count(escolas com IN_INF=1) | Tipico: 50-500 | NA em 2025 (IN_INF ausente) e em unidades sem oferta de EI",
      grupo == "Infraestrutura EI" & str_detect(variavel,"_4_") ~
        "Inteiro >= 0 | Escolas com IN_AV=1 E IN_BAN=1 E IN_JOG=1 E IN_ART=1 simultaneamente",
      grupo == "Infraestrutura EI" & str_detect(variavel,"_s4_") ~
        "Inteiro >= 0 | Escolas sem nenhum dos 4 itens de infraestrutura",
      grupo == "Infraestrutura EI" ~
        "Inteiro >= 0 | sum(mat_EI) em escolas com o atributo=1 | NA: escolas inativas 2022-2025",
      grupo == "Outros Segmentos" & variavel == "mat_fund_ai_dir" ~
        "Inteiro >= 0 | EF anos iniciais: 1 ao 5 ano | Somente rede direta",
      grupo == "Outros Segmentos" & variavel == "mat_fund_af_dir" ~
        "Inteiro >= 0 | EF anos finais: 6 ao 9 ano | Somente rede direta",
      grupo == "Outros Segmentos" & variavel == "mat_med_dir" ~
        "Inteiro >= 0 | Ensino medio regular | Somente rede direta",
      grupo == "Outros Segmentos" & variavel == "mat_eja_dir" ~
        "Inteiro >= 0 | EJA: Fund + Med + Profissional | Somente rede direta",
      grupo == "Ed Profissional" ~
        "Inteiro >= 0 | Dir + Ind | Desagregado por TP_LOCALIZACAO_DIFERENCIADA",
      grupo == "Ed Especial" ~
        "Inteiro >= 0 | Dir + Ind | Inclui CC (classe comum) e CE (classe especial)",
      grupo == "Cor Raca" ~
        "Inteiro >= 0 | Autodeclaracao no ato da matricula | QT_MAT_BAS_ND = nao declarado",
      str_detect(grupo,"Loc Dif") ~
        "Inteiro >= 0 | TP_LOC_DIF: 0=fora 1=assentamento 2=terra_indig 3=quilombo 8=trad.",
      TRUE ~ "—"
    ),
    presente = case_when(
      variavel %in% c("NO_MICRORREGIAO","CO_MICRORREGIAO","NO_DISTRITO","CO_DISTRITO",
                      "n_cre_dir","n_cre_ind","n_pre_dir","n_pre_ind") ~ "SP",
      TRUE ~ "M  C  SP"
    ),
    anos_disp = case_when(
      variavel %in% c("n_cre_dir","n_cre_ind","n_pre_dir","n_pre_ind",
                      "mat_ei_esc_dir","mat_ei_esc_ind") ~
        "2015-2024 (NA em 2025: IN_INF ausente)",
      variavel %in% c("mat_cre_ti_dir","mat_cre_ti_ind",
                      "mat_pre_ti_dir","mat_pre_ti_ind") ~
        "2015-2025 (QT_MAT_INF_CRE/PRE_INT disponivel todo o periodo)",
      variavel %in% c("NO_MICRORREGIAO","CO_MICRORREGIAO",
                      "NO_DISTRITO","CO_DISTRITO") ~
        "2015-2025 (somente tab_saopaulo)",
      grupo %in% c("Infraestrutura EI") ~
        "2015-2025 (cobertura 100% em 2015-2021; ~80-84% em 2022-2025)",
      TRUE ~ "2015-2025"
    )
  )

# ── Estatísticas calculadas do banco ─────────────────────────────────────────
message("Calculando estatisticas...")

calc_stats <- function(tbl, suf) {
  tbl |>
    summarise(across(where(is.numeric), list(
      pct_na = ~ round(mean(is.na(.)) * 100, 1),
      minimo = ~ if (all(is.na(.))) NA_real_ else round(min(., na.rm=TRUE), 1),
      maximo = ~ if (all(is.na(.))) NA_real_ else round(max(., na.rm=TRUE), 1),
      media  = ~ if (all(is.na(.))) NA_real_ else round(mean(., na.rm=TRUE), 1)
    ), .names = "{.col}___{.fn}")) |>
    pivot_longer(everything(), names_to="key", values_to="val") |>
    separate(key, into=c("variavel","stat"), sep="___") |>
    pivot_wider(names_from=stat, values_from=val) |>
    rename_with(~ paste0(., "_", suf), -variavel)
}

stats_m  <- calc_stats(tab_municipios, "m")
stats_c  <- calc_stats(tab_capitais,   "c")
stats_sp <- calc_stats(tab_saopaulo,   "sp")

defs <- defs |>
  left_join(stats_m  |> select(variavel, pct_na_m, minimo_m, maximo_m, media_m), by="variavel") |>
  left_join(stats_c  |> select(variavel, pct_na_c),                              by="variavel") |>
  left_join(stats_sp |> select(variavel, pct_na_sp),                             by="variavel")

# ── Cores por grupo ───────────────────────────────────────────────────────────
group_colors <- c(
  "Identificacao"    = C_GRN,
  "Escolas"          = "aliceblue",
  "Matricula EB"     = "lightcyan",
  "Matricula EI"     = "aliceblue",
  "EI Localizacao"   = "lavenderblush",
  "EI Loc Dif"       = "lavender",
  "Docentes Turma"   = C_YEL,
  "Razoes"           = C_YEL,
  "Infraestrutura EI"= C_GRN,
  "Outros Segmentos" = C_WHT,
  "Ed Profissional"  = C_WHT,
  "Ed Especial"      = C_WHT,
  "Cor Raca"         = "mistyrose",
  "EB Loc Dif"       = "lavenderblush"
)

# ── Escrever dicionário ───────────────────────────────────────────────────────
id_vars <- c("NU_ANO_CENSO","SG_UF","NO_MUNICIPIO","CO_MUNICIPIO")
vars_sp  <- c("NO_MICRORREGIAO","CO_MICRORREGIAO","NO_DISTRITO","CO_DISTRITO",
              "n_cre_dir","n_cre_ind","n_pre_dir","n_pre_ind")

row   <- 3
prev  <- ""
n_seq <- 0L

for (i in seq_len(nrow(defs))) {
  v   <- defs[i, ]
  grp <- v$grupo
  bg  <- group_colors[[grp]]; if (is.null(bg)||is.na(bg)) bg <- C_WHT
  
  if (grp != prev) {
    writeData(wb,"Dicionario",paste0("  >>> ",toupper(grp)),startRow=row,startCol=2)
    addStyle(wb,"Dicionario",st_sec,rows=row,cols=1:15,gridExpand=TRUE)
    setRowHeights(wb,"Dicionario",rows=row,heights=16)
    prev <- grp; row <- row + 1
  }
  
  n_seq <- n_seq + 1L
  
  na_m  <- if (!is.na(v$pct_na_m))  v$pct_na_m  else NA_real_
  na_c  <- if (!is.na(v$pct_na_c))  v$pct_na_c  else NA_real_
  na_sp <- if (!is.na(v$pct_na_sp)) v$pct_na_sp else NA_real_
  vmin  <- if (!is.na(v$minimo_m))  v$minimo_m  else NA_real_
  vmax  <- if (!is.na(v$maximo_m))  v$maximo_m  else NA_real_
  vmean <- if (!is.na(v$media_m))   v$media_m   else NA_real_
  
  if (v$variavel %in% id_vars) { na_m <- 0; na_c <- 0; na_sp <- 0 }
  if (v$variavel %in% vars_sp) { na_m <- NA_real_; na_c <- NA_real_ }
  
  writeData(wb,"Dicionario",n_seq,         startRow=row,startCol=1)
  writeData(wb,"Dicionario",v$variavel,    startRow=row,startCol=2)
  writeData(wb,"Dicionario",v$descricao,   startRow=row,startCol=3)
  writeData(wb,"Dicionario",v$grupo,       startRow=row,startCol=4)
  writeData(wb,"Dicionario",v$tipo,        startRow=row,startCol=5)
  writeData(wb,"Dicionario",v$unidade,     startRow=row,startCol=6)
  writeData(wb,"Dicionario",v$categorias,  startRow=row,startCol=7)
  writeData(wb,"Dicionario",v$presente,    startRow=row,startCol=8)
  writeData(wb,"Dicionario",v$anos_disp,   startRow=row,startCol=9)
  if (!is.na(na_m))  writeData(wb,"Dicionario",na_m, startRow=row,startCol=10)
  if (!is.na(na_c))  writeData(wb,"Dicionario",na_c, startRow=row,startCol=11)
  if (!is.na(na_sp)) writeData(wb,"Dicionario",na_sp,startRow=row,startCol=12)
  if (!is.na(vmin))  writeData(wb,"Dicionario",vmin, startRow=row,startCol=13)
  if (!is.na(vmax))  writeData(wb,"Dicionario",vmax, startRow=row,startCol=14)
  if (!is.na(vmean)) writeData(wb,"Dicionario",vmean,startRow=row,startCol=15)
  
  addStyle(wb,"Dicionario",est(bg,center=TRUE),         rows=row,cols=1)
  addStyle(wb,"Dicionario",est(bg,bold=TRUE),           rows=row,cols=2)
  addStyle(wb,"Dicionario",est(bg),                     rows=row,cols=3)
  addStyle(wb,"Dicionario",est(bg,center=TRUE),         rows=row,cols=4:6, gridExpand=TRUE)
  addStyle(wb,"Dicionario",est(bg),                     rows=row,cols=7)
  addStyle(wb,"Dicionario",est(bg,center=TRUE),         rows=row,cols=8:9, gridExpand=TRUE)
  addStyle(wb,"Dicionario",est(bg,center=TRUE,numFmt="0.0"),rows=row,cols=10:15,gridExpand=TRUE)
  setRowHeights(wb,"Dicionario",rows=row,heights=28)
  row <- row + 1
}

addFilter(wb,"Dicionario",row=2,cols=1:15)

# ============================================================
# ABA 3 — NOTAS METODOLÓGICAS
# ============================================================
addWorksheet(wb,"Notas",gridLines=FALSE)
setColWidths(wb,"Notas",cols=1:2,widths=c(34,74))

mergeCells(wb,"Notas",cols=1:2,rows=1)
writeData(wb,"Notas",
          "NOTAS METODOLOGICAS — DECISOES DE CONSTRUCAO E LIMITACOES",startRow=1)
addStyle(wb,"Notas",est(C_H1,"white",bold=TRUE,center=TRUE),
         rows=1,cols=1:2,gridExpand=TRUE)
setRowHeights(wb,"Notas",rows=1,heights=26)

secoes <- list(
  list(titulo="ESCOPO E COMPLETUDE DAS TABELAS (SCAFFOLD)", itens=list(
    c("tab_saopaulo — scaffold de 96 distritos x 11 anos",
      "Garantido por right_join com tabela-esqueleto montada a partir da API IBGE (servicodados.ibge.gov.br/api/v1/localidades/municipios/3550308/distritos). Sem o scaffold, distritos sem escola da rede municipal direta em algum ano nao apareciam (resultava em apenas 94 distritos). Com o scaffold: 1.056 linhas garantidas. Distritos sem oferta da rede direta em determinado ano aparecem com 0 em n_esc_dir e mat_*_dir, refletindo corretamente o universo de analise."),
    c("tab_capitais — scaffold de 27 capitais x 11 anos",
      "Mesma logica: tabela-esqueleto montada por tidyr::crossing(anos, capitais_meta). Garante 297 linhas mesmo que uma capital nao tenha rede direta em algum ano. Importante para DF (Brasilia), que usa filtro_dir=f_dir_capitais (TP_DEPENDENCIA=2 equiparada)."),
    c("tab_municipios — sem scaffold",
      "Para os ~5.570 municipios brasileiros, nao foi montado scaffold porque (a) o universo de municipios varia marginalmente entre anos no Censo Escolar e (b) a aplicacao analitica nao requer presenca obrigatoria de todos os municipios. Municipios sem rede direta em algum ano simplesmente nao aparecem nessa linha-ano."),
    c("Impacto sobre as estatisticas descritivas",
      "Apos o scaffold, NA% e medias em tab_capitais e tab_saopaulo refletem o universo completo (capital-ano ou distrito-ano), nao apenas os com oferta direta. Comparacoes entre as tres tabelas devem considerar esse fato: medias em tab_municipios sao condicionais a presenca de rede direta; em tab_capitais e tab_saopaulo, sao marginais sobre o universo completo.")
  )),
  list(titulo="RENOMEACOES HISTORICAS DE VARIAVEIS INEP", itens=list(
    c("IN_CONVENIADA_PP -> IN_PODER_PUBLICO_PARCERIA",
      "Renomeacao aplicada para 2015-2021 via educabR. Valores: 0=nao conveniada | 1=conveniada com poder publico. NA em 2022 (campo ausente nas tabelas separadas)."),
    c("TP_CONVENIO_PODER_PUBLICO -> TP_PODER_PUBLICO_PARCERIA",
      "Renomeacao em 2015-2022. A partir de 2023, nome canonico INEP. Valores: 0=sem convenio | 1=somente municipal | 2=somente estadual | 3=municipal e estadual.")
  )),
  list(titulo="CORRECOES SEMANTICAS NO PIPELINE", itens=list(
    c("TP_PODER_PUBLICO_PARCERIA — NA semantico 2022+",
      "A partir de 2022, INEP so registra escolas conveniadas neste campo. Escolas sem convenio ficam com NA (em vez de 0 como em 2015-2021). Correcao: NA->0 aplicada a partir de 2022 para manter comparabilidade longitudinal. Validacao empirica: NAs concentrados em TP_DEPENDENCIA 1, 2 e 3 (redes que nao celebram convenios). Resultado: 0% NA, valores 0/1/2/3 em todos os 11 anos."),
    c("TP_LOCALIZACAO_DIFERENCIADA — replicacao retroativa",
      "Variavel coletada sistematicamente a partir de 2023. Para 2015-2022, o valor foi replicado da observacao mais recente disponivel por CO_ENTIDADE (referencia: 2025). Pressuposto: localizacao diferenciada e atributo estrutural e estavel da escola. NA residual para escolas sem registro em nenhum ano (2023-2025)."),
    c("IN_AREA_VERDE, IN_BANHEIRO_EI, IN_MATERIAL_PED_* — patch de infraestrutura",
      "Quatro variaveis de infraestrutura adicionadas ao banco por leitura direta dos CSVs brutos e join por CO_ENTIDADE x NU_ANO_CENSO. Cobertura confirmada: 100% em 2015-2021 (escolas ativas); ~80-84% em 2022-2025 (NAs concentrados em TP_SITUACAO_FUNCIONAMENTO=2 ou 3, ou seja, escolas inativas). Decisao: NAs sao informativos — escolas inativas nao tem infraestrutura coletada pelo INEP."),
    c("NO_DISTRITO — fonte API IBGE",
      "Nome dos distritos de Sao Paulo recuperado da API IBGE/localidades (servicodados.ibge.gov.br) por CO_DISTRITO. Substitui o NO_DISTRITO original do censo (que tinha cobertura parcial). Garante grafia padronizada (str_to_title) e cobertura 100% para os 96 distritos oficiais.")
  )),
  list(titulo="DECISOES DE CONSTRUCAO DAS TABELAS ANALITICAS", itens=list(
    c("Brasilia/DF — equiparacao da rede estadual a rede municipal",
      "O Distrito Federal nao possui municipios nem governo municipal. Todas as escolas publicas do DF tem TP_DEPENDENCIA=2 (estadual). Para tab_capitais, escolas com TP_DEPENDENCIA=2 e CO_MUNICIPIO=5300108 foram incluidas como rede direta (usando filtro_dir=f_dir_capitais). Sem essa correcao, Brasilia nao apareceria adequadamente na tabela de capitais."),
    c("Rede indireta — definicao e filtros",
      "Rede indireta = escolas privadas com convenio exclusivamente municipal: TP_DEPENDENCIA=4 + IN_PODER_PUBLICO_PARCERIA=1 + TP_PODER_PUBLICO_PARCERIA=1. Convenio com municipio E estado (TP_PPP=3) contabilizado somente em n_esc_ind_mun_est. Contagem de escolas usa filtros adicionais: TP_SITUACAO_FUNCIONAMENTO=1 + QT_MAT_BAS>0."),
    c("Razao docentes por turma — formula e decisao metodologica",
      "Calculada como sum(QT_DOC_INF_CRE)/sum(QT_TUR_INF_CRE) ao nivel da unidade geografica. Nao como media das razoes individuais por escola, pois isso geraria vies de Jensen (escolas menores com razao maior pesariam igual a escolas maiores). A abordagem sum/sum e equivalente a uma media ponderada pelo numero de turmas."),
    c("Media de matriculas EI por escola — denominador",
      "Denominador = count de escolas com IN_INF=1 (escola oferta educacao infantil). Em 2025, IN_INF esta ausente dos microdados (mudanca de instrumento), entao o denominador nao pode ser calculado e a variavel retorna NA em todos os municipios para 2025."),
    c("Variaveis de infraestrutura EI — formula",
      "Formula: sum(mat_EI) para escolas onde IN_x=1, onde mat_EI = QT_MAT_INF_CRE + QT_MAT_INF_PRE. mat_ei_4_* = escolas com IN_AV=1 E IN_BAN=1 E IN_JOG=1 E IN_ART=1 (todos simultaneamente). mat_ei_s4_* = escolas sem nenhum dos 4 atributos. A soma das partes nao necessariamente igual ao total (escolas podem ter apenas alguns atributos).")
  )),
  list(titulo="TRATAMENTO DE NAs APOS SCAFFOLD", itens=list(
    c("Politica de zeragem — apenas contagens e somas",
      "Apos o scaffold, NAs sao convertidos em 0 APENAS para variaveis que comecam com n_esc_ ou mat_, exceto as que correspondem a razoes (doc_tur_*, mat_ei_esc_*). Justificativa: contagens e somatorios admitem zero como significado claro (sem oferta); razoes nao (denominador zero = indefinido, nao zero)."),
    c("Razoes preservadas como NA",
      "doc_tur_cre_dir, doc_tur_cre_ind, doc_tur_pre_dir, doc_tur_pre_ind, mat_ei_esc_dir, mat_ei_esc_ind mantidas como NA. Razao indefinida quando: (a) nao ha turmas (denominador=0), (b) nao ha escolas com IN_INF=1 (denominador=0), (c) unidade geografica nao tem oferta da rede analisada."),
    c("Impacto do scaffold sobre NA% das contagens",
      "Em tab_capitais e tab_saopaulo, NA% de n_esc_*_dir e mat_*_dir DIMINUIU apos o scaffold porque os NAs introduzidos pelo scaffold foram convertidos em 0. NA% de doc_tur_* e mat_ei_esc_* AUMENTOU porque novas linhas (capital-ano ou distrito-ano sem oferta da rede direta) entram com NA preservado nas razoes."),
    c("Verificacao de integridade",
      "CHECK mat_bas_dir: dif=0 entre tab_municipios e censo_completo em todos os 11 anos. Scaffold preserva somatorios (apenas adiciona linhas com 0, nao altera totais).")
  )),
  list(titulo="VARIAVEIS ESTRUTURALMENTE AUSENTES (NA 100% em alguns anos)", itens=list(
    c("IN_INF, IN_INF_CRE, IN_INF_PRE — ausentes em 2025",
      "Variaveis removidas do instrumento de coleta do INEP a partir de 2025. Afetam: mat_ei_esc_dir, mat_ei_esc_ind (denominador), n_cre_dir, n_cre_ind, n_pre_dir, n_pre_ind (filtros). Essas variaveis retornam NA em 2025."),
    c("QT_TUR_INF_INT, QT_TUR_INF_CRE_INT, QT_TUR_INF_PRE_INT — ausentes em 2015-2022",
      "Introduzidas em 2023. NA estrutural 100% em 2015-2022. Nao ha variaveis derivadas dessas no banco analitico atual."),
    c("QT_MAT_EJA_FUND_AI, QT_MAT_EJA_FUND_AF — ausentes em 2015-2021",
      "Desagregacao do EJA por nivel introduzida em 2022. Presentes no banco-base (censo_completo) mas nao nas tabelas analiticas municipais."),
    c("QT_MAT_ZR_URB, QT_MAT_ZR_RUR, QT_TRANSP_PUBLICO — ausentes em 2015-2021",
      "Introducao em 2022. QT_TRANSP_PUBLICO: numero de alunos com transporte publico. Presentes no banco-base mas nao usados nas tabelas analiticas. Variavel adicionada ao banco para uso futuro.")
  )),
  list(titulo="LIMITACOES E ALERTAS", itens=list(
    c("Descontinuidade estrutural 2025 — tabelas separadas",
      "A partir de 2025, INEP publicou os microdados em 4 tabelas separadas (Escola, Matricula, Docente, Turma). O pipeline faz join por CO_ENTIDADE apos agregar matriculas, docentes e turmas por escola. Possivel perda marginal por escolas sem match (CO_ENTIDADE ausente em alguma tabela). Validado: nrow antes = nrow depois do join."),
    c("Replicacao retroativa de TP_LOCALIZACAO_DIFERENCIADA",
      "O pressuposto de estabilidade da localizacao diferenciada pode nao ser valido em casos de: (1) escolas relocadas administrativamente, (2) criacao de novos territorios indigenas ou assentamentos, (3) reconhecimento tardio de quilombos. Para 2023-2025, os dados sao coletados diretamente. Para 2015-2022, o valor e imputado."),
    c("Municipios sem rede direta — tab_municipios apenas",
      "Em tab_municipios (sem scaffold), municipios onde TP_DEPENDENCIA=3 nunca aparece no censo (raros) nao constam. Pode incluir municipios muito pequenos com educacao gerida exclusivamente pelo estado ou municipios com sub-registro historico no Censo Escolar. Em tab_capitais e tab_saopaulo o problema nao se aplica gracas ao scaffold."),
    c("Comparabilidade de medias entre tabelas",
      "Medias em tab_municipios sao condicionais a presenca de rede direta no municipio-ano. Medias em tab_capitais e tab_saopaulo sao marginais sobre o universo completo (incluindo unidades-ano com 0). Para comparacoes longitudinais ou entre redes, considerar se a denominador apropriado e o universo completo (preferir tab_capitais/tab_saopaulo) ou apenas unidades com oferta (filtrar n_esc_dir > 0).")
  ))
)

row_n <- 3
for (sec in secoes) {
  mergeCells(wb,"Notas",cols=1:2,rows=row_n)
  writeData(wb,"Notas",sec$titulo,startRow=row_n)
  addStyle(wb,"Notas",st_sec,rows=row_n,cols=1:2,gridExpand=TRUE)
  setRowHeights(wb,"Notas",rows=row_n,heights=20)
  row_n <- row_n + 1
  for (j in seq_along(sec$itens)) {
    item <- sec$itens[[j]]
    bg   <- if (j %% 2 == 1) C_LBLUE else C_LBLU2
    writeData(wb,"Notas",item[1],startRow=row_n,startCol=1)
    writeData(wb,"Notas",item[2],startRow=row_n,startCol=2)
    addStyle(wb,"Notas",est(bg,bold=TRUE),rows=row_n,cols=1)
    addStyle(wb,"Notas",est(bg),          rows=row_n,cols=2)
    setRowHeights(wb,"Notas",rows=row_n,heights=52)
    row_n <- row_n + 1
  }
  row_n <- row_n + 1
}

# ── Salvar ────────────────────────────────────────────────────────────────────
saveWorkbook(wb, "codebook_censo_lemann_cem.xlsx", overwrite = TRUE)
message("Salvo: codebook_censo_lemann_cem.xlsx")