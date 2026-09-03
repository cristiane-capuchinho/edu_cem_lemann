# =============================================================
# INTEGRAÇÃO FINAL — população, PNAD-C (creche/pré-escola nas
# capitais) e Bolsa Família por distrito de SP no banco final
# tabelas_analiticas_2015_2025.xlsx, mais atualização do codebook.
#
# NÃO sobrescreve os arquivos originais: grava em
#   outputs/tabelas_analiticas_2015_2025_v2.xlsx
#   outputs/codebook_censo_lemann_cem_v2.xlsx
#
# Rode com o diretório de trabalho em qualquer lugar dentro do
# projeto (leman_cem, ou leman_cem/PNUD, ou leman_cem/outputs) —
# o script localiza a raiz sozinho subindo diretórios.
# =============================================================

pacotes <- c("dplyr", "readr", "stringr", "stringi", "openxlsx", "readxl", "purrr", "tibble")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

limpar_nome <- function(x) {
  x |> stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_trim()
}

# ── 0) Localiza a raiz do projeto (precisa achar outputs/ E PNUD/outputs/) ──
candidatos_raiz <- c(".", "..", "../..", "../../..")
raiz_valida <- NULL
for (r in candidatos_raiz) {
  if (file.exists(file.path(r, "outputs/tabelas_analiticas_2015_2025.xlsx")) &&
      file.exists(file.path(r, "PNUD/outputs/conferencia_pnadc_pop_bolsafamilia.rds"))) {
    raiz_valida <- r
    break
  }
}
if (is.null(raiz_valida)) {
  stop("Não encontrei 'outputs/tabelas_analiticas_2015_2025.xlsx' e ",
       "'PNUD/outputs/conferencia_pnadc_pop_bolsafamilia.rds' a partir do diretório atual (",
       getwd(), "). Rode este script com o diretório de trabalho dentro da pasta leman_cem.")
}
setwd(raiz_valida)
message("Raiz do projeto: ", normalizePath("."))

# ── 1) Lê o banco final atual e os dados coletados ──────────────
message("\nLendo banco final atual...")
tab_municipios <- readxl::read_excel("outputs/tabelas_analiticas_2015_2025.xlsx", sheet = "municipios")
tab_capitais   <- readxl::read_excel("outputs/tabelas_analiticas_2015_2025.xlsx", sheet = "capitais")
tab_saopaulo   <- readxl::read_excel("outputs/tabelas_analiticas_2015_2025.xlsx", sheet = "saopaulo")
message("    municipios: ", nrow(tab_municipios), " linhas, ", ncol(tab_municipios), " colunas")
message("    capitais:   ", nrow(tab_capitais), " linhas, ", ncol(tab_capitais), " colunas")
message("    saopaulo:   ", nrow(tab_saopaulo), " linhas, ", ncol(tab_saopaulo), " colunas")

message("\nLendo dados coletados (PNAD-C, população, Bolsa Família)...")
coletado <- readRDS("PNUD/outputs/conferencia_pnadc_pop_bolsafamilia.rds")
pop_municipios      <- coletado$pop_municipios
pnadc_capitais      <- coletado$pnadc_capitais
bf_distrito_painel  <- coletado$bf_distrito_painel

# ── 2) Prepara chaves de join com tipo compatível (texto) ───────
# No banco final, CO_MUNICIPIO e NU_ANO_CENSO são texto (confirmado
# no codebook: tipo "character"); os objetos coletados são numéricos.
# Usa colunas de chave auxiliares (.chave_*) para não alterar o tipo
# original das colunas do banco.
pop_join <- pop_municipios |>
  dplyr::transmute(
    CO_MUNICIPIO  = as.character(as.integer(co_municipio)),
    NU_ANO_CENSO  = as.character(as.integer(ano)),
    pop_total_mun = pop_total,
    pop_0a3_mun   = pop_0a3,
    pop_4a5_mun   = pop_4a5,
    pop_metodo_mun = metodo
  )

pnadc_join <- pnadc_capitais |>
  dplyr::transmute(
    CO_MUNICIPIO = as.character(as.integer(co_municipio)),
    NU_ANO_CENSO = as.character(as.integer(ano)),
    taxa_creche_0a3,
    taxa_pre_4a5
  )

bf_join <- bf_distrito_painel |>
  dplyr::transmute(
    NO_DISTRITO_limpo = distrito,  # já vem limpo (upper, sem acento)
    NU_ANO_CENSO = as.character(as.integer(ano)),
    fonte_bf,
    qtd_bf_creche_antigo,
    qtd_bf_creche_0a3,
    qtd_bf_pre_4a6,
    n_escolas_novo
  )

# ── 3) tab_municipios e tab_capitais: população + PNAD-C ────────
adicionar_pop_pnadc <- function(tab) {
  tab |>
    dplyr::mutate(.chave_co  = as.character(as.integer(CO_MUNICIPIO)),
                  .chave_ano = as.character(as.integer(NU_ANO_CENSO))) |>
    dplyr::left_join(pop_join,   by = c(".chave_co" = "CO_MUNICIPIO", ".chave_ano" = "NU_ANO_CENSO")) |>
    dplyr::left_join(pnadc_join, by = c(".chave_co" = "CO_MUNICIPIO", ".chave_ano" = "NU_ANO_CENSO")) |>
    dplyr::select(-.chave_co, -.chave_ano)
}

tab_municipios_v2 <- adicionar_pop_pnadc(tab_municipios)
tab_capitais_v2   <- adicionar_pop_pnadc(tab_capitais)

message("\n=========================================================")
message("VALIDAÇÃO — tab_municipios")
message("=========================================================")
message("Linhas: ", nrow(tab_municipios_v2), " (era ", nrow(tab_municipios), ", precisa ser igual)")
message("Colunas: ", ncol(tab_municipios_v2), " (era ", ncol(tab_municipios), ", esperado +6)")
message("Linhas com pop_total_mun preenchido: ", sum(!is.na(tab_municipios_v2$pop_total_mun)),
        " (esperado = total de linhas, população cobre todo município)")
message("Linhas com taxa_creche_0a3 preenchido: ", sum(!is.na(tab_municipios_v2$taxa_creche_0a3)),
        " (esperado 216, só as 27 capitais nos anos com PNAD-C)")

message("\n=========================================================")
message("VALIDAÇÃO — tab_capitais")
message("=========================================================")
message("Linhas: ", nrow(tab_capitais_v2), " (era ", nrow(tab_capitais), ")")
message("Linhas com pop_total_mun preenchido: ", sum(!is.na(tab_capitais_v2$pop_total_mun)),
        " (esperado = total de linhas, 297)")
message("Linhas com taxa_creche_0a3 preenchido: ", sum(!is.na(tab_capitais_v2$taxa_creche_0a3)),
        " (esperado 216)")

# ── 4) tab_saopaulo: população do MUNICÍPIO (repetida por distrito,
#       sufixo _mun deixa isso explícito) + Bolsa Família por distrito.
#       PNAD-C fica de fora dessa aba por decisão do usuário (só
#       existe em nível de município, não faz sentido repetir 96x). ──
tab_saopaulo_v2 <- tab_saopaulo |>
  dplyr::mutate(.chave_co       = as.character(as.integer(CO_MUNICIPIO)),
                .chave_ano      = as.character(as.integer(NU_ANO_CENSO)),
                .chave_distrito = limpar_nome(NO_DISTRITO)) |>
  dplyr::left_join(pop_join, by = c(".chave_co" = "CO_MUNICIPIO", ".chave_ano" = "NU_ANO_CENSO")) |>
  dplyr::left_join(bf_join,  by = c(".chave_distrito" = "NO_DISTRITO_limpo", ".chave_ano" = "NU_ANO_CENSO")) |>
  dplyr::select(-.chave_co, -.chave_ano, -.chave_distrito)

message("\n=========================================================")
message("VALIDAÇÃO — tab_saopaulo")
message("=========================================================")
message("Linhas: ", nrow(tab_saopaulo_v2), " (era ", nrow(tab_saopaulo), ")")
message("Colunas: ", ncol(tab_saopaulo_v2), " (era ", ncol(tab_saopaulo), ", esperado +9: 4 de população + 5 de Bolsa Família)")
message("Linhas com pop_total_mun preenchido: ", sum(!is.na(tab_saopaulo_v2$pop_total_mun)),
        " (esperado = total de linhas, 1056)")
message("Linhas com algum dado de Bolsa Família (fonte_bf não-NA): ", sum(!is.na(tab_saopaulo_v2$fonte_bf)),
        " de ", nrow(tab_saopaulo_v2),
        " (o painel de BF cobre 2015-2023 e 2024-2026; tab_saopaulo cobre 2015-2025, ",
        "então só 2026 do painel fica sem uso — 2015-2025 inteiro deveria vir preenchido)")

message("\n--- Linhas de tab_saopaulo SEM dado de Bolsa Família, por ano (esperado: nenhuma, 0 em todos) ---")
print(as.data.frame(tab_saopaulo_v2 |> dplyr::filter(is.na(fonte_bf)) |> dplyr::count(NU_ANO_CENSO)))

message("\n--- Amostra: distrito Sé, todos os anos (população repete o valor do município; Bolsa Família muda por distrito) ---")
print(as.data.frame(tab_saopaulo_v2 |>
  dplyr::filter(NO_DISTRITO == "Sé") |>
  dplyr::select(NU_ANO_CENSO, NO_DISTRITO, pop_total_mun, pop_metodo_mun,
                fonte_bf, qtd_bf_creche_antigo, qtd_bf_creche_0a3, qtd_bf_pre_4a6)))

# ── 5) Grava o banco final v2 ────────────────────────────────────
# openxlsx simples (createWorkbook/writeData/saveWorkbook), sem
# freezePane nem estilos complexos — o arquivo original tem um bug
# de relação de drawing quebrada (provavelmente de freezePane()) e
# não quero reintroduzir o mesmo problema aqui.
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "municipios"); openxlsx::writeData(wb, "municipios", tab_municipios_v2)
openxlsx::addWorksheet(wb, "capitais");   openxlsx::writeData(wb, "capitais", tab_capitais_v2)
openxlsx::addWorksheet(wb, "saopaulo");   openxlsx::writeData(wb, "saopaulo", tab_saopaulo_v2)
openxlsx::saveWorkbook(wb, "outputs/tabelas_analiticas_2015_2025_v2.xlsx", overwrite = TRUE)
message("\nSALVO: outputs/tabelas_analiticas_2015_2025_v2.xlsx")

# ── 6) Atualiza o codebook (v2) ──────────────────────────────────
message("\nAtualizando codebook...")
glossario_atual <- readxl::read_excel("outputs/codebook_censo_lemann_cem.xlsx", sheet = "glossario")
notas_atual     <- readxl::read_excel("outputs/codebook_censo_lemann_cem.xlsx", sheet = "notas_metodologicas")
cobertura_atual <- readxl::read_excel("outputs/codebook_censo_lemann_cem.xlsx", sheet = "cobertura")
validacao_atual <- readxl::read_excel("outputs/codebook_censo_lemann_cem.xlsx", sheet = "validacao")

glossario_novo <- tibble::tribble(
  ~variavel, ~descricao,
  "pop_total_mun", "Populacao total do municipio (fonte: brpop::ibge_pop(), IBGE, censos + estimativas). Em tab_saopaulo, valor do municipio de Sao Paulo repetido em toda linha de distrito daquele ano -- NAO e populacao do distrito (populacao geral por distrito nao esta disponivel em nenhuma fonte com API/exportacao viavel).",
  "pop_0a3_mun", "Populacao de 0 a 3 anos do municipio. 2022 e valor exato do Censo Demografico (SIDRA tabela 9514, idade em ano unico). Demais anos sao estimados aplicando a razao 0-3/4-5 observada no Censo 2022 sobre a faixa 0-4 anual (brpop::mun_pop_age, source datasus2024) -- estimativa derivada, nao contagem direta. Ver pop_metodo_mun. Em tab_saopaulo, valor do municipio, repetido por distrito.",
  "pop_4a5_mun", "Populacao de 4 a 5 anos do municipio. Mesma fonte e metodo de pop_0a3_mun. Em tab_saopaulo, valor do municipio, repetido por distrito.",
  "pop_metodo_mun", "'censo_exato' (2022, contagem direta do Censo Demografico 2022) ou 'estimado_razao_censo2022' (demais anos, estimativa derivada aplicando a razao etaria de 2022 sobre a populacao 0-4 anual).",
  "taxa_creche_0a3", "Taxa de frequencia escolar de criancas de 0 a 3 anos (PNAD Continua, SIDRA tabela 7138, variavel 10276, classificacao c58, categoria 99749). So preenchido para as 27 capitais (unicos municipios individualizaveis na amostra da PNAD-C, confirmado empiricamente via teste de controle com municipio nao-capital) e para os anos 2016, 2017, 2018, 2019, 2022, 2023, 2024, 2025 (2020-2021 com o modulo de frequencia escolar suspenso na pandemia). NA para os demais 5.541 municipios e para 2020-2021.",
  "taxa_pre_4a5", "Taxa de frequencia escolar de criancas de 4 a 5 anos (PNAD Continua, mesma tabela, categoria 47813, mesma cobertura de taxa_creche_0a3).",
  "fonte_bf", "Origem do dado de cobertura do Bolsa Familia por distrito em tab_saopaulo: 'antigo_2013_2023' (dataset descontinuado da Prefeitura de SP, so creche) ou 'novo_2024_2026' (dataset atual, separa creche de pre-escola). NA quando o distrito/ano nao tem dado de nenhuma das duas fontes.",
  "qtd_bf_creche_antigo", "Numero de criancas beneficiarias do Programa Bolsa Familia (chamado Auxilio Brasil em 2022, mesma estrutura de arquivo, Lei 14.284/2021) matriculadas em creche no distrito, 2015-2023. NAO e populacao total de 0-3 anos do distrito, e apenas o subconjunto de beneficiarias do programa matriculadas. Nao usar como denominador de taxa de cobertura populacional. Fonte: dados.prefeitura.sp.gov.br, dataset 'beneficiarios do bolsa familia em creches' (descontinuado).",
  "qtd_bf_creche_0a3", "Numero de criancas beneficiarias do Bolsa Familia matriculadas em creche (0-3 anos) no distrito, 2024-2026. Mesmo caveat de qtd_bf_creche_antigo: indicador de cobertura de programa, nao populacao.",
  "qtd_bf_pre_4a6", "Numero de criancas beneficiarias do Bolsa Familia matriculadas em pre-escola no distrito, 2024-2026. ATENCAO: faixa etaria do dataset da Prefeitura e 4 a 6 anos, NAO 4 a 5 -- rotulo mantido deliberadamente distinto do recorte de 4-5 anos usado no restante do banco (taxa_pre_4a5, pop_4a5_mun), para nao serem confundidos ou somados.",
  "n_escolas_novo", "Numero de escolas (CODEOL distintos) somadas para compor qtd_bf_creche_0a3 e qtd_bf_pre_4a6 naquele distrito e ano. So preenchido 2024-2026 (dataset novo reporta em nivel de escola; dataset antigo ja vem agregado por distrito)."
)
glossario_v2 <- dplyr::bind_rows(glossario_atual, glossario_novo)

nota_nova <- tibble::tribble(
  ~principio, ~titulo, ~descricao,
  "P9", "Integracao de fontes externas ao INEP (populacao, PNAD-C, Bolsa Familia)",
  paste0(
    "Populacao (brpop::ibge_pop() e SIDRA 9514), frequencia em creche/pre-escola (SIDRA 7138, so capitais) ",
    "e cobertura do Bolsa Familia por distrito de Sao Paulo (dados.prefeitura.sp.gov.br) sao fontes externas ",
    "ao microdado INEP, integradas por chave (CO_MUNICIPIO+NU_ANO_CENSO ou NO_DISTRITO+NU_ANO_CENSO), nao ",
    "derivadas do censo escolar. Bolsa Familia mede beneficiarios do programa, nao populacao geral -- nao ",
    "usar qtd_bf_* como denominador de taxa de cobertura populacional. Populacao em tab_saopaulo e do ",
    "municipio de Sao Paulo inteiro, repetida por distrito (sufixo _mun nas colunas); populacao geral por ",
    "distrito nao esta disponivel em nenhuma fonte com API ou exportacao viavel (Seade sem exportacao ",
    "automatizavel, Censo 2022 por setor censitario descartado por custo-beneficio). PNAD-C tabela 7138 so ",
    "cobre as 27 capitais (amostra restrita as areas auto-representativas do desenho amostral), confirmado ",
    "empiricamente via teste de controle (municipio nao-capital retorna NA em todos os anos). Regiao ",
    "Metropolitana de Sao Paulo (tabela 7139, cor/raca) e cor/raca por subprefeitura ficaram fora do escopo ",
    "por decisao do usuario."
  )
)
notas_v2 <- dplyr::bind_rows(notas_atual, nota_nova)

perfilar <- function(tab, nome_tabela) {
  purrr::map_dfr(names(tab), function(col) {
    x <- tab[[col]]
    tibble::tibble(
      tabela = nome_tabela,
      variavel = col,
      tipo = class(x)[1],
      n_obs = sum(!is.na(x)),
      n_na = sum(is.na(x)),
      pct_preenchido = round(100 * sum(!is.na(x)) / length(x), 1)
    )
  })
}
dicionario_municipios_v2 <- perfilar(tab_municipios_v2, "tab_municipios")
dicionario_capitais_v2   <- perfilar(tab_capitais_v2, "tab_capitais")
dicionario_saopaulo_v2   <- perfilar(tab_saopaulo_v2, "tab_saopaulo")

cobertura_novas <- tibble::tribble(
  ~metrica, ~valor,
  "Colunas novas em tab_municipios/tab_capitais", "6 (pop_total_mun, pop_0a3_mun, pop_4a5_mun, pop_metodo_mun, taxa_creche_0a3, taxa_pre_4a5)",
  "Colunas novas em tab_saopaulo", "9 (4 de populacao + fonte_bf, qtd_bf_creche_antigo, qtd_bf_creche_0a3, qtd_bf_pre_4a6, n_escolas_novo)",
  "Linhas de tab_saopaulo com dado de Bolsa Familia", as.character(sum(!is.na(tab_saopaulo_v2$fonte_bf))),
  "Municipios com taxa PNAD-C (0-3/4-5)", "27 (so capitais, anos 2016, 2017, 2018, 2019, 2022, 2023, 2024, 2025)"
)
cobertura_v2 <- dplyr::bind_rows(cobertura_atual, cobertura_novas)

wb_cb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_cb, "notas_metodologicas"); openxlsx::writeData(wb_cb, "notas_metodologicas", notas_v2)
openxlsx::addWorksheet(wb_cb, "validacao");           openxlsx::writeData(wb_cb, "validacao", validacao_atual)
openxlsx::addWorksheet(wb_cb, "cobertura");           openxlsx::writeData(wb_cb, "cobertura", cobertura_v2)
openxlsx::addWorksheet(wb_cb, "glossario");           openxlsx::writeData(wb_cb, "glossario", glossario_v2)
openxlsx::addWorksheet(wb_cb, "dicionario_municipios"); openxlsx::writeData(wb_cb, "dicionario_municipios", dicionario_municipios_v2)
openxlsx::addWorksheet(wb_cb, "dicionario_capitais");   openxlsx::writeData(wb_cb, "dicionario_capitais", dicionario_capitais_v2)
openxlsx::addWorksheet(wb_cb, "dicionario_saopaulo");   openxlsx::writeData(wb_cb, "dicionario_saopaulo", dicionario_saopaulo_v2)
openxlsx::saveWorkbook(wb_cb, "outputs/codebook_censo_lemann_cem_v2.xlsx", overwrite = TRUE)
message("SALVO: outputs/codebook_censo_lemann_cem_v2.xlsx")

message("\n=========================================================")
message("Cole de volta TODOS os prints acima, principalmente as")
message("validações de contagem de linhas/colunas e a amostra do")
message("distrito Sé — preciso confirmar antes de considerar isso")
message("pronto para promover a v2 como banco definitivo.")
message("=========================================================")
