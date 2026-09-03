# =============================================================
# COLETA — PNAD Contínua (capitais), População (municípios e
# capitais, 0-3 e 4-5 anos) e Bolsa Família por distrito (SP)
#
# Primeira versão para validação. Não grava ainda no banco final
# (tabelas_analiticas_2015_2025.xlsx), produz objetos em memória +
# um .rds de conferência em outputs/. Rode e me mande de volta:
#   1) os prints de validação (contagens, nº de linhas não
#      classificadas no dataset antigo de Bolsa Família)
#   2) qualquer ERRO impresso
# =============================================================

# Localiza a raiz do projeto subindo diretorios (roda de qualquer
# lugar dentro de leman_cem: raiz, PNUD/ ou outputs/).
for (r in c(".", "..", "../..", "../../..")) {
  if (dir.exists(file.path(r, "PNUD")) && file.exists(file.path(r, "run.R"))) {
    setwd(r); break
  }
}
message("Raiz do projeto: ", normalizePath("."))

pacotes <- c("dplyr", "tidyr", "stringr", "stringi", "readr", "httr", "jsonlite",
             "sidrar", "brpop", "purrr", "readxl", "tibble")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)

dir.create("PNUD/outputs", showWarnings = FALSE, recursive = TRUE)

UA_NAVEGADOR <- httr::user_agent(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36"
)

CAPITAIS_CO <- c(
  1100205, 1302603, 1200401, 1600303, 1721000, 1400100,
  1501402, 2111300, 2211001, 2304400, 2408102, 2507507,
  2611606, 2704302, 2800308, 2927408, 3106200, 3205309,
  3304557, 3550308, 4106902, 4205407, 4314902, 5002704,
  5103403, 5208707, 5300108
)
CO_MUN_SP <- 3550308
ANOS_PAINEL <- 2015:2025

message("=========================================================")
message("1) POPULAÇÃO — municípios e capitais, total e 0-3 / 4-5")
message("=========================================================")

# 1.1 — população total municipal (IBGE, censos + estimativas)
pop_total_mun <- brpop::ibge_pop() |>
  dplyr::filter(year %in% ANOS_PAINEL) |>
  dplyr::rename(co_municipio = code_muni, ano = year, pop_total = pop)

message("--- 1.1 População total municipal: ", nrow(pop_total_mun),
        " linhas, ", dplyr::n_distinct(pop_total_mun$co_municipio), " municípios ---")

# 1.2 — Censo 2022 por idade única (ANCORA a razão 0-3 / 4-5)
message("\n--- 1.2 Buscando idade única (Censo 2022) para todos os municípios ---")
message("    Isso baixa ~5.570 municípios de uma vez, pode levar alguns minutos.")

meta9514 <- jsonlite::fromJSON(httr::content(
  httr::GET("https://servicodados.ibge.gov.br/api/v3/agregados/9514/metadados"),
  as = "text", encoding = "UTF-8"
))
idx_idade <- which(meta9514$classificacoes$nome == "Idade")
cats_idade <- meta9514$classificacoes$categorias[[idx_idade]]
idades_0a5 <- cats_idade |>
  dplyr::filter(nivel == 2,
                nome == "Menos de 1 ano" | stringr::str_detect(nome, "^[1-5] anos?$"))

censo_0a5_mun <- sidrar::get_sidra(
  x = 9514, variable = 93, geo = "City",
  geo.filter = list("City" = "all"),
  classific = paste0("c", meta9514$classificacoes$id[idx_idade]),
  category = list(idades_0a5$id),
  period = "all"
) |>
  dplyr::transmute(
    co_municipio = as.numeric(`Município (Código)`),
    idade = as.integer(dplyr::if_else(Idade == "Menos de 1 ano", "0",
                                       stringr::str_extract(Idade, "^[0-9]+"))),
    pop = Valor
  ) |>
  dplyr::mutate(faixa_censo = dplyr::if_else(idade <= 3, "0a3", "4a5")) |>
  dplyr::group_by(co_municipio, faixa_censo) |>
  dplyr::summarise(pop_censo = sum(pop, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = faixa_censo, values_from = pop_censo,
                      names_prefix = "pop_censo_") |>
  dplyr::mutate(
    total_censo_0a5 = pop_censo_0a3 + pop_censo_4a5,
    razao_0a3 = pop_censo_0a3 / total_censo_0a5,
    razao_4a5 = pop_censo_4a5 / total_censo_0a5
  )

message("--- 1.2 Censo 2022, idade única: ", nrow(censo_0a5_mun), " municípios com razão calculada ---")
message("    São Paulo (conferência): ",
        censo_0a5_mun |> dplyr::filter(co_municipio == CO_MUN_SP) |>
          dplyr::mutate(txt = sprintf("0-3=%d | 4-5=%d | razão 0-3=%.3f",
                                       pop_censo_0a3, pop_censo_4a5, razao_0a3)) |>
          dplyr::pull(txt))

# 1.3 — faixa "0 a 4" anual do brpop (para aplicar a razão nos anos não-censitários)
pop_0a4_anual <- brpop::mun_pop_age(source = "datasus2024", sex = "all") |>
  dplyr::filter(age_group == "From 0 to 4 years", year %in% ANOS_PAINEL) |>
  dplyr::transmute(co_municipio = code_muni, ano = year, pop_0a4 = pop)

# 1.4 — junta tudo: 2022 usa o valor exato do Censo, outros anos usam a razão
#       aplicada sobre "0 a 4" (aproximação documentada, [ESTIMADO])
pop_0a3_4a5_mun <- pop_0a4_anual |>
  dplyr::left_join(censo_0a5_mun |> dplyr::select(co_municipio, razao_0a3, razao_4a5),
                    by = "co_municipio") |>
  dplyr::left_join(censo_0a5_mun |> dplyr::select(co_municipio, pop_censo_0a3, pop_censo_4a5),
                    by = "co_municipio") |>
  dplyr::mutate(
    metodo    = dplyr::if_else(ano == 2022, "censo_exato", "estimado_razao_censo2022"),
    pop_0a3   = dplyr::if_else(ano == 2022, pop_censo_0a3, round(pop_0a4 * razao_0a3)),
    pop_4a5_5 = dplyr::if_else(ano == 2022, pop_censo_4a5, round(pop_0a4 * razao_4a5))
  ) |>
  dplyr::select(co_municipio, ano, pop_0a3, pop_4a5 = pop_4a5_5, metodo)

pop_municipios_final <- pop_total_mun |>
  dplyr::left_join(pop_0a3_4a5_mun, by = c("co_municipio", "ano"))

pop_capitais_final <- pop_municipios_final |>
  dplyr::filter(co_municipio %in% CAPITAIS_CO)

message("\n--- 1.4 Resultado final: ", nrow(pop_municipios_final), " linhas (municípios x ano) ---")
message("    Capitais: ", nrow(pop_capitais_final), " linhas esperadas (27 x 11 = 297)")
message("    São Paulo, 2022 (deve ser exato) e 2023 (deve ser estimado):")
pop_municipios_final |>
  dplyr::filter(co_municipio == CO_MUN_SP, ano %in% c(2022, 2023)) |>
  print()

message("\n=========================================================")
message("2) PNAD CONTÍNUA — Tabela 7138, creche/pré-escola por capital")
message("=========================================================")

pnadc_capitais <- sidrar::get_sidra(
  x = 7138, variable = 10276, geo = "City",
  geo.filter = list(City = CAPITAIS_CO),
  classific = "c58", category = list(c(99749, 47813)),
  period = "all"
) |>
  dplyr::transmute(
    co_municipio = as.numeric(`Município (Código)`),
    ano = as.integer(Ano),
    faixa = dplyr::if_else(`Grupo de idade` == "0 a 3 anos", "taxa_creche_0a3", "taxa_pre_4a5"),
    valor = Valor
  ) |>
  tidyr::pivot_wider(names_from = faixa, values_from = valor)

message("--- 2) Tabela 7138: ", nrow(pnadc_capitais), " linhas (esperado até 27 x 8 anos = 216) ---")
message("    Anos presentes: ", paste(sort(unique(pnadc_capitais$ano)), collapse = ", "))
message("    (2020 e 2021 devem estar ausentes, módulo suspenso na pandemia)")

message("\n=========================================================")
message("3) BOLSA FAMÍLIA POR DISTRITO — dataset novo (2024-2026)")
message("=========================================================")

# os nomes de coluna mudam de grafia entre arquivos do mesmo dataset
# (ex.: "Pré-Escola" em 2026 vs. provavelmente "PRE ESCOLA" em 2024/2025,
# que é como o dicionário oficial declara o campo). Por isso detecta por
# padrão (sem acento, sem espaço) em vez de nome literal.
limpar_col <- function(x) {
  x |> stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^A-Z]", "")
}

ler_novabf <- function(url) {
  resp <- httr::GET(url, UA_NAVEGADOR, httr::config(followlocation = TRUE))
  txt <- httr::content(resp, as = "text", encoding = "latin1")
  df <- readr::read_delim(txt, delim = ";", show_col_types = FALSE,
                           locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  names(df) <- limpar_col(names(df))
  df
}

urls_novo <- c(
  "2024" = "https://dados.prefeitura.sp.gov.br/dataset/2e1a7fe7-284f-445a-8566-031f689f1b54/resource/8999e4a8-9cbc-4fd6-b7ce-cef505e55f34/download/novabolsafamilia_julho2024.csv",
  "2025" = "https://dados.prefeitura.sp.gov.br/dataset/2e1a7fe7-284f-445a-8566-031f689f1b54/resource/da872ee9-1b7c-4e7f-b56b-3a0e84250b4d/download/novabolsafamilia_julho2025.csv",
  "2026" = "https://dados.prefeitura.sp.gov.br/dataset/2e1a7fe7-284f-445a-8566-031f689f1b54/resource/16bd535c-17eb-406e-aecf-efa7d5e04c54/download/novabolsafamilia_julho2026.csv"
)

bf_novo <- purrr::imap_dfr(urls_novo, function(url, ano) {
  tryCatch({
    df <- ler_novabf(url)
    col_distrito <- names(df)[stringr::str_detect(names(df), "^DISTRITO$")][1]
    col_creche   <- names(df)[stringr::str_detect(names(df), "^CRECHE$")][1]
    col_pre      <- names(df)[stringr::str_detect(names(df), "PREESCOLA")][1]
    if (any(is.na(c(col_distrito, col_creche, col_pre)))) {
      message("    [", ano, "] AVISO: coluna não encontrada por padrão. Colunas disponíveis: ",
              paste(names(df), collapse = ", "))
      return(NULL)
    }
    tibble::tibble(
      ano = as.integer(ano),
      distrito = stringr::str_to_upper(df[[col_distrito]]),
      qtd_bf_creche_0a3 = df[[col_creche]],
      qtd_bf_pre_4a6 = df[[col_pre]]   # rótulo próprio, NÃO é o mesmo recorte de 4-5
    )
  }, error = function(e) {
    message("    ERRO ao ler/processar ", ano, ": ", conditionMessage(e))
    NULL
  })
})

bf_novo_distrito <- bf_novo |>
  dplyr::group_by(distrito, ano) |>
  dplyr::summarise(qtd_bf_creche_0a3 = sum(qtd_bf_creche_0a3, na.rm = TRUE),
                    qtd_bf_pre_4a6   = sum(qtd_bf_pre_4a6, na.rm = TRUE),
                    n_escolas = dplyr::n(), .groups = "drop")

message("--- 3) Dataset novo agregado por distrito: ", nrow(bf_novo_distrito), " linhas ---")
message("    Distritos distintos: ", dplyr::n_distinct(bf_novo_distrito$distrito),
        " (esperado próximo de 96)")
print(bf_novo_distrito |> dplyr::filter(ano == 2026) |> head(10))

message("\n=========================================================")
message("4) BOLSA FAMÍLIA POR DISTRITO — dataset antigo (2015-2023)")
message("=========================================================")

# As 13 grafias EXATAS de cabeçalho de Diretoria Regional de Educação,
# confirmadas linha a linha no arquivo 2023: cada uma é a primeira linha
# de um bloco, e seu valor bate exatamente com a soma das linhas de
# distrito que vêm logo depois (conferido manualmente, 13/13 blocos,
# somando ao total de 96 distritos). Versão anterior tinha variantes
# "por garantia" (ex. "FREGUESIA DO O", "JACANA", "PIRITUBA/JARAGUA"
# soltos) que na verdade são nomes de DISTRITO puro, não de cabeçalho,
# e causavam a exclusão errada desses 12 distritos homônimos da Diretoria.
DRES_SP <- c("BUTANTA", "CAMPO LIMPO", "CAPELA DO SOCORRO",
             "FREGUESIA/BRASILANDIA", "GUAIANASES", "IPIRANGA", "ITAQUERA",
             "JACANA/TREMEMBE", "PENHA", "PIRITUBA", "SANTO AMARO",
             "SAO MATEUS", "SAO MIGUEL")

limpar_nome <- function(x) {
  x |> stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_trim()
}

urls_antigo <- c(
  # CORRIGIDO: as URLs anteriores para 2015-2021 apontavam para o UUID do
  # recurso XLSX (erro meu ao montar a lista), não do CSV. O servidor serve
  # pelo UUID, o nome de arquivo na URL é só cosmético, por isso vinha
  # binário (assinatura ZIP) em vez de texto. UUIDs de CSV corrigidos abaixo.
  "2015" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/45617498-f829-4dd1-a132-2dc8483c91ab/download/beneficiariosbfcreches2015.csv",
  "2016" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/41ccefee-f067-47d7-91eb-86ae2f3ccd9a/download/beneficiariosbfcreches2016.csv",
  "2017" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/5e2b2cbe-7785-410a-8530-b75c3139381e/download/beneficiariosbfcreches2017.csv",
  "2018" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/6f17acb3-450e-4217-b491-a7ea4f5bfc35/download/beneficiariosbfcreches2018.csv",
  "2019" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/5d67b9aa-70bf-48f1-845d-4ecdb7852831/download/beneficiariosbfcreches2019.csv",
  "2020" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/b4455a6d-ed78-4149-823f-91a0ecc92018/download/beneficiariosbfcreches2020.csv",
  "2021" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/b0424a02-64ac-4b29-98e6-fc4cf8ad24bf/download/beneficiariosbfcreches2021.csv",
  "2022" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/b0fb14f4-d8e5-4c63-be57-ae1426a89292/download/beneficiariosabcreches2022.csv",
  "2023" = "https://dados.prefeitura.sp.gov.br/dataset/ab8db078-a345-428f-8304-18447b2f4ecc/resource/6436325f-a721-4d69-9d43-2609397c9135/download/beneficiariosbfcreches2023.csv"
  # 2022: mesmo dataset, só renomeado "Auxílio Brasil" (Lei 14.284/2021), estrutura igual conforme confirmado pelo usuário
)

ler_bf_antigo <- function(url, ano) {
  resp <- httr::GET(url, UA_NAVEGADOR, httr::config(followlocation = TRUE))
  txt  <- httr::content(resp, as = "text", encoding = "latin1")
  linhas <- stringr::str_split(txt, "\r\n|\n")[[1]]

  # acha a linha de cabeçalho procurando "Diretoria" e "Distrito" juntos
  idx_header <- which(stringr::str_detect(limpar_nome(linhas), "DIRETORIA.*DISTRITO|DISTRITO.*DIRETORIA"))[1]
  if (is.na(idx_header)) {
    message("    [", ano, "] AVISO: não achei linha de cabeçalho 'Diretoria/Distrito', pulando.")
    return(NULL)
  }

  df <- readr::read_delim(paste(linhas[idx_header:length(linhas)], collapse = "\n"),
                           delim = ";", show_col_types = FALSE,
                           locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  names(df)[1:2] <- c("localidade", "quantidade")
  df |>
    dplyr::filter(!is.na(localidade), !is.na(quantidade)) |>
    dplyr::transmute(
      ano = as.integer(ano),
      localidade_limpa = limpar_nome(localidade),
      quantidade = as.numeric(quantidade)
    )
}

bf_antigo_bruto <- purrr::imap_dfr(urls_antigo, function(url, ano) {
  tryCatch(ler_bf_antigo(url, ano), error = function(e) {
    message("    ERRO ao ler ", ano, ": ", conditionMessage(e)); NULL
  })
})

# ─────────────────────────────────────────────────────────────
# Classificação por POSIÇÃO, não por nome: confirmado manualmente no
# arquivo de 2023 que cada cabeçalho de Diretoria é sempre a PRIMEIRA
# linha com aquele nome no ano, e seu valor bate exatamente com a soma
# das linhas de distrito que vêm logo depois, até o próximo cabeçalho
# (13 blocos conferidos um a um, 13/13 batendo, somando aos 96 distritos
# oficiais). Uma ocorrência seguinte do mesmo nome (ex.: "BUTANTA"
# aparecendo de novo dentro do próprio bloco) é dado de distrito real,
# não repetição de Diretoria — daí a regra "1ª ocorrência do nome, SE
# esse nome está na lista de 13 cabeçalhos, vira Diretoria; qualquer
# outra ocorrência, e qualquer nome fora da lista, vira distrito".
# ─────────────────────────────────────────────────────────────
bf_antigo_classificado <- bf_antigo_bruto |>
  dplyr::mutate(linha_ordem = dplyr::row_number()) |>
  dplyr::group_by(ano, localidade_limpa) |>
  dplyr::mutate(ocorrencia_do_nome = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    tipo_linha = dplyr::case_when(
      stringr::str_detect(localidade_limpa, "TOTAL")         ~ "totalizador",
      localidade_limpa %in% DRES_SP & ocorrencia_do_nome == 1 ~ "diretoria_total",
      TRUE                                                     ~ "distrito"
    )
  )

message("--- 4) Linhas classificadas como total de Diretoria: ",
        sum(bf_antigo_classificado$tipo_linha == "diretoria_total"),
        " (esperado 13 x 9 anos = 117) ---")
message("    Linhas classificadas como totalizador (excluídas): ",
        sum(bf_antigo_classificado$tipo_linha == "totalizador"), " ---")
message("    Linhas classificadas como distrito: ",
        sum(bf_antigo_classificado$tipo_linha == "distrito"), " ---")
message("    Distritos distintos encontrados: ",
        dplyr::n_distinct(bf_antigo_classificado$localidade_limpa[
          bf_antigo_classificado$tipo_linha == "distrito"]),
        " (esperado próximo de 96)")

# checagem 1: nome duplicado dentro dos classificados como distrito no
# mesmo ano tem que ser ZERO agora, já que só a 1ª ocorrência de um
# nome de Diretoria vira cabeçalho — qualquer duplicidade remanescente
# é sinal de que a regra de posição não se sustentou nesse ano
message("\n--- Checagem 1: nomes duplicados no mesmo ano entre os classificados como distrito ---")
duplicados <- bf_antigo_classificado |>
  dplyr::filter(tipo_linha == "distrito") |>
  dplyr::count(ano, localidade_limpa) |>
  dplyr::filter(n > 1)
if (nrow(duplicados) == 0) {
  message("    Nenhum. A regra de posição eliminou a ambiguidade em todos os anos.")
} else {
  message("    ATENÇÃO, ainda achou duplicados:")
  print(duplicados)
}

# checagem 2: auditoria de soma — o valor de cada cabeçalho de Diretoria
# tem que bater com a soma dos distritos do bloco logo abaixo dele (até
# o próximo cabeçalho ou fim do ano). É a prova estrutural mais forte:
# se bater nos 9 anos, a classificação está correta mesmo que a grafia
# do cabeçalho tenha variado de um ano para o outro.
message("\n--- Checagem 2: auditoria de soma (cabeçalho == soma do bloco de distritos) ---")
auditoria_soma <- bf_antigo_classificado |>
  dplyr::arrange(ano, linha_ordem) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(bloco_id = cumsum(tipo_linha == "diretoria_total")) |>
  dplyr::ungroup() |>
  dplyr::filter(bloco_id > 0) |>
  dplyr::group_by(ano, bloco_id) |>
  dplyr::summarise(
    diretoria       = localidade_limpa[tipo_linha == "diretoria_total"][1],
    valor_cabecalho = quantidade[tipo_linha == "diretoria_total"][1],
    soma_distritos  = sum(quantidade[tipo_linha == "distrito"], na.rm = TRUE),
    n_distritos     = sum(tipo_linha == "distrito"),
    .groups = "drop"
  ) |>
  dplyr::mutate(diferenca = valor_cabecalho - soma_distritos)

message("    Blocos onde a soma NÃO bateu (esperado 0 linhas abaixo):")
print(as.data.frame(auditoria_soma |> dplyr::filter(diferenca != 0)))
message("    Total de blocos auditados: ", nrow(auditoria_soma),
        " (esperado 13 x 9 anos = 117)")

# checagem 3: cruza com a lista de 96 distritos oficiais (do Excel de
# referência), só para conferência, não é mais usada para classificar
caminhos_candidatos <- c(
  "outputs/tabelas_analiticas_2015_2025.xlsx",
  "../outputs/tabelas_analiticas_2015_2025.xlsx",
  "../../outputs/tabelas_analiticas_2015_2025.xlsx"
)
caminho_valido <- caminhos_candidatos[file.exists(caminhos_candidatos)][1]

distritos_oficiais <- tryCatch({
  if (is.na(caminho_valido)) stop("nenhum dos caminhos candidatos existe: ",
                                   paste(caminhos_candidatos, collapse = " | "))
  readxl::read_excel(caminho_valido, sheet = "saopaulo") |>
    dplyr::distinct(NO_DISTRITO) |>
    dplyr::pull(NO_DISTRITO) |>
    limpar_nome()
}, error = function(e) {
  message("    Não consegui ler o Excel de referência: ", conditionMessage(e))
  NULL
})

if (!is.null(distritos_oficiais)) {
  encontrados <- unique(bf_antigo_classificado$localidade_limpa[bf_antigo_classificado$tipo_linha == "distrito"])
  faltando <- setdiff(distritos_oficiais, encontrados)
  sobrando <- setdiff(encontrados, distritos_oficiais)
  message("\n--- Checagem 3: cruzamento com a lista de 96 distritos oficiais ---")
  message("    Distritos oficiais (", length(distritos_oficiais), ") SEM dado no Bolsa Família antigo: ",
          length(faltando))
  print(faltando)
  message("    Nomes no Bolsa Família antigo que NÃO batem com nenhum distrito oficial")
  message("    (provável lixo/DRE não capturada, precisa investigar): ", length(sobrando))
  print(sobrando)
} else {
  message("\n--- Checagem 3 pulada: sem Excel de referência disponível ---")
  message("--- Lista completa dos nomes classificados como distrito ---")
  bf_antigo_classificado |>
    dplyr::filter(tipo_linha == "distrito") |>
    dplyr::distinct(localidade_limpa) |>
    dplyr::arrange(localidade_limpa) |>
    print(n = 100)
}

bf_antigo_distrito <- bf_antigo_classificado |>
  dplyr::filter(tipo_linha == "distrito") |>
  dplyr::transmute(ano, distrito = localidade_limpa, qtd_bf_creche_antigo = quantidade)

message("\n=========================================================")
message("5) BOLSA FAMÍLIA POR DISTRITO — painel único (antigo + novo)")
message("=========================================================")

# junta as duas fontes num painel único por distrito x ano. Os períodos
# são disjuntos (antigo = 2015-2023, novo = 2024-2026), então o full_join
# não sobrepõe linha nenhuma — cada ano só tem as colunas da fonte que o
# cobre, o resto fica NA. NÃO soma/concatena os dois indicadores de creche
# (qtd_bf_creche_antigo e qtd_bf_creche_0a3) numa coluna só, porque são
# definições diferentes (o antigo não separa por faixa etária dentro de
# creche do jeito que o novo separa creche de pré-escola) — melhor manter
# rastreável qual fonte gerou cada valor.
bf_distrito_painel <- dplyr::full_join(
  bf_antigo_distrito,
  bf_novo_distrito |>
    dplyr::select(ano, distrito, qtd_bf_creche_0a3, qtd_bf_pre_4a6,
                   n_escolas_novo = n_escolas),
  by = c("ano", "distrito")
) |>
  dplyr::mutate(
    fonte_bf = dplyr::case_when(
      !is.na(qtd_bf_creche_antigo) ~ "antigo_2013_2023",
      !is.na(qtd_bf_creche_0a3)    ~ "novo_2024_2026",
      TRUE                         ~ NA_character_
    )
  ) |>
  dplyr::arrange(distrito, ano) |>
  dplyr::select(distrito, ano, fonte_bf, qtd_bf_creche_antigo,
                qtd_bf_creche_0a3, qtd_bf_pre_4a6, n_escolas_novo)

message("--- 5) Painel único: ", nrow(bf_distrito_painel), " linhas ",
        "(esperado exatamente ", nrow(bf_antigo_distrito), " + ", nrow(bf_novo_distrito),
        " = ", nrow(bf_antigo_distrito) + nrow(bf_novo_distrito),
        ", já que os anos não se sobrepõem) ---")
message("    Distritos distintos no painel: ", dplyr::n_distinct(bf_distrito_painel$distrito),
        " (esperado 96)")
message("    Anos cobertos: ", paste(sort(unique(bf_distrito_painel$ano)), collapse = ", "))
message("    Linhas com AMBAS as fontes preenchidas no mesmo ano (esperado 0, período é disjunto): ",
        sum(!is.na(bf_distrito_painel$qtd_bf_creche_antigo) &
              !is.na(bf_distrito_painel$qtd_bf_creche_0a3)))
message("\n--- Amostra do painel (São Paulo, distrito Sé, todos os anos) ---")
print(as.data.frame(bf_distrito_painel |> dplyr::filter(distrito == "SE")))

# ─────────────────────────────────────────────────────────────
# 6) SALVA TUDO PARA CONFERÊNCIA (não mexe no banco final ainda)
# ─────────────────────────────────────────────────────────────
saveRDS(
  list(
    pop_municipios   = pop_municipios_final,
    pop_capitais     = pop_capitais_final,
    pnadc_capitais   = pnadc_capitais,
    bf_distrito_novo = bf_novo_distrito,
    bf_distrito_antigo_bruto = bf_antigo_classificado,
    bf_distrito_antigo = bf_antigo_distrito,
    bf_distrito_painel = bf_distrito_painel
  ),
  "PNUD/outputs/conferencia_pnadc_pop_bolsafamilia.rds"
)

readr::write_excel_csv2(bf_distrito_painel, "PNUD/outputs/bf_distrito_painel_2015_2026.csv")

message("\n=========================================================")
message("SALVO em PNUD/outputs/conferencia_pnadc_pop_bolsafamilia.rds")
message("SALVO em PNUD/outputs/bf_distrito_painel_2015_2026.csv (painel único, para conferência visual)")
message("Cole de volta TODOS os prints acima, principalmente a seção 5")
message("(painel único) — preciso confirmar a contagem de linhas e a amostra.")
message("=========================================================")
