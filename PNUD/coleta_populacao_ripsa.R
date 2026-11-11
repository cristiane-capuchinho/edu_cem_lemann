# =============================================================
# COLETA — População de 0 a 3, 4 a 5 e 6 anos por município,
# 2015-2025, das estimativas populacionais do Ministério da Saúde
# (as mesmas do indicador RIPSA de população residente)
#
# Fonte: API de Dados Abertos do Ministério da Saúde
#   https://apidadosabertos.saude.gov.br/vigilancia-e-meio-ambiente/svsa-populacao-residente
# A API traz, para cada município, ano (2000-2025), sexo e idade
# simples (0 a 80, sendo 80 = "80 anos e mais"), a população
# estimada. São as mesmas estimativas do arquivo RIPSA de população
# residente (ripsa014dm.csv, Ministério da Saúde): conferidas par a
# par para 0-3 e 4-5 anos, 2016-2024, com resultado idêntico.
#
# Diferença em relação às colunas pop_0a3_mun / pop_4a5_mun
# (coleta_pnadc_pop_bolsafamilia.R): aquelas aplicam a razão etária
# do Censo 2022 sobre a faixa 0-4 anual do brpop; estas vêm direto
# da idade simples estimada pelo MS em cada ano, o que permite
# também separar a idade de 6 anos.
#
# Saída: PNUD/outputs/populacao_ripsa_0a6_municipios_2015_2025.csv
#   uma linha por município x ano, com pop_0a3_ripsa, pop_4a5_ripsa
#   e pop_6_ripsa (soma dos dois sexos)
#
# Observações sobre a API (testadas):
# - offset é o número de REGISTROS a pular (não o número da página,
#   como diz a documentação);
# - consultas filtradas só por ano exigem offsets altos e ficam
#   lentas (~10 s por página); por isso a consulta é feita município
#   a município: cada município tem 4.212 registros (26 anos x 2
#   sexos x 81 idades) = 5 páginas de 1.000;
# - o download é guardado em cache por lotes (PNUD/cache_populacao_ms/,
#   não versionado): se for interrompido, basta rodar de novo.
# =============================================================

# Localiza a raiz do projeto subindo diretorios
for (r in c(".", "..", "../..", "../../..")) {
  if (dir.exists(file.path(r, "PNUD")) && file.exists(file.path(r, "run.R"))) {
    setwd(r); break
  }
}
message("Raiz do projeto: ", normalizePath("."))

pacotes <- c("dplyr", "httr2", "jsonlite", "purrr", "readr", "tibble")
for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)

URL_API     <- "https://apidadosabertos.saude.gov.br/vigilancia-e-meio-ambiente/svsa-populacao-residente"
ANOS_PAINEL <- 2015:2025
IDADE_MAX   <- 6
PASTA_CACHE <- "PNUD/cache_populacao_ms"
ARQ_SAIDA   <- "PNUD/outputs/populacao_ripsa_0a6_municipios_2015_2025.csv"

dir.create(PASTA_CACHE, showWarnings = FALSE, recursive = TRUE)
dir.create("PNUD/outputs", showWarnings = FALSE, recursive = TRUE)

message("=========================================================")
message("1) Lista de municípios (API de Localidades do IBGE)")
message("=========================================================")

municipios <- jsonlite::fromJSON(
  "https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado"
) |>
  tibble::as_tibble() |>
  transmute(co_municipio = as.integer(`municipio-id`), no_municipio = `municipio-nome`,
            sg_uf = `UF-sigla`)

message("    Municípios: ", nrow(municipios))

message("\n=========================================================")
message("2) Download da API do MS, em lotes de 200 municípios")
message("=========================================================")

paginas <- seq(0, 4000, by = 1000)
lotes   <- split(municipios$co_municipio, ceiling(seq_along(municipios$co_municipio) / 200))

for (i in seq_along(lotes)) {
  arquivo_lote <- file.path(PASTA_CACHE, sprintf("lote_%03d.rds", i))
  if (file.exists(arquivo_lote)) next

  grade <- expand.grid(co_municipio = lotes[[i]], offset = paginas)
  reqs  <- purrr::map(seq_len(nrow(grade)), function(j) {
    httr2::request(URL_API) |>
      httr2::req_url_query(cod_mun = grade$co_municipio[j], limit = 1000,
                           offset = grade$offset[j]) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_timeout(120)
  })
  resps <- httr2::req_perform_parallel(reqs, max_active = 6, on_error = "continue",
                                       progress = FALSE)

  falhas <- !vapply(resps, inherits, logical(1), "httr2_response")
  if (any(falhas)) {
    stop("Lote ", i, ": ", sum(falhas), " requisições falharam. Rode de novo para retomar ",
         "(os lotes já baixados ficam no cache).")
  }

  dados <- purrr::map_dfr(resps, function(r) {
    tibble::as_tibble(jsonlite::fromJSON(httr2::resp_body_string(r))[[1]])
  })
  saveRDS(dplyr::filter(dados, ano %in% ANOS_PAINEL, idade <= IDADE_MAX), arquivo_lote)
  message("    Lote ", i, "/", length(lotes), " ok")
}

pop <- purrr::map_dfr(list.files(PASTA_CACHE, pattern = "^lote_.*\\.rds$", full.names = TRUE),
                      readRDS) |>
  transmute(co_municipio = as.integer(cod_mun), ano = as.integer(ano), sexo, idade = as.integer(idade),
            pop = as.numeric(pop))

message("\n=========================================================")
message("3) Validação")
message("=========================================================")

registros <- count(pop, co_municipio, ano)
incompletos <- municipios |>
  left_join(count(registros, co_municipio, name = "n_anos"), by = "co_municipio") |>
  filter(is.na(n_anos) | n_anos < length(ANOS_PAINEL))

stopifnot(
  # todos os municípios da lista do IBGE foram baixados
  all(municipios$co_municipio %in% pop$co_municipio),
  # cada município x ano tem (IDADE_MAX + 1) idades x 2 sexos
  all(registros$n == (IDADE_MAX + 1) * 2),
  # sem duplicatas
  !anyDuplicated(select(pop, co_municipio, ano, sexo, idade)),
  !anyNA(pop$pop)
)
message("    OK: todos os ", nrow(municipios), " municípios baixados, ",
        (IDADE_MAX + 1) * 2, " registros (idades x sexos) por município x ano, sem duplicatas.")
message("    Municípios sem a série completa ", min(ANOS_PAINEL), "-", max(ANOS_PAINEL),
        " (instalados depois do início do painel; esperado: 5101837 Boa Esperança do Norte/MT, só 2025):")
print(as.data.frame(incompletos))

message("\n=========================================================")
message("4) Agregação por faixa etária")
message("=========================================================")

pop_faixas <- pop |>
  group_by(co_municipio, ano) |>
  summarise(pop_0a3_ripsa = sum(pop[idade <= 3]),
            pop_4a5_ripsa = sum(pop[idade %in% 4:5]),
            pop_6_ripsa   = sum(pop[idade == 6]),
            .groups = "drop") |>
  left_join(municipios, by = "co_municipio") |>
  select(co_municipio, no_municipio, sg_uf, ano, pop_0a3_ripsa, pop_4a5_ripsa, pop_6_ripsa) |>
  arrange(co_municipio, ano)

message("    Linhas (município x ano): ", nrow(pop_faixas))
message("\n--- Totais Brasil por ano ---")
print(as.data.frame(pop_faixas |>
  group_by(ano) |>
  summarise(municipios = n(), pop_0a3 = sum(pop_0a3_ripsa), pop_4a5 = sum(pop_4a5_ripsa),
            pop_6 = sum(pop_6_ripsa))))

message("\n--- São Paulo (3550308), conferência ---")
print(as.data.frame(filter(pop_faixas, co_municipio == 3550308)))

readr::write_csv(pop_faixas, ARQ_SAIDA)
message("\nSALVO: ", ARQ_SAIDA)
