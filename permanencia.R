## Carrega bibliotecas

library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(lubridate)
library(tidyr)
library(ggthemes)
library(data.table)
library(survival)
library(survminer)

#########
# PIBID - Permanência
##########

## Importa dados
## Dados salvo na pasta dados

load("dados/bolsas_pibid_simulada.RData")

# cria id
bolsas_pibid_simulada <- bolsas_pibid_simulada |>
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") |>
  mutate(id = paste(NM_BOLSISTA, NR_DOCUMENTO, sep="_"))

# vou usar data.table
# ou precisaria usar group_by id. Muito lento.

setDT(bolsas_pibid_simulada)

# criar um índice de tempo inteiro: ano*12 + mês
# Vai permitir ordenar dados de bolsas
bolsas_pibid_simulada[, ME_REFERENCIA := as.integer(ME_REFERENCIA)]
bolsas_pibid_simulada[, time_index := AN_REFERENCIA * 12L + ME_REFERENCIA] # para ordenar
# aqui uso o fato de que: Tempo em meses é T = 12*a + m, em que a é o ano, e m o mes.
# portanto, a diferença ou intervalo de tempo entre duas datas T2 e t1 é:
# T2 - T1 = 12*a2 + m2 - (12*a1 + 1)

# calcular permanência por bolsista
permanencia <- bolsas_pibid_simulada[
  , .(
    primeiro_tempo       = min(time_index),
    ultimo_tempo         = max(time_index),
    duracao_total_meses  = max(time_index) - min(time_index) + 1L,
    meses_pagos          = uniqueN(time_index)
  ),
  by = id
][
  , `:=`(
    proporcao_paga = meses_pagos / duracao_total_meses,
    primeiro_ano   = primeiro_tempo %/% 12L,
    primeiro_mes   = primeiro_tempo %% 12L,
    ultimo_ano     = ultimo_tempo %/% 12L,
    ultimo_mes     = ultimo_tempo %% 12L
  )
][]


# Distribuição geral da duração das bolsas (permanência média)

resumo_duracao_geral <- permanencia |>
  distinct(id, duracao_total_meses) |>
  summarise(
    n_bolsistas      = n_distinct(id),
    media_meses      = mean(duracao_total_meses, na.rm = TRUE),
    mediana_meses    = median(duracao_total_meses, na.rm = TRUE),
    p25_meses        = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses        = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    max_meses        = max(duracao_total_meses, na.rm = TRUE),
    min_meses        = min(duracao_total_meses, na.rm = TRUE)
  )

# tabela
resumo_duracao_geral

# Histograma da duração
permanencia |>
  distinct(id, duracao_total_meses) |>
  ggplot(aes(x = duracao_total_meses)) +
  geom_histogram(binwidth = 3) +
  labs(
    x = "Duração da bolsa (meses)",
    y = "Número de bolsistas únicos",
    title = "Distribuição da duração das bolsas PIBID"
  )


## Permanência por coorte de ingresso (ano de início)

duracao_por_coorte <- permanencia |>
  distinct(id, primeiro_ano, duracao_total_meses) |>
  group_by(primeiro_ano) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE)
  )

duracao_por_coorte

# Boxplot por coorte

# 3. Permanência mínima por perfil (gênero, cor, área, região)
p_coorte <- ggplot(duracao_por_coorte, aes(x = primeiro_ano)) +
  # faixa do IQR
  geom_ribbon(aes(
    ymin = p25_meses,
    ymax = p75_meses
  ), alpha = 0.2) +
  # mediana
  geom_line(aes(y = mediana_meses), linewidth = 1.2, color = "#2C3E50") +
  geom_point(aes(y = mediana_meses), size = 2, color = "#2C3E50") +
  # média
  geom_line(aes(y = media_meses), linewidth = 0.8, color = "darkred", linetype = "dashed") +
  geom_point(aes(y = media_meses), size = 2, color = "darkred") +
  labs(
    x = "Ano de ingresso no programa",
    y = "Meses de permanência",
    title = "Permanência dos bolsistas no PIBID por coorte de ingresso",
    subtitle = "Linha azul = mediana | Faixa = IQR (25%–75%) | Linha vermelha tracejada = média"
  ) +
  theme_minimal(base_size = 13)

ggsave(p_coorte, file = "outputs/p_coorte.png", width = 8, height = 4.5, scale = .7)



# Por gênero, raça e área

perfil_bolsista <- bolsas_pibid_simulada[
  , .(
    DS_TIPO_GENERO,
    NM_COR,
    DS_AREA_SUBPROJETO,
    NM_REGIAO,
    SG_UF_IES_CORRIGIDO
    
  ),
  by = id
]

# 2) juntar com a permanência calculada em data.table
permanencia_full_dt <- merge(
  permanencia,
  perfil_bolsista,
  by = "id",
  all.x = TRUE
)

#tibble:
permanencia_full <- as_tibble(permanencia_full_dt)

duracao_genero <- permanencia_full |>
  group_by(DS_TIPO_GENERO) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

duracao_genero


# Tabela por cor/raça
duracao_cor <- permanencia_full |>
  group_by(NM_COR) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

duracao_cor

# Tabela por área do subprojeto (ex: top 10 áreas mais frequentes)
top_areas <- permanencia_full |>
  count(DS_AREA_SUBPROJETO, sort = TRUE) |>
  # slice_head(n = 10) |> # se quiser deixar só o top 10
  pull(DS_AREA_SUBPROJETO)

duracao_area <- permanencia_full |>
  filter(DS_AREA_SUBPROJETO %in% top_areas) |>
  group_by(DS_AREA_SUBPROJETO) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    DS_AREA_SUBPROJETO = fct_reorder(DS_AREA_SUBPROJETO, mediana_meses)
  )

duracao_area


# Tabela por região 
duracao_regiao <- permanencia_full |>
  group_by(NM_REGIAO) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

duracao_regiao

# Tabela por UF
duracao_uf <- permanencia_full |>
  group_by(SG_UF_IES_CORRIGIDO) |>
  summarise(
    n_bolsistas   = n(),
    mediana_meses = median(duracao_total_meses, na.rm = TRUE),
    media_meses   = mean(duracao_total_meses, na.rm = TRUE),
    p25_meses     = quantile(duracao_total_meses, 0.25, na.rm = TRUE),
    p75_meses     = quantile(duracao_total_meses, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

duracao_uf

