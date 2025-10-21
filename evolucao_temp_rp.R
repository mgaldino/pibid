library(dplyr)
library(ggplot2)
library(ggrepel)

df2 <- load("dados/bolsas_rp_simulada.RData")
glimpse(bolsas_rp_simulada)

#####
# Reorganizando dados

bolsas_rp_simulada <- bolsas_rp_simulada %>%
  mutate(id = paste(NM_BOLSISTA, NR_DOCUMENTO, sep="_"),
         area = case_when(grepl("LETRAS", DS_AREA_SUBPROJETO) ~ "LETRAS",
                          grepl("CAMPO", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO DO CAMPO",
                          grepl("INDÍGENA|QUILOMBOLA", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO INDÍGENA OU QUILÓMBOLA",
                          grepl("SURDOS|ESPECIAL", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO BILÍNGUE DE SURDOS OU ESPECIAL",
                          grepl("FILOSOFIA|HISTÓRIA|GEOGRAFIA|CIÊNCIAS SOCIAIS|SOCIOLOGIA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS HUMANAS",
                          grepl("AGRONOMIA|QUÍMICA|BIOLOGIA|CIÊNCIAS AGRÁRIAS|GEOLOGIA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS NATURAIS",
                          grepl("ARTES|DANÇA|TEATRO|MÚSICA", DS_AREA_SUBPROJETO) ~ "ARTES",
                          grepl("INFANTIL|ALFABETIZAÇÃO|", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO INFANTIL",
                          grepl("MATEMÁTICA|^FÍSICA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS EXATAS",
                          grepl("RELIG", DS_AREA_SUBPROJETO) ~ "RELIGIÃO",
                          .default = DS_AREA_SUBPROJETO)) 

# evolução temporal

# Panorama geral
p_geral_rp <- bolsas_rp_simulada %>%
  group_by(AN_INICIO_BOLSA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=num_bolsistas_unicos)) + geom_line() +
  scale_x_continuous(breaks = seq(2016, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Número de bolsistas RP") +
  theme_minimal()

ggsave(p_geral_rp, file = "outputs/p_geral_rp.png")

# remuneração
p_valor_bolsas_rp <- bolsas_rp_simulada %>%
  group_by(AN_INICIO_BOLSA) %>%
  summarise(valor = mean(VL_BOLSISTA_PAGAMENTO)) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=valor)) + geom_line() +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Valor médio de bolsas RP R$") +
  theme_minimal()

ggsave(p_valor_bolsas_rp, file = "outputs/p_valor_bolsas_rp.png")

# perc ppi ao longo do tempo
p_ppi_rp <- bolsas_rp_simulada %>%
  mutate(ppi = ifelse(NM_COR %in% c("PARDA", "PRETA", "INDÍGENA"), "ppi",
                      ifelse(NM_COR %in% c("Não informado", "IGNORAD"), NA, "não ppi"))) %>%
  group_by(AN_INICIO_BOLSA, ppi) %>%
  filter(!is.na(ppi)) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=ppi, color = ppi)) + geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "PPI") +
  theme_minimal()

ggsave(p_ppi_rp, file = "outputs/p_ppi_rp.png")

# perc genero ao longo do tempo
p_genero_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=DS_TIPO_GENERO, color = DS_TIPO_GENERO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "Gênero") +
  theme_minimal()

ggsave(p_genero_rp, file = "outputs/p_genero_rp.png")

# Área ao longo do tempo
df <- bolsas_rp_simulada %>%
  group_by(AN_INICIO_BOLSA, area) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final <- df %>% group_by(area) %>% slice_tail(n = 1)

p_area_rp <- ggplot(df, aes(x = AN_INICIO_BOLSA, y = perc, group = area, color = area)) +
  geom_line() +
  geom_text_repel(data = df_final,
                  aes(label = area),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(df$AN_INICIO_BOLSA), max(df$AN_INICIO_BOLSA) + 1)  # espaço pro texto

ggsave(p_area_rp, file = "outputs/p_area_rp.png")

# area vs genero
p_genero_area_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, area, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=area, color = area)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ DS_TIPO_GENERO) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "Área") +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal")

ggsave(p_genero_area_rp, file = "outputs/p_genero_area_rp.png")

# tipo de IEs ao longo do tempo

p_ies_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, DS_ORGANIZACAO_ACADEMICA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=DS_ORGANIZACAO_ACADEMICA, color = DS_ORGANIZACAO_ACADEMICA)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "IES") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal")


ggsave(p_ies_rp, file = "outputs/p_ies_rp.png")


###
# Região ao longo do tempo
p_regiao_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=NM_REGIAO, color = NM_REGIAO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "Região") +
  theme_minimal()

ggsave(p_regiao_rp, file = "outputs/p_regiao_rp.png")


###
# UF ao longo do tempo
df_uf <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, COD_UF) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total)

p_uf_rp <- df_uf %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=COD_UF)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ reorder(COD_UF, perc), ncol = 6, scales = "free_y") +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP",
       color = "UF") +
  theme_minimal()

ggsave(p_uf_rp, file = "outputs/p_uf_rp.png")

# Bolsistas com deficiência ao longo do tempo

p_deficiencia_rp <- bolsas_rp_simulada %>%
  group_by(AN_INICIO_BOLSA, ST_DEFICIENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  filter(ST_DEFICIENCIA == "S") %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent(), limits = c(0,.015)) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas RP com deficiência",
       color = "UF") +
  theme_minimal()

ggsave(p_deficiencia_rp, file = "outputs/p_deficiencia_rp.png")



# edital

df <- bolsas_rp_simulada %>%
  group_by(AN_INICIO_BOLSA, DS_PROJETO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id), .groups = "drop_last") %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc  = num_bolsistas_unicos / total) %>%
  ungroup() %>%
  group_by(DS_PROJETO) %>%
  mutate(total_proj = sum(num_bolsistas_unicos)) %>%      # para ordenar linhas por relevância
  ungroup() %>%
  mutate(DS_PROJETO = fct_reorder(DS_PROJETO, total_proj))

p_edital_rp <- ggplot(df, aes(x = AN_INICIO_BOLSA, y = DS_PROJETO, fill = perc)) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent, option = "C", direction = 1,
                       na.value = "grey95", name = "Participação") +
  labs(x = "Ano", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(p_edital_rp, file = "outputs/p_edital_rp.png")

