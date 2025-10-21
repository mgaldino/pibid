library(dplyr)
library(ggplot2)
library(ggrepel)

df1 <- load("dados/bolsas_pibid_simulada.RData")
df2 <- load("dados/bolsas_rp_simulada.RData")
glimpse(bolsas_pibid_simulada)

#####
# Reorganizando dados

bolsas_pibid_simulada <- bolsas_pibid_simulada %>%
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
p_geral <- bolsas_pibid_simulada %>%
  group_by(AN_INICIO_BOLSA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=num_bolsistas_unicos)) + geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID") +
  theme_minimal()

ggsave(p_geral, file = "outputs/p_geral.png")
# perc ppi ao longo do tempo
bolsas_pibid_simulada %>%
  mutate(ppi = ifelse(NM_COR %in% c("PARDA", "PRETA", "INDÍGENA"), "ppi",
                      ifelse(NM_COR %in% c("Não informado", "IGNORAD"), NA, "não ppi"))) %>%
  group_by(AN_INICIO_BOLSA, ppi) %>%
  filter(!is.na(ppi)) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=ppi, color = ppi)) + geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID",
       color = "ppi") +
  theme_minimal()

# perc sexo ao longo do tempo
bolsas_pibid_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=DS_TIPO_GENERO, color = DS_TIPO_GENERO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID",
       color = "Gênero") +
  theme_minimal()

# Área ao longo do tempo
df <- bolsas_pibid_simulada %>%
  group_by(AN_INICIO_BOLSA, area) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final <- df %>% group_by(area) %>% slice_tail(n = 1)

ggplot(df, aes(x = AN_INICIO_BOLSA, y = perc, group = area, color = area)) +
  geom_line() +
  geom_text_repel(data = df_final,
                  aes(label = area),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(df$AN_INICIO_BOLSA), max(df$AN_INICIO_BOLSA) + 1)  # espaço pro texto

# tipo de IEs ao longo do tempo

bolsas_pibid_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, DS_ORGANIZACAO_ACADEMICA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=DS_ORGANIZACAO_ACADEMICA, color = DS_ORGANIZACAO_ACADEMICA)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID",
       color = "IES") +
  theme_minimal()

###
# Região ao longo do tempo
bolsas_pibid_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=NM_REGIAO, color = NM_REGIAO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID",
       color = "Região") +
  theme_minimal()

###
# UF ao longo do tempo
df_uf <- bolsas_pibid_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_INICIO_BOLSA, COD_UF) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total)

df_uf %>%
  ggplot(aes(x=AN_INICIO_BOLSA, y=perc, group=COD_UF)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~ reorder(COD_UF, perc), ncol = 6, scales = "free_y") +
  labs(x = "Ano de início da bolsa",
       y = "Percentual de bolsistas PIBID",
       color = "UF") +
  theme_minimal()

