## Comparativo entre PIBID e PRP

## Carrega bibliotecas

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)

load("dados/bolsas_rp_simulada.RData")
load("dados/bolsas_pibid_simulada.RData")

bolsas_rp_simulada_discentes <- bolsas_rp_simulada %>%
  filter(NM_NIVEL == "RESIDENTE")  # só discentes bolsistas

bolsas_pibid_simulada_discentes <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") 

bolsas_pibid_prp <- bind_rows(bolsas_rp_simulada_discentes,
                              bolsas_pibid_simulada) %>%
  mutate(id = paste(NM_BOLSISTA, NR_DOCUMENTO, sep="_"))



# Bolsistas por programa


# Bolsistas por programa ao longo do tempo
df_programas <- bolsas_pibid_prp %>%
  group_by(AN_REFERENCIA, NM_PROGRAMA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

p_pogramas <- df_programas %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=NM_PROGRAMA, color = NM_PROGRAMA)) + 
  geom_line() + geom_point() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas",
       color = "Programa") +
  theme_minimal() +
  theme(
    legend.position = "bottom")

ggsave(p_pogramas, file = "outputs/p_pogramas.png", width = 8, height = 4.5, scale = .7)

## Núm IES
df_ies <- bolsas_pibid_prp %>%
  group_by(AN_REFERENCIA, NM_PROGRAMA) %>%
  summarise(num_ies_unicos = n_distinct(NM_IES_CORRIGIDO)) %>%
  mutate(total = sum(num_ies_unicos),
         perc = num_bolsistas_unicos / total)

p_evol_ies <- df_ies %>%
  ggplot(aes(x=AN_REFERENCIA, y=total, group=NM_PROGRAMA, color = NM_PROGRAMA)) + 
  geom_line() + geom_point()  +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Número de IES com bolsistas discentes",
       color = "Programa") +
  theme_minimal() +
  theme(
    legend.position = "bottom")
  
ggsave(p_evol_ies, file = "outputs/p_evol_ies.png", width = 8, height = 4.5, scale = .7)


