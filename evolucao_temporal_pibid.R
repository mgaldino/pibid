## Carrega bibliotecas

library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(lubridate)
library(tidyr)

#########
# PIBID
##########

## Importa dados
## Dados salvo na pasta dados

load("dados/bolsas_pibid_simulada.RData")

# opcional se não rodar o cript em ces.R
matriculados_ano <- readRDS("dados transformados/matriculados_ano.rds")
matriculados_ano_uf <- readRDS("dados transformados/matriculados_ano_uf.rds")


#####
# Reorganizando dados
#####

bolsas_pibid_simulada <- bolsas_pibid_simulada %>%
  mutate(id = paste(NM_BOLSISTA, NR_DOCUMENTO, sep="_"),
         area = case_when(grepl("LETRAS", DS_AREA_SUBPROJETO) ~ "LETRAS",
                          grepl("CAMPO", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO DO CAMPO",
                          grepl("INDÍGENA|QUILOMBOLA", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO INDÍGENA OU QUILÓMBOLA",
                          grepl("SURDOS|ESPECIAL", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO BILÍNGUE DE SURDOS OU ESPECIAL",
                          grepl("FILOSOFIA|HISTÓRIA|GEOGRAFIA|CIÊNCIAS SOCIAIS|SOCIOLOGIA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS HUMANAS",
                          grepl("AGRONOMIA|QUÍMICA|BIOLOGIA|CIÊNCIAS AGRÁRIAS|GEOLOGIA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS NATURAIS",
                          grepl("ARTES|DANÇA|TEATRO|MÚSICA", DS_AREA_SUBPROJETO) ~ "ARTES",
                          grepl("INFANTIL|ALFABETIZAÇÃO", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO INFANTIL",
                          grepl("MATEMÁTICA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS EXATAS",
                          grepl("FÍSICA$", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS EXATAS",
                          grepl("RELIG", DS_AREA_SUBPROJETO) ~ "RELIGIÃO",
                          .default = DS_AREA_SUBPROJETO)) 

# evolução temporal

# Panorama geral
p_geral <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  ggplot(aes(x=AN_REFERENCIA, y=num_bolsistas_unicos)) + geom_line() +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Ano de referência",
       y = "Número de bolsistas PIBID") +
  theme_minimal()

ggsave(p_geral, file = "outputs/p_geral.png", width = 8, height = 4.5, scale = .7)

# remuneração Um gráfico para discente e outro para discente
# olhar área com o programa diversidades

p_valor_bolsas <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA) %>%
  summarise(valor = mean(VL_BOLSISTA_PAGAMENTO)) %>%
  ggplot(aes(x=AN_REFERENCIA, y=valor)) + geom_line() +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Valor médio de bolsas PIBID R$") +
  theme_minimal()

ggsave(p_valor_bolsas, file = "outputs/p_valor_bolsas.png", width = 8, height = 4.5, scale = .7)


# perc ppi ao longo do tempo
p_ppi <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  mutate(ppi = ifelse(NM_COR %in% c("PARDA", "PRETA", "INDÍGENA"), "ppi",
                      ifelse(NM_COR %in% c("Não informado", "IGNORAD"), NA, "não ppi"))) %>%
  group_by(AN_REFERENCIA, ppi) %>%
  filter(!is.na(ppi)) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=ppi, color = ppi)) + geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "PPI") +
  theme_minimal()

ggsave(p_ppi, file = "outputs/p_ppi.png", width = 8, height = 4.5, scale = .7)

# perc gênero ao longo do tempo
p_genero <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_REFERENCIA, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=DS_TIPO_GENERO, color = DS_TIPO_GENERO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "Gênero") +
  theme_minimal()

ggsave(p_genero, file = "outputs/p_genero.png", width = 8, height = 4.5, scale = .7)

## ÁREA ##

# Macro Área ao longo do tempo
df <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, area) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final <- df %>% group_by(area) %>% slice_tail(n = 1)

p_area <- ggplot(df, aes(x = AN_REFERENCIA, y = perc, group = area, color = area)) +
  geom_line() +
  geom_text_repel(data = df_final,
                  aes(label = area),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(df$AN_REFERENCIA), max(df$AN_REFERENCIA) + 1)  # espaço pro texto

ggsave(p_area, file = "outputs/p_area.png", width = 8, height = 4.5, scale = .9)

# Micro Área ao longo do tempo
df <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, DS_AREA_SUBPROJETO, area) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total) %>%
  ungroup()

ordem <- df %>%
  group_by(DS_AREA_SUBPROJETO, area) %>%
  arrange(AN_REFERENCIA) %>%
  summarise(ultimo = last(perc), .groups = "drop") %>%
  arrange(desc(ultimo)) %>%
  pull(DS_AREA_SUBPROJETO)

# Ciências Naturais
p_micro_c_naturais <- df %>%
  filter(area == "CIÊNCIAS NATURAIS" ) %>% 
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_micro_c_naturais.png", p_micro_c_naturais, width = 8, height = 4.5, scale = .8)

# Humanas
p_micro_c_humanas <- df %>%
  filter(area == "CIÊNCIAS HUMANAS" ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_micro_c_humanas.png", p_micro_c_humanas, width = 8, height = 4.5, scale = .8)

# exatas
p_micro_c_exatas <- df %>%
  filter(area == "CIÊNCIAS EXATAS" ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_micro_c_exatas.png", p_micro_c_exatas, width = 8, height = 4.5, scale = .8)


## Outras

p_micro_outras <- df %>%
  filter(!area %in% c("CIÊNCIAS HUMANAS", "CIÊNCIAS NATURAIS") ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_micro_outras.png", p_micro_outras, width = 8, height = 4.5, scale = .8)

# todas as áreas, caso fique legível


## Area micro

df <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, DS_AREA_SUBPROJETO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

p_area_micro_todas <- ggplot(df, aes(x = AN_REFERENCIA, y = perc)) +
  geom_line() + facet_wrap(~DS_AREA_SUBPROJETO) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal()

ggsave(p_area_micro_todas, file = "outputs/p_area_micro_todas.png")

# area vs genero
p_genero_area_micro <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_REFERENCIA, DS_AREA_SUBPROJETO, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ DS_AREA_SUBPROJETO) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP",
       color = "Área") +
  theme_minimal()

ggsave(p_genero_area_micro, file = "outputs/p_genero_area_micro.png")



# Macro area vs genero
p_genero_area <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_REFERENCIA, area, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=area, color = area)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ DS_TIPO_GENERO) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "Área") +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal")

ggsave(p_genero_area, file = "outputs/p_genero_area.png", width = 8, height = 4.5, scale = .7)

# tipo de IEs ao longo do tempo
df_ies_0 <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, DS_ORGANIZACAO_ACADEMICA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final_0 <- df_ies_0 %>% group_by(DS_ORGANIZACAO_ACADEMICA) %>% slice_tail(n = 1)


p_ies <- df_ies_0 %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=DS_ORGANIZACAO_ACADEMICA, color = DS_ORGANIZACAO_ACADEMICA)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  geom_text_repel(data = df_final_0,
                  aes(label = DS_ORGANIZACAO_ACADEMICA),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "IES") +
  theme_minimal() +
  theme(
    legend.position = "none")


ggsave(p_ies, file = "outputs/p_ies.png", width = 8, height = 4.5, scale = .7)

# Tipo de IES 2 
df_ies <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, DS_CATEGORIA_ADMINISTRATIVA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final <- df_ies %>% group_by(DS_CATEGORIA_ADMINISTRATIVA) %>% slice_tail(n = 1)

p_ies2 <-  ggplot(df_ies, aes(x = AN_REFERENCIA, y = perc, 
                              group = DS_CATEGORIA_ADMINISTRATIVA, 
                              color = DS_CATEGORIA_ADMINISTRATIVA)) +
  geom_line() +
  geom_text_repel(data = df_final,
                  aes(label = DS_CATEGORIA_ADMINISTRATIVA),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(df$AN_REFERENCIA), max(df$AN_REFERENCIA) + 1)  # espaço pro texto


ggsave(p_ies2, file = "outputs/p_ies2.png", width = 8, height = 4.5, scale = .7)



###
# Região ao longo do tempo
df_regiao <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final_reg <- df_regiao %>% group_by(NM_REGIAO) %>% slice_tail(n = 1)


p_regiao <- df_regiao %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=NM_REGIAO, color = NM_REGIAO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  geom_text_repel(data = df_final_reg,
                  aes(label = NM_REGIAO),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "Região") +
  theme_minimal() +
  theme(legend.position = "none") 

ggsave(p_regiao, file = "outputs/p_regiao.png", width = 8, height = 4.5, scale = .7)


###
# UF ao longo do tempo
df_uf <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, COD_UF) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total)

p_uf <- df_uf %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=COD_UF)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ reorder(COD_UF, perc), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID",
       color = "UF") +
  theme_minimal()

ggsave(p_uf, file = "outputs/p_uf.png", width = 8, height = 4.5, scale = .7)

# Bolsistas com deficiência ao longo do tempo

p_deficiencia <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, ST_DEFICIENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  filter(ST_DEFICIENCIA == "S") %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent(), limits = c(0,.015)) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Bolsistas PIBID com deficiência",
       color = "UF") +
  theme_minimal()

ggsave(p_deficiencia, file = "outputs/p_deficiencia.png", width = 8, height = 4.5, scale = .7)



# edital geral

df <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, DS_PROJETO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id), .groups = "drop_last") %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc  = num_bolsistas_unicos / total) %>%
  ungroup() %>%
  group_by(DS_PROJETO) %>%
  mutate(total_proj = sum(num_bolsistas_unicos)) %>%      # para ordenar linhas por relevância
  ungroup() %>%
  mutate(DS_PROJETO = fct_reorder(DS_PROJETO, total_proj))

p_edital <- ggplot(df, aes(x = AN_REFERENCIA, y = DS_PROJETO, fill = perc)) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent, option = "C", direction = 1,
                       na.value = "grey95", name = "Participação") +
  labs(x = "Ano", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(p_edital, file = "outputs/p_edital.png", width = 8, height = 4.5, scale = .7)


# tempo mínimo, médio etc.
tempo_por_programa <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  mutate(data_inicio = ym(paste(AN_INICIO_BOLSA, ME_INICIO_BOLSA, sep="-")),
         data_fim = ym(paste(AN_FIM_BOLSA, ME_FIM_BOLSA, sep="-"))) %>%
  group_by(DS_PROJETO) %>% 
  summarise(q50 = as.numeric(median(data_fim - data_inicio)),
            qmin = min(as.numeric(data_fim - data_inicio)),
            q5 = as.numeric(quantile(data_fim - data_inicio, .05)),
            q25 = as.numeric(quantile(data_fim - data_inicio, .25)),
            q75 = as.numeric(quantile(data_fim - data_inicio, .75)),
            q95 = as.numeric(quantile(data_fim - data_inicio, .95)),
            qmax = max(as.numeric(data_fim - data_inicio))) %>%
  pivot_longer(!DS_PROJETO, names_to = "estatistica", values_to = "tempo_bolsa")

tempo_por_programa %>%
  ggplot(aes(x=reorder(estatistica,tempo_bolsa), y=tempo_bolsa)) + geom_col() +
  facet_wrap(~ DS_PROJETO)

##################
# Censo da Educação Superior
#############

# quadro geral
bolsistas_ces_ano <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  inner_join(matriculados_ano, join_by(AN_REFERENCIA== NU_ANO_CENSO)) %>%
  mutate(perc_matriculados = num_bolsistas_unicos/matriculados)

p_bolstistas_ces <- bolsistas_ces_ano %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent, limit = c(0, .2)) +
  labs(x = "Ano", y = "Percentual de martriculados em IES") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(p_bolstistas_ces, file = "outputs/p_bolstistas_ces.png", width = 8, height = 4.5, scale = .7)

# quadro por uf
bolsistas_ces_ano_uf <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL == "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, SG_UF_IES_CORRIGIDO, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  inner_join(matriculados_ano_uf, join_by(AN_REFERENCIA== NU_ANO_CENSO, SG_UF_IES_CORRIGIDO==SG_UF)) %>%
  mutate(perc_matriculados = num_bolsistas_unicos/matriculados)

p_bolstistas_ces_uf <- bolsistas_ces_ano_uf %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%y", date_breaks = "4 years") + 
  facet_wrap(~ reorder(SG_UF_IES_CORRIGIDO, perc_matriculados), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID matriculados em IES",
       color = "UF") + 
  theme_minimal(base_size = 11) 

ggsave(p_bolstistas_ces_uf, file = "outputs/p_bolstistas_ces_uf.png", width = 8, height = 4.5, scale = .7)


# quadro por região
p_bolsistas_ces_ano_regiao <- bolsistas_ces_ano_uf %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%y", date_breaks = "4 years") + 
  facet_wrap(~ reorder(NM_REGIAO, perc_matriculados), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID matriculados em IES",
       color = "UF") + 
  theme_minimal(base_size = 11) 

ggsave(p_bolsistas_ces_ano_regiao, file = "outputs/p_bolsistas_ces_ano_regiao.png", width = 8, height = 4.5, scale = .7)


