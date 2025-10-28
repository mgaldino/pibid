## Carrega bibliotecas

library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)

## Importa dados
## Dados salvo na pasta dados

load("dados/bolsas_rp_simulada.RData")

# opcional se não rodar o cript em ces.R
matriculados_ano <- readRDS("dados transformados/matriculados_ano.rds")
matriculados_ano_uf <- readRDS("dados transformados/matriculados_ano_uf.rds")

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
                          grepl("INFANTIL|ALFABETIZAÇÃO", DS_AREA_SUBPROJETO) ~ "EDUCAÇÃO INFANTIL",
                          grepl("MATEMÁTICA", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS EXATAS",
                          grepl("FÍSICA$", DS_AREA_SUBPROJETO) ~ "CIÊNCIAS EXATAS",
                          grepl("RELIG", DS_AREA_SUBPROJETO) ~ "RELIGIÃO",
                          .default = DS_AREA_SUBPROJETO)) 

bolsas_rp_simulada <- bolsas_rp_simulada %>%
  filter(NM_NIVEL == "RESIDENTE") # só discentes bolsistas


# evolução temporal

# Panorama geral
p_geral_rp <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  ggplot(aes(x=AN_REFERENCIA, y=num_bolsistas_unicos)) + geom_line() +
  scale_x_continuous(breaks = seq(2016, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Número de bolsistas PRP") +
  theme_minimal()

ggsave(p_geral_rp, file = "outputs/p_geral_rp.png")

# remuneração
p_valor_bolsas_rp <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA) %>%
  summarise(valor = mean(VL_BOLSISTA_PAGAMENTO)) %>%
  ggplot(aes(x=AN_REFERENCIA, y=valor)) + geom_line() +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Valor médio de bolsas PRP R$") +
  theme_minimal()

ggsave(p_valor_bolsas_rp, file = "outputs/p_valor_bolsas_rp.png")

# perc ppi ao longo do tempo
p_ppi_rp <- bolsas_rp_simulada %>%
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
       y = "Percentual de bolsistas PRP",
       color = "PPI") +
  theme_minimal()

ggsave(p_ppi_rp, file = "outputs/p_ppi_rp.png")

# perc gênero ao longo do tempo
p_genero_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_REFERENCIA, DS_TIPO_GENERO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=DS_TIPO_GENERO, color = DS_TIPO_GENERO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP",
       color = "Gênero") +
  theme_minimal()

ggsave(p_genero_rp, file = "outputs/p_genero_rp.png")

# Área macro ao longo do tempo
df <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, area) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

df_final <- df %>% group_by(area) %>% slice_tail(n = 1)

p_area_rp <- ggplot(df, aes(x = AN_REFERENCIA, y = perc, group = area, color = area)) +
  geom_line() +
  geom_text_repel(data = df_final,
                  aes(label = area),
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(min(df$AN_REFERENCIA), max(df$AN_REFERENCIA) + 1)  # espaço pro texto

ggsave(p_area_rp, file = "outputs/p_area_rp.png")


# Micro Área ao longo do tempo
df <- bolsas_rp_simulada %>%
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
p_rp_micro_c_naturais <- df %>%
  filter(area == "CIÊNCIAS NATURAIS" ) %>% 
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_rp_micro_c_naturais.png", p_rp_micro_c_naturais, width = 8, height = 4.5, scale = .8)

# Humanas
p_rp_micro_c_humanas <- df %>%
  filter(area == "CIÊNCIAS HUMANAS" ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_rp_micro_c_humanas.png", p_rp_micro_c_humanas, width = 8, height = 4.5, scale = .8)

# exatas
p_rp_micro_c_exatas <- df %>%
  filter(area == "CIÊNCIAS EXATAS" ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_rp_micro_c_exatas.png", p_rp_micro_c_exatas, width = 8, height = 4.5, scale = .8)


## Outras

p_rp_micro_outras <- df %>%
  filter(!area %in% c("CIÊNCIAS HUMANAS", "CIÊNCIAS NATURAIS") ) %>%
  mutate(DS_AREA_SUBPROJETO = factor(DS_AREA_SUBPROJETO, levels = ordem)) %>%
  ggplot(aes(x = AN_REFERENCIA, y = perc, group = DS_AREA_SUBPROJETO)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ DS_AREA_SUBPROJETO, scales = "free_y", ncol = 5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(x = "Ano de referência", y = "% de bolsistas PIBID") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 8))

ggsave("outputs/p_rp_micro_outras.png", p_rp_micro_outras, width = 8, height = 4.5, scale = .8)

# todas as áreas, caso fique legível


## Area micro

df <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, DS_AREA_SUBPROJETO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos / total)

p_area_micro_prp <- ggplot(df, aes(x = AN_REFERENCIA, y = perc)) +
  geom_line() + facet_wrap(~DS_AREA_SUBPROJETO) +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_minimal()

ggsave(p_area_micro_prp, file = "outputs/p_area_micro_prp.png")

# micro area vs genero
p_genero_area_micro_prp <- bolsas_rp_simulada %>%
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

ggsave(p_genero_area_micro_prp, file = "outputs/p_genero_area_micro_prp.png")



# area vs genero
p_genero_area_rp <- bolsas_rp_simulada %>%
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
       y = "Percentual de bolsistas PRP",
       color = "Área") +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal")

ggsave(p_genero_area_rp, file = "outputs/p_genero_area_rp.png")

# tipo de IEs ao longo do tempo

p_ies_rp <- bolsas_rp_simulada %>%
  filter(DS_TIPO_GENERO != "Não informado") %>%
  group_by(AN_REFERENCIA, DS_ORGANIZACAO_ACADEMICA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=DS_ORGANIZACAO_ACADEMICA, color = DS_ORGANIZACAO_ACADEMICA)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP",
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
  group_by(AN_REFERENCIA, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=NM_REGIAO, color = NM_REGIAO)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP",
       color = "Região") +
  theme_minimal()

ggsave(p_regiao_rp, file = "outputs/p_regiao_rp.png")


###
# UF ao longo do tempo
df_uf <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, COD_UF) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total)

p_uf_rp <- df_uf %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc, group=COD_UF)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  facet_wrap(~ reorder(COD_UF, perc), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP",
       color = "UF") +
  theme_minimal()

ggsave(p_uf_rp, file = "outputs/p_uf_rp.png")

# Bolsistas com deficiência ao longo do tempo

p_deficiencia_rp <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, ST_DEFICIENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc = num_bolsistas_unicos/total) %>%
  filter(ST_DEFICIENCIA == "S") %>%
  ggplot(aes(x=AN_REFERENCIA, y=perc)) + 
  geom_line() + scale_y_continuous(labels = scales::label_percent(), limits = c(0,.015)) +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PRP com deficiência",
       color = "UF") +
  theme_minimal()

ggsave(p_deficiencia_rp, file = "outputs/p_deficiencia_rp.png")



# edital

df <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, DS_PROJETO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id), .groups = "drop_last") %>%
  mutate(total = sum(num_bolsistas_unicos),
         perc  = num_bolsistas_unicos / total) %>%
  ungroup() %>%
  group_by(DS_PROJETO) %>%
  mutate(total_proj = sum(num_bolsistas_unicos)) %>%      # para ordenar linhas por relevância
  ungroup() %>%
  mutate(DS_PROJETO = fct_reorder(DS_PROJETO, total_proj))

p_edital_rp <- ggplot(df, aes(x = AN_REFERENCIA, y = DS_PROJETO, fill = perc)) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent, option = "C", direction = 1,
                       na.value = "grey95", name = "Participação no PRP") +
  labs(x = "Ano", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(p_edital_rp, file = "outputs/p_edital_rp.png")


# tempo mínimo, médio etc.
tempo_por_programa <- bolsas_rp_simulada %>%
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

ord <- c("qmin","q5","q25","q50","q75","q95","qmax")
rotulos <- c(
  qmin = "Mínimo",
  q5   = "5º percentil",
  q25  = "25º (Q1)",
  q50  = "Mediana (Q2)",
  q75  = "75º (Q3)",
  q95  = "95º percentil",
  qmax = "Máximo"
)

plot_df <- tempo_por_programa %>%
  mutate(estatistica = factor(estatistica, levels = ord, labels = rotulos))

# 2) Lollipop 
p_tempo_discente <- plot_df %>%
  ggplot(aes(x = estatistica, y = tempo_bolsa)) +
  geom_segment(aes(xend = estatistica, y = 0, yend = tempo_bolsa), linewidth = 0.9, alpha = 0.75) +
  geom_point(size = 2.6) +
  coord_flip() +
  facet_wrap(~DS_PROJETO) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Tempo de bolsa do discente (dias)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())

ggsave(p_tempo_discente, file = "outputs/p_tempo_discente.png", width = 8, height = 4.5, scale = .7)


ggsave(p_tempo, file = "outputs/p_tempo.png")


##################
# Censo da Educação Superior
#############

# quadro geral
bolsistas_ces_ano_rp <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  inner_join(matriculados_ano, join_by(AN_REFERENCIA== NU_ANO_CENSO)) %>%
  mutate(perc_matriculados = num_bolsistas_unicos/matriculados)

p_bolstistas_ces_prp <- bolsistas_ces_ano_rp %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent, limit = c(0, .2)) +
  labs(x = "Ano", y = "Percentual de martriculados em IES") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(p_bolstistas_ces_prp, file = "outputs/p_bolstistas_ces_prp.png", width = 8, height = 4.5, scale = .7)

# quadro por uf
bolsistas_ces_ano_uf_prp <- bolsas_rp_simulada %>%
  group_by(AN_REFERENCIA, SG_UF_IES_CORRIGIDO, NM_REGIAO) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  inner_join(matriculados_ano_uf, join_by(AN_REFERENCIA== NU_ANO_CENSO, SG_UF_IES_CORRIGIDO==SG_UF)) %>%
  mutate(perc_matriculados = num_bolsistas_unicos/matriculados)

p_bolstistas_ces_uf_prp <- bolsistas_ces_ano_uf_prp %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%y", date_breaks = "4 years") + 
  facet_wrap(~ reorder(SG_UF_IES_CORRIGIDO, perc_matriculados), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID matriculados em IES",
       color = "UF") + 
  theme_minimal(base_size = 11) 

ggsave(p_bolstistas_ces_uf_prp, file = "outputs/p_bolstistas_ces_uf_prp.png", width = 8, height = 4.5, scale = .7)


# quadro por região
p_bolsistas_ces_ano_regiao_prp <- bolsistas_ces_ano_uf_prp %>%
  mutate(data = lubridate::ym(paste(AN_REFERENCIA, "01", sep="-"))) %>%
  ggplot(aes(x=data, y = perc_matriculados)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%y", date_breaks = "4 years") + 
  facet_wrap(~ reorder(NM_REGIAO, perc_matriculados), ncol = 6, scales = "free_y") +
  labs(x = "Ano de referência",
       y = "Percentual de bolsistas PIBID matriculados em IES",
       color = "UF") + 
  theme_minimal(base_size = 11) 

ggsave(p_bolsistas_ces_ano_regiao_prp, file = "outputs/p_bolsistas_ces_ano_regiao_prp.png", width = 8, height = 4.5, scale = .7)


