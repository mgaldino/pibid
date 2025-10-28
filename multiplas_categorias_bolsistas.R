# Docentes e supervisores nas escolas

load("dados/bolsas_pibid_simulada.RData")

# Visão geral
p_geral_docentes <- bolsas_pibid_simulada %>%
  filter(NM_NIVEL != "INICIAÇÃO A DOCÊNCIA") %>%
  group_by(AN_REFERENCIA, NM_NIVEL) %>%
  summarise(num_bolsistas_unicos = n_distinct(id)) %>%
  ggplot(aes(x=AN_REFERENCIA, y=num_bolsistas_unicos)) + geom_line() +
  scale_x_continuous(breaks = seq(2009, 2022, by=3)) + 
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Ano de referência",
       y = "Número de bolsistas de supervisão PIBID") +
  theme_minimal() + facet_wrap(~ NM_NIVEL, scales = "free_y")

## Visão por Marcos do PIBID, criado pela Beth

df <- bolsas_pibid_simulada %>%
  mutate(marcos = case_when(grepl("2007|2008|2009", DS_PROJETO) ~"Fase Inicial 07-09",
                            grepl("2010|2011|2012", DS_PROJETO) ~"Expansão IES 10-12",
                            grepl("2013|DIVERSIDADE", DS_PROJETO) ~"Fase Inclusão 13-17",
                            grepl("2018", DS_PROJETO) ~ "Integração com PRP 18-19",
                            grepl("2020|2021|2022|2023|2024", DS_PROJETO) ~ "Ciclo Formativo Contínuo - 2020...",
                            .default = DS_PROJETO),
         ordem_marcos = case_when(grepl("Inicial", marcos) ~ 1,
                                  grepl("Expansão", marcos) ~ 2,
                                  grepl("Inclusão", marcos) ~ 3,
                                  grepl("PRP", marcos) ~ 4,
                                  .default = 5)) %>%
  group_by(marcos, NM_NIVEL) %>%
  summarise(num_bolsistas_unicos = n_distinct(id), 
            ordem_marcos = max(ordem_marcos),
            .groups = "drop_last") %>%
  mutate(total = sum(num_bolsistas_unicos)) %>%
  group_by(marcos, NM_NIVEL) %>%
  mutate(total_proj = sum(num_bolsistas_unicos)) %>%  
  ungroup() %>%
  mutate(marcos = fct_reorder(marcos, ordem_marcos))

p_marcos <- ggplot(df, aes(x = marcos, y = num_bolsistas_unicos)) +
  geom_col(fill = "blue") + coord_flip() +
  facet_wrap(~NM_NIVEL, scales = "free") +
  labs(x = "Marcos do PIBID", 
       y = "Número de bolsistas") +
  theme_minimal()

ggsave(p_marcos, file = "outputs/p_marcos.png", width = 8, height = 4.5, scale = .7)

## Tempo de bolsas

tempo_por_programa <- bolsas_pibid_simulada %>%
  mutate(data_inicio = ym(paste(AN_INICIO_BOLSA, ME_INICIO_BOLSA, sep="-")),
         data_fim = ym(paste(AN_FIM_BOLSA, ME_FIM_BOLSA, sep="-"))) %>%
  group_by(DS_PROJETO, NM_NIVEL) %>% 
  summarise(q50 = as.numeric(median(data_fim - data_inicio)),
            qmin = min(as.numeric(data_fim - data_inicio)),
            q5 = as.numeric(quantile(data_fim - data_inicio, .05)),
            q25 = as.numeric(quantile(data_fim - data_inicio, .25)),
            q75 = as.numeric(quantile(data_fim - data_inicio, .75)),
            q95 = as.numeric(quantile(data_fim - data_inicio, .95)),
            qmax = max(as.numeric(data_fim - data_inicio))) %>%
  ungroup() %>%
  pivot_longer(!c("DS_PROJETO", "NM_NIVEL"), names_to = "estatistica", values_to = "tempo_bolsa")

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

# 2) Lollipop + coord_flip + facets
p_tempo_coordenador_area <- plot_df %>%
  filter(NM_NIVEL == "COORDENADOR DE ÁREA") %>% 
  ggplot(aes(x = estatistica, y = tempo_bolsa)) +
  geom_segment(aes(xend = estatistica, y = 0, yend = tempo_bolsa), linewidth = 0.9, alpha = 0.75) +
  geom_point(size = 2.6) +
  coord_flip() +
  facet_wrap(NM_NIVEL ~ DS_PROJETO) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Tempo de bolsa (dias) de coordenador de área") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())

ggsave(p_tempo_coordenador_area, file = "outputs/p_tempo_coordenador_area.png", width = 8, height = 4.5, scale = .7)

# supervisor

p_tempo_supervisor <- plot_df %>%
  filter(NM_NIVEL == "SUPERVISÃO") %>% 
  ggplot(aes(x = estatistica, y = tempo_bolsa)) +
  geom_segment(aes(xend = estatistica, y = 0, yend = tempo_bolsa), linewidth = 0.9, alpha = 0.75) +
  geom_point(size = 2.6) +
  coord_flip() +
  facet_wrap(NM_NIVEL ~ DS_PROJETO) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Tempo de bolsa (dias) do supervisor") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())

ggsave(p_tempo_coordenador_area, file = "outputs/p_tempo_coordenador_area.png", width = 8, height = 4.5, scale = .7)


# "COORDENADOR INSTITUCIONAL"

p_tempo_institucional <- plot_df %>%
  filter(NM_NIVEL == "COORDENADOR INSTITUCIONAL") %>% 
  ggplot(aes(x = estatistica, y = tempo_bolsa)) +
  geom_segment(aes(xend = estatistica, y = 0, yend = tempo_bolsa), linewidth = 0.9, alpha = 0.75) +
  geom_point(size = 2.6) +
  coord_flip() +
  facet_wrap(NM_NIVEL ~ DS_PROJETO) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Tempo de bolsa (dias) do Coord. Institucional") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())

ggsave(p_tempo_institucional, file = "outputs/p_tempo_institucional.png", width = 8, height = 4.5, scale = .7)


# "COORDENADOR INSTITUCIONAL"

p_tempo_gestao <- plot_df %>%
  filter(NM_NIVEL == "COORDENADOR DE GESTÃO") %>% 
  ggplot(aes(x = estatistica, y = tempo_bolsa)) +
  geom_segment(aes(xend = estatistica, y = 0, yend = tempo_bolsa), linewidth = 0.9, alpha = 0.75) +
  geom_point(size = 2.6) +
  coord_flip() +
  facet_wrap(NM_NIVEL ~ DS_PROJETO) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Tempo de bolsa (dias) do Coord. de Gestão") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())

ggsave(p_tempo_gestao, file = "outputs/p_tempo_gestao.png", width = 8, height = 4.5, scale = .7)
