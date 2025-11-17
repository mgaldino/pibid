
library(ggplot2)

## 1. Ler a tabela colada do clipboard
## (copie a tabela, incluindo o cabeçalho, e rode a linha abaixo)
dados_uf <- read.table(
  pipe("pbpaste"),      # pega o conteúdo da área de transferência no macOS
  header = TRUE,
  sep = "\t",
  dec = ",",
  stringsAsFactors = FALSE
)


dados_uf <- dados_uf |>
  mutate(
    regiao = case_when(
      SG_UF_IES_CORRIGIDO %in% c("AC","AP","AM","PA","RO","RR","TO") ~ "Norte",
      SG_UF_IES_CORRIGIDO %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
      SG_UF_IES_CORRIGIDO %in% c("DF","GO","MS","MT") ~ "Centro-Oeste",
      SG_UF_IES_CORRIGIDO %in% c("ES","MG","RJ","SP") ~ "Sudeste",
      SG_UF_IES_CORRIGIDO %in% c("PR","RS","SC") ~ "Sul",
      TRUE ~ NA_character_
    )
  )

## 3. Ordenar UFs dentro de cada região pela mediana
dados_uf <- dados_uf |>
  arrange(regiao, mediana_meses) |>
  mutate(
    SG_UF_IES_CORRIGIDO = factor(SG_UF_IES_CORRIGIDO,
                                 levels = SG_UF_IES_CORRIGIDO),
    regiao = factor(regiao,
                    levels = c("Norte","Nordeste","Centro-Oeste","Sudeste","Sul"))
  )

## 4. Passar para formato longo: média, mediana e p75 em uma coluna
dados_long <- dados_uf |>
  select(SG_UF_IES_CORRIGIDO, regiao,
         mediana_meses, p75_meses) |>
  pivot_longer(
    cols = c(mediana_meses, p75_meses),
    names_to = "estatistica",
    values_to = "meses"
  ) |>
  mutate(
    estatistica = factor(
      estatistica,
      levels = c( "mediana_meses", "p75_meses"),
      labels = c("Mediana", "Percentil 75")
    )
  )

## 5. Gráfico: três barras por UF, facet por região
p_uf_permanencia <- ggplot(dados_long,
       aes(x = SG_UF_IES_CORRIGIDO,
           y = meses,
           fill = estatistica)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ regiao,
             scales = "free_x",
             ncol = 2,
             nrow=3) +
  labs(
    title = "Permanência dos bolsistas por UF da IES",
    x = "UF da IES",
    y = "Meses de permanência",
    fill = "Estatística"
  ) +
  scale_fill_manual(
    values = c(
      "Mediana"      = "#1F77B4",  # azul forte
      "Percentil 75" = "#FFD700"   # amarelo alto contraste
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

ggsave(p_uf_permanencia, file = "outputs/p_uf_permanencia.png", width = 8, height = 10, scale = 1)


## área


dados_area <- read.table(
  pipe("pbpaste"),
  header=TRUE,
  sep="\t",
  dec = ",",
  stringsAsFactors = FALSE
)

dados_area_1k <- dados_area %>%
  filter(n_bolsistas > 1000)

dados_area_1k %>%
  arrange(mediana_meses)

dados_area_1k %>%
  arrange(p75_meses)
  