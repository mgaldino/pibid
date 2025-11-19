# educ superior

# pacotes
library(data.table)
library(dplyr)
ces <- fread("dados/MICRODADOS_2009_2024.csv") 
glimpse(ces)

# Evolução ead e presencial
# a fazer.
# matriculados_ano <- ces %>%
#   filter(TP_MODALIDADE_ENSINO == 1 & TP_GRAU_ACADEMICO %in% c(2,4)) %>% # filtra modalide presencial e licenciatura
#   group_by(NU_ANO_CENSO) %>%
#   summarise(matriculados = sum(QT_MAT),
#             matriculados_feminino = sum(QT_MAT_FEM),
#             matriculados_masculino = sum(QT_MAT_MASC))


matriculados_ano <- ces %>%
  filter(TP_MODALIDADE_ENSINO == 1 & TP_GRAU_ACADEMICO %in% c(2,4)) %>% # filtra modalide presencial e licenciatura
  group_by(NU_ANO_CENSO) %>%
  summarise(matriculados = sum(QT_MAT),
            matriculados_feminino = sum(QT_MAT_FEM),
            matriculados_masculino = sum(QT_MAT_MASC))

saveRDS(matriculados_ano, file= "dados transformados/matriculados_ano.rds")


matriculados_ano_uf <- ces %>%
  filter(TP_MODALIDADE_ENSINO == 1 & TP_GRAU_ACADEMICO %in% c(2,4)) %>% # filtra modalide presencial e licenciatura
  group_by(NU_ANO_CENSO, SG_UF) %>%
  summarise(matriculados = sum(QT_MAT),
            matriculados_feminino = sum(QT_MAT_FEM),
            matriculados_masculino = sum(QT_MAT_MASC))

saveRDS(matriculados_ano_uf, file= "dados transformados/matriculados_ano_uf.rds")

mtriculados_ano_uf_curso <- ces %>%
  filter(TP_MODALIDADE_ENSINO == 1 & TP_GRAU_ACADEMICO %in% c(2,4)) %>% # filtra modalide presencial e licenciatura
  group_by(NU_ANO_CENSO, SG_UF, NO_CURSO) %>%
  summarise(matriculados = sum(QT_MAT),
            matriculados_feminino = sum(QT_MAT_FEM),
            matriculados_masculino = sum(QT_MAT_MASC))

saveRDS(mtriculados_ano_uf_curso, file= "dados transformados/mtriculados_ano_uf_curso.rds")
