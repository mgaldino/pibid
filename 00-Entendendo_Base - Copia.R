setwd("T:/")
options(scipen = 999)
library(tidyverse)
library(data.table)
library(vroom)

# Salvando CPFs
# pibid <- vroom('2. Bases_Externas/bolsas_pibid_MASC_xxxxxxx.csv')     # valor ocultado pelo sedap
# prp <- vroom("2. Bases_Externas/bolsas_rp_MASC_xxxxxxx.csv")    # valor ocultado pelo sedap
# 
# cpf <- bind_rows(pibid,prp) %>% filter(grepl("INICI|RESIDE",NM_NIVEL)) %>% select(NR_DOCUMENTO) %>% distinct() %>% unlist()
# length(cpf)
# # 472.551
# 
# saveRDS(as.data.frame(cpf), "T:/Capes/PIBID/00-Bases/cpfs.rds")

cpf <- readRDS("T:/Capes/PIBID/00-Bases/cpfs.rds") %>% select(cpf) %>% unlist() 

# sugestão pull
cpf <- readRDS("T:/Capes/PIBID/00-Bases/cpfs.rds") %>%
  distinct(cpf) %>% 
  pull(cpf)

# sugestão transfor cpf em data.table
cpf_dt <- data.table(CPF_MASC = cpf)
rm(cpf)

arquivos <- list.files("T:/Capes/Inicial/00-Bases/SUP_ALUNO")
arquivos <- arquivos[grepl("SUP",arquivos)]

# Filtrando CES 
# sugestão usar join com cpf_dt no data.table
# deveria ser mais rápido que %in%
sup <- NULL
for(i in arquivos){
  sup1 <- readRDS(paste0("T:/Capes/Inicial/00-Bases/SUP_ALUNO/",i))
  setDT(sup1)
  sup1 <- sup1[cpf_dt, on = "CPF_MASC", nomatch = 0]
  sup <- bind_rows(sup,sup1)
  gc()
  rm(sup1)
}
setDT(sup)
# Conclusão ----
# CPFs no CES
table(duplicated(sup$CPF_MASC))
#   FALSE    TRUE 
# 454.914 2851370 

table(duplicated(sup$CPF_MASC))[1]/length(cpf)*100
# table(duplicated(sup$CPF_MASC))[1]/nrow(cpf_dt)*100 
# 96.26 % de encontrados 

table(sup$IN_CONCLUINTE)
#       0        1 
# 2959822  346.462 

# Quantidade de graduações
sup <- sup[, qt_grad := .N, by= .(CPF_MASC,IN_CONCLUINTE)]

sup[IN_CONCLUINTE == 1, uniqueN(CPF_MASC), by = .(qt_grad)][order(qt_grad)]
#    qt_grad     V1
# <int>  <int>
# 1:       1 242368
# 2:       2  43228
# 3:       3   4652
# 4:       4    643
# 5:       5    145
# 6:       6     37
# 7:       7     17
# 8:       8      2
# 9:       9      2
# 10:      10     1

# Quantidade de licenciatura 
sup <- sup[, qt_grad_tp := .N, by= .(CPF_MASC,IN_CONCLUINTE,TP_GRAU_ACADEMICO)]
sup[IN_CONCLUINTE == 1 & TP_GRAU_ACADEMICO == 2, uniqueN(CPF_MASC), by = .(qt_grad_tp)][order(qt_grad_tp)]
# qt_grad_tp     V1
# <int>  <int>
# 1:          1 246684
# 2:          2  18550
# 3:          3   1361
# 4:          4    172
# 5:          5     29
# 6:          6     16
# 7:          7      4
# 8:         10      1

table(duplicated(paste(sup$CPF_MASC,sup$IN_CONCLUINTE)),sup$IN_CONCLUINTE)
#             0       1
# FALSE  454454  291095
# TRUE  2505368   55367
# 291.095 concluíram 

table(sup$IN_CONCLUINTE,sup$TP_SITUACAO)
#         2       3       4       5       6       7
# 0 2325420  238955  351824   43294       0     329
# 1       0       0       0       0  346462       0
# Ou seja, se é IN_CONCLUINTE == 1 é formado (TP_SITUACAO = 6)

table(duplicated(paste(sup$CPF_MASC,sup$IN_CONCLUINTE)),sup$TP_SITUACAO)
#             2       3       4       5       6       7
# FALSE  410867   14370   28578     631  291095       8

table(duplicated(paste(sup$CPF_MASC,sup$IN_CONCLUINTE)),sup$TP_GRAU_ACADEMICO,sup$IN_CONCLUINTE)
# , ,  = 1
#             1       2       3       4
# FALSE   27831  254788    8232     144
# TRUE    19216   34046    2095       3
# 254.788 cpf licenciatura

# Área ----
teste <- sup %>% select(CPF_MASC,CO_CINE_ROTULO,IN_CONCLUINTE) %>% distinct()
dim(teste)
# [1] 814.469      2

AUX_CINE_BRASIL <- read_delim("AUX_CINE_BRASIL.csv", delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
teste <- teste %>% left_join(AUX_CINE_BRASIL) %>% select(CPF_MASC,NO_CINE_ROTULO,IN_CONCLUINTE)
teste$NO_CINE_ROTULO <- str_to_upper(teste$NO_CINE_ROTULO)
# Joining with `by = join_by(CO_CINE_ROTULO)`

pibid <- vroom('2. Bases_Externas/bolsas_pibid_MASC_20291022152002.csv')
parea <- pibid %>% select(NR_DOCUMENTO, DS_AREA_SUBPROJETO) %>% distinct()
dim(parea)
# 484.438      2
length(unique(parea$DS_AREA_SUBPROJETO))
# [1] 52

comum <- intersect(str_to_upper(unique(AUX_CINE_BRASIL$NO_CINE_ROTULO)),str_to_upper(unique(parea$DS_AREA_SUBPROJETO)))
length(comum)
# [1] 32
# [1] "BIOLOGIA"                                    "QUÍMICA"                                    
# [3] "GEOLOGIA"                                    "FÍSICA"                                     
# [5] "MATEMÁTICA"                                  "PEDAGOGIA"                                  
# [7] "ARTES CÊNICAS"                               "DANÇA"                                      
# [9] "MÚSICA"                                      "TEATRO"                                     
# [11] "CIÊNCIAS DA RELIGIÃO"                        "HISTÓRIA"                                   
# [13] "FILOSOFIA"                                   "LETRAS ALEMÃO"                              
# [15] "LETRAS ESPANHOL"                             "LETRAS FRANCÊS"                             
# [17] "LETRAS INGLÊS"                               "LETRAS ITALIANO"                            
# [19] "LETRAS LÍNGUA BRASILEIRA DE SINAIS"          "LETRAS OUTRAS LÍNGUAS ESTRANGEIRAS MODERNAS"
# [21] "LETRAS PORTUGUÊS"                            "LETRAS PORTUGUÊS FRANCÊS"                   
# [23] "LETRAS PORTUGUÊS INGLÊS"                     "AGRONOMIA"                                  
# [25] "ENFERMAGEM"                                  "EDUCAÇÃO FÍSICA"                            
# [27] "CIÊNCIAS SOCIAIS"                            "GEOGRAFIA"                                  
# [29] "SOCIOLOGIA"                                  "PSICOLOGIA"                                 
# [31] "ARTES"                                       "ARTES VISUAIS" 

names(teste) <- c(names(parea),"IN_CONCLUINTE")
fim <- parea %>% inner_join(teste)
nrow(fim)
# [1] 134.754

table(fim$IN_CONCLUINTE)
#     0     1 
# 82645 52109 
# Match sem tratamento 

# 1) licenciatura ----
# Pegando o pegando o ultimo ano de bolsa recebida
setDT(pibid)
fpibid <- pibid[order(-AN_REFERENCIA), .SD[1], by =.(NR_DOCUMENTO, DS_PROJETO)]
dim(fpibid)
# [1] 539.438     22

lic1 <- sup[IN_CONCLUINTE == 1 & TP_GRAU_ACADEMICO == 2 & qt_grad_tp ==1  & qt_grad  == 1 & CPF_MASC %in% unique(fpibid$NR_DOCUMENTO)]
table(duplicated(lic1$CPF_MASC))
# FALSE 
# 199698 

f <- fpibid %>% inner_join(lic1, by = c("NR_DOCUMENTO" = "CPF_MASC"))
dim(f)
# [1] 228.122    137

f$dif_ano <- f$NU_ANO_CENSO - f$AN_REFERENCIA
table(f$dif_ano)

#-16   -15   -14   -13   -12   -11   -10    -9    -8    -7    -6    -5    -4    -3    -2 
# 27    63   128   201   267   394   488   522   581   665   732   873   971  1105  1241 

#    -1     0     1     2     3     4     5     6     7     8     9    10    11    12    13 
# 28893 62903 49793 37227 19597 10541  4993  2747  1327   799   544   298   113    66    14 

# 14    15 
# 8     1 

round(prop.table(table(f$dif_ano))*100,1)

# -16  -15  -14  -13  -12  -11  -10   -9   -8   -7   -6   -5   -4   -3   -2   -1    0    1    2    3    4    5    6 
# 0.0  0.0  0.1  0.1  0.1  0.2  0.2  0.2  0.3  0.3  0.3  0.4  0.4  0.5  0.5 12.7 27.6 21.8 16.3  8.6  4.6  2.2  1.2 
#   7    8    9   10   11   12   13   14   15 
# 0.6  0.4  0.2  0.1  0.0  0.0  0.0  0.0  0.0 
# 74.3% entre 0 e 3
# 87% entre -1 e 3


# Separa quem tem só 1 
# 


# Questões:
# 1) Corte de CPFs será em quais anos? Será pelo ano de ingresso ou pelo ano da bolsa? 
# 2) Correspondencia de área
# 3) Escolha de graduação



# mais de um edital em areas diferentes ou iguais












