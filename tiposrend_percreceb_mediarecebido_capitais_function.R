# pacotes -----------------

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (srvyr) == FALSE) {install.packages("srvyr"); require (srvyr)}
if (require (svrep) == FALSE) {install.packages("svrep"); require (svrep)}
if (require (PNADcIBGE) == FALSE) {install.packages("PNADcIBGE"); require (PNADcIBGE)}
if (require (survey) == FALSE) {install.packages("survey"); require (survey)}
if (require (convey) == FALSE) {install.packages("convey"); require (convey)}
if (require (zoo) == FALSE) {install.packages("zoo"); require (zoo)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}


rm(list = ls())

`%ni%` <- Negate(`%in%`)

# teste --------------

pnadc_anual_visita <- import ('data/pnad_trimestral_2024_3.rds')

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5001A2*CO1e)

db_bpc1 <- svyby (formula=~real,
                 design=subset(pnadc_anual_visita, V5001A=='Sim'),
                 by=~Capital,
                 FUN=svymean)

db_bpc2 <- svyby (formula=~V5001A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_bpc <- full_join(db_bpc1, db_bpc2)

colnames (db_bpc) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_bpc$tipo <- 'BPC'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5002A2*CO1e)

db_pbf1 <- svyby (formula=~real,
                  design=subset(pnadc_anual_visita, V5002A=='Sim'),
                  by=~Capital,
                  FUN=svymean)

db_pbf2 <- svyby (formula=~V5002A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_pbf <- full_join(db_pbf1, db_pbf2)

colnames (db_pbf) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_pbf$tipo <- 'PBF'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5003A2*CO1e)

db_out1 <- svyby (formula=~real,
                  design=subset(pnadc_anual_visita, V5003A=='Sim'),
                  by=~Capital,
                  FUN=svymean)

db_out2 <- svyby (formula=~V5003A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_out <- full_join(db_out1, db_out2)

colnames (db_out) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_out$tipo <- 'Outros programas sociais'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5004A2*CO1e)

db_inss1 <- svyby (formula=~real,
                   design=subset(pnadc_anual_visita, V5004A=='Sim'),
                   by=~Capital,
                   FUN=svymean)

db_inss2 <- svyby (formula=~V5004A,
                   design=pnadc_anual_visita,
                   by=~Capital,
                   FUN=svymean)

db_inss <- full_join(db_inss1, db_inss2)

colnames (db_inss) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_inss$tipo <- 'Aposentadoria ou pensão'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5005A2*CO1e)

db_seg1 <- svyby (formula=~real,
                  design=subset(pnadc_anual_visita, V5005A=='Sim'),
                  by=~Capital,
                  FUN=svymean)

db_seg2 <- svyby (formula=~V5005A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_seg <- full_join(db_seg1, db_seg2)

colnames (db_seg) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_seg$tipo <- 'Seguro-desemprego e/ou seguro-defeso'

# ----

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5006A2*CO1e)

db_ali1 <- svyby (formula=~real,
                  design=subset(pnadc_anual_visita, V5006A=='Sim'),
                  by=~Capital,
                  FUN=svymean)

db_ali2 <- svyby (formula=~V5006A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_ali <- full_join(db_ali1, db_ali2)

colnames (db_ali) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_ali$tipo <- 'Pensão alimentícia, doação e/ou mesada'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5007A2*CO1e)

db_alu1 <- svyby (formula=~real,
                  design=subset(pnadc_anual_visita, V5007A=='Sim'),
                  by=~Capital,
                  FUN=svymean)

db_alu2 <- svyby (formula=~V5007A,
                  design=pnadc_anual_visita,
                  by=~Capital,
                  FUN=svymean)

db_alu <- full_join(db_alu1, db_alu2)

colnames (db_alu) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_alu$tipo <- 'Aluguel e ou arrendamento'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5008A2*CO1e)

db_outr1 <- svyby (formula=~real,
                   design=subset(pnadc_anual_visita, V5008A=='Sim'),
                   by=~Capital,
                   FUN=svymean)

db_outr2 <- svyby (formula=~V5008A,
                   design=pnadc_anual_visita,
                   by=~Capital,
                   FUN=svymean)

db_outr <- full_join(db_outr1, db_outr2)

colnames (db_outr) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
db_outr$tipo <- 'Outros rendimentos como bolsas de estudo e aplicações financeiras'

# ---

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=VD4020*CO1e)

db_trab <- svyby (formula=~real,
                   design=subset(pnadc_anual_visita, V4001=='Sim'),
                   by=~Capital,
                   FUN=svymean,
                   na.rm=T)

colnames (db_outr) <- c('capital', 'rend', 'se_rend')
db_trab$tipo <- 'Trabalhos Remunerados em Dinheiro'

# jutando ---

db <- bind_rows(db_seg, db_pbf, db_outr, db_out, db_inss, db_bpc, db_alu, db_ali, db_trab)
glimpse (db)
db %>% count (tipo)

# function ------------------

tiposrend_percreceb_mediarecebido_capitais <- function (ano, entrevista=5){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnadc_anual_visita <- import (dataname)
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5001A2*CO1e)
  
  db_bpc1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5001A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_bpc2 <- svyby (formula=~V5001A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_bpc <- full_join(db_bpc1, db_bpc2)
  
  colnames (db_bpc) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_bpc$tipo <- 'BPC'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5002A2*CO1e)
  
  db_pbf1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5002A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_pbf2 <- svyby (formula=~V5002A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_pbf <- full_join(db_pbf1, db_pbf2)
  
  colnames (db_pbf) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_pbf$tipo <- 'PBF'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5003A2*CO1e)
  
  db_out1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5003A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_out2 <- svyby (formula=~V5003A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_out <- full_join(db_out1, db_out2)
  
  colnames (db_out) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_out$tipo <- 'Outros programas sociais'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5004A2*CO1e)
  
  db_inss1 <- svyby (formula=~real,
                     design=subset(pnadc_anual_visita, V5004A=='Sim'),
                     by=~Capital,
                     FUN=svymean)
  
  db_inss2 <- svyby (formula=~V5004A,
                     design=pnadc_anual_visita,
                     by=~Capital,
                     FUN=svymean)
  
  db_inss <- full_join(db_inss1, db_inss2)
  
  colnames (db_inss) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_inss$tipo <- 'Aposentadoria ou pensão'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5005A2*CO1e)
  
  db_seg1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5005A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_seg2 <- svyby (formula=~V5005A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_seg <- full_join(db_seg1, db_seg2)
  
  colnames (db_seg) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_seg$tipo <- 'Seguro-desemprego e/ou seguro-defeso'
  
  # ----
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5006A2*CO1e)
  
  db_ali1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5006A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_ali2 <- svyby (formula=~V5006A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_ali <- full_join(db_ali1, db_ali2)
  
  colnames (db_ali) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_ali$tipo <- 'Pensão alimentícia, doação e/ou mesada'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5007A2*CO1e)
  
  db_alu1 <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V5007A=='Sim'),
                    by=~Capital,
                    FUN=svymean)
  
  db_alu2 <- svyby (formula=~V5007A,
                    design=pnadc_anual_visita,
                    by=~Capital,
                    FUN=svymean)
  
  db_alu <- full_join(db_alu1, db_alu2)
  
  colnames (db_alu) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_alu$tipo <- 'Aluguel e ou arrendamento'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5008A2*CO1e)
  
  db_outr1 <- svyby (formula=~real,
                     design=subset(pnadc_anual_visita, V5008A=='Sim'),
                     by=~Capital,
                     FUN=svymean)
  
  db_outr2 <- svyby (formula=~V5008A,
                     design=pnadc_anual_visita,
                     by=~Capital,
                     FUN=svymean)
  
  db_outr <- full_join(db_outr1, db_outr2)
  
  colnames (db_outr) <- c('capital', 'rend', 'se_rend', 'perc_recebe', 'perc_nao_recebe', 'se_perc_recebe', 'se_perc_nao_recebe')
  db_outr$tipo <- 'Outros rendimentos como bolsas de estudo e aplicações financeiras'
  
  # ---
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=VD4020*CO1e)
  
  db_trab <- svyby (formula=~real,
                    design=subset(pnadc_anual_visita, V4001=='Sim'),
                    by=~Capital,
                    FUN=svymean,
                    na.rm=T)
  
  colnames (db_trab) <- c('capital', 'rend', 'se_rend')
  db_trab$tipo <- 'Trabalhos Remunerados em Dinheiro'
  
  # jutando ---
  
  db <- bind_rows(db_seg, db_pbf, db_outr, db_out, db_inss, db_bpc, db_alu, db_ali, db_trab)
  
  rm (db_seg1, db_pbf1, db_outr1, db_out1, db_inss1, db_bpc1, db_alu1, db_ali1,
      db_seg2, db_pbf2, db_outr2, db_out2, db_inss2, db_bpc2, db_alu2, db_ali2)
  
  db$ano <- {ano}
  db$entrevista <- {entrevista}
  db$capital <- str_remove(db$capital, 'Município de ')
  
  return (db)
  
}


# loop ------------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- tiposrend_percreceb_mediarecebido_capitais(i)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/tiposrend_percreceb_mediarecebido_capitais_2017a2023_anual_all5i.xlsx')

