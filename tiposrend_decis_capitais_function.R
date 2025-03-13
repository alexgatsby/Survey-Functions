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

# auxiliar functions --------------

attribute_quantiles <- function(data, earnings_var, n_quantiles = 4) {
  # Check if the earnings variable exists in the data
  if (!earnings_var %in% names(data)) {
    stop("The specified earnings variable does not exist in the data.")
  }
  
  # Calculate the quantiles and assign them to a new column as character
  quantile_labels <- paste0("Q", 1:n_quantiles)
  data$quantile <- as.character(cut(
    data[[earnings_var]],
    breaks = quantile(data[[earnings_var]], probs = seq(0, 1, length.out = n_quantiles + 1), na.rm = TRUE),
    labels = quantile_labels,
    include.lowest = TRUE,
    right = FALSE
  ))
  
  return(data)
}

# teste --------------

pnadc_anual_visita <- import ('data/pnad_anual_2023_interview5.rds')

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)

pnadc_anual_visita$variables <- attribute_quantiles(data=pnadc_anual_visita$variables, earnings_var='rmpc_real', n_quantiles=10)

# BPC

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5001A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_bpc <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_bpc) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_bpc$tipo <- 'BPC'

# PBF

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5002A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_pbf <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_pbf) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_pbf$tipo <- 'PBF'

# Outros programas

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5003A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_out <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_out) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_out$tipo <- 'Outros programas sociais'

# Aposentadoria ou pensão

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5004A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_inss <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_inss) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_inss$tipo <- 'Aposentadoria ou pensão'


# Seguro-desemprego e/ou seguro-defeso

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5005A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_seg <- svyby (formula=~real,
                  design=pnadc_anual_visita,
                  by=~quantile+Capital,
                  FUN=svymean,
                  na.rm=TRUE)

colnames (db_seg) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_seg$tipo <- 'Seguro-desemprego e/ou seguro-defeso'

# Pensão alimentícia, doação e/ou mesada

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5006A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_ali <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_ali) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_ali$tipo <- 'Pensão alimentícia, doação e/ou mesada'

# Aluguel e ou arrendamento

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5007A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_alu <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_alu) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_alu$tipo <- 'Aluguel e/ou arrendamento'

# Outros

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5008A2*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_outr <- svyby (formula=~real,
                 design=pnadc_anual_visita,
                 by=~quantile+Capital,
                 FUN=svymean,
                 na.rm=TRUE)

colnames (db_outr) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_outr$tipo <- 'Outros rendimentos como bolsas de estudo e aplicações financeiras'


# Trabalhos remunerados em dinheiro

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=VD4020*CO1e)

pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)

db_trab <- svyby (formula=~real,
                  design=pnadc_anual_visita,
                  by=~quantile+Capital,
                  FUN=svymean,
                  na.rm=TRUE)

colnames (db_trab) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
db_trab$tipo <- 'Trabalhos remunerados em dinheiro'


# juntando

db <- bind_rows(db_seg, db_pbf, db_outr, db_out, db_inss, db_bpc, db_alu, db_ali, db_trab)
glimpse (db)
db %>% count (tipo)

# function -------------------------

tiposrend_percreceb_mediarecebido_capitais_decis <- function (ano, entrevista=5){
  
  # prep
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnadc_anual_visita <- import (dataname)
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)
  
  pnadc_anual_visita$variables <- attribute_quantiles(data=pnadc_anual_visita$variables, earnings_var='rmpc_real', n_quantiles=10)
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, qcap=paste0(Capital,'_', quantile))
  
  # BPC
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5001A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_bpc <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_bpc) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_bpc$tipo <- 'BPC'
  
  # PBF
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5002A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_pbf <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_pbf) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_pbf$tipo <- 'PBF'
  
  # Outros programas
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5003A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_out <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_out) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_out$tipo <- 'Outros programas sociais'
  
  # Aposentadoria ou pensão
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5004A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_inss <- svyby (formula=~real,
                    design=pnadc_anual_visita,
                    by=~quantile+Capital,
                    FUN=svymean,
                    na.rm=TRUE)
  
  colnames (db_inss) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_inss$tipo <- 'Aposentadoria ou pensão'
  
  
  # Seguro-desemprego e/ou seguro-defeso
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5005A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_seg <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_seg) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_seg$tipo <- 'Seguro-desemprego e/ou seguro-defeso'
  
  # Pensão alimentícia, doação e/ou mesada
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5006A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_ali <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_ali) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_ali$tipo <- 'Pensão alimentícia, doação e/ou mesada'
  
  # Aluguel e ou arrendamento
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5007A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_alu <- svyby (formula=~real,
                   design=pnadc_anual_visita,
                   by=~quantile+Capital,
                   FUN=svymean,
                   na.rm=TRUE)
  
  colnames (db_alu) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_alu$tipo <- 'Aluguel e/ou arrendamento'
  
  # Outros
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=V5008A2*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_outr <- svyby (formula=~real,
                    design=pnadc_anual_visita,
                    by=~quantile+Capital,
                    FUN=svymean,
                    na.rm=TRUE)
  
  colnames (db_outr) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_outr$tipo <- 'Outros rendimentos como bolsas de estudo e aplicações financeiras'
  
  
  # Trabalhos remunerados em dinheiro
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, real=VD4020*CO1e)
  
  pnadc_anual_visita$variables$real <- ifelse (is.na(pnadc_anual_visita$variables$real), 0, pnadc_anual_visita$variables$real)
  
  db_trab <- svyby (formula=~real,
                    design=pnadc_anual_visita,
                    by=~quantile+Capital,
                    FUN=svymean,
                    na.rm=TRUE)
  
  colnames (db_trab) <- c('decil', 'capital', 'media_recebido_tiporenda', 'se_media_recebido_tiporenda')
  db_trab$tipo <- 'Trabalhos remunerados em dinheiro'
  
  
  # juntando
  
  db <- bind_rows(db_seg, db_pbf, db_outr, db_out, db_inss, db_bpc, db_alu, db_ali, db_trab)
  
  db$ano <- {ano}
  db$entrevista <- {entrevista}
  db$capital <- str_remove(db$capital, 'Município de ')
  
  return (db)
  
  
}

# loop ------------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- tiposrend_percreceb_mediarecebido_capitais_decis(i)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/tiposrend_mediarecebido_decis_capitais_2017a2023_anual_all5i.xlsx')
  
