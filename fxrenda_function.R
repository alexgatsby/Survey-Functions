#config

if (require (tidyverse) == FALSE) {install.packages("tidyverse"); require (tidyverse)}
if (require (rio) == FALSE) {install.packages("rio"); require (rio)}
if (require (here) == FALSE) {install.packages("here"); require (here)}
if (require (ggplot2) == FALSE) {install.packages("ggplot2"); require (ggplot2)}
if (require (stringr) == FALSE) {install.packages("stringr"); require (stringr)}
if (require (scales) == FALSE) {install.packages("scales"); require (scales)}
if (require (srvyr) == FALSE) {install.packages("srvyr"); require (srvyr)}
if (require (svrep) == FALSE) {install.packages("svrep"); require (svrep)}
if (require (PNADcIBGE) == FALSE) {install.packages("PNADcIBGE"); require (PNADcIBGE)}
if (require (survey) == FALSE) {install.packages("survey"); require (survey)}
if (require (convey) == FALSE) {install.packages("convey"); require (convey)}
if (require (zoo) == FALSE) {install.packages("zoo"); require (zoo)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}


rm(list = ls())
gc()

`%ni%` <- Negate(`%in%`)

# função --------------------

fx_renda_capitais <- function (ano, entrevista=5){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  data <- import (here (dataname))
  
  ext <- svyby (formula=~VD5012, by=~Capital, design=data, FUN=svymean, na.rm=TRUE)
  
  temp <- ext %>%
    as_tibble() %>%
    select (Capital:`VD5012Mais de 5 salários mínimos`) %>%
    pivot_longer (`VD5012Até ¼ salário mínimo`:`VD5012Mais de 5 salários mínimos`) %>%
    rename ('faixa_de_renda'=name,
            'perc'=value) %>%
    mutate (faixa_de_renda=str_remove(faixa_de_renda,'VD5012'),
            capital=str_remove(Capital,'Município de '),
            ano={ano}) %>%
    select (-Capital)
  
  rm (data, ext)
  
  return (temp)
  
}


# extrações e cálculos -----------------------


db <- fx_renda_capitais(2023, 5)

glimpse (db)

for (i in 2016:2022){
  
  temp <- fx_renda_capitais(i)
  
  db <- bind_rows(db, temp)
  
  rm (temp)
  
  print (db%>%count(ano))
  
}


glimpse (db)

write.csv(db, 'extracted_data/faixa_de_renda_capitais_perc_2016a2023.csv')


