#config ---------

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

# testando ---------------

# VD5005

pnad <- import ('data/pnad_anual_2023_interview5.rds')

db <- svyby (formula=~VD5005,
             by=~Capital,
             design=pnad,
             FUN=svymean,
             na.rm=T)

glimpse (db)

db <- svyby (formula=~VD5005,
             by=~UF,
             design=pnad,
             FUN=svymean,
             na.rm=T)

# function ------------------

rmpc_capitais <- function(ano, entrevista=5){
  
  dados <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  dados$variables <- transform(dados$variables, VD5005=VD5005*CO1e)
  
  db <- svyby (formula=~VD5005,
               by=~Capital,
               design=dados,
               FUN=svymean,
               na.rm=T)
  
  db <- as_tibble(db) %>% 
    mutate (ano={{ano}}, 
            entrevista={{entrevista}},
            Capital=str_remove(Capital, 'Município de '))
  
  colnames (db) <- c('capital', 'rmpc', 'se', 'ano', 'entrevista')
  
  return(db)
  
}

rmpc_estados <- function(ano, entrevista=5){
  
  dados <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  dados$variables <- transform(dados$variables, VD5005=VD5005*CO1e)
  
  db <- svyby (formula=~VD5005,
               by=~UF,
               design=dados,
               FUN=svymean,
               na.rm=T)
  
  db <- as_tibble(db) %>% 
    mutate (ano={{ano}}, 
            entrevista={{entrevista}})
  
  colnames (db) <- c('uf', 'rmpc', 'se', 'ano', 'entrevista')
  
  return(db)
  
}

# loop ------------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- rmpc_capitais(i, 5)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/renda_media_percapita_capitais_2017a2023_pnadcanual_all5i.xlsx')

db <- data.frame()

for (i in 2017:2023){
  
  temp <- rmpc_estados(i, 5)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/renda_media_percapita_estados_2017a2023_pnadcanual_all5i.xlsx')

