# config ------------------

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
gc()

# function ------------------

taxa_pobreza_capitais <- function(ano, entrevista=5, linhap=667){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnad <- import (here (dataname))
  
  pnad$variables <- transform(pnad$variables, VD5005_real=VD5005*CO1e)
  
  db <- svyby (formula=~(VD5005_real <= {linhap}), 
               by=~interaction(Capital),
               design=pnad, FUN=svymean, na.rm=T)
  
  colunas_raw <- c("capital", "fora_linha", "dentro_linha", "se1", "se2")
  colnames (db) <- colunas_raw
  
  db$ano <- {ano}
  db$entrevista <- {entrevista}
  db$linhap <- {linhap}
  
  return (db)
  
}

# testing ------------------

db <- taxa_pobreza_capitais(ano=2023)

glimpse (db)

# loop ---------------

for (i in 2016:2022){
  
  temp <- taxa_pobreza_capitais(i)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

glimpse (db)

db %>% count (ano, entrevista)

write.csv(db, 'extracted_data/taxa_pobreza_capitais_linha667_2016a2023.csv')

# loop extrema pobreza -----------

db <- data.frame()

for (i in 2016:2023){
  
  temp <- taxa_pobreza_capitais(ano=i, linhap=209)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

glimpse (db)

db %>% count (ano, entrevista)

write.csv(db, 'extracted_data/taxa_extpobreza_capitais_linha209_2016a2023.csv')
