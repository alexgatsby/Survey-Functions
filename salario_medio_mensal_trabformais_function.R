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

# testando

## renda do trabalho principal: VD4016


smmtf_capitais <- function(ano, entrevista=5){

  dados <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  db <- svyby(formula=~VD4016,
              by=~Capital, 
              design=subset(dados, V4029=='Sim'), 
              FUN=svymean, 
              na.rm=TRUE)
  
  db <- as_tibble(db) %>% 
    mutate (ano={{ano}}, 
            entrevista={{entrevista}},
            Capital=str_remove(Capital, 'Município de '))
  
  colnames (db) <- c('capital', 'smmtf', 'se', 'ano', 'entrevista')
  
  return(db)
  
}

db <- smmtf_capitais(ano=2023)

glimpse (db)

# loop ------------

for (i in 2017:2022){
  temp <- smmtf_capitais(i, 5)
  db <- bind_rows(db, temp)
  print (db%>%count(ano))
}

write.xlsx (db, 'extracted_data/salario_medio_mensal_trabformais_2017a2023_pnadcanual_all5i.xlsx')
