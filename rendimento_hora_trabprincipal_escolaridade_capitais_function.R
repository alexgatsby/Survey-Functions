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

pnadc_anual_visita <- import ('data/pnad_anual_2023_interview5.rds')

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rendatrabp=VD4017*CO1e)
pnadc_anual_visita$variables$rendatrabp <- ifelse (is.na(pnadc_anual_visita$variables$rendatrabp), 0, pnadc_anual_visita$variables$rendatrabp)


db <- svyby (formula=~rendatrabp,
             denominator=~V4039C,
             by=~VD3004,
             design=subset(pnadc_anual_visita, Capital=='Município de Recife (PE)'&V2009>=14),
             FUN=svyratio,
             vartype="se",
             na.rm=TRUE)

glimpse (db)




# function ---------

rhtpe <- function(ano, entrevista=5, capital='Município de Recife (PE)'){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnadc_anual_visita <- import (dataname)
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rendatrabp=VD4017*CO1e)
  pnadc_anual_visita$variables$rendatrabp <- ifelse (is.na(pnadc_anual_visita$variables$rendatrabp), 0, pnadc_anual_visita$variables$rendatrabp)
  
  db <- svyby (formula=~rendatrabp,
               denominator=~V4039C,
               by=~VD3004,
               design=subset(pnadc_anual_visita, Capital=={capital}&V2009>=14),
               FUN=svyratio,
               vartype="se",
               na.rm=TRUE)
  
  colnames (db) <- c('escolaridade', 'rhtpe', 'se_rhtpe')
  db$ano <- {ano}
  db$entrevista <- {entrevista}
  db$local <- {capital}
  
  return (db)
  
}

# loop -----------------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- rhtpe(i)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/rendimento_hora_trabalho_principal_escolaridaderecife_2017a2023_anual_all5i.xlsx')


