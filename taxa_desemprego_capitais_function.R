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

#VD4002 pop ocupada vs desocupada

taxa_desemp_capitais <- function (ano, entrevista) {
  
  dados <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  prep <- convey_prep(dados)
  
  tx <- svyby(formula=~(VD4002=="Pessoas desocupadas"), denominator=~(VD4001=="Pessoas na força de trabalho"), by=~Capital, design=dados, FUN=svyratio, na.rm=TRUE)

  cols <- c('capital', 'taxa_desemprego', 'SE', 'ano')
  
  db <- as_tibble(tx) %>% mutate (ano={{ano}})
  
  colnames(db) <- cols
  
  return(db)
  
}

# série: 2017 a 2023

db <- data.frame()

for (i in 2017:2023){
  tx <- taxa_desemp_capitais(i, 5)
  db <- bind_rows(db, tx)
  print (db)
}

write.xlsx (db, 'extracted_data/taxa_desemprego_capitais_2017a2023_pnadcanual_all5i.xlsx')

# série desde 2012

db <- taxa_desemp_capitais(2016,5)

for (i in 2012:2015){
  tx <- taxa_desemp_capitais(i, 1)
  db <- bind_rows(db, tx)
  print (db)
}

write.xlsx (db, 'extracted_data/taxa_desemprego_capitais_2012a2016_pnadcanual.xlsx')


# trimestral 2024

taxa_desemp_capitais_t <- function (ano, trimestre) {
  
  dados <- import (here (paste0('data/pnad_trimestral_', {{ano}}, 
                                '_', {{trimestre}},'.rds')))
  
  prep <- convey_prep(dados)
  
  tx <- svyby(formula=~(VD4002=="Pessoas desocupadas"), 
              denominator=~(VD4001=="Pessoas na força de trabalho"), 
              by=~Capital, design=dados, FUN=svyratio, na.rm=TRUE)
  
  cols <- c('capital', 'taxa_desemprego', 'SE', 'ano', 'trimestre')
  
  db <- as_tibble(tx) %>% 
    mutate (ano={{ano}},
            trimestre={{trimestre}})
  
  colnames(db) <- cols
  
  return(db)
  
}

a <- taxa_desemp_capitais_t (2024,3)
b <- taxa_desemp_capitais_t (2024,2)
c <- taxa_desemp_capitais_t (2024,1)

db <- bind_rows(a,b,c)
write.xlsx(db, 'extracted_data/desemprego_capitais_trimestral_2024_1a3.xlsx')


