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

tx_informalidade_capitais_anual <- function(ano, entrevista=5){
  
  dados <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  tx <- svyby(formula=~(VD4009=="Trabalhador doméstico sem carteira de trabalho assinada" |
                        (VD4009 == "Empregado no setor privado sem carteira de trabalho assinada") |
                        (VD4009 == "Trabalhador familiar auxiliar") |
                        (VD4009 == "Conta-própria" & V4019 == "Não")|
                        (VD4009 == "Empregador" & V4019 == "Não")), 
              denominator=~(VD4002=="Pessoas ocupadas"), 
              by=~Capital, design=dados, FUN=svyratio, na.rm=TRUE)
  
  cols <- c('capital', 'taxa_informalidade', 'se', 'ano', 'entrevista')
  
  db <- as_tibble(tx) %>% 
    mutate (ano={{ano}},
            entrevista={{entrevista}})
  
  colnames(db) <- cols
  
  return(db)
  
}

# loop --------------

db <- data.frame()

for (i in 2016:2023){
  
  temp <- tx_informalidade_capitais_anual (ano=i)
  
  db <- bind_rows (db, temp)
  
  print (db%>%count(ano))
  
}

for (i in 2012:2015){
  
  temp <- tx_informalidade_capitais_anual (ano=i, entrevista=1)
  
  db <- bind_rows (db, temp)
  
  print (db%>%count(ano))
  
}

saveRDS(db, 'extracted_data/taxa_informalidade_capitais_anual_atualizado.rds')


db %>% filter (capital=='Município de Recife (PE)') %>% data.frame()
