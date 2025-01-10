# config ----------------

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
if (require (cowplot) == FALSE) {install.packages("cowplot"); require (cowplot)}
if (require (openxlsx) == FALSE) {install.packages("openxlsx"); require (openxlsx)}

rm(list = ls())

# function vd4022 -------

ranking_gini_capitais <- function(ano, entrevista=5){
  
  data <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD4022, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- {{ano}}
  
  gini$ranking_menos_desigual <- rank(gini$VD4022)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD4022))
  
  return (gini)
  
}

# série: 2017 a 2023

db <- data.frame()

for (i in 2017:2023){
  tx <- ranking_gini_capitais(i, 5)
  db <- bind_rows(db, tx)
  print (db)
}

write.xlsx (db, 'extracted_data/gini_capitais_vd4022_2017a2023_pnadcanual_all5i.xlsx')

# function vd4020 -------

ranking_gini_capitais <- function(ano, entrevista=5){
  
  data <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD4020, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- {{ano}}
  
  gini$ranking_menos_desigual <- rank(gini$VD4020)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD4020))
  
  return (gini)
  
}

# série: 2017 a 2023

db <- data.frame()

for (i in 2017:2023){
  tx <- ranking_gini_capitais(i, 5)
  db <- bind_rows(db, tx)
  print (db)
}

write.xlsx (db, 'extracted_data/gini_capitais_vd4020_2017a2023_pnadcanual_all5i.xlsx')


# trimestral 2024

ranking_gini_capitais_t <- function(ano, trimestre){
  
  data <- import (here (paste0('data/pnad_trimestral_', {{ano}}, 
                               '_', {{trimestre}},'.rds')))
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD4020, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- {{ano}}
  
  gini$trimestre <- {{trimestre}}
  
  gini$ranking_menos_desigual <- rank(gini$VD4020)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD4020))
  
  return (gini)
  
}

a <- ranking_gini_capitais_t (2024,3)
b <- ranking_gini_capitais_t (2024,2)
c <- ranking_gini_capitais_t (2024,1)

db <- bind_rows(a,b,c)
write.xlsx(db, 'extracted_data/gini_capitais_vd4020_trimestral_2024_1a3.xlsx')

# function vd5005 ------------------

ranking_gini_capitais <- function(ano, entrevista=5){
  
  data <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  prep <- convey_prep(data)
  
  gini <- svyby(formula=~VD5005, by=~Capital, design=prep, FUN=svygini, na.rm=TRUE)
  
  gini <- as_tibble(gini)
  
  gini$ano <- {{ano}}
  
  gini$ranking_menos_desigual <- rank(gini$VD5005)
  
  gini$ranking_mais_desigual <- dense_rank(desc(gini$VD5005))
  
  return (gini)
  
}

# teste

db <- ranking_gini_capitais(2023)

# série: 2017 a 2023

db <- data.frame()

for (i in 2017:2023){
  tx <- ranking_gini_capitais(i, 5)
  db <- bind_rows(db, tx)
  print (db%>%count(ano))
}

write.xlsx (db, 'extracted_data/gini_capitais_vd5005_2017a2023_pnadcanual_all5i.xlsx')
