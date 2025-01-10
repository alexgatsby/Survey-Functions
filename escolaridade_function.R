#config --------------

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


# função escolaridade -----------------

escolaridade_capitais <- function(ano, entrevista=5, capital='Município de Recife (PE)'){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnad <- import (here (dataname))
  
  db <- svyby(formula=~V3009A,
                by=~V3014,
                design=subset(pnad, Capital=={capital}),
                FUN=svymean,
                vartype="se",
                na.rm=TRUE)
  
  db <- db %>%
    as_tibble() %>%
    pivot_longer(V3014) %>%
    rename ('concluiu'=value) %>%
    select (concluiu, `V3009APré-escola`:V3009ADoutorado) %>% 
    pivot_longer (`V3009APré-escola`:V3009ADoutorado) %>%
    mutate (escolaridade=str_remove(name, 'V3009A'),
            percentual=value*100) %>%
    select (escolaridade, concluiu, percentual) %>%
    mutate (ano={ano},
            entrevista={entrevista})
  
  ensino_medio <- c('Antigo científico, clássico, etc. (médio 2º ciclo)',
                    'Regular do ensino médio ou do 2º grau',
                    'Educação de jovens e adultos (EJA) ou supletivo do 2º grau')
  ensino_fundamental <- c('Antigo ginásio (médio 1º ciclo)',
                          'Regular do ensino fundamental ou do 1º grau',
                          'Educação de jovens e adultos (EJA) ou supletivo do 1º grau',
                          'Antigo primário (elementar)')
  alfabetizacao <- c('Classe de alfabetização - CA',
                     'Alfabetização de jovens e adultos')
  
  db$esc_simples <- case_when(db$escolaridade%in%ensino_medio~'Ensino Médio',
                              db$escolaridade%in%ensino_fundamental~'Ensino Fundamental',
                              db$escolaridade%in%alfabetizacao~'Alfabetização',
                              db$escolaridade=='Superior - graduação'~'Graduação',
                              .default = db$escolaridade)
  
  
  return(db)
  
}


db <- escolaridade_capitais(ano=2023)


# loop


for (i in 2016:2022){
  
  esc <- escolaridade_capitais(i,5)
  
  db <- bind_rows(db, esc)
  
  print (db%>%count(ano))
  
}

glimpse (db)

db %>% count (ano, entrevista)

write.csv(db, 'extracted_data/escolaridade_recife_perc_2016a2023.csv')

