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

decis <- seq(0.1,1,by=0.1)

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)

decilnolocal <- svyby (formula=~rmpc_real,
                       by=~Capital,
                       design=pnadc_anual_visita,
                       FUN=svyquantile,
                       quantiles=decis,
                       na.rm=TRUE)


# function ------------

renda_maxima_decil_capitais <- function(ano, entrevista=5){
  
  decis <- seq(0.1,1,by=0.1)
  
  pnadc_anual_visita <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)
  
  db <- svyby (formula=~rmpc_real,
               by=~Capital,
               design=pnadc_anual_visita,
               FUN=svyquantile,
               quantiles=decis,
               na.rm=TRUE)
  
  db <- as_tibble(db) %>% 
    mutate (ano={{ano}}, 
            entrevista={{entrevista}},
            capital=str_remove(Capital, 'Município de ')) %>%
    select (-Capital)
  
  return(db)
  
}

# loop --------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- renda_maxima_decil_capitais(i, 5)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/renda_maxima_decis_2017a2023_pnadcanual_all5i.xlsx')

# FAZENDO ANÁLISE SEM OUTLIERS --------------

remover_outliers_quartis <- function(dados) {
  # Calcula os quartis
  Q1 <- quantile(dados, 0.25, na.rm = T)
  Q3 <- quantile(dados, 0.75, na.rm = T)
  
  # Calcula o intervalo interquartil (IQR)
  IQR_value <- Q3 - Q1
  
  # Define os limites para outliers
  limite_inferior <- Q1 - 1.5 * IQR_value
  limite_superior <- Q3 + 1.5 * IQR_value
  
  # Filtra os dados removendo os outliers
  dados_filtrados <- dados[dados >= limite_inferior & dados <= limite_superior]
  
  return(dados_filtrados)
}

# teste

pnadc_anual_visita <- import ('data/pnad_anual_2023_interview5.rds')

a <- nrow (pnadc_anual_visita$variables)

decis <- seq(0.1,1,by=0.1)

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)

rmpc_real_noout <- remover_outliers_quartis(pnadc_anual_visita$variables$rmpc_real)

pnadc_anual_visita$variables <- pnadc_anual_visita$variables %>%
  filter (rmpc_real%in%rmpc_real_noout)

b <- nrow (pnadc_anual_visita$variables)

a
b

decilnolocal <- svyby (formula=~rmpc_real,
                       by=~Capital,
                       design=pnadc_anual_visita,
                       FUN=svyquantile,
                       quantiles=decis,
                       na.rm=TRUE)

