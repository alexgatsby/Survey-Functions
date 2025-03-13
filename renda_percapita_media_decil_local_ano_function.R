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

# auxiliar functions --------------

attribute_quantiles <- function(data, earnings_var, n_quantiles = 4) {
  # Check if the earnings variable exists in the data
  if (!earnings_var %in% names(data)) {
    stop("The specified earnings variable does not exist in the data.")
  }
  
  # Calculate the quantiles and assign them to a new column as character
  quantile_labels <- paste0("Q", 1:n_quantiles)
  data$quantile <- as.character(cut(
    data[[earnings_var]],
    breaks = quantile(data[[earnings_var]], probs = seq(0, 1, length.out = n_quantiles + 1), na.rm = TRUE),
    labels = quantile_labels,
    include.lowest = TRUE,
    right = FALSE
  ))
  
  return(data)
}

# teste --------------

pnadc_anual_visita <- import ('data/pnad_anual_2023_interview5.rds')

decis <- seq(0.1,1,by=0.1)

pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)

pnadc_anual_visita$variables <- attribute_quantiles(data=pnadc_anual_visita$variables, earnings_var='rmpc_real', n_quantiles=10)

decilnolocal <- svyby (formula=~rmpc_real,
                       by=~Capital+quantile,
                       design=pnadc_anual_visita,
                       FUN=svymean,
                       quantiles=decis,
                       na.rm=TRUE)


# function ------------

renda_media_decil_capitais <- function(ano, entrevista=5){
  
  decis <- seq(0.1,1,by=0.1)
  
  pnadc_anual_visita <- import (here (paste0('data/pnad_anual_', {{ano}}, '_interview', {{entrevista}},'.rds')))
  
  pnadc_anual_visita$variables <- transform(pnadc_anual_visita$variables, rmpc_real=VD5005*CO1e)
  
  pnadc_anual_visita$variables <- attribute_quantiles(data=pnadc_anual_visita$variables, earnings_var='rmpc_real', n_quantiles=10)
  
  db <- svyby (formula=~rmpc_real,
               by=~Capital+quantile,
               design=pnadc_anual_visita,
               FUN=svymean,
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
  
  temp <- renda_media_decil_capitais(i, 5)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/renda_media_decis_2017a2023_pnadcanual_all5i.xlsx')
