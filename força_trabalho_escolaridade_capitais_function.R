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

db <- svyby (formula=~VD3004,
             FUN=svymean,
             by=~Capital,
             design=subset(pnadc_anual_visita, VD4001=='Pessoas na força de trabalho'),
             na.rm=T,
             vartype="se")

glimpse (db)

dbt <- db %>%
  select (Capital:`VD3004Superior completo`) %>%
  pivot_longer(`VD3004Sem instrução e menos de 1 ano de estudo`:`VD3004Superior completo`) %>%
  mutate (escolaridade=str_remove(name, 'VD3004'),
          perc=value,
          capital=str_remove(Capital, 'Município de ')) %>%
  select (capital, escolaridade, perc)


dbse <- db %>%
  select (Capital, se1:se7) %>%
  pivot_longer(se1:se7) %>%
  mutate (capital=str_remove(Capital, 'Município de ')) %>%
  rename ('se'=value)

dbse$escolaridade <- case_when(dbse$name=='se1'~'Sem instrução e menos de 1 ano de estudo',
                               dbse$name=='se2'~'Fundamental incompleto ou equivalente',
                               dbse$name=='se3'~'Fundamental completo ou equivalente',
                               dbse$name=='se4'~'Médio incompleto ou equivalente',
                               dbse$name=='se5'~'Médio completo ou equivalente',
                               dbse$name=='se6'~'Superior incompleto ou equivalente',
                               dbse$name=='se7'~'Superior completo')

db <- full_join(dbt, dbse%>%select(capital, escolaridade, se))

glimpse (db)


# function ----------------

ftesc_capitais <- function(ano, entrevista=5){
  
  dataname <- paste0 ('data/pnad_anual_', {ano}, '_interview', {entrevista}, '.rds')
  
  pnadc_anual_visita <- import (dataname)
  
  db <- svyby (formula=~VD3004,
               FUN=svymean,
               by=~Capital,
               design=subset(pnadc_anual_visita, VD4001=='Pessoas na força de trabalho'),
               na.rm=T,
               vartype="se")
  
  dbt <- db %>%
    select (Capital:`VD3004Superior completo`) %>%
    pivot_longer(`VD3004Sem instrução e menos de 1 ano de estudo`:`VD3004Superior completo`) %>%
    mutate (escolaridade=str_remove(name, 'VD3004'),
            perc=value,
            capital=str_remove(Capital, 'Município de ')) %>%
    select (capital, escolaridade, perc)
  
  
  dbse <- db %>%
    select (Capital, se1:se7) %>%
    pivot_longer(se1:se7) %>%
    mutate (capital=str_remove(Capital, 'Município de ')) %>%
    rename ('se'=value)
  
  dbse$escolaridade <- case_when(dbse$name=='se1'~'Sem instrução e menos de 1 ano de estudo',
                                 dbse$name=='se2'~'Fundamental incompleto ou equivalente',
                                 dbse$name=='se3'~'Fundamental completo ou equivalente',
                                 dbse$name=='se4'~'Médio incompleto ou equivalente',
                                 dbse$name=='se5'~'Médio completo ou equivalente',
                                 dbse$name=='se6'~'Superior incompleto ou equivalente',
                                 dbse$name=='se7'~'Superior completo')
  
  db <- full_join(dbt, dbse%>%select(capital, escolaridade, se))
  
  db$ano <- {ano}
  db$entrevista <- {entrevista}
  
  return (db)
  
}

# loop -------------

db <- data.frame()

for (i in 2017:2023){
  
  temp <- ftesc_capitais(i)
  
  db <- bind_rows(db, temp)
  
  print (db%>%count(ano))
  
}

write.xlsx (db, 'extracted_data/forca_trabalho_escolaridade_capitais_anual_2017a2023_all5i.xlsx')
