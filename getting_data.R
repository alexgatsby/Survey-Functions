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

# anual

pnad2023 <- get_pnadc(year=2023, interview=5)

saveRDS (pnad2023, 'data/pnad_anual_2023_interview5.rds')

pnad2024 <- get_pnadc(year=2024, interview=1)
saveRDS (pnad2024, 'data/pnad_anual_2024_interview1.rds')

# anual no design

pnad2023 <- get_pnadc(year=2023, interview=5, design = F)

saveRDS (pnad2023, 'data/pnad_anual_2023_interview5_nodesign.rds')

# trimestral

## 2024

pnad20241 <- get_pnadc(year=2024, quarter=1)

saveRDS(pnad20241, 'data/pnad_trimestral_2024_1.rds')

pnad20242 <- get_pnadc(year=2024, quarter=2)

saveRDS(pnad20242, 'data/pnad_trimestral_2024_2.rds')

pnad20243 <- get_pnadc(year=2024, quarter=3)

saveRDS(pnad20243, 'data/pnad_trimestral_2024_3.rds')

pnad20244 <- get_pnadc(year=2024, quarter=4)

saveRDS(pnad20244, 'data/pnad_trimestral_2024_4.rds')

## 2023

pnad20231 <- get_pnadc(year=2023, quarter=1)

saveRDS(pnad20231, 'data/pnad_trimestral_2023_1.rds')

pnad20232 <- get_pnadc(year=2023, quarter=2)

saveRDS(pnad20232, 'data/pnad_trimestral_2023_2.rds')

pnad20233 <- get_pnadc(year=2023, quarter=3)

saveRDS(pnad20233, 'data/pnad_trimestral_2023_3.rds')

pnad20234 <- get_pnadc(year=2023, quarter=4)

saveRDS(pnad20234, 'data/pnad_trimestral_2023_4.rds')