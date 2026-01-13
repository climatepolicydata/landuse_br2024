##################

# Author : Renan Morais
# Date: 30.05.2023
# Email: renanflorias@hotmail.com
# Goal: transformação da base SES para landscape

## Modified by Julia Niemeyer
## Date: 28/05/2025



########################### Libraries ######################################
pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               deflateBR,
               httr,
               rjson,
               magrittr, 
               tibble, 
               Matrix,
               data.table,
               pivottabler,
               readr,
               dplyr)

########################### ACTION NEEDED ######################################
ano_ini = 2022 #the initial year to start analysis
ano_fim = 2024 #the final year to end your analysis

########################### Directories ########################################
root = 'A:\\finance\\ses\\'

dir_susep_dt_clean = paste0(root,'cleanData\\')

dir_susep_raw = paste0(root,'rawData\\')

########### import database #########
setwd(dir_susep_raw)

ses_seguros2 <- read_csv2("Ses_seguros.csv")

## Get unique values of coenti code
code_coenti <- read.csv("Ses_grupos_economicos.csv", sep = ";", encoding = "latin1") %>%
    select(coenti, noenti) %>%
    mutate(coenti = as.numeric(coenti)) %>%
    distinct()

##################### filters and transforms ###########

ses_seguros2 <- ses_seguros2 %>%
   mutate(ano = as.numeric(substr(damesano, 0, 4)))


##################### transforms ##################### 

ses_seguros2_clean<- ses_seguros2 %>%
 mutate(coenti = as.numeric(substr(coenti, 1, 6)))

#glimpse(ses_seguros2_clean)

ses_seguros2_clean_coenti <- merge(ses_seguros2_clean, code_coenti,
                                      by = "coenti")

## ver como fazer essa análise:
"algumas observações foram perdidas devido ao coenti não corresponder a nenhuma nome de instituição

Todas as observações perdidas estão contidas entre os anos de XXXXXX"
#glimpse(ses_seguros2_clean_coenti)

"criação id"

# ses_seguros_filter_landscape <- ses_seguros_filter_landscape %>%
#   group_by(coenti, coramo, ano) %>%
#   dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
#   ungroup()
#
# "agregando por coramo"
#
# df_ses_agregado <- aggregate(premio_direto ~ id_equals + coenti + coramo + ano + noenti, data = ses_seguros_filter_landscape, FUN = sum)

#df_ses_agregado <- merge(df_ses_agregado, codes2, by = "coramo")

#df_ses_agregado_clear <- df_ses_agregado





write.csv2(ses_seguros2_clean_coenti, paste0(dir_susep_dt_clean, "ses_clear_", ano_fim, ".csv"), fileEncoding = "latin1") 

