##################

# Author : Renan Morais / Eduardo Minsky
# Date: 20-02-2024
# Email: 
# Goal: clear database atlas seguro rural
# resource: https://dados.agricultura.gov.br/dataset/sisser3

# Modified by Julia Niemeyer
# Date: 28/05/2025

########################### Libraries ######################################
pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               magrittr, 
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler,
               readxl)

################## directories ###################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_atlas_raw <- ("A:/finance/atlas_Seguro_Rural/rawData")
dir_atlas_output <- ("A:/finance/atlas_Seguro_Rural/cleanData")
dir_atlas_doc <- ("A:/finance/atlas_Seguro_Rural/_documentation")


############### import database #####################
setwd(dir_atlas_raw)

df_atlas_2006_2015 <- 
    read_excel("psrdadosabertos2006a2015excel.xlsx") %>% 
    janitor::clean_names()


df_atlas_2016_2021 <- 
    read_excel("psrdadosabertos2016a2021excel.xlsx") %>% 
    janitor::clean_names()

df_atlas_2022_2023 <- 
    read_excel("psrdadosabertos2022a2023excel.xlsx") %>% 
    janitor::clean_names() 

df_atlas_2024 <- 
    read_excel("psr-2024.xlsx") %>% 
    janitor::clean_names()

#clean and transform variables

df_atlas_2006_2015 <- df_atlas_2006_2015 %>% 
    mutate(nr_decimal_longitude = "-",
           nr_decimal_latitude = "-")

df_atlas_2016_2021 <- df_atlas_2016_2021 %>% 
    mutate(nr_decimal_longitude = "-",
           nr_decimal_latitude = "-")

df_atlas_2022_2023 <- df_atlas_2022_2023 %>% 
    mutate(valor_indenizacao = "-",
           evento_preponderante = "-")

df_atlas_2024 <- df_atlas_2024 %>% 
    mutate(valor_indenizacao = "-",
           evento_preponderante = "-")




df_atlas_full <- rbind(df_atlas_2006_2015, df_atlas_2016_2021, df_atlas_2022_2023, df_atlas_2024)

#df_atlas_full <- rbind(df_atlas_full, df_atlas_2022)

df_atlas_full <- df_atlas_full %>% 
    mutate(vl_subvencao_federal = ifelse(is.na(df_atlas_full$vl_subvencao_federal), "0", vl_subvencao_federal)) %>% 
    mutate(vl_subvencao_federal = gsub(",",".", vl_subvencao_federal)) %>% 
    mutate(vl_subvencao_federal = as.numeric(vl_subvencao_federal))

df_atlas_full <- df_atlas_full %>% 
    mutate(nm_classif_produto = if_else(is.na(nm_classif_produto), "NÃO INFORMADO", nm_classif_produto))

#sum(df_atlas_full$vl_subvencao_federal, na.rm = T)

############describe class of variables ###################

#print(lapply(df_atlas_full,class))


###########################################

setwd(dir_atlas_output)

saveRDS(df_atlas_full, "atlas_2006_2024_clear.rds")

write.xlsx(df_atlas_full, "atlas_2006_2024_clear.xlsx")


