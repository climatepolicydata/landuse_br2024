##############################################################################
# Author : Renan Florias
# Date: 08.07.2024
# Email: renanflorias@hotmail.com
# Goal: join international databases

########################### Libraries ######################################

########################### Libraries ######################################
pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_internationals <- ("A:/projects/landuse_br2024/internacionais")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb\\output")

# dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")

###### import datasets #############

setwd(dir_internationals)

df_kfw <- read.xlsx("all_international_landscape.xlsx", sheet = "kfw") 

df_kfw <- df_kfw[-1,]

df_giz <- read.xlsx("all_international_landscape.xlsx", sheet = "GIZ")

df_norad <- read.xlsx("all_international_landscape.xlsx", sheet = "NORAD")


df_wb <- read.xlsx("all_international_landscape.xlsx", sheet = "world_bank")

df_gef <- read.xlsx("all_international_landscape.xlsx", sheet = "GEF")

# setwd(dir_oecd_output)
# 
# df_oecd_calculus <-readRDS("df_ocde_landscape_final_join_year_2022.rds")

# setwd(dir_idb_output)
#   
# df_idb_calculus<- readRDS("data_idb_final_landscape.rds")


### reorganize databases and join #####

df_giz_transform <- df_giz %>% 
  dplyr::mutate(value_original_currency = as.numeric(value_original_currency)
    ,source_original = "-",
                channel_original = "-",
                municipality= "-",
                sector_original = "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                source_finance_landscape = "International governments",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "EUR",
                instrument_original = "-",
                instrument_landscape = "Grants", subsector_original = "-", beneficiary_original ="-", localization_original = "-")

df_kfw_transform <- df_kfw %>% 
  dplyr::mutate(value_original_currency = as.numeric(value_original_currency),source_original = "-",
                year = 2023,
                channel_original = "-",
                municipality= "-",
                sector_original = "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "EUR",
                instrument_original = "-", subsector_original = "-", beneficiary_original ="-", localization_original = "-")

df_norad_transform <- df_norad %>% 
  dplyr::mutate(source_original = "-",
                channel_original = "-",
                instrument_landscape = "Grants",
                channel_lansdcape = "-",
                source_finance_landscape = "International governments",
                origin_private_public = "Public",
                origin_domestic_international = "International",
                municipality= "-",
                sector_original = "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "USD",
                instrument_original = "-", subsector_original = "-", beneficiary_original ="-", localization_original = "-") %>% 
  dplyr::mutate(value_original_currency = value_original_currency * 1000)


df_wb_transform <- df_wb %>% 
  dplyr::mutate(source_original = "-",
                channel_original = "-",
                municipality= "-",
                sector_original = "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "USD",
                instrument_original = "-", subsector_original = "-", beneficiary_original ="-", localization_original = "-") 
df_gef_transform <- df_gef %>% 
  dplyr::mutate(source_original = "-",
                channel_original = "-",
                municipality= "-",
                sector_original = "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "USD",
                instrument_original = "-", subsector_original = "-", beneficiary_original ="-", localization_original = "-")

## deflate and exchange ##

############ apply deflate and exchange ###########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))
cambio_sgs = read.csv("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio.csv") %>% select(-X)

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo escolhido acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)

tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>% 
    dplyr::mutate(value_brl_deflated = ifelse(original_currency == "brl", value_original_currency * deflator,
                                              ifelse(original_currency == "USD", 
                                                     value_original_currency * cambio * deflator, 
                                                     value_original_currency * 5.363696414 * deflator))) %>% 
    dplyr::mutate(value_usd = ifelse(original_currency == "EUR", value_brl_deflated/5.363696414/1.0813,
                                     value_brl_deflated/cambio))
  
  
  return(base_select_deflator)
}

all_objects <- ls()

# Filtrar os nomes dos dataframes que terminam com "transform"
transform_objects <- grep("transform$", all_objects, value = TRUE)

# Aplicar a função a cada dataframe e salvar o resultado
for (obj_name in transform_objects) {
  # Usar get para obter o dataframe a partir do nome
  df <- get(obj_name)
  
  # Aplicar a função deflate_and_exchange
  result_df <- deflate_and_exchange(tabela_deflator, df, tabela_cambio)
  
  # Criar um novo nome para o dataframe resultante
  new_name <- sub("transform$", "calculus", obj_name)
  
  # Atribuir o dataframe resultante ao novo nome no ambiente
  assign(new_name, result_df)
}


##### join ###

select_columns <- function(df) {
  df %>%
    select(id_original, data_source, year, project_name, project_description, source_original,
           source_finance_landscape, origin_domestic_international, origin_private_public,
           value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
           channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
           subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
           beneficiary_public_private, localization_original, region, uf, municipality)
}

# Obter todos os objetos no ambiente
all_objects <- ls()

# Filtrar os nomes dos dataframes que terminam com "calculus"
calculus_objects <- grep("calculus$", all_objects, value = TRUE)

# Aplicar a função select_columns a cada dataframe e combinar usando rbind
combined_df <- do.call(rbind, lapply(calculus_objects, function(obj_name) {
  df <- get(obj_name)
  select_columns(df)
}))

# Exibir as primeiras linhas do dataframe resultante
head(combined_df)


setwd(dir_internationals)

combined_df <- combined_df %>% dplyr::mutate(source_original = data_source,
                                             data_source= "Internationals")

saveRDS(combined_df,"all_internationals_landscape_deflated_exchange_04102024.rds")

write.xlsx(combined_df,"all_internationals_landscape_deflated_exchange_04102024.xlsx")
