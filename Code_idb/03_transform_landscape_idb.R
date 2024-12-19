##################

# Author : Renan Morais
# Date: 09-05-2024
# Email: renanflorias@hotmail.com
# Goal: transform idb database in format landscape


########## libraries ############
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
               pivottabler,
               cld3)

##### directory #########

dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_idb_clear <- ("A:/finance/idb/cleanData")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb\\output")

dir_idb_doc <- ("A:\\projects\\landuse_br2024\\idb")

###### import datasets #########

setwd(dir_idb_output)

df_idb_filter <- readRDS("df_idb_filter_30092024.rds") %>% dplyr::mutate(project_type = str_trim(project_type),
                                                                project_type = as.character(project_type))

# ###### out oecd ########
# df_idb_filter <- read.xlsx("idb_brasil_21_23.xlsx")

########## transform base ##########

codes_crop <- c("5440/oc-br",	"5611/oc-br",	"5612/oc-br",	"atn/oc-18644-br",	"atn/oc-18781-br",	"equ/ms-20143-br",	"equ/tc-20142-br",	"sp/oc-23-51-br")

codes_forest <- c("atn/az-19413-br",	"atn/az-20334-br",	"atn/gn-20510-br",	"atn/jf-20520-br",	"atn/oc-19412-br",	"atn/sx-19186-br")

codes_multi <- c("atn/az-20411-br",	"5836/oc-br","atn/lc-18953-br",	"atn/mc-20445-br",	"atn/oc-19258-br",	"atn/oc-19745-br",	"atn/oc-20410-br",	"atn/oc-20570-br")


df_idb_filter <- df_idb_filter %>% dplyr::mutate(sector_landscape = ifelse(operation_number %in% codes_crop, "Crop",
                                                                               ifelse(operation_number %in% codes_forest, "Forest",
                                                                                      ifelse(operation_number %in% codes_multi, "Multi-sector", "Null")))) %>% 
  dplyr::filter(sector_landscape %in% c("Crop","Forest","Multi-sector"))


"relational table"

setwd(dir_idb_doc)

df_idb_relational_source <- read.xlsx("13_idb_relational_tables.xlsx", sheet = "source_landscape") %>% 
  select(-funding_source)

df_idb_relational_instrument <- read.xlsx("13_idb_relational_tables.xlsx", sheet = "instrument_landscape") %>% select(-project_type) %>% 
  dplyr::mutate(instrument_original = str_trim(instrument_original),
                instrument_original = as.character(instrument_original))

df_idb_relational_channel <- read.xlsx("13_idb_relational_tables.xlsx", sheet = "channel_landscape") %>% select(-executing_agency)

########## transforms ######

df_idb_filter_transform <- df_idb_filter %>% 
  dplyr::rename(id_original = operation_number,
                year = ano,
                source_original = funding_source,
                channel_original = executing_agency,
                value_original_currency = total_cost,
                original_currency = currency,
                instrument_original = project_type,
                sector_original = sector,
                subsector_original = sub_sector,
                localization_original = project_country,
                beneficiary_original = borrower) %>% 
  dplyr::mutate(data_source = "idb_projects",
                municipality= "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-")


######### join with categories in relational table ######

df_idb_filter_transform <- left_join(df_idb_filter_transform,df_idb_relational_source, by = "source_original") 

df_idb_filter_transform <- left_join(df_idb_filter_transform, df_idb_relational_instrument, by = "instrument_original")

df_idb_filter_transform <- left_join(df_idb_filter_transform, df_idb_relational_channel, by = "channel_original") 


########### change climate ###########



df_idb_filter_transform <- df_idb_filter_transform %>% 
  dplyr::mutate(climate_component = ifelse(id_original %in% c("atn/az-19413-br",	"atn/lc-18953-br",	"atn/oc-19412-br"),"Mitigation", "-")) %>%  
  dplyr::mutate(climate_component = ifelse(id_original %in% c("5440/oc-br",	"5611/oc-br",	"5612/oc-br",	"atn/az-20334-br",	"atn/jf-20520-br","atn/mc-20445-br",
                                                              "atn/oc-18781-br",	"atn/oc-19258-br",	"atn/oc-19745-br",	"atn/sx-19186-br", "atn/oc-20570-br" ,
                                                              "5836/oc-br", "atn/gn-20510-br", "atn/oc-20410-br" , "atn/az-20411-br",
                                                              "equ/tc-20142-br", "equ/ms-20143-br", "sp/oc-23-51-br",	"atn/oc-18644-br"),"Mitigation and Adaptation",
                                           climate_component))

rm(df_idb_filter, df_idb_relational_channel, df_idb_relational_instrument, df_idb_relational_source)

df_idb_clear <- df_idb_filter_transform

############ apply deflate and exchange ###########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

# source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

cambio_sgs = read.csv("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio.csv") %>% select(-X)

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo escolhido acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


# cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>%  
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * cambio * deflator),
                  value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_idb_calculus <- deflate_and_exchange(tabela_deflator, df_idb_clear, tabela_cambio) 



df_idb_calculus <- df_idb_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)


############## save rds and csv ######
dir_idb_clear <- ("A:\\projects\\brlanduse_landscape102023\\idb")
setwd(dir_idb_clear)

published <- readRDS("idb_landscape_final.RDS")

published_2023 <- deflate_and_exchange(tabela_deflator, published, tabela_cambio) 

published_2023 <- published_2023 %>% select(-deflator,-cambio)

setwd(dir_idb_output)

df_idb_calculus <- rbind(df_idb_calculus, published_2023)

saveRDS(df_idb_calculus, "data_idb_final_landscape_30092024.rds")

write.xlsx(df_idb_calculus,"data_idb_final_landscape_30092024.xlsx")



