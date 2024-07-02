library(tidyverse)
library(stringi)
library(readxl)
# library(xlsx)
library(pdftools)
library(lexRankr)
pacman::p_load(tidyverse,
               readxl,
               readr,
               openxlsx,
               data.table,
               writexl,
               ggplot2,
               janitor,
               GetBCBData,
               rbcb,
               scales,
               stringi)
# source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

github <- "Documents"
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
source(paste0(root,github,"/GitHub/landuse_br2024/AuxFolder/Dictionary_Sectors.R"))
#Importando tabela relacional
source_finance_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "source_landscape") %>% select(-emissor_trade_name)
source_finance_landscape <- source_finance_landscape %>% mutate(source_original = str_to_lower(stri_trans_general(source_original,"Latin-ASCII")))
source_finance_landscape <- source_finance_landscape[,-5]
source_finance_landscape <- source_finance_landscape %>% unique()


channel_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "channel_landscape") %>% select(channel_original, channel_landscape) %>% 
  dplyr::mutate(channel_original = str_to_lower(stri_trans_general(channel_original,"Latin-ASCII"))) %>% unique()

instrument_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "instrument_landscape") %>% select(-tipo,-categoria,-instrumento_financeiro)
instrument_landscape <- instrument_landscape %>% mutate(instrument_original = str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII")))
#####################################################################################################################
nint_clear <- read_csv2("A:\\projects\\landuse_br2024\\NINT\\Clean_data\\nint_clear_19_03_2024.csv") %>% filter((data >=2021) & (data<= 2023))
nint_clear[nint_clear == "N/D"] <- NA
nint_clear%>%view

######################## filter manual ################### 

setwd("A:\\projects\\landuse_br2024\\nint\\Check\\")

data_filter <- read_xlsx("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx", sheet = "climate_select") %>% select(-emissor_trade_name) %>% 
  dplyr::rename(climate_component = use_cpi)


df_nint_filter <- nint_clear %>% dplyr::filter(number %in% data_filter$id_original) %>% 
  dplyr::mutate(value_original_currency = valor * 1000000,
                instrument_original = paste(instrumento_financeiro, categoria, sep = "_"),
                channel_original = paste(tipo_de_emissor, emissor_trade_name, sep = "_"),
                localization_original = "-", municipality = "-", region = "-", uf= "-",
                data_source = "nint",
                project_name = paste(mercado, tipo, verificador_externo,cbi, sep = "_"),
                subsector_original = "-",
                rio_marker = "-", beneficiary_original = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-") %>% dplyr::rename(id_original = number,year=data,
                                                                               original_currency = moeda, source_original = emissor_trade_name)
  

df_nint_filter <- left_join(df_nint_filter, data_filter, by = "id_original" )

df_nint_filter <- left_join(df_nint_filter, instrument_landscape, by = "instrument_original")

df_nint_filter <- left_join(df_nint_filter, source_finance_landscape, by = "source_original")

df_nint_filter <- left_join(df_nint_filter, channel_landscape, by = "channel_original")



#### apply deflate and exchange ###

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
                                              ifelse(original_currency == "usd", 
                                                     value_original_currency * cambio * deflator, 
                                                     value_original_currency * 3.69995 * deflator))) %>% 
    dplyr::mutate(value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_nint_calculus <- deflate_and_exchange(tabela_deflator, df_nint_filter, tabela_cambio)


df_nint_calculus <- df_nint_calculus %>% select(id_original, data_source, year, project_name, project_description, source_original,
       source_finance_landscape, origin_domestic_international, origin_private_public,
       value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
       channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
       subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
       beneficiary_public_private, localization_original, region, uf, municipality)

passado <- read_rds("A:\\projects\\brlanduse_landscape102023\\nint\\output\\df_nint_landscape_final_replication.rds")
ano_ini = 2015
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)



df_nint_calculus <- rbind(df_nint_calculus, passado)

setwd("A:\\projects\\landuse_br2024\\NINT\\")

write_rds(df_nint_calculus,"df_nint_landscape_02072024.rds")

write_xlsx(df_nint_calculus, "df_nint_landscape_02072024.xlsx")

