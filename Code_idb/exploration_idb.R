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

dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_idb_clear <- ("A:/finance/idb/cleanData")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb\\output")

##### directory #########

dir_idb_raw <- ("A:/finance/idb/rawData")

dir_idb_clear <- ("A:\\projects\\brlanduse_landscape102023\\idb")


###### import datasets #########

setwd(dir_idb_output)

df_idb_full <- read.xlsx("idb_brasil_21_23.xlsx") %>% 
  dplyr::rename(year = ano,
                value_original_currency = total_cost) %>% mutate(original_currency = "usd")

########## transform base ##########

codes_crop <- c("5440/oc-br",	"5611/oc-br",	"5612/oc-br",	"atn/oc-18644-br",	"atn/oc-18781-br",	"equ/ms-20143-br",	"equ/tc-20142-br",	"sp/oc-23-51-br")

codes_forest <- c("5836/oc-br",	"atn/az-19413-br",	"atn/az-20334-br",	"atn/gn-20510-br",	"atn/jf-20520-br",	"atn/oc-19412-br",	"atn/sx-19186-br")

codes_multi <- c("atn/az-20411-br",	"atn/lc-18953-br",	"atn/mc-20445-br",	"atn/oc-19258-br",	"atn/oc-19745-br",	"atn/oc-20410-br",	"atn/oc-20570-br")


data_filter_sectors <- df_idb_full %>% dplyr::mutate(sector_landscape = ifelse(operation_number %in% codes_crop, "Crop",
                                                                                       ifelse(operation_number %in% codes_forest, "Forest",
                                                                                              ifelse(operation_number %in% codes_multi, "Multi-sector", "Null")))) %>% 
  dplyr::filter(sector_landscape %in% c("Crop","Forest","Multi-sector"))

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

df_idb_calculus <- deflate_and_exchange(tabela_deflator, data_filter_sectors, tabela_cambio) 



# write.xlsx(df_idb_calculus, "df_idb_landscape_out_ocde.xlsx")


df_wb <- read.xlsx("A:\\projects\\landuse_br2024\\internacionais\\worldbank\\wb_21_23_all_projects.xlsx", sheet = "data") %>% 
  janitor::clean_names(.) %>% dplyr::filter(year >= 2021 & year <2024)

df_wb <- df_wb %>%  mutate(total_ida_and_ibrd_commitment_us = ifelse(is.na(ibrd_commitment_us),0,ibrd_commitment_us))

df_wb <- df_wb %>% dplyr::rename(value_original_currency = ibrd_commitment_us) %>% 
  dplyr::mutate(original_currency = "usd",
         value_original_currency = as.numeric(value_original_currency))




df_wb_calculius <-  deflate_and_exchange(tabela_deflator, df_wb, tabela_cambio) 

df_wb_calculus_landscape <- deflate_and_exchange(tabela_deflator, df_wb_transform, tabela_cambio) 

setwd("A:\\projects\\landuse_br2024\\internacionais\\worldbank")

write.xlsx(df_wb_calculius,"wb_21_23_deflated.xlsx")



dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_oecd_project <- ("A:/projects/landuse_br2024/oecd_cf")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")


###### import datasets  & Select Variables #########

setwd(dir_oecd_output)

df_ocde_filter <- readRDS("df_oecd_final_filter_10062024.rds")



################## include full wb looking projects in ocde #############

##### directory #########

dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_oecd_project <- ("A:/projects/landuse_br2024/oecd_cf")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")


###### import datasets  & Select Variables #########

setwd(dir_oecd_Cf_clear)

df_oecd_clear <- readRDS("OECD_DAC_clear_2022.RDS") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2021, recipient == "brazil")

setwd(dir_oecd_project)

sector_reference_table <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "sector_landscape")

sector_reference_table <- sector_reference_table[-c(1,2),]




######### filter ####################

padroes <- c("support to amazon fund", "amazona fund", "amazon fund", "amazona funds", "amazon funds", "fundo amazonia")

#filter amazon fund projects
fmz_project <- df_oecd_clear %>% 
  dplyr::filter((
    str_detect(description, padroes[1]) |
      str_detect(description, padroes[2]) |
      str_detect(description, padroes[3]) |
      str_detect(description, padroes[4]) |
      str_detect(description, padroes[5])))



df_fmz_project_reviewed <- df_oecd_clear %>% dplyr::filter(!crs_identification_n %in% fmz_project$crs_identification_n & provider %in% "wb")

df_fmz_project_no_wb <- df_fmz_project_reviewed %>% dplyr::filter(!crs_identification_n %in% c("p177070","p177632","p178339","p178563","p178729","p178888","p168634"))







