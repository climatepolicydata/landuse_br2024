##################

# Author : Renan Morais
# Date: 11-04-2024
# Email: renanflorias@hotmail.com
# Goal: filter bndes indiretas automaticas
# resource: 

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
               pivottabler,
               cld3)

##### directory #########

dir_bndes_aut_raw <- ("A:/finance/bndes_Aut/rawData")

dir_bndes_aut_clear <- ("A:/finance/bndes_Aut/CleanData")


dir_project_bndes_output <- ("A:/projects/landuse_br2024/BNDES_Aut/output")


dir_bndes_aut_doc <- ("A:/projects/landuse_br2024/BNDES_Aut")

##### import datasets #########

setwd(dir_project_bndes_output)

df_bndes_aut_filter <- readRDS("df_bndes_aut_filter_pre_transf.rds")


############ import data ###########

setwd(dir_project_bndes_output)

df_bndes_filter <-  readRDS("df_bndes_aut_filter_pre_transf.rds")

setwd(dir_bndes_aut_doc)

source_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "channel_landscape")

instrument_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()

sector_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "sector_landscape")

#climate_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "climate_select") %>% 
#dplyr::rename(climate_original = id_select)

###### create variables to transform #####

df_bndes_filter <- df_bndes_filter %>% mutate(beneficiary_original = paste(natureza_do_cliente,porte_do_cliente,cliente, sep = "_"),
                                              channel_original = paste(forma_de_apoio, instituicao_financeira_credenciada, sep = "_"))



###### select and transform variables ############
df_bndes_transform <- left_join(df_bndes_filter,source_bndes_aut, by = "fonte_de_recurso_desembolsos")
rm(df_bndes_filter)
df_bndes_transform <-  left_join(df_bndes_transform, channel_bndes_aut, by ="channel_original")
# df_bndes_transform <- df_bndes_transform[,-c(29,30)]
df_bndes_transform <- left_join(df_bndes_transform,instrument_bndes_aut, by = "instrumento_financeiro")
df_bndes_transform <- left_join(df_bndes_transform,sector_bndes_aut, by = "subsetor_cnae_nome")
df_bndes_transform <- df_bndes_transform %>% dplyr::mutate(beneficiary_landscape = "Corporations",
                                                    beneficiary_public_private = "Public")


###### select and change names of landscape ######

df_bndes_transform_landscape <- df_bndes_transform %>% 
  dplyr::rename(year = ano,
                project_name = cliente,
                value_original_currency = valor_da_operacao_em_reais,
                municipality = municipio,
                source_original = source_of.finance_original,
                origin_domestic_international = national_internacional,
                origin_private_public = source_private_public) %>% 
  dplyr::mutate(data_source = "bndes_aut",
         original_currency = "BRL",
         localization_original = uf,
         project_name = "-",
         subactivity_landscape = "-",
         region = "-",
         project_description = "-",
         id_original = "-",
         activity_landscape = "-",
         climate_use = "-",
         subactivity_landscape = "-",
         rio_marker = "-")


############ apply deflate and exchange ###########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/Funcao_taxa_cambio_v2.r"))

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo escolhido acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>%  
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_bndes_aut_calculus <- deflate_and_exchange(tabela_deflator, df_bndes_transform_landscape, tabela_cambio)

### to select variables ####

rm(cambio_sgs,df_bndes_transform_landscape, ibge_ipca, tabela_cambio, tabela_deflator, teste)

df_bndes_aut_calculus <- df_bndes_aut_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_of_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_use, rio_marker, 
         beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)


setwd(dir_project_bndes_output)



saveRDS(df_bndes_aut_calculus,"df_bndes_aut_landscape_final.rds")

write.xlsx(df_bndes_aut_calculus, "df_bndes_aut_landscape_final.xlsx")
