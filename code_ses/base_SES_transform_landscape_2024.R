##################

# Author : Renan Morais
# Date: 30.05.2023
# Email: renanflorias@hotmail.com
# Goal: transformação da base SES para landscape



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



########################### Directories ########################################
root <- ('A:\\finance\\ses\\')
dir_susep_dt_clean <- paste0(root,'cleanData\\')
dir_susep_output <- ("A:/projects/landuse_br2024/ses/output")
dir_susep_doc <- ("A:/projects/landuse_br2024/ses")
########### import databases #########
setwd(dir_susep_dt_clean)

ses_seguros2 <- read.csv("ses_clear.csv", fileEncoding = "latin1", sep = ";")

ses_seguros2$premio_direto <- gsub(",",".",ses_seguros2$premio_direto) %>% as.numeric()

ses_seguros2$noenti <- str_trim(ses_seguros2$noenti)

setwd(dir_susep_doc)

codes2 <- read.xlsx("codes_ramo_rural_landscape.xlsx") %>% janitor::clean_names()

##################### filters and transforms ###########3

ses_seguros2 <- ses_seguros2 %>%
  #dplyr::mutate(ano = as.numeric(substr(damesano, 0, 4))) %>% 
  dplyr::filter(ano >= 2015 & ano <= 2023) %>% 
  dplyr::filter(premio_direto != 0)


ses_seguros_filter_landscape <- ses_seguros2 %>% 
  dplyr::filter(coramo %in% codes2$codigos)

sum(ses_seguros_filter_landscape$premio_direto) 

##################### transforms ########


#inserindo a descrição de códigos no database

codes2 <- codes2 %>% 
  dplyr::rename(coramo = codigos)

"criação id"

ses_seguros_filter_landscape <- ses_seguros_filter_landscape %>%
  group_by(coenti, coramo, ano) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

"agregando por coramo"

df_ses_agregado <- aggregate(premio_direto ~ id_equals + coenti + coramo + ano + noenti, data = ses_seguros_filter_landscape, FUN = sum)

df_ses_agregado <- merge(df_ses_agregado, codes2, by = "coramo")

df_ses_agregado_clear <- df_ses_agregado

# write.csv2(df_ses_agregado_clear, "ses_agregado_clear.csv", fileEncoding = "latin1") 

#eliminando observações com a descrição dita "seguro benf e produtos agropecuarios"
df_ses_agregado <- df_ses_agregado %>% 
  dplyr::filter(!descricao %in% "Seguro Benf. e Prod. Agropecuários")


#inserindo algumas classificações das categorias landscape
df_ses_agregado_transform <- df_ses_agregado %>% 
  dplyr::rename(id_original = id_equals, channel_original = noenti,sector_original = descricao, year = ano,
                value_brl = premio_direto) %>% 
  dplyr::mutate(instrument_original = "Seguro do Ramo Rural", subsector_original = "-", activity_landscape = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural Producers", beneficiary_public_private = "-", region = "-",
         uf= "-", municipality = "-",
         subactivity_landscape= "Rural insurance for farming and forestry", ecossystem_layer = "Gestão de Riscos Agropecuários",
         climate_component = "Adaptation", final_beneficiaries_final = "-", climate_component = "-", rio_marker = "-",
         CPI_riomarker_mitigation = "-",rio_marker = "-", Subfunção = "-", data_source = "ses_seguros", project_name = "Seguro Rural", project_description = "Prêmio Pago para Contratação de Seguro do Ramo Rural",
         source_original = "-", source_finance_landscape = "Rural Producers", origin_domestic_international = "National", origin_private_public = "Private", channel_landscape = "Financial Institution",
         instrument_landscape = "Risk management",localization_original = "-",
         sector_landscape = if_else(sector_original %in% c("Seguro Agrícola com cob. do FESR", "Seguro Agrícola sem cob. do FESR","Agrícola",
                                                           "Seguro Benf. e Prod. Agropecuários"), "Crop",
                                    "Forest"))

rm(df_ses_agregado, df_ses_agregado_clear, ses_seguros_filter_landscape, ses_seguros2, codes2)

"reorganizando a base"

df_ses_agregado <- df_ses_agregado_transform %>%
  select(id_original, data_source, year, project_name,
       project_description, source_original, source_finance_landscape,
       origin_domestic_international, origin_private_public,
       value_brl,channel_original, channel_landscape, instrument_original,
       instrument_landscape, sector_original, sector_landscape,
       subsector_original,activity_landscape, beneficiary_original,
       beneficiary_landscape,beneficiary_public_private,
       region, uf,municipality,
       ecossystem_layer,climate_component, final_beneficiaries_final,Subfunção,subactivity_landscape, rio_marker, localization_original)

rm(df_ses_agregado_transform)

df_ses_agregado <- df_ses_agregado %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  dplyr::mutate(original_currency = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)


############ apply deflatd and exchange #######

# Directory - Clone

# Inserir o caminho onde foi feito o clone do projeto 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

"The object github could be receive the file that corresponds to the project repository"

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/Funcao_taxa_cambio_v2.r"))

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>%  
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_ses_calculus <- deflate_and_exchange(tabela_deflator, df_ses_agregado, tabela_cambio)


##### save dataset #####

rm(cambio_sgs,df_ses_agregado, ibge_ipca, tabela_cambio, tabela_deflator, teste, serie, anos, ano_fim, ano_ini)



df_ses_calculus <- df_ses_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)



setwd(dir_susep_output)

write.csv(df_ses_calculus, "ses_agregado_landscape_completo_2024.csv")

saveRDS(df_ses_calculus, "ses_agregado_landscape_completo_2024.rds")

write.csv2(df_ses_calculus, "base_landscape_final_18032024.csv",fileEncoding = "ISO-8859-1")



