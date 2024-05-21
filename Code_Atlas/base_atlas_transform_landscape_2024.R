##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transformação base atlas rural para landscape
# resource: 


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
               pivottabler)

################## directories ###################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_sisser_mapa_dt_clean <- ("A:/finance/atlas_Seguro_Rural/cleanData")
dir_sisser_mapa_output <- ("A:/finance/sisser/cleanData")


dir_mapa_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/MAPA/0.Database/2.Raw")

setwd(dir_sisser_mapa_dt_clean)


############### import database #####################

df_atlas <- readRDS("atlas_2006_2022_clear.rds")

df_atlas_2015_2022 <- df_atlas %>% 
  dplyr::filter(ano_apolice >= 2015 & ano_apolice <= 2022,
         !nm_cultura_global %in% c("Pecuário"))


df_atlas_2015_2022 <- df_atlas_2015_2022  %>% 
  select(nm_razao_social, nm_municipio_propriedade, sg_uf_propriedade, 
         nm_classif_produto, nm_cultura_global, vl_subvencao_federal,
         ano_apolice, evento_preponderante) 

df_atlas <- df_atlas_2015_2022 %>% 
  group_by(nm_razao_social, nm_municipio_propriedade, sg_uf_propriedade, 
           nm_classif_produto, nm_cultura_global,
           ano_apolice, evento_preponderante) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

"remove values with count for 'livestock' "

df_atlas <- df_atlas %>% 
  dplyr::filter(!nm_cultura_global %in% c("Pecuário"))

df_atlas_sub_negative <- aggregate( vl_subvencao_federal ~ id_equals + nm_razao_social + nm_municipio_propriedade + 
                                      sg_uf_propriedade + nm_classif_produto+ nm_cultura_global+ vl_subvencao_federal+
                                    ano_apolice + evento_preponderante, data = df_atlas,
                                FUN = sum)

df_atlas_subvencao <- aggregate(vl_subvencao_federal ~ id_equals + nm_razao_social+ nm_municipio_propriedade + 
                                   sg_uf_propriedade + nm_classif_produto+ nm_cultura_global+ vl_subvencao_federal+
                                   ano_apolice + evento_preponderante, data = df_atlas,
                                 FUN = sum)

rm(df_atlas)


############### transform ########

setwd("A:/projects/landuse_br2024/atlas")

relational_table <- read.xlsx("03_atlas_relational_tables.xlsx", sheet = "sector_landscape") %>% 
  select(nm_cultura_global, sector_landscape) %>% unique()


df_atlas_sub_negative <- join(df_atlas_sub_negative,relational_table %>% select(nm_cultura_global, sector_landscape), by = "nm_cultura_global")

df_atlas_subvencao <- join(df_atlas_subvencao,relational_table %>% select(nm_cultura_global, sector_landscape), by = "nm_cultura_global")


"base para valor subvenção negativo"

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  dplyr::rename(year = ano_apolice, channel_original = nm_razao_social,
                subsector_original = evento_preponderante, region = sg_uf_propriedade, uf = nm_municipio_propriedade) %>% 
  dplyr::mutate(data_source = "atlas_seguro_mapa",project_name = "Abate no SES para Subvenção PSR",
         project_description = "nm_classif_produto", source_original = "Produtores Rurais",
         source_finance_landscape = "Rural Producers", origin_domestic_international = "National",
         origin_private_public = "Private", channel_landscape= "Financial institutions",
         instrument_original = "-", instrument_landscape= "Risk management",
         sector_original = paste0(nm_classif_produto,nm_cultura_global),
         activity_landscape = "Financial services", subactivity_landscape = '-',
         climate_component = "Adaptation", rio_marker = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural producers", 
         beneficiary_public_private = "-", localization_original = "-",
         municipality = "-")%>%
  dplyr::mutate(id_original = paste(id_equals,"VLN", sep = ""),
         subactivity_landscape = "Tipo de Produto") %>% 
  dplyr::rename(value_brl = vl_subvencao_federal) %>% 
  dplyr::mutate(value_brl = -1*value_brl)

"base para valor da subvenção"

df_atlas_subvencao <- df_atlas_subvencao %>% 
  dplyr::rename(year = ano_apolice, channel_original = nm_razao_social,
                subsector_original = evento_preponderante, region = sg_uf_propriedade, uf = nm_municipio_propriedade) %>% 
  dplyr::mutate(data_source = "atlas_seguro_mapa",project_name = "Subvenção PSR",
         project_description = "nm_classif_produto", source_original = "MAPA",
         source_finance_landscape = "Federal and state governments", origin_domestic_international = "National",
         origin_private_public = "Public", channel_landscape= "Financial institutions",
         instrument_original = "Subvenção PSR", instrument_landscape= "Risk management",
         sector_original = paste0(nm_classif_produto,nm_cultura_global),
         activity_landscape = "Financial services",
         climate_component = "Adaptation", rio_marker = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural producers", 
         beneficiary_public_private = "-", localization_original = "-",
         municipality = "-")%>% 
  dplyr::mutate(id_original = paste(id_equals,"SUB", sep = ""),
         subactivity_landscape = "Rural insurance for farming and forestry") %>% 
  dplyr::rename(value_brl = vl_subvencao_federal)

############ reogarnizando a base#########

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  select(id_original,value_brl ,data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)

df_atlas_subvencao <- df_atlas_subvencao %>% 
  select(id_original, value_brl, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)


"JUNÇÃO DAS BASES"

df_atlas_premio_liq_sub <- rbind(df_atlas_sub_negative,df_atlas_subvencao)



df_atlas_final <- df_atlas_premio_liq_sub %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  dplyr::mutate(original_currency = "BRL",
         year = as.numeric(year)) %>%
  relocate(original_currency, .after = value_original_currency)

rm(df_atlas_premio_liq_sub, df_atlas_sub_negative, df_atlas_subvencao)



############ apply deflated and exchange #######

# Inserir o caminho onde foi feito o clone do projeto 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

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
    left_join(tabela_deflator, by= "year") %>%
    left_join(tabela_cambio, by= "year")  %>%  
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_atlas_calculus <- deflate_and_exchange(tabela_deflator, df_atlas_final, tabela_cambio)

rm(cambio_sgs,df_atlas_final, ibge_ipca, tabela_cambio, tabela_deflator, teste)


df_atlas_calculus <- df_atlas_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)

setwd("A:/projects/landuse_br2024/atlas/output")

saveRDS(df_atlas_calculus,"database_atlas_landscape_2024.rds")


