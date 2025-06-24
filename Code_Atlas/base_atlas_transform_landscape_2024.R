##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transformação base atlas rural para landscape
# resource: 


# Modified by Julia Niemeyer
# Date: 28/05/2025

########################### ACTION NEEDED ######################################
ano_ini = 2020 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

## set the path to your github clone
 github <- "Documents/"


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

setwd(dir_sisser_mapa_dt_clean)


############### import database #####################

df_atlas <- readRDS(paste0("atlas_2006_", ano_fim, "_clear.rds"))

df_atlas_filter <- df_atlas %>% 
  dplyr::filter(ano_apolice >= ano_ini & ano_apolice <= ano_fim,
         !nm_cultura_global %in% c("Pecuário"))


df_atlas_filter <- df_atlas_filter  %>% 
  select(nm_razao_social, nm_municipio_propriedade, sg_uf_propriedade, 
         nm_classif_produto, nm_cultura_global, vl_subvencao_federal,
         ano_apolice, evento_preponderante) 

df_atlas <- df_atlas_filter %>% 
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
         origin_private_public = "Private", channel_landscape= "Financial Institution",
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
         origin_private_public = "Public", channel_landscape= "Financial Institution",
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

############## ATUALIZADO EM 2025 -- automatico -- atualiza com base em ano_ini e ano_fim
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))

############# ATUALIZADO EM 2024 -- pega valores para deflacionar USD na base USD A:\\macro\\usd_FED\\rawData\\Inflation_FED.xls
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd_v2.r"))

####### rodar essa função para atualizar a tabela de taxa de cambio
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
tabela_deflatorUSD <- deflator_usd(ano_ini, ano_fim, usd_inflation)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)


df_atlas_calculus <- deflate_and_exchange(tabela_deflator, df_atlas_final, tabela_cambio)
df_atlas_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, df_atlas_calculus, tabela_cambio)


df_atlas_calculus3 <- df_atlas_calculus2 %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_BRLm, value_USDm, value_USDm_deflated, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)

setwd("A:/projects/landuse_br2024/atlas/output")

saveRDS(df_atlas_calculus3, paste0("database_atlas_landscape_", ano_ini, "_", ano_fim, "_TESTE_USD.rds"))
write.csv2(df_atlas_calculus3, paste0("database_atlas_landscape_", ano_ini, "_", ano_fim, "_TESTE_USD.csv"))


