##################
# Author :  Renan Morais Florias
# Date: 07/12/2023
# Email: renanflorias@hotmail.com
# Goal: transform landscape ocde database
# resource: OECD Miltilateral Climate Funds


########################## Libraries ######################################
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


dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_oecd_project <- ("A:/projects/landuse_br2024/oecd_cf")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")


###### import datasets  & Select Variables #########

setwd(dir_oecd_output)

df_ocde_filter <- readRDS("df_oecd_final_filter.rds")

# df_ocde_filter <- readRDS("ocde_filter_reviewed.rds")

setwd(dir_oecd_project)

source_ocde <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "source_landscape") %>% 
  unique() %>% 
  select(-provider_type, -provider_detailed, -extending_agency)

channel_ocde <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "channel_landscape") %>% 
  dplyr::mutate(channel_original = tolower(channel_original)) %>% 
  select(-5)

instrument_ocde <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "instrument_landscape") %>% 
  mutate_if(is.character, tolower) %>% 
  select(instrument_original, instrument_landscape)

instrument_ocde$instrument_landscape <- str_to_sentence(instrument_ocde$instrument_landscape)

sector_ocde <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "sector_landscape")

# climate_ocde <- read.xlsx("10_oecd_relational_tables.xlsx", sheet = "project_selection")

# data_landscape_original <- read.xlsx("10_oecd_relational_tables.xlsx", sheet = "data_landscape_oecd") %>%
# #   mutate_if(is.character, tolower) %>%
#   dplyr::mutate(project_name =  str_trim(project_name))%>% 
#   dplyr::rename( source_original = source_of_finance_original)%>% 
#   select(year, project_name,project_description, source_original,value_original, sector_landscape) %>% 
#   mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>% 
#   dplyr::mutate(project_description = str_replace_all(project_description,';', ','))%>% 
#   dplyr::mutate(id_join = paste0(year, project_name, project_description, source_original, value_original)) %>%
#   unique() 
# 
# data_subsector_crs <- read.xlsx("10_oecd_relational_tables.xlsx", sheet = "purpose codes_oecd", startRow = 3) %>% 
#   mutate_if(is.character, tolower) %>% 
#   dplyr::rename(clarifications ='Clarifications./.Additional.notes.on.coverage',
#                 sub_sector = DESCRIPTION )%>% slice(-1) %>% 
#   dplyr::mutate(clarifications = if_else(is.na(clarifications), "Not available", clarifications),
#                 CRS = as.numeric(CRS)) %>% 
#   dplyr::filter(!is.na(CRS) & is.na(voluntary.code))%>% 
#   dplyr::rename(purpose_code = CRS) %>% 
#   select(-DAC.5, -voluntary.code)

# data_subsector_voluntary_code <- read.xlsx("10_oecd_relational_tables.xlsx", sheet = "purpose codes_oecd", startRow = 3) %>% 
#   mutate_if(is.character, tolower) %>% 
#   dplyr::rename(clarifications ='Clarifications./.Additional.notes.on.coverage',
#                 sub_sector = DESCRIPTION )%>% slice(-1) %>% 
#   dplyr::mutate(clarifications = if_else(is.na(clarifications), "Not available", clarifications),
#                 voluntary.code = as.numeric(voluntary.code)) %>% 
#   dplyr::filter(!is.na(voluntary.code) & is.na(CRS)) %>% 
#   dplyr::rename(purpose_code = voluntary.code) %>% 
#   select(-DAC.5, -CRS)

# data_subsector <- rbind(data_subsector_crs, data_subsector_voluntary_code)
# 
# rm(data_subsector_crs, data_subsector_voluntary_code)
  
  


###### select and transform variables ############

df_ocde_filter <- df_ocde_filter %>% dplyr::rename(channel_original = channel_of_delivery) %>% 
  dplyr::mutate(source_original = paste(provider_type, provider_detailed, extending_agency,sep ="_"),
         instrument_original = paste(concessionality, financial_instrument, sep = "_")) %>% 
  dplyr::mutate(description = if_else(is.na(description), "not available", description)) %>% 
  dplyr::mutate(id_join = paste0(year, project_title, description, provider_detailed, climate_related_development_finance_commitment_current_usd_thousand)) %>% 
  dplyr::filter(!sector_landscape %in% c("Education","?"))



# df_ocde_transform <- left_join(df_ocde_filter, data_landscape_original %>% select(id_join,sector_landscape), by = "id_join")

"os projetos classificados como NA no processo de join das bases serão alterados manualmente
devido a mudança de características na própria base da ocde. Entretanto, a verificação manual validou a igualdade das observações em cada base.
Todos são categorizados como forest"

# df_ocde_transform <- df_ocde_transform %>% 
#   dplyr::mutate(sector_landscape = if_else(is.na(sector_landscape), "Forest", sector_landscape))
# sum(df_ocde_transform$climate_related_development_finance_commitment_current_usd_thousand)

df_ocde_transform <-  left_join(df_ocde_filter, channel_ocde, by ="channel_original")
sum(df_ocde_transform$climate_related_development_finance_commitment_current_usd_thousand)

df_ocde_transform <- left_join(df_ocde_transform,source_ocde, by = "source_original")

sum(df_ocde_transform$climate_related_development_finance_commitment_current_usd_thousand)

df_ocde_transform <- left_join(df_ocde_transform,instrument_ocde, by = "instrument_original")

sum(df_ocde_transform$climate_related_development_finance_commitment_current_usd_thousand)

df_ocde_transform <- df_ocde_transform %>% dplyr::mutate(channel_landscape = ifelse(is.na(channel_landscape),"-",channel_landscape))

df_ocde_transform <- left_join(df_ocde_transform, sector_ocde %>% select(purpose_code, 'description.Sub-sector.OCDE'), by = "purpose_code") %>% 
  dplyr::rename(clarifications = 'description.Sub-sector.OCDE')


rm(channel_ocde,climate_ocde,data_landscape_original, instrument_ocde,sector_ocde,source_ocde, data_subsector)

### classify climate use ################


df_ocde_transform <- df_ocde_transform %>% 
  dplyr::mutate(climate_component = if_else(adaptation_related_development_finance_commitment_current_usd_thousand > 0 & 
                                 mitigation_related_development_finance_commitment_current_usd_thousand == 0,
                               "Adaptation", 
                               if_else(adaptation_related_development_finance_commitment_current_usd_thousand == 0 & 
                                         mitigation_related_development_finance_commitment_current_usd_thousand > 0,
                                       "Mitigation", 
                                       if_else(adaptation_related_development_finance_commitment_current_usd_thousand > 0 & 
                                                 mitigation_related_development_finance_commitment_current_usd_thousand > 0,
                                       "Dual", "-")))) %>% 
  dplyr::rename(value_original_currency = climate_related_development_finance_commitment_current_usd_thousand,
                id_original = crs_identification_n,
                project_name = project_title,
                project_description = description,
                origin_domestic_international = source_national_international,
                origin_private_public = source_private_public,
                sector_original = sub_sector,
                subsector_original = clarifications) %>% 
  dplyr::mutate(data_source = "oecd_cf",
         original_currency = "usd",
         activity_landscape = "-",
         subactivity_landscape = "-",
         rio_marker = "-",
         beneficiary_original = "-",
         beneficiary_landscape = "-",
         beneficiary_public_private = "-",
         localization_original = "-",
         region = "-",
         uf = "-",
         municipality = "-")

df_ocde_transform <- df_ocde_transform %>% 
  dplyr::mutate(sector_landscape = ifelse(sector_landscape %in% "agricultura", "Crop",
                                   ifelse(sector_landscape %in% "pecuaria", "Cattle",
                                          ifelse(sector_landscape %in% "conservacao, restauracao e reflorestamento", "Forest",
                                          ifelse(sector_landscape %in% "bioenergia e combustiveis", "Bioenergy and fuels",
                                                 ifelse(sector_landscape %in% "multisetorial", "Multi-sector", sector_landscape))))))
  
  
  

################## apply deflate and exchange ####################

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
  dplyr::filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>% 
    dplyr::mutate(value_brl_deflated =  value_original_currency * cambio * deflator) %>% 
    dplyr::mutate(value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_ocde_calculus <- deflate_and_exchange(tabela_deflator, df_ocde_transform, tabela_cambio)

#### validate data sector

rm(cambio_sgs,df_ocde_transform, ibge_ipca, tabela_cambio, tabela_deflator, teste)

df_ocde_calculus <- df_ocde_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

setwd(dir_oecd_output)

oecd_publicado_deflated_2023 <- readRDS("df_ocde_landscape_final_2020.rds")

df_ocde_calculus_join <- rbind(oecd_publicado_deflated_2023, df_ocde_calculus)

write.xlsx(df_ocde_calculus,"df_ocde_landscape_final_att.xlsx")

saveRDS(df_ocde_calculus,"df_ocde_landscape_final_att.rds")

write.xlsx(df_ocde_calculus_join,"df_ocde_landscape_final_join_year.xlsx")

# saveRDS(df_ocde_calculus,"df_ocde_landscape_final_reviewed.rds")


#################### calculus oecd 2015 to 2020 ###############
setwd("A:\\projects\\brlanduse_landscape102023\\oecd\\output")

oecd_publicado_deflated_2023 <- readRDS("df_ocde_landscape_final_reviewed.rds") %>% 
  select(-value_brl_deflated,-value_usd)


oecd_publicado_deflated_2023 <- deflate_and_exchange(tabela_deflator, oecd_publicado_deflated_2023, tabela_cambio)

setwd(dir_oecd_output)

oecd_publicado_deflated_2023 <- oecd_publicado_deflated_2023 %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

saveRDS(oecd_publicado_deflated_2023,"oecd_15_20_deflated_23.rds")



############## save data########
setwd(dir_oecd_output)

df_ocde_calculus_join <- rbind(oecd_publicado_deflated_2023, df_ocde_calculus)

write.xlsx(df_ocde_calculus,"df_ocde_landscape_final_att.xlsx")

saveRDS(df_ocde_calculus,"df_ocde_landscape_final_att.rds")

write.xlsx(df_ocde_calculus_join,"df_ocde_landscape_final_join_year.xlsx")

saveRDS(df_ocde_calculus_join, "df_ocde_landscape_final_join_year.rds")

