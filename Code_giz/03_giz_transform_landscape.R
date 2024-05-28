##################

# Author : Renan Morais
# Date: 23-05-2024
# Email: renanflorias@hotmail.com
# Goal: transform giz database in format landscape


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

dir_giz_output <- ("A:\\projects\\landuse_br2024\\internacionais\\giz\\output\\")

dir_giz_doc <- ("A:\\projects\\landuse_br2024\\internacionais\\giz")
###### import datasets #########

setwd(dir_giz_output)

df_giz_clear<- readRDS("giz_manual_filter.rds")


"relational table"

setwd(dir_giz_doc)

df_giz_relational_source <- read.xlsx("14_giz_relational_table.xlsx", sheet = "source_landscape") %>% select(-'export_select_participating_org_funding_de_1') %>% 
  dplyr::mutate(source_original = str_trim(source_original))

df_giz_relational_channel <- read.xlsx("14_giz_relational_table.xlsx", sheet = "channel_landscape")%>% select(-'export_select_participating_org_implementing')


########## transforms ######

df_giz_transform <- df_giz_clear %>% 
  dplyr::rename(id_original = iati_identifier,
                source_original = export_select_participating_org_funding_de_1,
                channel_original = export_select_participating_org_implementing,
                value_original_currency = outgoing_commitment,
                sector_original = sector,
                project_description = export_select_description_objectives,
                project_name = title) %>% 
  dplyr::mutate(data_source = "idb_projects",
                municipality= "-",
                activity_landscape = "-",
                subactivity_landscape = "-",
                rio_marker = "-",
                beneficiary_landscape = "-",
                beneficiary_public_private = "-", region = "-" , uf = "-",
                original_currency = "EUR",
                instrument_original = "-",
                instrument_landscape = "-", subsector_original = "-", beneficiary_original ="-", localization_original = "-")


######### join with categories in relational table ######

df_giz_transform <- left_join(df_giz_transform,df_giz_relational_source, by = "source_original") 

df_giz_transform <- left_join(df_giz_transform, df_giz_relational_channel, by = "channel_original")


########### change climate ###########


df_giz_transform <- df_giz_transform %>% dplyr::mutate(climate_component = ifelse(id_original %in% c("de-1-202330272-0","de-1-202330280-0",	"de-1-202330579-0",	"de-1-202330892-0","de-1-202331528-0",	"de-1-202333631-0"), "Mitigation" , "-")) %>% 
                                                         dplyr::mutate(climate_component = ifelse(id_original %in% c("de-1-202331965-0",	"de-1-202332047-0",	"de-1-202332526-0"), "Dual", climate_component)) %>% 
  dplyr::mutate(climate_component = ifelse(id_original %in% c("de-1-202306009-6822","de-1-202306009-7336",	"de-1-202306009-7352",	"de-1-202321453",	"de-1-202330256-0",	"de-1-202330264-0",	"de-1-202330546-0",	"de-1-202330553-0",
                                                             "de-1-202330595-0",	"de-1-202330645-0",	"de-1-202330652-0",	"de-1-202330884-0",	"de-1-202330918-0",	"de-1-202331502-0",	"de-1-202331536-0",
                                                             "de-1-202331544-0",	"de-1-202331999-0",	"de-1-202332005-0", "de-1-202332013-0",	"de-1-202332245-0",	
                                                             "de-1-202332252-0",	"de-1-202332666-0",	"de-1-202332674-0",	"de-1-202332682-0",	"de-1-202333383-0",	"de-1-202333623-0",	
                                                             "de-1-202333821-0",	"de-1-202333839-0",	"de-1-202339042-0",	"de-1-202376218-0"), "Adaptation", climate_component))

lista2 <- df_giz_transform %>% select(id_original)


# lista <- c("de-1-202306009-6822", "de-1-202306009-7336", "de-1-202306009-7352", "de-1-202321453", "de-1-202330256-0", "de-1-202330264-0", "de-1-202330272-0", "de-1-202330280-0", "de-1-202330546-0", "de-1-202330553-0", "de-1-202330579-0", "de-1-202330595-0", "de-1-202330645-0", "de-1-202330652-0", "de-1-202330884-0", "de-1-202330892-0", "de-1-202330918-0", "de-1-202331502-0", "de-1-202331528-0", "de-1-202331536-0", "de-1-202331544-0", "de-1-202331965-0", "de-1-202331999-0", "de-1-202332005-0", "de-1-202332013-0", "de-1-202332047-0", "de-1-202332245-0", "de-1-202332252-0", "de-1-202332526-0", "de-1-202332666-0", "de-1-202332674-0", "de-1-202332682-0", "de-1-202333383-0", "de-1-202333623-0", "de-1-202333631-0", "de-1-202333821-0", "de-1-202333839-0", "de-1-202339042-0", "de-1-202376218-0")
# lista <- data.frame(c("de-1-202306009-6822", "de-1-202306009-7336", "de-1-202306009-7352", "de-1-202321453", "de-1-202330256-0", "de-1-202330264-0", "de-1-202330272-0", "de-1-202330280-0", "de-1-202330546-0", "de-1-202330553-0", "de-1-202330579-0", "de-1-202330595-0", "de-1-202330645-0", "de-1-202330652-0", "de-1-202330884-0", "de-1-202330892-0", "de-1-202330918-0", "de-1-202331502-0", "de-1-202331528-0", "de-1-202331536-0", "de-1-202331544-0", "de-1-202331965-0", "de-1-202331999-0", "de-1-202332005-0", "de-1-202332013-0", "de-1-202332047-0", "de-1-202332245-0", "de-1-202332252-0", "de-1-202332526-0", "de-1-202332666-0", "de-1-202332674-0", "de-1-202332682-0", "de-1-202333383-0", "de-1-202333623-0", "de-1-202333631-0", "de-1-202333821-0", "de-1-202333839-0", "de-1-202339042-0", "de-1-202376218-0"))
# 
# anti <- anti_join(lista,lista2)


############ apply deflate and exchange ###########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

# source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

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
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * 5.363696414 * deflator),
                  value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_giz_calculus <- deflate_and_exchange(tabela_deflator, df_giz_transform, tabela_cambio) 

df_giz_calculus <- df_giz_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

############ save rds ###########

setwd(dir_giz_output)

saveRDS(df_giz_calculus,"df_giz_transform_landscape.rds")
