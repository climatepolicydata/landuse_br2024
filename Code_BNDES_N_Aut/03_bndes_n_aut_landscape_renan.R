library(tidyverse)
library(readxl)
library(stringi)
library(readr)
# library(xlsx)

# Fazendo o load do script com as funções para classificar o sector landscape
# source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

github <- "Documents"
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
source(paste0(root,github,"/GitHub/landuse_br2024/AuxFolder/Dictionary_Sectors.R"))

df_bndes_filter <- read_rds("A:\\projects\\landuse_br2024\\bndes_n_aut\\output\\df_bndes_n_aut_filter_reviewed_03_24.rds")%>%as_tibble
df_bndes_filter%>%view
source_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")%>%select(channel_original,channel_landscape)

sector_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "sector_landscape")%>%select(sector_original,subsector_original)

instrument_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()%>% select(instrument_original,instrument_landscape)
beneficiary_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape") %>% select(beneficiary_original,beneficiary_landscape,beneficiary_public_private)

climate_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "climate_select") %>% 
  dplyr::mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>%
  dplyr::mutate(climate_original = paste0(id_original,year,
                                          project_name, sector_original))
#   mutate_if(is.character, tolower) %>% filter(!id_original == "15208221")

# df_bndes_filter_landscape <- df_bndes_filter %>% dplyr::mutate(
#     id_original = numero_do_contrato) %>% 
#   dplyr::filter(id_original %in% climate_bndes_n_aut$id_original)

# Inicio da transformacao para landscapedf_bndes_filter
df_bndes_filter_landscape <- df_bndes_filter %>% dplyr::rename(id_original = numero_do_contrato,
                                                               sector_original = subsetor_cnae_nome,
                                                               value_original_currency = valor_contratado_reais,
                                                               instrument_original = instrumento_financeiro) %>% 
  dplyr::mutate(data_source = "bndes_naut",
                year = ano,
                project_name = cliente,
                project_description = descricao_do_projeto,
                source_original = fonte_de_recurso_desembolsos,
                climate_original = paste0(id_original,year,
                                          project_name, sector_original),
                original_currency = "brl",
                channel_original = str_c(forma_de_apoio,instituicao_financeira_credenciada,sep = "_"),
                rio_marker = "-",
                beneficiary_original = str_c(natureza_do_cliente,porte_do_cliente,cliente,sep = "_"),
                localization_original = uf,
                region = "-",
                uf = uf,
                municipality = municipio)

df_bndes_filter_landscape$value_original_currency <- gsub(",", ".", df_bndes_filter_landscape$value_original_currency)

df_bndes_filter_landscape$value_original_currency <- as.numeric(df_bndes_filter_landscape$value_original_currency)



df_bndes_filter_landscape <- df_bndes_filter_landscape %>% dplyr::filter(climate_original %in% climate_bndes_n_aut$climate_original)

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape, climate_bndes_n_aut %>% select(climate_original,sector_landscape, activity_landscape,
                                                                                                 subactivity_landscape, climate_component), by= "climate_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,source_bndes_n_aut,by="source_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,channel_bndes_n_aut,by="channel_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,instrument_bndes_n_aut,by="instrument_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,sector_bndes_n_aut,by="sector_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,beneficiary_bndes_n_aut,by="beneficiary_original")

#### deflate

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

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
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
                  value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

dados2<- read_rds("A:\\projects\\brlanduse_landscape102023\\output_final\\base_landscape_final_20052024_reviewed.rds")
dados2 <- dados2 %>% select(-value_brl_deflated_mean,-value_usd_mean)
dados2<-dados2 %>% filter(data_source  == "bndes_naut")


df_bndes_naut_calculus <- deflate_and_exchange(tabela_deflator, df_bndes_filter_landscape, tabela_cambio)


df_bndes_naut_calculus <- df_bndes_naut_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

a <- rbind(df_bndes_naut_calculus,dados2) %>% select(-value_usd,-value_brl_deflated)

df_bndes_naut_calculus <- deflate_and_exchange(tabela_deflator, a, tabela_cambio)

df_bndes_naut_calculus <- df_bndes_naut_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)


saveRDS(df_bndes_naut_calculus,"A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\data_bndes_landscape_25062024.rds")

write.xlsx(df_bndes_naut_calculus,"A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\data_bndes_landscape_25062024.xlsx")
