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

df_atlas <- readRDS("df_atlas_seguro_rural_clear.rds")


df_atlas <- df_atlas %>% 
  select(-data_apolice,-taxa_comercial_percent, -taxa_efetiva_paga_produtor_percent, -faixa_de_taxa,
         -area_intermediaria_ha, -area_segurada_ha, -valor_pago_de_indenizado_r) 

df_atlas <- df_atlas %>% 
  group_by(produto, categoria_de_produto, ramo_do_produto,tipo_de_produto,  sub_ramo_do_produto,ramo_do_produto_fesr,
           sisser_apolices_ds_atividade_bacen, seguradora, ano_apolice) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

"remove values with count for 'livestock' "

df_atlas <- df_atlas %>% 
  filter(!ramo_do_produto_fesr %in% c("SEGURO PECUÁRIO COM COBERTURA DO FESR","SEGURO PECUÁRIO SEM COBERTURA DO FESR"))

df_atlas_sub_negative <- aggregate( valor_subvencao ~ id_equals + produto + categoria_de_produto + ramo_do_produto + 
                                  tipo_de_produto + sub_ramo_do_produto + ramo_do_produto_fesr + 
                                  sisser_apolices_ds_atividade_bacen + seguradora + ano_apolice, data = df_atlas,
                                FUN = sum)

df_atlas_subvencao <- aggregate( valor_subvencao ~ id_equals + produto + categoria_de_produto + ramo_do_produto + 
                                  tipo_de_produto + sub_ramo_do_produto + ramo_do_produto_fesr + 
                                  sisser_apolices_ds_atividade_bacen + seguradora + ano_apolice, data = df_atlas,
                                FUN = sum)

rm(df_atlas)


############### transform ########

"base para valor subvenção negativo"

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  dplyr::rename(year = ano_apolice, channel_original = seguradora, sector_original = ramo_do_produto_fesr,
                subsector_original = sub_ramo_do_produto) %>% 
  mutate(data_source = "atlas_seguro_mapa", project_name = "Seguro Rural Valor Subvenção negativo", 
         project_description = "Valor de abate para o valor da subvenção do prêmio dos fluxos do ses/susep",
         final_beneficiaries_final = "-",
         climate_component = "-", CPI_riomarker = "-",
         CPI_riomarker_mitigation = "-", CPI_riomarker_adaption = "-", cambio = "-", und_orc= "-", org_orc = "-",
         und_orc_landscape = "-", Subfunção = "-", source_of_finance_original = "-",
         source_finance_landscape = "Rural Producers", source_domestic_international = "National",
         source_private_public = "Private", channel_landscape = "Financial institutions", 
         instrument_landscape = "Risk management",
         activity_landscape = "-", recipient_landscape = "Rural producers",
         recipient_original = "-", recipient_public_private = "-",
         region = "-", uf_landscape ="-",
         municipality = "-",
         climate_component = "Adaptation",
         instrument_original = "Seguro do Ramo Rural") %>%
  mutate(sector_landscape = if_else(sector_original %in% c("SEGURO AGRÍCOLA COM COBERTURA DO FESR",
                                                           "SEGURO AGRÍCOLA SEM COBERTURA DO FESR",
                                                           "NÃO INFORMADO"), 
                                    "Crop", if_else(sector_original %in% c("SEGURO FLORESTAS SEM COBERTURA DO FESR",
                                                           "SEGURO FLORESTAS COM COBERTURA DO FESR"),
                                                           "Forest", 
                                                           sector_original))) %>% 
  mutate(id_original = paste(id_equals,"VLN", sep = ""),
         subactivity_landscape = "Tipo de Produto") %>% 
  dplyr::rename(value_brl = valor_subvencao) %>% 
  mutate(value_brl = -1*value_brl)

"base para valor da subvenção"

df_atlas_subvencao <- df_atlas_subvencao %>% 
  dplyr::rename(year = ano_apolice, channel_original = seguradora, sector_original = ramo_do_produto_fesr,
                subsector_original = sub_ramo_do_produto) %>% 
  mutate(data_source = "atlas_seguro_mapa", project_name = "Seguro Rural Valor Subvenção", 
         project_description = paste(tipo_de_produto, sisser_apolices_ds_atividade_bacen, sep = "_"), final_beneficiaries_final = "-",
         climate_component = "-", CPI_riomarker = "-",
         CPI_riomarker_mitigation = "-", CPI_riomarker_adaption = "-", cambio = "-", und_orc= "-", org_orc = "-",
         und_orc_landscape = "-", Subfunção = "-", source_of_finance_original = "-",
         source_finance_landscape = "Federal and state governments", source_domestic_international = "National",
         source_private_public = "Public", channel_landscape = "Financial institutions", 
         instrument_landscape = "Risk management",
         activity_landscape = "-", recipient_landscape = "Rural producers",
         recipient_original = "-", recipient_public_private = "-",
         region = "-", uf_landscape ="-",
         municipality = "-",
         climate_component = "Adaptation",
         instrument_original = "Seguro do Ramo Rural") %>%
  mutate(sector_landscape = if_else(sector_original %in% c("SEGURO AGRÍCOLA COM COBERTURA DO FESR",
                                                           "SEGURO AGRÍCOLA SEM COBERTURA DO FESR",
                                                           "NÃO INFORMADO"), 
                                    "Crop", if_else(sector_original %in% c("SEGURO FLORESTAS SEM COBERTURA DO FESR",
                                                           "SEGURO FLORESTAS COM COBERTURA DO FESR"),
                                                           "Forest", 
                                                           sector_original))) %>% 
  mutate(id_original = paste(id_equals,"SUB", sep = ""),
         subactivity_landscape = "Rural insurance for farming and forestry") %>% 
  dplyr::rename(value_brl = valor_subvencao)

############ reogarnizando a base#########

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  select(id_original, data_source, year, project_name,
         project_description, source_of_finance_original, source_finance_landscape,
         source_domestic_international, source_private_public,
         value_brl,channel_original, channel_landscape, instrument_original,
         instrument_landscape, sector_original, sector_landscape,
         subsector_original,activity_landscape, recipient_original,
         recipient_landscape,recipient_public_private,
         region, uf_landscape,municipality,climate_component, final_beneficiaries_final,
         climate_component,CPI_riomarker,CPI_riomarker_mitigation,CPI_riomarker_adaption,
         und_orc,org_orc,und_orc_landscape,Subfunção,
         subactivity_landscape)

df_atlas_subvencao <- df_atlas_subvencao %>% 
  select(id_original, data_source, year, project_name,
         project_description, source_of_finance_original, source_finance_landscape,
         source_domestic_international, source_private_public,
         value_brl,channel_original, channel_landscape, instrument_original,
         instrument_landscape, sector_original, sector_landscape,
         subsector_original,activity_landscape, recipient_original,
         recipient_landscape,recipient_public_private,
         region, uf_landscape,municipality,climate_component, final_beneficiaries_final,
         climate_component,CPI_riomarker,CPI_riomarker_mitigation,CPI_riomarker_adaption,
         und_orc,org_orc,und_orc_landscape,Subfunção,
         subactivity_landscape)


"JUNÇÃO DAS BASES"

df_atlas_premio_liq_sub <- rbind(df_atlas_sub_negative,df_atlas_subvencao)



df_atlas_final <- df_atlas_premio_liq_sub %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  mutate(original_currency = "BRL",
         year = as.numeric(year)) %>%
  relocate(original_currency, .after = value_original_currency)

rm(df_atlas_premio_liq_sub, df_atlas_sub_negative, df_atlas_subvencao)



############ apply deflated and exchange #######

# Inserir o caminho onde foi feito o clone do projeto 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/Funcao_taxa_cambio_v2.r"))


ano_ini = 2015
ano_fim = 2020

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  filter(year >= 2015 & year <= 2020)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year") %>%
    left_join(tabela_cambio, by= "year")  %>%  
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_atlas_calculus <- deflate_and_exchange(tabela_deflator, df_atlas_final, tabela_cambio)

rm(cambio_sgs,df_atlas_final, ibge_ipca, tabela_cambio, tabela_deflator, teste)

df_atlas_calculus <- df_atlas_calculus %>% 
  dplyr::rename (source_original  = source_of_finance_original,
                 origin_domestic_international = source_domestic_international,
                 origin_private_public  = source_private_public) %>% 
  mutate(rio_marker = "-",
         beneficiary_original = "-",
         beneficiary_landscape = "Rural producers",
         uf = "-",
         beneficiary_public_private ="-",
         localization_original= "-")

df_atlas_calculus <- df_atlas_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

setwd("A:/projects/brlanduse_landscape102023/atlas_Seguro_Rural/output")

saveRDS(df_atlas_calculus,"database_atlas_vf_replication.rds")


############################### agregado por ramo do produto ############

# ptn <- PivotTable$new()
# 
# ptn$addData(df_atlas_premio_liq_sub)
# ptn$addColumnDataGroups("project_name")
# ptn$addColumnDataGroups("instrument_original")
# ptn$addRowDataGroups("year")
# ptn$defineCalculation(calculationName = "TOTAL_VALUE_BRL", summariseExpression = "sum(value_brl)")
# ptn$renderPivot()
# 
# wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wb, "valor_parc_credito")
# 
# writeData(wa,"my sheet 1",ptn$writeToExcelWorksheet(wb=wb, wsName = "valor_parc_credito", 
#                                                           topRowNumber=1, leftMostColumnNumber=1, 
#                                                           applyStyles=TRUE, mapStylesFromCSS=TRUE))
# 
# saveWorkbook(wb, file="df_atlas_prem_Sub_agregado_ramo_do_produto.xlsx", overwrite = TRUE)


############### backup data atlas clean ########
# setwd(dir_susep_dt_clean)
# 
# saveRDS(df_atlas,"df_atlas_cleaned.RDS")
# 
# saveRDS(df_atlas_prem_liq,"df_atlas_premio_produtor_landscape.rds")
# 
# saveRDS(df_atlas_subvencao,"df_atlas_subvencao.rds")
# 
# saveRDS(df_atlas_sub_negative,"df_atlas_sub_negativo.rds")
# 
# saveRDS(df_atlas_premio_liq_sub, "df_atlas_transform_complete.RDS")
# 
# write.csv2(df, "df_atlas_transform_complete_v2.csv", fileEncoding = "latin1")
# 
# 
# ############### base final  ########
# 
# saveRDS(df_atlas_subvencao,"df_atlas_subvencao.rds")
# 
# saveRDS(df_atlas_sub_negative,"df_atlas_sub_negativo.rds")
# 
# write.csv(df_atlas_sub_negative, "df_atlas_sub_negativo.csv", fileEncoding = "latin1")
# 
# write.csv(df_atlas_subvencao, "df_atlas_subvencao.csv", fileEncoding = "latin1")
