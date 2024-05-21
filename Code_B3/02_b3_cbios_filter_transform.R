##################

# Author : Renan Morais
# Date: 02-01-2024
# Email: renanflorias@hotmail.com
# Goal: clear B3 CBIOS database
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
               pivottabler)

##### directory #########

dir_b3_output<- ("A:/finance/b3_cbios/cleanData")

dir_b3_project <-("A:/projects/landuse_br2024/b3/output")

################ import databases #############

setwd(dir_b3_output)

df_b3_clear <- readRDS("cbios_b3_nd_clear.rds")


#função para calcular os valores financeiros dos anos

CalculaValorFinanc <-  function(data, select_ano){
  

  valor_finc_ano <- data %>% select(ano, valor_financeiro) %>% 
    group_by(ano) %>% 
    filter(ano == select_ano) %>% 
    summarise(soma = sum(valor_financeiro/2)) %>% 
    mutate(ano = select_ano)
  
  return(valor_finc_ano)

}

## o for evita que digitalizemos a mesma função para os anos de interesse (dupla contagem).
#caso seja necessário aumentar o intervalo de anos a serem analisados basta acrescentar na lista "anos"

anos <- 2020:2023

valor_financeiro <- data.frame()
tabela_valores_ano <- data.frame()

for (i in anos){
  
  valor_financeiro <- CalculaValorFinanc(df_b3_clear, i)
  tabela_valores_ano <- bind_rows(valor_financeiro,tabela_valores_ano)
  
}

tabela_valores_ano <- tabela_valores_ano %>% arrange(ano)


#### transform landscape

df_b3_transform_landscape <- tabela_valores_ano %>% 
  mutate(data_source = "b3_cbios",
         id_original = "-",
         project_name = "CBIOs - Decarbonization Credit - Tradded volume",
         project_description = "The Decarbonization Credit (CBIO) is an instrument adopted by RenovaBio as a tool to reach this target. CBIOs will be issued by biofuels producers and importers duly certified by the National Petroleum Agency (ANP), based on their purchase and sale invoices. Each CBIO is equivalent to 1 ton of CO2 avoided, it will not expire and can only be withdrawn from circulation when its retirement is requested.",
         source_original = "Fuel distributors - mandatory and non-mandatory buyers.",
         source_finance_landscape = "Corporations",
         origin_domestic_international = "National",
         origin_private_public = "Private",
         original_currency = "BRL",
         channel_original = "Instituições Financeiras",
         channel_landscape = "Financial Institution",
         instrument_original = "CBIOs - Decarbonization Credit",
         instrument_landscape = "CBIOs",
         sector_original = "Petróleo e Biocombustível",
         sector_landscape = "Bioenergy and Fuels",
         subsector_original = "-",
         activity_landscape = "Production of biofuels, including biodiesel and bioethanol",
         subactivity_landscape = "Issuance of CBIOs by biofuel producers and importers based on commercialization of their production.",
         climate_component = "Mitigation",
         rio_marker = "-",
         beneficiary_original = "biofuel producer /importer",
         beneficiary_landscape = "Corporations",
         beneficiary_public_private = "Private",
         localization_original = "-",
         region = "-",
         uf = "-",
         municipality = "-") %>% 
  dplyr::rename(year = ano,
                value_original_currency = soma)

rm(tabela_valores_ano,valor_financeiro, anos, df_b3_clear)


################## apply deflate and exchange ####################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


"The object github could be receive the file that corresponds to the project repository"

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/Funcao_taxa_cambio_v2.r"))

ano_ini = 2020
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

df_b3_cbios_calculus <- deflate_and_exchange(tabela_deflator, df_b3_transform_landscape, tabela_cambio)


#### to select variables ####

rm(cambio_sgs,df_b3_transform_landscape, ibge_ipca, tabela_cambio, tabela_deflator, teste)

df_b3_cbios_calculus <- df_b3_cbios_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

#save in rds

setwd(dir_b3_project)


saveRDS(df_b3_cbios_calculus, "b3_cbios_landscape_final.rds")
write.xlsx(df_b3_cbios_calculus,"b3_cbios_landscape_final.xlsx")
