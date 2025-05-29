##################

# Author : Renan Morais
# Date: 02-01-2024
# Email: renanflorias@hotmail.com
# Goal: clear B3 CBIOS database
# resource: 


# Modified by Julia Niemeyer
# Date 29/05/2025


########################## ACTION NEEDED ##################################

# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2020 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2014 #the year to base inflation
# #
# # # ## set the path to your github clone
github <- "Documents/"

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

interval <- ano_ini:ano_fim

valor_financeiro <- data.frame()
tabela_valores_ano <- data.frame()

for (i in interval){
  
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
         sector_landscape = "Bioenergy and fuels",
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

rm(tabela_valores_ano,valor_financeiro, interval, df_b3_clear)


################## apply deflate and exchange ####################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

############## ATUALIZADO EM 2025 -- automatico -- atualiza com base em ano_ini e ano_fim
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))

####### rodar essa função para atualizar a tabela de taxa de cambio
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)



df_b3_cbios_calculus <- deflate_and_exchange(tabela_deflator, df_b3_transform_landscape, tabela_cambio)


#### to select variables ####


df_b3_cbios_calculus <- df_b3_cbios_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

#save in rds and excel

saveRDS(df_b3_cbios_calculus, paste0(dir_b3_project, "/b3_cbios_landscape_final_", ano_ini, "-", ano_fim, ".rds"))
write.xlsx(df_b3_cbios_calculus, paste0(dir_b3_project,"b3_cbios_landscape_final_", ano_ini, "-", ano_fim, ".xlsx"))
