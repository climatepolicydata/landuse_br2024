##################
# Author : Renan Morais
# Date: 07/12/2023
# Email: renanflorias@hotmail.com
# Goal: filter ocde database att
# resource: OECD Miltilateral Climate Funds
#source: https://docs.ropensci.org/cld3/


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
               pivottabler,
               cld3)

##### directory #########

dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_oecd_project <- ("A:/projects/landuse_br2024/oecd_cf")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")


###### import datasets  & Select Variables #########

setwd(dir_oecd_Cf_clear)

df_oecd_clear <- readRDS("OECD_DAC_clear_2022.RDS") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2015, recipient == "brazil") %>% dplyr::mutate(source_original = paste(provider_type,provider_detailed,extending_agency,
                                                                                               sep="_"),
                                                                       value_original_currency = climate_related_development_finance_commitment_current_usd_thousand,
                                                                       original_currency = "USD")

setwd(dir_oecd_output)

######## apply deflate and create usd value ##########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

# github <- readline("digite a pasta do seu repositório clone: ")

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

# source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

cambio_sgs = read.csv("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio.csv") %>% select(-X)

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>% 
    dplyr::mutate(value_brl_deflated =  value_original_currency * cambio * deflator) %>% 
    dplyr::mutate(value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}

df_oecd_deflated = deflate_and_exchange(tabela_deflator, df_oecd_clear, tabela_cambio)


setwd("A:/projects/landuse_br2024/oecd_cf/checks")

write.xlsx(df_oecd_deflated,"ocde_total_source_original_2015_2022_deflated.xlsx")
