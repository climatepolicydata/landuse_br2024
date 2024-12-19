##################

# Author : Renan Morais
# Date: 11-04-2024
# Email: renanflorias@hotmail.com
# Goal: filter bndes indiretas automaticas
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
               pivottabler,
               cld3)

##### directory #########

dir_bndes_aut_raw <- ("A:/finance/bndes_Aut/rawData")

dir_bndes_aut_clear <- ("A:/finance/bndes_Aut/CleanData")


dir_project_bndes_output <- ("A:/projects/landuse_br2024/BNDES_Aut/output")


dir_bndes_aut_doc <- ("A:/projects/landuse_br2024/BNDES_Aut")


setwd(dir_bndes_aut_doc)

sector_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "sector_landscape")

instrument_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "instrument_landscape") %>% 
  dplyr::mutate(instrument_original = tolower(instrument_original),
                instrument_original = stri_trans_general(instrument_original, "Latin-ASCII"))



##### import datasets #########

setwd(dir_bndes_aut_clear)

df_bndes_aut <- readRDS("operacoes_financiamento_operacoes_indiretas_automatica_clear.RDS")


###### filter year #####

df_bndes_aut_filter <- df_bndes_aut %>%
  filter(ano >= 2015 & ano <= 2023) %>%
  dplyr::mutate(instrumento_financeiro = tolower(instrumento_financeiro))

setwd(dir_project_bndes_output)

write.xlsx(df_bndes_aut_filter,"total_bndes_aut_full_2015_2023.xlsx")

###### filter without 'credito rural' #####

lista_instrument <- c("pronamp custeio", "pronamp investimento", "pronaf investimento","moderagro", "moderfrota", "moderinfra","prodecoop",
                      "pronaf investimento","pronaf investimento", "pronamp custeio","pronamp investimento","programa abc","inovagro")

df_bndes_without_cr <- df_bndes_aut_filter %>% filter(!instrumento_financeiro %in% lista_instrument) %>% 
  dplyr::rename(value_original_currency = valor_desembolsado_reais,
                year = ano) %>% dplyr::mutate(original_currency = "brl")



##### deflated #####

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
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}


df_bndes_aut <- deflate_and_exchange(tabela_deflator, df_bndes_without_cr, tabela_cambio)


############ bndes_n aut#############

df <- readRDS("A:\\finance\\bndes_naut\\cleanData\\operacoes_financiamento_operacoes_nao_automaticas_clear_03_24.rds")

df_filter_naut <- df %>% filter(ano >= 2015 & ano <= 2023)


df_bndes_naut_without_cr <- df_filter_naut %>% filter(!instrumento_financeiro %in% lista_instrument) %>% 
  dplyr::rename(value_original_currency = valor_desembolsado_reais,
                year = ano) %>% dplyr::mutate(original_currency = "brl")


df_bndes_naut <- deflate_and_exchange(tabela_deflator, df_bndes_naut_without_cr, tabela_cambio)



####save###
setwd(dir_project_bndes_output)

write.xlsx(df_bndes_aut,"total_bndes_aut_full_2015_2023.xlsx")

setwd("A:\\projects\\landuse_br2024\\bndes_n_aut\\output")

write.xlsx(df_bndes_naut,"total_bndes_naut_full_2015_2023.xlsx")
