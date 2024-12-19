##################

# Author : Renan Morais
# Date: 26-05-2023
# Email: renanflorias@hotmail.com
# Goal: join base: sicor_operacao_basica_estado with table "empreendimento"
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

options(scipen = 999)

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
github <- "documents"
dir_bcb<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned")

dir_bcb_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_bioeconomy <- ("A:\\projects\\bioeconomy_landscape2024\\output")

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_sicor_doc <- ("A:/finance/sicor/rawData/auxiliary")


dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

### import dataset ############

setwd(dir_sicor_output)
mdcr_op_basic_modify <- readRDS("df_sicor_op_basica_all_dummies_aggregate_v2.RDS") %>% filter(CODIGO_PRODUTO %in% c(160,1420,3380,4920,5660,6640,40,1770,5860,660,2080,1780,3780,5640,4760,680,2980,6720), ANO >= 2021 & ANO <=2023)

df_all_products <- readRDS("df_sicor_op_basica_all_dummies_aggregate_v2.RDS") %>% filter(ANO >= 2021 & ANO <=2023)

df_final <- mdcr_op_basic_modify %>% 
  dplyr::rename(value_original_currency = VL_PARC_CREDITO, year = ANO) %>% 
  mutate(original_currency = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)

df_final_full <- df_all_products %>% 
  dplyr::rename(value_original_currency = VL_PARC_CREDITO, year = ANO) %>% 
  mutate(original_currency = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)

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
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}


df_sicor_calculus <- deflate_and_exchange(tabela_deflator, df_final, tabela_cambio)

df_full <- deflate_and_exchange(tabela_deflator, df_final_full, tabela_cambio)

setwd(dir_bioeconomy)

write.xlsx(df_sicor_calculus,"df_comparative_soja.xlsx")
write.xlsx(df_full,"df_comparative_full.xlsx")

######### comparative ########


#### sum by finalidade

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(df_sicor)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("finalidade")
pt$addRowDataGroups("ano")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="Total Parc Credito", 
                     summariseExpression="sum(vl_parc_credito)")

# Renderizar a tabela
pt$renderPivot()
