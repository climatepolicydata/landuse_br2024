##################

# Author : Renan Morais
# Date: 26-05-2023
# Email: renanflorias@hotmail.com
# Goal: join base: sicor_operacao_basica_estado with table "empreendimento"
# resource: 

### Modified by Julia Niemeyer
# Date 25/05/2025
tic()

## set anos de analise caso não esteja rodando pelo master
# 

# ano_ini = 2022 #the initial year to star analysis
# ano_fim = 2024 #the final year to end your analysis
# ano_base = 2024 #the year to base inflation
# 

# ## set the path to your github clone
# github <- "Documents"

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
dir_bcb<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned")

dir_bcb_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_output <- paste0("A:/projects/landuse_br2024/sicor/output/", ano_ini, "-", ano_fim)



##### import datasets #########
setwd(dir_output)
mdcr_op_basic_modify <- readRDS(paste0("df_sicor_op_basica_all_dummies_aggregate_v2_", ano_ini, "-", ano_fim, ".RDS"))



########### Create variable sum dummy to filter operations at least 1 dummy #####


"foi retirado da soma a dummy produto por suposta duplicação de contabilização"  
mdcr_op_basic_modify <- mdcr_op_basic_modify %>% 
  mutate(sum_dummy = DUMMY_TP_AGRICULTURA + DUMMY_TP_CULTIVO + DUMMY_TP_INTEGRACAO +
           DUMMY_SUBPROGRAMA + DUMMY_TP_IRRIGACAO + DUMMY_MODALIDADE + DUMMY_ABC + DUMMY_PRONAF_ABC+
           DUMMY_PRODUTO_FINALIDADE_VARIEDADE + DUMMY_PRODUTO_MODALIDADE + DUMMY_VARIEDADE)


mdcr_op_basic_modify_filter <- mdcr_op_basic_modify %>%
  dplyr::filter(sum_dummy >= 1)

mdcr_op_basic_modify_filter <- mdcr_op_basic_modify_filter %>% filter(!CODIGO_FINALIDADE == 1)

#rm(mdcr_op_basic_modify)
setwd(dir_output)

saveRDS(mdcr_op_basic_modify_filter, paste0("sicor_op_basica_sum_dummies_aggregate_v2_", ano_ini, "-", ano_fim, ".RDS"))
write.xlsx(mdcr_op_basic_modify_filter,paste0("sicor_op_basica_sum_dummies_aggregate_v2_", ano_ini, "-", ano_fim, ".xlsx"))


############## ATUALIZADO EM 2025 -- automatico -- atualiza com base em ano_ini e ano_fim
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)

tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca, current_year)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)

sicor_op_basica_sum_dummies_aggregate_v2 <- mdcr_op_basic_modify_filter %>% dplyr::rename(year = ANO,
                                                                                          value_original_currency = VL_PARC_CREDITO)

sicor_op_basica_sum_dummies_aggregate_v2 <- deflate_and_exchange(tabela_deflator, sicor_op_basica_sum_dummies_aggregate_v2, tabela_cambio)

write.xlsx(sicor_op_basica_sum_dummies_aggregate_v2, paste0("sicor_op_basica_sum_dummies_aggregate_climate", ano_ini, "-", ano_fim, ".xlsx"))


mdcr_op_basic_modify <- mdcr_op_basic_modify %>% 
  dplyr::rename(year = ANO, value_original_currency = VL_PARC_CREDITO)

mdcr_op_basic_modify <- deflate_and_exchange(tabela_deflator, mdcr_op_basic_modify, tabela_cambio)

write.xlsx(mdcr_op_basic_modify, paste0("sicor_op_basica_sum_dummies_no_filter", ano_ini, "-", ano_fim, ".xlsx"))

toc()
gc()
