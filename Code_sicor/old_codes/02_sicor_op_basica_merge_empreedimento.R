##################

# Author : Renan Morais
# Date: 24-04-2023
# Email: renanflorias@hotmail.com
# Goal: join base: sicor_operacao_basica_estado with table "empreendimento"
# resource: 

########################### Libraries ######################################

pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr, data.table)
##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_bcb<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned")

dir_bcb_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/data_empreendimento/")

dir_bcb_output<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/3_Temp files")

##### import datasets #########
setwd(dir_bcb)

mdcr_operacao_basica_2015_2020 <- fread("sicor_2015_2020_25042023_v2.csv", sep = ",")
mdcr_operacao_basica_2015_2020$CD_EMPREENDIMENTO <- as.numeric(mdcr_operacao_basica_2015_2020$CD_EMPREENDIMENTO)

interesting_codes <- read.xlsx(paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/4_Documentation/Tabelas_Relacionais/códigos_consulta_pública_82_bcb.xlsx"))

setwd(dir_bcb_raw)

df_empreendimento <- read.csv("Empreendimento.csv", sep = ",", encoding = "latin1") %>%
  mutate_all(tolower)


############### clear datas and include information about empreendimento ##############
interesting_codes <- interesting_codes %>% 
  select(-DESC_MODALIDADE,-CD_MODALIDADE,-DESC_PRODUTO,-CD_PRODUTO,-CD_PRODUTO_FINANCIADO,-DESC_PRODUTO_FINANCIADO,-X19)


####### join #######

df_empreendimento <- df_empreendimento %>%
  select(-DATA_FIM) %>%
  dplyr::rename(CD_EMPREENDIMENTO = X.CODIGO)

mdcr_operacao_basica_2015_2020_modify <- join(mdcr_operacao_basica_2015_2020, df_empreendimento, by = "CD_EMPREENDIMENTO") %>%
  select(-DT_VENCIMENTO, -CD_FASE_CICLO_PRODUCAO, -VL_JUROS, -VL_PRESTACAO_INVESTIMENTO, -VL_PERC_RISCO_FUNDO_CONST, -VL_REC_PROPRIO_SRV,
         -VL_AREA_FINANC, -VL_PERC_CUSTO_EFET_TOTAL, -CD_TIPO_ENCARG_FINANC, -VL_JUROS, -VL_PRESTACAO_INVESTIMENTO, -VL_PREV_PROD,
         -VL_RECEITA_BRUTA_ESPERADA, -VL_REC_PROPRIO, -VL_REC_PROPRIO_SRV, -VL_PERC_RISCO_FUNDO_CONST, -VL_PERC_CUSTO_EFET_TOTAL,
         -CD_CONTRATO_STN, -CD_CNPJ_CADASTRANTE, -CEDULA_MAE, -DT_VENCIMENTO, -CD_INST_CREDITO, -VL_AREA_FINANC, -CD_FASE_CICLO_PRODUCAO,
         -CESTA, -ZONEAMENTO, -UNIDADE_MEDIDA, -UNIDADE_MEDIDA_PREVISAO, -CD_ESTADO, -CONSORCIO)


setwd(dir_bcb_output)

write.csv2(mdcr_operacao_basica_2015_2020_modify, "mdcr_op_basica_merge_empreendimento_clear.csv")
rm(mdcr_operacao_basica_2015_2020)

#including dummies 

df_sicor_op_basica_empreendimento_all_dummies <- mdcr_operacao_basica_2015_2020_modify %>% 
  mutate(DUMMY_TP_AGRICULTURA = if_else(CD_TIPO_AGRICULTURA  %in% interesting_codes$CD_TIPO_AGRICULTURA, 1, 0)) %>% 
  relocate(DUMMY_TP_AGRICULTURA, .after = CD_TIPO_AGRICULTURA) %>% 
  mutate(DUMMY_TP_CULTIVO = if_else(CD_TIPO_CULTIVO  %in% interesting_codes$CD_TIPO_CULTIVO, 1, 0)) %>% 
  relocate(DUMMY_TP_CULTIVO, .after = CD_TIPO_CULTIVO) %>% 
  mutate(DUMMY_TP_INTEGRACAO = if_else(CD_TIPO_INTGR_CONSOR  %in% interesting_codes$CD_TIPO_INTEGR_CONSOR, 1, 0)) %>% 
  relocate(DUMMY_TP_INTEGRACAO, .after = CD_TIPO_INTGR_CONSOR) %>% 
  mutate(DUMMY_PROGRAMA = if_else(CD_PROGRAMA  %in% interesting_codes$CD_PROGRAMA, 1, 0)) %>% 
  relocate(DUMMY_PROGRAMA, .after = CD_PROGRAMA) %>% 
  mutate(DUMMY_SUBPROGRAMA = if_else(CD_SUBPROGRAMA  %in% interesting_codes$CD_SUBPROGRAMA, 1, 0)) %>%
  relocate(DUMMY_SUBPROGRAMA, .after = CD_SUBPROGRAMA) %>% 
  mutate(DUMMY_TP_IRRIGACAO = if_else(CD_TIPO_IRRIGACAO  %in% interesting_codes$CD_TIPO_IRRIGACAO, 1, 0)) %>% 
  relocate(DUMMY_TP_IRRIGACAO, .after = CD_TIPO_IRRIGACAO) %>% 
  mutate(DUMMY_MODALIDADE = if_else(MODALIDADE  %in% interesting_codes$DESC_MODALIDADE, 1, 0)) %>% 
  relocate(DUMMY_MODALIDADE, .after = MODALIDADE) %>%
  mutate(DUMMY_PRODUTO = if_else(PRODUTO  %in% interesting_codes$DESC_PRODUTO, 1, 0)) %>% 
  relocate(DUMMY_PRODUTO, .after = PRODUTO)
rm(mdcr_operacao_basica_2015_2020_modify )


##### TRANSFORM VARIABLES ###

df_sicor_op_basica_empreendimento_all_dummies$DT_EMISSAO <- as.numeric(format(mdy(df_sicor_op_basica_empreendimento_all_dummies$DT_EMISSAO), '%Y'))

df_sicor_op_basica_empreendimento_all_dummies$VL_PARC_CREDITO <- as.numeric(df_sicor_op_basica_empreendimento_all_dummies$VL_PARC_CREDITO)

saveRDS(df_sicor_op_basica_empreendimento_all_dummies, "df_sicor_op_basica_all_dummies_190623_v2.RDS")

rm(df_sicor_op_basica_empreendimento_all_dummies)

##### validation by finalidade #######

# 
# ptvl_parc <- PivotTable$new()
# 
# ptvl_parc$addData(df_sicor_op_basica_empreendimento_all_dummies)
# # ptvl_parc$addColumnDataGroups("FINALIDADE")
# ptvl_parc$addRowDataGroups("DT_EMISSAO", totalCaption = "Total")
# ptvl_parc$defineCalculation(calculationName = "TOTAL_PARC_CREDITO", summariseExpression = "sum(VL_PARC_CREDITO)",format= "%.2f ")
# ptvl_parc$renderPivot()
# 
# wf <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wf, "FINALIDADE")
# 
# writeData(wf,"my sheet 1",ptvl_parc$writeToExcelWorksheet(wb=wf, wsName = "FINALIDADE",
#                                                                    topRowNumber=1, leftMostColumnNumber=1,
#                                                                    applyStyles=TRUE, mapStylesFromCSS=TRUE))
# 
# saveWorkbook(wf, file="total_credito_rural.xlsx", overwrite = TRUE)


############## validation about relation between subprograma and programa #############

# mdcr_op_baisc_modify <- readRDS("df_sicor_op_basica_all_dummies.RDS")
# 
# mdcr_op_baisc_modify <- mdcr_op_baisc_modify %>%
#   mutate(sum_dummy = DUMMY_TP_AGRICULTURA + DUMMY_TP_CULTIVO + DUMMY_TP_INTEGRACAO +
#            DUMMY_SUBPROGRAMA + DUMMY_TP_IRRIGACAO + DUMMY_MODALIDADE + DUMMY_PRODUTO) %>%
#   select(-CD_ESTADO)
# 
# mdcr_op_baisc_modify_filter <- mdcr_op_baisc_modify %>%
#   filter(sum_dummy >= 1)
# 
# 
# dummy_sub1 <- mdcr_op_baisc_modify %>%
#   filter(DUMMY_SUBPROGRAMA == 1)
# 
# pts <- PivotTable$new()
# 
# pts$addData(mdcr_op_baisc_modify_filter)
# pts$addColumnDataGroups("FINALIDADE")
# pts$addRowDataGroups("DT_EMISSAO")
# pts$defineCalculation(calculationName = "TOTAL_PARC_CREDITO", summariseExpression = "sum(VL_PARC_CREDITO)")
# pts$renderPivot()
# 
# wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wb, "Data")
# 
# pts$writeToExcelWorksheet(wb=wb, wsName="Data",
#                           topRowNumber=1, leftMostColumnNumber=1,
#                           applyStyles=TRUE, mapStylesFromCSS=TRUE)
# 
# saveWorkbook(wb, file="aggregate_finalidade_dummy1.xlsx", overwrite = TRUE)
# 
# 
