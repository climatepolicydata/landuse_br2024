
##################

# Author : Renan Morais
# Date: 24-04-2023
# Email: renanflorias@hotmail.com
# Goal: join base: sicor_operacao_basica_estado with table "empreendimento"
# resource: 

########################### Libraries ######################################

pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

## set anos de analise caso não esteja rodando pelo master
# 

# Fill the information to run your analysis
# ano_ini = 2022 #the initial year to star analysis
# ano_fim = 2024 #the final year to end your analysis
# ano_base = 2024 #the year to base inflation
# 
# ## set the path to your github clone
# github <- "Documents"

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_bcb<- ("A:/finance/sicor/cleanData")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR")

dir_bcb_clear <- ("A:/finance/sicor/cleanData")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

### import dataset ############

setwd(dir_sicor_output)


df_sicor <- readRDS("df_sicor_op_basica_pre_dummie_aggregate.RDS")


#### df "bcb_c82" possui todos códigos e descrições das características climáticas da consulta pública 82

#### insert dummies

setwd(dir_sicor_landuse2024)

bcb_c82 <- read.xlsx("01_sicor_relational_tables.xlsx", sheet = "climate_use_bcb_82")
bcb_c82 <- bcb_c82 %>% select(-USE_IRRIGACAO,-USE_MODALIDADE, -USE_PRODUTO,
                                  -USE_SUBPROGRAMA, -USE_CULTIVO, -USE_INTEGR, -USE_AGRICULTURA,
                              -USE_PRODUTO_2, - USE_SUBPROGRAMA, -USE_VARIEDADE, -USE_PROGRAMA_ABC,
                              -USE_SUBPROGRAMA_PRONAF)


# names(bcb_c82) <- tolower(names(bcb_c82))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor %>%
  mutate(DUMMY_TP_AGRICULTURA = if_else(CD_TIPO_AGRICULTURA  %in% bcb_c82$CD_TIPO_AGRICULTURA, 1, 0)) %>%
  relocate(DUMMY_TP_AGRICULTURA, .after = CD_TIPO_AGRICULTURA) %>%
  mutate(DUMMY_TP_CULTIVO = if_else(CD_TIPO_CULTIVO %in% bcb_c82$CD_TIPO_CULTIVO, 1, 0)) %>%
  relocate(DUMMY_TP_CULTIVO, .after = CD_TIPO_CULTIVO) %>%
  mutate(DUMMY_TP_INTEGRACAO = if_else(CD_TIPO_INTGR_CONSOR  %in% bcb_c82$CD_TIPO_INTGR_CONSOR, 1, 0)) %>%
  relocate(DUMMY_TP_INTEGRACAO, .after = CD_TIPO_INTGR_CONSOR) %>%
  # mutate(DUMMY_PROGRAMA = if_else(CD_PROGRAMA  %in% bcb_c82$CD_PROGRAMA, 1, 0)) %>%
  # relocate(DUMMY_PROGRAMA, .after = CD_PROGRAMA) %>%
  mutate(DUMMY_SUBPROGRAMA = if_else(CD_SUBPROGRAMA  %in% bcb_c82$CD_SUBPROGRAMA, 1, 0)) %>%
  relocate(DUMMY_SUBPROGRAMA, .after = CD_SUBPROGRAMA) %>%
  mutate(DUMMY_TP_IRRIGACAO = if_else(CD_TIPO_IRRIGACAO  %in% bcb_c82$CD_TIPO_IRRIGACAO, 1, 0)) %>%
  relocate(DUMMY_TP_IRRIGACAO, .after = CD_TIPO_IRRIGACAO) %>%
  mutate(DUMMY_MODALIDADE = if_else(CODIGO_MODALIDADE  %in% bcb_c82$CODIGO_MODALIDADE, 1, 0)) %>%
  relocate(DUMMY_MODALIDADE, .after = CODIGO_MODALIDADE) %>%
  mutate(DUMMY_PRODUTO = if_else(CODIGO_PRODUTO  %in% bcb_c82$CODIGO_PRODUTO, 1, 0)) %>%
  relocate(DUMMY_PRODUTO, .after = CODIGO_PRODUTO) %>%
  mutate(DUMMY_ABC = if_else(CD_PROGRAMA %in% bcb_c82$CD_PROGRAMA, 1, 0)) %>% 
  # relocate(DUMMY_ABC, .after = DUMMY_PRODUTO) %>%
  mutate(DUMMY_PRONAF_ABC = if_else(CD_SUBPROGRAMA %in% bcb_c82$CD_SUBPROGRAMA_PRONAF_ABC, 1, 0)) %>% 
  mutate(DUMMY_VARIEDADE = if_else(CODIGO_VARIEDADE  %in% bcb_c82$CODIGO_VARIEDADE, 1, 0)) %>%
  relocate(DUMMY_VARIEDADE, .after = CODIGO_VARIEDADE) 

rm(df_sicor)

#dummies conjuntas

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>% 
  mutate(modalidade_produto = paste(CODIGO_MODALIDADE, CODIGO_PRODUTO, sep= "_"))


#merge to condition modalidade and produto

#DUMMY PRODUTO E MODALIDADE

bcb_c82 <- bcb_c82 %>% mutate(modalidade_produto = paste("12", CODIGO_PRODUTO, sep = "_"))
#I manually have verified 12_NULL does not exist in sicor, but let's remove it : 
bcb_c82 <- bcb_c82 %>% mutate(modalidade_produto =ifelse(modalidade_produto=="12_NULL","NULL", modalidade_produto))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  mutate(DUMMY_PRODUTO_MODALIDADE = if_else((modalidade_produto %in% bcb_c82$modalidade_produto) ,1,0)) %>% 
  mutate(DUMMY_PRODUTO_MODALIDADE = if_else((CODIGO_MODALIDADE == 11) & (CODIGO_PRODUTO != 3630),1,DUMMY_PRODUTO_MODALIDADE))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  relocate(DUMMY_PRODUTO_MODALIDADE, .after = CODIGO_MODALIDADE)

                                      
#DUMMY_PRODUTO_FINALIDADE_VARIEDADE


df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  mutate(DUMMY_PRODUTO_FINALIDADE_VARIEDADE = if_else((CODIGO_PRODUTO  %in% bcb_c82$CODIGO_PRODUTO_2),1,0))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  mutate(DUMMY_PRODUTO_FINALIDADE_VARIEDADE = if_else((CODIGO_FINALIDADE  %in% bcb_c82$CD_FINALIDADE_EXCEÇÃO),0,DUMMY_PRODUTO_FINALIDADE_VARIEDADE))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  mutate(DUMMY_PRODUTO_FINALIDADE_VARIEDADE = if_else((CODIGO_PRODUTO==1880) & (CODIGO_VARIEDADE==370),0,DUMMY_PRODUTO_FINALIDADE_VARIEDADE))

df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  mutate(DUMMY_PRODUTO_FINALIDADE_VARIEDADE = if_else((CODIGO_PRODUTO==6640) & (CODIGO_VARIEDADE==213),0,DUMMY_PRODUTO_FINALIDADE_VARIEDADE))


df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>%
  relocate(DUMMY_PRODUTO_FINALIDADE_VARIEDADE, .after = CODIGO_VARIEDADE)



############ new dummy ###########

# df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>% 
#   mutate(DUMMY_ABC = if_else(CD_PROGRAMA %in% c(156,180), 1, 0))
# 
# "dummy com o objetivo de captar os programas do pronaf ABC"
# 
# df_sicor_op_basica_empreendimento_all_dummies <- df_sicor_op_basica_empreendimento_all_dummies %>% 
#   mutate(DUMMY_PRONAF_ABC = if_else(CD_SUBPROGRAMA %in% c(4,5,8,52,1234), 1, 0))



setwd(dir_sicor_output)

saveRDS(df_sicor_op_basica_empreendimento_all_dummies, "df_sicor_op_basica_all_dummies_aggregate_v2.RDS")
write.xlsx(df_sicor_op_basica_empreendimento_all_dummies,"df_sicor_op_basica_all_dummies_aggregate.xlsx")


########deflated##############


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

df_deflated <- df_sicor_op_basica_empreendimento_all_dummies %>% dplyr::rename(year = ANO,
                                                                                          value_original_currency = VL_PARC_CREDITO)
df_deflated <- deflate_and_exchange(tabela_deflator, df_deflated, tabela_cambio)

write.xlsx(df_deflated,"df_sicor_deflated_analise.xlsx")

