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

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_bcb<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned")

dir_bcb_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/")

dir_bcb_output<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/3_Temp files")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR_2021")


##### import datasets #########
setwd("A:/projects/brlanduse_landscape102023/sicor/tempfiles")
mdcr_op_basic_modify <- readRDS("df_sicor_op_basica_all_dummies_att.RDS")

setwd(dir_bcb_doc)
produto <- read.csv("Produto.csv", sep = ";", encoding = "latin1")
modalidade <- read.csv("Modalidade.csv", sep = ";", encoding = "latin1")

############ clear ###########

# produto$PRODUTO <- tolower(produto$PRODUTO)
# modalidade$MODALIDADE <- tolower(modalidade$MODALIDADE)

### mudança das colunas para maiusculo com o objetivo de sincronizar os códigos de sicor 03 e 04

names(mdcr_op_basic_modify) <- toupper(names(mdcr_op_basic_modify))

############ new dummy ###########

# mdcr_op_basic_modify <- mdcr_op_basic_modify %>% 
#   mutate(DUMMY_ABC = if_else(CD_PROGRAMA %in% c(156,180), 1, 0))
# 
# "dummy com o objetivo de captar os programas do pronaf ABC"
# 
# mdcr_op_basic_modify <- mdcr_op_basic_modify %>% 
#   mutate(DUMMY_PRONAF_ABC = if_else(CD_SUBPROGRAMA %in% c(4,5,8,52,1234), 1, 0))


# mdcr_op_basic_modify <- join(mdcr_op_basic_modify , modalidade, by ="MODALIDADE")
# 
# mdcr_op_basic_modify_v2 <- join(mdcr_op_basic_modify , produto, by ="PRODUTO")

mdcr_op_basic_modify <- mdcr_op_basic_modify%>%
  dplyr::rename(CD_PRODUTO = X.CODIGO) %>%
  relocate(DUMMY_TP_AGRICULTURA, .after = DUMMY_ABC) %>% 
  relocate(DUMMY_TP_CULTIVO, .after = DUMMY_TP_AGRICULTURA) %>% 
  relocate(DUMMY_TP_INTEGRACAO, .after = DUMMY_TP_CULTIVO) %>% 
  relocate(DUMMY_PROGRAMA, .after = DUMMY_TP_INTEGRACAO) %>% 
  relocate(DUMMY_SUBPROGRAMA, .after = DUMMY_PROGRAMA) %>% 
  relocate(DUMMY_TP_IRRIGACAO, .after = DUMMY_SUBPROGRAMA) %>% 
  relocate(DUMMY_MODALIDADE, .after = DUMMY_TP_IRRIGACAO) %>%
  relocate(DUMMY_PRODUTO, .after = DUMMY_MODALIDADE) %>% 
  relocate(DUMMY_PRONAF_ABC, .after = DUMMY_PRODUTO) %>% 
  relocate(CD_PRODUTO, .after = PRODUTO) %>% 
  relocate(CODIGO_MODALIDADE, .after = MODALIDADE)

  
mdcr_op_basic_modify_v2 <- mdcr_op_basic_modify %>% 
  mutate(sum_dummy = DUMMY_TP_AGRICULTURA + DUMMY_TP_CULTIVO + DUMMY_TP_INTEGRACAO +
           DUMMY_SUBPROGRAMA + DUMMY_TP_IRRIGACAO + DUMMY_MODALIDADE + DUMMY_PRODUTO + DUMMY_ABC + DUMMY_PRONAF_ABC)

rm(mdcr_op_basic_modify)

mdcr_op_basic_modify_filter <- mdcr_op_basic_modify_v2 %>%
  filter(sum_dummy >= 1) %>%
  filter(!(FINALIDADE %in% "comercialização"))

rm(mdcr_op_basic_modify_v2)
setwd("A:/projects/brlanduse_landscape102023/sicor/tempfiles")

saveRDS(mdcr_op_basic_modify_filter, "sicor_op_basica_DUMMY_ABC_PRONAF_att.RDS")

#write.csv(mdcr_op_basic_modify_filter, "sicor_op_basica_filter_consulta_pub_abc.csv")

rm(mdcr_op_basic_modify_filter, produto, modalidade)



