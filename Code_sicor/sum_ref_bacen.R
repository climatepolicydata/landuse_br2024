pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_bcb<- ("A:/finance/sicor/cleanData")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR")

dir_bcb_clear <- ("A:/finance/sicor/cleanData")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

### import dataset ############

# setwd(dir_sicor_output)
# 
# 
# df_sicor <- readRDS("df_sicor_op_basica_pre_dummie_aggregate.RDS")

df_sicor_filter <- df_sicor %>% filter(FINALIDADE != 'comercialização')


df_sicor_teste <- aggregate(vl_parc_credito ~ ref_bacen + cd_programa, data=df_sicor , FUN = "sum")

df_sicor_teste <- df_sicor_teste %>% mutate(duplicado = ifelse(duplicated(ref_bacen), "true", "false"))

teste2 <- df_sicor_teste$ref_bacen %>% duplicated %>% count()
1 FALSE 18142195
2  TRUE     1529

df_dupli_sem_vinculo <- df_sicor_teste %>% filter(duplicado == 'true' | cd_programa == '999')

