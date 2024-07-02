pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr,pivottabler,reshape2)

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_bcb<- ("A:/finance/sicor/cleanData")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR")

dir_bcb_clear <- ("A:/finance/sicor/cleanData")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR_2021")

### import dataset ############

setwd(dir_sicor_output)


df_sicors <- readRDS("df_sicor_op_basica_pre_dummie_aggregate.RDS")

setwd(dir_bcb_doc)

finalidade <- fread("Finalidade.csv", sep = ",", encoding = "Latin-1", quote = "")%>% select("#CODIGO", DESCRICAO)%>%
  dplyr::rename(CODIGO_FINALIDADE = "#CODIGO")%>%
  dplyr::rename(FINALIDADE = DESCRICAO)
finalidade$CODIGO_FINALIDADE <- gsub("\"", "", finalidade$CODIGO_FINALIDADE)
finalidade$FINALIDADE <- gsub("\"", "", finalidade$FINALIDADE)
finalidade$FINALIDADE <- trimws(finalidade$FINALIDADE)

df_sicor <- left_join(df_sicor , finalidade, by ="CODIGO_FINALIDADE")


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

# save pivot

wb <- createWorkbook()

# Adicionar uma planilha ao workbook
addWorksheet(wb, "PivotTable")

# Renderizar a tabela dinâmica em um data frame
pivot_df <- as.data.frame(pt$asMatrix())

# Escrever a tabela dinâmica na planilha
writeData(wb, sheet = "PivotTable", pivot_df, rowNames = TRUE)

# Salvar o workbook em um arquivo Excel
saveWorkbook(wb, "PivotTablereviewed.xlsx", overwrite = TRUE)



df_sicor_teste <- aggregate(vl_parc_credito ~ ref_bacen + cd_programa, data=df_sicor , FUN = "sum")

df_sicor_teste <- df_sicor_teste %>% mutate(duplicado = ifelse(duplicated(ref_bacen), "true", "false"))

teste2 <- df_sicor_teste$ref_bacen %>% duplicated %>% count()
1 FALSE 18142195
2  TRUE     1529

df_dupli_sem_vinculo <- df_sicor_teste %>% filter(duplicado == 'true' | cd_programa == '999')


##### export table with dummies to xlsx

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_sicor_doc <- ("A:/finance/sicor/rawData/auxiliary")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")


################ import databases #############
setwd(dir_sicor_output)


# Read main dataset
df <-readRDS("sicor_op_basica_sum_dummies_aggregate_v2.RDS") %>% 
  dplyr::rename(year = ANO, value_original_currency = VL_PARC_CREDITO) %>% dplyr::mutate(original_currency = "BRL")


# Read main dataset

github <- "Documents"
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

# github <- readline("digite a pasta do seu repositório clone: ")

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
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


df_sicor_calculus <- deflate_and_exchange(tabela_deflator, df, tabela_cambio)

write.xlsx(df_sicor_calculus,"demonstration_by_dummies_deflated.xlsx")

#################################################



##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_sicor_doc <- ("A:/finance/sicor/rawData/auxiliary")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")


################ import databases #############
setwd(dir_sicor_output)


# Read main dataset
df <-readRDS("sicor_op_basica_sum_dummies_aggregate_v2.RDS")

#### render pivot #########

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(df)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("CODIGO_PRODUTO")
pt$addRowDataGroups("ANO")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="Total Parc Credito", 
                     summariseExpression="sum(VL_PARC_CREDITO)")

# Renderizar a tabela
pt$renderPivot()
