##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transform database for landscape
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
               pivottabler)

##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_sicor_doc <- ("A:/finance/sicor/rawData/auxiliary")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_sicor_output <- ("A:/projects/landuse_br2024/sicor/output")


################ import databases #############
setwd(dir_sicor_output)


# Read main dataset
df <-readRDS("sicor_op_basica_sum_dummies_aggregate.RDS")

# Read description tables
setwd(dir_sicor_doc)
tb_irrigacao <- read.csv("TipoIrrigacao.csv", sep = "," ,encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_IRRIGACAO = X.CODIGO,
                DESC_IRRIGACAO = DESCRICAO)
tb_agricultura <- read.csv("TipoAgropecuaria.csv", sep = "," ,encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_AGRICULTURA = X.CODIGO,
                DESC_AGRICULTURA = DESCRICAO)
tb_subprograma <- read.csv("Subprogramas.csv", sep = ";", encoding = "latin1") %>% 
  dplyr::rename(CD_SUBPROGRAMA = X.CODIGO_SUBPROGRAMA)
  
tb_cat_emitente <- read.csv("CategoriaEmitente.csv", sep = ",", encoding = "latin1") %>% 
  dplyr::rename(CD_CATEG_EMITENTE = X.CODIGO)

tb_intgr <- read.csv("TipoIntegracao.csv", sep = ",", encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_INTGR_CONSOR = X.CODIGO,
                DESCRICAO_INTGR = DESCRICAO)

tb_modalidade <- read.csv("Modalidade.csv", sep = "," ,encoding = "latin1") %>% 
  select(CODIGO_MODALIDADE, NOME_MODALIDADE)%>% 
  dplyr::rename(MODALIDADE = NOME_MODALIDADE) %>% distinct()

tb_finalidade <- read.csv("Modalidade.csv", sep = "," ,encoding = "latin1") %>% 
  select(X.CODIGO_FINALIDADE, NOME_FINALIDADE)%>% 
  dplyr::rename(FINALIDADE = NOME_FINALIDADE,
                CODIGO_FINALIDADE =X.CODIGO_FINALIDADE ) %>% distinct()

tb_produto <- read.csv("Produto.csv", sep = "," ,encoding = "latin1") %>% 
  select(X.CODIGO, DESCRICAO)%>% 
  dplyr::rename(PRODUTO = DESCRICAO,
                CODIGO_PRODUTO = X.CODIGO) %>% distinct()


df_if <- read.csv("SICOR_LISTA_IFS.csv", sep = ";", encoding = "latin1") %>% 
  dplyr::rename(CNPJ_IF = X.CNPJ_IF)
tb_cultivo <- read.csv("TipoCultivo.csv", sep = ",", encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_CULTIVO = X.CODIGO,
                DESCRICAO_TP_CULTIVO = DESCRICAO)


# Join description tables with main dataset
df <- join(df , tb_irrigacao, by = "CD_TIPO_IRRIGACAO")
df <- join(df , tb_agricultura, by = "CD_TIPO_AGRICULTURA")
df <- join(df, tb_subprograma, by ="CD_SUBPROGRAMA")
df <- join(df, tb_cultivo, by="CD_TIPO_CULTIVO")
#joins ok

rm(tb_irrigacao,tb_agricultura,tb_subprograma,tb_cultivo)

# Read relational tables
setwd(dir_sicor_landuse2024)
tabela_instrument <- read.xlsx("01_sicor_relational_tables.xlsx", sheet = "instrument_landscape")
tabela_recipient <- read.xlsx("01_sicor_relational_tables.xlsx", sheet = "beneficiary_landscape") 
tabela_fonte_recurso <- read.xlsx("01_sicor_relational_tables.xlsx", sheet = "source_landscape")
tabela_climate_use <- read.xlsx("01_sicor_relational_tables.xlsx", sheet = "climate_use_bcb_82") %>% 
  dplyr::rename(DESC_IRRIGACAO = IRRIGACAO,
                DESCRICAO_SUBPROGRAMA = SUBPROGRAMA,
                DESC_AGRICULTURA = TP_AGRICULTURA,
                DESCRICAO_INTGR = TP_INTGR_CONSOR)

# Import description and relational tables to transform codes into their respective descriptions
df <- join(df, tabela_instrument, by= "CD_PROGRAMA") #ok
df <- join(df, tabela_recipient, by = "CD_PROGRAMA") #ok
df <- join(df, tabela_fonte_recurso, by = "CD_FONTE_RECURSO")
df <- join(df, tb_intgr, by = "CD_TIPO_INTGR_CONSOR")

df <- join(df, tb_modalidade, by = "CODIGO_MODALIDADE")
df <- join(df, tb_finalidade, by = "CODIGO_FINALIDADE")

df <- join(df, tb_produto, by = "CODIGO_PRODUTO")

#ok

########### Creation of IDs and transformations #################

df <- df %>% 
  mutate(project_name = paste(FINALIDADE, DESCRICAO_SUBPROGRAMA, sep = "_"),
    project_description = paste(MODALIDADE, PRODUTO, DESC_AGRICULTURA, DESC_IRRIGACAO, DESCRICAO_TP_CULTIVO, sep = "_"))


#select 

tabela_climate_modalidade <- tabela_climate_use %>% select(CODIGO_MODALIDADE,USE_MODALIDADE)
tabela_climate_irrigacao <- tabela_climate_use %>% select(CD_IRRIGACAO,USE_IRRIGACAO)
tabela_climate_produto <- tabela_climate_use %>% select(CODIGO_PRODUTO,USE_PRODUTO)
tabela_climate_programa <- tabela_climate_use %>% select(CD_PROGRAMA_ABC,USE_PROGRAMA_ABC)
tabela_climate_subprograma<- tabela_climate_use %>% select(CD_SUBPROGRAMA,USE_SUBPROGRAMA)
tabela_climate_cultivo <- tabela_climate_use %>% select(CD_TP_CULTIVO,USE_CULTIVO)
tabela_climate_intgr <- tabela_climate_use %>% select(CD_TP_INTGR_CONSOR,USE_INTEGR)
tabela_climate_agricultra <- tabela_climate_use %>% select(CD_TP_AGRICULTURA,USE_AGRICULTURA)
tabela_climate_pronaf_abc <- tabela_climate_use %>% select(CD_SUBPROGRAMA_PRONAF_ABC,USE_SUBPROGRAMA_PRONAF)


# df3 <- left_join(df2, tabela_climate_modalidade, by = "MODALIDADE")
# df4 <- left_join(df3, tabela_climate_irrigacao, by = "DESC_IRRIGACAO")
# df5 <- left_join(df4, tabela_climate_produto, by = "PRODUTO")
# df6 <- left_join(df5, tabela_climate_programa, by = "instrument_original")
# df7 <- left_join(df6, tabela_climate_subprograma, by = "DESCRICAO_SUBPROGRAMA")
# df8 <- left_join(df7, tabela_climate_cultivo, by = "DESCRICAO_TP_CULTIVO")
# df9 <- left_join(df8, tabela_climate_intgr, by = "DESCRICAO_INTGR")
# df <- left_join(df9, tabela_climate_agricultra, by = "DESC_AGRICULTURA")

df2 <- df %>%
  left_join(tabela_climate_modalidade, by = "CODIGO_MODALIDADE") %>%
  left_join(tabela_climate_irrigacao, by = "CD_TP_INTGR_CONSOR") %>%
  left_join(tabela_climate_produto, by = "") %>%
  left_join(tabela_climate_programa, by = "instrument_original") %>%
  left_join(tabela_climate_subprograma, by = "DESCRICAO_SUBPROGRAMA") %>%
  left_join(tabela_climate_cultivo, by = "DESCRICAO_TP_CULTIVO") %>%
  left_join(tabela_climate_intgr, by = "DESCRICAO_INTGR") %>%
  left_join(tabela_climate_agricultra, by = "DESC_AGRICULTURA")

sum(df$VL_PARC_CREDITO)


df <- df %>% 
         mutate(DUMMY_ADAPTATION = case_when(USE_IRRIGACAO == "adaptation" | USE_MODALIDADE == "adaptation"  |USE_PRODUTO == "adaptation"  
                                             | USE_PROGRAMA == "adaptation"  | USE_SUBPROGRAMA == "adaptation"  | USE_CULTIVO == "adaptation"
                                             | USE_INTEGR== "adaptation"  | USE_AGRICULTURA== "adaptation"  ~ 1, .default = 0),
                DUMMY_MITIGATION = case_when(USE_IRRIGACAO == "mitigation" | USE_MODALIDADE == "mitigation" |USE_PRODUTO == "mitigation" 
                                             | USE_PROGRAMA == "mitigation" | USE_SUBPROGRAMA == "mitigation" | USE_CULTIVO == "mitigation" 
                                             | USE_INTEGR== "mitigation" | USE_AGRICULTURA== "mitigation" ~ 1, .default = 0),
                DUMMY_DUAL = case_when(USE_IRRIGACAO == "dual"| USE_MODALIDADE == "dual" |USE_PRODUTO == "dual" 
                                             | USE_PROGRAMA == "dual" | USE_SUBPROGRAMA == "dual" | USE_CULTIVO == "dual" 
                                             | USE_INTEGR== "dual" | USE_AGRICULTURA== "dual" ~ 1, .default = 0))



df <- df %>% 
  mutate(climate_use = ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 0, "adaptation",
                          ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 0 , "mitigation",
                              ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 1 , "dual",
                                     ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 0, "dual",
                                            ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 1, "dual", 
                                                   ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 1,"dual",
                                                   ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 1, "dual","none"))))))))


rm(df)


# Import description and relational tables to transform codes into their respective descriptions
df_ajust <- join(df_ajust, tabela_instrument, by= "CD_PROGRAMA")
df_ajust <- join(df_ajust, tabela_recipient, by = "CD_PROGRAMA")
df_ajust <- join(df_ajust, tabela_fonte_recurso, by = "CD_FONTE_RECURSO")

rm(tabela_instrument,tabela_recipient,tabela_fonte_recurso)

df_ajust <- df_ajust %>% 
  mutate(
    data_source = "SICOR_BCB",
    channel_original = "Financial Institutions",
    ecossystem_layer = "Política de Credito Agropecuário"
  )


df_ajust <- join(df_ajust, tb_cat_emitente, by ="CD_CATEG_EMITENTE")
df_ajust <- join(df_ajust, tb_intgr, by = "CD_TIPO_INTGR_CONSOR")
df_ajust <- join(df_ajust, df_if , by ="CNPJ_IF")




mdcr_op_basica_if <- df_ajust %>% 
  mutate(NOME_IF = if_else(NOME_IF %in% c("BCO DO BRASIL S.A.", "BCO DO NORDESTE DO BRASIL S.A.","BCO BRADESCO S.A.","BCO SANTANDER (BRASIL) S.A.",
                        "CC CREDICITRUS","ITAÚ UNIBANCO S.A.","SICOOB COCRED CC,BCO DA AMAZONIA S.A.",
                        "CAIXA ECONOMICA FEDERAL","BNDES,BCO COOPERATIVO SICREDI S.A.","BCO DO ESTADO DO RS S.A.",
                        "COOPECREDI GUARIBA - CC","BCO BOCOM BBM S.A.","BCO RABOBANK INTL BRASIL S.A.",
                        "BCO ABC BRASIL S.A.","BCO SAFRA S.A.","CC SICOOB CREDICOONAI","COOP SICREDI UNIÃO RS",
                        "BANCO BTG PACTUAL S.A.", "CC COCRE", "BD REGIONAL DO EXTREMO SUL", "BANCO SICOOB S.A.",
                        "COOP SICREDI CAMPOS GERAIS", "COOP SICREDI PLANALTO RS/MG"), NOME_IF, "Outros"))

mdcr_op_basica_if <- mdcr_op_basica_if %>% 
  mutate(channel_original = paste(SEGMENTO_IF,NOME_IF, sep = "_")) %>% 
  dplyr::rename(subsector_original = DESCRICAO_INTGR)

mdcr_op_basica_if_sort <- mdcr_op_basica_if %>% 
  dplyr::rename(id_original = id_equals) %>% 
  dplyr::rename(source_original = source_of.finance_original) %>%
  dplyr::rename(origin_domestic_international = domestic_International,
                year = ANO,
                value_brl = VL_PARC_CREDITO,
                sector_original = ATIVIDADE,
                climate_component = climate_use) %>% 
  mutate(channel_landscape = "Instituições Financeiras",
         activity_landscape = "-",
         region = "-",
         uf = "-",
         municipality = "-",
         beneficiary_landscape = "Produtores Rurais",
         climate_component = "-",
         subactivity_landscape = "-",
         rio_marker = "-",
         beneficiary_public_private= "-",
         beneficiary_original = "-",
         localization_original = "-",
         subactivity_landscape = "-") %>% 
  mutate(sector_landscape = if_else(sector_original %in% "agrícola", "Agricultura","Pecuária")) %>% 
  mutate(sector_landscape = if_else(str_detect(project_name, "Florestas"),"Conservação, Restauração e Reflorestamento",
                                    sector_landscape)) %>%
  mutate(sector_landscape = if_else(str_detect(project_description, "extrativismo|florestamento|eucalipto|pinus|madeira|seringueira"),
                                    "Conservação, Restauração e Reflorestamento",
                                    sector_landscape))
  
  
mdcr_op_basica_sort <- mdcr_op_basica_if_sort  %>%  
  select(id_original, data_source, year, project_name,
         project_description, source_original, source_finance_landscape,
         origin_domestic_international, origin_private_public,
         value_brl,channel_original, channel_landscape, instrument_original,
         instrument_landscape, sector_original, sector_landscape,
         subsector_original,activity_landscape,
         region, uf,municipality, beneficiary_landscape,
         ecossystem_layer,
         climate_component, beneficiary_original,localization_original,
         subactivity_landscape, rio_marker,
         beneficiary_public_private)

df_final <- mdcr_op_basica_sort %>% 
  mutate(project_description = gsub(";"," ", mdcr_op_basica_sort$project_description)) %>%
  mutate(source_finance_landscape = if_else(source_original 
                                               %in% c("LETRA DE CRÉDITO DO AGRONEGÓCIO (LCA) - TAXA FAVORECIDA",
                                                      "LETRA DE CRÉDITO DO AGRONEGÓCIO (LCA) - TAXA LIVRE"),"Outros", 
                                               if_else(source_original %in%
                                                         c("FUNCAFE-FUNDO DE DEFESA DA ECONOMIA CAFEEIRA"), 
                                                       "Governos", source_finance_landscape))) %>% 
  mutate(origin_private_public = if_else(source_original %in%
                                          c("FUNCAFE-FUNDO DE DEFESA DA ECONOMIA CAFEEIRA"), 
                                        "Pública", origin_private_public))

rm(mdcr_op_basica_sort)

df_final <- df_final %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  mutate(original_currency = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)

######## apply deflate and create usd value ##########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

# github <- readline("digite a pasta do seu repositório clone: ")

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/Funcao_taxa_cambio_v2.r"))

ano_ini = 2015
ano_fim = 2020

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  filter(year >= 2015 & year <= 2020)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>%  
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
           value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}


df_sicor_calculus <- deflate_and_exchange(tabela_deflator, df_final, tabela_cambio)

rm(cambio_sgs,df_final, ibge_ipca, tabela_cambio, tabela_deflator, teste)

df_sicor_calculus <- df_sicor_calculus %>% 
  select(id_original, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public,
         value_original_currency, original_currency, value_brl_deflated, value_usd, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf, municipality)

setwd("A:/projects/brlanduse_landscape102023/sicor/output")

saveRDS(df_sicor_calculus,"df_sicor_format_landscape_final_att.rds")




##################### tabela agregada para base final ##############

# base_agregado <- mdcr_op_basica_sort %>% 
#   mutate(finalidade_agreg = gsub("_.*$","",project_name))
# 
# pt <- PivotTable$new()
# 
# pt$addData(base_agregado)
# # ptif$addColumnDataGroups("SEGMENTO_IF")
# pt$addColumnDataGroups("finalidade_agreg")
# pt$addRowDataGroups("year", totalCaption = "Total")
# pt$defineCalculation(calculationName = "TOTAL_VALUE", summariseExpression = "sum(value_brl)",format= "%.2f")
# pt$renderPivot()
# 
# 
# setwd(dir_bcb_output)
# wd <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wd, "ag_if")
# 
# writeData(wd,"my sheet 1",pt$writeToExcelWorksheet(wb=wd, wsName = "ag_if", 
#                                                      topRowNumber=1, leftMostColumnNumber=1, 
#                                                      applyStyles=TRUE, mapStylesFromCSS=TRUE))
# 
# 
# saveWorkbook(wd, file="agregado_finalidade_sicor_landscape.xlsx", overwrite = TRUE)
################# salvando protótipo da base ###########



######################## subprograma por codigo ###################

# df_ajust <- df %>% 
#   mutate(finalidade_agreg = gsub(".*_","",project_name))
# 
# pt <- PivotTable$new()
# 
# pt$addData(df_ajust)
# # ptif$addColumnDataGroups("SEGMENTO_IF")
# # pt$addColumnDataGroups("instrument_original")
# # pt$addRowDataGroups("project_name")
# pt$addRowDataGroups("instrument_original")
# pt$addRowDataGroups("finalidade_agreg")
# pt$defineCalculation(calculationName = "TOTAL_VALUE", summariseExpression = "sum(value_brl)",format= "%.2f")
# # pt$defineCalculation(calculationName = "TOTAL", summariseExpression = "n()",)
# pt$renderPivot()
# 
# 
# setwd(dir_bcb_output)
# wd <- createWorkbook(creator = Sys.getenv("USERNAME"))
# addWorksheet(wd, "ag_if")
# 
# writeData(wd,"my sheet 1",pt$writeToExcelWorksheet(wb=wd, wsName = "ag_if", 
#                                                    topRowNumber=1, leftMostColumnNumber=1, 
#                                                    applyStyles=TRUE, mapStylesFromCSS=TRUE))
# 
# 
# saveWorkbook(wd, file="contidos_programa_sub_programa.xlsx", overwrite = TRUE)
# 
# df_dual_dummy <- df %>% 