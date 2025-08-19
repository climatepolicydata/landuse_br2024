##################

# Author : Renan Morais
# Date: 02-02-2024
# Email: renanflorias@hotmail.com
# Goal: get database from server cpi"

##Modified by Julia Niemeyer
#Date: 05/25/2025

#### ---------------------------------------------------------------------- ####
####    Environment                                                         #### 
#### ---------------------------------------------------------------------- ####

# ano_ini = 2022 #the initial year to star analysis
# ano_fim = 2024 #the final year to end your analysis
# ano_base = 2024 #the year to base inflation

# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation
# #
# # # ## set the path to your github clone
github <- "Documents/"
# 
# ############## ATUALIZAR SEMPRE #############################
arquivo_sicor <- paste0("A:/finance/sicor/cleanData/sicor_main_2013_", ano_fim+1, "_empreendimento.Rds")


pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr,arrow, tictoc)

##### directory #########
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


dir_bcb <- "A:\\projects\\landuse_br2024\\sicor\\backup_data\\" 

## GET AUXILIARY DATA
## antigamente o caminho eram as pastas documentation
#dir_bcb_doc <- paste0("A:/finance/sicor/_documentation/tabelas_sicor_MDCR_", ano_fim)

## agora pegamos diretamente em auxiliary, que já mantém os dados atualizados
dir_bcb_doc <- paste0("A:\\finance\\sicor\\rawData\\auxiliary")



#### ---------------------------------------------------------------------- ####
####    Load SICOR data files                                               #### 
#### ---------------------------------------------------------------------- ####
setwd(dir_bcb)

### Load full database
df_sicor <- readRDS(arquivo_sicor) 

df_sicor <- df_sicor %>% select(-cesta,
                                -unidade_medida_previsao,
                                -cedula_mae,
                                -zoneamento,
                                -vl_area_financ,
                                -vl_juros,
                                -vl_aliq_proagro,
                                -vl_prev_prod,
                                -vl_perc_risco_fundo_const,
                                -dt_fim_plantio,
                                -vl_juros_enc_finan_posfix,
                                -dt_vencimento,
                                -vl_quantidade,
                                -vl_rec_proprio,
                                -vl_rec_proprio_srv,
                                -vl_produtiv_obtida,
                                -dt_inic_colheita,
                                -vl_perc_custo_efet_total,
                                -cd_fase_ciclo_producao,
                                -vl_prestacao_investimento,
                                -vl_receita_bruta_esperada,
                                -vl_perc_risco_stn,
                                -dt_fim_colheita,
                                -cd_contrato_stn,
                                -consorcio, -dt_inic_plantio, -cd_inst_credito,-nu_ordem, -cd_tipo_seguro,
                                -cd_tipo_encarg_financ,-data_inicio,-cd_ref_bacen_investimento,
                                -unidade_medida_previsao, -unidade_medida,-cnpj_agente_invest, -cd_cnpj_cadastrante)
#nrow = 25.636.221

#### ---------------------------------------------------------------------- ####
####    Cleaning and Creating variables                                     #### 
#### ---------------------------------------------------------------------- ####

### Create Year variable
df_sicor <- df_sicor %>% mutate(#ano = as.numeric(format(mdy(df_sicor$dt_emissao),'%Y')),
                                ano = ano_base,
                                mes = as.numeric(format(dmy(df_sicor$dt_emissao),'%m'))) %>% ##nrow(df_sicor) = 25636221
  filter(ano_base >= ano_ini & ano_base <= ano_fim) # nrow(df_sicor) = 12204945

write_parquet(df_sicor, paste0("sicor_data_", ano_ini, "-", ano_fim, ".parquet"))


### Create SAFRA Year variable
# A vigência do Plano Safra é de um ano. Ela começa em 1º de julho e vai até junho do ano seguinte, período que acompanha o calendário das safras agrícolas no Brasil.
# Convert the date column to Date format
df_sicor <- df_sicor %>%  mutate(dt_emissao = as.Date(df_sicor$dt_emissao, format = "%d/%m/%Y")) ## nrow(df_sicor) = 
  
 

### Set as numeric the variable vl_parc_credito
# df_sicor$vl_parc_credito <- as.numeric(df_sicor$vl_parc_credito)



#### ---------------------------------------------------------------------- ####
####    Inserting the codes for variables we need                           #### 
#### ---------------------------------------------------------------------- ####

#### insert codes for modalidade, produto, variedade, finalidade
# To identify the sustainability of each iniciative, We will need the following vars Tipo de cultivo/exploração, Tipo de agricultura, Irrigação, Programa, Subprograma,Integração/consórcio, Modalidade, Finalidade, Produto, Variedade
# Let´s add the code for the variables that we need and do not have the code yet in df_sicor
setwd(dir_bcb_doc)

#produto
produto <- read.csv("Produto.csv", encoding = "latin1")%>% select(X.CODIGO, DESCRICAO)%>%
  dplyr::rename(CODIGO_PRODUTO = X.CODIGO,
                PRODUTO = DESCRICAO)

#modalidade (I need to change some modalities, to be identique to sicor modalities)
modalidade <- read.csv("Modalidade.csv", sep = ",", encoding = "latin1") %>% select(CODIGO_MODALIDADE, NOME_MODALIDADE) %>%
  dplyr::rename(MODALIDADE = NOME_MODALIDADE)
modalidade$MODALIDADE <- tolower(modalidade$MODALIDADE)
modalidade <- modalidade %>% mutate(MODALIDADE=ifelse(
  MODALIDADE=="fgpp-financiamento para garantia de preços ao produtor","fgpp-financiamento para garantia de preços ao prod",MODALIDADE))
modalidade <- modalidade %>% mutate(MODALIDADE=ifelse(
  MODALIDADE=="aquisição de matéria prima direto do produtor/cooperativa","aquisição de matéria prima direto do produtor/coop",MODALIDADE))
modalidade <- modalidade %>% mutate(MODALIDADE=ifelse(
  MODALIDADE=="financiamento para aquisição da produção/materia prima - encerrado","financiamento para aquisição da produção/materia p",MODALIDADE))

# variedade
variedade <- read.csv("VariedadeProduto.csv", sep = ",", encoding = "latin1")%>% select(X.CODIGO, DESCRICAO)%>%
  dplyr::rename(CODIGO_VARIEDADE = X.CODIGO)%>%
  dplyr::rename(VARIEDADE = DESCRICAO)

# finalidade 
finalidade <- fread("Finalidade.csv", sep = ",", encoding = "Latin-1", quote = "")%>% select("#CODIGO", DESCRICAO)%>%
  dplyr::rename(CODIGO_FINALIDADE = "#CODIGO")%>%
  dplyr::rename(FINALIDADE = DESCRICAO)
finalidade$CODIGO_FINALIDADE <- gsub("\"", "", finalidade$CODIGO_FINALIDADE)
finalidade$FINALIDADE <- gsub("\"", "", finalidade$FINALIDADE)
finalidade$FINALIDADE <- trimws(finalidade$FINALIDADE)

# Distinct the dictionaries 
modalidade <- distinct(modalidade)
produto <- distinct(produto)
variedade <- distinct(variedade)
finalidade <- distinct(finalidade)

#lower case 
produto$PRODUTO <- tolower(produto$PRODUTO)
modalidade$MODALIDADE <- tolower(modalidade$MODALIDADE)
variedade$VARIEDADE <- tolower(variedade$VARIEDADE)
finalidade$FINALIDADE <- tolower(finalidade$FINALIDADE)

#upper case the variables' name
names(df_sicor) <- toupper(names(df_sicor))

#add the codes to df_sicor: 
  #nrow(df_sicor) 20767222
df_sicor <- left_join(df_sicor , modalidade, by ="MODALIDADE")
  # Are there modalidades that had not receive a code? 
  #print(any(is.na(df_sicor[is.na(MODALIDADE)==FALSE,]$CODIGO_MODALIDADE))) #every one receives a code
  
df_sicor <- left_join(df_sicor , produto, by ="PRODUTO")
  # Are there modalidades that had not receive a code? 
  #print(any(is.na(df_sicor[is.na(PRODUTO)==FALSE,]$CODIGO_PRODUTO))) #every one receives a code

df_sicor <- left_join(df_sicor , variedade, by ="VARIEDADE")
  # Are there modalidades that had not receive a code? 
  #print(any(is.na(df_sicor[is.na(VARIEDADE)==FALSE,]$CODIGO_VARIEDADE))) #every one receives a code
  
df_sicor <- left_join(df_sicor , finalidade, by ="FINALIDADE")
  # Are there modalidades that had not receive a code? 
  #print(any(is.na(df_sicor[is.na(FINALIDADE)==FALSE,]$CODIGO_FINALIDADE))) #every one receives a code

####ELIMINATE DESCRIPTIONS
df_sicor <- df_sicor %>% 
  select(-PRODUTO,- MODALIDADE, - VARIEDADE, -FINALIDADE, -PRODUTO)

###### changing values na to avoid ocorrency problems with aggregation.
  
df_sicor <- df_sicor %>% 
  mutate(ATIVIDADE = ifelse(is.na(ATIVIDADE),"NÃO INFORMADO",ATIVIDADE),
         CODIGO_VARIEDADE = ifelse(is.na(CODIGO_VARIEDADE), "0", CODIGO_VARIEDADE),
         CD_SUBPROGRAMA = ifelse(is.na(CD_SUBPROGRAMA),"0", CD_SUBPROGRAMA),
         CD_TIPO_CULTURA = ifelse(is.na(CD_TIPO_CULTURA),"0",CD_TIPO_CULTURA),
         CODIGO_FINALIDADE = ifelse(is.na(CODIGO_FINALIDADE),"0",CODIGO_FINALIDADE))

df_sicor <- mutate_all(df_sicor, ~replace_na(.,0))


####AGGREGATE BY ID
set.seed(42)
df_sicor <- df_sicor %>%
  group_by(ANO, ATIVIDADE,CODIGO_FINALIDADE, CD_PROGRAMA, CD_TIPO_CULTIVO,CODIGO_VARIEDADE,
           CD_SUBPROGRAMA, CD_FONTE_RECURSO, CD_TIPO_INTGR_CONSOR,
           CD_TIPO_AGRICULTURA, CODIGO_PRODUTO, CODIGO_MODALIDADE,
           CD_TIPO_IRRIGACAO, CNPJ_IF,CD_ESTADO ) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

df_sicor_aggregate <- aggregate(VL_PARC_CREDITO ~ ATIVIDADE+CODIGO_FINALIDADE+ CD_PROGRAMA+ CD_TIPO_CULTIVO+CODIGO_VARIEDADE+
                                CD_SUBPROGRAMA+ CD_FONTE_RECURSO+ CD_TIPO_INTGR_CONSOR+
                                CD_TIPO_AGRICULTURA+ CODIGO_PRODUTO+ CODIGO_MODALIDADE+
                                CD_TIPO_IRRIGACAO+ CNPJ_IF+ CD_ESTADO + ANO , data = df_sicor, FUN = sum)

sum(df_sicor$VL_PARC_CREDITO)
sum(df_sicor_aggregate$VL_PARC_CREDITO)


#### ---------------------------------------------------------------------- ####
####    Saving the dataframe                                                #### 
#### ---------------------------------------------------------------------- ####

dir_output <- paste0("A:/projects/landuse_br2024/sicor/output/", ano_ini, "-", ano_fim)
if(!dir.exists(dir_output)){
  dir.create(dir_output)
}
setwd(dir_output)

saveRDS(df_sicor_aggregate, paste0("df_sicor_op_basica_pre_dummie_aggregate_", ano_ini, "-", ano_fim, "V2.RDS"))

  
gc()
###### test to verify variables in aggregated
toc()



