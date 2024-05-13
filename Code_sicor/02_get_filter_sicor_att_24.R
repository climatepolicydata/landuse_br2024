##################

# Author : Renan Morais
# Date: 02-02-2024
# Email: renanflorias@hotmail.com
# Goal: get database from server cpi"

#### ---------------------------------------------------------------------- ####
####    Environment                                                         #### 
#### ---------------------------------------------------------------------- ####

pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

##### directory #########
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_bcb <- ("A:/finance/sicor/cleanData")

dir_bcb_doc <- ("A:/finance/sicor/_documentation/tabelas_sicor_MDCR_2021")

#### ---------------------------------------------------------------------- ####
####    Load SICOR data files                                               #### 
#### ---------------------------------------------------------------------- ####
setwd(dir_bcb)

### Load full database
df_sicor <- readRDS("sicor_main_2013_2023_with_empreendimento.Rds")

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
                                -consorcio, -dt_inic_plantio, -cd_inst_credito,
                                -ref_bacen,-nu_ordem,-ano_base,-cd_tipo_seguro,
                                -cd_tipo_encarg_financ,-data_inicio,-cd_ref_bacen_investimento,
                                -unidade_medida_previsao, -unidade_medida,-cnpj_agente_invest, -cd_cnpj_cadastrante)
#### ---------------------------------------------------------------------- ####
####    Cleaning and Creating variables                                     #### 
#### ---------------------------------------------------------------------- ####

### Create Year variable
df_sicor <- df_sicor %>% mutate(ano = as.numeric(format(mdy(df_sicor$dt_emissao),'%Y')),
                                mes = as.numeric(format(mdy(df_sicor$dt_emissao),'%m'))) %>% 
  filter(ano >= 2015 & ano < 2024)


### Create SAFRA Year variable
# A vigência do Plano Safra é de um ano. Ela começa em 1º de julho e vai até junho do ano seguinte, período que acompanha o calendário das safras agrícolas no Brasil.
# Convert the date column to Date format
df_sicor <- df_sicor %>%  mutate(dt_emissao = as.Date(df_sicor$dt_emissao, format = "%m/%d/%Y"))
  
 

### Set as numeric the variable vl_parc_credito
df_sicor$vl_parc_credito <- as.numeric(df_sicor$vl_parc_credito)



#### ---------------------------------------------------------------------- ####
####    Inserting the codes for variables we need                           #### 
#### ---------------------------------------------------------------------- ####

#### insert codes for modalidade, produto, variedade, finalidade
# To identify the sustainability of each iniciative, We will need the following vars Tipo de cultivo/exploração, Tipo de agricultura, Irrigação, Programa, Subprograma,Integração/consórcio, Modalidade, Finalidade, Produto, Variedade
# Let´s add the code for the variables that we need and do not have the code yet in df_sicor
setwd(dir_bcb_doc)

#produto
produto <- read.csv("Produto.csv", sep = ";", encoding = "latin1")%>% select(X.CODIGO, PRODUTO)%>%
  dplyr::rename(CODIGO_PRODUTO = X.CODIGO)

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

setwd("A:/projects/landuse_br2024/sicor/output")

saveRDS(df_sicor_aggregate, "df_sicor_op_basica_pre_dummie_aggregate.RDS")

  
  
###### test to verify variables in aggregated




