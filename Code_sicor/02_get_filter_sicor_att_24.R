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

df_original_year <- aggregate(VL_PARC_CREDITO~ CD_SUBPROGRAMA, df_sicor, FUN = sum)

"ANO VL_PARC_CREDITO
1 2015    154147140907
2 2016    116272306995
3 2017    166926675548
4 2018    180823615845
5 2019    178579717461
6 2020    206263420191
7 2021    294074500439
8 2022    361036390269
9 2023    395303670670"

df_aggregate_year <- aggregate(VL_PARC_CREDITO~ CD_SUBPROGRAMA, df_sicor_aggregate, FUN = sum)


"ANO VL_PARC_CREDITO
1 2015    154147140907
2 2016    116272306995
3 2017    166926675548
4 2018    180823615845
5 2019    178579717461
6 2020    206263420191
7 2021    294074500439
8 2022    361036390269
9 2023    395303670670"

"
DF ORIGINAL
CD_SUBPROGRAMA VL_PARC_CREDITO
1               0    1.624687e+12
2               1    1.502547e+11
3              10    1.079410e+10
4              11    1.026379e+10
5              12    6.919187e+09
6            1234    5.706226e+08
7            1235    1.198042e+09
8            1240    4.340762e+05
9              14    4.750760e+09
10             15    3.304356e+08
11             16    3.250000e+04
12              2    8.505137e+10
13             23    4.359565e+08
14             24    5.688488e+09
15             25    1.020677e+08
16             26    5.603793e+09
17             27    1.197465e+09
18             28    7.537470e+09
19             29    8.735528e+08
20              3    2.422893e+09
21             30    5.120884e+10
22             31    6.285242e+08
23             32    6.706360e+09
24             33    3.695201e+07
25             34    6.743959e+09
26             35    1.026940e+09
27             36    8.687578e+08
28             37    2.979872e+08
29             38    1.024508e+08
30             39    2.053511e+06
31              4    4.345562e+08
32             40    1.897560e+07
33             41    5.675511e+09
34             42    1.040661e+10
35             43    1.028431e+07
36             44    3.351678e+06
37             45    7.988731e+06
38             46    1.308210e+06
39             49    1.434644e+08
40              5    1.178401e+09
41             50    1.989000e+08
42             51    1.379130e+08
43             52    2.390994e+09
44             53    8.880261e+09
45             54    1.313946e+09
46             55    4.710492e+07
47             56    2.215422e+10
48             57    5.448733e+08
49             58    1.202153e+08
50             59    2.313284e+08
51              6    2.443709e+07
52             60    2.903104e+07
53             62    1.524322e+07
54             63    1.000000e+06
55             64    2.231630e+08
56           6789    1.585457e+08
57           6790    1.628022e+08
58           6791    3.230058e+06
59              7    1.454641e+09
60             70    1.571039e+09
61             71    4.169203e+09
62              8    4.844336e+07
63             81    1.055290e+09
64             82    5.878925e+06
65             83    1.312082e+09
66             84    1.685363e+08
67             85    1.176348e+08
68             86    4.656560e+07
69             87    1.926051e+07
70             89    1.255300e+07
71              9    1.874732e+07
72             90    1.738112e+09
73           9999    8.669497e+08


DF AGREGADO
CD_SUBPROGRAMA VL_PARC_CREDITO
1               0    1.624687e+12
2               1    1.502547e+11
3              10    1.079410e+10
4              11    1.026379e+10
5              12    6.919187e+09
6            1234    5.706226e+08
7            1235    1.198042e+09
8            1240    4.340762e+05
9              14    4.750760e+09
10             15    3.304356e+08
11             16    3.250000e+04
12              2    8.505137e+10
13             23    4.359565e+08
14             24    5.688488e+09
15             25    1.020677e+08
16             26    5.603793e+09
17             27    1.197465e+09
18             28    7.537470e+09
19             29    8.735528e+08
20              3    2.422893e+09
21             30    5.120884e+10
22             31    6.285242e+08
23             32    6.706360e+09
24             33    3.695201e+07
25             34    6.743959e+09
26             35    1.026940e+09
27             36    8.687578e+08
28             37    2.979872e+08
29             38    1.024508e+08
30             39    2.053511e+06
31              4    4.345562e+08
32             40    1.897560e+07
33             41    5.675511e+09
34             42    1.040661e+10
35             43    1.028431e+07
36             44    3.351678e+06
37             45    7.988731e+06
38             46    1.308210e+06
39             49    1.434644e+08
40              5    1.178401e+09
41             50    1.989000e+08
42             51    1.379130e+08
43             52    2.390994e+09
44             53    8.880261e+09
45             54    1.313946e+09
46             55    4.710492e+07
47             56    2.215422e+10
48             57    5.448733e+08
49             58    1.202153e+08
50             59    2.313284e+08
51              6    2.443709e+07
52             60    2.903104e+07
53             62    1.524322e+07
54             63    1.000000e+06
55             64    2.231630e+08
56           6789    1.585457e+08
57           6790    1.628022e+08
58           6791    3.230058e+06
59              7    1.454641e+09
60             70    1.571039e+09
61             71    4.169203e+09
62              8    4.844336e+07
63             81    1.055290e+09
64             82    5.878925e+06
65             83    1.312082e+09
66             84    1.685363e+08
67             85    1.176348e+08
68             86    4.656560e+07
69             87    1.926051e+07
70             89    1.255300e+07
71              9    1.874732e+07
72             90    1.738112e+09
73           9999    8.669497e+08
"



