##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transform database for landscape
# resource: 

### Modified by Julia Niemeyer
# Date 25/05/2025

## set anos de analise caso não esteja rodando pelo master

ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

# ## set the path to your github clone
github <- "Documents"


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
               pivottabler,
               tictoc)


##### directory #########

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

dir_sicor_doc <- ("A:/finance/sicor/rawData/auxiliary")

dir_sicor_landuse2024 <- ("A:/projects/landuse_br2024/sicor")

dir_output <- paste0("A:/projects/landuse_br2024/sicor/output/", ano_ini, "-", ano_fim)


########### import databases #########
# import landuse br database to get columns names and order 
Landscape_columns <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 


# Read main dataset
df <-readRDS(paste0(dir_output, "/sicor_op_basica_sum_dummies_aggregate_v2_", ano_ini, "-", ano_fim, ".RDS"))

df_sicor_op_basica_empreendimento_all_dummies <- readRDS(paste0(dir_output, "/df_sicor_op_basica_all_dummies_aggregate_v2_", ano_ini, "-", ano_fim, ".RDS"))                



# Read description tables
setwd("A:\\projects\\landuse_br2024\\sicor\\auxiliary")
tb_irrigacao <- read.csv("TipoIrrigacao.csv", sep = "," ,encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_IRRIGACAO = X.CODIGO,
                DESC_IRRIGACAO = DESCRICAO)
tb_agricultura <- read.csv("TipoAgropecuaria.csv", sep = "," ,encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_AGRICULTURA = X.CODIGO,
                DESC_AGRICULTURA = DESCRICAO)
tb_subprograma <- read.csv("Subprogramas.csv", sep = ";", encoding = "latin1") %>% 
  dplyr::rename(CD_SUBPROGRAMA = X.CODIGO_SUBPROGRAMA) %>% 
  select(CD_SUBPROGRAMA, DESCRICAO_SUBPROGRAMA)
  
tb_cat_emitente <- read.csv("CategoriaEmitente.csv", sep = ",", encoding = "latin1") %>% 
  dplyr::rename(CD_CATEG_EMITENTE = X.CODIGO) %>% 
  select()

tb_intgr <- read.csv("TipoIntegracao.csv", sep = ",", encoding = "latin1") %>% 
  dplyr::rename(CD_TIPO_INTGR_CONSOR = X.CODIGO,
                DESCRICAO_INTGR = DESCRICAO)

tb_modalidade <- read.csv("Modalidade.csv", sep = "," ,encoding = "latin1") %>% 
  select(CODIGO_MODALIDADE, NOME_MODALIDADE)%>% 
  dplyr::rename(MODALIDADE = NOME_MODALIDADE) %>% distinct()

tb_finalidade <- read.csv("Modalidade.csv", sep = "," ,encoding = "latin1") %>% 
  select(X.CODIGO_FINALIDADE, NOME_FINALIDADE)%>% 
  dplyr::rename(FINALIDADE = NOME_FINALIDADE,
                CODIGO_FINALIDADE =X.CODIGO_FINALIDADE ) %>% distinct() %>% 
  dplyr::mutate(CODIGO_FINALIDADE = as.integer(CODIGO_FINALIDADE))

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

df <- df %>% 
  dplyr::mutate(DESCRICAO_SUBPROGRAMA = ifelse(is.na(DESCRICAO_SUBPROGRAMA),"Nao informado",DESCRICAO_SUBPROGRAMA),
         CODIGO_MODALIDADE = as.integer(CODIGO_MODALIDADE))
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


#select interesting variables to classify climate component

tabela_climate_modalidade <- tabela_climate_use %>% select(CODIGO_MODALIDADE,USE_MODALIDADE) %>%  
  dplyr::filter(CODIGO_MODALIDADE != "NULL") %>%  unique() 

tabela_climate_irrigacao <- tabela_climate_use %>% select(CD_TIPO_IRRIGACAO,USE_IRRIGACAO) %>% 
  filter(CD_TIPO_IRRIGACAO != "NULL") %>% 
  mutate(CD_TIPO_IRRIGACAO = as.numeric(CD_TIPO_IRRIGACAO))

tabela_climate_produto <- tabela_climate_use %>% select(CODIGO_PRODUTO,USE_PRODUTO) %>% 
  filter(CODIGO_PRODUTO != "NULL") %>% 
  mutate(CODIGO_PRODUTO = as.numeric(CODIGO_PRODUTO))

tabela_climate_produto2 <- tabela_climate_use %>% select(CODIGO_PRODUTO_2
,USE_PRODUTO_2) %>% 
  filter(CODIGO_PRODUTO_2
 != "NULL") %>% 
  mutate(CODIGO_PRODUTO_2
 = as.numeric(CODIGO_PRODUTO_2
)) %>% 
  dplyr::rename(CODIGO_PRODUTO = CODIGO_PRODUTO_2
)

tabela_climate_programa <- tabela_climate_use %>% select(CD_PROGRAMA,USE_PROGRAMA_ABC) %>% 
  filter(CD_PROGRAMA != "NULL") %>% 
  mutate(CD_PROGRAMA = as.numeric(CD_PROGRAMA))

tabela_climate_subprograma<- tabela_climate_use %>% select(CD_SUBPROGRAMA,USE_SUBPROGRAMA) %>% 
  filter(CD_SUBPROGRAMA != "NULL") %>% 
  mutate(CD_SUBPROGRAMA = as.numeric(CD_SUBPROGRAMA))

tabela_climate_subprograma_abc<- tabela_climate_use %>% select(CD_SUBPROGRAMA_PRONAF_ABC,USE_SUBPROGRAMA_PRONAF) %>% 
dplyr::rename(CD_SUBPROGRAMA = CD_SUBPROGRAMA_PRONAF_ABC) %>% 
  filter(CD_SUBPROGRAMA != "NULL") %>% 
  mutate(CD_SUBPROGRAMA = as.numeric(CD_SUBPROGRAMA))

tabela_climate_cultivo <- tabela_climate_use %>% select(CD_TIPO_CULTIVO,USE_CULTIVO)%>% 
  filter(CD_TIPO_CULTIVO != "NULL") %>% 
  mutate(CD_TIPO_CULTIVO = as.numeric(CD_TIPO_CULTIVO))

tabela_climate_intgr <- tabela_climate_use %>% select(CD_TIPO_INTGR_CONSOR,USE_INTEGR) %>% 
  filter(CD_TIPO_INTGR_CONSOR != "NULL") %>% 
  mutate(CD_TIPO_INTGR_CONSOR = as.numeric(CD_TIPO_INTGR_CONSOR))

tabela_climate_agricultra <- tabela_climate_use %>% select(CD_TIPO_AGRICULTURA,USE_AGRICULTURA) %>% 
  filter(CD_TIPO_AGRICULTURA != "NULL") %>% 
  mutate(CD_TIPO_AGRICULTURA = as.numeric(CD_TIPO_AGRICULTURA))

tabela_climate_variedade <- tabela_climate_use %>% select(CODIGO_VARIEDADE,USE_VARIEDADE) %>% 
  filter(CODIGO_VARIEDADE != "NULL")

## se certifica que de os tipos de dado serão iguais, permitindo left_join
class(tabela_climate_variedade$CODIGO_VARIEDADE) <- class(df$CODIGO_VARIEDADE)[1] 


#adjust to join with table climate
df <- df %>% 
  mutate(CD_SUBPROGRAMA = as.numeric(CD_SUBPROGRAMA))

df <- df %>%
  left_join(tabela_climate_modalidade, by = "CODIGO_MODALIDADE") %>%
  left_join(tabela_climate_programa, by = "CD_PROGRAMA") %>%
  left_join(tabela_climate_irrigacao, by = "CD_TIPO_IRRIGACAO") %>%
  left_join(tabela_climate_produto, by = "CODIGO_PRODUTO") %>%
  left_join(tabela_climate_produto2, by = "CODIGO_PRODUTO") %>% 
  left_join(tabela_climate_subprograma, by = "CD_SUBPROGRAMA") %>%
  left_join(tabela_climate_cultivo, by = "CD_TIPO_CULTIVO") %>%
  left_join(tabela_climate_intgr, by = "CD_TIPO_INTGR_CONSOR") %>%
  left_join(tabela_climate_agricultra, by = "CD_TIPO_AGRICULTURA") %>% 
  left_join(tabela_climate_subprograma_abc, by = "CD_SUBPROGRAMA") %>% 
  left_join(tabela_climate_variedade, by = "CODIGO_VARIEDADE")

sum(df$VL_PARC_CREDITO)


df <- df %>% 
         mutate(DUMMY_ADAPTATION = case_when(USE_IRRIGACAO == "Adaptation" | USE_MODALIDADE == "Adaptation"  |USE_PRODUTO == "Adaptation"  
                                             | USE_PROGRAMA_ABC == "Adaptation"  | USE_SUBPROGRAMA == "Adaptation"  | USE_CULTIVO == "Adaptation"
                                             | USE_INTEGR== "Adaptation"  | USE_AGRICULTURA== "Adaptation" | 
                                               USE_PRODUTO_2 == "Adaptation" | USE_SUBPROGRAMA_PRONAF == "Adaptation" | USE_VARIEDADE == "Adaptation" ~ 1, .default = 0),
                DUMMY_MITIGATION = case_when(USE_IRRIGACAO == "Mitigation" | USE_MODALIDADE == "Mitigation" | USE_PRODUTO == "Mitigation" 
                                             | USE_PROGRAMA_ABC == "Mitigation" | USE_SUBPROGRAMA == "Mitigation" | USE_CULTIVO == "Mitigation" 
                                             | USE_INTEGR== "Mitigation" | USE_AGRICULTURA== "Mitigation" | 
                                               USE_PRODUTO_2 == "Mitigation" | USE_SUBPROGRAMA_PRONAF == "Mitigation" | USE_VARIEDADE == "Mitigation"  ~ 1, .default = 0),
                DUMMY_DUAL = case_when(USE_IRRIGACAO == "Dual"| USE_MODALIDADE == "Dual" |USE_PRODUTO == "Dual" 
                                             | USE_PROGRAMA_ABC == "Dual" | USE_SUBPROGRAMA == "Dual" | USE_CULTIVO == "Dual" 
                                             | USE_INTEGR== "Dual" | USE_AGRICULTURA== "Dual" | 
                                              USE_PRODUTO_2 == "Dual" | USE_SUBPROGRAMA_PRONAF == "Dual" | USE_VARIEDADE =="Dual"  ~ 1, .default = 0))



df <- df %>% 
  mutate(climate_use = ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 0, "Adaptation",
                          ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 0 , "Mitigation",
                              ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 1 , "Dual",
                                     ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 0, "Dual",
                                            ifelse(DUMMY_ADAPTATION == 0 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 1, "Dual", 
                                                   ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 0 & DUMMY_DUAL == 1,"Dual",
                                                   ifelse(DUMMY_ADAPTATION == 1 & DUMMY_MITIGATION == 1 & DUMMY_DUAL == 1, "Dual","none"))))))))




# Import description and relational tables to transform codes into their respective descriptions
# df_ajust <- join(df, tabela_instrument, by= "CD_PROGRAMA")
# df_ajust <- join(df_ajust, tabela_recipient, by = "CD_PROGRAMA")
# df_ajust <- join(df, tabela_fonte_recurso, by = "CD_FONTE_RECURSO")

rm(tabela_instrument,tabela_recipient,tabela_fonte_recurso)

df <- df %>% 
  mutate(
    data_source = "SICOR_BCB",
    channel_original = "Financial Institution",
    ecossystem_layer = "Política de Credito Agropecuário"
  )


# df_ajust <- join(df, tb_cat_emitente, by ="CD_CATEG_EMITENTE")
# df_ajust <- join(df, tb_intgr, by = "CD_TIPO_INTGR_CONSOR")
df_ajust <- join(df, df_if , by ="CNPJ_IF")




mdcr_op_basica_if <- df_ajust %>% 
  mutate(NOME_IF = if_else(NOME_IF %in% c("BCO DO BRASIL S.A.", "BCO DO NORDESTE DO BRASIL S.A.","BCO BRADESCO S.A.","BCO SANTANDER (BRASIL) S.A.",
                        "CC CREDICITRUS","ITAÚ UNIBANCO S.A.","SICOOB COCRED CC,BCO DA AMAZONIA S.A.",
                        "CAIXA ECONOMICA FEDERAL","BNDES,BCO COOPERATIVO SICREDI S.A.","BCO DO ESTADO DO RS S.A.",
                        "COOPECREDI GUARIBA - CC","BCO BOCOM BBM S.A.","BCO RABOBANK INTL BRASIL S.A.",
                        "BCO ABC BRASIL S.A.","BCO SAFRA S.A.","CC SICOOB CREDICOONAI","COOP SICREDI UNIÃO RS",
                        "BANCO BTG PACTUAL S.A.", "CC COCRE", "BD REGIONAL DO EXTREMO SUL", "BANCO SICOOB S.A.",
                        "COOP SICREDI CAMPOS GERAIS", "COOP SICREDI PLANALTO RS/MG"), NOME_IF, "Others"))

mdcr_op_basica_if <- mdcr_op_basica_if %>% 
  mutate(channel_original = paste(SEGMENTO_IF,NOME_IF, sep = "_")) %>% 
  dplyr::rename(subsector_original = DESCRICAO_INTGR)

mdcr_op_basica_if_sort <- mdcr_op_basica_if%>% 
  dplyr::rename(source_original = source_of.finance_original) %>%
  dplyr::rename(origin_domestic_international = domestic_International,
                year = ANO,
                value_brl = VL_PARC_CREDITO,
                sector_original = ATIVIDADE,
                climate_component = climate_use) %>% 
  mutate(channel_landscape = "Financial Institution",
         activity_landscape = "-",
         region = "-",
         uf = CD_ESTADO,
         municipality = "-",
         beneficiary_landscape = "Rural producers",
         subactivity_landscape = "-",
         rio_marker = "-",
         beneficiary_public_private= "-",
         beneficiary_original = "-",
         localization_original = CD_ESTADO,
         subactivity_landscape = "-",
         id_original = "-") %>% 
  mutate(sector_landscape = if_else(sector_original %in% "agrícola", "Crop","Cattle")) %>% 
  mutate(sector_landscape = if_else(str_detect(project_name, "Florestas"),"Forest",
                                    sector_landscape)) %>%
  mutate(sector_landscape = if_else(str_detect(project_description, "extrativismo|florestamento|eucalipto|pinus|madeira|seringueira"),
                                    "Forest",
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
         beneficiary_public_private,CODIGO_PRODUTO)

df_final <- mdcr_op_basica_sort %>% 
  mutate(project_description = gsub(";"," ", mdcr_op_basica_sort$project_description)) %>%
  mutate(source_finance_landscape = if_else(source_original 
                                               %in% c("LETRA DE CRÉDITO DO AGRONEGÓCIO (LCA) - TAXA FAVORECIDA",
                                                      "LETRA DE CRÉDITO DO AGRONEGÓCIO (LCA) - TAXA LIVRE"),"Financial Institution", 
                                               if_else(source_original %in%
                                                         c("FUNCAFE-FUNDO DE DEFESA DA ECONOMIA CAFEEIRA"), 
                                                       "Federal and state governments", source_finance_landscape))) %>% 
  mutate(origin_private_public = if_else(source_original %in%
                                          c("FUNCAFE-FUNDO DE DEFESA DA ECONOMIA CAFEEIRA"), 
                                        "Public", origin_private_public))

rm(mdcr_op_basica_sort)

df_final <- df_final %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  mutate("original_currency" = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)



############ apply deflatd and exchange #######


source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))
############# ATUALIZADO EM 2024 -- pega valores para deflacionar USD na base USD A:\\macro\\usd_FED\\rawData\\Inflation_FED.xls
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd_v2.r"))

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)



tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
tabela_deflatorUSD <- deflator_usd(ano_ini, ano_fim, usd_inflation)


tabela_cambio <- cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)


df_sicor_calculus <- deflate_and_exchange(tabela_deflator, df_final, tabela_cambio)
df_sicor_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, df_sicor_calculus, tabela_cambio)

###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(df_sicor_calculus2)

# Quais colunas não existem no df original?
setdiff(landuse_cols, df_cols)
#[1] NA               "sub_sector_cpi"


# Criar dicionário de renomeação ignorando NAs
rename_vector <- Landscape_columns %>%
  filter(!is.na(`LAND USE`)) %>%
  mutate(`LAND USE` = trimws(`LAND USE`)) %>%
  distinct() %>%
  deframe()  # cria named vector: nomes atuais -> novos nomes

# Filtra o vetor de renomeação para colunas que existem no df
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(df_sicor_calculus2)]

# Renomeia apenas as colunas que existem
df_sicor_calculus_renamed <- df_sicor_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))


### Fazer DePara do sub_sector_cpi com base em sheet = "DeParaLandUse"
DePara.sub_sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(`Variavel Land Use` = trimws(`Variavel Land Use`),
         sub_sector_cpi = trimws(sub_sector_cpi)) %>%
  distinct(`Variavel Land Use`, sub_sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_sicor_calculus_dePara <- df_sicor_calculus_renamed %>%
  mutate(sub_sector_cpi = recode(sub_sector_cpi, !!!DePara.sub_sector))


## Adicionar 'sector_cpi" com base em "no depara
### Fazer DePara 
DePara.sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(sub_sector_cpi = trimws(sub_sector_cpi),
         sector_cpi = trimws(sector_cpi)) %>%
  distinct(sub_sector_cpi, sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_sicor_calculus_dePara <- df_sicor_calculus_dePara %>%
  mutate(sector_cpi = recode(sub_sector_cpi, !!!DePara.sector))



### Inserir informações em solution_cpi com base em "Solution" do UniqueKeys com base na relational
# Atlas e SES é "Rural Insurence for Climate Resilience"
DePara.solution <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(sub_sector_cpi = trimws(sub_sector_cpi),
         solution_cpi = trimws(solution_cpi)) %>%
  distinct(sub_sector_cpi, solution_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


df_sicor_calculus_dePara <- df_sicor_calculus_dePara %>%
  mutate(solution_cpi = recode(sub_sector_cpi, !!!DePara.solution))


### Adicionar sector_key: 
# a partir da coluna "Key_Sector" da planilha do excel UniqueKeys fazendo correspondência com "sector_cpi" feito acima

DePara.keysector <- planilha_uniqueKeys %>%
  select(Sector, Key_Sector) %>%
  mutate(
    Sector = trimws(Sector),
    Key_Sector = trimws(Key_Sector)
  ) %>%
  filter(!is.na(Sector), !is.na(Key_Sector), Sector != "") %>%  # remove entradas problemáticas
  distinct(Sector, Key_Sector) %>%
  deframe()

#Substitui valores com base no Depara
df_sicor_calculus_final <- df_sicor_calculus_dePara %>%
  mutate(sector_key_cpi = trimws(sector_cpi)) %>%
  mutate(sector_key_cpi = recode(sector_key_cpi, !!!DePara.keysector))



# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_sicor_calculus_final))
dif_cols
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_sicor_calculus_final[[col]] <- NA
  }
}



df_sicor_calculus_final2 <- df_sicor_calculus_final %>%
  mutate(country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         ID_Landscape = id_original) %>% 
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)


saveRDS(df_sicor_calculus,paste0(dir_output, "df_sicor_format_landscape_final_", ano_ini, "-", ano_fim, ".rds"))


write.xlsx(df_sicor_calculus,paste0(dir_output, "df_sicor_format_landscape_final_", ano_ini, "-", ano_fim, ".xlsx"))

toc()
gc()
