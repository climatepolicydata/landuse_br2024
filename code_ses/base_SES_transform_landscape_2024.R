##################

# Author : Renan Morais
# Date: 30.05.2023
# Email: renanflorias@hotmail.com
# Goal: transformação da base SES para landscape

##Modified by Julia Niemeyer
#Date: 27/05/2025

########################### Libraries ######################################

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               deflateBR,
               httr,
               rjson,
               magrittr, 
               tibble, 
               Matrix,
               data.table,
               pivottabler,
               readr,
               dplyr,
               readxl)

########################### ACTION NEEDED ######################################
# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

## set the path to your github clone
github <- "Documents/"

########################### Directories ########################################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_susep_dt_clean <- paste0('A:\\finance\\ses\\cleanData\\')

dir_susep_output <- "A:/projects/landuse_br2024/ses/output/"

if(!dir.exists(dir_susep_output)){
  dir.create(dir_susep_output)
}

dir_susep_doc <- ("A:/projects/landuse_br2024/ses")


########### import databases #########
# import landuse br database to get columns names and order 
Landscape_columns <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 


setwd(dir_susep_dt_clean)

ses_seguros2 <- read.csv(paste0(dir_susep_dt_clean, "ses_clear_", ano_fim, ".csv"), fileEncoding = "latin1", sep = ";")

ses_seguros2$premio_direto <- gsub(",",".",ses_seguros2$premio_direto) %>% as.numeric()

ses_seguros2$noenti <- str_trim(ses_seguros2$noenti)

setwd(dir_susep_doc)

codes2 <- read.xlsx("codes_ramo_rural_landscape.xlsx") %>% janitor::clean_names()

##################### filters and transforms ###########3

ses_seguros2 <- ses_seguros2 %>%
  #dplyr::mutate(ano = as.numeric(substr(damesano, 0, 4))) %>% 
  dplyr::filter(ano >= ano_ini & ano <= ano_fim) %>% 
  dplyr::filter(premio_direto != 0)


ses_seguros_filter_landscape <- ses_seguros2 %>% 
  dplyr::filter(coramo %in% codes2$codigos)

#sum(ses_seguros_filter_landscape$premio_direto) 

##################### transforms ########


#inserindo a descrição de códigos no database

codes2 <- codes2 %>% 
  dplyr::rename(coramo = codigos)

"criação id"

ses_seguros_filter_landscape <- ses_seguros_filter_landscape %>%
  group_by(coenti, coramo, ano) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

"agregando por coramo"

df_ses_agregado <- aggregate(premio_direto ~ id_equals + coenti + coramo + ano + noenti, data = ses_seguros_filter_landscape, FUN = sum)

df_ses_agregado <- merge(df_ses_agregado, codes2, by = "coramo")

df_ses_agregado_clear <- df_ses_agregado

# write.csv2(df_ses_agregado_clear, "ses_agregado_clear.csv", fileEncoding = "latin1") 

#eliminando observações com a descrição dita "seguro benf e produtos agropecuarios"
df_ses_agregado <- df_ses_agregado %>% 
  dplyr::filter(!descricao %in% "Seguro Benf. e Prod. Agropecuários")


#inserindo algumas classificações das categorias landscape
df_ses_agregado_transform <- df_ses_agregado %>% 
  dplyr::rename(id_original = id_equals, channel_original = noenti,sector_original = descricao, year = ano,
                value_brl = premio_direto) %>% 
  dplyr::mutate(instrument_original = "Seguro do Ramo Rural", subsector_original = "-", activity_landscape = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural Producers", beneficiary_public_private = "-", region = "-",
         uf= "-", municipality = "-",
         subactivity_landscape= "Rural insurance for farming and forestry", ecossystem_layer = "Gestão de Riscos Agropecuários",
         climate_component = "Adaptation", final_beneficiaries_final = "-", rio_marker = "-",
         CPI_riomarker_mitigation = "-",rio_marker = "-", Subfunção = "-", data_source = "ses_seguros", project_name = "Seguro Rural", project_description = "Prêmio Pago para Contratação de Seguro do Ramo Rural",
         source_original = "-", source_finance_landscape = "Rural Producers", origin_domestic_international = "National", origin_private_public = "Private", channel_landscape = "Financial Institution",
         instrument_landscape = "Risk management",localization_original = "-",
         sector_landscape = if_else(sector_original %in% c("Seguro Agrícola com cob. do FESR", "Seguro Agrícola sem cob. do FESR","Agrícola",
                                                           "Seguro Benf. e Prod. Agropecuários"), "Crop",
                                    "Forest"))

rm(df_ses_agregado, df_ses_agregado_clear, ses_seguros_filter_landscape, ses_seguros2, codes2)

"reorganizando a base"

df_ses_agregado <- df_ses_agregado_transform %>%
  select(id_original, data_source, year, project_name,
       project_description, source_original, source_finance_landscape,
       origin_domestic_international, origin_private_public,
       value_brl,channel_original, channel_landscape, instrument_original,
       instrument_landscape, sector_original, sector_landscape,
       subsector_original,activity_landscape, beneficiary_original,
       beneficiary_landscape,beneficiary_public_private,
       region, uf,municipality,
       ecossystem_layer,climate_component, final_beneficiaries_final,Subfunção,subactivity_landscape, rio_marker, localization_original)

rm(df_ses_agregado_transform)

df_ses_agregado <- df_ses_agregado %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  dplyr::mutate(original_currency = "BRL") %>%
  relocate(original_currency, .after = value_original_currency)



#################### REMOVE DUPLICATES
df_ses_filter <- df_ses_agregado %>%
  distinct(year, id_original, value_original_currency, .keep_all = TRUE)





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


df_ses_calculus <- deflate_and_exchange_Landuse(tabela_deflator, df_ses_filter, tabela_cambio)
df_ses_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, df_ses_calculus, tabela_cambio)



###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(df_ses_calculus2)

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
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(df_ses_calculus2)]

# Renomeia apenas as colunas que existem
df_ses_calculus_renamed <- df_ses_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))



### Fazer DePara do sub_sector_cpi com base em sheet = "DeParaLandUse"
DePara.sub_sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(`Variavel Land Use` = trimws(`Variavel Land Use`),
         sub_sector_cpi = trimws(sub_sector_cpi)) %>%
  distinct(`Variavel Land Use`, sub_sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_ses_calculus_dePara <- df_ses_calculus_renamed %>%
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
df_ses_calculus_dePara <- df_ses_calculus_dePara %>%
  mutate(sector_cpi = recode(sub_sector_cpi, !!!DePara.sector))




### Inserir informações em solution_cpi com base em "Solution" do UniqueKeys com base na relational
# Atlas e SES é Climate resiliency building rural insurance
# DePara.solution <- DePara %>%
#   filter(sector_landscape == 'sector_landscape') %>%
#   mutate(sub_sector_cpi = trimws(sub_sector_cpi),
#          solution_cpi = trimws(solution_cpi)) %>%
#   distinct(sub_sector_cpi, solution_cpi) %>%
#   deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


df_ses_calculus_dePara <- df_ses_calculus_dePara %>%
  mutate(solution_cpi = "Climate resiliency building rural insurance")


### Adicionar sector_key_cpi: 
# a partir da coluna "Key_Sector", "Key_Subsector" e Key_Solution da planilha do excel UniqueKeys fazendo correspondência com "sector_cpi", subsector_cpi e solution_cpi

DePara.keysector <- planilha_uniqueKeys %>%
  select(Sector, Key_Sector) %>%
  mutate(
    Sector = trimws(Sector),
    Key_Sector = trimws(Key_Sector)
  ) %>%
  filter(!is.na(Sector), !is.na(Key_Sector), Sector != "") %>%  # remove entradas problemáticas
  distinct(Sector, Key_Sector) %>%
  deframe()


DePara.keysubsector <- planilha_uniqueKeys %>%
  select(Subsector, Key_Subsector) %>%
  mutate(
    Subsector = trimws(Subsector),
    Key_Subsector = trimws(Key_Subsector)
  ) %>%
  filter(!is.na(Subsector), !is.na(Key_Subsector), Subsector != "") %>%  # remove entradas problemáticas
  distinct(Subsector, Key_Subsector) %>%
  deframe()



DePara.keysolution <- planilha_uniqueKeys %>%
  select(Solution, Key_Solution) %>%
  mutate(
    Solution = trimws(Solution),
    Key_Solution = trimws(Key_Solution)
  ) %>%
  filter(!is.na(Solution), !is.na(Key_Solution), Solution != "") %>%  # remove entradas problemáticas
  distinct(Solution, Key_Solution) %>%
  deframe()



#Substitui valores com base no Depara
df_ses_final <- df_ses_calculus_dePara %>%
  mutate(keysector = trimws(sector_cpi),
         keysubsector = trimws(sub_sector_cpi),
         keysolution = trimws(solution_cpi)) %>%
  mutate(keysector = recode(keysector, !!!DePara.keysector),
         keysubsector = recode(keysubsector, !!!DePara.keysubsector),
         keysolution = recode(keysolution, !!!DePara.keysolution)) %>%
  mutate(sector_key_cpi = str_c(keysector, "_", keysubsector, "_", keysolution)) %>%
  select(- c(keysector, keysubsector, keysolution))





# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_ses_final))
dif_cols
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_ses_final[[col]] <- NA
  }
}



df_ses_final2 <- df_ses_final %>%
  mutate(country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         ID_Landscape = NA) %>% 
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)




##### save dataset #####


write.csv(df_ses_calculus, paste0(dir_susep_output, "ses_agregado_landscape_completo_", ano_ini, "-", ano_fim, ".csv"))

saveRDS(df_ses_calculus, paste0(dir_susep_output, "ses_agregado_landscape_completo_", ano_ini, "-", ano_fim, ".rds"))

write.csv2(df_ses_final2, paste0(dir_susep_output, "base_landscape_final_", ano_ini, "-", ano_fim, ".csv"), fileEncoding = "Latin1", row.names = F)



