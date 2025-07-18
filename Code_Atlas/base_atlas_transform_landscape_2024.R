##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transformação base atlas rural para landscape
# resource: 


# Modified by Julia Niemeyer
# Date: 28/05/2025

########################### ACTION NEEDED ######################################
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

## set the path to your github clone
github <- "Documents/"


########################### Libraries ######################################

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               readxl,
               openxlsx, 
               httr,
               magrittr, 
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)

################## directories ###################
 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_sisser_mapa_dt_clean <- ("A:/finance/atlas_Seguro_Rural/cleanData")



############### import databases #####################

# import landuse br database to get columns names and order 
Landscape_columns <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"),  sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 
                                

df_atlas <- readRDS(paste0(dir_sisser_mapa_dt_clean, "/atlas_2006_", ano_fim, "_clear.rds"))

df_atlas_filter <- df_atlas %>% 
  dplyr::filter(ano_apolice >= ano_ini & ano_apolice <= ano_fim,
         !nm_cultura_global %in% c("Pecuário"))


df_atlas_filter <- df_atlas_filter  %>% 
  select(nm_razao_social, nm_municipio_propriedade, sg_uf_propriedade, 
         nm_classif_produto, nm_cultura_global, vl_subvencao_federal,
         ano_apolice, evento_preponderante) 

df_atlas <- df_atlas_filter %>% 
  group_by(nm_razao_social, nm_municipio_propriedade, sg_uf_propriedade, 
           nm_classif_produto, nm_cultura_global,
           ano_apolice, evento_preponderante) %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

"remove values with count for 'livestock' "

df_atlas <- df_atlas %>% 
  dplyr::filter(!nm_cultura_global %in% c("Pecuário"))

df_atlas_sub_negative <- aggregate( vl_subvencao_federal ~ id_equals + nm_razao_social + nm_municipio_propriedade + 
                                      sg_uf_propriedade + nm_classif_produto+ nm_cultura_global+ vl_subvencao_federal+
                                    ano_apolice + evento_preponderante, data = df_atlas,
                                FUN = sum)

df_atlas_subvencao <- aggregate(vl_subvencao_federal ~ id_equals + nm_razao_social+ nm_municipio_propriedade + 
                                   sg_uf_propriedade + nm_classif_produto+ nm_cultura_global+ vl_subvencao_federal+
                                   ano_apolice + evento_preponderante, data = df_atlas,
                                 FUN = sum)

rm(df_atlas)


############### transform ########

setwd("A:/projects/landuse_br2024/atlas")

relational_table <- read.xlsx("03_atlas_relational_tables.xlsx", sheet = "sector_landscape") %>% 
  select(nm_cultura_global, sector_landscape) %>% unique()


df_atlas_sub_negative <- join(df_atlas_sub_negative,relational_table %>% select(nm_cultura_global, sector_landscape), by = "nm_cultura_global")

df_atlas_subvencao <- join(df_atlas_subvencao,relational_table %>% select(nm_cultura_global, sector_landscape), by = "nm_cultura_global")


"base para valor subvenção negativo"

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  dplyr::rename(year = ano_apolice, channel_original = nm_razao_social,
                subsector_original = evento_preponderante, uf = sg_uf_propriedade, municipality = nm_municipio_propriedade) %>% 
  dplyr::mutate(data_source = "atlas_seguro_mapa",project_name = "Abate no SES para Subvenção PSR",
         project_description = "nm_classif_produto", source_original = "Produtores Rurais",
         source_finance_landscape = "Rural Producers", origin_domestic_international = "National",
         origin_private_public = "Private", channel_landscape= "Financial Institution",
         instrument_original = "-", instrument_landscape= "Risk management",
         sector_original = paste0(nm_classif_produto,"_", nm_cultura_global),
         activity_landscape = "Financial services", subactivity_landscape = '-',
         climate_component = "Adaptation", rio_marker = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural producers", 
         beneficiary_public_private = "-", localization_original = "-",
         region = uf)%>%
  dplyr::mutate(id_original = paste(id_equals,"VLN", sep = ""),
         subactivity_landscape = "Tipo de Produto") %>% 
  dplyr::rename(value_brl = vl_subvencao_federal) %>% 
  dplyr::mutate(value_brl = -1*value_brl)

"base para valor da subvenção"

df_atlas_subvencao <- df_atlas_subvencao %>% 
  dplyr::rename(year = ano_apolice, channel_original = nm_razao_social,
                subsector_original = evento_preponderante, uf = sg_uf_propriedade, municipality = nm_municipio_propriedade) %>% 
  dplyr::mutate(data_source = "atlas_seguro_mapa",project_name = "Subvenção PSR",
         project_description = "nm_classif_produto", source_original = "MAPA",
         source_finance_landscape = "Federal and state governments", origin_domestic_international = "National",
         origin_private_public = "Public", channel_landscape= "Financial Institution",
         instrument_original = "Subvenção PSR", instrument_landscape= "Risk management",
         sector_original = paste0(nm_classif_produto,nm_cultura_global),
         activity_landscape = "Financial services",
         climate_component = "Adaptation", rio_marker = "-",
         beneficiary_original = "-", beneficiary_landscape = "Rural producers", 
         beneficiary_public_private = "-", localization_original = "-",
         region = uf)%>% 
  dplyr::mutate(id_original = paste(id_equals,"SUB", sep = ""),
         subactivity_landscape = "Rural insurance for farming and forestry") %>% 
  dplyr::rename(value_brl = vl_subvencao_federal)

############ reogarnizando a base#########

df_atlas_sub_negative <- df_atlas_sub_negative %>% 
  select(id_original,value_brl ,data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)

df_atlas_subvencao <- df_atlas_subvencao %>% 
  select(id_original, value_brl, data_source, year, project_name, project_description, source_original,
         source_finance_landscape, origin_domestic_international, origin_private_public, channel_original,
         channel_landscape, instrument_original, instrument_landscape, sector_original, sector_landscape,
         subsector_original, activity_landscape, subactivity_landscape, climate_component, rio_marker, beneficiary_original, beneficiary_landscape,
         beneficiary_public_private, localization_original, region, uf,municipality)


"JUNÇÃO DAS BASES"

df_atlas_premio_liq_sub <- rbind(df_atlas_sub_negative,df_atlas_subvencao)



df_atlas_final <- df_atlas_premio_liq_sub %>% 
  dplyr::rename(value_original_currency = value_brl) %>% 
  dplyr::mutate(original_currency = "BRL",
         year = as.numeric(year)) %>%
  relocate(original_currency, .after = value_original_currency)

rm(df_atlas_premio_liq_sub, df_atlas_sub_negative, df_atlas_subvencao)



############ apply deflated and exchange #######

# Inserir o caminho onde foi feito o clone do projeto 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

############## ATUALIZADO EM 2025 -- automatico -- atualiza com base em ano_ini e ano_fim
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))

############# ATUALIZADO EM 2024 -- pega valores para deflacionar USD na base USD A:\\macro\\usd_FED\\rawData\\Inflation_FED.xls
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd_v2.r"))

####### rodar essa função para atualizar a tabela de taxa de cambio
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
tabela_deflatorUSD <- deflator_usd(ano_ini, ano_fim, usd_inflation)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)


df_atlas_calculus <- deflate_and_exchange_Landuse(tabela_deflator, df_atlas_final, tabela_cambio)
df_atlas_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, df_atlas_calculus, tabela_cambio)


###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(df_atlas_calculus2)

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
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(df_atlas_calculus2)]

# Renomeia apenas as colunas que existem
df_atlas_calculus_renamed <- df_atlas_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))


### Fazer DePara do sub_sector_cpi com base em sheet = "DeParaLandUse"
DePara.sub_sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(`Variavel Land Use` = trimws(`Variavel Land Use`),
         sub_sector_cpi = trimws(sub_sector_cpi)) %>%
  distinct(`Variavel Land Use`, sub_sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_atlas_calculus_dePara <- df_atlas_calculus_renamed %>%
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
df_atlas_calculus_dePara <- df_atlas_calculus_dePara %>%
  mutate(sector_cpi = recode(sub_sector_cpi, !!!DePara.sector))


### Inserir informações em solution_cpi com base em "Solution" do UniqueKeys com base na relational
# Atlas e SES é "Rural Insurence for Climate Resilience"
# DePara.solution <- DePara %>%
#   filter(sector_landscape == 'sector_landscape') %>%
#   mutate(sub_sector_cpi = trimws(sub_sector_cpi),
#          solution_cpi = trimws(solution_cpi)) %>%
#   distinct(sub_sector_cpi, solution_cpi) %>%
#   deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


df_atlas_calculus_dePara <- df_atlas_calculus_dePara %>%
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
df_atlas_final <- df_atlas_calculus_dePara %>%
  mutate(keysector = trimws(sector_cpi),
         keysubsector = trimws(sub_sector_cpi),
         keysolution = trimws(solution_cpi)) %>%
  mutate(keysector = recode(keysector, !!!DePara.keysector),
         keysubsector = recode(keysubsector, !!!DePara.keysubsector),
         keysolution = recode(keysolution, !!!DePara.keysolution)) %>%
  mutate(sector_key_cpi = str_c(keysector, "_", keysubsector, "_", keysolution)) %>%
  select(- c(keysector, keysubsector, keysolution))




# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_atlas_final))
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_atlas_final[[col]] <- NA
  }
}



df_atlas_final2 <- df_atlas_final %>%
  mutate(country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         ID_Landscape = id_original) %>% 
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)







setwd("A:/projects/landuse_br2024/atlas/output")

saveRDS(df_atlas_final2, paste0("database_atlas_landscape_", ano_ini, "_", ano_fim, ".rds"))
write.csv2(df_atlas_final2, paste0("database_atlas_landscape_", ano_ini, "_", ano_fim, ".csv"), fileEncoding = "Latin1")


