library(tidyverse)
library(readxl)
library(stringi)
library(xlsx)

# Fazendo o load do script com as funções para classificar o sector landscape
source("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

df_bndes_filter <- readRDS("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/df_bndes_n_aut_filter_reviewed.rds")%>%as_tibble

source_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")%>%select(channel_original,channel_landscape)

instrument_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()%>% select(instrument_original,instrument_landscape)
beneficiary_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape") %>% select(beneficiary_original,beneficiary_landscape,beneficiary_public_private)

climate_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "climate_select") %>% 
  mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>% 
  mutate(climate_original = paste0(numero_do_contrato,instrumento_financeiro)) %>% 
  mutate_if(is.character, tolower) %>% filter(!numero_do_contrato == "15208221")

# Inicio da transformacao para landscape:
df_bndes_filter_landscape <- df_bndes_filter %>%
    mutate(
        id_original = numero_do_contrato,
        data_source = "bndes_n_aut",
        year = ano,
        project_name = str_c(cliente,cnpj, sep =";"),
        project_description = descricao_do_projeto,
        source_original = fonte_de_recurso_desembolsos) %>%left_join(source_bndes_n_aut,by = "source_original") %>% mutate(
            value_original_currency = valor_contratado_reais,
            original_currency = "BRL",
            channel_original = str_c(forma_de_apoio,instituicao_financeira_credenciada,sep = "_")
        ) %>% left_join(channel_bndes_n_aut, by="channel_original") %>% mutate(
            instrument_original = instrumento_financeiro) %>% left_join(instrument_bndes_n_aut,by="instrument_original") %>%
            mutate(sector_original = subsetor_cnae_nome,
            subsector_original=subsetor_cnae_agrupado,
            Coluna_search = str_c(sector_original,subsector_original,project_description,sep=";"))
            
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% mutate(
    beneficiary_original = str_c(natureza_do_cliente,porte_do_cliente,cliente,sep = "_")) %>% left_join(beneficiary_bndes_n_aut, by="beneficiary_original")%>% 
    mutate(localization_original = uf,
    region = "-",
    municipality = municipio)


# Filtrando as observações do BNDES 2020-2023 em que os projetos sao similares aos projetos do Landscape antigo. Assim a gente já consegue classificar o sector landscape e a atividade climatica
# Primeiro, vamos ler a base do landscape antigo
last_landscape <- read_rds("./brlanduse_landscape2024_dados/Dict/base_landscape_final_01022024.rds")
last_landscape <- last_landscape %>% mutate(sector_landscape= case_when(
  sector_landscape == "crop" ~ "Crop",sector_landscape == "forest" ~ "Forest", sector_landscape=="cattle" ~ "Cattle",
  sector_landscape == "Bioenergy and fuels" | sector_landscape == "Bioenergy And Fuels" ~ "Bioenergy and Fuels",sector_landscape == "Agriculture" ~ "Crop",.default = sector_landscape
))

# Vamos agora dar um inner join entre essa base com nossa base atual pelo nome do cliente
bndes <- last_landscape%>%filter(data_source=="bndes_naut")%>% as_tibble()
bndes_filter_cols <- bndes %>% select(beneficiary_original,sector_landscape,climate_component,rio_marker)%>%distinct(beneficiary_original,.keep_all = TRUE)
bndes_filter_cols%>%view
df_bndes_filter_landscape_climate_select <- df_bndes_filter_landscape %>% inner_join(bndes_filter_cols,by = "beneficiary_original")
filter_fora <- df_bndes_filter_landscape_climate_select%>%select(id_original)%>% as_vector

# Agora vamos filtrar os contratos restantes que nao houve match
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!id_original %in% filter_fora)
# Criando os dataframes com cada atividade landscape baseado no dicionario:
bioenergy_contracts <- bioenergy_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
bioenergy_contracts%>%view
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!numero_do_contrato %in% bioenergy_contracts$numero_do_contrato)

cattle_contracts <- cattle_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!numero_do_contrato %in% cattle_contracts$numero_do_contrato)

forest_contracts <- forest_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!numero_do_contrato %in% forest_contracts$numero_do_contrato)

multiSector_contracts <- multiSector_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!numero_do_contrato %in% multiSector_contracts$numero_do_contrato)



crop_contracts <- crop_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
df_bndes_filter_landscape <- df_bndes_filter_landscape %>% filter(!numero_do_contrato %in% crop_contracts$numero_do_contrato)


# Unindo essas bases mas antes criando a coluna de sector_landscape
bioenergy_contracts$sector_landscape = "Bioenergy and fuels"
cattle_contracts$sector_landscape = "Cattle"
forest_contracts$sector_landscape = "Forest"
multiSector_contracts$sector_landscape = "Multisector"
crop_contracts$sector_landscape = "Crop"
df_bndes_filter_landscape_v2 <- rbind(bioenergy_contracts,cattle_contracts,forest_contracts,multiSector_contracts,crop_contracts)
df_bndes_filter_landscape_v2%>%view

# Dando sequencia a criação das colunas para o landscape:

df_bndes_filter_landscape_v2 %>% names
df_bndes_filter_landscape_climate_select %>% names




























