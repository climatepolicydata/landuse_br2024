library(tidyverse)
library(readxl)
library(stringi)

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

# Criando os dataframes com cada atividade landscape baseado no dicionario:
bioenergy_contracts <- bioenergy_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)
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
df_bndes_filter_landscape_v2 %>% mutate(
    beneficiary_original = str_c(natureza_do_cliente,porte_do_cliente,cliente,sep = "_")) %>% left_join(beneficiary_bndes_n_aut, by="beneficiary_original")%>% 
    mutate(localization_original = uf,
    region = "-",
    municipality = municipio)%>%write.xlsx("BNDES_landscape_21_23.xlsx")































