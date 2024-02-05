library(tidyverse)
library(readxl)
library(stringi)

# Fazendo o load do script com as funções para classificar o sector landscape
source("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

df_bndes_filter <- readRDS("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/df_bndes_n_aut_filter_reviewed.rds")%>%as_tibble

source_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")%>%select(channel_original,channel_landscape)

instrument_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()%>% select(instrument_original,instrument_landscape)
beneficiary_bndes_n_aut <- read_excel("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape")

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



bioenergy_contracts <- bioenergy_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)

cattle_contracts <- cattle_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)

forest_contracts <- forest_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)

multiSector_contracts <- MultiSector_search_pattern_BNDES(data_frame_BNDES = df_bndes_filter_landscape, Coluna_search = Coluna_search)



