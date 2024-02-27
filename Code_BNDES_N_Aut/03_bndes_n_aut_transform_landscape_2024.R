library(tidyverse)
library(readxl)
library(stringi)
library(xlsx)

# Fazendo o load do script com as funções para classificar o sector landscape
source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

df_bndes_filter <- readRDS("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/df_bndes_n_aut_filter_reviewed.rds")%>%as_tibble

source_bndes_n_aut <- read_excel("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_n_aut <- read_excel("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")%>%select(channel_original,channel_landscape)

instrument_bndes_n_aut <- read_excel("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()%>% select(instrument_original,instrument_landscape)
beneficiary_bndes_n_aut <- read_excel("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape") %>% select(beneficiary_original,beneficiary_landscape,beneficiary_public_private)

climate_bndes_n_aut <- read_excel("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/BNDES_N_Aut/06_bndes_naut_relational_tables.xlsx", sheet = "climate_select") %>% 
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

cattle_contracts <- cattle_contracts %>% mutate(sector_landscape = case_when(
              grepl("\\bfabricacao de acucar em bruto\\b", x = Coluna_search , ignore.case = TRUE) ~ "Bioenergy and fuels",.default = sector_landscape
)) 
df_bndes_filter_landscape_v2 <- rbind(bioenergy_contracts,cattle_contracts,forest_contracts,multiSector_contracts,crop_contracts)

df_bndes_filter_landscape_v2
# Classificacao Climatica

#Produção de cana-de-açúcar, inclusive para geração de energia
producaoCanaAcucarGeracaoEnergia <- df_bndes_filter_landscape_v2 %>% filter((grepl("\\bacucar\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bacucar\\b",x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos alimenticios\\b",x = Coluna_search , ignore.case = TRUE))) %>% mutate(
  activity_landscape = "Produção de cana-de-açúcar, inclusive para geração de energia",
  subactivity_landscape = "Expansão e renovação de canaviais, otimização da colheita e ampliação da capacidade de moagem de cana. Inclui aquisição de máquinas, equipamentos e construção de unidades de armazenamento para etanol e açúcar."
) %>% mutate(climate_use = "Mitigação")

producaoCanaAcucarGeracaoEnergia%>% select(sector_original,subsector_original,project_description,sector_landscape,Coluna_search) %>% unique() %>% view

# Geração de energia renovável e medidas para eficiência energética
geracaoEnergiaRenovavelMedidasEficiencia <- df_bndes_filter_landscape_v2 %>% filter(
  (grepl("\\bfabricacao de alcool\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\brenovabio\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bdistribuicao de combustiveis gasosos por redes urbanas\\b", x = Coluna_search , ignore.case = TRUE) )
) %>% mutate(activity_landscape = "Geração de energia renovável e medidas para eficiência energética",
            subactivity_landscape = if_else((grepl("\\bcontratacao de credito para aquisicao\\b",x = Coluna_search,ignore.case=TRUE) | grepl("\\bapoio financeiro, por meio de credito\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bapoio direto por meio de credito asg\\b",x = Coluna_search,ignore.case = TRUE)),true="Modernização industrial e agrícola para aumentar a eficiência, expansão da exportação de energia renovável,investimentos em eficiência energética",
            false = if_else((grepl("\\bapoio ao plano de investimentos da companhia de gas\\b",x = Coluna_search,ignore.case = TRUE)),true="Tratamento de água e resíduos para produção de energia a partir do biogás.",
            false = if_else((grepl("\\bplacas fotovoltaicas\\b",x = Coluna_search,ignore.case = TRUE)),true="Energia solar para redes centralizadas, incluindo células fotovoltaicas e sistemas de energia solar concentrada, e para redes isoladas e sistemas autônomos, incluindo minirredes e sistemas solares residenciais",
            false = "Sem Classificacao")))) %>% mutate(climate_use = "Mitigação")


#Fazendo o 1o filtro:

filtro_1 <- rbind(producaoCanaAcucarGeracaoEnergia,geracaoEnergiaRenovavelMedidasEficiencia)
df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_1 , by="Coluna_search")

#Atividades para redução de emissões por desmatamento e degradação
AtividadesReducaoEmissoes <- df_bndes_filter_landscape_v2 %>% filter(
  (grepl("\\brestauracao\\b",x = Coluna_search,ignore.case = TRUE))
) %>% mutate(activity_landscape = "Atividades para redução de emissões por desmatamento e degradação",
            subactivity_landscape = if_else((grepl("\\brestauracao\\b",x = Coluna_search,ignore.case = TRUE)),true = "Conservação de florestas, restauração e recuperação de áreas degradadas, inclusive de vegetação nativa e áreas de preservação permanente, para melhorar o abastecimento de água. Projetos de reserva florestal privada.",false = "Sem Classificacao"))%>% mutate(climate_use = "Mitigação e Adaptação")

# Fazendo o 2o filtro:
filtro_2 <- rbind(filtro_1,AtividadesReducaoEmissoes)
df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_2 , by="Coluna_search")

# Atividades relacionadas à indústria de florestas plantadas, celulose e papel
AtividadesIndustriaFlorestas <- df_bndes_filter_landscape_v2 %>% filter(
  (grepl("\\bcelulose\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bcelulosi\\b",x = Coluna_search,ignore.case = TRUE))
) %>% mutate(activity_landscape = "Atividades relacionadas à indústria de florestas plantadas, celulose e papel",
            subactivity_landscape = if_else((grepl("\\bcontratacao de credito",x = Coluna_search,ignore.case = TRUE) | grepl("\\bcontratacao de limite de credito\\b",x = Coluna_search,ignore.case = TRUE)), true="Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
            false = if_else((grepl("\\beucalipto\\b",x = Coluna_search,ignore.case = TRUE)),true = "Implantação, manutenção e melhoramento do manejo de florestas comerciais, incluindo aquelas destinadas ao uso industrial ou à produção de carvão vegetal, bem como florestas comerciais de eucalipto e pinus tanto por meio de reforma quanto pela implantação de novas áreas.",
            false = "Sem Classificacao"))) %>% 
            mutate(climate_use = "Mitigação e Adaptação") 

# Fazendo 3o filtro:
filtro_3 <- rbind(AtividadesIndustriaFlorestas,filtro_2)
df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_3 , by="Coluna_search")


df_bndes_filter_landscape_v2 %>% select(sector_original,subsector_original,project_description,sector_landscape,Coluna_search) %>% unique() %>% view




