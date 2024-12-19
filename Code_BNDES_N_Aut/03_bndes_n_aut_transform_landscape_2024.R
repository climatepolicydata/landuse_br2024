library(tidyverse)
library(readxl)
library(stringi)
library(readr)
# library(xlsx)

# Fazendo o load do script com as funções para classificar o sector landscape
# source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")

github <- "Documents"
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
source(paste0(root,github,"/GitHub/landuse_br2024/AuxFolder/Dictionary_Sectors.R"))

df_bndes_filter <- read_rds("A:\\projects\\landuse_br2024\\bndes_n_aut\\output\\df_bndes_n_aut_filter_reviewed_03_24.rds")%>%as_tibble
df_bndes_filter%>%view
source_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")

channel_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")%>%select(channel_original,channel_landscape)

instrument_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()%>% select(instrument_original,instrument_landscape)
beneficiary_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape") %>% select(beneficiary_original,beneficiary_landscape,beneficiary_public_private)

climate_bndes_n_aut <- read_excel("A:\\projects\\landuse_br2024\\bndes_n_aut\\06_bndes_naut_relational_tables.xlsx", sheet = "climate_select") %>% 
  dplyr::mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>%
  dplyr::mutate(climate_original = paste0(id_original,year,
                                   project_name, sector_original))
#   mutate_if(is.character, tolower) %>% filter(!id_original == "15208221")

# df_bndes_filter_landscape <- df_bndes_filter %>% dplyr::mutate(
#     id_original = numero_do_contrato) %>% 
#   dplyr::filter(id_original %in% climate_bndes_n_aut$id_original)

# Inicio da transformacao para landscapedf_bndes_filter
df_bndes_filter_landscape <- df_bndes_filter %>% dplyr::rename(id_original = numero_do_contrato,
                                                               sector_original = subsetor_cnae_nome,
                                                               value_original_currency = valor_contratado_reais,
                                                               instrument_original = instrumento_financeiro) %>% 
    dplyr::mutate(data_source = "bndes_n_aut",
        year = ano,
        project_name = cliente,
        project_description = descricao_do_projeto,
        source_original = fonte_de_recurso_desembolsos,
        climate_original = paste0(id_original,year,
                                  project_name, sector_original),
        original_currency = "brl",
        channel_original = str_c(forma_de_apoio,instituicao_financeira_credenciada,sep = "_")) 

df_bndes_filter_landscape <- df_bndes_filter_landscape %>% dplyr::filter(climate_original %in% climate_bndes_n_aut$climate_original)

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape, climate_bndes_n_aut %>% select(climate_original,sector_landscape, activity_landscape,
                                                                                                 subactivity_landscape, climate_component), by= "climate_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,source_bndes_n_aut,by="source_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,channel_bndes_n_aut,by="channel_original")

df_bndes_filter_landscape <- left_join(df_bndes_filter_landscape,instrument_bndes_n_aut,by="instrument_original")



%>%left_join(source_bndes_n_aut,by = "source_original") %>% mutate(
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
last_landscape <- read_rds("A:\\projects\\brlanduse_landscape102023\\output_final\\base_landscape_final_14032024.rds")
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

# 
# # Unindo essas bases mas antes criando a coluna de sector_landscape
# bioenergy_contracts$sector_landscape = "Bioenergy and fuels"
# cattle_contracts$sector_landscape = "Cattle"
# forest_contracts$sector_landscape = "Forest"
# multiSector_contracts$sector_landscape = "Multi-sector"
# crop_contracts$sector_landscape = "Crop"
# 
# cattle_contracts <- cattle_contracts %>% mutate(sector_landscape = case_when(
#               grepl("\\bfabricacao de acucar em bruto\\b", x = Coluna_search , ignore.case = TRUE) ~ "Bioenergy and fuels",.default = sector_landscape
# )) 
# df_bndes_filter_landscape_v2 <- rbind(bioenergy_contracts,cattle_contracts,forest_contracts,multiSector_contracts,crop_contracts)
# 
# df_bndes_filter_landscape_v2%>%view
# # Classificacao Climatica
# 
# #Produção de cana-de-açúcar, inclusive para geração de energia
# producaoCanaAcucarGeracaoEnergia <- df_bndes_filter_landscape_v2 %>% filter((grepl("\\bacucar\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bacucar\\b",x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos alimenticios\\b",x = Coluna_search , ignore.case = TRUE))) %>% mutate(
#   activity_landscape = "Produção de cana-de-açúcar, inclusive para geração de energia",
#   subactivity_landscape = "Expansão e renovação de canaviais, otimização da colheita e ampliação da capacidade de moagem de cana. Inclui aquisição de máquinas, equipamentos e construção de unidades de armazenamento para etanol e açúcar."
# ) %>% mutate(climate_use = "Mitigation")
# 
# producaoCanaAcucarGeracaoEnergia%>% select(sector_original,subsector_original,project_description,sector_landscape,Coluna_search) %>% unique() %>% view
# 
# # Geração de energia renovável e medidas para eficiência energética
# geracaoEnergiaRenovavelMedidasEficiencia <- df_bndes_filter_landscape_v2 %>% filter(
#   (grepl("\\bfabricacao de alcool\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\brenovabio\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\benergia\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bdistribuicao de combustiveis gasosos por redes urbanas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bconstrucao de uma unidade industrial para producao de biometano\\b", x = Coluna_search , ignore.case = TRUE) |
#   grepl("\\bbiogas\\b", x = Coluna_search , ignore.case = TRUE))
# ) %>% mutate(activity_landscape = "Geração de energia renovável e medidas para eficiência energética",
#             subactivity_landscape = if_else((grepl("\\bcontratacao de credito para aquisicao\\b",x = Coluna_search,ignore.case=TRUE) | grepl("\\bapoio financeiro, por meio de credito\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bapoio direto por meio de credito asg\\b",x = Coluna_search,ignore.case = TRUE)),true="Modernização industrial e agrícola para aumentar a eficiência, expansão da exportação de energia renovável,investimentos em eficiência energética",
#             false = if_else((grepl("\\bapoio ao plano de investimentos da companhia de gas\\b",x = Coluna_search,ignore.case = TRUE)),true="Tratamento de água e resíduos para produção de energia a partir do biogás.",
#             false = if_else((grepl("\\bplacas fotovoltaicas\\b",x = Coluna_search,ignore.case = TRUE)),true="Energia solar para redes centralizadas, incluindo células fotovoltaicas e sistemas de energia solar concentrada, e para redes isoladas e sistemas autônomos, incluindo minirredes e sistemas solares residenciais",
#             false = if_else((grepl("\\bunidade de cogeracao de energia\\b",x = Coluna_search,ignore.case = TRUE)),true = "Produção de vapor e cogeração de energia a partir da cana-de-açúcar.",
#             false = if_else((grepl("\\bcontratacao de limite de credito para financiamento\\b",x = Coluna_search,ignore.case = TRUE)),true = "Modernização industrial e agrícola para aumentar a eficiência, expansão da exportação de energia renovável, investimentos em eficiência energética.",
#             false = if_else((grepl("\\bconstrucao de nova linha de transmissao\\b",x = Coluna_search,ignore.case = TRUE)),true = "Construção de subestações e linhas de transmissão para conexão à rede elétrica nacional.",
#             false = if_else((grepl("\\bunidade industrial\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bbiometano\\b",x = Coluna_search,ignore.case = TRUE)),
#             true = "Usinas de energia movidas a biocombustíveis que utilizam biomassa e biogases para geração direta de energia.",
#             false = if_else((grepl("\\bconstrucao\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bplanta de purificacao de biogas\\b",x = Coluna_search,ignore.case = TRUE)),true = "Usinas de energia movidas a biocombustíveis que utilizam biomassa e biogases para geração direta de energia.",
#             false = if_else((grepl("\\bconstrucao\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bcentral de recebimento e processamento da biomassa\\b",x = Coluna_search,ignore.case = TRUE)),true = "Usinas de energia movidas a biocombustíveis que utilizam biomassa e biogases para geração direta de energia",
#             false = "Sem Classificacao")))))))))) %>% mutate(climate_use = "Mitigation")%>%  filter(
#               (!grepl("\\bplantio de ate 1.780 hectares de variedades protegidas\\b", x = Coluna_search , ignore.case = TRUE))&
#               (!grepl("\\bapoio as atividades de producao agropecuaria de produtores rurais\\b", x = Coluna_search , ignore.case = TRUE)) &
#               (!grepl("\\bimplantacao de fabrica de biofertilizantes\\b", x = Coluna_search , ignore.case = TRUE))
#             )
# 
# #Fazendo o 1o filtro:
# 
# filtro_1 <- rbind(producaoCanaAcucarGeracaoEnergia,geracaoEnergiaRenovavelMedidasEficiencia)
# df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_1 , by="Coluna_search")
# 
# #Atividades para redução de emissões por desmatamento e degradação
# AtividadesReducaoEmissoes <- df_bndes_filter_landscape_v2 %>% filter(
#   (grepl("\\brestauracao\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bapoiar os investimentos destinados a revitalizacao, modernizacao e manutencao de areas dos parques nacionais\\b",x = Coluna_search,ignore.case = TRUE)|
#   (grepl("\\bapoiar o fortalecimento do manejo florestal comunitario\\b",x = Coluna_search,ignore.case = TRUE)) | (grepl("\\bgestao\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bterritorial\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bambiental\\b",x = Coluna_search,ignore.case = TRUE)))
# ) %>% mutate(activity_landscape = "Atividades para redução de emissões por desmatamento e degradação",
#             subactivity_landscape = if_else((grepl("\\brestauracao\\b",x = Coluna_search,ignore.case = TRUE)),true = "Conservação de florestas, restauração e recuperação de áreas degradadas, inclusive de vegetação nativa e áreas de preservação permanente, para melhorar o abastecimento de água. Projetos de reserva florestal privada.",
#             false = if_else((grepl("\\bapoiar os investimentos destinados a revitalizacao, modernizacao e manutencao de areas dos parques nacionais\\b",x = Coluna_search,ignore.case = TRUE)),true = "Gestão de áreas protegidas (unidades de conservação e terras indígenas), incluindo planos de gestão territorial e ambiental.",
#             false = if_else((grepl("\\bapoiar o fortalecimento do manejo florestal comunitario\\b",x = Coluna_search,ignore.case = TRUE)) ,true="Produção extrativista, manejo florestal comunitário e a projetos socioambientais de organizações agroextrativistas, com ações de desenvolvimento de competências, suporte técnico e associativismo. Cadeias de valor de óleos vegetais, cacau silvestre e borracha e fortalecimento das cadeias produtivas florestais não madeireiras.",
#             false =if_else((grepl("\\bgestao\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bterritorial\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bambiental\\b",x = Coluna_search,ignore.case = TRUE)),
#             true = "Gestão de áreas protegidas (unidades de conservação e terras indígenas), incluindo planos de gestão territorial e ambiental.",
#             false = "Sem Classificacao") ))))%>% mutate(climate_use = "Mitigation and Adaptation")
# 
# filtro_1 %>%inner_join(AtividadesReducaoEmissoes,by="Coluna_search")%>%view
# # Fazendo o 2o filtro:
# filtro_2 <- rbind(filtro_1,AtividadesReducaoEmissoes)
# df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_2 , by="Coluna_search")
# 
# # Atividades relacionadas à indústria de florestas plantadas, celulose e papel
# AtividadesIndustriaFlorestas <- df_bndes_filter_landscape_v2 %>% filter(
#   (grepl("\\bcelulose\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bcelulosi\\b",x = Coluna_search,ignore.case = TRUE) | (grepl("\\bprojeto\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bflorestal\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\brebrota\\b",x = Coluna_search,ignore.case = TRUE)) |
#   (grepl("\\bprograma\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bflorestal\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bbrotacao\\b",x = Coluna_search,ignore.case = TRUE))|
#   (grepl("\\bprograma florestal abrangendo implantacao, reforma e conducao\\b",x = Coluna_search,ignore.case = TRUE)))) %>% mutate(activity_landscape = "Atividades relacionadas à indústria de florestas plantadas, celulose e papel",
#             subactivity_landscape = if_else((grepl("\\bcontratacao de credito",x = Coluna_search,ignore.case = TRUE) | grepl("\\bcontratacao de limite de credito\\b",x = Coluna_search,ignore.case = TRUE)), true="Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
#             false = if_else((grepl("\\beucalipto\\b",x = Coluna_search,ignore.case = TRUE)),true = "Implantação, manutenção e melhoramento do manejo de florestas comerciais, incluindo aquelas destinadas ao uso industrial ou à produção de carvão vegetal, bem como florestas comerciais de eucalipto e pinus tanto por meio de reforma quanto pela implantação de novas áreas.",
#             false = if_else((grepl("\\bbrotacao\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\brebrota\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bimplantacao\\b",x = Coluna_search,ignore.case = TRUE)),true = "Aquisição e construção de infraestrutura para beneficiamento de madeira. Plantio e replantio, produção e aquisição de mudas, preparo de solo, proteção e manutenção das mudas plantadas até a colheita",
#             false = if_else((grepl("\\bsuzano\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bmodernizacao\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bfabricacao de celulose\\b",x = Coluna_search,ignore.case = TRUE)),true = "Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
#             false = if_else((grepl("\\blimite de credito para financiamento\\b",x = Coluna_search,ignore.case = TRUE) & grepl("\\bfabricacao de chapas e de embalagens\\b",x = Coluna_search,ignore.case = TRUE)),true = "Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
#             false = if_else(21544004 %in% id_original, true = "Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
#             false = "Sem Classificacao"))))))) %>% 
#             mutate(climate_use = "Mitigation and Adaptation") 
# filtro_2 %>%inner_join(AtividadesIndustriaFlorestas,by="Coluna_search")%>%view
# 
# 
# 
# # Fazendo 3o filtro:
# filtro_3 <- rbind(AtividadesIndustriaFlorestas,filtro_2)
# df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_3 , by="Coluna_search")
# 
# #Produção de defensivos agrícolas biológicos e orgânicos
# Producao_DefensivosAgricolas <- df_bndes_filter_landscape_v2 %>% filter(
#   (grepl("\\bbiofertilizantes\\b",x = Coluna_search,ignore.case = TRUE))
# ) %>% mutate(activity_landscape = "Produção de defensivos agrícolas biológicos e orgânicos",
#             subactivity_landscape = if_else((grepl("\\bbiofertilizantes\\b",x = Coluna_search,ignore.case = TRUE)),true = "Fabricação de fertilizantes orgânicos, produtos para o controle biológico de pragas e desenvolvimento de novas tecnologias.",
#             false = "Sem Classificacao"),
#             climate_use = "Adaptation")
# filtro_3 %>%inner_join(Producao_DefensivosAgricolas,by="Coluna_search")%>%view
# 
# #Fazendo 4o filtro
# filtro_4 <- rbind(Producao_DefensivosAgricolas,filtro_3)
# df_bndes_filter_landscape_v2 <- df_bndes_filter_landscape_v2 %>% anti_join(filtro_4 , by="Coluna_search")
# # Dando continuidade ao landscape
# 
# landscape_bndesNAUT <- filtro_4 %>% select(
#   all_of(c("id_original","data_source","year","project_name","project_description","source_original",
#   "source_of_finance_landscape","national_internacional","source_private_public","value_original_currency",
#   "original_currency","channel_original","channel_landscape","instrument_original","instrument_landscape","sector_original",
#   "sector_landscape","subsector_original","activity_landscape","subactivity_landscape","climate_use",
#   "beneficiary_original","beneficiary_landscape","beneficiary_public_private","localization_original","region","uf","municipality","valor_contratado_reais")) 
# )
# getwd()
# 
# library(writexl)
# landscape_bndesNAUT %>% write_xlsx("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_23052024.xlsx")
# landscape_bndesNAUT%>% write_rds("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_23052024.rds")
# 
# 
# bndes_NAUT_fora <- df_bndes_filter_landscape_v2%>% select(
#   all_of(c("id_original","data_source","year","project_name","project_description","source_original",
#   "source_of_finance_landscape","national_internacional","source_private_public","value_original_currency",
#   "original_currency","channel_original","channel_landscape","instrument_original","instrument_landscape","sector_original",
#   "sector_landscape","subsector_original",
#   "beneficiary_original","beneficiary_landscape","beneficiary_public_private","localization_original","region","uf","municipality","valor_contratado_reais")) 
# )
# bndes_NAUT_fora %>% write_xlsx("bndes_naut_Fora5.xlsx")

# base_select_deflator <- landscape_bndesNAUT %>% mutate(value_original_currency = as.numeric(str_replace(value_original_currency,pattern = ",",".")))  %>% 
#   left_join(teste, by= "year")%>%
#   dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
# base_select_deflator$value_brl_deflated %>% sum
# 
# base_select_deflator = base_select_deflator %>% 
#   left_join(tabela_cambio,by='year') %>% 
#   dplyr::mutate(value_usd = value_brl_deflated/cambio)
# 
# base_select_deflator %>% select(-valor_contratado_reais,-deflator,-cambio) %>% mutate(rio_marker="-") %>% write_rds("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_23052024.rds")
# base_select_deflator %>% select(-valor_contratado_reais,-deflator,-cambio) %>% mutate(rio_marker="-") %>% write_xlsx("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_23052024.xlsx")
# 
# 
# 
# df_bndes_naut_calculus <- readRDS("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_23052024.rds") %>% 
#   dplyr::rename(climate_component = climate_use,
#                 source_finance_landscape = source_of_finance_landscape,
#                 origin_domestic_international = national_internacional,
#                 origin_private_public = source_private_public)
# dados2<- read_rds("A:\\projects\\brlanduse_landscape102023\\output_final\\base_landscape_final_20052024_reviewed.rds")
# dados2 <- dados2 %>% select(-value_brl_deflated_mean,-value_usd_mean)
# dados2<-dados2 %>% filter(data_source  == "bndes_naut")
# f_bndes_naut_calculus$data_source = "bndes_naut"
# a <- rbind(f_bndes_naut_calculus,dados2)
# 
# base_select_deflator <- a %>% 
#   left_join(teste, by= "year")%>%
#   dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
# base_select_deflator %>% view
# 
# base_select_deflator = base_select_deflator %>% 
#   left_join(tabela_cambio,by='year') %>% 
#   dplyr::mutate(value_usd = value_brl_deflated/cambio)
# base_select_deflator %>% select(-cambio,-deflator) %>% write_rds("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_20152023_04062024.rds")


### teste ##############


#  Realizando a deflação
ibge_ipca <- read_excel("A:\\macro\\IPCA\\cleanData\\ipca_ibge_cl.xlsx")
ibge_ipca <- ibge_ipca %>%
  mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))

#criando a funcao

deflator_automatico <- function(ano_ini, ano_fim, anos, base) {

  # Defina o seu projeto no Google Cloud, é importante criar o projeto e colar o id no "set_billing_id". Fora isso, nao funcionarah
  # Existe um bug no datalake da Base dos Dados que não permite o download direto.
  # set_billing_id("scenic-notch-360215")
  #
  # # criacao do data frame direto da base de dados
  # serie_basedosdados <- basedosdados::bdplyr("br_ibge_ipca.mes_brasil") %>% bd_collect()

  serie_basedosdados <- base

  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>%
    select(ano, mes, variacao_doze_meses) %>%
    filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>%
    arrange(desc(ano))

  indice = 1

  #criacao do data frame para o deflator
  for (l in anos) {
    # chamei novamente a base feita pela funcao do api, pois a base precisa ser percorrida ano a ano e
    # se nao criarmos essa tabela, a tabela a ser percorrida novamente terá sempre o ano inicial como observacao
    tabela <- serie_filtrada

    tabela <- tabela %>%
    filter(ano == l)

    if (l == ano_fim) {
      tabela <- tabela %>%  mutate(deflator = indice)
      tabela_final <- tabela
      indice_ano_anterior = indice * (1+ (tabela$variacao_doze_meses/100))
    } else {
      tabela <- tabela %>% mutate(deflator = indice_ano_anterior)

      tabela_final <- rbind(tabela_final, tabela)
      indice_ano_anterior = indice_ano_anterior * (1 + (tabela$variacao_doze_meses/100))
    }
  }
  tabela_final <- tabela_final %>%
    select(ano, deflator) %>%
    dplyr::rename(year = ano) %>%
    arrange(year)
  return(tabela_final)
}

#aplicando a funcao na base
#essa funcao eh arbitraria, devido as diferentes variáveis que queremos multiplicar e diferentes bases

calculo_deflator <- function(tabela_deflator, base_select_deflator) {

  base_select_deflator <- base_select_deflator %>%
    left_join(tabela_deflator, by= "year")%>%
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator))


  return(base_select_deflator)

}
# eh necessario a escolha dos anos nos quais quer criar os indices. Assim, a funcao toma como base/indice = 1 o ano final

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)

teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
# Dolarizando

coleta_dados_sgs = function(series,datainicial="01/01/2012", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)

    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }

  base$data = as.Date(base$data, "%d/%m/%Y")
  base$ano = as.integer(format(base$data,"%Y"))#Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data

  base <- base %>% select(ano, `3694`)
  base <- base %>% dplyr::rename(c(year = ano, cambio = `3694`))
  return(base)
}
cambio_sgs = coleta_dados_sgs(3694)
tabela_cambio <-cambio_sgs %>%
  filter(year >= 2015 & year <= 2023)



