##################

# Author : Renan Morais
# Date: 11-04-2024
# Email: renanflorias@hotmail.com
# Goal: filter bndes indiretas automaticas
# resource: 

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
               cld3)

##### directory #########

dir_bndes_aut_raw <- ("A:/finance/bndes_Aut/rawData")

dir_bndes_aut_clear <- ("A:/finance/bndes_Aut/CleanData")


dir_project_bndes_output <- ("A:/projects/landuse_br2024/BNDES_Aut/output")


dir_bndes_aut_doc <- ("A:/projects/landuse_br2024/BNDES_Aut")


##### import datasets #########

setwd(dir_bndes_aut_clear)

df_bndes_aut <- readRDS("operacoes_financiamento_operacoes_indiretas_automatica_clear.RDS")



setwd(dir_bndes_aut_doc)

#bndes automatico e bndes nao automatico utilizam o mesmo filtro para o subsetor cnae nome
sector_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "sector_landscape")
instrument_bndes_aut <- read.xlsx("07_bndes_aut_relational_tables.xlsx", sheet = "instrument_landscape") %>% 
  dplyr::mutate(instrument_original = tolower(instrument_original))


###### filter year #####

df_bndes_aut_filter <- df_bndes_aut %>%
  filter(ano >= 2015 & ano <= 2023)

##### select variables ########
remove <- c("valor_desembolsado_reais", "custo_financeiro", "juros", "prazo_carencia_meses",
            "prazo_amortizacao_meses","inovacao", "area_operacional", 
            "setor_cnae", "setor_bndes")


df_filter <- df_bndes_aut_filter %>% 
  select(!all_of(remove))
rm(df_bndes_aut_filter, remove)

########### filter ######
lista_instrument <- instrument_bndes_aut$instrument_original


lista_sector <- sector_bndes_aut$subsetor_cnae_nome

df_filter <- df_filter %>% filter(instrumento_financeiro %in% lista_instrument & subsetor_cnae_nome %in% lista_sector)

########### save data #####

setwd(dir_project_bndes_output)

saveRDS(df_filter,"df_bndes_aut_filter_pre_transf.rds")

write.xlsx(df_filter, "df_bndes_aut_filter_pre_transf.xlsx")


