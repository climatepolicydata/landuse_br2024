##################
# Author : Renan Morais
# Date: 07/12/2023
# Email: renanflorias@hotmail.com
# Goal: filter ocde database att
# resource: OECD Miltilateral Climate Funds
#source: https://docs.ropensci.org/cld3/


########################## Libraries ######################################
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

dir_oecd_Cf_clear <- ("A:/finance/oecd_Cf/cleanData")

dir_oecd_project <- ("A:/projects/landuse_br2024/oecd_cf")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")


###### import datasets  & Select Variables #########

setwd(dir_oecd_Cf_clear)

df_oecd_clear <- readRDS("OECD_DAC_clear_2021.rds") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2021 & year <= 2023, recipient == "brazil")

setwd(dir_oecd_project)

sector_reference_table <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "sector_landscape_review")

sector_reference_table <- sector_reference_table[-c(1,2),]

# 
# sector_classify <- read.xlsx("10_oecd_relational_tables.xlsx", sheet = "sector_classification") %>%
#   select(sector_key, sector) %>%
#   unique() %>% filter(sector == "Agriculture, Forestry, Other land uses and Fisheries")
# 
# sector_landscape <- read.csv("ref_sector.csv") %>% 
#   dplyr::filter(sector_key %in% sector_classify$sector_key) %>% filter(reference_data == "OECD")



df_oecd_clear <- df_oecd_clear %>% 
  mutate(text_language = detect_language(description))


######### filter ####################

padroes <- c("support to amazon fund", "amazona fund", "amazon fund", "amazona funds", "amazon funds")

#filter amazon fund projects
fmz_project <- df_oecd_clear %>% 
  dplyr::filter((
    str_detect(description, padroes[1]) |
      str_detect(description, padroes[2]) |
      str_detect(description, padroes[3]) |
      str_detect(description, padroes[4]) |
      str_detect(description, padroes[5])))





df_fmz_project_reviewed <- df_oecd_clear %>% dplyr::filter(!crs_identification_n %in% fmz_project$crs_identification_n)

"NÃO FOI IDENTIFICADO PROJETOS DO FUNDO AMAZONIA NOS DADOS DA OECD PARA 2021 - 2022 - 2023"

"How can I do the filter do landuse?"


df_oecd_sector_reference_table <- df_oecd_clear %>% 
  dplyr::filter(purpose_code %in% sector_reference_table$purpose_code)

df_oecd_sector_reference_table <- left_join(df_oecd_sector_reference_table,sector_reference_table %>%  select(purpose_code, sector_landscape), by = "purpose_code")


"retire what is manual check"

df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table %>% filter(sector_landscape == "Manual Check")

df_oecd_sector_reference_table_automatic <- df_oecd_sector_reference_table %>% filter(sector_landscape != "Manual Check")


"Etapa de análise manual para cada df (manual e automatico)"

"after analisys"

df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table_manual %>% 
  mutate(sector_landscape = ifelse(crs_identification_n %in% c(2021003024,	2021004051-1,	2021012456,	2021023022), "Crop", 
                                   ifelse(crs_identification_n %in% c(2018001585, 2018001586, 2020000042, 2021000032, 2021000033, 2021000035, 2021000036, 2021000037, 
                                                                    2021000039, 2021000042, 2021000044, 2021000045, 2021000047, 2021000048, 2021000050, 2021000490, 
                                                                    2021000809, 2021000814, 2021000888, 2021001134, 2021001238, 2021001329, 2021001782, 2021002575, 
                                                                    2021002814, 2021002815, 2021003823, 2021005580, 2021027587, 2021638010, 2021668038, 2021668042), "Forest",
                                          ifelse(crs_identification_n %in% c(2021000046, 2021000169, 2021000188, 2021000780, 2021000901, 2021000961, 2021001063, 2021002914, 2021003308, 2021003309, 
                                                 2021003310, 2021003311, 2021003312, 2021003313, 2021003314, 2021003315, 2021004049-1, 2021004603, 2021005081, 2021013599, 2021100189),"Multi-sector", sector_landscape))))

setwd(dir_oecd_output)

write.xlsx(df_oecd_sector_reference_table_manual, "data_oecd_analise_manual.xlsx")

write.xlsx(df_oecd_sector_reference_table_automatic, "data_oecd_analise_automatic.xlsx")


"Reimportando a planilha com a análise manual feita."


df_oecd_manual_checked <- read.xlsx("data_oecd_analise_manual_checked_040424.xlsx")

df_oecd_automatic_checked <- read.xlsx("data_oecd_analise_automatic_checked_040424.xlsx")


"combinando as planilhas"

df_oecd_join_checked <- rbind(df_oecd_manual_checked, df_oecd_automatic_checked)


setwd(dir_oecd_output)

write.xlsx(df_oecd_join_checked,"df_oecd_final_filter.xlsx")

saveRDS(df_oecd_join_checked, "df_oecd_final_filte.rds")
