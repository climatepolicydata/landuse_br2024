##################

# Author : Renan Morais
# Date: 16-05-2024
# Email: renanflorias@hotmail.com
# Goal: filter giz
# resource: 

########################### Libraries ######################################


pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

##### directory #########

dir_giz_clear <- ("A:/finance/giz/cleanData")

dir_oecd_clear <- ("A:/finance/oecd_cf/cleanData")

dir_giz_output <- ("A:\\projects\\landuse_br2024\\internacionais\\giz\\output\\")

####### import database ########
setwd(dir_giz_clear)

giz_clear <- readRDS("giz_clear.rds")

setwd(dir_oecd_clear)
df_oecd_clear <- readRDS("OECD_DAC_clear_2021.rds")

giz_clear<- giz_clear %>% select(iati_identifier, title, export_select_description_objectives, activity_date, export_select_participating_org_funding_de_1,
                                 sector, outgoing_commitment,export_select_disbursement,co_financing_contributions, export_select_transaction_incoming_funds,
                                 export_select_participating_org_implementing, export_select_sectors, year)


#####filter years #######

df_oecd_clear <- df_oecd_clear %>% dplyr::filter(year >= 2020)
giz_clear <- giz_clear %>% dplyr::filter(year >= 2020 & year <= 2023)


# Adicionar a coluna "years" ao dataframe raw_giz
giz_clear <- giz_clear %>%
  dplyr::mutate(iati_identifier_modify = substr(iati_identifier, start = 6, stop = nchar(iati_identifier)))


#### filter to select projects there aren't in oecd ######

search_vetor <- paste(giz_clear$iati_identifier_modify,collapse="|")

data_filter <- grepl(search_vetor, df_oecd_clear$donor_project_n)


data_both <- df_oecd_clear[data_filter,]
data_both2 <- giz_clear[data_filter,]



data_anti_join_giz <- anti_join(giz_clear, data_both) %>% 
  dplyr::filter(year > 2022) %>% 
  dplyr::filter(sector %in% c("agriculture", "fisheries", "environmental protection in general", "state and civil society in general","water and sewage/waste disposal"))


data_anti_join_giz <- data_anti_join_giz %>% 
  dplyr::mutate(sector_landscape = ifelse(iati_identifier %in%  c("de-1-202232858-0", "de-1-202276814-0", "de-1-202330264-0", 
                                                                  "de-1-202330272-0", "de-1-202330280-0", "de-1-202330595-0", 
                                                                  "de-1-202330645-0", "de-1-202330652-0", "de-1-202330892-0", 
                                                                  "de-1-202331536-0", "de-1-202331544-0", "de-1-202332005-0", 
                                                                  "de-1-202332013-0", "de-1-202332047-0", "de-1-202332245-0", 
                                                                  "de-1-202332252-0", "de-1-202332666-0", "de-1-202332674-0", 
                                                                  "de-1-202332682-0", "de-1-202333383-0", "de-1-202333623-0", 
                                                                  "de-1-202333821-0", "de-1-202333839-0"), "Crop", "-")) %>% 
  dplyr::mutate(sector_landscape = ifelse(iati_identifier %in% c("de-1-201968239", "de-1-201969252", "de-1-201969369", 
                                                                 "de-1-202306009-6822", "de-1-202306009-7352", "de-1-202330546-0",
                                                                 "de-1-202330553-0", "de-1-202330884-0", "de-1-202331502-0",
                                                                 "de-1-202331528-0", "de-1-202331965-0", "de-1-202331999-0", 
                                                                 "de-1-202332526-0", "de-1-202333631-0", "de-1-202339042-0"
  ),"Forest", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(iati_identifier %in% c("de-1-202306009-7336",	"de-1-202330256-0",	"de-1-202330579-0",	"de-1-202330918-0"),
                                          "Multi-sector", sector_landscape)) %>% 
  dplyr::filter(!sector_landscape == "-")

######## save database #########

"manual check"
setwd(dir_giz_output)

write.xlsx(data_anti_join_giz,"giz_manual_check_sector_23052024.xlsx")
