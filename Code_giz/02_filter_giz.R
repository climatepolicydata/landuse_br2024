##################

# Author : Renan Morais
# Date: 28-08-2023
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
  dplyr::filter(sector %in% c("agriculture", "fisheries", "environmental protection in general", "state and civil society in general"))



######## save database #########

"manual check"
setwd(dir_giz_output)

write.xlsx(data_anti_join_giz,"giz_manual_check_sector.xlsx")
