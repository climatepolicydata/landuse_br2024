##################

# Author : Renan Morais
# Date: 09-05-2024
# Email: renanflorias@hotmail.com
# Goal: transform idb database in format landscape


########## libraries ############
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

dir_idb_clear <- ("A:/finance/idb/cleanData")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb\\output")


###### import datasets #########

setwd(dir_idb_output)

df_idb_filter <- readRDS("df_idb_filter.rds")


########## transforms ######

df_idb_filter_transform <- df_idb_filter %>% 
  dplyr::rename(id_original = operation_number,
                year = ano,
                source_original = executing_agency,
                channel_original = funding_source,
                value_original_currency = total_cost,
                original_currency = currency,
                instrument_original = project_type,
                sector_original = sector,
                subsector_original = sub_sector,
                localization_original = project_country) %>% 
  dplyr::mutate(data_source = "idb_projects")
