##################

# Author : Renan Morais
# Date: 23-05-2024
# Email: renanflorias@hotmail.com
# Goal: transform giz database in format landscape


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

dir_giz_output <- ("A:\\projects\\landuse_br2024\\internacionais\\giz\\output\\")

dir_giz_doc <- ("A:\\projects\\landuse_br2024\\internacionais\\giz")
###### import datasets #########

setwd(dir_giz_output)

df_idb_clear<- readRDS("giz_clear.rds")
