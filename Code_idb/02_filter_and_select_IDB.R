##################

# Author : Renan Morais
# Date: 09-05-2024
# Email: renanflorias@hotmail.com
# Goal: get database from server cpi"


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


###### import datasets  & Select Variables #########

setwd(dir_oecd_Cf_clear)

df_oecd_clear <- readRDS("OECD_DAC_clear_2022.rds") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2015 & year <= 2023, recipient == "brazil")


setwd(dir_idb_clear)

df_idb_clear <- readRDS("report_projects_clear.rds") %>% 
  dplyr::filter(project_country == "brazil" & (ano >= 2015 & ano <= 2024))

df_idb_clear <- readRDS("report_projects_clear.rds") %>% 
  dplyr::filter(project_country == "brazil" & (ano >= 2021 & ano <= 2023))

setwd(dir_idb_output)

write.xlsx(df_idb_clear,"idb_brasil_21_23.xlsx")


#### filter to select projects there aren't in oecd ######
df_idb_clear$project_number
vetor_procura <- paste(df_idb_clear$project_number,collapse="|")


data_filter <- grepl(vetor_procura, df_oecd_clear$donor_project_n)

# Filtrar df_oecd_clear com data_filter para obter os valores que deram TRUE
matching_projects <- df_oecd_clear[data_filter, "donor_project_n"]

# Encontrar as linhas correspondentes em df_idb_clear usando semi_join
match_project_numbers <- function(donor_project_n) {
  any(grepl(df_idb_clear$project_number, donor_project_n))
}

# Filtrar df_idb_clear para obter as linhas que correspondem
result <- df_idb_clear %>%
  rowwise() %>%
  filter(any(grepl(project_number, matching_projects)))

# Verificar o resultado
print(result)

data_anti_join_both <- anti_join(df_idb_clear, result, by="project_number") %>% dplyr::filter(ano >= 2021 & (sector == "water and sanitation" | sector == "environment and natural disasters"|
                                                                                             sector == "other" | sector == "agriculture and rural development" |
                                                                                             sector == "energy" | sector == "sustainable tourism"))





############# codes to each sector validate in manual analisys ################
"5440/oc-br  5612/oc-br atn/oc-18781-br" #this observations are in ocde database and including only contable.

codes_crop <- c("5611/oc-br",	"atn/oc-18644-br",	"equ/ms-20143-br",	"equ/tc-20142-br",	"sp/oc-23-51-br")

codes_forest <- c("atn/az-19413-br",	"atn/az-20334-br",	"atn/gn-20510-br",	"atn/jf-20520-br",	"atn/oc-19412-br",	"atn/sx-19186-br")

codes_multi <- c("atn/az-20411-br","5836/oc-br",	"atn/lc-18953-br",	"atn/mc-20445-br",	"atn/oc-19258-br",	"atn/oc-19745-br",	"atn/oc-20410-br",	"atn/oc-20570-br")



data_filter_sectors <- data_anti_join_both %>% dplyr::mutate(sector_landscape = ifelse(operation_number %in% codes_crop, "Crop",
                                                                                ifelse(operation_number %in% codes_forest, "Forest",
                                                                                       ifelse(operation_number %in% codes_multi, "Multi-sector", "Null")))) %>% 
  dplyr::filter(sector_landscape %in% c("Crop","Forest","Multi-sector"))


"this obs there is in oecd database too"
"a observação aparece oriunda de outra instituição, mas relacionada com o idb. O número do projeto não tem relação, entretanto os valores e o nome do projeto são iguais."

data_filter_sectors <- data_filter_sectors %>% dplyr::filter(!project_name == "forest management information for the conservation and valorization of forest resources in brazil")



teste <- anti_join(data_anti_join_both,data_filter_sectors)
################### save filtered data ########

setwd(dir_idb_output)


saveRDS(data_filter_sectors,"df_idb_filter_30092024.rds")

write.xlsx(data_filter_sectors,"df_idb_filter_30092024.xlsx")








