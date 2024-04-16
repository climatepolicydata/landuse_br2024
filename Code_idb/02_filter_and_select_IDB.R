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

df_oecd_clear <- readRDS("OECD_DAC_clear_2021.rds") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2015 & year <= 2023, recipient == "brazil")

setwd(dir_idb_clear)

df_idb_clear <- readRDS("report_projects_clear.rds") %>% 
  dplyr::filter(project_country == "brazil" & (ano >= 2015 & ano <= 2024))


#### filter to select projects there aren't in oecd ######
df_idb_clear$project_number
vetor_procura <- paste(df_idb_clear$project_number,collapse="|")


data_filter <- grepl(vetor_procura, df_oecd_clear$donor_project_n)
data_both <- df_oecd_clear[data_filter,]
data_both2 <- df_idb_clear[data_filter,]

data_both2 <- data_both2[1:26,]



data_anti_join_idb <- anti_join(data_both, df_idb_clear)

data_anti_join_both <- anti_join(df_idb_clear, data_both2, by="project_number") %>% filter(ano >= 2021 & )


setwd(dir_idb_output)

write.xlsx(data_anti_join_both, "data_idb_analise_manual.xlsx")



