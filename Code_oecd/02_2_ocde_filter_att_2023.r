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

df_oecd_clear <- readRDS("OECD_DAC_clear_2022.RDS") 

df_oecd_clear <- df_oecd_clear %>%
  dplyr::filter(year >= 2021, recipient == "brazil")

setwd(dir_oecd_project)

sector_reference_table <- read.xlsx("10_oecd_relational_tables_review.xlsx", sheet = "sector_landscape")

sector_reference_table <- sector_reference_table[-c(1,2),]




######### filter ####################

padroes <- c("support to amazon fund", "amazona fund", "amazon fund", "amazona funds", "amazon funds", "fundo amazonia")

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

df_oecd_sector_no_selected <- df_oecd_clear %>% 
  dplyr::filter(!purpose_code %in% sector_reference_table$purpose_code)

df_oecd_sector_reference_table <- left_join(df_oecd_sector_reference_table,sector_reference_table %>%  select(purpose_code, sector_landscape), by = "purpose_code")


"retire what is manual check"

df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table %>% dplyr::filter(sector_landscape == "Manual Check")

df_oecd_sector_reference_table_automatic <- df_oecd_sector_reference_table %>% dplyr::filter(sector_landscape != "Manual Check")


# write.xlsx(df_oecd_sector_reference_table_manual,"oecd_table_manual_check_2022.xlsx")
# 
# write.xlsx(df_oecd_sector_reference_table_automatic,"oecd_table_automatic_check_2022.xlsx")
# "Etapa de análise manual para cada df (manual e automatico)"

#change obs to automatic classify 2021

df_oecd_sector_reference_table_automatic <- df_oecd_sector_reference_table_automatic %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021003024","2021000024","2021000028","2021000040","2021000118c","2021353911","2021009702",
                                                                      "2021007294","2020001108","2021000906","2021000914","2021000604","2021000612",
                                                                      "2021100063","2021100185","2021100231","2021100680","2021605105",
                                                                      "2021960467",
                                                                      "2021960470",
                                                                      "2021951841",
                                                                      "2021951842"), "eliminate", sector_landscape)) %>%
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021002909", "2022953771"), "Cattle", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021178018","2021005112","2021012958","2021004071",
                                                                      "2021005298",
                                                                      "2021001975"), "Forest", sector_landscape)) %>%
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021001429","2021009820","2021006906","2021000019","2021000682","2021000697",
                                                                      "2021000705","2021000610","2021000042","2021000043","2018001302","2021110099","2021110100",
                                                                      "2021100252","2021100491","2021000013","2021004343-1",
                                                                      "2021004403"),"Multi-sector", sector_landscape))

#troca para valores de 2022.

df_oecd_sector_reference_table_automatic <- df_oecd_sector_reference_table_automatic %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021001975", "2022000005", "2022000047", "2022000151", "2022000211", "2022001463-1",
                                                                      "2022001833", "2022002015", "2022005910", "2022006563", "2022008217", "2022016338",
                                                                      "2022025338", "2022025339", "2022025340", "2022085087", "2022110122", "2022160600",
                                                                      "2022312136ct", "2022950268", "2022953768", "2022954570", "2022cirad0024",
                                                                      "2022meae003988", "2022meae003989", "2022meae003990", "2022meae004016"), "Crop", sector_landscape)) %>%
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022000004", "2022000087", "2022000277", "2022000380", "2022000381", 
                                                                      "20220004290001", "20220004290002", "2022006142", "2022015840", 
                                                                      "2022039118", "2022039126", "2022039149", "2022039150", "2022133986", 
                                                                      "2022648115", "2022953770", "2022954793", "2022960240", "2022960241", 
                                                                      "2022961237", "2022961651"),"Forest", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022000259",	"2022149069"),"Multi-sector",sector_landscape)) %>%
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022000008", "2022000009", "2022000029", "2022000079", "2022000082",
                                                                      "2022000083", "2022000084", "2022000095", "2022000102", "2022000108",
                                                                      "2022000129", "2022000145-9", "2022000163", "2022000180", "2022000191",
                                                                      "2022000209", "2022000921", "2022000974", "2022001095-1", "2022001568",
                                                                      "2022001603", "2022002022", "2022025706", "2022025783", "2022025784",
                                                                      "2022025940", "2022025945", "2022027049a", "2022027717", "2022037164",
                                                                      "2022110120", "2022110155", "2022110157", "2022110160", "2022110161",
                                                                      "2022605135", "2022951869", "2022951870", "2022952644", "2022953764",
                                                                      "2022957436", "2022960233", "2022999057", "2022999064", "2022stoa0004"),"eliminate",sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022000008", "2022000009", "2022000029", "2022000079", "2022000082", "2022000083",
                                                                      "2022000084", "2022000095", "2022000102", "2022000108", "2022000129", "2022000145-9",
                                                                      "2022000163", "2022000180", "2022000191", "2022000209", "2022000921", "2022000974",
                                                                      "2022001095-1", "2022001568", "2022001603", "2022002022", "2022007966", "2022009634",
                                                                      "2022011143", "2022011688", "2022025706", "2022025783", "2022025784", "2022025940",
                                                                      "2022025945", "2022027049a", "2022027717", "2022037164", "2022110120", "2022110155",
                                                                      "2022110157", "2022110160", "2022110161", "2022605135", "2022605139", "2022605140",
                                                                      "2022951869", "2022951870", "2022952644", "2022953764", "2022957436", "2022960233",
                                                                      "2022999057", "2022999064", "2022stoa0004"),"eliminate", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022953771"), "Cattle", sector_landscape))
  
  

df_oecd_sector_reference_table_automatic <- df_oecd_sector_reference_table_automatic %>% filter(!sector_landscape == "eliminate")

#changes obs reviewed manual classify 2021

df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table_manual %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000123","2021000606","2021000609","2021005653","2021012955"), "eliminate", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% "2021000780", "Bioenergy and fuels", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000048","2021000490",	"2021004051-1"), "Cattle", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021004049-1","2021023022","2021960480"), "Crop", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2018001585",	"2018001586",	"2020000042",	"2021000032",	"2021000033",	"2021000035",	"2021000036",	"2021000037",
                                                                      "2021000039",	"2021000042",	"2021000044",	"2021000045",	"2021000046", "2021000047",	"2021000048",	"2021000050",	"2021000169",	
                                                                      "2021000809" ,"2021000888","2021001134",	"2021001329",	"2021001782",	"2021002814",	"2021002815",	"2021002914",	"2021003823",	
                                                                      "2021004603",	"2021027587",	"2021638010",	"2021668038",	"2021668042"), "Forest", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000047",	"2021000116",	"2021000181",	"2021000188",	"2021000313",	"2021000790",
                                                                      "2021000814",	"2021000901", "2021000961",	"2021001063",	"2021001238",	"2021002575",	
                                                                      "2021003024",	"2021003088",	"2021003308",	"2021003309",	"2021003310",	"2021003311",
                                                                      "2021003312",	"2021003313",	"2021003314",	"2021003315",	"2021003358",	"2021005081",
                                                                      "2021005580",	"2021012456",	"2021012958",	"2021013599",	"2021024458",	"2021100189"), "Multi-sector", sector_landscape))

#troca para valores de 2022.
df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table_manual %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2022000049"),"Cattle",sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2019004242a", "2022000922", "2022006659","2022029816"), "Crop", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2020001103", "2021000035", "2022000004", "2022000005", "20220000360001", "2022000039",
                                                                      "2022000040", "2022000041", "2022000042", "2022000044", "2022000046", "2022000047",
                                                                      "2022000050", "2022000051", "2022000052", "2022000064", "2022000154", "20220004290001",
                                                                      "20220004290002", "2022000740", "2022000794", "2022001242", "2022001358", "2022002178",
                                                                      "2022002179", "2022003950", "2022004030", "2022006699", "2022006700", "2022638013",
                                                                      "2022ird00083", "2022meae005209", "2022meae005210", "2022meae005212", "2022meae005214"),"Forest",sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000901", "2022000380", "2022001738-1", "2022026001", "2021003308", "2021003312",
                                                                      "2021003313", "2021003314", "2021003315", "2022000043", "2022000048", "2022000061",
                                                                      "2022000167", "2022000882", "2022001640", "2022001641", "2022001776", "2022001920",
                                                                      "2022007038", "2022009255", "2022060222a", "2022060222c", "2022060222h", "2022060222i"),"Multi-sector", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2019000053", "2022000005", "2022000030", "2022000035", "2022000044", "2022000060",
                                                                      "2022000064", "2022000162", "2022000168", "2022000268", "2022000412", "2022001362",
                                                                      "2022001493", "2022001557", "2022001590", "2022001591", "2022001611", "2022001639",
                                                                      "2022005880-1", "2022006562", "2022006696", "2022006852", "2022009252", "2022021454",
                                                                      "2022023643", "2022023851", "2022025205", "2022025335", "2022025341", "2022025461",
                                                                      "2022025649", "2022026289", "2022027049b", "2022027304b", "2022085226", "2022110857",
                                                                      "2022139886", "2022648110", "2022950287", "2022953775", "2022960247", "2022961652",
                                                                      "2022ird00068", "2022ird00072", "2022ird00074", "2022ird00078", "2022ird00081",
                                                                      "2022meae003985","2021000024"),"eliminate",sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000780"), "Bioenergy and fuels", sector_landscape))

df_oecd_sector_reference_table_manual <- df_oecd_sector_reference_table_manual %>% filter(!sector_landscape == "eliminate")


#include some observations that not include in automatic and manual check (this observations are outside of manual and automatic classify)

df_reviewed_include <- df_oecd_clear %>% dplyr::filter(crs_identification_n %in% c("2021012456","2021000083","2021100200","2021100397","2021605106",
                                                                              "2021960477","2021000049", "2021003062-1","2021001749-1","2021000682-1","2021003899","2022000162",
                                                                              "2022008045-1", "2022311931ct", "2021000049", "2022000038", 
                                                                              "2022000272","2022001765", "2022008537-1", "2022001732-1", 
                                                                              "2022004558-1")) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021012456", "2022001732-1","2022004558-1"), "Multi-sector", "-")) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021960477","2022000162"), "Cattle", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021100200","2021100397","2021605106","2021003062-1","2021000682-1","2022008045-1","2022311931ct"), "Crop", sector_landscape)) %>% 
  dplyr::mutate(sector_landscape = ifelse(crs_identification_n %in% c("2021000083","2021000049","2021001749-1","2021003899",
                                                                      "2021000049","2022000038","2022000272","2022001765",
                                                                      "2022008537-1"), "Forest", sector_landscape))

"combinando as planilhas"

df_oecd_join_checked <- rbind(df_oecd_sector_reference_table_manual, df_oecd_sector_reference_table_automatic)

df_oecd_join_checked_reviewed <- rbind(df_oecd_join_checked, df_reviewed_include) %>% dplyr::filter(!sector_landscape %in% "Manual Check")

df_no_Selected <- df_oecd_join_checked %>% filter(sector_landscape  == "Manual Check")

"save"

setwd(dir_oecd_output)


write.xlsx(df_oecd_join_checked_reviewed,"df_oecd_final_filter_10062024.xlsx")

saveRDS(df_oecd_join_checked_reviewed, "df_oecd_final_filter_10062024.rds")

write.xlsx(df_oecd_sector_no_selected, "df_oecd_resto.xlsx")
# 
# write.xlsx(df_no_Selected,"df_oecd_filter_no_selected.xlsx")
# write.xlsx(df_no_Selected,"df_oecd_filter_no_selected.xlsx")
