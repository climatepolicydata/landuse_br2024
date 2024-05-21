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

data_anti_join_both <- anti_join(df_idb_clear, data_both2, by="project_number") %>% dplyr::filter(ano >= 2021 & (sector == "water and sanitation" | sector == "environment and natural disasters"|
                                                                                             sector == "other" | sector == "agriculture and rural development" |
                                                                                             sector == "energy" | sector == "sustainable tourism"))


# setwd(dir_idb_output)
# 
# write.xlsx(data_anti_join_both, "data_idb_analise_manual.xlsx")


##### classify by key words ############

setwd("A:\\projects\\landuse_br2024\\key_words_en_version")

# key_words_list_bioenergy <- read.xlsx("list_key_words_all_sector.xlsx") %>% janitor::clean_names() %>% dplyr::mutate(bioenergy_and_fuels = str_trim(bioenergy_and_fuels))
# 
# key_words_bioenergy <- ("(?=.*\\bCorn\\b)(?=.*\\bethanol\\b)|(?=.*\\bEthanol\\b)|(?=.*\\bbioenergy\\b)|(?=.*\\bRenovabio\\b)|(?=.*\\bIndustrialization\\b)(?=.*\\bsugarcane\\b) |
# (?=.*\\bIncrease\\b)(?=.*\\befficiency\\b)(?=.*\\bethanol\\b)(?=.*\\bproduction\\b) |
# (?=.*\\bPlant\\b)(?=.*\\bcogeneration\\b)|
# (?=.*\\bBagasse\\b)(?=.*\\bsugarcane\\b) |
# (?=.*\\bBiofuels\\b) |
# (?=.*\\bProduction\\b)(?=.*\\bbiofuel\\b)(?=.*\\bethanol\\b) |
# (?=.*\\bCogeneration\\b)(?=.*\\benergy\\b) |
# (?=.*\\bAcquisition\\b)(?=.*\\binputs\\b)(?=.*\\bethanol\\b)(?=.*\\bproduction\\b)|
# (?=.*\\bCogeneration\\b)(?=.*\\brenewable\\b)(?=.*\\benergy\\b) |
# (?=.*\\bProduction\\b)(?=.*\\bbiofuel\\b)(?=.*\\bethanol\\b) |
# (?=.*\\bCogeneration\\b)(?=.*\\benergy\\b)(?=.*\\bbiomass\\b)(?=.*\\bsugarcane\\b) |
# (?=.*\\bSolar\\b)(?=.*\\bcredit\\b) |
# (?=.*\\bSolar\\b)(?=.*\\benergy\\b)(?=.*\\bprojects\\b) |
# (?=.*\\bEthanol\\b)(?=.*\\bcorn\\b) |
# (?=.*\\bSolar\\b)(?=.*\\bwind\\b)(?=.*\\bsmall\\b)(?=.*\\bhydroelectric\\b)(?=.*\\bpower\\b) |
# (?=.*\\bBiogas\\b) |
# (?=.*\\bSteam\\b)(?=.*\\bgeneration\\b) |
# (?=.*\\bCogeneration\\b)(?=.*\\belectrical\\b)(?=.*\\benergy\\b)(?=.*\\bbiomass\\b) |
# (?=.*\\bFinancing\\b)(?=.*\\bproduction\\b)(?=.*\\bethanol\\b) |
# (?=.*\\bProduction\\b)(?=.*\\bethanol\\b) |
# (?=.*\\bRaízen\\b) |
# (?=.*\\bNational\\b)(?=.*\\bbiofuels\\b)(?=.*\\bpolicy\\b) |
# (?=.*\\bNational\\b)(?=.*\\bbiofuel\\b)(?=.*\\bpolicy\\b) |
# (?=.*\\bEnergy\\b)(?=.*\\bbiofuels\\b) |
# (?=.*\\bEnergy\\b)(?=.*\\bbiofuel\\b) |
# (?=.*\\bPetroleum\\b)(?=.*\\bgas\\b)(?=.*\\bderivatives\\b)(?=.*\\bbiofuels\\b) |
# (?=.*\\bBiofuels\\b)(?=.*\\bMinistry\\b)(?=.*\\bof\\b)(?=.*\\bMines\\b)(?=.*\\band\\b)(?=.*\\bEnergy\\b) |
# (?=.*\\bBiofuel\\b)(?=.*\\bMinistry\\b)(?=.*\\bof\\b)(?=.*\\bMines\\b)(?=.*\\band\\b)(?=.*\\bEnergy\\b) |
# (?=.*\\bAgroenergy\\b) |
# (?=.*\\bAgroenergetics\\b) |
# (?=.*\\bBiofuels\\b)(?=.*\\bproduction\\b)(?=.*\\bchains\\b) |
# (?=.*\\bBiofuels\\b)(?=.*\\bindustry\\b) |
# (?=.*\\bBiofuel\\b) |
# (?=.*\\bBiofuels\\b) |
# (?=.*\\bAgroenergy\\b)(?=.*\\bdevelopment\\b) |
# (?=.*\\bManufacture\\b)(?=.*\\balcohol\\b) |
# (?=.*\\bManufacture\\b)(?=.*\\bbiofuels\\b) |
# (?=.*\\bCo\\b)(?=.*\\bgeneration\\b) |
# (?=.*\\bGeneration\\b)(?=.*\\belectricity\\b)(?=.*\\balternative\\b)(?=.*\\bsources\\b) |
# (?=.*\\bGeneration\\b)(?=.*\\belectricity\\b)(?=.*\\bthermal\\b) |
# (?=.*\\bProduction\\b)(?=.*\\bdistribution\\b)(?=.*\\bvapor\\b) |
# (?=.*\\bBiomass\\b)(?=.*\\bsugarcane\\b) |
# (?=.*\\bSteam\\b)(?=.*\\bgeneration\\b) |
# (?=.*\\bEnergy\\b)(?=.*\\bbagasse\\b)(?=.*\\bsugarcane\\b) |
# (?=.*\\bCorn\\b)(?=.*\\bethanol\\b) |
# (?=.*\\bThermoelectric\\b)(?=.*\\bbiogas\\b)")
# 
# key_words_forest <- ("(?=.*\bjardim botânico\b)(?=.*\bzoo\b)(?=.*\bnational park\b)(?=.*\becological reserve\b)(?=.*\benvironmental protection\b)|(?=.*\bexpansion\b)(?=.*\bconsolidation\b)(?=.*\bnational system of conservation units\b)|(?=.*\\bdeforestation\\b)|(?=.*\benvironmental management\b)(?=.*\bstandardization and monitoring\b)|(?=.*\bconservation units\b)|
# (?=.*\bland structure\b)(?=.*\bagrarian reform\b) |
# (?=.*\bagrarian organization\b)(?=.*\bterritorial planning\b) |
# (?=.*\bforest research organizations\b) |
# (?=.*\benvironmental management\b)(?=.*\bdissemination of scientific and technological knowledge\b) |
# (?=.*\bBrazilian Forest Service\b)(?=.*\bSFB\b) |
# (?=.*\bSFB\b) |
# (?=.*\benvironmental recovery\b) |
# (?=.*\benvironmental management\b)(?=.*\brecovery of degraded areas\b) |
# (?=.*\bradioactive mining-industrial decommissioning\b)(?=.*\bdegraded areas\b) |
# (?=.*\bdegraded area recovery\b) |
# (?=.*\bscience and technology\b)(?=.*\bdegraded area recovery\b) |
# (?=.*\benergy\b)(?=.*\bdegraded area recovery\b) |
# (?=.*\bnational forest inventory\b) |
# (?=.*\bBrazilian flora\b) |
# (?=.*\benvironmental management\b)(?=.*\bscientific development\b) |
# (?=.*\bRio de Janeiro Botanical Garden\b)(?=.*\bJBRJ\b) |
# (?=.*\bbotanical garden\b) |
# (?=.*\benvironmental management\b)(?=.*\benvironmental preservation and conservation\b) |
# (?=.*\bChico Mendes Institute for Biodiversity Conservation\b) |
# (?=.*\bspecies conservation research\b) |
# (?=.*\benvironmental regularization\b)(?=.*\breal estate\b)(?=.*\bstates\b) |
# (?=.*\benvironmental management\b)(?=.*\bgeneral administration\b) |
# (?=.*\benvironmental regularization\b)(?=.*\birrigation projects\b) |
# (?=.*\bagriculture\b)(?=.*\birrigation\b) |
# (?=.*\bcultural preservation\b)(?=.*\bindigenous peoples\b) |
# (?=.*\bcommunity\b)(?=.*\bfamily forest management\b) |
# (?=.*\bconservation and sustainable use of biodiversity and natural resources\b) |
# (?=.*\bforest production registration\b) |
# (?=.*\brural cadastre management\b)(?=.*\binspection\b) |
# (?=.*\benvironmental policy management\b) |
# (?=.*\bwatershed preservation\b)(?=.*\bcultural preservation\b) |
# (?=.*\bplanted forests\b)(?=.*\brubber cultivation\b) |
# (?=.*\bplanted forests\b) |
# (?=.*\brubber cultivation\b) |
# (?=.*\bsocio-productive inclusion\b) |
# (?=.*\bterritorial\b)(?=.*\bfarmer socio-environmental management\b) |
# (?=.*\bforest information\b) |
# (?=.*\bforest concessions\b) |
# (?=.*\bPNMA\b) |
# (?=.*\bsustainable use\b)(?=.*\bbiodiversity\b) |
# (?=.*\bhazard control\b)(?=.*\bIBAMA\b) |
# (?=.*\bprevention\b)(?=.*\bpreparedness\b)(?=.*\bresponse\b)(?=.*\benvironmental damage\b) |
# (?=.*\brural settlements development\b) |
# (?=.*\bforest research and development\b) |
# (?=.*\bflooded forest research and development\b) |
# (?=.*\bnational biodiversity policy\b) |
# (?=.*\becotoxicological assessment\b)(?=.*\bIBAMA\b) |
# (?=.*\benvironmental quality\b)(?=.*\bIBAMA\b) |
# (?=.*\bSIPAM\b) |
# (?=.*\bAmazon protection system\b) |
# (?=.*\bland structure regularization\b) |
# (?=.*\bland georeferencing\b)(?=.*\bdigitalization\b) |
# (?=.*\benvironmental management\b)(?=.*\bethnodevelopment\b) |
# (?=.*\bforest management\b) |
# (?=.*\bdeforestation prevention\b)(?=.*\bcontrol\b) |
# (?=.*\benvironmental conservation\b)(?=.*\bbuffer zones\b)(?=.*\bconcessions\b)(?=.*\blicensing\b)(?=.*\benvironmental protection\b) |
# (?=.*\bhazard assessment\b)(?=.*\bcontrol\b)(?=.*\bproduct assessment\b)(?=.*\bIBAMA\b) |
# (?=.*\bnative vegetation cover recovery\b) |
# (?=.*\becological-economic zoning\b) |
# (?=.*\bFUNAI unit administration\b) |
# (?=.*\bIBAMA unit administration\b) |
# (?=.*\bPrevFogo\b) |
# (?=.*\bforest fires\b) |
# (?=.*\bforest fire fighting\b) |
# (?=.*\bMinistry of the Environment maintenance\b) |
# (?=.*\brural cadastre\b) |
# (?=.*\bsoil\b)(?=.*\bwater conservation\b) |
# (?=.*\bsustainable forest development\b) |
# (?=.*\bnative\b)(?=.*\bagroforestry silviculture expansion\b) |
# (?=.*\bsociobiodiversity\b) |
# (?=.*\bINPE\b) |
# (?=.*\bcertified sustainable forest management\b)|(?=.*\\bforests\\b)(?=.*\\bamazon\\b)|
# (?=.*\bsilviculture\b) |
# (?=.*\breforestation\b)")

############# codes to each sector validate in manual analisys ################

codes_crop <- c("5440/oc-br",	"5611/oc-br",	"5612/oc-br",	"atn/oc-18644-br",	"atn/oc-18781-br",	"equ/ms-20143-br",	"equ/tc-20142-br",	"sp/oc-23-51-br")

codes_forest <- c("5836/oc-br",	"atn/az-19413-br",	"atn/az-20334-br",	"atn/gn-20510-br",	"atn/jf-20520-br",	"atn/oc-19412-br",	"atn/sx-19186-br")

codes_multi <- c("atn/az-20411-br",	"atn/lc-18953-br",	"atn/mc-20445-br",	"atn/oc-19258-br",	"atn/oc-19745-br",	"atn/oc-20410-br",	"atn/oc-20570-br")



data_filter_sectors <- data_anti_join_both %>% dplyr::mutate(sector_landscape = ifelse(operation_number %in% codes_crop, "Crop",
                                                                                ifelse(operation_number %in% codes_forest, "Forest",
                                                                                       ifelse(operation_number %in% codes_multi, "Multi-sector", "Null")))) %>% 
  dplyr::filter(sector_landscape %in% c("Crop","Forest","Multi-sector"))


################### save filtered data ########

setwd(dir_idb_output)


saveRDS(data_filter_sectors,"df_idb_filter.rds")

write.xlsx(data_filter_sectors,"df_idb_filter.xlsx")








