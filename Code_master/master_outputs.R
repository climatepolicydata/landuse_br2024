##############################################################################
# Author : Renan Florias
# Date: 02.10.2023
# Email: renanflorias@hotmail.com
# Goal: Masterfile to outputs landscape

########################### Libraries ######################################
install.packages('xlsx')
library(xlsx)

######################### Directories and databases #########################

atlas_output <- ("A:/projects/landuse_br2024/atlas/output")

sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

dir_b3_output <-("A:/projects/landuse_br2024/b3/output")

dir_nint_output <- ("A:\\projects\\landuse_br2024\\nint\\output")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb\\output")

fnmc_output <- ("A:/projects/landuse_br2024/gov_fnmc")

dir_bndes_output <- ("A:/projects/landuse_br2024/bndes_n_aut/output")

dir_susep_output <- ("A:/projects/landuse_br2024/ses/output")

dir_fund_amaz <- ("A:\\projects\\landuse_br2024\\FundoAmazonia_BNDES\\data\\")

dir_siop_landscape <- ("A:\\projects\\landuse_br2024\\siop\\output_landscape\\")

dir_bndes_aut <- ("A:/projects/landuse_br2024/BNDES_Aut/output")

dir_giz_output <- ("A:\\projects\\landuse_br2024\\internacionais\\giz\\output\\")

########## import datasets ###########

setwd(atlas_output)

df_atlas_calculus  <- readRDS("database_atlas_landscape_2024.rds")

setwd(dir_b3_output)

df_b3_cbios_calculus <- readRDS("b3_cbios_landscape_final.rds")

output_proagro <- readRDS('A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.rds')

# setwd(dir_bndes_output)

df_bndes_naut_calculus <- readRDS("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\Landscape_climateUse_bndes_naut_20152023_04062024.rds")
  # dplyr::rename(climate_component = climate_use,
  #               source_finance_landscape = source_of_finance_landscape,
  #               origin_domestic_international = national_internacional,
  #               origin_private_public = source_private_public)

setwd(dir_oecd_output)

df_ocde_calculus_join <- readRDS("df_ocde_landscape_final_join_year.rds")

# setwd(dir_nint_output)

df_nint_calculus <- read_xlsx("A:\\projects\\landuse_br2024\\NINT\\NINT_Landscape_2015_2023.xlsx")

setwd(dir_idb_output)
# 
df_idb_calculus <- readRDS("data_idb_final_landscape.rds")

# setwd(fnmc_output)


fnmc_landscape <- readRDS("A:\\projects\\landuse_br2024\\fnmc\\FNMC_2015_2023Landscape.rds")


setwd(dir_susep_output)

df_ses_calculus <- readRDS("ses_agregado_landscape_completo_2024.rds")

setwd(sicor_output)

df_sicor_calculus <- readRDS("df_sicor_format_landscape_final_att.rds")

setwd(dir_giz_output)

df_giz_calculus <- readRDS("df_giz_transform_landscape.rds")

# setwd(dir_fund_amaz)

df_fund_amaz <- read_xlsx("A:\\projects\\landuse_br2024\\Fundo Amazonia\\FundoAmazonia_Landscape_2015_2023.xlsx") %>% select(-deflator,-cambio)

setwd(dir_bndes_aut) 

df_bndes_aut <- readRDS("df_bndes_aut_landscape_final.rds")

df_siop_landscape <- readRDS("A:\\projects\\landuse_br2024\\siop\\preview_data\\Siop_Expansao_Ver3_26052024.rds")

df_siop_1520 <- readRDS("A:\\projects\\landuse_br2024\\siop\\preview_data\\Siop_Expansao_04062024.rds")

data_landscape_final <- do.call("rbind",
                                list(df_ses_calculus,
                                     fnmc_landscape,
                                     df_atlas_calculus,
                                     output_proagro,
                                     df_bndes_naut_calculus,
                                     df_b3_cbios_calculus,
                                     df_ocde_calculus_join,
                                     df_nint_calculus,
                                     # df_idb_calculus,
                                     df_sicor_calculus,
                                     df_fund_amaz,
                                     df_bndes_aut,
                                     df_siop_landscape))
                                     # df_giz_calculus))


data_landscape_final <- data_landscape_final %>% 
  dplyr::mutate(value_brl_deflated_mean  = value_brl_deflated / 9,
                value_usd_mean = value_usd / 9) %>% 
  dplyr::mutate(climate_component = ifelse(climate_component == "Dual", "Mitigation and Adaptation", climate_component)) 

############## AGGREGATION DATA #############

data_aggregated <- aggregate(cbind(value_original_currency, value_brl_deflated, value_usd) ~ data_source + original_currency + source_finance_landscape + origin_domestic_international
+ origin_private_public + channel_landscape + instrument_landscape + sector_landscape + climate_component + year , data = data_landscape_final, FUN = sum)

nas_por_coluna <- colSums(is.na(data_landscape_final))

print(nas_por_coluna)

sum(data_landscape_final$value_original_currency)

sum(data_aggregated$value_original_currency)

setwd("A:\\projects\\landuse_br2024\\output_final")

saveRDS(data_landscape_final,"base_landscape_final_expansion_05062024.rds")


write.csv2(data_landscape_final, "base_landscape_final_expansion_05062024.csv")

write.csv2(data_aggregated, "base_landscape_final_expansion_aggregate_05062024.csv")
