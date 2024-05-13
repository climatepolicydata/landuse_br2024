##############################################################################
# Author : Renan Florias
# Date: 02.10.2023
# Email: renanflorias@hotmail.com
# Goal: Masterfile to outputs landscape

########################### Libraries ######################################


######################### Directories and databases #########################

atlas_output <- ("A:/projects/landuse_br2024/atlas/output")

sicor_output <- ("A:/projects/landuse_br2024/sicor/output")

dir_b3_output <-("A:/projects/landuse_br2024/b3/output")

dir_nint_output <- ("A:\\projects\\landuse_br2024\\nint\\output")

dir_oecd_output <- ("A:/projects/landuse_br2024/oecd_cf/output")

dir_idb_output <- ("A:\\projects\\landuse_br2024\\idb")

fnmc_output <- ("A:/projects/landuse_br2024/gov_fnmc")

dir_bndes_output <- ("A:/projects/landuse_br2024/bndes_n_aut/output")

dir_susep_output <- ("A:/projects/landuse_br2024/ses/output")

dir_fund_amaz <- ("A:\\projects\\landuse_br2024\\FundoAmazonia_BNDES\\data\\")

dir_siop_landscape <- ("A:\\projects\\landuse_br2024\\siop\\output_landscape\\")

dir_bndes_aut <- ("A:/projects/landuse_br2024/BNDES_Aut/output")

########## import datasets ###########

setwd(atlas_output)

df_atlas_calculus  <- readRDS("database_atlas_landscape_2024.rds")

setwd(dir_b3_output)

df_b3_cbios_calculus <- readRDS("b3_cbios_landscape_final.rds")

output_proagro <- read.csv('A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.rds') %>% 
  select(-X)

# setwd(dir_bndes_output)

df_bndes_naut_calculus <- readRDS("A:\\finance\\bndes_N_aut\\cleanData\\operacoes_financiamento_operacoes_nao_automaticas_clear_03_24.rds")

setwd(dir_oecd_output)

df_ocde_calculus <- readRDS("df_ocde_landscape_final_2020.rds")

# setwd(dir_nint_output)

df_nint_calculus <- readRDS("A:\\projects\\landuse_br2024\\NINT\\NINT_landscape_04_04_2024.rds")

setwd(dir_idb_output)

df_idb_calculus <- readRDS("idb_landscape_final.RDS")

setwd(fnmc_output)


fnmc_landscape <- readRDS("A:\\projects\\landuse_br2024\\fnmc\\FNMC_Landscape2024.rds") %>% select(-"Subactivity Landscape")


setwd(dir_susep_output)

df_ses_calculus <- readRDS("ses_agregado_landscape_completo_2024.rds")

setwd(sicor_output)

df_sicor_calculus <- readRDS("df_sicor_format_landscape_final_att.rds")


setwd(dir_fund_amaz)

df_fund_amaz <- readRDS("A:\\projects\\brlanduse_landscape102023\\FundoAmazonia_BNDES\\data\\FundoAmazonia_BNDES_N_Aut_LandScape.rds")

setwd(dir_bndes_aut) 

df_bndes_aut <- readRDS("df_bndes_aut_landscape_final.rds")

# df_siop_landscape<-read_rds("siop_landscape_15_20.rds")%>%mutate(id_original = "-")

data_landscape_final <- do.call("rbind",
                                list(df_ses_calculus,
                                     fnmc_landscape ,
                                     df_atlas_calculus,
                                     output_proagro,
                                     df_bndes_naut_calculus,
                                     df_b3_cbios_calculus,
                                     df_ocde_calculus,
                                     df_nint_calculus,
                                     df_idb_calculus,
                                     df_sicor_calculus,
                                     df_fund_amaz))


data_landscape_final <- data_landscape_final %>% 
  dplyr::mutate(value_brl_deflated_mean  = value_brl_deflated / 6,
                value_usd_mean = value_usd / 6) %>% 
  dplyr::mutate(climate_component = ifelse(climate_component == "Dual", "Mitigation and Adaptation", climate_component)) 




setwd("A:\\projects\\landuse_br2024\\output_final")

saveRDS(data_landscape_final,"base_landscape_final_expansion.rds")


write.csv2(data_landscape_final, "base_landscape_final_expansion.rds.csv",fileEncoding = "ISO-8859-1")