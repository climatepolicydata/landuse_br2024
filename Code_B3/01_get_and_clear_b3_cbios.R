##################

# Author : Renan Morais
# Date: 02-01-2024
# Email: renanflorias@hotmail.com
# Goal: clear cbios
# resource: 


## Modified by Julia Niemeyer
## Date: 28/05/2025

########################### ANTION NEEDED ##################################
ano <- 2024 ### colocar o ano atual de atualização do Landscape, que irá ler a  ase mais atualizada e também para o nome do arquivo final

########################### Libraries ######################################
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
               readxl)

##### directory #########

dir_b3_raw <- ("A:/finance/b3_cbios/rawData")

dir_b3_output<- ("A:/finance/b3_cbios/cleanData")

################ import databases #############

setwd(dir_b3_raw)


##pegar a última planilha
df_negociacao_raw_antiga <- read_xlsx("cbios_negociacao_definitiva_2020_2023.xlsx", skip = 7) %>% 
  janitor::clean_names(.)

#ler a mais atualizada

df_negociacao_raw_atual <- read_xlsx(paste0(dir_b3_raw, "/cbios_negociacao_definitiva_", ano, ".xlsx")) %>% 
    janitor::clean_names(.) %>%
    dplyr::rename(qtde_negociada = total) %>%
    select(1:3, 8:13) %>%
    mutate(data_str = as.character(as.Date(data))) %>%  # cria nova coluna em texto
    separate(data_str, into = c("ano", "mes", "dia"), sep = "-", convert = TRUE) %>%
    relocate(dia, ano, mes, .after = last_col())
    

#ajusta o nome das colunas
names <- names(df_negociacao_raw_atual)
colnames(df_negociacao_raw_antiga) <- names

##juntar 

df_negociacao_raw <- rbind(df_negociacao_raw_antiga, df_negociacao_raw_atual)

##mudar o formato do ano
df_negociacao_raw <- df_negociacao_raw %>% 
  mutate(ano = as.numeric(ano))


### Export

setwd(dir_b3_output)

write_xlsx(df_negociacao_raw, paste0(dir_b3_raw, "/cbios_negociacao_definitiva_2020_", ano, ".xlsx"))
saveRDS(df_negociacao_raw, "cbios_b3_nd_clear.rds")

