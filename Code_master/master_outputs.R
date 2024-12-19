##############################################################################
# Author : Renan Florias
# Date: 02.10.2023
# Email: renanflorias@hotmail.com
# Goal: Masterfile to outputs landscape

########################### Libraries ######################################

library(readxl)

library(dplyr)
library(psych)
library(shiny)
library(pivottabler)


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

dir_internationals <- ("A:/projects/landuse_br2024/internacionais")

dir_idb_clear <- ("A:\\projects\\brlanduse_landscape102023\\idb")

dir_usd_fed <- ("A:\\macro\\usd_FED\\rawData\\")

########## import datasets ###########



######### ATLAS DO SEGURO RURAL########

setwd(atlas_output)

df_atlas_calculus  <- readRDS("database_atlas_landscape_2024.rds")

df_atlas_2023 <- readRDS("database_atlas_att_2023.rds")

df_atlas_calculus <- rbind(df_atlas_calculus,df_atlas_2023)

################ B3 CBIOS ########

setwd(dir_b3_output)

df_b3_cbios_calculus <- readRDS("b3_cbios_landscape_final.rds")

################# PROAGRO ##########

output_proagro <- readRDS('A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.rds')

# setwd(dir_bndes_output)

################## BNDES NAO AUTOMATICO ###########

df_bndes_naut_calculus <- readRDS("A:\\projects\\landuse_br2024\\bndes_n_aut\\Preview Data\\data_bndes_landscape_18072024.rds")
  # dplyr::rename(climate_component = climate_use,
  #               source_finance_landscape = source_of_finance_landscape,
  #               origin_domestic_international = national_internacional,
  #               origin_private_public = source_private_public)


############## OECD ############
setwd(dir_oecd_output)

df_ocde_calculus_join <- readRDS("df_ocde_landscape_final_join_01102024.rds")

############### NINT ###########

setwd("A:\\projects\\landuse_br2024\\NINT\\")

df_nint_calculus <- readRDS("df_nint_landscape_04102024.rds")

################ IDB ##############

setwd(dir_idb_output)

df_idb_calculus <- readRDS("data_idb_final_landscape_30092024.rds")


######## FNMC ###########
# setwd(fnmc_output)

fnmc_landscape <- readRDS("A:\\projects\\landuse_br2024\\fnmc\\FNMC_2015_2023Landscape_11062024.rds")

############### SES SUSEP ##########

setwd(dir_susep_output)

df_ses_calculus <- readRDS("ses_agregado_landscape_completo_2024.rds")

setwd(sicor_output)

############## SICOR MDCR #########

df_sicor_calculus <- readRDS("df_sicor_format_landscape_final_09072024.rds")

# setwd(dir_giz_output)
# 
# df_giz_calculus <- readRDS("df_giz_transform_landscape.rds")

############### FUNDO AMAZONIA ########

# setwd(dir_fund_amaz)

df_fund_amaz <- read_xlsx("A:\\projects\\landuse_br2024\\Fundo Amazonia\\FundoAmazonia_Landscape_2015_2023_VERSAO2.xlsx") %>% select(-deflator,-cambio)


######### BNDES Automatico #######

setwd(dir_bndes_aut) 

df_bndes_aut <- readRDS("df_bndes_aut_landscape_final.rds")

############ SIOP #####

df_siop_landscape <- readRDS("A:\\projects\\landuse_br2024\\siop\\preview_data\\Siop_Expansao_Ver17_23092024.rds") %>% 
  dplyr::mutate(channel_landscape = ifelse(channel_original == "transferencias a estados e ao distrito federal;recursos sob supervisao do ministerio do meio ambiente e mudanca do clima",
                                           "Government agencies", channel_landscape))

######## INTERNACIONAIS #########

setwd(dir_internationals)

df_internationals <- readRDS("all_internationals_landscape_deflated_exchange_04102024.rds") 

########### COMBINE DAABASES  #####

data_landscape_final <- do.call("rbind",
                                list(df_ses_calculus,
                                     fnmc_landscape,
                                     df_atlas_calculus,
                                     output_proagro,
                                     df_bndes_naut_calculus,
                                     df_b3_cbios_calculus,
                                     df_ocde_calculus_join,
                                     df_nint_calculus,
                                     df_idb_calculus,
                                     df_sicor_calculus,
                                     df_fund_amaz,
                                     df_bndes_aut,
                                     df_siop_landscape,
                                     df_internationals))
                                     # df_giz_calculus))


data_landscape_final <- data_landscape_final %>% 
  dplyr::mutate(value_brl_deflated_mean  = value_brl_deflated / 9,
                value_usd_mean = value_usd / 9) %>% 
  dplyr::mutate(climate_component = ifelse(climate_component %in% c("Dual","Adaptation and Mitigation"), "Mitigation and Adaptation", climate_component),
                channel_landscape = ifelse(channel_landscape%in% c("Civil Society organization","Civil Society Organization"),"Civil society organization",channel_landscape),
                instrument_landscape = ifelse(instrument_landscape %in% "Risk Management", "Risk management", instrument_landscape),
                instrument_landscape = ifelse(instrument_landscape %in% "Thematic bonds", "Thematic Bonds", instrument_landscape),
                original_currency = ifelse(original_currency == "brl", "BRL",
                                           ifelse(original_currency == "usd", "USD",
                                                  ifelse(original_currency == "eur", "EUR", original_currency))))

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd.R"))
cambio_sgs = read.csv("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio.csv") %>% select(-X)

ano_ini = 2015
ano_fim = 2023

tabela_deflator <- deflator_usd(ano_ini, ano_fim, anos,usd_inflation)
tabela_cambio <-cambio_sgs %>%
  dplyr::filter(year >= 2015 & year <= 2023)

deflate_usd_calculus <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  tabela_deflator <- tabela_deflator %>% dplyr::rename(deflator_usd = deflator)
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>% 
    dplyr::mutate(value_usd_deflated = ifelse(original_currency == "BRL", (value_original_currency/cambio) * deflator_usd,
                                              ifelse(original_currency == "USD", value_original_currency * deflator_usd,
                                                     ifelse(data_source == "nint" & year == 2015 & original_currency == "EUR", value_original_currency / 1.1095 *deflator_usd,
                                                     value_original_currency /1.0813 * deflator_usd))))
  
  return(base_select_deflator)
}

data_landscape_final<- deflate_usd_calculus(tabela_deflator, data_landscape_final, tabela_cambio)

############## AGGREGATION DATA #############

data_aggregated <- aggregate(cbind(value_original_currency, value_brl_deflated, value_usd, value_usd_deflated) ~ data_source + original_currency + source_finance_landscape + origin_domestic_international
+ origin_private_public + channel_landscape + instrument_landscape + sector_landscape + climate_component + year , data = data_landscape_final, FUN = sum)

describe(data_aggregated)

nas_por_coluna <- colSums(is.na(data_landscape_final))

print(nas_por_coluna)

sum(data_landscape_final$value_original_currency)

sum(data_aggregated$value_original_currency)

####### function to visualize categorys #########

visualizar_categorias_unicas <- function(df, colunas) {
  categorias_unicas <- list()
  
  for (coluna in colunas) {
    categorias_unicas[[coluna]] <- unique(df[[coluna]])
  }
  
  return(categorias_unicas)
}

# Supondo que 'df' seja o seu data frame e que as variáveis mencionadas sejam as colunas
colunas <- c(
  "data_source", "original_currency", "source_finance_landscape", 
  "origin_domestic_international", "origin_private_public", 
  "channel_landscape", "instrument_landscape", 
  "sector_landscape", "climate_component", "year"
)

categorias_unicas <- visualizar_categorias_unicas(data_landscape_final, colunas)

# Para visualizar o resultado
for (coluna in names(categorias_unicas)) {
  cat("Coluna:", coluna, "\n")
  print(categorias_unicas[[coluna]])
  cat("\n")
}

######### save tables ##############

setwd("A:\\projects\\landuse_br2024\\output_final")

saveRDS(data_landscape_final,"base_landscape_final_expansion_24102024.rds")

write.xlsx(data_landscape_final, "base_landscape_final_expansion_24102024.xlsx")
write.csv2(data_landscape_final, "base_landscape_final_expansion_24102024.csv")

write.csv2(data_aggregated, "base_landscape_final_expansion_24102024_agregado.csv")
write.xlsx(data_aggregated, "base_landscape_final_expansion_24102024_agregado.xlsx")


#############validate aggregated ########

pivottable <- data_landscape_final %>%
  group_by(year, data_source) %>%    # Agrupando por ano e data_source
  summarise(total_deflated = sum(value_brl_deflated, na.rm = TRUE)) %>%    # Somando os valores de vl_brl_deflated
  pivot_wider(names_from = year, values_from = total_deflated)           # Transformando anos em colunas

# Visualizar a tabela pivô
print(pivottable)


# ######## teste dash #########
# df <- data_aggregated
# 
# # Interface do usuário
# ui <- fluidPage(
#   titlePanel("Visualização de Categorias Únicas e Valores"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("coluna", "Selecione a Coluna", 
#                   choices = names(df)[-ncol(df)],  # Exclui a coluna de valores do dropdown
#                   selected = names(df)[1])
#     ),
#     mainPanel(
#       h4("Categorias Únicas:"),
#       verbatimTextOutput("categoriasUnicas"),
#       h4("Distribuição dos Valores:"),
#       plotOutput("graficoValores")
#     )
#   )
# )

# Supondo que seu DataFrame se chame 'df'
# Supondo que seu DataFrame se chame 'data_landscape_final'
pt <- PivotTable$new()

# Definir as variáveis e construir a tabela pivô
pt$addData(data_landscape_final)                      # Adiciona os dados
pt$addColumnDataGroups("year")                        # Adiciona os anos como colunas
pt$addRowDataGroups("data_source")                    # Adiciona data_source como linhas
# Definir o cálculo da soma
pt$defineCalculation(
  calculationName="Total Deflated",
  summariseExpression="sum(value_usd_deflated, na.rm=TRUE)/1000000000",
  format="%.3f"
)

pt$renderPivot()

# Exibir a tabela pivô
pt

# # Servidor
# server <- function(input, output) {
#   
#   output$categoriasUnicas <- renderPrint({
#     coluna_selecionada <- input$coluna
#     categorias <- unique(df[[coluna_selecionada]])
#     categorias
#   })
#   
#   output$graficoValores <- renderPlot({
#     coluna_selecionada <- input$coluna
#     dados_filtrados <- aggregate(value_brl_deflated ~ df[[coluna_selecionada]], data = df, sum)
#     colnames(dados_filtrados) <- c("Categoria", "Valor_Total")
#     
#     ggplot(dados_filtrados, aes(x = reorder(Categoria, -Valor_Total), y = Valor_Total)) +
#       geom_bar(stat = "identity", fill = "steelblue") +
#       labs(x = coluna_selecionada, y = "Valor Total (BRL Deflated)") +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
