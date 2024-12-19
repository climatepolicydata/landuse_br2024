##################

# Author : Renan Morais
# Date: 20-08-2024
# Email: renanflorias@hotmail.com
# Goal: transform database for landscape
# resource: 


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
               shiny)


setwd("A:\\projects\\landuse_br2024\\output_final")

dezesseisjulho <- readRDS("base_landscape_final_expansion_16072024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                           data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source))

trezeagosto <- readRDS("base_landscape_final_expansion_13082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                        data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source))

dezenoveagosto <- readRDS("base_landscape_final_expansion_19082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6)


################## analisys ###############

#### sum by finalidade

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(dezesseisjulho)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("year")
pt$addRowDataGroups("data_source")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="total_deflated", 
                     summariseExpression="sum(value_mi)", format="%.2f")

# Renderizar a tabela
pt$renderPivot()


# 13/08

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(trezeagosto)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("year")
pt$addRowDataGroups("data_source")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="total_deflated", 
                     summariseExpression="sum(value_mi)", format="%.2f")

# Renderizar a tabela
pt$renderPivot()

##### 19/08

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(dezenoveagosto)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("year")
pt$addRowDataGroups("data_source")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="total_deflated", 
                     summariseExpression="sum(value_mi)", format="%.2f")

# Renderizar a tabela
pt$renderPivot()



#dashboard



###### TESTE ######


# Define a interface do usuário (UI)
ui <- fluidPage(
  tags$style(HTML("
    body {
      font-size: 16px;
    }
    h3 {
      font-size: 24px;
    }
    .pvtTable {
      font-size: 16px;
    }
  ")),
  
  titlePanel("Dashboard de Análise de Dados"),
  
  # Botão para download do Excel
  downloadButton("downloadData", "Download Excel"),
  
  # Colocando as tabelas uma embaixo da outra
  fluidRow(
    column(12, h3("Tabela 13/08")),
    column(12, pivottablerOutput("table_trezeagosto"))
  ),
  fluidRow(
    column(12, h3("Tabela 16/07")),
    column(12, pivottablerOutput("table_dezesseisjulho"))
  ),
  fluidRow(
    column(12, h3("Tabela 19/08")),
    column(12, pivottablerOutput("table_dezenoveagosto"))
  )
)

# Define o server
server <- function(input, output) {
  
  # Renderiza a tabela para 13/08
  output$table_trezeagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(trezeagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
  
  # Renderiza a tabela para 16/07
  output$table_dezesseisjulho <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezesseisjulho)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
  
  # Renderiza a tabela para 19/08
  output$table_dezenoveagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezenoveagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
  
  # Função para exportar os dados para Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("tabelas_pivo.xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      # Criar as tabelas como data frames
      pt1 <- PivotTable$new()
      pt1$addData(trezeagosto)
      pt1$addColumnDataGroups("year")
      pt1$addRowDataGroups("data_source")
      pt1$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt1$evaluatePivot()
      df1 <- as.data.frame(pt1)
      
      pt2 <- PivotTable$new()
      pt2$addData(dezesseisjulho)
      pt2$addColumnDataGroups("year")
      pt2$addRowDataGroups("data_source")
      pt2$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt2$evaluatePivot()
      df2 <- as.data.frame(pt2)
      
      pt3 <- PivotTable$new()
      pt3$addData(dezenoveagosto)
      pt3$addColumnDataGroups("year")
      pt3$addRowDataGroups("data_source")
      pt3$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt3$evaluatePivot()
      df3 <- as.data.frame(pt3)
      
      # Adiciona as tabelas ao workbook
      addWorksheet(wb, "13/08")
      writeData(wb, "13/08", df1)
      
      addWorksheet(wb, "16/07")
      writeData(wb, "16/07", df2)
      
      addWorksheet(wb, "19/08")
      writeData(wb, "19/08", df3)
      
      # Salva o arquivo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)


# Define a interface do usuário (UI)
ui <- fluidPage(
  tags$style(HTML("
    body {
      font-size: 16px;
    }
    h3 {
      font-size: 24px;
    }
    .pvtTable {
      font-size: 16px;
    }
  ")),
  
  titlePanel("Dashboard de Análise de Dados"),
  
  # Botão para download do Excel
  downloadButton("downloadData", "Download Excel"),
  
  # Colocando as tabelas uma embaixo da outra
  fluidRow(
    column(12, h3("Tabela 13/08")),
    column(12, pivottablerOutput("table_trezeagosto"))
  ),
  fluidRow(
    column(12, h3("Tabela 16/07")),
    column(12, pivottablerOutput("table_dezesseisjulho"))
  ),
  fluidRow(
    column(12, h3("Tabela 19/08")),
    column(12, pivottablerOutput("table_dezenoveagosto"))
  )
)

# Define o server
server <- function(input, output) {
  
  # Renderiza a tabela para 13/08
  output$table_trezeagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(trezeagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })

  # Renderiza a tabela para 16/07
  output$table_dezesseisjulho <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezesseisjulho)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })

  # Renderiza a tabela para 19/08
  output$table_dezenoveagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezenoveagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })

  # Função para exportar os dados para Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      "tabelas_pivo.xlsx"  # Nome do arquivo com extensão .xlsx
    },
    content = function(file) {
      wb <- createWorkbook()
      
      # Criar as tabelas como data frames
      pt1 <- PivotTable$new()
      pt1$addData(trezeagosto)
      pt1$addColumnDataGroups("year")
      pt1$addRowDataGroups("data_source")
      pt1$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt1$evaluatePivot()
      df1 <- as.data.frame(pt1)
      
      pt2 <- PivotTable$new()
      pt2$addData(dezesseisjulho)
      pt2$addColumnDataGroups("year")
      pt2$addRowDataGroups("data_source")
      pt2$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt2$evaluatePivot()
      df2 <- as.data.frame(pt2)
      
      pt3 <- PivotTable$new()
      pt3$addData(dezenoveagosto)
      pt3$addColumnDataGroups("year")
      pt3$addRowDataGroups("data_source")
      pt3$defineCalculation(calculationName="total_deflated", 
                            summariseExpression="sum(value_mi)")
      pt3$evaluatePivot()
      df3 <- as.data.frame(pt3)
      
      # Adiciona as tabelas ao workbook
      addWorksheet(wb, "13-08")
      writeData(wb, "13-08", df1)
      
      addWorksheet(wb, "16-07")
      writeData(wb, "16-07", df2)
      
      addWorksheet(wb, "19-08")
      writeData(wb, "19-08", df3)
      
      # Salva o arquivo Excel corretamente
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)


### new test save pivotsss #######
# Função para converter a tabela pivô em um data.frame
convert_pivot_to_df <- function(pivot_table) {
  pivot_table$evaluatePivot()
  rows <- pivot_table$asDataFrame()  # Removendo argumentos desnecessários
  return(rows)
}

# Função para criar e salvar o arquivo Excel diretamente
save_excel <- function() {
  wb <- createWorkbook()
  
  # Criar as tabelas como data frames
  pt1 <- PivotTable$new()
  pt1$addData(trezeagosto)
  pt1$addColumnDataGroups("year")
  pt1$addRowDataGroups("data_source")
  pt1$defineCalculation(calculationName="total_deflated", 
                        summariseExpression="sum(value_mi)")
  df1 <- convert_pivot_to_df(pt1)
  
  pt2 <- PivotTable$new()
  pt2$addData(dezesseisjulho)
  pt2$addColumnDataGroups("year")
  pt2$addRowDataGroups("data_source")
  pt2$defineCalculation(calculationName="total_deflated", 
                        summariseExpression="sum(value_mi)")
  df2 <- convert_pivot_to_df(pt2)
  
  pt3 <- PivotTable$new()
  pt3$addData(dezenoveagosto)
  pt3$addColumnDataGroups("year")
  pt3$addRowDataGroups("data_source")
  pt3$defineCalculation(calculationName="total_deflated", 
                        summariseExpression="sum(value_mi)")
  df3 <- convert_pivot_to_df(pt3)
  
  # Adiciona as tabelas ao workbook
  addWorksheet(wb, "13-08")
  writeData(wb, "13-08", df1)
  
  addWorksheet(wb, "16-07")
  writeData(wb, "16-07", df2)
  
  addWorksheet(wb, "19-08")
  writeData(wb, "19-08", df3)
  
  # Salva o arquivo Excel diretamente no diretório de trabalho ou caminho especificado
  saveWorkbook(wb, "tabelas_pivo.xlsx", overwrite = TRUE)
}

# Salvar o arquivo Excel assim que o script for executado
save_excel()

# Define a interface do usuário (UI)
ui <- fluidPage(
  tags$style(HTML("
    body {
      font-size: 16px;
    }
    h3 {
      font-size: 24px;
    }
    .pvtTable {
      font-size: 16px;
    }
  ")),
  
  titlePanel("Dashboard de Análise de Dados"),
  
  # Colocando as tabelas uma embaixo da outra
  fluidRow(
    column(12, h3("Tabela 13/08")),
    column(12, pivottablerOutput("table_trezeagosto"))
  ),
  fluidRow(
    column(12, h3("Tabela 16/07")),
    column(12, pivottablerOutput("table_dezesseisjulho"))
  ),
  fluidRow(
    column(12, h3("Tabela 19/08")),
    column(12, pivottablerOutput("table_dezenoveagosto"))
  )
)

# Define o server
server <- function(input, output) {
  
  # Renderiza a tabela para 13/08
  output$table_trezeagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(trezeagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
  
  # Renderiza a tabela para 16/07
  output$table_dezesseisjulho <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezesseisjulho)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
  
  # Renderiza a tabela para 19/08
  output$table_dezenoveagosto <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(dezenoveagosto)
    pt$addColumnDataGroups("year")
    pt$addRowDataGroups("data_source")
    pt$defineCalculation(calculationName="total_deflated", 
                         summariseExpression="sum(value_mi)", format="%.2f")
    pt$renderPivot()
  })
}

# Executa o aplicativo Shiny
shinyApp(ui = ui, server = server)


#### analisys each observation ##########

dezesseisjulho_filter <- readRDS("base_landscape_final_expansion_16072024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                           data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source)) %>% 
  dplyr::filter(data_source %in% c("bndes_naut", "idb_projects","nint","oecd_cf","siop_painel")) %>% select(-id_original)


dezenoveagosto_filter <- readRDS("base_landscape_final_expansion_19082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6) %>% 
  dplyr::filter(data_source %in% c("bndes_naut", "idb_projects","nint","oecd_cf","siop_painel")) %>% select(-id_original)


anti_join_both <- anti_join(dezesseisjulho_filter,dezenoveagosto_filter)


#### sum by finalidade

# Criar o objeto Pivot Table
pt <- PivotTable$new()

# Adicionar os dados ao Pivot Table
pt$addData(dezenoveagosto_filter)

# Adicionar as colunas e linhas
pt$addColumnDataGroups("year")
pt$addRowDataGroups("data_source")

# Adicionar o cálculo (soma de vl_parc_credito)
pt$defineCalculation(calculationName="total_deflated", 
                     summariseExpression="sum(value_mi)", format="%.2f")

# Renderizar a tabela
pt$renderPivot()


#diferença idb
setwd("A:\\projects\\landuse_br2024\\output_final")

write.xlsx(dezenoveagosto_filter %>% filter(data_source %in% "idb_projects", year >= 2018 & year <= 2020), "diff_idb.xlsx")


### diferença bndes

dezesseisjulho_filter_bndes <- readRDS("base_landscape_final_expansion_16072024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                                  data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source)) %>% 
  dplyr::filter(data_source %in% c("bndes_naut")) 

dezenoveagosto_filter_bndes <- readRDS("base_landscape_final_expansion_19082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6) %>% 
  dplyr::filter(data_source %in% c("bndes_naut"))


anti_join_both_bndes <- anti_join(dezesseisjulho_filter_bndes,dezenoveagosto_filter_bndes, by = "value_original_currency")

setwd("A:\\projects\\landuse_br2024\\output_final")

write.xlsx(anti_join_both_bndes, "diff_bndes_naut.xlsx")

####### diff oecd ####

dezesseisjulho_filter_oecd <- readRDS("base_landscape_final_expansion_16072024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                                        data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source)) %>% 
  dplyr::filter(data_source %in% c("oecd_cf")) 

dezenoveagosto_filter_oecd <- readRDS("base_landscape_final_expansion_19082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6) %>% 
  dplyr::filter(data_source %in% c("oecd_cf"))

anti_join_both_oecd <- anti_join(dezesseisjulho_filter_oecd,dezenoveagosto_filter_oecd, by = "value_original_currency")

write.xlsx(anti_join_both_oecd, "diff_oecd.xlsx")
####### diff siop ####

dezesseisjulho_filter_siop <- readRDS("base_landscape_final_expansion_16072024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6,
                                                                                                       data_source = ifelse(data_source %in% c("GEF","GIZ","KfW","WB","NORAD"),"Internationals",data_source)) %>% 
  dplyr::filter(data_source %in% c("siop_painel")) 

dezenoveagosto_filter_siop <- readRDS("base_landscape_final_expansion_19082024.rds") %>% dplyr::mutate(value_mi = value_original_currency/1e6) %>% 
  dplyr::filter(data_source %in% c("siop_painel"))

anti_join_both_siop <- anti_join(dezesseisjulho_filter_siop,dezenoveagosto_filter_siop, by = c("value_original_currency","year","project_name"))

write.xlsx(anti_join_both_siop, "diff_siop.xlsx")     


#################3 DIFF MASTER #####


