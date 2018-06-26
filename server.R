#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(DT)
library(dplyr)
library(shiny)

source("bolao_utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  palpites_file <- "dados/palpites_3a_rodada.csv"
  resultados <- get_resultados(palpites_file, update_placares = TRUE)

  print("Loaded resultados")
  print(head(resultados))
  
  pontuacao <- resultados %>%
    group_by(`Palpiteiro` = apostador) %>%
    summarise(
      `Acertos em cheio` = sum(pontos == 3, na.rm = TRUE),
      `Acertos do vencedor ou empate` = sum(pontos == 1, na.rm = TRUE),
      `Total de pontos` = sum(pontos, na.rm = TRUE)) %>%
    arrange(desc(`Total de pontos`)) %>%
    mutate(`Ranking` = paste0(dense_rank(desc(`Total de pontos`)), tags$sup("o"))) %>%
    select(`Ranking`, 1:4)
  
  output$pontuacao_table <- DT::renderDataTable(
    DT::datatable(
      pontuacao, rownames = FALSE, escape = FALSE, extensions = 'Buttons',
      options = list(pageLength = 20, dom = 'Bfrtip',
                     buttons = c('pageLength', 'copy', 'excel', 'pdf'),
                     columnDefs = list(list(className = 'dt-center', targets = 2:4))
                )
      )
  )
    
  palpites <- resultados %>%
    group_by(apostador) %>%
    mutate(x = "x") %>%
    select(`Palpiteiro` = apostador,
           `Data` = Date,
           `Time 1` = time1,
           `G1` = gols_time1,
           `x`,
           `G2` = gols_time2,
           `Time 2` = time2) %>%
    arrange(`Palpiteiro`)
  
  output$palpites_table <- DT::renderDataTable(
    DT::datatable(palpites, extensions = 'Buttons', rownames = FALSE,
      options = list(pageLength = 16, lengthMenu = 16 * c(1, 4, 8, 12, 16, 20), dom = 'Bfrtip',
                     buttons = c('pageLength', 'copy', 'excel', 'pdf'),
                     columnDefs = list(list(className = 'dt-center', targets = 1:6))
                )
      )
  )
  
  palpites_hoje <- palpites %>%
    mutate(jogo_id = 1:n()) %>%
    arrange(jogo_id, `Palpiteiro`) %>%
    filter(Data == as.Date(Sys.time())) %>%
    select(-`Data`, -jogo_id)
  
  output$palpites_hoje_table <- DT::renderDataTable(
    DT::datatable(palpites_hoje, extensions = 'Buttons', rownames = FALSE,
                  options = list(pageLength = 15, lengthMenu = 15 * 1:4, dom = 'Bfrtip',
                                 buttons = c('pageLength', 'copy', 'excel', 'pdf'),
                                 columnDefs = list(list(className = 'dt-center', targets = 1:5))
                            )
                  )
  )
  
  resultados_copa <- resultados %>%
    filter(!is.na(Home.Score)) %>%
    mutate(x = "x") %>%
    select(Data = Date, `Time 1` = time1, `G1` = Home.Score, x,
           `G2` = Away.Score, `Time 2` = time2) %>%
    distinct() %>%
    arrange(desc(`Data`))
    
    output$resultados_copa_table <- DT::renderDataTable(
      DT::datatable(resultados_copa, extensions = 'Buttons', rownames = FALSE,
                    options = list(dom = 'Bfrtip',
                                   buttons = c('pageLength', 'copy', 'excel', 'pdf'),
                                   columnDefs = list(list(className = 'dt-center', targets = 0:5))
                              )
                    )
    )
})
