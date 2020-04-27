#  Bibliotecas
suppressMessages(library(tidyverse)) # manipular
suppressMessages(library(shiny)) # shiny
suppressMessages(library(shinydashboard)) # shiny
suppressMessages(library(rsconnect)) # publicar 
suppressMessages(library(readr)) # ler csv
suppressMessages(library(lubridate)) # manipular data
suppressMessages(library(BatchGetSymbols)) # dados B3
suppressMessages(library(dplyr)) # manipular 
suppressMessages(library(DT)) # tabela
suppressMessages(library(ggplot2))
suppressMessages(library(alphavantager)) # API

# autenticação da API
av_api_key("your_key")

# captação dos códigos da IBOV
ibov <- GetIbovStocks()
tickers <- paste0(ibov$tickers, ".SA")


#Shiny
ui <- dashboardPage(
    dashboardHeader(title = "Bolsa de Valores"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "geral", icon = icon("globe")),
            menuItem("Ano de 2020", tabName = "2020", icon = icon("line-chart")),
            menuItem("Principais acontecimentos", tabName = "acontecimentos",icon = icon("line-chart"),
                     menuSubItem("No Brasil", tabName = "brasil", icon = icon("line-chart")),
                     menuSubItem("No mundo", tabName = "mundo", icon = icon("line-chart"))),
            menuItem("Modelos Preditivos - Em breve", tabName = "pred", icon = icon("desktop")),
            menuItem("Por Matheus Duzzi", icon = icon("paper-plane"), href = "https://www.linkedin.com/in/matheusduzziribeiro/", newtab = T)
            
        )
    ),
    dashboardBody(    
        tabItems(
            tabItem("geral",
                    fluidRow(
                        sidebarLayout(      
                            sidebarPanel(
                                selectInput("acao1", "Ação:", 
                                            choices = tickers),
                                hr(),
                                helpText("Escolha a ação da B3 que você deseja ver o desempenho")
                            ),
                            mainPanel(
                                plotOutput("b2020")  
                            )
                        ),
                        DT::dataTableOutput("tablereal")
                    )
            ),
            tabItem("2020",
                    fluidRow(
                        column(4,
                               selectInput("codigo",
                                           "Código:",choices = tickers),
                               hr(),
                               helpText("Escolha a ação da B3 para ver os dados de 2020")
                        )
                    ),
                    DT::dataTableOutput("tablegeral")
            )
        )
    )
)


server <- function(input, output) { 
    
    output$b2020 <- renderPlot({
        inicio <- "2020-01-01"  
        final <- Sys.Date()
        bench.ticker <- "^BVSP"
        saida <- BatchGetSymbols(tickers = input$acao1, first.date = inicio, last.date = final, 
                                 bench.ticker = bench.ticker)
        saida <- as_tibble(saida$df.tickers)
    ggplot(saida,aes(x = ref.date, y = price.close)) + geom_line() + xlab("Data") + ylab("Preço")
    })
    
    output$tablegeral = DT::renderDataTable({
        inicio <- "2020-01-01" 
        final <- Sys.Date()
        bench.ticker <- "^BVSP"
        saida <- BatchGetSymbols(tickers = tickers, first.date = inicio, last.date = final, 
                                 bench.ticker = bench.ticker)
        saida <- as_tibble(saida$df.tickers)
        saida %>% filter(ticker == input$codigo) %>% select(-ticker,-ret.closing.prices,-ret.adjusted.prices,-price.adjusted)
    })
    
    output$tablereal = DT::renderDataTable({
        cotacao <- as.tibble(av_get(symbol = input$acao1, av_fun = "TIME_SERIES_INTRADAY", interval = "1min", outputsize = "compact"))
        colnames(cotacao) <- c("tempo","Cotação atual","alta","baixa","fechamento","volume")
        cotacao[nrow(cotacao),2]
    })
}

shinyApp(ui, server)

