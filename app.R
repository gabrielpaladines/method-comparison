library(shiny)
library(DT)
library(mcr)
  
ui <- fluidPage(
  titlePanel("Comparación de Métodos"),
  sidebarLayout(
    sidebarPanel(
      h4("Ingrese datos"),
      textInput("xAxis", "Método Actual", placeholder = "Ingrese nombre del método"),
      textInput("yAxis", "Nuevo Método", placeholder = "Ingrese nombre del método"),
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
      ),
    mainPanel(
      h4("Vista previa de los datos"),
      DT::dataTableOutput("contents"),
      plotOutput("plot1")
      )
    )
  )
  
server <- function(input, output) {
  data <- reactive({
  inFile <- input$file1
      
  if (is.null(inFile))
    return(NULL)
      
  tbl <- read.csv(inFile$datapath, header = input$header)
  return(tbl)
  })
  xTitle <- reactive({
    return(input$xAxis)
  })
  yTitle <- reactive({
    return(input$yAxis)
  })
  output$contents <- renderDataTable({
    datatable(data(),
              colnames = c(xTitle(), yTitle()), 
              options = list(paging = TRUE))
  })
  output$plot1 <- renderPlot({
    if (is.null(data()))
      return(NULL)
    x <- data()[,1]
    y <- data()[,2]
    plot(x=x, y=y,  main = "Regression Comparison", xlab = xTitle(), ylab = yTitle())
    lin.reg <- lm(y~x)
    dem.reg <- mcreg(x, y, method.reg = "Deming", error.ratio = 1.2)
    abline(lin.reg, col="blue")
    abline(dem.reg@para[1:2], col = "red")
  })
}
  
shinyApp(ui, server)


