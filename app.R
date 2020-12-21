library(shiny)
library(shinythemes)
library(mcr)
  
ui <- fluidPage( theme = shinytheme("flatly"),
  navbarPage(HTML("<i class='fas fa-flask'></i>&nbsp; Análisis de datos para Laboratorio Clínicos"), 
             windowTitle="UOC - Gabriel Paladines"),
  sidebarLayout(
    sidebarPanel(
      textInput("xAxis", "Método Actual", placeholder = "Ingrese nombre del método"),
      textInput("yAxis", "Nuevo Método", placeholder = "Ingrese nombre del método"),
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      checkboxInput("header", "Header", TRUE),
      tags$hr(),
      selectInput("modeloInput", "Modelo de Regresión:",
                  c("Ordinary Linear Regression" = "LinReg",
                    "Deming" = "Deming",
                    "Passing-Bablok" = "PaBa")),
      fluidRow(
        column(6,
               selectInput("metodoCi", "Método CI:",
                           c("bootstrap",
                             "jackknife",
                             "analytical",
                             "nestedbootstrap")),
               selectInput("corMethod", "Método de Correlación:",
                           c("Pearson" = "pearson",
                             "Kendall" = "kendall",
                             "Spearman" = "spearman"))
        ),
        column(6,
               selectInput("bootstrapCi", "Bootstrap CI:",
                           c("quantile",
                             "Student",
                             "BCa",
                             "tBoot")),
               numericInput("errorRatio", "Error Ratio", 1, min = 0,
                 max = 100, step = 1
               )
        )
      )
      ),
    mainPanel(
      plotOutput("plot1"),
      tags$hr(),
      fluidRow(
        column(6,
          h4("Summary"),
          verbatimTextOutput("summary")
        ),
        column(6, align='center',
          verbatimTextOutput("correlation"),
          downloadButton("report", "Descargar Reporte")
        )
      )
    )
  ),
  fluidRow(
    column(12, align='center',
           tags$hr(),
           tags$span(style="color:#CCC", HTML("&copy; Trabajo Final de Máster - Universitat Oberta de Catalunya"))
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
  metodoCi <- reactive({
    return(input$metodoCi)
  })
  corMethod <- reactive({
    return(input$corMethod)
  })
  bootstrapCi <- reactive({
    return(input$bootstrapCi)
  })
  errorRatio <- reactive({
    return(input<- input$errorRatio)
  })
  modeloSelected <- reactive({
    return(input$modeloInput)
  })
  modeloReg <- reactive({
    if (is.null(data()) || is.null(modeloSelected()))
      return(NULL)
    x <- data()[,1]
    y <- data()[,2]
    modelo <- mcreg(x, y, method.reg = modeloSelected(),
                    mref.name=xTitle(),
                    mtest.name=yTitle(),
                    method.ci= metodoCi(),
                    method.bootstrap.ci = bootstrapCi(),
                    error.ratio = errorRatio())
  })
  modeloCor <- reactive({
    if (!is.null(modeloReg()))
      res <- cor.test(data()[,1], data()[,2], method = corMethod())
      return(res)
  })
  output$plot1 <- renderPlot({
    if (is.null(data()) || is.null(modeloSelected()))
      return(NULL)
    plot(modeloReg(),  main = "Regression Comparison",
         add.legend=TRUE,identity=TRUE,
         ci.area=TRUE,add.cor=TRUE, cor.method = corMethod())
  })
  output$summary <- renderPrint({
    if (!is.null(modeloReg()))
      printSummary(modeloReg())
  })
  output$correlation <- renderPrint({
    if (!is.null(modeloReg()))
      modeloCor()
  })
  output$report <- downloadHandler(
    filename = "report.pdf",
    
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(method1 = xTitle(), 
                     method2 = yTitle(),
                     modelo = modeloReg(),
                     modelo_cor = modeloCor(),
                     cor_method = corMethod())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        output_format="pdf_document",
                        envir = new.env(parent = globalenv())
      )
    }
  )
}
  
shinyApp(ui, server)


