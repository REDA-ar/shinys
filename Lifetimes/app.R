library(shiny)

genero_ej4 <- function(n, semilla)
{
  lambda <- 0.25
  set.seed(semilla)
  salida <- rep(0, n)
  for (i in 1:n)
  {
    salida[i] <-round(rexp(1, lambda),2)
  }
  dd <- data.frame(salida)
  colnames(dd) <- c("lamps")
  return(dd)
}

resuelvo_ej4 <- function(datos){
  ans <- list()
  ans$media <- mean(datos)
  ans$mediana <- mean(datos)
  ans
}

chequeo <- function(nosotros, ellos){
  if(abs(nosotros-ellos)< 0.001){ans <- "well done!"}else{
    ans <- "try again"
  } 
  ans
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  salida <- reactive({
    dd <- genero_ej4(input$obs, input$libreta)
  })
  
  
  output$datos <- renderTable({
    salida()
  })
  
  
  # 
  # 
  output$chequeo_media <- renderText({
    dif<-as.numeric(try(abs(input$media-mean(salida()[,1])),TRUE))
    if(is.na(dif)){ans <- "Enter the result"}else{
      if(dif<0.1){ans <- "Great!"}
      else {ans <- "Try again!"}}
    ans
    
  })
  
  
  
  output$chequeo_mediana <- renderText({
    dif<-as.numeric(try(abs(input$mediana-median(salida()[,1])),TRUE))
    if(is.na(dif)){ans <- "Enter the result"}else{
      if(dif<0.1){ans <- "Great!"}
      else {ans <- "Try again!"}}
    ans
  })
  
  
  
  output$media_nos <- renderTable({
    mean(salida()[,1])
  })
  output$mediana_nos <- renderTable({
    median(salida()[,1])
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("lamps_id",
            input$libreta,
            "_n_",
            input$obs,
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(
        salida(),
        file,
        row.names = FALSE,
        col.names = TRUE,
        sep = ""
      )
    }
  )
  
  
  
}

ui <-fluidPage(
  # titlePanel("Exercise: Duration of lamps"
  #            # app title/description
  # ),
  h1("Exercise: Duration of lamps"),
  fluidPage(
    mainPanel(h5( div("You want to study the distribution of
                      the lifetime (in days) of the lamps produced
                      by a certain company. With this aim you
                      select n lamps from its production, try them
                      and register their lifetime. Enter your id number
                      and a value of n to get your data")))),
  fluidRow(
    column(4, 
           headerPanel(h4("Getting data: Choose a sample size and enter you id number")),
           numericInput("obs", "Sample size (between 1 and 1000):", min = 1, max = 1000, value = 50),
           numericInput("libreta", "Student id:", min = 1, max = 100000, value = 24292),
           downloadButton("downloadData", "Download"),
    ),
    column(2,  
           headerPanel(h4("YOUR data")),
           tableOutput("datos")
    ),
    column(4,
           headerPanel(h4("Your turn: Enter and check your responses")),
           numericInput("media", "Mean:", min = -100000, max =100000, value = "?"),
           textOutput("chequeo_media"),
           #tableOutput("media_nos"),
           numericInput("mediana", "Median:", min = -100000, max =100000, value = "?"),
           textOutput("chequeo_mediana"),
           #tableOutput("mediana_nos")
           
           
           
    ) 
  )
  
    )


shinyApp(ui = ui, server = server)

