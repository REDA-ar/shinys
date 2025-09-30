library(shiny)
generacionMuestra<- function(semilla){
  #Definimos el modelo y fijamos los valores de los parámetros (beta0, beta1, sigma)
  # Y = beta0 + beta1*x + e
  beta0 = 615  
  beta1 = 24
  sigma = sqrt(120000)
  
  nivelesX = 9
  repeticiones = 8
  # total de puntos
  n = nivelesX*repeticiones
  
  #simulamos muestreo: valores de X: valores de la var explicatoria. Caso de regresion con replicas
  
  X <- rep(seq(50, 90, length=nivelesX), repeticiones)
  
  set.seed(semilla)
  #valores de los residuos provenientes de una distribución normal
  e = rnorm(n,0,sigma)
  
  #valores simulados de Y bajo el modelo
  Y = beta0 + beta1*X + e
  
  #graficamos
  # plot(X,Y)
  # abline(lm(Y~X), col="red")
  bd <- cbind.data.frame(X,round(Y,2))
  colnames(bd)<-c("Cob", "Rend")
  return (bd)
}

ui <-fluidPage(
  titlePanel("Rendimientos"
  ),
    fluidRow(
      column(6,
      h3("Rendimiento de girasol en el oeste de la provincia de Buenos Aires  (Ejemplo adaptado de Perelman, S. B., Garibaldi, L. A., & Tognetti, P. M. (2019). Experimentación y modelos estadísticos)"), 
      
      p(" Se desea ajustar un modelo para predecir el rendimiento del girasol (kg/ha) en el oeste de la provincia de Buenos Aires en base a la cobertura.  Se sabe que la relación entre ambas variables es lineal y directa. 
        Se realiza un estudio en la localidad de Carlos Casares. Se eligieron 9 valores de cobertura del suelo entre 50 y 90 % y dentro de cada valor se seleccionaron al azar 8 lotes a los que luego se les registró el valor de rendimiento (en kg/ha). Cada estudiante realiza el ensayo y obtiene un conjunto de datos (x, y).
        Estimar la ordenada al origen, la pendiente y el R2 del modelo ajustado.",style = "font-size: 20px;"),
             
      numericInput("libreta", "numero de libreta:", min = 1, max = 100000, value = 12906),
      downloadButton("downloadData", "Descargar")
    ),
    column(2,
      textOutput("txtOutput"), 
      tableOutput("datos")
    ),
    column(4,
           h3("Manos a la obra"),
          
           numericInput("beta_0", "Indique el valor estimado del intercepto", min =1, max =500, value = "?"),
           textOutput("chequeo_beta_0"),
           
           br(), br(), br(),
           numericInput("beta_1", "Indique el valor estimado de la pendiente", min =1, max =500, value = "?"),
           textOutput("chequeo_beta_1"),
           br(), br(), br(),
           
           numericInput("R_cuad", "Indique el valor obtenido de R^2", min =1, max =500, value = "?"),
           textOutput("chequeo_R_cuad"),
           
           
                      )
    

    
    )
  
)

server <- function(input,output){
  
  salida <- reactive({
    # if(input$ejercicio=="marcaA"){
    #    dd <- mermeladaA(input$obs,input$libreta)
    # }
    # if(input$ejercicio=="marcaB"){
    #    dd <- mermeladaB(input$obs,input$libreta)
    # }
dd <- generacionMuestra(input$libreta)
        dd
  })

  
  
  output$datos <- renderTable({
    salida()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos_libreta_", input$libreta, ".txt", sep = "")
    },
    content = function(file) {
      write.table(salida(), file, row.names = TRUE)
    }
  )
    
  
  
  #############################
  #Preguntas
  
  
  
  beta_0_ok <- reactive({
    ajuste <- lm(salida()$Rend~salida()$Cob)
    pp <- ajuste$coefficients[1]
    pp
  })
  
  
  output$chequeo_beta_0 <- renderText({
    dif<-as.numeric(try(abs(input$beta_0-beta_0_ok()),TRUE))
    if(is.na(dif)){ans <-"Ingresar la estimación pedida"
      }else{
      if(dif<0.1){ans <- "¡Fantástico!"}
      else {ans <- "Intentelo nuevamente!"}}
    ans
  })
  
  
  
  
  
  beta_1_ok <- reactive({
    ajuste <- lm(salida()$Rend~salida()$Cob)
    pp <- ajuste$coefficients[2]
    pp
  })
  
  
  output$chequeo_beta_1 <- renderText({
    dif<-as.numeric(try(abs(input$beta_1-beta_1_ok()),TRUE))
    if(is.na(dif)){ans <-"Ingresar la estimación pedida"
    }else{
      if(dif<0.1){ans <- "¡Genial!"}
      else {ans <- "Intentelo nuevamente!"}}
    ans
  })
  
  
  
  
  R_cuad_ok <- reactive({
    ajuste <- lm(salida()$Rend~salida()$Cob)
    pp <- summary(ajuste)$r.squared
    pp
  })
  
  
  output$chequeo_R_cuad <- renderText({
    dif<-as.numeric(try(abs(input$R_cuad-R_cuad_ok()),TRUE))
    if(is.na(dif)){ans <-"Ingresar el valor solicitado"
    }else{
      if(dif<0.1){ans <- "¡Muy Bien!"}
      else {ans <- "Intentelo nuevamente!"}}
    ans
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)

