
####################################################3
########################################################33
library(shiny)
# library(vroom)
# library(ggplot2)
# library(shinycssloaders)
# library(caret)
# library(dplyr)
# library(latex2exp)
library(shinythemes)
library(shinyjs)

genero_gas1 <- function(n,semilla)
{
  set.seed(semilla)
  salida<-rep(NA,n)
  for(i in 1:n){
    salida[i]<-round(rnorm(1,70,3),2)
  }
  dd<-data.frame(salida)
  colnames(dd)<-c("gas_equipo_1")
  return(dd)
}



genero_gas2 <- function(n,semilla)
{
  set.seed(semilla)
  salida<-rep(NA, n)
  for(i in 1:n){
    salida[i]<-round(runif(1,67,73),2)
  }
  dd<-data.frame(salida)
  colnames(dd)<-c("gas_equipo_2")
  return(dd)
}


genero_lamparas <- function(n,semilla)
{
  lambda<-1/10
  set.seed(semilla)
  salida<-rep(NA, n)
  for(i in 1:n)
  {salida[i]<-round(rexp(1,lambda),1)
  }
  dd<-data.frame(salida)
  colnames(dd)<-c("lamparas")
  return(dd)
}




genero_dados <- function(n,semilla)
{
  set.seed(semilla)
  salida<-rep(NA, n)
  for(i in 1:n){
    salida[i]<-sum(sample(1:6,2, replace = TRUE))
  }
  dd<-data.frame(salida)
  colnames(dd)<-c("sumaDados")
  return(dd)
}

genero_dos_dados <- function(n,semilla)
{
  set.seed(semilla)
  salida<-matrix(NA, n,2)
  for(i in 1:n){
    salida[i,]<-sample(1:6,2, replace = TRUE)
  }
  dd<-data.frame(salida)
  colnames(dd)<-c("dadoAzul","dadoRojo")
  return(dd)
}


ui <-fluidPage(
  titlePanel("Cálculo de Frecuencias Relativas "
             # app title/description
  ),
  
  tabsetPanel( 
    type = 'tabs', id = "tabs_upload",
    tabPanel("La propuesta",
             h4("A lo largo de de cada una de las actividades propuestas en las diferentes pestañas, 
     te vamos a dar datos personalizados 
    procurando  emular una situación  experimental,  donde cada uno obtiene sus propios resultados. 
    También permitimos que puedas elegir la cantidad n de datos con la que querés trabajar, 
    de modo que, al agrandar  el valor de n, 
       vamos agregando más valores experimentales. Para eso, necesitas utilizar
       un número de identificación que garantice la reproducibilidad 
       de la actividad; es decir,  que  siempre te demos los mismos n datos una vez que inidiques 
       las cantidad  querés utilizar. 
       Cuando completes  la respuesta de un ejercicio, 
       podrás verificar si el resultado es correcto o no."),
             
             br(),
             h4("Para poder visualizar bien el material, necesitas desplegar la ventana  ocupando la pantalla completa")
             
    ),
    
    
    
    
    tabPanel("Dos dados", 
             hr(),
             br(),
             
             fluidRow(
               column(5,
                      
                      h4("Se lanzan dos dados y se resistra el valor de cada uno de ellos.
                             Ingresa tu número de identificación 
                    y la cantidad de datos n de veces que repetís el experimento."),
                      numericInput("libretadados", "numero de libreta:", min = 1, max = 100000000, value = 24292),
                      
                      numericInput("obsdados", "Cantidad de datos (entre 1 y 1000):", min = 1, max = 1000, value = 15),
                      
                      hr(),
                      
                      
                      
               ),
               column(3,  
                      downloadButton("downloadDatadados", "Descargar datos"),
                      #textOutput("datos"), 
                      tableOutput("datosdados")
               ),
               column(4,
                      h4("Utilizando los datos, 
                      calcular la frecuencia relativa correspondiente a cada uno 
                         de los siguientes eventos, utilizando 4 decimales  "),
                      
                      br(),
                      
                      h4("1) A=", em("la suma es igual a 7")),
                      h4(div(textOutput("verificodados1"), style = "color:blue")),  
                      numericInput("respuestadados1","", min =0, max =10000, value = "?"),
                      br(),
                      
                      hr(),
                      h4("2)  B=",em(" la suma es igual a 2")),
                      
                      h4(div(textOutput("verificodados2"), style = "color:blue")),  
                      numericInput("respuestadados2","", min =0, max =10000, value = "?"),
                      br(),
                      
                      hr(),
                      h4("3) C=", em("la suma es mayor o igual a 10")),
                      
                      h4(div(textOutput("verificodados3"), style = "color:blue")),
                      numericInput("respuestadados3","", min =0, max =10000, value = "?"),
                      br(),
                      
                        
                      
                      hr(),
                      h4("4) D=", em("los dados son inguales")),
                      
                      h4(div(textOutput("verificodados4"), style = "color:blue")),
                      numericInput("respuestadados4","", min =0, max =10000, value = "?"),
                      br(),
                      
                      
                      
                      hr(),
                      h4("5) E=", em("el resultado del  dado Rojo es par")),
                      
                      h4(div(textOutput("verificodados5"), style = "color:blue")),
                      numericInput("respuestadados5","", min =0, max =10000, value = "?"),
                      br(),
                      
                      
                      
               )
             )
    ),
    
    
    tabPanel("Lámparas",
             hr(),
             br(),
             
             fluidRow(
               column(5,
                      
                      h4("Se registra el tiempo (en meses) de duración de lámparas producidas por cierta fábrica.
                  Ingresa tu número de identificación
                     y la cantidad n de lámparas cuyo tiempo de duración registrás. "),
                      numericInput("libretalamparas", "numero de libreta:", min = 1, max = 100000000, value = 24292),
                      
                      numericInput("obslamparas", "Cantidad de datos (entre 1 y 1000):", min = 1, max = 1000, value = 15),
                      
                      hr()
                      
                      
                      
               ),
               column(2,
                      downloadButton("downloadDatalamparas", "Descargar datos"),
                      #textOutput("datos"), 
                      tableOutput("datoslamparas")
               ),
               column(5,
                      h4("Utilizando los datos, 
                       calcular la frecuencia relativa correspondiente a cada uno 
                          de los siguientes eventos, utilizando 4 decimales"),
                      
                      br(),
                      
                      h4("1) A=", em("la lámpara dura al menos un año (12 meses)")),
                      h4(div(textOutput("verifico1lamparas"), style = "color:blue")),  
                      numericInput("respuesta1lamparas","", min =0, max =10000, value = "?"),
                      br(),
                      
                      hr(),
                      h4("2) B=", em("la lámpara dura a lo sumo  dos años.")),
                      
                      h4(div(textOutput("verifico2lamparas"), style = "color:blue")),
                      numericInput("respuesta2lamparas","", min =0, max =10000, value = "?"),
                      br()
                      
               )      
               
             )
    ),
    tabPanel("Mediciones",
             hr(),
             br(),
             
             fluidRow(
               column(5,
                      
                      h4("Se mide la concentración de monoxido de carbono (ppm) en una dada muestra de gas. 
                       Ingresa tu número de identificación y la cantidad n de mediciones que realizas.  "),
                      numericInput("libreta", "numero de libreta:", min = 1, max = 100000000, value = 24292),
                      
                      numericInput("obs", "Cantidad de datos (entre 1 y 1000):", min = 1, max = 1000, value = 15),
                      
                      hr()
                      
                      
                      
               ),
               column(2,
                      downloadButton("downloadData", "Descargar datos"),
                      #textOutput("datos"), 
                      tableOutput("datos")
               ),
               column(5,
                      h4("Utilizando los datos, 
                       calcular la frecuencia relativa correspondiente a cada uno 
                          de los siguientes eventos, utilizando 4 decimales"),
                      
                      br(),
                      
                      h4("1) A=", em("el valor de la medición es menor o igual a 72")),
                      h4(div(textOutput("verifico1"), style = "color:blue")),  
                      numericInput("respuesta1","", min =0, max =10000, value = "?"),
                      br(),
                      
                      hr(),
                      h4("2) B=",em(" la medición dista de 70 en dos unidades o menos.")),
                      
                      h4(div(textOutput("verifico2"), style = "color:blue")),
                      numericInput("respuesta2","", min =0, max =10000, value = "?"),
                      br()
                      
               )      
               
             )
    )
    
    
    
    
  )
)



# Define server logic required to draw a histogram
server <- function(input,output){
  
  salida <- reactive({
    dd<-genero_gas1(input$obs,input$libreta)
    dd
  })
  
  output$datos <- renderTable({
    salida()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos_",input$ejercicio,"libreta", input$libreta, "n_", input$obs, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(salida(), file, row.names = FALSE, col.names = TRUE, sep = "")
      # write.table(datos(), file, row.names = TRUE)
    }
  )
  
  output$verifico1<- renderText({
    tama <- dim(salida())[1]
    calculo <- (sum(salida()[,1]<=72))/tama
    dif <- as.numeric(try(abs(input$respuesta1-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  
  output$verifico2<- renderText({
    tama <- dim(salida())[1]
    calculo <-1- (sum(salida()[,1]>72)+sum(salida()[,1]<68))/tama
    dif <- as.numeric(try(abs(input$respuesta2-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  ############################################
  #dado
  salidadados <- reactive({
    dd<-genero_dos_dados(input$obsdados,input$libretadados)
    dd
  })
  
  output$datosdados <- renderTable({
    salidadados()
  })
  
  
  output$downloadDatadados <- downloadHandler(
    filename = function() {
      paste("datos_",input$ejercicio,"libreta", input$libretadados, "n_", input$obsdados, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(salidadados(), file, row.names = FALSE, col.names = TRUE, sep = "")
      # write.table(datos(), file, row.names = TRUE)
    }
  )
  
  output$verificodados1<- renderText({
    suma <- apply(salidadados(),1,sum)
    tama <-  length(suma)
    calculo <- (sum(suma==7))/tama
    dif <- as.numeric(try(abs(input$respuestadados1-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  
  output$verificodados2<- renderText({
    suma <- apply(salidadados(),1,sum)
    tama <-  length(suma)
    calculo <-sum(suma==2)/tama
    dif <- as.numeric(try(abs(input$respuestadados2-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  output$verificodados3<- renderText({
    suma <- apply(salidadados(),1,sum)
    tama <- length(suma)
    calculo <-sum(suma>=10)/tama
    dif <- as.numeric(try(abs(input$respuestadados3-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  
  output$verificodados4<- renderText({
    mios <- salidadados()
    calculo <-mean(mios[,1]==mios[,2])
    dif <- as.numeric(try(abs(input$respuestadados4-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  
  output$verificodados5<- renderText({
    mios <- salidadados()
    calculo <-mean(mios[,2]==2)+mean(mios[,2]==4)+mean(mios[,2]==6)
    dif <- as.numeric(try(abs(input$respuestadados5-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  ###############################3
  #lamparas
  #dado
  salidalamparas <- reactive({
    dd<-genero_lamparas(input$obslamparas,input$libretalamparas)
    dd
  })
  
  output$datoslamparas <- renderTable({
    salidalamparas()
  })
  
  
  output$downloadDatalamparas <- downloadHandler(
    filename = function() {
      paste("datos_",input$ejercicio,"libreta", input$libretalamparas, "n_", input$obslamparas, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(salidalamparas(), file, row.names = FALSE, col.names = TRUE, sep = "")
      # write.table(datos(), file, row.names = TRUE)
    }
  )
  
  output$verifico1lamparas<- renderText({
    tama <- dim(salidalamparas())[1]
    calculo <- (sum(salidalamparas()[,1]>=12))/tama
    dif <- as.numeric(try(abs(input$respuesta1lamparas-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
  
  output$verifico2lamparas<- renderText({
    tama <- dim(salidalamparas())[1]
    calculo <-sum(salidalamparas()[,1]<24)/tama
    dif <- as.numeric(try(abs(input$respuesta2lamparas-calculo),TRUE))
    if(is.na(dif)){
      resp <- "Esperando respuesta"
    }else{
      if(dif<0.0001){
        resp <- sample(c("¡Correcto! ", " ¡Muy Bien!", "¡Genial! "),1)
      }else{
        resp <- "Inténtelo nuevamente !!"
      }
    }
    return(resp)
  })
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)

