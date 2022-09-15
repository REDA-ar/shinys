#https://anibal.shinyapps.io/shiny/
####################################################3
########################################################33
library(shiny)
library(vroom)
library(ggplot2)
library(shinycssloaders)
library(caret)
library(dplyr)
library(latex2exp)
library(shinythemes)
library(shinyjs)

predigo_movil_ventana<- function(X,Y,xNuevo,h){
  if(sum(X>=(xNuevo-h) & X<=(xNuevo+h))==0){predigo<- NA}
  else{indices<-which(X>=(xNuevo-h) & X<=(xNuevo+h))
  predigo<-mean(Y[indices])}
  predigo
}




mum<-159.2
peso.madre<-0.5
muh<-2*(0.5*mum+6.5) #172.2
desvio<-3
desvio_hijo<-desvio/sqrt(2)
factor<-1.067
dec<-1



contextura_madre<-function(madre)
{
  if (madre < round(mum - factor * desvio, 0)) {
    salida <- "bajita"
  }
  else if (madre > round(mum + factor * desvio, 0)) {
    salida <- "alta"
  }
  else {
    salida <- "mediana"
  }
  
  salida
}




genero.alturas<-function(nsample, semilla)
{ 
  set.seed(semilla)
  datos_alturas<-data.frame(id=seq(1:nsample), altura=rep(NA,nsample),
                            genero=rep(NA,nsample), contextura_madre=rep(NA,nsample), altura_madre=rep(NA,nsample))
  
  for(i in 1:nsample)
  {
    altura_padre<-round(rnorm(1,muh,desvio),dec)
    altura_madre<-round(rnorm(1,mum,desvio),dec)
    genero<-sample(c("F","M"), replace=TRUE,1)
    sesgo_genero<-6.5*(genero=="M")-6.5*(genero=="F")
    altura_individuo<-round(((1-peso.madre)*altura_padre+peso.madre*altura_madre)
                            +rnorm(1,0,desvio_hijo)+
                              sesgo_genero,dec)
    categoria_altura_madre <- contextura_madre(altura_madre)
    
    datos_alturas[i,-c(3,4)]<-c(i,altura_individuo,altura_madre)
    datos_alturas[i,c(3,4)] <-c(genero, categoria_altura_madre)
  }
  
  
  return(datos_alturas)
}

variables <- c("altura","genero", "contextura_madre",
               "altura_madre")



genero.alturas_Masulinos<-function(nsample, semilla)
{ 
  set.seed(semilla)
  datos_alturas<-data.frame(altura_madre=rep(NA,nsample), altura=rep(NA,nsample))
  
  for(i in 1:nsample)
  {
    altura_padre<-round(rnorm(1,muh,desvio),dec)
    altura_madre<-round(rnorm(1,mum,desvio),dec)
    genero<-"M"
    #  sample(c("F","M"), replace=TRUE,1)
    sesgo_genero<-6.5*(genero=="M")-6.5*(genero=="F")
    altura_individuo<-round(((1-peso.madre)*altura_padre+peso.madre*altura_madre)
                            +rnorm(1,0,desvio_hijo)+
                              sesgo_genero,dec)
    
    datos_alturas[i,]<-c(altura_madre,altura_individuo)
  }
  
  colnames(datos_alturas) <- c("predictora", "respuesta")  
  
  return(datos_alturas)
}




punta_grafico <- function(semilla)
{set.seed(semilla)
  x0 <- sample(-10:10,1)
  y0 <- sample(-5:5,1)
  salida <- c(x0,y0)
  salida
}

grafico<- function(x0,y0)
{
  delta <- 0.01
  paso <- 0.01
  
  fgrafico <- function(x) {(- x^{2}*(x+1)/10+2)*(-3<=x )*(x<=(-1))+
      (1/abs(x)^{1/2})*((-1)<x)*(x<0)+(1/x)*(x>0)+y0}
  
  ff <- function(x){fgrafico(x-x0)}
  
  a <- -3+x0
  b <- -1+x0
  c <- 0+x0
  d <- 5+x0
  
  grilla1 <- seq(a,b-delta,by=paso)
  grilla2 <- seq(b+delta,c-delta,by=paso)
  grilla3 <- seq(c+delta,d,by=paso)
  
  plot(grilla1,sapply(grilla1, ff), col=2, xlim=c(a,d), type="l", ylim=c(0+y0,5+y0), xlab="",  ylab="")
  lines(grilla2,sapply(grilla2, ff), col=2)
  lines(grilla3,sapply(grilla3, ff), col=2)
  points(-1+x0,ff(-1+x0), col="red", pch=19)
  points(-1+x0,ff(-1+x0+paso), col="red")
}

acumulada <- function(semilla)
{
  set.seed(semilla)
  n1 <- sample(2:10,1)
  k <- sample(2:4,1)
  n2 <- n1+k
  datos <- sample(n1:n2, 10, replace = TRUE)
  
  n_disc <- length(unique(datos))
  
  sol1 <- length(unique(datos))
  sol2 <- min(unique(datos))
  sol3 <- 0
  sol4 <- 1
  salida <- c(datos, sol1,sol2)
  salida
}

lineal_1 <- function(semilla)
{
  set.seed(semilla)
  m <- sample(c(-3,-2,-1,1,2,3),1)
  b <- sample(-5:5,1)
  
  xx <- sample(-10:10,2, replace = FALSE)
  yy <- m*xx+b
  
  xnew <- sample(1:4,1)
  ynew <- m*xnew+b
  #eval(parse(text = paste0("suc", sample(1:2,1))))(sample(1:10,1),a,1) esto es para elegir la funcion que quiero usar.
  
  sol1 <- m
  sol2 <- b
  sol3 <- ynew
  salida <- c(xx,yy,xnew,sol1,sol2,sol3)
  salida
}

enes <- function(semilla)
{
  set.seed(semilla)
  salida <- sample(3:15,6, replace=FALSE)
  salida[6] <- 2
  salida
}
aa <- function(n)
{
  salida <-   1/sqrt(n)+(1/2)^n
  salida
}


bb <- function(n)
{
  salida <- n
  salida
}


cc <- function(n)
{
  salida <-(-1)^(n+3)/ n^2
  salida
}

dd <- function(n)
{
  salida <-sin(n*pi/2)/(n*cos(n*pi)+3)
  salida
}


ee <- function(n)
{
  salida <-sin(n*pi/2)/(n*cos(n*pi)+3)
  salida
}



ff <- function(n)
{
  salida <-(2^n+5^(n+1))/3
  +7
  salida
}



ui <-fluidPage(
  titlePanel("Alturas - Primeras Predicciones  "
             # app title/description
  ),
  
  tabsetPanel( 
    type = 'tabs', id = "tabs_upload",
    tabPanel("La propuesta",
             h4("A lo largo de esta aplicación   vamos a ir resolviendo 
             los ejercicios que aparecen en las pestañas. Los valores númericos son individuales
             y para garantizar  la reproducibilidad 
       de la actividad, es decir,  que  siempre te demos los mismos enunciados)  necesitas ingresar un número de identificación. 
       Cuando completes  la respuesta de un ejercicio, 
       podrás verificar si el resultado es correcto o no. También podes descargar una base con tus 
                datos."),
             br(),
             numericInput("obs0", "Número de observaciones para descargar:", min = 1, max = 10000, value = 50),
             
             br(),
             br(),
             numericInput("libreta", "Número de identidicación:", min = 1, max = 100000000, value = 24292),
             br(),
           downloadButton("downloadData", "Descargar datos"),
             
            # textInput("nombre", "Ingresa tu nombre", value = "", width = NULL, placeholder = NULL),
             br(),
             br(),
             h4("Para poder visualizar bien el material, necesitas desplegar la ventana  ocupando la pantalla completa")
             
    ),
    
    
    
    
    tabPanel("Predictora Categórica", 
             withMathJax(),
             hr(),
             br(),
             h4("Vamos ahora a considerar el género del hije y la contextura de la madre."),
             
             
             fluidRow(
               column(3,
                      
                      numericInput("obs1", "Cantidad de Individuos:", min = 1, max = 1000, value = 50),
                  #    numericInput("libreta", "Número de Indentificación", min = 1, 
                      #             max = 1000, value = 24292)
                      #   downloadButton("downloadData", "Descargar")
                      
                      
               ),
               
               column(6,
                      headerPanel(h4("TUS datos")),
                      #       plotOutput("grafico1"),
                      
                      
                      tableOutput("datos1")
                      
               ),
               column(3,
                      headerPanel(h4("Manos a la obra")),
                      
                      #tableOutput("prediccion_nos"),
                      
                      #   checkboxInput("ordenado", "Quiere ordenar los datos según la altura de la madre?",  value = FALSE),
                      
                      checkboxGroupInput("sexo1", "Elija género del hijE", c("F", "M"), selected = "?"),
                      checkboxGroupInput("contextura", "Elija la contextura de la madre", 
                                         c("bajita", "mediana", "alta"), selected = "?"),
                      
                      
                      #           numericInput("x_madre_nueva", "Altura de la nueva madre:", min = 1, max =500, value = 156),
                      
                      #          numericInput("ventana", "Tamaño de la ventana", min=0.01, max=20, value = 1), 
                      
                      numericInput("prediccion1", "Prediga la altura del hijE:", min =1, max =500, value = "?"),
                      textOutput("chequeo_prediccion1"),
                      #tableOutput("media_nos"),
                      # numericInput("mediana", "Median:", min = -100000, max =100000, value = "?"),
                      # textOutput("chequeo_prediccion"),
                      
                      
                      
               )
             )
    ),
    tabPanel("¿Graficamos?", 
             
             sidebarPanel(
               numericInput("obs3", "Cantidad de Datos:", 
                            min = 1, max = 10000, value = 50),
               sliderInput(
                 "xnew",
                 "Altura de la nueva madre:",
                 min = 150,
                 max = 180,
                 step = 0.01,
                 value = 156
               ),
               
               #         numericInput("xnew", "Altura de la nueva madre:", min = 1, max =500, value = 156),
               
               sliderInput(
                 "ventana3",
                 "Ventana",
                 min = 0.1,
                 max = 20,
                 step = 0.001,
                 value = 1
               ),
               
               
               radioButtons(
                 "toca",
                 "Individuos en la muestra con el valor de la altura_madre pedido:",
                 c("NO","SI"),
                 inline = TRUE,
                 selected = "NO"
               ),
               
               
               radioButtons(
                 "barrita",
                 "Incluir representación ventana:",
                 c("NO","SI"),
                 inline = TRUE,
                 selected = "NO"
               ),
               
               radioButtons(
                 "suavizado",
                 "Incluir Gráfico con Predicciones:",
                 c("NO","SI"),
                 inline = TRUE,
                 selected = "NO"
               ),
               radioButtons(
                 "MC",
                 "Incluir recta de cuadrados minimos:",
                 c("NO","SI"),
                 inline = TRUE,
                 selected = "NO"
               ),
               
             ),
             mainPanel(
               plotOutput( "distPlot" )
             )
    ),
    
    
    
    
    tabPanel("Predecimos con Altura madre", 
             withMathJax(),
             hr(),
             br(),
             
             fluidRow(
               column(3,
                      
                      numericInput("obs2", "Cantidad de Individuos:", min = 1, max = 1000, value = 50),
                      #numericInput("libreta", "Número de Indentificación", min = 1, 
                       #            max = 1000, value = 24292),
                      #   downloadButton("downloadData", "Descargar")
                      
                      
               ),
               
               column(6,
                      
                      #plotOutput(grafico1),
                      
                      headerPanel(h4("TUS datos")),
                      #       plotOutput("grafico1"),
                      
                      
                      tableOutput("datos2")
                      
               ),
               column(3,
                      headerPanel(h4("Manos a la obra")),
                      
                      #tableOutput("prediccion_nos"),
                      
                      checkboxInput("ordenado", "Quiere ordenar los datos según la altura de la madre?",  value = FALSE),
                      checkboxGroupInput("sexo2", "Elija género del hijE", c("F", "M"), selected = "?"),
                      
                      
                      numericInput("x_madre_nueva", "Altura de la nueva madre:", min = 1, max =500, value = 156),
                      
                      numericInput("ventana", "Tamaño de la ventana", min=0.01, max=20, value = 1), 
                      
                      numericInput("prediccion2", "Prediga la altura del hijE:", min =1, max =500, value = "?"),
                      textOutput("chequeo_prediccion2"),
                      #tableOutput("media_nos"),
                      # numericInput("mediana", "Median:", min = -100000, max =100000, value = "?"),
                      # textOutput("chequeo_prediccion"),
                      
                      
                      
               )
             )
    )
             
             
    )
    
    
  )




# Define server logic required to draw a histogram
server <- function(input,output){
  
  
  # 

  alturasdescarga <- reactive({
    dd <- genero.alturas(input$obs0, input$libreta)
    dd
  })


  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("alturas_identificacion_",
            input$libreta,
            "_n_",
            input$obs0,
            ".csv",
            sep = "")},
    content = function(file) {
      write.csv(
        alturasdescarga(),
        file,
        row.names = FALSE,
        col.names = TRUE,
        sep = "")})





  # 
  # 
  # 
  
  
  
###################################
  #Predictoras Categoricas
  
alturas <- reactive({
  dd <- genero.alturas(input$obs1, input$libreta) 
  dd
})


  
  muestro1 <- reactive({
    if (length(input$sexo1)==1)
    { pepe <-   alturas()[alturas()$genero==input$sexo1,c(3,4,2 )]}
    else {pepe  <-alturas()[,c(3,4,2 )] }
    
    if (0<length(input$contextura) & length(input$contextura)<3)       
    {salida <- pepe[pepe$contextura==input$contextura,]}
    else {salida <- pepe}
    salida
    
  })
  
  
  output$datos1 <- renderTable({
    muestro1()}, rownames = TRUE)  
  
  
  
  
  predigo1 <- reactive({
    p <- mean(muestro1()$altura)
    #  p <- ksmooth(muestro()$altura_madre,muestro()$altura, "box", bandwidth = input$ventana, x.points = input$x_madre_nueva)[[2]]
    p     
  })
  
  

  output$chequeo_prediccion1 <- renderText({
    dif<-as.numeric(try(abs(input$prediccion1-predigo1()),TRUE))
    if(is.na(dif)){ans <- "Ingrese su predicción"}else{
      if(dif<0.1){ans <- "Fantástico!"}
      else {ans <- "Intentelo nuevamente!"}}
    ans
  })
  
  #########################################################
  #predictora continua
  alturas2 <- reactive({
    dd <- genero.alturas(input$obs2, input$libreta) 
    dd
  })
  
  muestro2 <- reactive({
    if (length(input$sexo2)==1)
    { pepe <-   alturas2()[alturas2()$genero==input$sexo2,c(3,5,2 )]}
    else {pepe  <-alturas2()[,c(3,5,2 )] }
    
    if (input$ordenado==TRUE)       
    {salida <- pepe[order(pepe$altura_madre),]}
    else {salida <- pepe}
    salida
    
  })
  
  
  output$datos2 <- renderTable({
    muestro2()}, rownames = TRUE)  
  
  
  
  
  predigo2 <- reactive({
    p <- predigo_movil_ventana(X=muestro2()$altura_madre,Y=muestro2()$altura,
                               xNuevo=input$x_madre_nueva,h=input$ventana)
    #  p <- ksmooth(muestro()$altura_madre,muestro()$altura, "box", bandwidth = input$ventana, x.points = input$x_madre_nueva)[[2]]
    p     
  })
  
  
  # output$prediccion_nos <-  renderTable({predigo()})  
  
  output$chequeo_prediccion2 <- renderText({
    dif<-as.numeric(try(abs(input$prediccion2-predigo2()),TRUE))
    if(is.na(dif)){ans <- "Ingrese su predicción"}else{
      if(dif<0.1){ans <- "Fantástico!"}
      else {ans <- "Intentelo nuevamente!"}}
    ans
  })
  
  
#################################
  hola <- reactive({    
    p <- genero.alturas_Masulinos(input$obs3,input$libreta)
    p
  })
  grafico<- reactive( {
    pirulo <- function(x, h)
    {
      salida <-   predigo_movil_ventana(hola()$predictora,hola()$respuesta,x,h)
      # salida <- ksmooth(hola()$predictora,hola()$respuesta,x.points=x,bandwidth =h)[[2]]
      salida
    }
    v_pirulo <- Vectorize(pirulo)
    
    ven<-input$ventana3
    
    
p <-     ggplot(hola(), aes(x=predictora,y=respuesta)) +geom_point()

if(input$toca=="SI"){
  p <- p+geom_vline(xintercept=input$xnew, linetype=4)}
  

      if(input$barrita=="SI"){
        p <- p+
                annotate("rect", xmin =input$xnew-ven,
                 xmax =input$xnew+ven , ymin =-Inf,
                 ymax = Inf, fill="blue",
                 alpha = .2)+
          geom_vline(xintercept=input$xnew, linetype=4)+
          
            geom_point(aes(x=input$xnew,y=pirulo(input$xnew,ven)) ,col="blue",
                     size=2)}

      # 
      # 
       if(input$suavizado=="SI"){
        p <- p+stat_function(fun=v_pirulo,args = list(h=ven), col="green")}

      # 
      # #stat_function(fun=pirulo,args = list(h=ven), col="green")+ 
      if(input$MC=="SI"){
      p <- p+  geom_smooth(method ="lm", se=FALSE)}
    # geom_vline(xintercept=156.5, linetype=1, size=10*ven, alpha=0.2)
    p
  })     
  
  output$distPlot <-   renderPlot({
    print(grafico())
  })
  
  


  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
