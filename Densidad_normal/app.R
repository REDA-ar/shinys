library(shiny)
library(ggplot2)
library(shiny)
library(vroom)
library(ggplot2)
library(shinycssloaders)
library(caret)
library(dplyr)
library(latex2exp)
library(shinythemes)
library(shinyjs)


# hola <- genero.alturas_Masulinos(100,24292)
# class(hola)

load <- function() {
  if ( require( "ggplot2" ) != TRUE ) {
    print( "Required library 'ggplot2' could not be loaded" )
    return( FALSE )
  } else if ( require( "shiny" ) != TRUE ) {
    print( "Required library 'shiny' could not be loaded" )
    return( FALSE )
  } else {
    return( TRUE )
  }
}

if ( load() == TRUE ) {
  den_ui <-  fluidPage(
    withMathJax(), 
    titlePanel( "La densidad normal" ),
    
    sidebarLayout(
     
        sidebarPanel(
       
          includeHTML("densidad.html"),
        
        
      
      ),
      
      
      
      mainPanel(
        
        sliderInput(
          "sigma",
          "Desvío=\\( \\sigma \\)",
          min = 0.5,
          max = 5,
          step = 0.01,
          value = 1
        ),
        
        
        sliderInput(
          "mu",
          "Esperanza= \\( \\mu \\) ",
          min = -10,
          max = 10,
          step = 0.01,
          value = 0
        ),
        
       
        plotOutput( "distPlot" )
      )
    )
  ) 
  
  den_server <- function( input, output ) {
#  hola <- reactive({    
#    p <- genero.alturas_Masulinos(input$obs,input$libreta)
#     p
#  })
# #    datof <- reactive( {return( subset( datossimu, tipo == input$Tipo) )    } )
 #   limitex<-c(range(datosf$datos)[1],range(datosf$datos)[2])
    
 
 
 
 #pirulo<-function(x)^2
 
 
 grafico<- reactive( {
   xx <- seq(-10,10,0.001)
   yy <- dnorm(xx,mean=input$mu,sd=input$sigma)
   
   plot(xx,yy, ylim=c(0, dnorm(0,0,0.5)), xlab="", ylab="")
       #ggplot( aes(xx,yy)) 
      #   stat_function(fun=dnorm(),args = list(mean=input$mu,sd=input$sigma))
 })     
      
    output$distPlot <-   renderPlot({
   print(grafico())
 })
 
 
   } 
  
} 

shinyApp( ui = den_ui, server = den_server )
