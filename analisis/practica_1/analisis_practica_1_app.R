#copia con previsualizacion y todo a la izquierda
#https://maria-lara-gauder.shinyapps.io/TP-final-Reg-NP/
rm(list = ls())
library(shiny)
library(vroom)
library(ggplot2)
library(shinycssloaders)
library(caret)
library(dplyr)
library(latex2exp)
library(shinythemes)
library(shinyjs)
library(rmarkdown)

interpolador <- function(x_nuevo,x_tabla,y_tabla)
{
  
  n_puntos <- length(x_tabla) #cuantos puntos voy a interpolar
  l <- rep(NA,n_puntos)
  for(i in 1:n_puntos) # ojo, recorremos empezando en 1, no en cero. 
  {
    l[i] <-   prod(x_nuevo-x_tabla[-i])/prod(x_tabla[i]-x_tabla[-i])
  }
  sum(y_tabla*l)  
}


fun_lineal <- function(x,m,b)
{
  salida <- m*x+b
  return(salida)
}



fun_cuad <- function(x,a,b,c)
{
  salida <- a*x^2+b*x+c
  return(salida)
}

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  
  titlePanel("Cuadrados mínimos"),
  tabsetPanel( 
    type = 'tabs', id = "tabs_upload",
    
    
    tabPanel("Los datos",
             fluidRow(
               
               
               column(4,
                      wellPanel(
                        
                        sliderInput("ene",
                                    "Cantidad de Datos:",
                                    min = 1,
                                    max = 10000,
                                    value = 10),
                        
                      )       
               ),
               
               column(8,
                      plotOutput("scatter"),
                      #plotOutput("plotlineal2"),
                      # plotOutput("plot_cuadrados_minimos_recta")
               )
             )
             
    )
    ,
    
    tabPanel("La mejor recta CONSTANTE",
             withMathJax(),
             fluidRow(
               #includeHTML("funcion_constante.html"),
               includeHTML(rmarkdown::render(file.path("rmd","funcion_constante.Rmd"))),
               
               column(4,
                      wellPanel(
                        
                     #   includeHTML("funcion_constante.html"),
                        
                        sliderInput("a_constante",
                                    "\\(a\\) Valor de la constante ",
                                    min = -1000,
                                    max = 1000,
                                    value = 0,
                                    step=0.01),
                        
                        checkboxInput("constante", "Constante ingresada (en azul)" , value = FALSE),
                        textOutput("perdida_constante"),
                        #checkboxInput("puntos",  "Puntos", value=TRUE),
                        checkboxInput("cuad_min_constante", "Constante de Mínimos Cuadrados (en rojo)" , value = FALSE),
                        textOutput("constante_optima")
                        
                        # numericInput("x0", "Valor para x0:", value = 0, min = -10, max =10),
                        # numericInput("y0", "Valor para y0:", value = 0, min =-20 , max = 20)
                        # 
                      )       
               ),
               
               column(8,
                      plotOutput("plotconstante"),
                      #plotOutput("plotlineal2"),
                      # plotOutput("plot_cuadrados_minimos_recta")
               )
             )
             
    ),
    
    
    
    tabPanel("La mejor recta",
             withMathJax(),
             fluidRow(
               #includeHTML("cuadrados_minimos.html"),
               includeHTML(rmarkdown::render(file.path("rmd","cuadrados_minimos.Rmd"))),
               
               column(4,
                      wellPanel(
                        
               #         includeHTML("cuadrados_minimos.html"),
                        
                        sliderInput("m",
                                    "\\(a_1\\): Pendiente de la recta=",
                                    min = -200,
                                    max = 200,
                                    value = 0,
                                    step=0.01),
                        sliderInput("b",
                                    "\\(a_0\\): Ordenada al Origen=",
                                    min = -1000,
                                    max = 1000,
                                    value = 0,
                                    step=0.01),
                        
                        checkboxInput("recta", "Recta con los parámetros ingresados (en azul)" , value = FALSE),
                        
                        
                        textOutput("perdida"),
                        #checkboxInput("puntos",  "Puntos", value=TRUE),
                        checkboxInput("cuad_min", "Recta de cuadrados mínimos  (en rojo)" , value = FALSE),
                        textOutput("recta_optima")
                        
                        # numericInput("x0", "Valor para x0:", value = 0, min = -10, max =10),
                        # numericInput("y0", "Valor para y0:", value = 0, min =-20 , max = 20)
                        # 
                      )       
               ),
               
               column(8,
                      plotOutput("plotlineal"),
                      #plotOutput("plotlineal2"),
                      # plotOutput("plot_cuadrados_minimos_recta")
               )
             )
             
    ),
    
    tabPanel("La mejor cuadrática",
             withMathJax(),
             fluidRow(
               #includeHTML("funcion_cuadratica.html"),
               includeHTML(rmarkdown::render(file.path("rmd","funcion_cuadratica.Rmd"))),

               column(4,
                      wellPanel(
                        
                       # includeHTML("funcion_cuadratica.html"),
#                        
                        sliderInput("a_par",
                                    "\\(a_2\\)",
                                    min = -20,
                                    max = 20,
                                    value = 0,
                                    step=0.01),
                        sliderInput("b_par",
                                    "\\(a_1\\)",
                                    min = -1000,
                                    max = 1000,
                                    value = 0,
                                    
                                    step=0.01),
                        sliderInput("c_par",
                                    "\\(a_0\\)",
                                    min = -1000,
                                    max = 1000,
                                    value = 0,
                                    step=0.01),
                        
                        
                        checkboxInput("cuad", "Cuadrática con los parámetros ingresados (en azul)" , value = FALSE),
                        
                        textOutput("cuad_perdida"),
                        #checkboxInput("puntos",  "Puntos", value=TRUE),
                        checkboxInput("cuad_min_cuadratica", "Cuadrática de cuadrados mínimos (en rojo)" , value = FALSE),
                        textOutput("cuad_optima")
                      )
               ),
               
               column(8,
                      plotOutput("plotcuad")
               )
             )
             
    ),
    
    
    
    tabPanel("CO2",
             withMathJax(),
             fluidRow(
               column(4,
                      wellPanel(
                        
                        
                        
                        checkboxInput("constante_co2", "Mejor Constante" , value = FALSE),
                        checkboxInput("recta_co2", "Mejor Recta" , value = FALSE),
                        checkboxInput("interpolador", "Polinomio Interpolador de Lagrange" , value = FALSE),
                        
                        
                      )       
               ),
               
               column(8,
                      plotOutput("plotco2"),
                      #plotOutput("plotlineal2"),
                      # plotOutput("plot_cuadrados_minimos_recta")
               )
             )
             
    )
    
    
    
    
    
    
    
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  x_4 <- c(8,3,6)
  y_4 <- c(28,19,13)
  
  cuest_4 <- data.frame(x_4,y_4)
  
  output$scatter_4 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    p <- ggplot(data = cuest_4, mapping = aes(x = x_4,y=y_4))+ 
      xlab("predictora")+ylab("respuesta")
    
    
    p <- p+geom_point(col="steelblue", size=2)
    p  
  })
  
  
  output$perdida_4<- renderText({
    salida <- c("El valor de la funcón de pérdida en con los parámetros indicados es ", 
                round(sum((x_4*input$m_4+input$b_4-y_4)^2),2))
    
    
  })
  
  
  
  output$recta_optima_4<- renderText({
    salida <- c()
    if(input$cuad_min_4==TRUE)
    {
      ajuste <-  lm(y_4~x_4)
      a0_opt <- round(ajuste$coefficients[[1]],2)
      a1_opt <- round(ajuste$coefficients[[2]],2)
      
      salida <- c("La recta  de cuadrados mínimos está dada por y=", a0_opt,"+",a1_opt,"x,  y la función
       de perdida en los parámetros óptimos vale", round(sum((x_4*a1_opt+a0_opt-y_4)^2),2))
    }
    salida
  })
  
  
  
  
  
  
  output$plotlineal_4 <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    p <- ggplot(data = cuest_4, mapping = aes(x = x_4,y=y_4))+ 
      xlab("predictora")+ylab("respuesta")
    
    
    p <- p+geom_point(col="steelblue", size=2)
    
    p <- p+
      if(input$recta_4==TRUE)
      {stat_function(fun = fun_lineal,args = list("m"=input$m_4,"b"=input$b_4), col="blue", size=1)}
    
    p <- p+
      if(input$cuad_min_4==TRUE)
      { geom_smooth(method='lm', formula= y~x, se=FALSE, col="red") }
    
    
    p
  })
  
  
  
  
  
  ########################3
  datos<- reactive({
    n <- input$ene
    K <- 10
    sigma <- 100
    set.seed(000)
    xx <- c()
    yy <- c()
    for(i in 1:n)
    {
      xnuevo <- runif(1,0,K)
      xx <- c(xx,xnuevo)
      ynuevo <- xnuevo^3+3*xnuevo-400+rnorm(1,0,sigma)
      
      yy <- c(yy,ynuevo) 
    }
    todo <- data.frame(xx,yy)
    todo
  })
  
  
  output$scatter <- renderPlot({
    # generate bins based on input$bins from ui.R
    tabla <- datos()        
    p <- ggplot(data = tabla, mapping = aes(x = xx,y=yy))+ 
      xlab("predictora")+ylab("respuesta")
    
    
    p <- p+geom_point(col="steelblue", size=2)
    p  
  })
  
  ###########################
  #Mundo constante
  
  output$perdida_constante<- renderText({
    salida <- c("El valor de la funcón de pérdida en con el parámetro indicado es  ", 
                round(sum((input$a_constante-datos()$yy)^2),2))
    
    
  })
  
  
  
  output$constante_optima<- renderText({
    salida <- c()
    if(input$cuad_min_constante==TRUE)
    {
      a_opt <- round(mean(datos()$yy),2)
      salida <- c("La recta  CONSTANTE de cuadrados mínimos está dada por y=", a_opt, ", y el valor 
                            de la función de perdida en el parámetro óptimo es ", round(sum((mean(datos()$yy)-datos()$yy)^2),2))
      
    }
    salida
  })
  
  
  
  
  
  
  output$plotconstante <- renderPlot({
    # generate bins based on input$bins from ui.R
    tabla <- datos()        
    p <- ggplot(data = tabla, mapping = aes(x = xx,y=yy))+ 
      xlab("predictora")+ylab("respuesta")
    
    
    p <- p+geom_point(col="steelblue", size=2)
    
    p <- p+
      if(input$constante==TRUE)
      {stat_function(fun = fun_lineal,args = list("m"=0,"b"=input$a_constante), col="blue", size=1)}
    
    p <- p+
      if(input$cuad_min_constante==TRUE)
      { geom_smooth(method='lm', formula= y~1, se=FALSE, col="red") }
    
    
    p
  })
  
  
  ########################
  #Mundo lineal
  output$perdida<- renderText({
    salida <- c("El valor de la funcón de pérdida en con los parámetros indicados es ", 
                round(sum((datos()$xx*input$m+input$b-datos()$yy)^2),2))
    
    
  })
  
  
  
  output$recta_optima<- renderText({
    salida <- c()
    if(input$cuad_min==TRUE)
    {
      ajuste <-  lm(datos()$yy~datos()$xx)
      a0_opt <- round(ajuste$coefficients[[1]],2)
      a1_opt <- round(ajuste$coefficients[[2]],2)
      
      salida <- c("La recta  de cuadrados mínimos está dada por y=", a0_opt,"+",a1_opt,"x, y el valor 
            de la función de perdida en los parámetros óptimos es",
                  round(sum((datos()$xx*a1_opt+a0_opt-datos()$yy)^2),2))
      
    }
    salida
  })
  
  
  
  
  
  
  output$plotlineal <- renderPlot({
    # generate bins based on input$bins from ui.R
    tabla <- datos()        
    p <- ggplot(data = tabla, mapping = aes(x = xx,y=yy))+ 
      xlab("predictora")+ylab("respuesta")
    
    
    p <- p+geom_point(col="steelblue", size=2)
    
    p <- p+
      if(input$recta==TRUE)
      {stat_function(fun = fun_lineal,args = list("m"=input$m,"b"=input$b), col="blue", size=1)}
    
    p <- p+
      if(input$cuad_min==TRUE)
      { geom_smooth(method='lm', formula= y~x, se=FALSE, col="red") }
    
    
    p
  })
  
  
  #######################################################
  #co2
  
  
  
  output$plotco2 <- renderPlot({
    anho <- seq(from=1992, to=2018, by=2)
    co2 <- c(356.3,358.6,362.4,366.5,369.4,373.2,377.5,381.9,385.6,389.9,393.9,398.6,404.2,408.5)
    
    
    
    datos <- data.frame(anho,co2)
    p <- ggplot(datos,aes(x=anho,y=co2))+xlab("años")
    p <- p+geom_point()
    
    p <-     p+
      if(input$constante_co2==TRUE)
      {geom_smooth(method='lm', formula= y~1, se=FALSE, col="red")
      }
    
    p <-  p+
      if(input$recta_co2==TRUE)
      {geom_smooth(method='lm', formula= y~x, se=FALSE, col="blue")
      }
    
    
    grilla <- seq(from=anho[1],to=anho[length(anho)],by=0.01)
    ngrilla <- length(grilla)
    pol_en_grilla <- function(x) interpolador(x,x_tabla=anho,y_tabla=co2)
    
    ev_en_grilla <- sapply(grilla,pol_en_grilla)
    
    
    df_2 <- data.frame(grilla, ev_en_grilla)
    
    p <- p+
      if(input$interpolador==TRUE)
      {
        geom_line(data=df_2, aes(grilla,ev_en_grilla))
        
        
      }
    
    
    p
  })
  
  
  
  
  
  
  
  ##############################################
  #Cuadratics 
  
  output$cuad_perdida<- renderText({
    salida <- c("El valor de   
                               la función de pérdida en con los parámetros indicados es ", 
                round(sum((datos()$xx^2*input$a_par+datos()$xx*input$b_par+input$c_par-datos()$yy)^2),2))
    
    
  })
  
  
  output$cuad_optima<- renderText({
    salida <- c()
    if(input$cuad_min_cuadratica==TRUE)
    {
      zz <- datos()$xx^2
      ajuste <-  lm(datos()$yy~datos()$xx+zz)
      a0_opt <- round(ajuste$coefficients[[1]],2)
      a1_opt <- round(ajuste$coefficients[[2]],2)
      
      a2_opt <- round(ajuste$coefficients[[3]],2)
      
      salida <- c("La cuadrática  de cuadrados mínimos está dada por y=", a0_opt,"+",a1_opt,"x","+",
                  a2_opt,"x^2, y el valor de la función de perdida en los parámetros  óptimos es", 
                  round(sum((datos()$xx^2*a2_opt+datos()$xx*a1_opt+a0_opt-datos()$yy)^2),2))
      
    }
    salida
  })
  
  
  
  
  
  output$plotcuad <- renderPlot({
    # generate bins based on input$bins from ui.R
    tabla <- datos()        
    p <- ggplot(data = tabla, mapping = aes(x = xx,y=yy))+ 
      xlab("predictora")+ylab("respuesta")
    # 
    # 
    p <- p+geom_point(col="steelblue", size=2)
    
    p <- p+
      if(input$cuad==TRUE)
      {stat_function(fun = fun_cuad,args = list("a"=input$a_par,"b"=input$b_par,"c"=input$c_par ), col="blue", size=1)}
    
    zz <- datos()$xx^2
    ajuste <-  lm(datos()$yy~datos()$xx+zz)
    a0_opt <- round(ajuste$coefficients[[1]],2)
    a1_opt <- round(ajuste$coefficients[[2]],2)
    a2_opt <- round(ajuste$coefficients[[3]],2)
    
    
    
    p <- p+
      
      if(input$cuad_min_cuadratica==TRUE)
      { stat_function(fun = fun_cuad,args = list("a"=a2_opt,"b"=a1_opt,"c"=a0_opt ), col="red", size=1)
      }
    
    
    p
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
