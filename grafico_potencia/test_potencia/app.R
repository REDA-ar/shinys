
# grafico de la potencia de un test
#
library(shiny)
library(vroom)
library(ggplot2)
library(shinycssloaders)
library(caret)
library(dplyr)
library(latex2exp)
library(shinythemes)
library(shinyjs)

library(gridExtra)

shade_curve <- function(df, zstart, zend, fill = "red", alpha =.5){
    geom_area(data = subset(df, x >= zstart & x < zend), aes(y=y), 
              fill = fill, color = NA, alpha = alpha)}

potencia <- function(mu0=80,mu,sigma=3,n,alpha=0.05){
    1-pnorm(qnorm(1-alpha)+(mu0-mu)*sqrt(n)/sigma)
}


marco <- function(mu0,sigma,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu0, sd = sigma/sqrt(n)))
    # izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    # der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte <- mu0 + (qnorm(1-alpha))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) +geom_line(col="white")+
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous(breaks = NULL) +
        theme_classic() +
        #shade_curve(df = mi_df, zstart = corte, zend = mu0 + sigma*L, fill = "blue") + 
    #    ylab("Densidad del promedio")+
        xlab("")+geom_segment(x=corte,y=0,xend=mu0 + sigma*L,yend=0,col="red")+
        ylab("")
    
    return(g)
}





graf <- function(mu0,sigma,mu,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma/sqrt(n)))
    # izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    # der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte <- mu0 + (qnorm(1-alpha))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) + geom_line() +
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous(breaks = NULL) +
        theme_classic() +
        shade_curve(df = mi_df, zstart = corte, zend = mu0 + sigma*L, fill = "blue") + 
        ylab("Densidad del promedio")+
        xlab("")+geom_segment(x=corte,y=0,xend=mu0 + sigma*L,yend=0,col="red")
    return(g)
}



graf_curva <- function(mu0=80,sigma=3, alpha=0.05,n,L=2){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df_2 <- data.frame(x = x, y = potencia(mu0,x,sigma,n,alpha))
    g <- ggplot(mi_df_2, aes(x = x,y = y)) +
        geom_line(col="green4")+ 
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous() +
        theme_classic() +
        #ggtitle('BLA') +
        ylab("Potencia") +
        xlab(expression(mu))
    return(g)
}


#################################################

marco_bilateral <- function(mu0,sigma,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu0, sd = sigma/sqrt(n)))
    corte_izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte_der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) + geom_line(color="white") +
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous() +
        theme_classic() +
            xlab("")+
        ylab(" ")
    return(g)
}

marco_bilateral(mu0=94,sigma=13,alpha=0.05,n=5,L=2)

graf_bilateral <- function(mu0,sigma,mu,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma/sqrt(n)))
    corte_izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte_der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    #corte <- mu0 + (qnorm(1-alpha))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) + geom_line() +
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous(breaks = NULL) +
        theme_classic() +
        shade_curve(df = mi_df, zstart = mu0 - sigma*L, zend = corte_izq, fill = "blue") + 
        shade_curve(df = mi_df, zstart = corte_der, zend = mu0 + sigma*L, fill = "blue") + 
        ylab("Densidad del promedio")+
        geom_segment(x=corte_der,y=0,xend=mu0 + sigma*L,yend=0,col="red")+
        geom_segment(x=mu0 - sigma*L,y=0,xend=corte_izq ,yend=0,col="red")
    
    
    xlab("")
    return(g)
}
potencia_bilateral <- function(mu0,mu,sigma=3,n,alpha=0.05){
    pnorm(-qnorm(1-alpha/2)+(mu0-mu)*sqrt(n)/sigma)+1-pnorm(qnorm(1-alpha/2)+(mu0-mu)*sqrt(n)/sigma)
}

graf_curva_bilateral <- function(mu0,sigma=3,alpha=0.05,n,L=2){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df_2 <- data.frame(x = x, y = potencia_bilateral(mu0,x,sigma,n,alpha))
    g <- ggplot(mi_df_2, aes(x = x,y = y)) +
        geom_line(col="green4")+ 
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous() +
        theme_classic() +
        #ggtitle('BLA') +
        ylab("Potencia") +
        xlab(expression(mu))
    return(g)
}

#####################################
#menor

potencia_menor <- function(mu0=80,mu,sigma=3,n,alpha=0.05){
    pnorm(-qnorm(1-alpha)+(mu0-mu)*sqrt(n)/sigma)
}


marco_menor <- function(mu0,sigma,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu0, sd = sigma/sqrt(n)))
    # izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    # der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte <- mu0 - (qnorm(1-alpha))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) + geom_line(col="white") +
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous(breaks = NULL) +
        theme_classic() +
        # shade_curve(df = mi_df, zstart = mu0 - sigma*L, zend = corte, fill = "blue") + 
    #    ylab("Densidad del promedio")+
        xlab("")+geom_segment(x=mu0 - sigma*L,y=0,xend=corte,yend=0,col="red")+
        ylab("")
    return(g)
}

graf_menor <- function(mu0,sigma,mu,alpha,n,L){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df <- data.frame(x = x, y = dnorm(x, mean = mu, sd = sigma/sqrt(n)))
    # izq <- mu0 - (qnorm(1-alpha/2))*sigma/sqrt(n)
    # der <- mu0 + (qnorm(1-alpha/2))*sigma/sqrt(n)
    corte <- mu0 - (qnorm(1-alpha))*sigma/sqrt(n)
    g <- ggplot(mi_df, aes(x = x,y = y)) + geom_line() +
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous() +
        theme_classic() +
        shade_curve(df = mi_df, zstart = mu0 - sigma*L, zend = corte, fill = "blue") + 
        ylab("Densidad del promedio")+
        xlab("")+geom_segment(x=mu0 - sigma*L,y=0,xend=corte,yend=0,col="red")
    return(g)
}


graf_curva_menor <- function(mu0=80,sigma=3, alpha=0.05,n,L=2){
    x <- seq(from = mu0 - sigma*L, to = mu0 + sigma*L, by = sigma/100)
    mi_df_2 <- data.frame(x = x, y = potencia_menor(mu0,x,sigma,n,alpha))
    g <- ggplot(mi_df_2, aes(x = x,y = y)) +
        geom_line(col="green4")+ 
        scale_x_continuous(breaks = (round(mu0)-L*sigma):(round(mu0)+L*sigma)) +
        scale_y_continuous() +
        theme_classic() +
        ylab("Potencia") +
        xlab(expression(mu))
    return(g)
}



ui <- fluidPage(
    
    useShinyjs(),
    withMathJax(),
    
    titlePanel("La Función de Potencia - Mundo normal Varianza conocida"),
    tabsetPanel( 
        type = 'tabs', id = "tabs_upload",
        
        
        tabPanel("Los pollos - Alternativa mayor",
                 
                 
                 fluidRow(
                     column(12,
                            includeHTML("mayor.html")
                     )
                 ),
                 
                 sidebarLayout(
                     sidebarPanel(
                         withMathJax(),   
                         sliderInput("alpha",
                                     "\\(\\alpha\\) (nivel)",
                                     min = 0.001,
                                     max = 0.2,
                                     value = 0.05),
                         sliderInput("n",
                                     "n (cantidad de datos)",
                                     min = 1,
                                     max = 150,
                                     value = 5),
                         sliderInput("mu",
                                     "\\(\\mu\\) (media real)",
                                     min = 75,
                                     max = 85,
                                     value = 80,
                                     step = 0.2),
                         checkboxInput("area", "Representar \\(\\Pi(\\mu)\\) con area" , value = TRUE),
                         checkboxInput("curva", "Gráfico de la función de potencia" , value = TRUE),
                         
                         checkboxInput("seg", "Indicar la potencia en el valor de \\(\\mu\\) elegido" , value = TRUE),
                         
                         
                         
                     ),
                     mainPanel(
                         plotOutput("grafico_bis"),
                         h4(textOutput("potencia"))
                     )
                 ),
                 fluidRow(
                     column(12,
                            includeHTML("partedos_grafico_potencia.html")
                     )
                 )
        ), #cierro pestaña
        
        tabPanel("Calcio - Test Bilateral",
                 fluidRow(
                     column(12,
                            includeHTML("bilateral.html")
                     )
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("alpha_bi",
                                     "alpha (nivel)",
                                     min = 0.001,
                                     max = 0.2,
                                     value = 0.05),
                         sliderInput("n_bi",
                                     "n (cantidad de datos)",
                                     min = 1,
                                     max = 150,
                                     value = 5),
                         sliderInput("mu_bi",
                                     "\\(\\mu\\)  (media real)",
                                     min = 84,
                                     max = 104,
                                     value = 94,
                                     step = 0.2),
                         checkboxInput("area_bi", "Representar \\(\\Pi(\\mu)\\) con area" , value =TRUE),
                         checkboxInput("curva_bi", "Gráfico de la función de potencia" , value = TRUE),
                         
                         checkboxInput("seg_bi", "Indicar la potencia en el valor de \\(\\mu\\) elegido" , value =TRUE),
                         
                         
                         
                     ),
                     mainPanel(
                         plotOutput("grafico_bilateral_bis"),
                         h4(textOutput("potencia_bilateral"))
                     )
                 )
        ),#aca termina pestaña
        tabPanel("Las hamburgesas - Alternativa menor",
                 fluidRow(
                     column(12,
                            includeHTML("menor.html")
                     )
                 ),
                 
                 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("alpha_menor",
                                     "\\(\\alpha\\)  (nivel)",
                                     min = 0.001,
                                     max = 0.2,
                                     value = 0.05),
                         sliderInput("n_menor",
                                     "n (cantidad de datos)",
                                     min = 1,
                                     max = 150,
                                     value = 5),
                         sliderInput("mu_menor",
                                     "\\(\\mu\\)  (media real)",
                                     min = 10,
                                     max = 30,
                                     value = 20,
                                     step = 0.2),
                         checkboxInput("area_menor", "Representar \\(\\Pi(\\mu)\\) con area" , value =TRUE),
                         checkboxInput("curva_menor", "Gráfico de la función de potencia" , value = TRUE),
                         
                         checkboxInput("seg_menor", "Indicar la potencia en el valor de \\(\\mu\\) elegido" , value = TRUE),
                         
                         
                         
                     ),
                     mainPanel(
                         plotOutput("grafico_menor_bis"),
                         h4(textOutput("potencia_menor"))
                     )
                 )
        )#aca termina pestaña
        
    )
)
server <- function(input, output) {
    
    output$potencia <- renderText({
        n <- input$n
        mu0 <- 80
        sigma <- 3
        mu <- input$mu
        alpha <- input$alpha
        corte <- round(mu0 + (qnorm(1-alpha))*sigma/sqrt(n),2)
        pot<-round(potencia(mu0,mu,sigma,n,alpha),5)
        resp <- paste("Borde de la región de rechazo=", corte,", ", "potencia(",mu,")=", pot)
        resp
    })
    
    output$grafico_bis <- renderPlot({
        n <- input$n
        mu0 <- 80
        sigma <- 3
        mu <- input$mu
        alpha <- input$alpha
        L <- 2
        p3 <- marco(mu0,sigma,alpha,n,L)
        
        if(input$area==TRUE)
        { p1<- graf(mu0,sigma,mu,alpha,n,L)
        
        }
        
        
        
        
        p2 <-  graf_curva(mu0=80,sigma=3, alpha,n,L=2)
        if(input$seg==TRUE)
        {p2 <- p2+
            geom_segment(x= mu, y=0,xend=mu,yend=potencia(mu0,mu,sigma,n,alpha),size=0.2)+
            geom_segment(x= mu, y=potencia(mu0,mu,sigma,n,alpha),xend=0,yend=potencia(mu0,mu,sigma,n,alpha),size=0.2 )
        }
        
        if(input$curva==TRUE & input$area==TRUE)
        { p3 <-grid.arrange(p1, p2, ncol=1)
        }
        if(input$curva==TRUE & input$area==FALSE)
        { p3 <-grid.arrange( p2)
        }
        if(input$curva==FALSE & input$area==TRUE)
        { p3 <-grid.arrange( p1)
        }
        
        p3
    })
    
    
    
    ##################
    
    output$potencia_bilateral <- renderText({
        n <- input$n_bi
        mu0 <- 94
        sigma <- 13
        mu <- input$mu_bi
        alpha <- input$alpha_bi
        pot<-round(potencia_bilateral(mu0=mu0,mu=mu,sigma=sigma,n=n,alpha=alpha),5)
        corte_izq <-round( mu0-qnorm(1-alpha/2)*sqrt(sigma^2/n),2)
        corte_der <-round( mu0+qnorm(1-alpha/2)*sqrt(sigma^2/n),2)
        
        resp <- paste("Borde de la región de rechazo: izquierdo=", corte_izq,",","borde derecho=", 
        corte_der, ",","potencia (",mu,")=", pot)
        resp
    })
    #####################
    output$grafico_bilateral_bis <- renderPlot({
        n <- input$n_bi
        mu0 <- 94
        sigma <- 13
        mu <- input$mu_bi
        alpha <- input$alpha_bi
        L <- 2
        
        p3 <- marco_bilateral(mu0=mu0,sigma=sigma,alpha=alpha,n=n,L=L)
        p1 <- graf_bilateral(mu0=mu0,sigma=sigma,mu=mu,alpha=alpha,n=n,L=L)
        p2 <-graf_curva_bilateral(mu0=mu0,sigma=sigma,alpha=alpha,n=n,L=L)

        if(input$seg_bi==TRUE)
        {p2 <- p2+
            geom_segment(x= mu, y=0,xend=mu,yend=potencia_bilateral(mu0=mu0,mu=mu,sigma=sigma,n=n,alpha=alpha),size=0.2)+
            geom_segment(x= mu, y=potencia_bilateral(mu0=mu0,mu=mu,sigma=sigma,n=n,alpha=alpha),xend=0,yend=potencia_bilateral(mu0=mu0,mu=mu,sigma=sigma,n=n,alpha=alpha),size=0.2 )

        }

        if(input$curva_bi==TRUE & input$area_bi==TRUE)
        { p3 <-grid.arrange(p1, p2, ncol=1)
        }
        if(input$curva_bi==TRUE & input$area_bi==FALSE)
        { p3 <-grid.arrange( p2)
        }
        if(input$curva_bi==FALSE & input$area_bi==TRUE)
        { p3 <-grid.arrange( p1)
        }
        #        p3 <-grid.arrange( p2, ncol=1)
        p3
    })
    
    
    
    ########################
    #menor
    output$potencia_menor <- renderText({
        n <- input$n_menor
        mu0 <- 20
        sigma <- 4
        mu <- input$mu_menor
        alpha <- input$alpha_menor
        
        pot<-round(potencia_menor(mu0,mu,sigma,n,alpha),5)
        
        corte <-round( mu0-qnorm(1-alpha)*sqrt(9/n),2)
        resp <- paste("Borde de la región de rechazo: =", corte,", ","potencia (",mu,")=", pot)
        resp
    })
    
    output$grafico_menor_bis <- renderPlot({
        n <- input$n_menor
        mu0 <- 20
        sigma <- 4
        mu <- input$mu_menor
        alpha <- input$alpha_menor
        L <- 2
        p3 <- marco_menor(mu0=mu0,sigma=sigma,alpha=alpha,n=n,L=L)
        
        if(input$area_menor==TRUE)
        { p1<- graf_menor(mu0=mu0,sigma=sigma,mu=mu,alpha=alpha,n=n,L=L)
        
        }
        
        
        
        
        p2 <-     graf_curva_menor(mu0=mu0,sigma=sigma,alpha=alpha,n=n,L=L)
        if(input$seg_menor==TRUE)
        {p2 <- p2+
            geom_segment(x= mu, y=0,xend=mu,yend=potencia_menor(mu0=mu0,mu=input$mu_menor,sigma=sigma,n=n,alpha=alpha),size=0.2)+
            geom_segment(x=0,y=potencia_menor(mu0=mu0,mu=input$mu_menor,sigma=sigma,n=n,alpha=alpha),xend= mu, yend=potencia_menor(mu0=mu0,mu=input$mu_menor,sigma=sigma,n=n,alpha=alpha),size=0.2 )
        }
        
        if(input$curva_menor==TRUE & input$area_menor==TRUE)
        { p3 <-grid.arrange(p1, p2, ncol=1)
        }
        if(input$curva_menor==TRUE & input$area_menor==FALSE)
        { p3 <-grid.arrange( p2)
        }
        if(input$curva_menor==FALSE & input$area_menor==TRUE)
        { p3 <-grid.arrange( p1)
        }
        
        p3
    })
    
    
    
}

shinyApp(ui = ui, server = server)
