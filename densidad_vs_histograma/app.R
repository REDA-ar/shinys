
library(shiny)


ui <-fluidPage(
    titlePanel("Densidad vs.Histograma"
               # app title/description
    ),
    
    fluidRow(
        column(6, 
               sliderInput("obs", "Cantidad de observaciones:", min = 1, max = 1000, value = 500)
        ),
        
        

            column(6,
        selectInput( "distribucion", h3("Elija la distribucion"), 
                     choices = list("Normal" = "normal", 
                                    "Exponencial" = "exponencial",
                                    "Uniforme" = "uniforme"), selected = 1)
            )
        
    )
    ,   
    fluidRow(
        column(6, 
               plotOutput("densidad")
        ),
        column(6, 
               plotOutput("histograma")
        )
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$histograma <- renderPlot({
        n<-input$obs
        if(input$distribucion=="normal")
        { 
            mu<-0
            sigma<-8
datos<-rnorm(n,mu,sigma)            
        }

        if(input$distribucion=="exponencial")
        { 
            lambda<-1
                mu<-1/lambda
            sigma<-1/lambda
datos<-rexp(n,lambda)
} 
        if(input$distribucion=="uniforme")
        {  
            a<-0
            b<-1
            mu<-(a+b)/2
            sigma<-sqrt((b-a)^ 2/12)
 datos<-runif(n,a,b)     
 }

            hist(datos,prob=TRUE,xlab="datos",  main = "Histograma")
        # ,breaks = bins, col = "#75AADB", border = "white",
        # xlab = "Waiting time to next eruption (in mins)",
        # main = "Histogram of waiting times")
        # 
    })                                          

    
    
    output$densidad <- renderPlot({
        n<-input$obs
        if(input$distribucion=="normal")
        { 
            mu<-0
            sigma<-8
            #datos<-rnorm(n,mu,sigma)            
        grilla<-seq(mu-3*sigma,mu+3*sigma,by=0.01)
        yy<-dnorm(grilla,mu,sigma)
            }
        
        if(input$distribucion=="exponencial")
        { 
            lambda<-1
            mu<-1/lambda
            sigma<-1/lambda
            grilla<-seq(-1,10,by=0.01)
            yy<-dexp(grilla, lambda)
#            datos<-rexp(n,lambda)
        } 
        if(input$distribucion=="uniforme")
        {  
            a<-0
            b<-1
            mu<-(a+b)/2
            grilla<-seq(-0.5,1.5,by=0.01)

            yy<-dunif(grilla,a,b)
            #sigma<-sqrt((b-a)^ 2/12)
            #datos<-runif(n,a,b)     
        }
        
        plot(grilla,yy,  main = "Densidad",xlab="", ylab="")
        # ,breaks = bins, col = "#75AADB", border = "white",
        # xlab = "Waiting time to next eruption (in mins)",
        # main = "Histogram of waiting times")
        # 
    })                                          
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# hist(runif(100) ,freq=FALSE)
# lines(grilla,dunif(grilla),type="l")




