  #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI ----

# El objeto de interfaz de usuario ( user interfase ui) controla el 
#dise?o y la apariencia de su aplicaci?n. 

#fluidPage create a display that automatically adjusts to the 
#  dimensions of your user's browser window. You lay out the user 
#  interface of your app by placing elements in the fluidPage function. 



ui <- fluidPage(
  titlePanel("Visualización de la Empírica"),
  
  sidebarLayout(
    sidebarPanel(
      #helpText("Histograma de normales"),
     
      numericInput("ene",h3("Cantidad de datos"), value=10),
      
    selectInput( "distribucion", h3("Elija la distribucion para generar datos"), 
                             choices = list("Poisson" = "poisson","Normal" = "normal", 
                                            "Exponencial" = "exponencial",
                                            "Uniforme" = "uniforme"), selected = 1)
    ),
  
    mainPanel(
      
      
      
      
#     Output: Histogram ----
     plotOutput(outputId = "grafico")

    )
      
  )

  )
#  widget:  A web element that your users can interact with. 
#place a widget function in sidebarPanel or mainPanel in your ui object.


# Each widget function requires several arguments. The first
# two arguments for each widget are
# 
# a name for the widget: The user will not see this 
# name, but you can use it to access the widget's value. 
# The name should be a character string.
# 
# a label: This label will appear with the widget in your app.
# It should be a character string, but it can be an empty string ""

# function 	widget
# actionButton 	Action Button
# checkboxGroupInput 	A group of check boxes
# checkboxInput 	A single check box
# dateInput 	A calendar to aid date selection
# dateRangeInput 	A pair of calendars for selecting a date range
# fileInput 	A file upload control wizard
# helpText 	Help text that can be added to an input form
# numericInput 	A field to enter numbers
# radioButtons 	A set of radio buttons
# selectInput 	A box with choices to select from
# sliderInput 	A slider bar
# submitButton 	A submit button
# textInput 	A field to enter text
    

  
  
#   titlePanel("title panel"),
#   
#   sidebarLayout(
#     # sidebarLayout always takes two arguments:
#     #   
#     # sidebarPanel function output
#     # 
#     # mainPanel function output
#     # 
#     # These functions place content in 
#     # either the sidebar or the main panels.
#     # 
#     # The sidebar panel will appear on the left side of your app by 
#     # default.  You can move it to the right side by giving 
#     #sidebarLayout the optional 
#     # argument position = "right"
#     sidebarPanel("sidebar panel"),
#     mainPanel("main panel")
#   )
# )

# Define server logic ----
server <- function(input, output) {
  
    
#  input: aca se guardan todas las cosas que fuimos 
  #creando con los widgets 
  # is a second list-like object. 
  # It stores the current values of all of the widgets in your app. 
  # These values will be saved under 
  # the names that you gave the widgets in your ui.
  
  # output: is a list-like object
  # that stores instructions for building the R objects in your app.  
  #las cosas se construyen con los render 
  
  # render function 	creates
  # renderDataTable 	DataTable
  # renderImage 	images (saved as a link to a source file)
  # renderPlot 	plots
  # renderPrint 	any printed output
  # renderTable 	data frame, matrix, other table like structures
  # renderText 	character strings
  # renderUI 	a Shiny tag object or HTML
 
  
    output$grafico <- renderPlot({
      n<-input$ene
    
      
      if(input$distribucion=="poisson")
      { 
        lambda<-1
        datos<-rpois(n,lambda)
        plot(ecdf(datos), main="La empírica y la verdadera (en rojo)", ylab="")
        desde<--1
        hasta<-max(datos)+0.5
        grilla<-seq(desde, hasta, length=1000)
        yy<-ppois(grilla,lambda)
        lines(grilla,yy, col="red",type="point")
      }
      
        
    if(input$distribucion=="normal")
    { 
      mu<-0
      sigma<-8
      datos<-rnorm(n,mu,sigma)
      plot(ecdf(datos), main="La empírica y la verdadera", ylab="")
      desde<-min(datos)-sigma
      hasta<-max(datos)+sigma
      grilla<-seq(desde, hasta, length=1000)
      yy<-pnorm(grilla,mu,sigma)
      lines(grilla,yy, col="red")
     }
    if(input$distribucion=="exponencial")
    {
      lambda<-1
      mu<-1/lambda
      sigma<-1/lambda
      datos<-rexp(n,lambda)
      plot(ecdf(datos), main="La empírica y la verdadera")
      desde<-min(datos)-sigma
      hasta<-max(datos)+sigma
      grilla<-seq(desde, hasta, length=1000)
      yy<-pexp(grilla,lambda)
      lines(grilla,yy, col="red")
      }
    if(input$distribucion=="uniforme")
    {
      a<-0
      b<-1
      mu<-(a+b)/2
      sigma<-sqrt((b-a)^ 2/12)
      datos<-runif(n,a,b)
      plot(ecdf(datos), main="La empírica y la verdadera")
      desde<-min(datos)-sigma
      hasta<-max(datos)+sigma
      grilla<-seq(desde, hasta, length=1000)
      yy<-punif(grilla,a,b)
      lines(grilla,yy, col="red")
    }
  #   hist(estandarizados,freq=FALSE,xlab="promedios", 
  #        col = "#75AADB", border = "white",main = "Histograma")
  #        # ,breaks = bins, col = "#75AADB", border = "white",
  #        # xlab = "Waiting time to next eruption (in mins)",
  #        # main = "Histogram of waiting times")
  #        # 
  # }
    
  
  
})
}
# Run the app ----
shinyApp(ui = ui, server = server)
#,display.mode = "showcase")
