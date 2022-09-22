#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())
list.of.packages <- c("shiny", "rmarkdown")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
lapply(list.of.packages, require, character.only = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # withMathJax(includeMarkdown(file.path("rmd","about.rmd")))
  includeHTML(rmarkdown::render("meta.rmd"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
