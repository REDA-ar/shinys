library(shiny)

genero.probs <- function(semilla){
	set.seed(semilla)
	primero <- round(runif(1,min=0.01,max=0.06),2)
	segundo <- round(runif(1,min=0.9,max=0.98),2)
	tercero <- round(runif(1,min=0.8,max=0.95),2)
	Probabilidad <- c(primero,segundo,tercero)
	Probabilidad
}

ui <- fluidPage(
	titlePanel("Especificidad y Sensibilidad"),
    	mainPanel(h4("Se realiza un análisis de laboratorio para diagnosticar cierta enfermedad. 
    	El test utilizado puede dar dos resultados: positivo, indicando que la persona está enferma; o negativo,
    	sugiriendo que no hay enfermedad. En tal caso, 
    	se está frente a la posibilidad de
		cometer dos tipos de error en el diagnóstico:"),
    	          h4("(i) Falso positivo: diagnosticar como enferma a una persona sana." ), 
    	          h4("(ii) Falso negativo: diagnosticar como sana a una persona enferma."),
    	          h4("En este contexto, se definen los siguientes conceptos: "),
	h4("Especificidad: es la probabilidad de que el análisis resulte negativo en un paciente sano."),
h4("Sensibilidad: es la probabilidad de que el análisis resulte positivo en un paciente enfermo."), 
h4("Prevalencia: es la proporción de la población que padece la enfermedad."), 
h4("En esta aplicación se encuentra información hipotética sobre una enfermedad y sobre la prueba 
utilizada
para su detección. La información brindada queda determinada por su número de libreta."), 
	sidebarLayout(
      	sidebarPanel(numericInput("libreta", "Número de libreta:", min = 1, max = 100000, value = 24292)),
       	mainPanel( verbatimTextOutput("info1"),verbatimTextOutput("info2"),verbatimTextOutput("info3")),
		position = c("left", "right"),
		#sidebarPanel(numericInput("probabilidad","Calcule la probabilidad solicitada", min =0, max =1, value = "?")),
            #textOutput("verifico")
	),
	fluidRow(column(3,
	                h4("Utilizando estos datos, 
calcule la probabilidad de que un paciente esté enfermo sabiendo que el resultado de la prueba es positivo.")),
	         
      	numericInput("probabilidad","Calcule la probabilidad solicitada (con al menos 4 decimales)", min =0, max =1, value = "?"),
            textOutput("verifico"))
	)
)

server <- function(input,output) {

	datos <- reactive({
		genero.probs(input$libreta)
	})
  
	output$info1 <- renderText({
		paste("Prevalencia","=","",datos()[1])
	})	
	output$info2 <- renderText({
		paste("Sensibilidad","=","",datos()[2])
	})
	#Sensibilidad: Probabilidad de un resultado positivo sabiendo que el paciente está enfermo:
	output$info3 <- renderText({
		paste("Especificidad","=","",datos()[3])
	})

#	Especidifidad: Probabilidad  de dar como  negativos un caso  realmente sanos;
	
#	Sensibilidad: Probabilidad de  dar como casos positivos un  caso realmente enfermos
	
		output$verifico <- renderText({
		  ppositivo <- datos()[2]*datos()[1]+(1-datos()[3])*(1-datos()[1])
		calculo <- (datos()[2]*datos()[1])/ppositivo
		dif <- as.numeric(try(abs(input$probabilidad-calculo),TRUE))
		if(is.na(dif)){
			resp <- "Esperando respuesta"
			}else{
			if(dif<0.0001){
				resp <- " ¡Correcto! "
				}else{
				resp <- "Inténtelo nuevamente"
			}
		}
		resp
      })

} 

shinyApp(ui = ui, server = server)   
   

