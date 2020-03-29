
Fun<-function(x,m,a)
{
  if (exp(x+m)>a) 1 else 0
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Algorithme de la descente du gradient stochastique
"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("n",
                     "n:",
                     min = 100,
                     max = 1000000,
                     value = 100000,
                     step = 400),
        numericInput("mu",
                     "mu:",
                     min = 0,
                     max = 200,
                     value = 50,
                     step = 5),
        numericInput("sigma",
                     "sigma:",
                     min = 0,
                     max = 200,
                     value = 23,
                     step = 1),
        numericInput("a",
                     "a:",
                     min = 0,
                     max = 20,
                     value = 10,
                     step = 1) ),
      
      # Show a plot of the generated distribution
      mainPanel(         
        verbatimTextOutput("p1"),
        verbatimTextOutput("m"),
        plotOutput("drift")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$drift <- renderPlot({
     n=input$n
     mu=input$mu
     sigma=input$sigma
     a=input$a
     m=0
     p=0
     s=0  
     drift=rep(0, n-1)
     for (i in 1:n) { 
       drift[i]=m
       x=rnorm(1,mu,sigma)
       p=(i/(i+1))*p+ (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(2*sigma^2))
       s= (i/(i+1))*s + (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(sigma^2))
       m = m - (1/(i+1))*(m-(x-mu))*(Fun(x,0,a))*exp((-2*m*(x-mu)+m^2)/(2*sigma^2))
     }
     plot(drift,type="l")
   })
   output$m  <- renderText({
     n=input$n
     mu=input$mu
     sigma=input$sigma
     a=input$a
     m=0
     p=0
     s=0  
     drift=rep(0, n-1)
     for (i in 1:n) { 
       drift[i]=m
       x=rnorm(1,mu,sigma)
       p=(i/(i+1))*p+ (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(2*sigma^2))
       s= (i/(i+1))*s + (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(sigma^2))
       m = m - (1/(i+1))*(m-(x-mu))*(Fun(x,0,a))*exp((-2*m*(x-mu)+m^2)/(2*sigma^2))
     }
     paste("m egale a ", as.character(m))
     })
   output$p1  <- renderText({
     n=input$n
     mu=input$mu
     sigma=input$sigma
     a=input$a
     m=0
     p=0
     s=0  
     drift=rep(0, n-1)
     for (i in 1:n) { 
       drift[i]=m
       x=rnorm(1,mu,sigma)
       p=(i/(i+1))*p+ (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(2*sigma^2))
       s= (i/(i+1))*s + (1/(i+1))*(Fun(x,m,a))*exp(-(2*m*(x-mu)+m^2)/(sigma^2))
       m = m - (1/(i+1))*(m-(x-mu))*(Fun(x,0,a))*exp((-2*m*(x-mu)+m^2)/(2*sigma^2))
     }
     paste("P1 egale a ", as.character(p[1]))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

