#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

covidData = read.csv("owid-covid-data.csv")
populations = unique(covidData[c("location", "population")])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select box"), 
                        choices = populations["location"], 
                        selected = 1),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Location"),
           fluidRow(column(10, verbatimTextOutput("value"))),
           h2("Population"),
           fluidRow(column(10, verbatimTextOutput("value2")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$value <- renderPrint({ input$select })
  output$value2 <- renderPrint({ populations[populations$location ==input$select, 2]})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
