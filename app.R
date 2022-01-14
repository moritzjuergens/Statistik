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
require(smooth)
require(Mcomp)
require(tidyquant)
require(shinythemes)
#install.packages("thematic")
#install.packages("rmarkdown")
require(thematic)
require(rmarkdown)
require(plotly)

setwd("~/dev/UniCode/Statistik/projekt")
cv_cases = read.csv("owid-covid-data.csv")
populations = unique(cv_cases[c("location", "population")])



### DATA PROCESSING ###

## Cases ## 

# aggregate of cases by location and time
cv_cases_aggregate = aggregate(cv_cases$total_cases, 
                               by=list(Category=cv_cases$date, 
                                       Category=cv_cases$location), 
                               FUN=sum)
names(cv_cases_aggregate) = c("date", "location", "cases")

# aggregate global
cv_cases_aggregate_global= aggregate(cv_cases$total_cases, 
                                     by=list(Category=cv_cases$date), 
                                     FUN=sum)
names(cv_cases_aggregate_global) = c("date", "cases")

# aggregate of cases per mil
cv_cases_aggregate_per_mil = aggregate(cv_cases$total_cases_per_million, 
                                       by=list(Category=cv_cases$date, 
                                               Category=cv_cases$location), 
                                       FUN=sum)
names(cv_cases_aggregate_per_mil) = c("date", "location", "cases")

# new cases per mil
cv_new_cases_per_mil = aggregate(cv_cases$new_cases_per_million, 
                                 by=list(Category=cv_cases$date, 
                                         Category=cv_cases$location), 
                                 FUN=sum)

names(cv_new_cases_per_mil) = c("date", "location", "cases")


cv_new_deaths_per_mil = aggregate(cv_cases$new_deaths_per_million, 
                                  by=list(Category=cv_cases$date, 
                                          Category=cv_cases$location),
                                  FUN=sum)
names(cv_new_deaths_per_mil) = c("date", "location", "deaths")

## Vaccinations ##
# aggregate of total vax by location and time
cv_vax_aggregate = aggregate(cv_cases$total_vaccinations, 
                             by=list(Category=cv_cases$date, 
                                     Category=cv_cases$location), 
                             FUN=sum)
names(cv_vax_aggregate) = c("date", "location", "vaccinations")

cv_vax_new = aggregate(cv_cases$new_vaccinations, 
                                     by=list(Category=cv_cases$date, 
                                             Category=cv_cases$location),
                                     FUN=sum)
names(cv_vax_new) = c("date", "location", "vaccinations")

cv_vax_fully = aggregate(cv_cases$people_fully_vaccinated, 
                         by=list(Category=cv_cases$date, 
                                 Category=cv_cases$location), 
                         FUN=sum)
names(cv_vax_fully) = c("date", "location", "vaccinations")



every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme('darkly'),
    title = "COVID Dashboard",
    main_page <- tabPanel(
      title = "Dashboard",
      # Application title
      titlePanel("Covid Data"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("select", label = h3("Select box"),
                      choices = populations["location"],
                      selected = "Europe"),
          h3("Location"),
          fluidRow(column(10, textOutput("value"))),
          h3("Population"),
          fluidRow(column(10, textOutput("value2"))),
          
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Cases",
              
              plotlyOutput("plotCasesPerMil"),
              
              plotlyOutput("plotCasesTotal"),
              
              plotlyOutput("plotNewCases"),
            ),
            tabPanel(
              title = "Vaccinations",
              
              plotlyOutput("plotVaxNew"),
              
              plotlyOutput("plotVaxTotal"),
              
              plotlyOutput("plotVaxFully"),
            )
          )
          
        )
      )
    ),
    about_page <- tabPanel(
      title = "About",
      titlePanel("About"),
      "Created with R Shiny",
      br(),
      "2022 January"
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$value <- renderPrint({ input$select })
  output$value2 <- renderPrint({ populations[populations$location ==input$select, 2]})
  
  ### PLOTS ###
  ## CASES PER MIL ##
  output$plotCasesPerMil <- renderPlotly(ggplotly({ggplot(cv_cases_aggregate_per_mil[cv_cases_aggregate_per_mil$location==input$select,], aes(x=date, y=cases, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Cases",   title = sprintf("Cases/Mil for %s", input$select))}))
  
  ## CASES TOTAL ##
  output$plotCasesTotal <- renderPlotly(ggplotly({ggplot(cv_cases_aggregate[cv_cases_aggregate$location==input$select,],aes(x=date, y=cases, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Cases",   title = sprintf("Cases for %s", input$select))}))                  
  ## NEW CASES PER MIL ##                    
  output$plotNewCases <- renderPlotly(ggplotly({ggplot(cv_new_cases_per_mil[cv_new_cases_per_mil$location==input$select,], aes(x=date, y=cases, group=1)) + geom_line() + geom_smooth(method=loess) + geom_ma(ma_fun = SMA, n = 7, aes(colour="red", linetype="solid"))+ scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(x = "Date", y = "Cases", title = "New Cases per Mil")}))
  
  
  ## VAX NEW ##
  output$plotVaxNew <- renderPlotly(ggplotly({ggplot(cv_vax_new[cv_vax_new$location==input$select,], aes(x=date, y=vaccinations, group=1)) + geom_line() + geom_smooth(method=loess) + geom_ma(ma_fun = SMA, n = 7, aes(colour="red", linetype="solid"))+ scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(x = "Date", y = "Vax%",   title = sprintf("Vax for %s", input$select))})) 
  
  ## TOTAL VAX ##
  output$plotVaxTotal <- renderPlotly(ggplotly({ggplot(cv_vax_aggregate[cv_vax_aggregate$location==input$select,],aes(x=date, y=vaccinations, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Vaccinations",   title = sprintf("Vax for %s", input$select))}))    
  
  ## VAX FULLY ##
  output$plotVaxFully <- renderPlotly(ggplotly({ggplot(cv_vax_fully[cv_vax_fully$location==input$select,], aes(x=date,y=vaccinations, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30))  + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Vaccinations", title = "People Vaccinated Fully")}))
}
thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server)
