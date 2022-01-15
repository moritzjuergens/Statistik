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
require(thematic)
require(rmarkdown)
require(plotly)
library(rsconnect)


cv_cases = read.csv("owid-covid-data.csv")
populations = unique(cv_cases[c("location", "population", "population_density", "median_age", "human_development_index", "gdp_per_capita", "life_expectancy")])



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


## Casualties ##
# aggregate of new deaths per mil
cv_new_deaths_per_mil = aggregate(cv_cases$new_deaths_per_million, 
                                  by=list(Category=cv_cases$date, 
                                          Category=cv_cases$location),
                                  FUN=sum)
names(cv_new_deaths_per_mil) = c("date", "location", "deaths")

# aggregate of deaths per mil
cv_deaths_per_mil = aggregate(cv_cases$total_deaths_per_million, 
                                  by=list(Category=cv_cases$date, 
                                          Category=cv_cases$location),
                                  FUN=sum)
names(cv_deaths_per_mil) = c("date", "location", "deaths")

# aggregate of total deaths
cv_total_deaths = aggregate(cv_cases$total_deaths, 
                                  by=list(Category=cv_cases$date, 
                                          Category=cv_cases$location),
                                  FUN=sum)
names(cv_total_deaths) = c("date", "location", "deaths")

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme('darkly'),
    title = "COVID Dashboard",
    main_page <- tabPanel(
      title = "Dashboard",
      
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("select", label = h3("Select Location"),
                      choices = populations["location"],
                      selected = "Europe"),
          h3(textOutput("location_name")),
          h4("Population: "),
          p(textOutput("location_population")), br(),
          h4("Population Density: "),
          p(textOutput("location_population_density")),br(),
          h4("GDP per Capita: "),
          p(textOutput("location_gdp_per_capita")),br(),
          h4("HDI: "),
          p(textOutput("location_hdi")),br(),
          h4("Avg Life Expectancy: "),
          p(textOutput("location_life_expectancy")),br(),
          h4("Median Age: "),
          p(textOutput("location_median_age")),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              title = "Cases",
              
              plotlyOutput("plotNewCases"),
              
              plotlyOutput("plotCasesPerMil"),
              
              plotlyOutput("plotCasesTotal"),
            ),
            tabPanel(
              title = "Vaccinations",
              
              plotlyOutput("plotVaxNew"),
              
              plotlyOutput("plotVaxTotal"),
              
              plotlyOutput("plotVaxFully"),
            ),
            tabPanel(
              title = "Deaths",
              
              plotlyOutput("plotDeathsNew"),
              
              plotlyOutput("plotDeathsPerMil"),
              
              plotlyOutput("plotDeathsTotal"),
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
    
  output$location_name <- renderText({populations[populations$location==input$select, "location"] })
  output$location_population <- renderText({ populations[populations$location ==input$select, 2]})
  output$location_population_density <- renderText({populations[populations$location==input$select,"population_density"]})
  output$location_median_age <- renderText({populations[populations$location==input$select,"median_age"]})
  output$location_gdp_per_capita <- renderText({populations[populations$location==input$select,"gdp_per_capita"]})
  output$location_hdi <- renderText({populations[populations$location==input$select,"human_development_index"]})
  output$location_life_expectancy <- renderText({populations[populations$location==input$select,"life_expectancy"]})
  
  ### PLOTS ###
  ## CASES ##
  ## CASES PER MIL ##
  output$plotCasesPerMil <- renderPlotly(ggplotly({ggplot(cv_cases_aggregate_per_mil[cv_cases_aggregate_per_mil$location==input$select,], aes(x=date, y=cases, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Cases",   title = sprintf("Cases/Mil for %s", input$select))}))
  
  ## CASES TOTAL ##
  output$plotCasesTotal <- renderPlotly(ggplotly({ggplot(cv_cases_aggregate[cv_cases_aggregate$location==input$select,],aes(x=date, y=cases, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Cases",   title = sprintf("Cases for %s", input$select))}))                  
  ## NEW CASES PER MIL ##                    
  output$plotNewCases <- renderPlotly(ggplotly({ggplot(cv_new_cases_per_mil[cv_new_cases_per_mil$location==input$select,], aes(x=date, y=cases, group=1)) + geom_line() + geom_smooth(method=loess) + geom_ma(ma_fun = SMA, n = 7, aes(colour="red", linetype="solid"))+ scale_x_discrete(breaks = every_nth(n = 30)) +  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(x = "Date", y = "Cases", title = "New Cases per Mil")}))
  
  
  ## VACCINATIONS ##
  ## VAX NEW ##
  output$plotVaxNew <- renderPlotly(ggplotly({ggplot(cv_vax_new[cv_vax_new$location==input$select,], aes(x=date, y=vaccinations, group=1)) + geom_line() + geom_smooth(method=loess) + geom_ma(ma_fun = SMA, n = 7, aes(colour="red", linetype="solid"))+ scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(x = "Date", y = "Vax%",   title = sprintf("Vax for %s", input$select))})) 
  
  ## TOTAL VAX ##
  output$plotVaxTotal <- renderPlotly(ggplotly({ggplot(cv_vax_aggregate[cv_vax_aggregate$location==input$select,],aes(x=date, y=vaccinations, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Vaccinations",   title = sprintf("Vax for %s", input$select))}))    
  ## VAX FULLY ##
  output$plotVaxFully <- renderPlotly(ggplotly({ggplot(cv_vax_fully[cv_vax_fully$location==input$select,], aes(x=date,y=vaccinations, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30))  + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Vaccinations", title = "People Vaccinated Fully")}))
  
  
  ## DEATHS ##
  ## NEW DEATHS ##
  output$plotDeathsNew <- renderPlotly(ggplotly({ggplot(cv_new_deaths_per_mil[cv_new_deaths_per_mil$location==input$select,], aes(x=date, y=deaths, group=1)) + geom_line() + geom_smooth(method=loess) + geom_ma(ma_fun = SMA, n = 7, aes(colour="red", linetype="solid"))+ scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(x = "Date", y = "Deaths",   title = sprintf("Deaths in %s", input$select))})) 
  
  ## TOTAL DEATHS ##
  output$plotDeathsTotal <- renderPlotly(ggplotly({ggplot(cv_total_deaths[cv_total_deaths$location==input$select,],aes(x=date, y=deaths, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Deaths",   title = sprintf("Deaths in %s", input$select))})) 
  
  ## DEATHS PER MIL ##
  output$plotDeathsPerMil <- renderPlotly(ggplotly({ggplot(cv_deaths_per_mil[cv_deaths_per_mil$location==input$select,],aes(x=date, y=deaths, group=1)) + geom_line() + scale_x_discrete(breaks = every_nth(n = 30)) + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Date", y = "Deaths",   title = sprintf("Deaths in %s", input$select))}))    
}
thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server)
