#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(leaflet)

monthList <- seq(2, 12, by=1)
yearList <- seq(2011, 2014, by=1)
#view layer
shinyUI(fluidPage(
    
    #input elements
    theme = shinytheme("superhero"),
    # Application title
    titlePanel("Super Store Data Analysis"),
    tabsetPanel(
        tabPanel("Regional Prediction", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         helpText("
                         "),
                         selectInput("month", label = h3("Month"), 
                                     choices = monthList, 
                                     selected = 2),
                         selectInput("year", label = h3("Year"), 
                                     choices = yearList, 
                                     selected = 2011),
                         radioButtons("predictMapFlag", label = h3("Select Result Type"),
                                      choices = c("actual result"=0, "predicted result"=1), 
                                      selected = 1),
                         actionButton("button", "Submit")
                     ),
                     mainPanel(
                         tabPanel("Sales Prediction", 
                                  leafletOutput("mymap")
                         ),
                         
                         tabPanel("temp")
                     )
                 )
        )
    )
))
