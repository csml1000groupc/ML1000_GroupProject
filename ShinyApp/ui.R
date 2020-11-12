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
                         )
                     )
                 )
        ),
        tabPanel("[Futer Release - Recommendation Service]", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         helpText("
                            In the future release, the following button will generate a list of the products fitting the customers' preference for the advertisement.
                         "),
                         actionButton("recommend", "Generate")
                     ),
                     mainPanel(
                         tabPanel("Contruction is going on", 
                              helpText("The Service will be later release. In this release, the Market Basket Analysis and The Apriori Algorithm to see what kind of products that cutomers usually purchase together, which is tated in the report, will generate the brochure or the list of the products satisfying the customers' taste.")
                         )
                     )
                 )
        )
    )
))
