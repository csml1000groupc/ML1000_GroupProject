#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(Hmisc)
library(knitr)
library(tidyverse)
library(ggplot2) 
library(gridExtra)
library(mice)  
library(corrplot)
library(pROC)
library(png) 
library(xtable)
library(caret)
library(dplyr)
library(reshape2)
library(arules)
library(randomForest)
library(ggthemes)
library(scales)
library(rpart)
library(class)
library(ROSE)
library(rpart)
library(rpart.plot)
library(rattle)
library(car)
library(e1071)
library(tinytex)
library(fpc)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)

    output$mymap <- eventReactive(input$button, {
        if(input$predictMapFlag==0){
            return(getActualSalesMap())
        }
        if(input$predictMapFlag==1){
            return(getMap())
        }
    })
    month <- 2
    year <- 2011
    getMap <- renderLeaflet({
        if(month != input$month || month!=input$year){
            month <- input$month
            year <- input$year
        }
        predictRes = getPredictionResult(testData, month, year)
        predictResPositive=predictRes%>%filter(sumPredSale>=0)
        predictResNegative=predictRes%>%filter(sumPredSale<0)
        
        mymap = leaflet() %>%
            addTiles() %>%
            setView(lng = -93.85, lat = 37.45, zoom = 4)%>%
            addCircleMarkers(data = predictResPositive, lng = ~Longitude2, lat = ~Latitude2,
                             label =paste0("Expected Sales: ",predictResPositive$sumPredSale,
                                            " Place: ",predictResPositive$City), 
                             color = "blue", fillColor = "blue", radius = ~RadialSize+10, weight = 4,
                             opacity = .3, fillOpacity = 0.4,
                             labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label"))%>%
            addCircleMarkers(data = predictResNegative, lng = ~Longitude2, lat = ~Latitude2,
                             label =paste0("Expected Sales: ",predictResNegative$sumPredSale,
                                           " Place: ",predictResNegative$City), 
                             color = "red", fillColor = "red", radius = ~RadialSize+10, weight = 4,
                             opacity = .9, fillOpacity = 0.4,
                             labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label"))
        
    })
    
    getActualSalesMap <- renderLeaflet({
        if(month != input$month || month!=input$year){
            month <- input$month
            year <- input$year
        }
        predictRes = getPredictionResult(testData, month, year)
        actualRes = predictRes
        actualRes$ScaledsummedSale = scale(actualRes$summedSale)*10
        positiveActual = actualRes%>%filter(ScaledsummedSale>=0)
        negativeActual = actualRes%>%filter(ScaledsummedSale<0)
        
        mymap = leaflet() %>%
            addTiles() %>%
            setView(lng = -93.85, lat = 37.45, zoom = 4)%>%
            addCircleMarkers(data = positiveActual, lng = ~Longitude2, lat = ~Latitude2,
                             label =paste0("Actual Sales: ",positiveActual$summedSale,
                                           " Place: ",positiveActual$City), 
                             color = "blue", fillColor = "blue", radius = ~ScaledsummedSale+10, weight = 4,
                             opacity = .9, fillOpacity = 0.4,
                             labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label")) %>%
            addCircleMarkers(data = negativeActual, lng = ~Longitude2, lat = ~Latitude2,
                             label =paste0("Actual Sales: ",negativeActual$summedSale,
                                           " Place: ",negativeActual$City), 
                             color = "red", fillColor = "red", radius = ~ScaledsummedSale+10, weight = 4,
                             opacity = .9, fillOpacity = 0.4,
                             labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label")) 
        
        
    })
    
    #testing list based on m and year 
    getPredictionResult<-function(td=testData, mth=mon, yr=year){
        tempTesting<-getTestDataByMonth(td=testData, m=mth, y=yr)
        trimmedData<-testDataTrimming(tempTesting, trainData)
        tempResult<-salePredict(trimmedData)
        tempResult<-getResultWithRadial(tempResult)
        return(tempResult)
    }

    #get testing data
    getTestDataByMonth<-function(td=testData, m=month, y=Year){
        tempTestData<-td%>%filter(Year==y)
        tempTestData<-tempTestData%>%filter(Month==m)
        return(tempTestData)
    }
    
    #trimming data
    testDataTrimming<-function(testData, trainData){
        getFilteredTestByGivenFeature<-function(testDtSet, testFeature, trainFeature){
            featureValList = levels(as.factor(trainFeature))
            tempList=unlist(lapply(testFeature, function(x){
                if(x %in% featureValList){
                    return(x)
                }
                return("NA")
            }))
            newSet <- data.frame(cbind(testDtSet, filteredList=tempList))
            newSet <- newSet%>%filter(filteredList!="NA")
            newSet <- newSet%>%select(-filteredList)
            return(newSet)
        }
        newTestDataSet<-getFilteredTestByGivenFeature(testData, testData$City, trainData$City)
        newTestDataSet<-getFilteredTestByGivenFeature(newTestDataSet, newTestDataSet$Customer.ID, trainData$Customer.ID)
        newTestDataSet<-getFilteredTestByGivenFeature(newTestDataSet, newTestDataSet$State, trainData$State)
        return(newTestDataSet)
    }
    
    #Prediction
    salePredict<-function(newTestDataSet=trimmedData){
        tempRes<-predict(lmModel, newdata=newTestDataSet%>%select(-summedSaleByCustomer), interval = c("none", "confidence", "prediction"))
        result = cbind(newTestDataSet, tempRes)%>%select(summedSaleByCustomer, City, State, Year, Month, tempRes)
        result = result%>%group_by(City, State)%>%
            summarise(summedSale=sum(summedSaleByCustomer),
                      sumPredSale=round(sum(tempRes), digits = 2))
        tempCitStateInfo<-inner_join(result, citStateInfo,by=c("City", "State"))
        tempCitStateInfo<-tempCitStateInfo%>%distinct(summedSale, sumPredSale, State, City, Latitude2, Longitude2)
        return(tempCitStateInfo)    
    }
    
    #getRadial
    getResultWithRadial<-function(tempRes=inputData){
        tempRes$scaledVal<-scale(tempRes$sumPredSale)
        tempRes$RadialSize<-tempRes$scaledVal*10
        tempRes<-tempRes%>%select(-scaledVal)
        return(tempRes)
    }
    
    
})

#load datas
lmModel <- readRDS("./models/lmModel.RDS")
trainData <- read.csv("./data/trainData.csv")[-1]
testData <- read.csv("./data/testData.csv")[-1]
citStateInfo <- read.csv("./data/citStateInfo.csv")
