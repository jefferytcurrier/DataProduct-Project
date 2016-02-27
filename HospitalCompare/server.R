require(UsingR)
require(shiny)
require(ggplot2)

## Read outcome data
Outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#Outcomes <- read.csv("http://datascience-enthusiast.com/data/outcome-of-care-measures.csv", colClasses = "character")
#http://datascience-enthusiast.com/data/outcome-of-care-measures.csv

#Keep columns for data table
keepCols <- data.frame(c(1:5))
colnames(keepCols)[1] <- "index"

#Drop columns not needed
Outcomes <- subset(Outcomes[,c(2,3, 6:8, 11, 17, 23, 29, 35, 41)])

#Update the colnames to proper names
colnames(Outcomes)[6] <- gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "30 Day Death Rate - ", colnames(Outcomes)[6])
colnames(Outcomes)[7] <- gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "30 Day Death Rate - ", colnames(Outcomes)[7])
colnames(Outcomes)[8] <- gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "30 Day Death Rate - ", colnames(Outcomes)[8])
colnames(Outcomes)[9] <- gsub("Hospital.30.Day.Readmission.Rates.from.", "30 Day Readmission Rate - ", colnames(Outcomes)[9])
colnames(Outcomes)[10] <- gsub("Hospital.30.Day.Readmission.Rates.from.", "30 Day Readmission Rate - ", colnames(Outcomes)[10])
colnames(Outcomes)[11] <- gsub("Hospital.30.Day.Readmission.Rates.from.", "30 Day Readmission Rate - ", colnames(Outcomes)[11])

#Unique and sorted list of states
stateList <- unique(Outcomes$State)
stateList <- sort(stateList)

filter <- function(condition, state, zip) {
  
  #initialize the filtered set
  filteredData <- Outcomes
  
  #subset the data based on filters
  #Conditions
  #If NO conditions are selected then no filtered columns
  if(!is.null(condition)) {
    i <- data.frame(grep(as.character(condition), names(filteredData),ignore.case = T, value = F))
    colnames(i)[1] <- "index"
    keepCols <- rbind(keepCols,i)
    filteredData <- subset(filteredData[,keepCols[,1]])
    rm(i)
  }
  
  #States
  #If NO conditions are selected then no filtered columns
  if(!is.null(state)) {
    filteredData <- subset(filteredData, filteredData[,"State"] %in% state)
  }
  
  #Zip Code
  if (length(zip) != 0) {
  if(zip !="") {
    filteredData <- subset(filteredData, filteredData[,"ZIP.Code"] == as.character(zip))
  }  
  }
  filteredData <- filteredData[order(filteredData[,6], filteredData[,7]),]
  
  return(filteredData)
  
}


measures <- function(filteredData) {
  
  x <- ncol(filteredData)
  i <- 6
  for (i in 6:x) {
    filteredData <- subset(filteredData, filteredData[,i] != "Not Available", na.rm = TRUE)
  }
  i <- 6
  for (i in 6:x) {
    temp <- as.numeric(filteredData[,i])
    if (i != 6) {
      histData <- cbind(histData, temp)
      names(histData)[i-5] <- names(filteredData)[i]
    }
    else {
      histData <- as.data.frame(temp)
      names(histData)[i-5] <- names(filteredData)[i]
    }   
  }
  
  return(histData)
  
}

shinyServer(
  function(input, output, session) {

    output$scatter <- renderPlot({
      input$run
      isolate({

        filteredData <- filter(input$condition, input$state, input$ZipText)
        filteredData <- measures(filteredData)
        
        qplot(filteredData[,1], filteredData[,2], data = filteredData, geom = c("point", "smooth"), 
                      main = paste("Hosptial Quality for: ", input$condition), xlab = "30 Day Death Mortality Rate", ylab = "30 Day Readmission Rate")

      })
    })


    #This code produces the data table for Top 5 by death
    output$top5death = renderTable({
      input$run
      isolate({
        
        filteredData <- filter(input$condition, input$state, input$ZipText)
        #Show the filtered data
        x <- 5
        y <- 1
        if (nrow(filteredData) < 5) { x <- nrow(filteredData) }
        if (nrow(filteredData) == 0) { y <- 0 }
        
        filteredData[y:x,]
        
      })
    })   
    
    #This code produces the data table for Top 5 by readmit
    output$top5readmit = renderTable({
      input$run
      isolate({
        
        filteredData <- filter(input$condition, input$state, input$ZipText)
        
        #sort by admit
        filteredData <- filteredData[order(filteredData[,7]),]
        
        #Show the filtered data
        x <- 5
        y <- 1
        if (nrow(filteredData) < 5) { x <- nrow(filteredData) }
        if (nrow(filteredData) == 0) { y <- 0 }
        
        filteredData[y:x,]
        
      })
    }) 
    
    #This code produces the data table for interacting with the data vs. charts/plots
    output$mytable = renderDataTable({
      input$run
      isolate({
            
            filteredData <- filter(input$condition, input$state, input$ZipText)
            #Show the filtered data
            filteredData

      })
    })

    ###########################################
    ##  Prompt controls - react to ResetAll  ##
    ###########################################

    output$conditionListDropDown <- renderUI({
      input$resetAll
      isolate({
        selectInput("condition", "Choose a Condition:", multiple = FALSE, selected = NULL, choices = c("Heart.Attack","Heart.Failure","Pneumonia"))
      })
    })


    output$stateListDropDown <- renderUI({
      input$resetAll
      isolate({
        selectInput("state", "Choose a State:", multiple = TRUE, selected = NULL, choices = stateList)
      })
    })

    output$inputZipText <- renderUI({
      input$resetAll
      isolate({
        textInput("ZipText",  "Zip:", value = NULL, placeholder = "(Optional) Type a single Zip")
      })
    })

    #############################
    ##Future additions to the app - City and County are not used in current version
    #############################

    output$inputCityText <- renderUI({
      input$resetAll
      isolate({
        textInput("CityText",  "City:", placeholder = "(Optional) Type a single city")
      })
    })

    output$inputCountyText <- renderUI({
      input$resetAll
      isolate({
        textInput("CountyText",  "County:", placeholder = "(Optional) Type a single county")
      })
    })



    }
)
