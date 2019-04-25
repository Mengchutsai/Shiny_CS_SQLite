library(caret)
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(RSQLite)
#setwd("C:/Users/mtsai1/Documents/Shiny_CS_SQLite_2")
#con <- dbConnect(RSQLite::SQLite(), "sample.sqlite")
#dbGetQuery(con, "select * from csdata")
#dbDisconnect(con)
#dbSendQuery(con, "create table csdata (children int ,age int ,bmi float)")

#input_data <- read_csv("C:/Users/mtsai1/Documents/ShinyCS/Shiny_CS_test/test.csv")


dataset <- read_csv("insurance.csv")
#factors_dataset <- c("sex", "gender", "smoker","region")
#dataset[factors_dataset]<-lapply(dataset[factors_dataset], function (x) as.factor(x))
#delete row with missings 
dataset <- dataset[complete.cases(dataset), ]
dataset$children <- as.numeric(dataset$children)
#write.csv(sample_input_data, file = "input_data.csv")
#colnames(input_data) = c("children","age","bmi")
#con <- dbConnect(SQLite(), dbname="sample.sqlite")
#dbWriteTable(con, "csdata", input_data, append = TRUE)
#dbDisconnect(con)
##################### Response variable transformation #####################


#pdf("hist_Charges_trans.pdf")
# log_charges <- log(dataset$charges)
# ggplot(dataset, aes(x = log_charges)) + 
#   geom_histogram(aes(y =..density..),
#                  breaks = seq(6.5, 12, by =0.45), 
#                  colour = "black", 
#                  fill = "darkolivegreen") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(log_charges), 
#                             sd = sd(log_charges)))

#Split data into train and test sets
set.seed(54321)
indexes <- createDataPartition(dataset$charges,
                               times = 1,
                               p = 0.7,
                               list=FALSE)

dataset.train <- dataset[indexes,]
dataset.test <- dataset[-indexes,]


set.seed(54321)
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)
################################## Linear Regression Model ###############################################
caret.lm <- train(log(charges) ~ age+bmi+children, 
                  data = dataset.train,
                  method = "lm",
                  preProcess = c("center", "scale"),
                  trControl = train.control
)
summary(caret.lm)
finalModel <- caret.lm$finalModel
###############save model############
#save(finalModel , file = 'finalModel.rda')
#load("finalModel.rda")    # Load saved model

### Application###
server <- shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize = 800*1024^2)   # This is a number which specifies the maximum web request size, 
  # which serves as a size limit for file uploads. 
  # If unset, the maximum request size defaults to 5MB.
  # The value I have put here is 80MB
  
  
  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Uploaded data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    
    inFile <- input$file
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file$datapath, col_names = TRUE)
      print(input_data)
    }
  })
  
  
  
  observeEvent(input$file, {
    
    
    #db_insert_into(pool, "test", data.frame(id = input$new.id, user = input$new.user))
    input_data =  readr::read_csv(input$file$datapath, col_names = TRUE)
    con <- dbConnect(RSQLite::SQLite(), "sample.sqlite")
    #db_insert_into(con, "csdata", input_data)
    DBI::dbWriteTable(con, "csdata", input_data, append = TRUE)
    dbDisconnect(con)
    
    #write.csv(input_data, file = "C:/Users/mtsai1/Desktop/input_data2.csv")
    
  })
  

  predictions<-reactive({
    
    inFile <- input$file
    
    if (is.null(inFile)){
      return(NULL)
    }
    else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file$datapath, col_names = TRUE)
        
        colnames(input_data) = c("children","age","bmi")
        
        prediction = predict(finalModel, input_data)
        
        input_data_with_prediction = cbind(input_data,prediction)
        input_data_with_prediction
        
      })
    }
  })
  
  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Predicted Value')
    }
  })
  
  output$sample_predictions = renderTable({   # the last 6 rows to show
    pred = predictions()
    print(pred)
    
  })
  
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
  
})


library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(skin="red",
                    dashboardHeader(title=tags$em("Shiny Application Capstone Project", style="color:black;font-size:150%"),titleWidth = 500),
                    
                    dashboardSidebar(width = 250,
                                     sidebarMenu(
                                       br(),
                                       menuItem(tags$em("Introduction Page",style="font-size:120%"),icon=icon("chart-area"),tabName="intro"),
                                       menuItem(tags$em("Upload Dataset",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                       menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                                       
                                       
                                     )
                    ),
                    
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName="intro",
                                br(),
                                tags$h4("Bank of University wants to start offering self-funded Health Plans (SHP). 
                                        SHP is cheap since BOU does not have to pay the insurance company for extra fees.  
                                        BOU has received historical medical cost records from the current insurance carrier. 
                                        The Goal is to use the dataset to build a predictive model to project future medical 
                                        costs so BOU knows how it needs to price SHP and understand the potential risk it will be taking."
                                        , style="font-size:150%"),
                                
                                br(), br()
                        ),
                        
                        tabItem(tabName="data",
                                
                                br(),
                                tags$h4("To predict using this model, upload test data in csv format 
                                        (you can change the code to read other data types) by using the
                                        button below. Then, go to the Download Predictions section in 
                                        the sidebar to download the predictions.", style="font-size:150%"),
                                br(), br(), br(),
                                fluidRow(
                                  br(), 
                                  column(width = 6,
                                         fileInput('file', em('Upload test data in csv format ',style="text-align:center;color:black;font-size:120%"),multiple = FALSE,
                                                   accept=c('.csv')),
                                         
                                         column(width = 6,     
                                                uiOutput("sample_input_data_heading"),
                                                tableOutput("sample_input_data"))
                                         
                                  ))),
                        
                        
                        tabItem(tabName="download",
                                fluidRow(
                                  br(),
                                  column(width = 8,
                                         tags$h4("After you upload a test dataset, you can download the predictions in csv format by
                                                 clicking the button below.", 
                                                 style="font-size:150%"),
                                         br(),
                                         br()
                                  )),
                                fluidRow(
                                  
                                  column(width = 6,
                                         downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:black;font-size:120%"))),
                                  column(width = 12,
                                         uiOutput("sample_prediction_heading"),
                                         tableOutput("sample_predictions")
                                  )
                                  
                                ))
                        )
                      
                        ) )


shinyApp(ui = ui, server = server)