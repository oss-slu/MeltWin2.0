library(shiny)
library(dplyr)
library(ggplot2)
#library(MeltR)
library(shiny)

# Define UI ----
ui <- navbarPage("MeltShiny",
                 navbarMenu("File",
                            tabPanel("Add Data", 
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           textInput(label="Enter the Pathlength for each Absorbance Reading(separated by commas)",
                                                     placeholder = "E.g: 2,5,3,2,...",
                                                     inputId = "pathlengths"),
                                           fileInput(label = "Add Data",
                                                     inputId = "inputFile",
                                                     multiple = FALSE,
                                                     accept = ".csv")
                                         ),
                                         mainPanel(
                                           tableOutput("contents")
                                         )
                                       )
                                     )
                            )
                 )
)      

server <- function(input,output){
  #Reactive list variable 
  values <- reactiveValues(masterFrame=NULL,up=NULL)
  #Upload Project File
  upload <- observeEvent(eventExpr =input$inputFile,
                         handlerExpr = {
                           req(input$inputFile)
                           #Declaring variables
                           pathlengths <- c(unlist(strsplit(input$pathlengths,",")))
                           req(input$inputFile)
                           fileName = input$inputFile$datapath
                           cd <- read.csv(file = fileName,header=FALSE)
                           df <- cd %>% select_if(~ !any(is.na(.)))
                           #Creating temporary frame to store sample data
                           columns <- c("Sample", "Pathlength", "Temperature", "Absorbance")
                           tempFrame <- data.frame(matrix(nrow = 0, ncol = 4))
                           colnames(tempFrame) <- columns
                           readings <- ncol(df)
                           #Loop that appends sample data 
                           counter <- 1
                           for (x in 2:readings){
                             col <- df[x]
                             sample<-rep(c(counter),times=nrow(df[x]))
                             pathlength<-rep(c(pathlengths[counter]),times=nrow(df[x]))
                             col <- df[x]
                             t <- data.frame(sample,pathlength,df[1],df[x])
                             names(t) <- names(tempFrame)
                             tempFrame <- rbind(tempFrame, t)
                             counter <- counter + 1
                           }
                           values$numReadings <- counter-1
                           values$masterFrame <- tempFrame
                           values$up <- 1
                           #print(values$up)
                         }
  )
  output$contents <- renderTable({
    return(values$masterFrame)})
}
 
 
# Run the app
shinyApp(ui = ui, server = server)