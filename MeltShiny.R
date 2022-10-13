library(shiny)
library(dplyr)
library(ggplot2)
#library(MeltR)
library(shiny)

# The UI consists of a navbar page, with a single drop down menu, "File" , which contains a single option "Add data".
ui <- navbarPage("MeltShiny",
                 navbarMenu("File",
                            # When the user clicks the "Add Data" tab panel, a fluid page is created below the nav bar.
                            # This page contains a side bar panel and a main panel.
                            # The side bar contains the given options a user has when they click one of the options in the nav bar.
                            # The main panel contains the graphs or tables.
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
                                           tableOutput("Table")
                                         )
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Help",
                            tabPanel("Absorbance in MeltR", 
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                         ),
                                         mainPanel(
                                           tableOutput("Console")
                                         )
                                       )
                                     )
                            )
                 )
)      

# Back end
server <- function(input,output){
  #Reactive list variable 
  values <- reactiveValues(masterFrame=NULL,up=NULL)
  #Upload Project File
  upload <- observeEvent(eventExpr =input$inputFile,
                         handlerExpr = {
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
                           values$masterFrame <- rbind (values$masterFrame, tempFrame)
                           values$up <- 1
                         }
  )
  
  output$Table <- renderTable({
    return(values$masterFrame)})
}
 
# Run the app
shinyApp(ui = ui, server = server)