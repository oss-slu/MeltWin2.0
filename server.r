library(MeltR) #Needs to be removed when Anothy pushes code
server <- function(input,output, session){
  #Reactive list variable 
  values <- reactiveValues(masterFrame = NULL,numReadings = NULL)
  #Upload Project File
  upload <- observeEvent(eventExpr = input$inputFile,
                         handlerExpr = {
                           # Declaring variables
                           req(input$inputFile)
                           pathlengths <- c(unlist(strsplit(input$pathlengths,",")))
                           molStateVal <<- input$molState
                           helix <<- trimws(strsplit(input$helixInput,",")[[1]],which="both")
                           blank <<- as.numeric(input$blankSample)
                           if(molStateVal == "Heteroduplex"){
                             molStateVal <<- "Heteroduplex.2State"
                           }else if(molStateVal == "Homoduplex"){
                             molStateVal <<- "Homoduplex.2State"
                           }else{
                             molStateVal <<- "Monomolecular.2State"
                           }
                           removeUI(
                             selector = "div:has(> #helixInput)"
                           )
                           removeUI(
                             selector="div:has(> #molState)"
                           )
                           fileName <- input$inputFile$datapath
                           cd <- read.csv(file = fileName,header = FALSE)
                           df <- cd %>% select_if(~ !any(is.na(.)))
                           # Creating temporary frame to store sample data
                           columns <- c("Sample", "Pathlength", "Temperature", "Absorbance")
                           tempFrame <- data.frame(matrix(nrow = 0, ncol = 4))
                           colnames(tempFrame) <- columns
                           readings <- ncol(df)
                           # Loop that appends sample data 
                           p <- 1
                           for (x in 2:readings) {
                             col <- df[x]
                             sample <- rep(c(counter),times = nrow(df[x]))
                             pathlength <- rep(c(as.numeric(pathlengths[p])),times = nrow(df[x]))
                             col <- df[x]
                             t <- data.frame(sample,pathlength,df[1],df[x])
                             names(t) <- names(tempFrame)
                             tempFrame <- rbind(tempFrame, t)
                             p <- p + 1
                             counter <<- counter + 1
                           }
                           values$numReadings <- counter - 1
                           values$masterFrame <- rbind(values$masterFrame, tempFrame)
                           myConnecter <<- connecter(df = values$masterFrame,
                                                           NucAcid = helix,
                                                           Mmodel = molStateVal,
                                                           blank = blank)
                           myConnecter$constructObject()
                         }
  )
  #Outputs the post-processed data frame
  output$Table <- renderTable({
    return(values$masterFrame)})
  
  #Hides "Analysis" and "Results tabs until file successfully uploads
  observeEvent(
    eventExpr = is.null(values$numReadings),
    handlerExpr = {
      hideTab(inputId = "navbar",target = "Analysis")
      hideTab(inputId = "navbar",target = "Results")
    }
  )
  
  
  #Creates n tabPanels for the "Analysis" tabPanel
  observe({
    req(values$numReadings)
    lapply(start:values$numReadings,function(i){
      if(i != blank){
        data = values$masterFrame[values$masterFrame$Sample == i,]
        xmin = round(min(data$Temperature),1)
        xmax = round(max(data$Temperature),1)
        #output elements
        plotBoth = paste0("plotBoth",i)
        plotBestFit = paste0("plotBestFit",i)
        plotFit = paste0("plotFit",i)
        plotName = paste0("plot",i)
        plotSlider <- paste0("plotSlider",i)
        plotDerivative = paste0("plotDerivative",i)
        #Check box  and tab Panel variables
        firstDerivative = paste0("firstDerivative",i)
        bestFit = paste0("bestFit",i)
        tabName = paste("Sample",i,sep = " ")
        appendTab(inputId = "tabs",
                  tab = tabPanel(
                    tabName,
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          #side-panel code
                          h2("Features"),
                          checkboxInput(inputId = bestFit,label = "Best Fit"),
                          checkboxInput(inputId = firstDerivative,label = "First Derivative")
                        ),mainPanel(
                          #main-panel code
                          conditionalPanel(
                            condition = glue("!input.{firstDerivative} && !input.{bestFit}"),
                            plotOutput(plotName)
                          ),
                          conditionalPanel(
                            condition = glue("input.{firstDerivative} && !input.{bestFit}"),
                            plotOutput(plotDerivative)
                          ),
                          conditionalPanel(
                            condition = glue("input.{bestFit} && !input.{firstDerivative}"),
                            plotOutput(plotBestFit)
                          ),
                          conditionalPanel(
                            condition = glue("input.{firstDerivative} && input.{bestFit}"),
                            plotOutput(plotBoth)
                          ),
                          sliderInput(plotSlider,
                                      glue("Plot{i}: Range of values"),
                                      min = xmin,
                                      max = xmax,
                                      value = c(xmin,xmax),
                                      round = TRUE,
                                      step = .10,
                                      width = "85%")
                        )
                      )
                    )
                  ))
      }
      })
    start <<- values$numReadings + 1
    showTab(inputId = "navbar",target = "Analysis")
  })
  
  #Dynamically creates a renderPlot object of each absorbance readings
  observe({
    req(input$inputFile)
    for (i in 1:values$numReadings) {
      if(i != blank){
        local({
          myI <- i 
          plotDerivative = paste0("plotDerivative",myI)
          plotBestFit = paste0("plotBestFit",myI)
          plotBoth = paste0("plotBoth",myI)
          plotName = paste0("plot",myI)
          plotSlider = paste0("plotSlider",myI)
          #plot containing raw data
          output[[plotName]] <- renderPlot({
            myConnecter$constructRawPlot(myI) +
              geom_vline(xintercept = input[[plotSlider]][1]) +
              geom_vline(xintercept = input[[plotSlider]][2])
          })
          #plot containing first derivative with raw data
          output[[plotDerivative]] <- renderPlot({
            myConnecter$constructFirstDerivative(myI) +
              geom_vline(xintercept = input[[plotSlider]][1]) +
              geom_vline(xintercept = input[[plotSlider]][2])
          })
          #plot containing best fit with raw data
          output[[plotBestFit]] <- renderPlot({
            myConnecter$constructBestFit(myI) + 
              geom_vline(xintercept = input[[plotSlider]][1]) +
              geom_vline(xintercept = input[[plotSlider]][2])
          })
          #plot containing best, first derivative, and raw data
          output[[plotBoth]] <- renderPlot({
            myConnecter$constructBoth(myI) + 
              geom_vline(xintercept = input[[plotSlider]][1]) +
              geom_vline(xintercept = input[[plotSlider]][2])
          })
      })
      }
    }
  })

  #code that plots a van't hoff plot
  output$vantplots <- renderPlot({
    Model <- paste(molStateVal,".2State", sep = "")
    data <- meltR.A(data_frame = df,
                    blank = 1,
                    NucAcid = helix,
                    Mmodel = Model)
    caluclations <- data$Method.2.data
    InverseTemp <- caluclations$invT
    LnConcentraion <- caluclations$lnCt
    plot(LnConcentraion,InverseTemp)
    
  }, res = 100)
}


# Run the app
#shinyApp(ui = ui, server = server)
