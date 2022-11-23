library(MeltR) #Needs to be removed when Anothy pushes code
server <- function(input,output, session){
  #Reactive list variable 
  values <- reactiveValues(masterFrame = NULL,numReadings = NULL)
  #Upload Project File
  upload <- observeEvent(eventExpr = input$inputFile,
                         handlerExpr = {
                           # Declaring variables
                           pathlengths <- c(unlist(strsplit(input$pathlengths,",")))
                           molStateVal <<- input$molState
                           helix <<- c(unlist(strsplit(input$helixInput,",")))
                           req(input$inputFile)
                           removeUI(
                             selector = "div:has(> #helixInput)"
                             selector = "div:has(>> #molState)"
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
                             pathlength <- rep(c(pathlengths[p]),times = nrow(df[x]))
                             col <- df[x]
                             t <- data.frame(sample,pathlength,df[1],df[x])
                             names(t) <- names(tempFrame)
                             tempFrame <- rbind(tempFrame, t)
                             p <- p + 1
                             counter <<- counter + 1
                           }
                           values$numReadings <- counter - 1
                           values$masterFrame <- rbind(values$masterFrame, tempFrame)
                           values$uploaded <- 1
                         }
  )
  output$Table <- renderTable({
    return(values$masterFrame)})
  
  #code that hides "Plots" drop-down hidden until file successfully uploads
  observeEvent(
    eventExpr = is.null(values$numReadings),
    handlerExpr = {
      hideTab(inputId = "navbar",target = "Analysis")
    }
  )
  
  
  #code that creates n tabPanels for the "Analysis" tabPanel
  observe({
    req(values$numReadings)
    lapply(start:values$numReadings,function(i){
      tabName = paste("Sample",i,sep = " ")
      plotName = paste0("plot",i)
      plotFit = paste0("plotFit",i)
      plotBoth = paste0("plotBoth",i)
      plotSlider <- paste0("plotSlider",i)
      data = values$masterFrame[values$masterFrame$Sample == i,]
      xmin = round(min(data$Temperature),1)
      xmax = round(max(data$Temperature),1)
      plotDerivative = paste0("plotDerivative",i)
      fitData = paste0("fit",i)
      firstDerivative = paste0("firstDerivative",i)
      bestFit = paste0("bestFit",i)
      fitIterations = paste0("fitIteration",i)
      appendTab(inputId = "tabs",
                tab = tabPanel(
                  tabName,
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        #side-panel code
                        h2("Features"),
                        checkboxInput(inputId = firstDerivative,label = "First Derivative")
                      ),mainPanel(
                        #main-panel code
                        conditionalPanel(
                          condition = glue("!input.{firstDerivative}"),
                          plotOutput(plotName)
                        ),
                        conditionalPanel(
                          condition = glue("input.{firstDerivative}"),
                          plotOutput(plotDerivative)
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
    })
    start <<- values$numReadings + 1
    showTab(inputId = "navbar",target = "Analysis")
  })
  
  firstDerivativePlot <- function(df,plotSlider){
    df = df[,c(3,4)]
    columns = c("time","intensity")
    colnames(df) = columns
    time <- df$time
    
    dataInput <- data.frame(intensity = df$intensity, time = df$time)
    #normalizedInput <- normalizeData(dataInput, dataInputName = "sample001")
    parameterVector <- multipleFitFunction(dataInput = df,
                                           model = "sigmoidal",
                                           n_runs_min = 20,
                                           n_runs_max = 500)
    
    #Check the results
    if(parameterVector$isThisaFit){
      intensityTheoretical <- sigmoidalFitFormula(time,
                                                  maximum = parameterVector$maximum_Estimate,
                                                  slopeParam = parameterVector$slopeParam_Estimate,
                                                  midPoint = parameterVector$midPoint_Estimate)
      
      comparisonData <- cbind(dataInput, intensityTheoretical)
      
      require(ggplot2)
      ggplot(comparisonData)+
        geom_point(aes(x = time, y = intensity)) +
        geom_line(aes(x = time, y = intensityTheoretical), color = "blue") +
        xlab("Temperature") +
        ylab("Absorbance") +
        theme_classic() +
        expand_limits(x = 0, y = 0) +
        geom_vline(xintercept = input[[plotSlider]][1]) +
        geom_vline(xintercept = input[[plotSlider]][2])
    }
  }
  
  #Dynamically creates a renderPlot object of each absorbance readings
  observe({
    req(input$inputFile)
    for (i in 1:values$numReadings) {
      local({
        myI <- i 
        plotSlider = paste0("plotSlider",myI)
        #plot containing raw data
        plotName = paste0("plot",myI)
        output[[plotName]] <- renderPlot({
          data = values$masterFrame[values$masterFrame$Sample == myI,]
          ggplot(data, aes(x = Temperature, 
                           y = Absorbance)) +
            geom_point() + theme_classic() +
            ylim(min(data$Absorbance),max(data$Absorbance) + 1) +
            geom_vline(xintercept = input[[plotSlider]][1]) +
            geom_vline(xintercept = input[[plotSlider]][2])
        })
        #plot containing first derivative with raw data
        plotDerivative = paste0("plotDerivative",myI)
        output[[plotDerivative]] <- renderPlot({
          data = values$masterFrame[values$masterFrame$Sample == myI,]
          firstDerivativePlot(data,plotSlider)
      })
    })
    }
  })
  
  #Dynamically renders & outputs the created plots
  #Dynamically renders & outputs inputSliders for each plot
  #Output Structure: two plots & sliders per column
  output$dataVisualContents <- renderUI({
    req(input$inputFile)
    lapply(1:values$numReadings, function(i){
      plotSlider <- paste0("plotSlider",i)
      plotName <- paste0("plot",i)
      nextPlot <- paste0("plot",i + 1)
      nextSlider = paste0("plotSlider",i + 1)
      data <- values$masterFrame[values$masterFrame$Sample == i,]
      xmin <- min(data$Temperature)
      xmax <- max(data$Temperature)
      #even # of plots
      if (values$numReadings %% 2 == 0) {
        if (i %% 2 != 0) {
          div(
            fluidRow(
              column(6,plotOutput(plotName)),
              column(6,plotOutput(nextPlot))
            ),
            fluidRow(
              column(6,sliderInput(plotSlider,glue("Plot{i}: Range of values"),min = xmin,max = xmax,value = c(xmin,xmax))),
              column(6,sliderInput(nextSlider,glue("Plot{i + 1}: Range of values"),min = xmin,max = xmax,value = c(xmin,xmax))),
            ),
            hr()
          )
        }
        #odd # of plots
      }else{
        if (i == values$numReadings) {
          tagList(
            plotOutput(plotName),
            sliderInput(plotSlider,glue("Plot{i}: Range of values"),min = xmin,max = xmax,value = c(xmin,xmax))
          )
        }else {
          if (i %% 2 != 0) {
            div(
              fluidRow(
                column(6,plotOutput(plotName)),
                column(6,plotOutput(nextPlot))
              ),
              fluidRow(
                column(6,sliderInput(plotSlider,glue("Plot{i}: Range of values"),min = xmin,max = xmax,value = c(xmin,xmax))),
                column(6,sliderInput(nextSlider,glue("Plot{i+1}: Range of values"),min = xmin,max = xmax,value = c(xmin,xmax)))
              ),
              hr()
            )
          }
        }
      }
    })
  })
  
  #Dynamically output # of check boxes
  output$checkboxes <- renderUI({
    req(input$inputFile)
    boxOutput <- lapply(1:values$numReadings, function(i){
      plotName <- paste0("plot",i)
      plotBox <- paste0("plotBox",i)
      checkboxInput(plotBox,plotName,value = TRUE)
    })
    do.call(tagList,boxOutput)
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
