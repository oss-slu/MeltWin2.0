# Back end
server <- function(input,output, session){
  #Reactive list variable 
  values <- reactiveValues(masterFrame = NULL,numReadings = NULL)
  #Upload Project File
  
  upload <- observeEvent(eventExpr = input$inputFile,
                         handlerExpr = {
                           # Declaring variables
                           pathlengths <- c(unlist(strsplit(input$pathlengths,",")))
                           req(input$inputFile)
                           fileName <- input$inputFile$datapath
                           cd <- read.csv(file = fileName,header = FALSE)
                           df <- cd %>% select_if(~ !any(is.na(.)))
                           # Creating temporary frame to store sample data
                           columns <- c("Sample", "Pathlength", "Temperature", "Absorbance")
                           tempFrame <- data.frame(matrix(nrow = 0, ncol = 4))
                           colnames(tempFrame) <-columns
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
      tabName = paste("Sample",i,sep=" ")
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
                        checkboxInput(inputId=firstDerivative,label="First Derivative"),
                        checkboxInput(inputId=bestFit,label="Best Fit"),
                        hr(),
                        textInput(fitIterations,"Enter the number of combitions to test for fitting.",value=100),
                        actionButton(fitData,"Fit Data")
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
                          plotOutput(plotFit)
                        ),
                        conditionalPanel(
                          condition = glue("input.{firstDerivative} && input.{bestFit}"),
                          plotOutput(plotBoth),
                        ),
                        sliderInput(plotSlider,
                                    glue("Plot{i}: Range of values"),
                                    min=xmin,
                                    max=xmax,
                                    value=c(xmin,xmax),
                                    round=TRUE,
                                    step=.10,
                                    width="85%")
                      )
                    )
                  )
                ))
    })
    start <<- values$numReadings + 1
    showTab(inputId = "navbar",target = "Analysis")
  })
  
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
            ylim(min(data$Absorbance),max(data$Absorbance)+1) +
            geom_vline(xintercept = input[[plotSlider]][1]) +
            geom_vline(xintercept = input[[plotSlider]][2])
        })
        #plot containing first derivative with raw data
        plotDerivative = paste0("plotDerivative",myI)
        output[[plotDerivative]] <- renderPlot({
          data = values$masterFrame[values$masterFrame$Sample == myI,]
          data %>%
            arrange(Temperature) %>%
            mutate(slope = (Absorbance - lag(Absorbance))/
                     (Temperature - lag(Temperature))) %>%
            ggplot(aes(Temperature)) +
            geom_line(aes(y= Absorbance), size = 1.2) +
            geom_smooth(aes(y= slope * 20 + 1.4), se = FALSE, size = 0.8) +
            ylim(min(data$Absorbance),max(data$Absorbance)+1) +
            theme_classic() +
            geom_vline(xintercept = input[[plotSlider]][1]) +
            geom_vline(xintercept = input[[plotSlider]][2])
          #scale_y_continuous(sec.axis = sec_axis(trans = ~(.x - 1.4)/20, name = "slope"))
        })
        #plot containing best fit line with raw data
        plotFit = paste0("plotFit",myI)
        output[[plotFit]] = renderPlot({
          data = values$masterFrame[values$masterFrame$Sample == myI,]
          fit <- nls(Absorbance ~ SSfpl(Temperature, left, right, midpt, scale),
                     data = data)
          fit2 <- nls(Absorbance ~ left+(right-left)/(1+exp((midpt-Temperature)/scale)) 
                      + (Temperature-midpt)*slope,
                      start = c(as.list(coef(fit)), slope = 0),
                      data = data)
          pred <- data.frame(Temperature = data$Temperature,
                             Absorbance = predict(fit),
                             Absorbance2 = predict(fit2))
          ggplot(data, aes(x = Temperature, y = Absorbance)) +
            geom_point(color = "red") +
            #geom_line(data=pred, lwd = 2) +
            geom_line(data=pred, aes(y=Absorbance2)) +
            ylim(min(data$Absorbance),max(data$Absorbance)+1) +
            theme_classic() +
            geom_vline(xintercept = input[[plotSlider]][1]) +
            geom_vline(xintercept = input[[plotSlider]][2])
        })
        #plot containing both first derivative & best fit line with raw data
        plotBoth = paste0("plotBoth",myI)
        output[[plotBoth]] = renderPlot({
          data = values$masterFrame[values$masterFrame$Sample == myI,]
          fit <- nls(Absorbance ~ SSfpl(Temperature, left, right, midpt, scale),
                     data = data)
          fit2 <- nls(Absorbance ~ left+(right-left)/(1+exp((midpt-Temperature)/scale)) 
                      + (Temperature-midpt)*slope,
                      start = c(as.list(coef(fit)), slope = 0),
                      data = data)
          pred <- data.frame(Temperature = data$Temperature,
                             Absorbance = predict(fit),
                             Absorbance2 = predict(fit2))
          data %>%
            arrange(Temperature) %>%
            mutate(slope = (Absorbance - lag(Absorbance))/
                     (Temperature - lag(Temperature))) %>%
            ggplot(aes(Temperature)) +
            geom_line(aes(y= Absorbance), size = 1.2) +
            geom_smooth(aes(y= slope * 20 + 1.4), se = FALSE, size = 0.8) +
            geom_line(data=pred, aes(y=Absorbance2),color="red") +
            ylim(min(data$Absorbance),max(data$Absorbance)+1) +
            theme_classic() +
            geom_vline(xintercept = input[[plotSlider]][1]) +
            geom_vline(xintercept = input[[plotSlider]][2])
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
}



# Run the app
#shinyApp(ui = ui, server = server)