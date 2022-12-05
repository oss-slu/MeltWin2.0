library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(glue)
library(methods)
library(MeltR)
library(shiny)

# The UI consists of a navbar page, with a single drop down menu, "File" , which contains a single option "Add data".

ui <- navbarPage(title = "MeltShiny",id = "navbar",
                 navbarMenu("File",
                            # When the user clicks the "Add Data" tab panel, a fluid page is created below the nav bar.
                            # This page contains a side bar panel and a main panel.
                            # The side bar contains the given options a user has when they click one of the options in the nav bar.
                            # The main panel contains the graphs or tables.
                            tabPanel("Add Data", 
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           textInput(label = "Enter the blank sample for the dataset",
                                                     placeholder = "E.g: 1",
                                                     value = 1,
                                                     inputId = "blankSample"),
                                           textInput(label = "Enter the Pathlength for each absorbance reading. (Note, they should be separated by commas and have no spaces inbetween.)",
                                                     placeholder = "E.g: 2,5,3,2,...",
                                                     inputId = "pathlengths"),
                                           textInput(label = "Enter the sequence information in the following order: Nucleic Acid, A sequence, and its complement)",
                                                     placeholder = "E.g: RNA,CGAAAGGU,ACCUUUCG",
                                                     inputId = "helixInput"),
                                           selectInput("molState", 
                                                       "Select the molecular state.(Please note that your selection will apply to all samples, beyond just the ones in the current dataset.)", 
                                                       choices = c("Heteroduplex", "Homoduplex","Monomolecular"), 
                                                       selected = "Heteroduplex"),
                                           selectInput("wavelength", 
                                                       "Select the wavelength. (Note that thermodynamic parameters can only
                                                                                 be collected for DNA at 260 nm.)", 
                                                       choices = c("260", "300","295","290","285","280","275","270",
                                                                   "265","260","255","250","245","240","235","230"), 
                                                       selected = "260"),
                                           fileInput(label = "Select the dataset file.",                                        
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
                                           #tableOutput("Console")
                                         )
                                       )
                                     )
                            ),
                 ),tabPanel("Analysis",
                            tabsetPanel(id = "tabs")),
                 tabPanel("Data Visualization",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h2(align = "center","Data Visualization"),
                              strong("Check Which Plots to View"),
                              uiOutput("checkboxes"),
                              actionButton("automate","Automate Fitting Data")
                            ),mainPanel(
                              uiOutput("dataVisualContents"),
                            )
                          )
                 ),
                 navbarMenu("Results",
                            tabPanel("Vant Hoff Plots", 
                                     fluidPage(
                                         mainPanel(
                                           plotOutput("vantplots"),
                                         )
                                       )
                                     ),
                            tabPanel("Results Table", 
                                     fluidPage(
                                       mainPanel(
                                         "Individual fits",
                                         tableOutput("resulttable"),
                                         "Summary Tables",
                                         tableOutput("summarytable"),
                                         tableOutput("summarytable2"),
                                         "Error",
                                         tableOutput("error")
                                       )
                                     )
                            )
                            )
) 
