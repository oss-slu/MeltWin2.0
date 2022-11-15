library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(glue)
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
                                           textInput(label = "Enter the Pathlength for each Absorbance Reading(separated by commas)",
                                                     placeholder = "E.g: 2,5,3,2,...",
                                                     inputId = "pathlengths"),
                                           fileInput(label = "Add Data",
                                                     inputId = "inputFile",
                                                     multiple = FALSE,
                                                     accept = ".csv"),
                                           textInput(label = "Enter the sequence information in the following order: Nucleic Acid, A sequence, and its complement)",
                                                     placeholder = "E.g: RNA,CGAAAGGU,ACCUUUCG",
                                                     inputId = "helixInput")
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
                          
                          
                 )
) 