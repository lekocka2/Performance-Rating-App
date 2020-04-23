#
#  App created by Logan Kocka   19 Feb 2020
#

library(rsconnect)
library(utils)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(pracma)
library(readr) 
library(lubridate)
library(zoo)
library(shinycssloaders) 
library(devtools)
library(rhandsontable)
library(clipr)
library(EmersonDataScience)
library(DT)
library(formattable)
library(knitr)
library(datapasta)
library(jsonlite)

#sourceDir("C:/Users/krrichar/Desktop/Coef/Functions")
source("functions.R")

ui <- fluidPage(
  navbarPage("Rating App",
             #first tab
             tabPanel("Create",
                      #Text
                      fluidRow(column(12,
                                      h4("Make Coefficients and Curves"))),
                      #Sidebar panel with inputs
                      fluidRow(sidebarPanel(width = 12,
                                            fluidRow(
                                              column(2,
                                                     textInput("refrigerant", "Refrigerant", value="R410A.mix", width='200px')
                                              ),
                                              column(1, 
                                                     numericInput("sh", "Superheat", value=20, width='150px')
                                              ),
                                              column(1,
                                                     numericInput("sc", "Subcooling", value=15, width='150px')
                                              ),
                                              column(2,
                                                     numericInput("minEvap", "Min evap temp (F)", value=-10, width='150px'),
                                                     numericInput("minCond", "Min cond temp (F)", value=80, width='150px')
                                              ),
                                              column(2,
                                                     numericInput("maxEvap", "Max evap temp (F)", value=60, width='150px'),
                                                     numericInput("maxCond", "Max cond temp (F)", value=150, width='150px')
                                              ),
                                              column(2,
                                                     numericInput("incEvap", "Increment by", value=5, width='150px'),
                                                     numericInput("incCond", "Increment by", value=10, width='150px')
                                              ),
                                              column(2,
                                                     numericInput("displacement", "Displacement", value=NULL, width='150px'))
                                            ),
                                            radioButtons("choice1", "Input Type",
                                                         c("Coefficients" = "coeffsChoice1",
                                                           "ELT Data" = "testValuesChoice1")),
                                            
                                            #conditional 1
                                            conditionalPanel(condition = "input.choice1 == 'coeffsChoice1'",
                                                             fluidRow(column(2,
                                                                             actionButton("paste", "Paste"),
                                                                             br(),br(),
                                                                             "Unprotect sheet, select cells, change type to 'number',",
                                                                             "increase # decimals (important!) to desired number, press ctrl+c",
                                                                             br(),br(),br(),
                                                                             actionButton("reset", "Reset")
                                                             ),
                                                             column(3,
                                                                    dataTableOutput("table1A", width='100px')
                                                             ),
                                                             column(3,
                                                                    numericInput("numPtsAdded", "Add additional test points?", value=0, width='240px'),
                                                                    conditionalPanel(condition = "input.numPtsAdded > '0'",
                                                                      rHandsontableOutput("addPts"),
                                                                      br(),
                                                                      actionButton("save", "Recalculate"))
                                                             ),
                                                             column(2,
                                                                    textInput("condition", "Simulate Condition:", value="50/115", width='140px'),
                                                                    numericInput("desired", "Desired EER:", value=18, width='140px'),
                                                                    htmlOutput("shiftOutput")
                                                             ),
                                                             column(2,
                                                                    numericInput("adjCap", "Adjust Capacity/Mass Flow", value=1, width='140px'),
                                                                    numericInput("adjPow", "Adjust Power/Current", value=1, width='140px'),
                                                                    actionButton("adjust", "Adjust & Plot")
                                                             )),
                                                             fluidRow(
                                                               column(6,
                                                                      plotOutput("capCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("currCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("measMFCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("percDiffCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("volEffyCurve1A"),
                                                                      br(),br(),br(),br(),br(),br()
                                                               ),
                                                               column(6,
                                                                      plotOutput("powCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("EERCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("calcMFCurve1A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("isenEffyCurve1A")
                                                               ))
                                            ),
                                            
                                            #conditional 2
                                            conditionalPanel(condition = "input.choice1 == 'testValuesChoice1'",
                                                             fluidRow(
                                                               column(4,
                                                                      "Accepts .csv file type and must contain at least 12 unique test points.",
                                                                      fileInput("upload1B", "File Upload", multiple=F, accept=c(".csv"), width = '400px')
                                                               ),
                                                               column(8,
                                                                      formattableOutput("cooksDist", width = "65%"),
                                                                      textOutput("omitThese")
                                                                      )),
                                                             
                                                             DT::dataTableOutput("uploadDeleteRows"),
                                                             
                                                             actionButton("calculate", "Calculate/Reset"),
                                                             fluidRow(
                                                               column(7,
                                                                      tableOutput("createCoeffs1B")
                                                               ),
                                                               column(2,
                                                                      textInput("condition2", "Simulate Condition:", value="50/115", width='140px'),
                                                                      numericInput("desired2", "Desired EER:", value=18, width='140px'),
                                                                      
                                                                      htmlOutput("shiftOutput2")
                                                               ),
                                                               column(2,
                                                                      numericInput("adjCap2","Adjust Capacity/Mass Flow",value=1,width='140px'),
                                                                      numericInput("adjPow2","Adjust Power/Current",value=1,width='140px'),
                                                                      actionButton("adjust2","Adjust & Plot")
                                                               )),
                                                             fluidRow(
                                                               column(6,
                                                                      plotOutput("capCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("currCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("measMFCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("percDiffCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("volEffyCurve1B"),
                                                                      br(),br(),br(),br(),br(),br()
                                                               ),
                                                               column(6,
                                                                      plotOutput("powCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("EERCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("calcMFCurve1B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("isenEffyCurve1B")
                                                               )
                                                             ))
                      )), #end sidebar
                      #Main panel with outputs
                      mainPanel(
                        
                      )
             ),
             #########################################################
             #second tab
             tabPanel("Compare",
                      fluidRow(column(12,
                                      h4("Compare Compressors"))),
                      #Sidebar panel with inputs
                      fluidRow(sidebarPanel(width = 12,
                                            fluidRow(
                                              column(2,
                                                     textInput("refrigerant2", "Refrigerant", value="R410A.mix", width='200px')
                                              ),
                                              column(1, 
                                                     numericInput("sh2", "Superheat", value=20, width='150px')
                                              ),
                                              column(1,
                                                     numericInput("sc2", "Subcooling", value=15, width='150px')
                                              ),
                                              column(2,
                                                     selectInput("envel", "Envelope:", 
                                                                 c("Fixed Speed"="fixedspeed", 
                                                                   "Two Stage"="twostage", 
                                                                   "Other"="other"))
                                              ),
                                              column(2,
                                                     numericInput("displacement2", "Displacement", value=NULL,width='150px')),
                                              conditionalPanel(condition = "input.envel == 'other'",
                                                               column(3,
                                                                      textInput("evapEnvTemps", "Evap (F) envelope points", value = "-20,-20,35,55,55"),
                                                                      textInput("condEnvTemps", "Cond (F) envelope points", value = "80,95,145,145,80"))
                                              )),
                                            radioButtons("choice2", "Input Type",
                                                         c("Coefficients" = "coeffs2",
                                                           "Coefficients & ELT Data" = "both2")),
                                            #first conditional panel
                                            conditionalPanel(condition = "input.choice2 == 'coeffs2'",
                                                             fluidRow(
                                                               column(6,
                                                                      actionButton("paste2A.1", "Paste"),
                                                                      dataTableOutput("table2A.1")),
                                                               column(6,
                                                                      actionButton("paste2A.2", "Paste"),
                                                                      dataTableOutput('table2A.2'))),
                                                             fluidRow(
                                                               column(12,
                                                                      div(formattableOutput("compareTable2A"), style = "height:600px; overflow-y: scroll;overflow-x: scroll;", fixedHeader=T),
                                                                      br())),
                                                             fluidRow(
                                                               column(6,
                                                                      plotOutput("capDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("currDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("MeasMFDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("isenEffyPlot2A"),
                                                                      br(),br(),br(),br(),br(),br()
                                                               ),
                                                               column(6,
                                                                      plotOutput("powDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("EERDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("CalcMFDifPlot2A"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("volEffyPlot2A")
                                                               ))
                                            ), #end conditional panel
                                            #second conditional panel
                                            conditionalPanel(condition = "input.choice2 == 'both2'",
                                                             fluidRow(
                                                               column(3,
                                                                      "Unprotect sheet, select cells, change type to 'number', increase #", 
                                                                      "decimals (important!) to desired number, press ctrl+c",
                                                                      br(),br(),
                                                                      actionButton("paste2B", "Paste"),
                                                                      br(),br(),
                                                                      dataTableOutput("table2B")
                                                               ),
                                                               column(6,
                                                                      "Accepts .csv file type and must contain at least 12 unique test points.",
                                                                      br(),br(),
                                                                      fileInput("upload2B", "File Upload", multiple=F, accept=c(".csv"), width = '400px'),
                                                                      dataTableOutput("createCoeffs2B")
                                                               )),
                                                             fluidRow(
                                                               div(formattableOutput("compareTable2B"), style = "height:600px; overflow-y: scroll;overflow-x: scroll;"),
                                                               br()),
                                                             fluidRow(
                                                               column(6,
                                                                      plotOutput("capDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("currDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("MeasMFDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("isenEffyPlot2B"),
                                                                      br(),br(),br(),br(),br(),br()
                                                               ),
                                                               column(6,
                                                                      plotOutput("powDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("EERDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("CalcMFDifPlot2B"),
                                                                      br(),br(),br(),br(),br(),br(),
                                                                      plotOutput("volEffyPlot2B")
                                                               ))) #end conditional panel
                      )), #end sidebar panel
                      
                      mainPanel( #outputs 
                      )
             )
  )
)

####################################################

server <- function(input, output, session) {
  
  #########################################################
  
  RV <- reactiveValues()
  
  #choice 1, coefficients
  observeEvent(input$paste, {
    
    #coefficients table output
    pasted1A <- readClipboard()
    pasted1A <- as.numeric(unlist((strsplit(pasted1A, split = "\t"))))
    originalPasted <- pasted1A
    #reset button
    observeEvent(input$reset, {
      RV$pasted1A <- originalPasted
      
      RV$pastedCoeffs1 <- data.frame(CAP = pasted1A[1:10],
                                     POW = pasted1A[11:20],
                                     CURR = pasted1A[21:30],
                                     MF = pasted1A[31:40])
      
      output$table1A = renderDataTable({
        datatable(RV$pastedCoeffs1, rownames=F, selection='none',filter='none', 
                  callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                  options=list(dom='t', ordering=F))
      })
    })
    
    RV$pastedCoeffs1 <- data.frame(CAP = pasted1A[1:10],
                                   POW = pasted1A[11:20],
                                   CURR = pasted1A[21:30],
                                   MF = pasted1A[31:40])
    
    output$table1A = renderDataTable({
      datatable(RV$pastedCoeffs1, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })
    
    #Replace** additional test points
    observe({
      req(input$numPtsAdded)
      if(input$numPtsAdded > 0){
        Evap <- rep("",input$numPtsAdded)
        Cond <- rep("",input$numPtsAdded)
        Cap <- rep("",input$numPtsAdded)
        Pow <- rep("",input$numPtsAdded)
        Curr <- rep("",input$numPtsAdded)
        MF <- rep("",input$numPtsAdded)
        RV$data.in <- data.table::data.table(Evap, Cond, Cap, Pow, Curr, MF)
      }
    })
    
    data <- reactive({
      hot <- input$addPts
      if (!is.null(hot)) as.data.frame(hot_to_r(hot))
    })
    
    output$addPts <- renderRHandsontable({
      rhandsontable(RV$data.in)
    })
    
    #recalculate coefficients based on additional test points
    observeEvent(input$save, { 
      hotDF <- data()
      
      #create plot values
      evapPlot <- seq(input$minEvap,input$maxEvap,input$incEvap)
      condPlot <- seq(input$minCond,input$maxCond,input$incCond)
      numTotal1A <- length(evapPlot)*length(condPlot)
      
      addingDF <- data.frame(evap = numeric(numTotal1A),
                             cond = numeric(numTotal1A)) %>%
        mutate(evap = rep_len(evapPlot, numTotal1A),
               cond = rep_len(condPlot, numTotal1A))
      
      addingDF$Capacity <- mapply(perfCoeff,addingDF$evap,addingDF$cond,RV$pastedCoeffs1$CAP[1],
                                  RV$pastedCoeffs1$CAP[2],RV$pastedCoeffs1$CAP[3],RV$pastedCoeffs1$CAP[4],
                                  RV$pastedCoeffs1$CAP[5],RV$pastedCoeffs1$CAP[6],RV$pastedCoeffs1$CAP[7],
                                  RV$pastedCoeffs1$CAP[8],RV$pastedCoeffs1$CAP[9],RV$pastedCoeffs1$CAP[10])
      addingDF$Power <- mapply(perfCoeff,addingDF$evap,addingDF$cond,RV$pastedCoeffs1$POW[1],
                               RV$pastedCoeffs1$POW[2],RV$pastedCoeffs1$POW[3],RV$pastedCoeffs1$POW[4],
                               RV$pastedCoeffs1$POW[5],RV$pastedCoeffs1$POW[6],RV$pastedCoeffs1$POW[7],
                               RV$pastedCoeffs1$POW[8],RV$pastedCoeffs1$POW[9],RV$pastedCoeffs1$POW[10])
      addingDF$Current <- mapply(perfCoeff,addingDF$evap,addingDF$cond,RV$pastedCoeffs1$CURR[1],
                                 RV$pastedCoeffs1$CURR[2],RV$pastedCoeffs1$CURR[3],RV$pastedCoeffs1$CURR[4],
                                 RV$pastedCoeffs1$CURR[5],RV$pastedCoeffs1$CURR[6],RV$pastedCoeffs1$CURR[7],
                                 RV$pastedCoeffs1$CURR[8],RV$pastedCoeffs1$CURR[9],RV$pastedCoeffs1$CURR[10])
      addingDF$MeasMF <- mapply(perfCoeff,addingDF$evap,addingDF$cond,RV$pastedCoeffs1$MF[1],
                                RV$pastedCoeffs1$MF[2],RV$pastedCoeffs1$MF[3],RV$pastedCoeffs1$MF[4],
                                RV$pastedCoeffs1$MF[5],RV$pastedCoeffs1$MF[6],RV$pastedCoeffs1$MF[7],
                                RV$pastedCoeffs1$MF[8],RV$pastedCoeffs1$MF[9],RV$pastedCoeffs1$MF[10])
      
      ###below is using data from handsontable inputs
      evaps <- as.numeric(hotDF[,1])
      conds <- as.numeric(hotDF[,2])
      #get indices of extisting conditions that need replaced
      indices <- list()
      for(i in 1:length(evaps)){
        indices[[i]] <- which(addingDF[,1] == evaps[i] & addingDF[,2] == conds[i])
      }
      indices <- unlist(indices) #flatten list
      print(indices)
      print(addingDF)
      addingDF %>% filter(!row_number() %in% indices)
      # addingDF <- addingDF[-indices,]
      print(addingDF)
      
      #add in extra test points at bottom of df
      el1 <- as.numeric(append(frame$evap, hotDF$Evap))
      el2 <- as.numeric(append(frame$cond, hotDF$Cond))
      el3 <- as.numeric(append(frame$Capacity, hotDF$Cap))
      el4 <- as.numeric(append(frame$Power, hotDF$Pow))
      el5 <- as.numeric(append(frame$Current, hotDF$Curr))
      el6 <- as.numeric(append(frame$MF, hotDF$MF))
      #make coefficients with lm
      RV$pastedCoeffs1$CAP <- makeCoefficientsWithLM(el1, el2, el3)
      RV$pastedCoeffs1$POW <- makeCoefficientsWithLM(el1, el2, el4)
      RV$pastedCoeffs1$CURR <- makeCoefficientsWithLM(el1, el2, el5)
      RV$pastedCoeffs1$MF <- makeCoefficientsWithLM(el1, el2, el6)
      
      RV$pastedCoeffs1 <- round(RV$pastedCoeffs1,10)
    })
    
    #calculate curve % shift
    output$shiftOutput = renderUI({
      conditionString <- input$condition
      condition = input$condition %>%
        str_split_fixed(pattern = '/',n = 2) %>%
        as.numeric()
      evap = condition[1]
      cond = condition[2]
      
      shift <- round(curveShift(evap, cond, RV$pastedCoeffs1$CAP,  RV$pastedCoeffs1$POW, input$desired),4)
      
      str1 <- paste("Capacity at", conditionString, ":", shift[3])
      str2 <- paste("Power at", conditionString, ":", shift[4])
      str3 <- paste("Shift capacity", shift[1], "OR shift power", shift[2], "to achieve desired EER") 
      HTML(paste(str1,str2,'<br/>','<br/>',str3))
    })
  })#end observe event
  
  
  #adjust coeffs using inputs
  observeEvent(input$adjust, {
    
    #create plot values
    evapPlot <- seq(input$minEvap,input$maxEvap,input$incEvap)
    condPlot <- seq(input$minCond,input$maxCond,input$incCond)
    numTotal1A <- length(evapPlot)*length(condPlot)
    
    RV$pastedCoeffs1$CAP <- RV$pastedCoeffs1$CAP*input$adjCap
    RV$pastedCoeffs1$POW <- RV$pastedCoeffs1$POW*input$adjPow
    RV$pastedCoeffs1$CURR <- RV$pastedCoeffs1$CURR*input$adjPow
    RV$pastedCoeffs1$MF <- RV$pastedCoeffs1$MF*input$adjCap
    
    
    capPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                cond = numeric(numTotal1A),
                                Capacity = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    capPlotVals1A$Capacity <- mapply(perfCoeff,capPlotVals1A$evap,capPlotVals1A$cond,RV$pastedCoeffs1$CAP[1],
                                     RV$pastedCoeffs1$CAP[2],RV$pastedCoeffs1$CAP[3],RV$pastedCoeffs1$CAP[4],
                                     RV$pastedCoeffs1$CAP[5],RV$pastedCoeffs1$CAP[6],RV$pastedCoeffs1$CAP[7],
                                     RV$pastedCoeffs1$CAP[8],RV$pastedCoeffs1$CAP[9],RV$pastedCoeffs1$CAP[10])
    
    output$capCurve1A = renderPlot({
      ggplot(capPlotVals1A, aes(x=evap, y=Capacity, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Capacity (Btu / hr)",
             title='Capacity vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) 
    }, height = 500, width = 700)#end output
    
    powPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                cond = numeric(numTotal1A),
                                Power = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    powPlotVals1A$Power <- mapply(perfCoeff,powPlotVals1A$evap,powPlotVals1A$cond,RV$pastedCoeffs1$POW[1],
                                  RV$pastedCoeffs1$POW[2],RV$pastedCoeffs1$POW[3],RV$pastedCoeffs1$POW[4],
                                  RV$pastedCoeffs1$POW[5],RV$pastedCoeffs1$POW[6],RV$pastedCoeffs1$POW[7],
                                  RV$pastedCoeffs1$POW[8],RV$pastedCoeffs1$POW[9],RV$pastedCoeffs1$POW[10])
    
    output$powCurve1A = renderPlot({
      ggplot(powPlotVals1A, aes(x=evap, y=Power, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Power (Watts)", 
             title='Power vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    currPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                 cond = numeric(numTotal1A),
                                 Current = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    currPlotVals1A$Current <- mapply(perfCoeff,currPlotVals1A$evap,currPlotVals1A$cond,RV$pastedCoeffs1$CURR[1],
                                     RV$pastedCoeffs1$CURR[2],RV$pastedCoeffs1$CURR[3],RV$pastedCoeffs1$CURR[4],
                                     RV$pastedCoeffs1$CURR[5],RV$pastedCoeffs1$CURR[6],RV$pastedCoeffs1$CURR[7],
                                     RV$pastedCoeffs1$CURR[8],RV$pastedCoeffs1$CURR[9],RV$pastedCoeffs1$CURR[10])
    
    output$currCurve1A = renderPlot({
      ggplot(currPlotVals1A, aes(x=evap, y=Current, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Current (Amps)", 
             title='Current vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    EERPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                cond = numeric(numTotal1A),
                                EER = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    EERPlotVals1A$EER <- capPlotVals1A$Capacity / powPlotVals1A$Power
    
    output$EERCurve1A = renderPlot({
      ggplot(EERPlotVals1A, aes(x=evap, y=EER, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "EER ( Btu/Watt-hr )", 
             title='EER vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    measMFPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                   cond = numeric(numTotal1A),
                                   MeasMassFlow = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    measMFPlotVals1A$MeasMassFlow <- mapply(perfCoeff,measMFPlotVals1A$evap,measMFPlotVals1A$cond,RV$pastedCoeffs1$MF[1],
                                            RV$pastedCoeffs1$MF[2],RV$pastedCoeffs1$MF[3],RV$pastedCoeffs1$MF[4],
                                            RV$pastedCoeffs1$MF[5],RV$pastedCoeffs1$MF[6],RV$pastedCoeffs1$MF[7],
                                            RV$pastedCoeffs1$MF[8],RV$pastedCoeffs1$MF[9],RV$pastedCoeffs1$MF[10])
    
    output$measMFCurve1A = renderPlot({
      ggplot(measMFPlotVals1A, aes(x=evap, y=MeasMassFlow, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Mass Flow ( lbm/hr )", 
             title='Measured Mass Flow vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    calcMFPlotVals1A <- data.frame(evap = numeric(numTotal1A),
                                   cond = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    calcMFPlotVals1A$PSuc <- mapply(refprope, 'P', 'T', calcMFPlotVals1A$evap, 'Q', 1, input$refrigerant)
    calcMFPlotVals1A$H1 <- mapply(refprope, 'H', 'T', calcMFPlotVals1A$evap+input$sh, 'P', calcMFPlotVals1A$PSuc, input$refrigerant)
    calcMFPlotVals1A$PDis <- mapply(refprope, 'P', 'T', calcMFPlotVals1A$cond, 'Q', 1, input$refrigerant)
    calcMFPlotVals1A$T3_Q0 <- mapply(refprope, 'T', 'P', calcMFPlotVals1A$PDis, 'Q', 1, input$refrigerant)
    calcMFPlotVals1A$H2 <- mapply(refprope, 'H', 'T', calcMFPlotVals1A$cond-input$sc, 'P', calcMFPlotVals1A$PDis, input$refrigerant)
    
    calcMFPlotVals1A$CalcMassFlow <- capPlotVals1A$Capacity / (calcMFPlotVals1A$H1 - calcMFPlotVals1A$H2)
    
    output$calcMFCurve1A = renderPlot({
      ggplot(calcMFPlotVals1A, aes(x=evap, y=CalcMassFlow, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Mass Flow ( lbm/hr )", 
             title='Calculated from Capacity Mass Flow vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    massFlowDiff1A <- data.frame(evap = numeric(numTotal1A),
                                 cond = numeric(numTotal1A),
                                 MeasMassFlow = measMFPlotVals1A$MeasMassFlow,
                                 CalcMassFlow = calcMFPlotVals1A$CalcMassFlow,
                                 PercDiff = abs((calcMFPlotVals1A$CalcMassFlow - measMFPlotVals1A$MeasMassFlow) / calcMFPlotVals1A$CalcMassFlow * 100)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    massFlowDiff1A$PercDiff <- round(massFlowDiff1A$PercDiff, 2)
    
    output$percDiffCurve1A = renderPlot({
      ggplot(massFlowDiff1A, aes(x=evap, y=PercDiff /100, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE)+
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Percent Difference", 
             title='Percent Difference Between Actual vs Measured Mass Flow', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        coord_cartesian(ylim=c(-.02,.05))
    }, height = 500, width = 700) #end output
    
    isenEffy1A <- data.frame(evap = numeric(numTotal1A),
                             cond = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    isenEffy1A$PSuc = mapply(refprope,'P','T',isenEffy1A$evap,'Q',1,input$refrigerant)
    isenEffy1A$PDis = mapply(refprope,'P','T',isenEffy1A$cond,'Q',1,input$refrigerant)
    isenEffy1A$T3_Q0 = mapply(refprope,'T','P',isenEffy1A$PDis,'Q',0,input$refrigerant)
    
    isenEffy1A$SubcoolLiqH = mapply(refprope,'H','T',(isenEffy1A$T3_Q0 - input$sc),'P',isenEffy1A$PDis,input$refrigerant)
    isenEffy1A$ReturnGasH = mapply(refprope,'H','T',(isenEffy1A$evap + input$sh),'P',isenEffy1A$PSuc,input$refrigerant)
    isenEffy1A$Inlet_Entropy = mapply(refprope,'S','T',(isenEffy1A$evap + input$sh), spec2 = 'P',isenEffy1A$PSuc,input$refrigerant)
    isenEffy1A$DischargeGasIdealH = mapply(refprope,'H','P',isenEffy1A$PDis,'S',isenEffy1A$Inlet_Entropy,input$refrigerant)
    
    isenEffy1A$EER <- capPlotVals1A$Capacity / powPlotVals1A$Power
    isenEffy1A$TEER <- (isenEffy1A$ReturnGasH - isenEffy1A$SubcoolLiqH)/(isenEffy1A$DischargeGasIdealH - isenEffy1A$ReturnGasH) * 3.412
    isenEffy1A$isenEffy <- isenEffy1A$EER / isenEffy1A$TEER
    
    output$isenEffyCurve1A = renderPlot({
      ggplot(isenEffy1A, aes(x=evap, y=isenEffy, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Isentropic Efficiency (%)",
             title='Isentropic Efficiency (%)', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    volEffy1A <- data.frame(evap = numeric(numTotal1A),
                            cond = numeric(numTotal1A)) %>%
      mutate(evap = rep_len(evapPlot, numTotal1A),
             cond = rep_len(condPlot, numTotal1A))
    
    volEffy1A$PSuc <- mapply(refprope,'P','T',volEffy1A$evap,'Q',1,input$refrigerant)
    volEffy1A$Density <- mapply(refprope, 'D', 'T', volEffy1A$evap+input$sh, 'P', volEffy1A$PSuc,input$refrigerant)  
    volEffy1A$Density <- volEffy1A$Density * 1728 #conversion to lbm/ft3
    volEffy1A$specVol <- 1/volEffy1A$Density
    volEffy1A$mf <- measMFPlotVals1A$MeasMassFlow
    volEffy1A$volEffy <- measMFPlotVals1A$MeasMassFlow / (volEffy1A$Density * input$displacement * 3500 * 0.034722)
    
    output$volEffyCurve1A = renderPlot({
      ggplot(volEffy1A, aes(x=evap, y=volEffy, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Volumetric Efficiency (%)",
             title='Volumetric Efficiency (%)', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    
  })#end observe event
  
  
  #########################################################
  #choice 1, tested values
  RVChoice1B <- reactiveValues()
  RVChoice1B$newCoef1B <- data.frame(Capacity = numeric(10),
                                     Power = numeric(10),
                                     Current = numeric(10),
                                     CalcMF = numeric(10),
                                     MeasMF = numeric(10))
  
  observeEvent(input$upload1B, {
    req(input$upload1B)
    RVChoice1B$uploadDF1B <- read_csv(input$upload1B$datapath)
    numRows <- nrow(RVChoice1B$uploadDF1B)
    RVChoice1B$uploadDF1B <- RVChoice1B$uploadDF1B[,c("EvapTemp","CondTemp", "MassFlow", "Capacity", "EER",
                                                      "Power", "Current", "MeasMassFlow", "MassFlow")]
    
    RVChoice1B$newCoef1B$Capacity <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Capacity)
    RVChoice1B$newCoef1B$Power <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Power)
    RVChoice1B$newCoef1B$Current <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Current)
    RVChoice1B$newCoef1B$CalcMF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MassFlow)
    RVChoice1B$newCoef1B$MeasMF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MeasMassFlow)
    
    CAP <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Capacity)
    POW <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Power)
    CURR <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Current)
    MeasMF <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MeasMassFlow)
    CalcMF <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MassFlow)
    
    # #output cook's distance table
    # output$cooksDist = renderFormattable({
    #   formattable(cooksDistance, list(
    #     CAP = color_tile("white", "steelblue3"),
    #     POW = color_tile("white", "steelblue3"),
    #     CURR = color_tile("white", "steelblue3"),
    #     MeasMF = color_tile("white", "steelblue3")
    #   ), caption = "Cook's Distance") 
    # }) #end output
    
    #creates button inside table for removing rows based on results of cook's dist.
    Delete = shinyInput(actionButton, numRows, 'button_', label = "Remove", onclick = 'Shiny.onInputChange(\"select_button\", this.id)')
    
    RVChoice1B$uploadDF1B = cbind.data.frame(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, Delete, RVChoice1B$uploadDF1B$Capacity, CAP,
                                             RVChoice1B$uploadDF1B$Power, POW, RVChoice1B$uploadDF1B$Current, CURR, RVChoice1B$uploadDF1B$MeasMassFlow,
                                             MeasMF, RVChoice1B$uploadDF1B$MassFlow, CalcMF, stringsAsFactors = F)
    #rouding
    RVChoice1B$uploadDF1B[,c(4,6,8,10,12)] <- round(RVChoice1B$uploadDF1B[,c(4,6,8,10,12)],2)
    RVChoice1B$uploadDF1B[,c(5,7,9,11,13)] <- round(RVChoice1B$uploadDF1B[,c(5,7,9,11,13)],5)
    RVChoice1B$uploadDF1B[,c(1,2)] <- round(RVChoice1B$uploadDF1B[,c(1,2)],1)
    colnames(RVChoice1B$uploadDF1B) <- c("Evap", "Cond", "Remove", "CAP", "CD_CAP", "POW", "CD_POW", "CURR", "CD_CURR", "MeasMF", "CD_MMF", "CalcMF", "CD_CMF")
    
    iso <- isolate(RVChoice1B$uploadDF1B)
    #which rows to remove
    cutoffValue <- round(4/(length(iso[,1])),4)
    delete <- which(iso[,5] > cutoffValue)
    delete1 <- which(iso[,7] > cutoffValue)
    delete2 <- which(iso[,9] > cutoffValue)
    delete3 <- which(iso[,11] > cutoffValue)
    delete4 <- which(iso[,13] > cutoffValue)
    
    listoflists <- c(delete, delete1, delete2, delete3, delete4)
    listoflists <- listoflists[!is.na(listoflists)] #get rid of empty lists
    flatList <- toString(unique(unlist(listoflists))) #flatten list, remove duplciates, and change to string for printing
    
    output$omitThese = renderText({
      str1 <- paste("Cook's distance is used to quantitatively estimate the influence (and likelihood that it's an outlier) of a data point in a regression model.")
      str1.5 <- paste("The higher the Cook's distance value is, the more influence that test point likely has.")
      str2 <- paste("The standard cutoff for outlier removal is", cutoffValue, "(4/n). Rows", flatList, "are recommended for removal.")
      HTML(paste(str1, str1.5, str2))
      })
    
    ####action button within DT is preventing me from coloring cell backgrounds with formatStyle()
    output$uploadDeleteRows = DT::renderDataTable(
      RVChoice1B$uploadDF1B,
      options = list(ordering=F),
      server = FALSE, escape = FALSE, selection = 'none'
    )
    
    observeEvent(input$select_button, {
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      RVChoice1B$uploadDF1B <- RVChoice1B$uploadDF1B[rownames(RVChoice1B$uploadDF1B) != selectedRow, ]
    })
    
  }) #end observe event, upload
  
  #option to shift curves up or down by %
  observeEvent(input$calculate, {
    uploadDF1B <- isolate(RVChoice1B$uploadDF1B[,c(1,2,4,6,8,10,12)])
    RVChoice1B$newCoef1B$Capacity <- makeCoefficientsWithLM(uploadDF1B$Evap, uploadDF1B$Cond, uploadDF1B$CAP)
    RVChoice1B$newCoef1B$Power <- makeCoefficientsWithLM(uploadDF1B$Evap, uploadDF1B$Cond, uploadDF1B$POW)
    RVChoice1B$newCoef1B$Current <- makeCoefficientsWithLM(uploadDF1B$Evap, uploadDF1B$Cond, uploadDF1B$CURR)
    RVChoice1B$newCoef1B$MeasMF <- makeCoefficientsWithLM(uploadDF1B$Evap, uploadDF1B$Cond, uploadDF1B$MeasMF)
    RVChoice1B$newCoef1B$CalcMF <- makeCoefficientsWithLM(uploadDF1B$Evap, uploadDF1B$Cond, uploadDF1B$CalcMF)
    
    output$createCoeffs1B <- renderTable({
      RVChoice1B$newCoef1B}, digits = 15
    ) #end output
    
    #calculate curve % shift
    output$shiftOutput2 = renderText({
      conditionString <- input$condition
      condition = input$condition2 %>%
        str_split_fixed(pattern = '/',n = 2) %>%
        as.numeric()
      evap = condition[1]
      cond = condition[2]
      
      shift <- round(curveShift(evap, cond, RVChoice1B$newCoef1B$Capacity,  RVChoice1B$newCoef1B$Power, input$desired2),5)
      
      str1 <- paste("Capacity at", conditionString, ":", shift[3])
      str2 <- paste("Power at", conditionString, ":", shift[4])
      str3 <- paste("Shift capacity", shift[1], "OR shift power", shift[2], "to achieve desired EER") 
      HTML(paste(str1,str2,'<br/>','<br/>',str3))
    })
    
  })
  
  #option to shift curves up or down by %
  observeEvent(input$adjust2, {
    coefAdjust <- isolate(RVChoice1B$newCoef1B)
    coefAdjust <- coefAdjust %>%
      mutate(Capacity = Capacity*input$adjCap2,
             Power = Power*input$adjPow2,
             Current = Current*input$adjPow2,
             CalcMF = CalcMF*input$adjCap2,
             MeasMF = MeasMF*input$adjCap2)
    RVChoice1B$newCoef1B <- coefAdjust
    
    #create plot values
    evapPlot <- seq(as.numeric(input$minEvap),as.numeric(input$maxEvap),as.numeric(input$incEvap))
    condPlot <- seq(as.numeric(input$minCond),as.numeric(input$maxCond),as.numeric(input$incCond))
    numTotal <- as.numeric(length(evapPlot)*length(condPlot))
    
    capPlotVals1B <- data.frame(evap = numeric(numTotal),
                                cond = numeric(numTotal),
                                Capacity = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    capPlotVals1B$Capacity <- mapply(perfCoeff,capPlotVals1B$evap,capPlotVals1B$cond,RVChoice1B$newCoef1B$Capacity[1],
                                     RVChoice1B$newCoef1B$Capacity[2],RVChoice1B$newCoef1B$Capacity[3],RVChoice1B$newCoef1B$Capacity[4],
                                     RVChoice1B$newCoef1B$Capacity[5],RVChoice1B$newCoef1B$Capacity[6],RVChoice1B$newCoef1B$Capacity[7],
                                     RVChoice1B$newCoef1B$Capacity[8],RVChoice1B$newCoef1B$Capacity[9],RVChoice1B$newCoef1B$Capacity[10])
    
    output$capCurve1B = renderPlot({
      ggplot(capPlotVals1B, aes(x=evap, y=Capacity, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Capacity (Btu / hr)",
             title='Capacity vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    powPlotVals1B <- data.frame(evap = numeric(numTotal),
                                cond = numeric(numTotal),
                                Power = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    powPlotVals1B$Power <- mapply(perfCoeff,powPlotVals1B$evap,powPlotVals1B$cond,RVChoice1B$newCoef1B$Power[1],
                                  RVChoice1B$newCoef1B$Power[2],RVChoice1B$newCoef1B$Power[3],RVChoice1B$newCoef1B$Power[4],
                                  RVChoice1B$newCoef1B$Power[5],RVChoice1B$newCoef1B$Power[6],RVChoice1B$newCoef1B$Power[7],
                                  RVChoice1B$newCoef1B$Power[8],RVChoice1B$newCoef1B$Power[9],RVChoice1B$newCoef1B$Power[10])
    
    output$powCurve1B = renderPlot({
      ggplot(powPlotVals1B, aes(x=evap, y=Power, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Power (Watts)", 
             title='Power vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    currPlotVals1B <- data.frame(evap = numeric(numTotal),
                                 cond = numeric(numTotal),
                                 Current = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    currPlotVals1B$Current <- mapply(perfCoeff,currPlotVals1B$evap,currPlotVals1B$cond,RVChoice1B$newCoef1B$Current[1],
                                     RVChoice1B$newCoef1B$Current[2],RVChoice1B$newCoef1B$Current[3],RVChoice1B$newCoef1B$Current[4],
                                     RVChoice1B$newCoef1B$Current[5],RVChoice1B$newCoef1B$Current[6],RVChoice1B$newCoef1B$Current[7],
                                     RVChoice1B$newCoef1B$Current[8],RVChoice1B$newCoef1B$Current[9],RVChoice1B$newCoef1B$Current[10])
    
    output$currCurve1B = renderPlot({
      ggplot(currPlotVals1B, aes(x=evap, y=Current, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Current (Amps)", 
             title='Current vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    EERPlotVals1B <- data.frame(evap = numeric(numTotal),
                                cond = numeric(numTotal),
                                EER = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    EERPlotVals1B$EER <- capPlotVals1B$Capacity / powPlotVals1B$Power
    
    output$EERCurve1B = renderPlot({
      ggplot(EERPlotVals1B, aes(x=evap, y=EER, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "EER ( Btu/Watt-hr )", 
             title='EER vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700)#end output
    
    measMFPlotVals1B <- data.frame(evap = numeric(numTotal),
                                   cond = numeric(numTotal),
                                   MeasMF = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    measMFPlotVals1B$MeasMF <- mapply(perfCoeff,measMFPlotVals1B$evap,measMFPlotVals1B$cond,RVChoice1B$newCoef1B$MeasMF[1],
                                      RVChoice1B$newCoef1B$MeasMF[2],RVChoice1B$newCoef1B$MeasMF[3],RVChoice1B$newCoef1B$MeasMF[4],
                                      RVChoice1B$newCoef1B$MeasMF[5],RVChoice1B$newCoef1B$MeasMF[6],RVChoice1B$newCoef1B$MeasMF[7],
                                      RVChoice1B$newCoef1B$MeasMF[8],RVChoice1B$newCoef1B$MeasMF[9],RVChoice1B$newCoef1B$MeasMF[10])
    
    output$measMFCurve1B = renderPlot({
      ggplot(measMFPlotVals1B, aes(x=evap, y=MeasMF, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Mass Flow ( lbm/hr )", 
             title='Measured Mass Flow vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    calcMFPlotVals1B <- data.frame(evap = numeric(numTotal),
                                   cond = numeric(numTotal),
                                   CalcMF = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    calcMFPlotVals1B$CalcMF <- mapply(perfCoeff,calcMFPlotVals1B$evap,calcMFPlotVals1B$cond,RVChoice1B$newCoef1B$CalcMF[1],
                                      RVChoice1B$newCoef1B$CalcMF[2],RVChoice1B$newCoef1B$CalcMF[3],RVChoice1B$newCoef1B$CalcMF[4],
                                      RVChoice1B$newCoef1B$CalcMF[5],RVChoice1B$newCoef1B$CalcMF[6],RVChoice1B$newCoef1B$CalcMF[7],
                                      RVChoice1B$newCoef1B$CalcMF[8],RVChoice1B$newCoef1B$CalcMF[9],RVChoice1B$newCoef1B$CalcMF[10])
    
    output$calcMFCurve1B = renderPlot({
      ggplot(calcMFPlotVals1B, aes(x=evap, y=CalcMF, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Mass Flow ( lbm/hr )", 
             title='Calculated from Capacity Mass Flow vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    massFlowDiff1B <- data.frame(evap = numeric(numTotal),
                                 cond = numeric(numTotal),
                                 MeasMF = measMFPlotVals1B$MeasMF,
                                 CalcMF = calcMFPlotVals1B$CalcMF,
                                 PercDiff = abs((calcMFPlotVals1B$CalcMF - measMFPlotVals1B$MeasMF) / calcMFPlotVals1B$CalcMF * 100)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    massFlowDiff1B$PercDiff <- round(massFlowDiff1B$PercDiff, 2)
    
    output$percDiffCurve1B = renderPlot({
      ggplot(massFlowDiff1B, aes(x=evap, y=PercDiff /100, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE)+
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Percent Difference", 
             title='Percent Difference Between Actual vs Measured Mass Flow', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        coord_cartesian(ylim=c(-.02,.05))
    }, height = 500, width = 700) #end output
    
    isenEffy1B <- data.frame(evap = numeric(numTotal),
                             cond = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    isenEffy1B$PSuc = mapply(refprope,'P','T',isenEffy1B$evap,'Q',1,input$refrigerant)
    isenEffy1B$PDis = mapply(refprope,'P','T',isenEffy1B$cond,'Q',1,input$refrigerant)
    isenEffy1B$T3_Q0 = mapply(refprope,'T','P',isenEffy1B$PDis,'Q',0,input$refrigerant)
    
    isenEffy1B$SubcoolLiqH = mapply(refprope,'H','T',(isenEffy1B$T3_Q0 - input$sc),'P',isenEffy1B$PDis,input$refrigerant)
    isenEffy1B$ReturnGasH = mapply(refprope,'H','T',(isenEffy1B$evap + input$sh),'P',isenEffy1B$PSuc,input$refrigerant)
    isenEffy1B$Inlet_Entropy = mapply(refprope,'S','T',(isenEffy1B$evap + input$sh), spec2 = 'P',isenEffy1B$PSuc,input$refrigerant)
    isenEffy1B$DischargeGasIdealH = mapply(refprope,'H','P',isenEffy1B$PDis,'S',isenEffy1B$Inlet_Entropy,input$refrigerant)
    
    isenEffy1B$EER <- capPlotVals1B$Capacity / powPlotVals1B$Power
    isenEffy1B$TEER <- (isenEffy1B$ReturnGasH - isenEffy1B$SubcoolLiqH)/(isenEffy1B$DischargeGasIdealH - isenEffy1B$ReturnGasH) * 3.412
    isenEffy1B$isenEffy <- isenEffy1B$EER / isenEffy1B$TEER
    
    
    output$isenEffyCurve1B = renderPlot({
      ggplot(isenEffy1B, aes(x=evap, y=isenEffy, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Isentropic Efficiency (%)",
             title='Isentropic Efficiency (%)', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
    volEffy1B <- data.frame(evap = numeric(numTotal),
                            cond = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    volEffy1B$PSuc <- mapply(refprope,'P','T',volEffy1B$evap,'Q',1,input$refrigerant)
    volEffy1B$Density <- mapply(refprope, 'D', 'T', volEffy1B$evap+input$sh, 'P', volEffy1B$PSuc,input$refrigerant)
    volEffy1B$Density <- volEffy1B$Density * 1728 #conversion to lbm/ft3
    volEffy1B$specVol <- 1/volEffy1B$Density
    volEffy1B$mf <- measMFPlotVals1B$MeasMF
    volEffy1B$volEffy <- measMFPlotVals1B$MeasMF / (volEffy1B$Density * input$displacement * 3500 * 0.034722)
    
    output$volEffyCurve1B = renderPlot({
      ggplot(volEffy1B, aes(x=evap, y=volEffy, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Volumetric Efficiency (%)",
             title='Volumetric Efficiency (%)', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 500, width = 700) #end output
    
  }) #end observe event, calculate button
  
  
  
  ########################################################################################
  #tab 2
  ########################################################################################
  
  #########################################################
  #choice 2, two sets of coefficients
  RV2 <- reactiveValues()
  
  #first set of coeffs
  observeEvent(input$paste2A.1, {
    #coefficients table output
    pasted2A.1 <- readClipboard()
    pasted2A.1 <- as.numeric(unlist((strsplit(pasted2A.1, split = "\t"))))
    pastedCoeffs2A.1 <- data.frame(CAP = pasted2A.1[1:10],
                                   POW = pasted2A.1[11:20],
                                   CURR = pasted2A.1[21:30],
                                   MF = pasted2A.1[31:40])
    
    RV2$pastedCoeffs2A.1 <- pastedCoeffs2A.1
    
    output$table2A.1 = renderDataTable({
      datatable(RV2$pastedCoeffs2A.1, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end observe event
  })
  
  #second set of coeffs
  observeEvent(input$paste2A.2, {
    #coefficients table output
    pasted2A.2 <- readClipboard()
    pasted2A.2 <- as.numeric(unlist((strsplit(pasted2A.2, split = "\t"))))
    pastedCoeffs2A.2 <- data.frame(CAP = pasted2A.2[1:10],
                                   POW = pasted2A.2[11:20],
                                   CURR = pasted2A.2[21:30],
                                   MF = pasted2A.2[31:40])
    
    RV2$pastedCoeffs2A.2 <- pastedCoeffs2A.2
    
    output$table2A.2 = renderDataTable({
      datatable(RV2$pastedCoeffs2A.2, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end observe event
  })
  
  observeEvent(input$paste2A.2, {
    #create envelope
    if(input$envel == 'other') {
      evapEnv <- input$evapEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$evapEnvTemps,',') +1) %>%
        as.numeric()
      condEnv <- input$condEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$condEnvTemps,',') +1) %>%
        as.numeric()
    }
    if(input$envel == 'fixedspeed') {
      evapEnv <- c(-10,-10,10,40,55,55)
      condEnv <- c(80,100,115,145,145,80)
    }
    if(input$envel == 'twostage'){
      evapEnv <- c(-10,-10,40,55,55)
      condEnv <- c(80,100,145,145,80)
    }
    #the below uses the envelope_builder function of the refprop package
    Boundary = as.data.frame(cbind(evapEnv, condEnv))
    Test_Data = Envelope_Builder(Boundary, 10)
    evapTest <- append(Test_Data$Evap,50,55)
    condTest <- append(Test_Data$Cond,115,80)
    
    envel <- as.data.frame(cbind(evapEnv,condEnv))
    comparisonDF2A <- data.frame(Evap = evapTest,
                                 Cond = condTest)
    
    #####first set of coefficients
    comparisonDF2A$CAP1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$CAP[1],
                                  RV2$pastedCoeffs2A.1$CAP[2],RV2$pastedCoeffs2A.1$CAP[3],RV2$pastedCoeffs2A.1$CAP[4],
                                  RV2$pastedCoeffs2A.1$CAP[5],RV2$pastedCoeffs2A.1$CAP[6],RV2$pastedCoeffs2A.1$CAP[7],
                                  RV2$pastedCoeffs2A.1$CAP[8],RV2$pastedCoeffs2A.1$CAP[9],RV2$pastedCoeffs2A.1$CAP[10])
    comparisonDF2A$POW1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$POW[1],
                                  RV2$pastedCoeffs2A.1$POW[2],RV2$pastedCoeffs2A.1$POW[3],RV2$pastedCoeffs2A.1$POW[4],
                                  RV2$pastedCoeffs2A.1$POW[5],RV2$pastedCoeffs2A.1$POW[6],RV2$pastedCoeffs2A.1$POW[7],
                                  RV2$pastedCoeffs2A.1$POW[8],RV2$pastedCoeffs2A.1$POW[9],RV2$pastedCoeffs2A.1$POW[10])
    comparisonDF2A$EER1 <- comparisonDF2A$CAP1 / comparisonDF2A$POW1
    comparisonDF2A$CURR1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$CURR[1],
                                   RV2$pastedCoeffs2A.1$CURR[2],RV2$pastedCoeffs2A.1$CURR[3],RV2$pastedCoeffs2A.1$CURR[4],
                                   RV2$pastedCoeffs2A.1$CURR[5],RV2$pastedCoeffs2A.1$CURR[6],RV2$pastedCoeffs2A.1$CURR[7],
                                   RV2$pastedCoeffs2A.1$CURR[8],RV2$pastedCoeffs2A.1$CURR[9],RV2$pastedCoeffs2A.1$CURR[10])
    comparisonDF2A$MeasMF1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$MF[1],
                                     RV2$pastedCoeffs2A.1$MF[2],RV2$pastedCoeffs2A.1$MF[3],RV2$pastedCoeffs2A.1$MF[4],
                                     RV2$pastedCoeffs2A.1$MF[5],RV2$pastedCoeffs2A.1$MF[6],RV2$pastedCoeffs2A.1$MF[7],
                                     RV2$pastedCoeffs2A.1$MF[8],RV2$pastedCoeffs2A.1$MF[9],RV2$pastedCoeffs2A.1$MF[10])
    #calcs calc mf
    calcMF1 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    calcMF1$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    calcMF1$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    calcMF1$T3_Q0 = mapply(refprope,'T','P',calcMF1$PDis,'Q',0,input$refrigerant2)
    calcMF1$H1 = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',calcMF1$PSuc,input$refrigerant2)
    calcMF1$H2 = mapply(refprope,'H','T',(calcMF1$T3_Q0 - input$sc2),'P',calcMF1$PDis,input$refrigerant2)
    comparisonDF2A$CalcMF1 <- comparisonDF2A$CAP1 / (calcMF1$H1 - calcMF1$H2)
    #calcs isen effy
    isenTemp1 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    isenTemp1$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    isenTemp1$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    isenTemp1$T3_Q0 = mapply(refprope,'T','P',isenTemp1$PDis,'Q',0,input$refrigerant2)
    isenTemp1$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp1$T3_Q0 - input$sc2),'P',isenTemp1$PDis,input$refrigerant2)
    isenTemp1$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp1$PSuc,input$refrigerant2)
    isenTemp1$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp1$PSuc,input$refrigerant2)
    isenTemp1$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp1$PDis,'S',isenTemp1$Inlet_Entropy,input$refrigerant2)
    isenTemp1$EER <- comparisonDF2A$CAP1 / comparisonDF2A$POW1
    isenTemp1$TEER <- (isenTemp1$ReturnGasH - isenTemp1$SubcoolLiqH)/(isenTemp1$DischargeGasIdealH - isenTemp1$ReturnGasH) * 3.412
    isenTemp1$isenEffy <- isenTemp1$EER / isenTemp1$TEER
    comparisonDF2A$IsenEffy1 <- isenTemp1$isenEffy
    #calcs vol effy
    volTemp1 <- data.frame(Evap = comparisonDF2A$Evap)
    volTemp1$PSuc <- mapply(refprope,'P','T',volTemp1$Evap,'Q',1,input$refrigerant2)
    volTemp1$Density <- mapply(refprope, 'D', 'T', volTemp1$Evap+input$sh2, 'P', volTemp1$PSuc,input$refrigerant2)  
    volTemp1$Density <- volTemp1$Density * 1728 #conversion to lbm/ft3
    volTemp1$mf <- comparisonDF2A$MeasMF1
    volTemp1$volEffy <- volTemp1$mf / (volTemp1$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2A$VolEffy1 <- volTemp1$volEffy
    
    ##### second set of coefficients
    comparisonDF2A$CAP2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$CAP[1],
                                  RV2$pastedCoeffs2A.2$CAP[2],RV2$pastedCoeffs2A.2$CAP[3],RV2$pastedCoeffs2A.2$CAP[4],
                                  RV2$pastedCoeffs2A.2$CAP[5],RV2$pastedCoeffs2A.2$CAP[6],RV2$pastedCoeffs2A.2$CAP[7],
                                  RV2$pastedCoeffs2A.2$CAP[8],RV2$pastedCoeffs2A.2$CAP[9],RV2$pastedCoeffs2A.2$CAP[10])
    comparisonDF2A$POW2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$POW[1],
                                  RV2$pastedCoeffs2A.2$POW[2],RV2$pastedCoeffs2A.2$POW[3],RV2$pastedCoeffs2A.2$POW[4],
                                  RV2$pastedCoeffs2A.2$POW[5],RV2$pastedCoeffs2A.2$POW[6],RV2$pastedCoeffs2A.2$POW[7],
                                  RV2$pastedCoeffs2A.2$POW[8],RV2$pastedCoeffs2A.2$POW[9],RV2$pastedCoeffs2A.2$POW[10])
    comparisonDF2A$EER2 <- comparisonDF2A$CAP2 / comparisonDF2A$POW2
    comparisonDF2A$CURR2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$CURR[1],
                                   RV2$pastedCoeffs2A.2$CURR[2],RV2$pastedCoeffs2A.2$CURR[3],RV2$pastedCoeffs2A.2$CURR[4],
                                   RV2$pastedCoeffs2A.2$CURR[5],RV2$pastedCoeffs2A.2$CURR[6],RV2$pastedCoeffs2A.2$CURR[7],
                                   RV2$pastedCoeffs2A.2$CURR[8],RV2$pastedCoeffs2A.2$CURR[9],RV2$pastedCoeffs2A.2$CURR[10])
    comparisonDF2A$MeasMF2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$MF[1],
                                     RV2$pastedCoeffs2A.2$MF[2],RV2$pastedCoeffs2A.2$MF[3],RV2$pastedCoeffs2A.2$MF[4],
                                     RV2$pastedCoeffs2A.2$MF[5],RV2$pastedCoeffs2A.2$MF[6],RV2$pastedCoeffs2A.2$MF[7],
                                     RV2$pastedCoeffs2A.2$MF[8],RV2$pastedCoeffs2A.2$MF[9],RV2$pastedCoeffs2A.2$MF[10])
    #calcs calc mf
    calcMF2 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    calcMF2$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    calcMF2$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    calcMF2$T3_Q0 = mapply(refprope,'T','P',calcMF2$PDis,'Q',0,input$refrigerant2)
    calcMF2$H1 = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',calcMF2$PSuc,input$refrigerant2)
    calcMF2$H2 = mapply(refprope,'H','T',(calcMF2$T3_Q0 - input$sc2),'P',calcMF2$PDis,input$refrigerant2)
    comparisonDF2A$CalcMF2 <- comparisonDF2A$CAP2 / (calcMF2$H1 - calcMF2$H2)
    #calcs isen effy
    isenTemp2 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    isenTemp2$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    isenTemp2$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    isenTemp2$T3_Q0 = mapply(refprope,'T','P',isenTemp2$PDis,'Q',0,input$refrigerant2)
    isenTemp2$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp2$T3_Q0 - input$sc2),'P',isenTemp2$PDis,input$refrigerant2)
    isenTemp2$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp2$PSuc,input$refrigerant2)
    isenTemp2$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp2$PSuc,input$refrigerant2)
    isenTemp2$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp2$PDis,'S',isenTemp2$Inlet_Entropy,input$refrigerant2)
    isenTemp2$EER <- comparisonDF2A$CAP2 / comparisonDF2A$POW2
    isenTemp2$TEER <- (isenTemp2$ReturnGasH - isenTemp2$SubcoolLiqH)/(isenTemp2$DischargeGasIdealH - isenTemp2$ReturnGasH) * 3.412
    isenTemp2$isenEffy <- isenTemp2$EER / isenTemp2$TEER
    comparisonDF2A$IsenEffy2 <- isenTemp2$isenEffy
    #calcs vol effy
    volTemp2 <- data.frame(Evap = comparisonDF2A$Evap)
    volTemp2$PSuc <- mapply(refprope,'P','T',volTemp2$Evap,'Q',1,input$refrigerant2)
    volTemp2$Density <- mapply(refprope, 'D', 'T', volTemp2$Evap+input$sh2, 'P', volTemp2$PSuc,input$refrigerant2)  
    volTemp2$Density <- volTemp2$Density * 1728 #conversion to lbm/ft3
    volTemp2$mf <- comparisonDF2A$MeasMF2
    volTemp2$volEffy <- volTemp2$mf / (volTemp2$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2A$VolEffy2 <- volTemp2$volEffy
    #calcualate %difference columns
    comparisonDF2A$Error_CAP <- (comparisonDF2A$CAP1 - comparisonDF2A$CAP2) / comparisonDF2A$CAP1 * 100
    comparisonDF2A$Error_POW <- (comparisonDF2A$POW1 - comparisonDF2A$POW2) / comparisonDF2A$POW1 * 100
    comparisonDF2A$Error_EER <- (comparisonDF2A$EER1 - comparisonDF2A$EER2) / comparisonDF2A$EER1 * 100
    comparisonDF2A$Error_CURR <- (comparisonDF2A$CURR1 - comparisonDF2A$CURR2) / comparisonDF2A$CURR1 * 100
    comparisonDF2A$Error_MMF <- (comparisonDF2A$MeasMF1 - comparisonDF2A$MeasMF2) / comparisonDF2A$MeasMF1 * 100
    comparisonDF2A$Error_CMF <- (comparisonDF2A$CalcMF1 - comparisonDF2A$CalcMF2) / comparisonDF2A$CalcMF1 * 100
    comparisonDF2A$Error_IsenEffy <- (comparisonDF2A$IsenEffy1 - comparisonDF2A$IsenEffy2) / comparisonDF2A$IsenEffy1 * 100
    comparisonDF2A$Error_VolEffy <- (comparisonDF2A$VolEffy1 - comparisonDF2A$VolEffy2) / comparisonDF2A$VolEffy1 * 100
    
    comparisonDF2A <- round(comparisonDF2A[,c(1,2,3,11,19,4,12,20,5,13,21,6,14,22,7,15,23,8,16,24,9,17,25,10,18,26)],2)
    
    output$compareTable2A = renderFormattable({
      formattable(comparisonDF2A, list(
        'Evap' = formatter("span", style = x ~ style("font-weight" = "bold")),
        'Cond' = formatter("span", style = x ~ style("font-weight" = "bold")),
        'Error_CAP' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_POW' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x > 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_EER' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_CURR' = formatter("span",
                                 x ~ percent(x / 100),
                                 style = x ~ style(color = ifelse(x > 0, "red", "green")),
                                 x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_MMF' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_CMF' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_IsenEffy' = formatter("span",
                                     x ~ percent(x / 100),
                                     style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                     x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_VolEffy' = formatter("span",
                                    x ~ percent(x / 100),
                                    style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                    x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down")))
        
      ), extensions = c('FixedHeader'),
      caption = "Error columns calculated by equation (Model1_Value - Model2_Value) / Model1_Value * 100", 
      options=list(paging=F))
    }) #end output
    
    #CAP DIF PLOT
    output$capDifPlot2A = renderPlot({
      capDifDF2A <- comparisonDF2A %>% 
        mutate(Capacity = comparisonDF2A$Error_CAP) %>%
        select(Evap,Cond,Capacity)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(capDifDF2A$Capacity[i] <= 0.5 & capDifDF2A$Capacity[i] >= -0.5){
          capDifDF2A$color[i] <- 1
        } 
        if(capDifDF2A$Capacity[i] < -0.5){
          capDifDF2A$color[i] <- 2
        }
        if(capDifDF2A$Capacity[i] > 0.5){
          capDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(capDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=capDifDF2A, aes(label=paste0(round(Capacity,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Capacity", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #POW DIF PLOT
    output$powDifPlot2A = renderPlot({
      powDifDF2A <- comparisonDF2A %>% 
        mutate(Power = comparisonDF2A$Error_POW) %>%
        select(Evap,Cond,Power)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(powDifDF2A$Power[i] <= 0.5 & powDifDF2A$Power[i] >= -0.5){
          powDifDF2A$color[i] <- 1
        } 
        if(powDifDF2A$Power[i] < -0.5){
          powDifDF2A$color[i] <- 2
        }
        if(powDifDF2A$Power[i] > 0.5){
          powDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(powDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=powDifDF2A, aes(label=paste0(round(Power,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("2"="darkgreen", "1"="orange", "0"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Power", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #CURR DIF PLOT
    output$currDifPlot2A = renderPlot({
      currDifDF2A <- comparisonDF2A %>% 
        mutate(Current = comparisonDF2A$Error_CURR) %>%
        select(Evap,Cond,Current)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(currDifDF2A$Current[i] <= 0.5 & currDifDF2A$Current[i] >= -0.5){
          currDifDF2A$color[i] <- 1
        } 
        if(currDifDF2A$Current[i] < -0.5){
          currDifDF2A$color[i] <- 2
        }
        if(currDifDF2A$Current[i] > 0.5){
          currDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(currDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=currDifDF2A, aes(label=paste0(round(Current,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("2"="darkgreen", "1"="orange", "0"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Current", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #EER DIF PLOT
    output$EERDifPlot2A = renderPlot({
      EERDifDF2A <- comparisonDF2A %>% 
        mutate(EER = comparisonDF2A$Error_EER) %>%
        select(Evap,Cond,EER)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(EERDifDF2A$EER[i] <= 0.5 & EERDifDF2A$EER[i] >= -0.5){
          EERDifDF2A$color[i] <- 1
        } 
        if(EERDifDF2A$EER[i] < -0.5){
          EERDifDF2A$color[i] <- 2
        }
        if(EERDifDF2A$EER[i] > 0.5){
          EERDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(EERDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=EERDifDF2A, aes(label=paste0(round(EER,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - EER", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #MEAS MF DIF PLOT
    output$MeasMFDifPlot2A = renderPlot({
      MeasMFDifDF2A <- comparisonDF2A %>% 
        mutate(MeasMF = comparisonDF2A$Error_MMF) %>%
        select(Evap,Cond,MeasMF)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(MeasMFDifDF2A$MeasMF[i] <= 0.5 & MeasMFDifDF2A$MeasMF[i] >= -0.5){
          MeasMFDifDF2A$color[i] <- 1
        } 
        if(MeasMFDifDF2A$MeasMF[i] < -0.5){
          MeasMFDifDF2A$color[i] <- 2
        }
        if(MeasMFDifDF2A$MeasMF[i] > 0.5){
          MeasMFDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(MeasMFDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=MeasMFDifDF2A, aes(label=paste0(round(MeasMF,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Measured Mass Flow", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #CALC MF DIF PLOT
    output$CalcMFDifPlot2A = renderPlot({
      CalcMFDifDF2A <- comparisonDF2A %>% 
        mutate(CalcMF = comparisonDF2A$Error_CMF) %>%
        select(Evap,Cond,CalcMF)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(CalcMFDifDF2A$CalcMF[i] <= 0.5 & CalcMFDifDF2A$CalcMF[i] >= -0.5){
          CalcMFDifDF2A$color[i] <- 1
        } 
        if(CalcMFDifDF2A$CalcMF[i] < -0.5){
          CalcMFDifDF2A$color[i] <- 2
        }
        if(CalcMFDifDF2A$CalcMF[i] > 0.5){
          CalcMFDifDF2A$color[i] <- 0
        }
      }
      
      ggplot(CalcMFDifDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=CalcMFDifDF2A, aes(label=paste0(round(CalcMF,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Calculated Mass Flow", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #ISEN EFFY DIF PLOT
    output$isenEffyPlot2A = renderPlot({
      isenEffyDF2A <- comparisonDF2A %>%
        mutate(isenEffy = comparisonDF2A$Error_IsenEffy) %>%
        select(Evap,Cond,isenEffy)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(isenEffyDF2A$isenEffy[i] <= 0.5 & isenEffyDF2A$isenEffy[i] >= -0.5){
          isenEffyDF2A$color[i] <- 1
        }
        if(isenEffyDF2A$isenEffy[i] < -0.5){
          isenEffyDF2A$color[i] <- 2
        }
        if(isenEffyDF2A$isenEffy[i] > 0.5){
          isenEffyDF2A$color[i] <- 0
        }
      }
      
      ggplot(isenEffyDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=isenEffyDF2A, aes(label=paste0(round(isenEffy,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Isentropic Efficiency", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #VOL EFFY DIF PLOT
    output$volEffyPlot2A = renderPlot({
      volEffyDF2A <- comparisonDF2A %>%
        mutate(volEffy = comparisonDF2A$Error_VolEffy) %>%
        select(Evap,Cond,volEffy)
      
      for(i in 1:length(comparisonDF2A$Evap)){
        if(volEffyDF2A$volEffy[i] <= 0.5 & volEffyDF2A$volEffy[i] >= -0.5){
          volEffyDF2A$color[i] <- 1
        }
        if(volEffyDF2A$volEffy[i] < -0.5){
          volEffyDF2A$color[i] <- 2
        }
        if(volEffyDF2A$volEffy[i] > 0.5){
          volEffyDF2A$color[i] <- 0
        }
      }
      
      ggplot(volEffyDF2A, aes(x=Evap, y=Cond)) +
        geom_label(data=volEffyDF2A, aes(label=paste0(round(volEffy,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Volumetric Efficiency", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
  })
  
  
  #########################################################
  #choice 2, one set of coefficients, one file of ELT data
  
  RV3 <- reactiveValues()
  
  observeEvent(input$paste2B, {
    #coefficients table output
    pasted2B <- readClipboard()
    pasted2B <- as.numeric(unlist((strsplit(pasted2B, split = "\t"))))
    pastedCoeffs2B <- data.frame(CAP = pasted2B[1:10],
                                 POW = pasted2B[11:20],
                                 CURR = pasted2B[21:30],
                                 MeasMF = pasted2B[31:40])
    
    RV3$pastedCoeffs2B <- pastedCoeffs2B
    
    output$table2B = renderDataTable({
      datatable(RV3$pastedCoeffs2B, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
  })
  
  observeEvent(input$upload2B, {
    req(input$upload2B)
    uploadDF2B <- read_csv(input$upload2B$datapath)
    
    #create coefficients
    newCoef2B <- data.frame(CAP = numeric(10),
                            POW = numeric(10),
                            CURR = numeric(10),
                            MeasMF = numeric(10))
    
    newCoef2B$CAP <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Capacity)
    newCoef2B$POW <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Power)
    newCoef2B$CURR <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Current)
    newCoef2B$MeasMF <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$MeasMassFlow)
    
    output$createCoeffs2B = renderDataTable({
      datatable(newCoef2B, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
    
    #create envelope
    if(input$envel == 'other') {
      evapEnv <- input$evapEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$evapEnvTemps,',') +1) %>%
        as.numeric()
      condEnv <- input$condEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$condEnvTemps,',') +1) %>%
        as.numeric()
    }
    if(input$envel == 'fixedspeed') {
      evapEnv <- c(-10,-10,10,40,55,55)
      condEnv <- c(80,100,115,145,145,80)
    }
    if(input$envel == 'twostage'){
      evapEnv <- c(-10,-10,40,55,55)
      condEnv <- c(80,100,145,145,80)
    }
    #the below uses the envelope_builder function of the refprop package
    Boundary = as.data.frame(cbind(evapEnv, condEnv))
    Test_Data = Envelope_Builder(Boundary, 10)
    evapTest <- append(Test_Data$Evap,50,55)
    condTest <- append(Test_Data$Cond,115,80)
    
    envel <- as.data.frame(cbind(evapEnv,condEnv))
    comparisonDF2B <- data.frame(Evap = evapTest,
                                 Cond = condTest)
    
    #####first set of coefficients
    comparisonDF2B$CAP1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$CAP[1],
                                  RV3$pastedCoeffs2B$CAP[2],RV3$pastedCoeffs2B$CAP[3],RV3$pastedCoeffs2B$CAP[4],
                                  RV3$pastedCoeffs2B$CAP[5],RV3$pastedCoeffs2B$CAP[6],RV3$pastedCoeffs2B$CAP[7],
                                  RV3$pastedCoeffs2B$CAP[8],RV3$pastedCoeffs2B$CAP[9],RV3$pastedCoeffs2B$CAP[10])
    comparisonDF2B$POW1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$POW[1],
                                  RV3$pastedCoeffs2B$POW[2],RV3$pastedCoeffs2B$POW[3],RV3$pastedCoeffs2B$POW[4],
                                  RV3$pastedCoeffs2B$POW[5],RV3$pastedCoeffs2B$POW[6],RV3$pastedCoeffs2B$POW[7],
                                  RV3$pastedCoeffs2B$POW[8],RV3$pastedCoeffs2B$POW[9],RV3$pastedCoeffs2B$POW[10])
    comparisonDF2B$EER1 <- comparisonDF2B$CAP1 / comparisonDF2B$POW1
    comparisonDF2B$CURR1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$CURR[1],
                                   RV3$pastedCoeffs2B$CURR[2],RV3$pastedCoeffs2B$CURR[3],RV3$pastedCoeffs2B$CURR[4],
                                   RV3$pastedCoeffs2B$CURR[5],RV3$pastedCoeffs2B$CURR[6],RV3$pastedCoeffs2B$CURR[7],
                                   RV3$pastedCoeffs2B$CURR[8],RV3$pastedCoeffs2B$CURR[9],RV3$pastedCoeffs2B$CURR[10])
    comparisonDF2B$MeasMF1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$MeasMF[1],
                                     RV3$pastedCoeffs2B$MeasMF[2],RV3$pastedCoeffs2B$MeasMF[3],RV3$pastedCoeffs2B$MeasMF[4],
                                     RV3$pastedCoeffs2B$MeasMF[5],RV3$pastedCoeffs2B$MeasMF[6],RV3$pastedCoeffs2B$MeasMF[7],
                                     RV3$pastedCoeffs2B$MeasMF[8],RV3$pastedCoeffs2B$MeasMF[9],RV3$pastedCoeffs2B$MeasMF[10])
    #calcs calc mf
    calcMF3 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    calcMF3$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    calcMF3$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    calcMF3$T3_Q0 = mapply(refprope,'T','P',calcMF3$PDis,'Q',0,input$refrigerant2)
    calcMF3$H1 = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',calcMF3$PSuc,input$refrigerant2)
    calcMF3$H2 = mapply(refprope,'H','T',(calcMF3$T3_Q0 - input$sc2),'P',calcMF3$PDis,input$refrigerant2)
    comparisonDF2B$CalcMF1 <- comparisonDF2B$CAP1 / (calcMF3$H1 - calcMF3$H2)
    #calcs isen effy
    isenTemp3 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    isenTemp3$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    isenTemp3$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    isenTemp3$T3_Q0 = mapply(refprope,'T','P',isenTemp3$PDis,'Q',0,input$refrigerant2)
    isenTemp3$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp3$T3_Q0 - input$sc2),'P',isenTemp3$PDis,input$refrigerant2)
    isenTemp3$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp3$PSuc,input$refrigerant2)
    isenTemp3$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp3$PSuc,input$refrigerant2)
    isenTemp3$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp3$PDis,'S',isenTemp3$Inlet_Entropy,input$refrigerant2)
    isenTemp3$EER <- comparisonDF2B$CAP1 / comparisonDF2B$POW1
    isenTemp3$TEER <- (isenTemp3$ReturnGasH - isenTemp3$SubcoolLiqH)/(isenTemp3$DischargeGasIdealH - isenTemp3$ReturnGasH) * 3.412
    isenTemp3$isenEffy <- isenTemp3$EER / isenTemp3$TEER
    comparisonDF2B$IsenEffy1 <- isenTemp3$isenEffy
    #calcs vol effy
    volTemp3 <- data.frame(Evap = comparisonDF2B$Evap)
    volTemp3$PSuc <- mapply(refprope,'P','T',volTemp3$Evap,'Q',1,input$refrigerant2)
    volTemp3$Density <- mapply(refprope, 'D', 'T', volTemp3$Evap+input$sh2, 'P', volTemp3$PSuc,input$refrigerant2)  
    volTemp3$Density <- volTemp3$Density * 1728 #conversion to lbm/ft3
    volTemp3$mf <- comparisonDF2B$MeasMF1
    volTemp3$volEffy <- volTemp3$mf / (volTemp3$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2B$VolEffy1 <- volTemp3$volEffy
    
    ##### second set, created from uploaded test data
    comparisonDF2B$CAP2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,newCoef2B$CAP[1],
                                  newCoef2B$CAP[2],newCoef2B$CAP[3],newCoef2B$CAP[4],
                                  newCoef2B$CAP[5],newCoef2B$CAP[6],newCoef2B$CAP[7],
                                  newCoef2B$CAP[8],newCoef2B$CAP[9],newCoef2B$CAP[10])
    comparisonDF2B$POW2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,newCoef2B$POW[1],
                                  newCoef2B$POW[2],newCoef2B$POW[3],newCoef2B$POW[4],
                                  newCoef2B$POW[5],newCoef2B$POW[6],newCoef2B$POW[7],
                                  newCoef2B$POW[8],newCoef2B$POW[9],newCoef2B$POW[10])
    comparisonDF2B$EER2 <- comparisonDF2B$CAP2 / comparisonDF2B$POW2
    comparisonDF2B$CURR2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,newCoef2B$CURR[1],
                                   newCoef2B$CURR[2],newCoef2B$CURR[3],newCoef2B$CURR[4],
                                   newCoef2B$CURR[5],newCoef2B$CURR[6],newCoef2B$CURR[7],
                                   newCoef2B$CURR[8],newCoef2B$CURR[9],newCoef2B$CURR[10])
    comparisonDF2B$MeasMF2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,newCoef2B$MeasMF[1],
                                     newCoef2B$MeasMF[2],newCoef2B$MeasMF[3],newCoef2B$MeasMF[4],
                                     newCoef2B$MeasMF[5],newCoef2B$MeasMF[6],newCoef2B$MeasMF[7],
                                     newCoef2B$MeasMF[8],newCoef2B$MeasMF[9],newCoef2B$MeasMF[10])
    
    #calcs calc mf
    calcMF4 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    calcMF4$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    calcMF4$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    calcMF4$T3_Q0 = mapply(refprope,'T','P',calcMF4$PDis,'Q',0,input$refrigerant2)
    calcMF4$H1 = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',calcMF4$PSuc,input$refrigerant2)
    calcMF4$H2 = mapply(refprope,'H','T',(calcMF4$T3_Q0 - input$sc2),'P',calcMF4$PDis,input$refrigerant2)
    comparisonDF2B$CalcMF2 <- comparisonDF2B$CAP2 / (calcMF4$H1 - calcMF4$H2)
    #calcs isen effy
    isenTemp4 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    isenTemp4$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    isenTemp4$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    isenTemp4$T3_Q0 = mapply(refprope,'T','P',isenTemp4$PDis,'Q',0,input$refrigerant2)
    isenTemp4$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp4$T3_Q0 - input$sc2),'P',isenTemp4$PDis,input$refrigerant2)
    isenTemp4$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp4$PSuc,input$refrigerant2)
    isenTemp4$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp4$PSuc,input$refrigerant2)
    isenTemp4$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp4$PDis,'S',isenTemp4$Inlet_Entropy,input$refrigerant2)
    isenTemp4$EER <- comparisonDF2B$CAP2 / comparisonDF2B$POW2
    isenTemp4$TEER <- (isenTemp4$ReturnGasH - isenTemp4$SubcoolLiqH)/(isenTemp4$DischargeGasIdealH - isenTemp4$ReturnGasH) * 3.412
    isenTemp4$isenEffy <- isenTemp4$EER / isenTemp4$TEER
    comparisonDF2B$IsenEffy2 <- isenTemp4$isenEffy
    #calcs vol effy
    volTemp4 <- data.frame(Evap = comparisonDF2B$Evap)
    volTemp4$PSuc <- mapply(refprope,'P','T',volTemp4$Evap,'Q',1,input$refrigerant2)
    volTemp4$Density <- mapply(refprope, 'D', 'T', volTemp4$Evap+input$sh2, 'P', volTemp4$PSuc,input$refrigerant2)  
    volTemp4$Density <- volTemp4$Density * 1728 #conversion to lbm/ft3
    volTemp4$mf <- comparisonDF2B$MeasMF2
    volTemp4$volEffy <- volTemp4$mf / (volTemp4$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2B$VolEffy2 <- volTemp4$volEffy
    #calcualate %difference columns
    comparisonDF2B$Error_CAP <- (comparisonDF2B$CAP1 - comparisonDF2B$CAP2) / comparisonDF2B$CAP1 * 100
    comparisonDF2B$Error_POW <- (comparisonDF2B$POW1 - comparisonDF2B$POW2) / comparisonDF2B$POW1 * 100
    comparisonDF2B$Error_EER <- (comparisonDF2B$EER1 - comparisonDF2B$EER2) / comparisonDF2B$EER1 * 100
    comparisonDF2B$Error_CURR <- (comparisonDF2B$CURR1 - comparisonDF2B$CURR2) / comparisonDF2B$CURR1 * 100
    comparisonDF2B$Error_MMF <- (comparisonDF2B$MeasMF1 - comparisonDF2B$MeasMF2) / comparisonDF2B$MeasMF1 * 100
    comparisonDF2B$Error_CMF <- (comparisonDF2B$CalcMF1 - comparisonDF2B$CalcMF2) / comparisonDF2B$CalcMF1 * 100
    comparisonDF2B$Error_IsenEffy <- (comparisonDF2B$IsenEffy1 - comparisonDF2B$IsenEffy2) / comparisonDF2B$IsenEffy1 * 100
    comparisonDF2B$Error_VolEffy <- (comparisonDF2B$VolEffy1 - comparisonDF2B$VolEffy2) / comparisonDF2B$VolEffy1 * 100
    
    comparisonDF2B <- round(comparisonDF2B[,c(1,2,3,11,19,4,12,20,5,13,21,6,14,22,7,15,23,8,16,24,9,17,25,10,18,26)],2)
    
    output$compareTable2B = renderFormattable({
      formattable(comparisonDF2B, list(
        'Evap' = formatter("span", style = x ~ style("font-weight" = "bold")),
        'Cond' = formatter("span", style = x ~ style("font-weight" = "bold")),
        'Error_CAP' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_POW' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x > 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_EER' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_CURR' = formatter("span",
                                 x ~ percent(x / 100),
                                 style = x ~ style(color = ifelse(x > 0, "red", "green")),
                                 x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_MMF' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_CMF' = formatter("span",
                                x ~ percent(x / 100),
                                style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_IsenEffy' = formatter("span",
                                     x ~ percent(x / 100),
                                     style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                     x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"))),
        'Error_VolEffy' = formatter("span",
                                    x ~ percent(x / 100),
                                    style = x ~ style(color = ifelse(x < 0, "red", "green")),
                                    x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down")))
        
      ), caption = "Error columns calculated by equation (Model1_Value - Model2_Value) / Model1_Value * 100", options=list(paging=F))
    }) #end output
    
    #CAP DIF PLOT
    output$capDifPlot2B = renderPlot({
      capDifDF2B <- comparisonDF2B %>% 
        mutate(Capacity = comparisonDF2B$Error_CAP) %>%
        select(Evap,Cond,Capacity)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(capDifDF2B$Capacity[i] <= 0.5 & capDifDF2B$Capacity[i] >= -0.5){
          capDifDF2B$color[i] <- 1
        } 
        if(capDifDF2B$Capacity[i] < -0.5){
          capDifDF2B$color[i] <- 2
        }
        if(capDifDF2B$Capacity[i] > 0.5){
          capDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(capDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=capDifDF2B, aes(label=paste0(round(Capacity,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Capacity", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #POW DIF PLOT
    output$powDifPlot2B = renderPlot({
      powDifDF2B <- comparisonDF2B %>% 
        mutate(Power = comparisonDF2B$Error_POW) %>%
        select(Evap,Cond,Power)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(powDifDF2B$Power[i] <= 0.5 & powDifDF2B$Power[i] >= -0.5){
          powDifDF2B$color[i] <- 1
        } 
        if(powDifDF2B$Power[i] < -0.5){
          powDifDF2B$color[i] <- 2
        }
        if(powDifDF2B$Power[i] > 0.5){
          powDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(powDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=powDifDF2B, aes(label=paste0(round(Power,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("2"="darkgreen", "1"="orange", "0"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Power", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #CURR DIF PLOT
    output$currDifPlot2B = renderPlot({
      currDifDF2B <- comparisonDF2B %>% 
        mutate(Current = comparisonDF2B$Error_CURR) %>%
        select(Evap,Cond,Current)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(currDifDF2B$Current[i] <= 0.5 & currDifDF2B$Current[i] >= -0.5){
          currDifDF2B$color[i] <- 1
        } 
        if(currDifDF2B$Current[i] < -0.5){
          currDifDF2B$color[i] <- 2
        }
        if(currDifDF2B$Current[i] > 0.5){
          currDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(currDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=currDifDF2B, aes(label=paste0(round(Current,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("2"="darkgreen", "1"="orange", "0"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Current", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #EER DIF PLOT
    output$EERDifPlot2B = renderPlot({
      EERDifDF2B <- comparisonDF2B %>% 
        mutate(EER = comparisonDF2B$Error_EER) %>%
        select(Evap,Cond,EER)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(EERDifDF2B$EER[i] <= 0.5 & EERDifDF2B$EER[i] >= -0.5){
          EERDifDF2B$color[i] <- 1
        } 
        if(EERDifDF2B$EER[i] < -0.5){
          EERDifDF2B$color[i] <- 2
        }
        if(EERDifDF2B$EER[i] > 0.5){
          EERDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(EERDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=EERDifDF2B, aes(label=paste0(round(EER,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - EER", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #MEAS MF DIF PLOT
    output$MeasMFDifPlot2B = renderPlot({
      MeasMFDifDF2B <- comparisonDF2B %>% 
        mutate(MeasMF = comparisonDF2B$Error_MMF) %>%
        select(Evap,Cond,MeasMF)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(MeasMFDifDF2B$MeasMF[i] <= 0.5 & MeasMFDifDF2B$MeasMF[i] >= -0.5){
          MeasMFDifDF2B$color[i] <- 1
        } 
        if(MeasMFDifDF2B$MeasMF[i] < -0.5){
          MeasMFDifDF2B$color[i] <- 2
        }
        if(MeasMFDifDF2B$MeasMF[i] > 0.5){
          MeasMFDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(MeasMFDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=MeasMFDifDF2B, aes(label=paste0(round(MeasMF,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Measured Mass Flow", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #CALC MF DIF PLOT
    output$CalcMFDifPlot2B = renderPlot({
      CalcMFDifDF2B <- comparisonDF2B %>% 
        mutate(CalcMF = comparisonDF2B$Error_CMF) %>%
        select(Evap,Cond,CalcMF)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(CalcMFDifDF2B$CalcMF[i] <= 0.5 & CalcMFDifDF2B$CalcMF[i] >= -0.5){
          CalcMFDifDF2B$color[i] <- 1
        } 
        if(CalcMFDifDF2B$CalcMF[i] < -0.5){
          CalcMFDifDF2B$color[i] <- 2
        }
        if(CalcMFDifDF2B$CalcMF[i] > 0.5){
          CalcMFDifDF2B$color[i] <- 0
        }
      }
      
      ggplot(CalcMFDifDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=CalcMFDifDF2B, aes(label=paste0(round(CalcMF,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Calculated Mass Flow", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #ISEN EFFY DIF PLOT
    output$isenEffyPlot2B = renderPlot({
      isenEffyDF2B <- comparisonDF2B %>%
        mutate(isenEffy = comparisonDF2B$Error_IsenEffy) %>%
        select(Evap,Cond,isenEffy)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(isenEffyDF2B$isenEffy[i] <= 0.5 & isenEffyDF2B$isenEffy[i] >= -0.5){
          isenEffyDF2B$color[i] <- 1
        }
        if(isenEffyDF2B$isenEffy[i] < -0.5){
          isenEffyDF2B$color[i] <- 2
        }
        if(isenEffyDF2B$isenEffy[i] > 0.5){
          isenEffyDF2B$color[i] <- 0
        }
      }
      
      ggplot(isenEffyDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=isenEffyDF2B, aes(label=paste0(round(isenEffy,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Isentropic Efficiency", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
    #VOL EFFY DIF PLOT
    output$volEffyPlot2B = renderPlot({
      volEffyDF2B <- comparisonDF2B %>%
        mutate(volEffy = comparisonDF2B$Error_VolEffy) %>%
        select(Evap,Cond,volEffy)
      
      for(i in 1:length(comparisonDF2B$Evap)){
        if(volEffyDF2B$volEffy[i] <= 0.5 & volEffyDF2B$volEffy[i] >= -0.5){
          volEffyDF2B$color[i] <- 1
        }
        if(volEffyDF2B$volEffy[i] < -0.5){
          volEffyDF2B$color[i] <- 2
        }
        if(volEffyDF2B$volEffy[i] > 0.5){
          volEffyDF2B$color[i] <- 0
        }
      }
      
      ggplot(volEffyDF2B, aes(x=Evap, y=Cond)) +
        geom_label(data=volEffyDF2B, aes(label=paste0(round(volEffy,2), "%"), fill = factor(color)), colour = "white", size = 4) +
        scale_colour_manual(values=c("0"="darkgreen", "1"="orange", "2"="red"), aesthetics = "fill") +
        geom_polygon(data=envel, aes(x=evapEnv, y=condEnv), color = "black", fill=NA) +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq((min(envel$evapEnv)-10),(max(envel$evapEnv)+10),10)) +
        scale_y_continuous(breaks = seq((min(envel$condEnv)-10),(max(envel$condEnv)+10),10)) +
        labs(title= "Two Model Comparison - Volumetric Efficiency", y="Condenser Temperature (F)", x = "Evaporator Temperature (F)")
    }, height = 500, width = 700) #end output
    
  })
  
  
}#end server
####################################################

# Run the application 
shinyApp(ui = ui, server = server)





