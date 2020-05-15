#
#  App created by Logan Kocka   19 Feb 2020
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(readr) 
library(pracma)
library(rhandsontable)
library(EmersonDataScience)
library(DT)
library(formattable)
library(sp)
# library(rsconnect)

source("functions.R")

ui <- fluidPage(
  navbarPage("Rating App",
             
             #first tab
             tabPanel("About",
                      fluidRow(column(12,
                                      h2("This application allows for easy creation, manipulation, and comparison of coefficients."),
                                      h6("Created by Logan Kocka in Spring 2020 for AC Unitary NPD group."),
                                      h6("Contact Bryan Penkal with questions or maintenance requests."),br())),
                      sidebarLayout(
                        sidebarPanel(width=7,
                                     fluidRow(
                                       column(12,
                                              h2("How to Use"),
                                              
                                              h3("Working with Coefficients"),
                                              
                                              h4("Inputs"),
                                              "Change refrigerant, superheat, and subcooling as needed. Refrigerant input must include '.fld' or '.mix'.",br(),
                                              "Min Evap/Cond, Max Evap/Cond, and Increment inputs create numerical lists defined by those parameters. These are used for defining",
                                              "the conditions simulated during plotting. Displacement is used in volumetric efficiency calculations.",br(),
                                              
                                              h4("Importing Coefficients"),
                                              "1. Open the correct Excel coefficients table spreadsheet in CPIDView.",br(),
                                              "2. Unprotect the sheet.",br(),
                                              "3. Select the complete range of coefficients (C0-M9) in row 2 of the spreadsheet.",br(),
                                              "4. Change the cell type to 'Number'", strong("(even though it already appears to be numerical!)."),br(),
                                              "5. Use the increase decimal button to increase decimal places as desired for your analysis.",br(),
                                              "6. Copy with the mouse or ctrl+c and paste in the app using the 'Paste' button in the user interface.",br(),
                                              "You only have to do steps 1-5 once per spreadsheet. You don't have to do this each time you want to recopy from the same sheet.",br(),
                                              
                                              h4(" Adding Additional Test Points"),
                                              "Input the number of test points that should be included.",br(),
                                              "Input conditions and test data into the editable table that renders below numerical input.",br(),
                                              "Click 'Recalculate'. Coefficients will be recalculated in the same coefficients table.",br(),
                                              
                                              h4("EER Goal Seek"),
                                              "Input a condition and target EER. The adjustment value you would need to use to achieve that EER using the coefficients shown in the table",
                                              "prints below as well as the simulated capacity and power test values.",br(),
                                              "Adjust either capacity OR power to reach the desired EER,", strong("not both."),
                                              
                                              h4("Curve Adjustment"),
                                              "The 'Adjust & Plot' button below the adjustment inputs will initiate recalculation of the provided coefficients shown in the table and plots",
                                              "of Capacity, Power, Current, EER, Calculated and Measured Mass Flows, Percent Difference in Mass Flows, Isentropic Efficieny, and Volumetric Efficiency.",br(),
                                              "The curves are shifted up or down based on the adjustment value provided by the user.",br(),
                                              strong("A value of 1 is no change; a value of 1.1 is a 10% increase."),br(),
                                              
                                              h4("Reset"),
                                              "Click the reset button at any time to revert the working coefficients back to the original pasted set.",br(),
                                              
                                              h3("Working with Rating ELT Data"),
                                              
                                              h4("File Upload"),
                                              "This app accepts output files from the 'Performance Data Pull' R script created by Ref Data Science.",br(),
                                              "Before uploading,", strong("ensure that any repeating logs with incorrect data were removed."), br(),
                                              "Files must contain", strong("at least 12 unique combinations of Evap/Cond conditions"), "to create an acceptable model from which to make coefficients.",
                                              "Similarly, you may not delete so many rows that you have below 12 unique data points.",br(),
                                              "To upload, click 'Browse' and select a file to import from the file explorer window.",br(),br(),
                                              
                                              "To be provided access to this tool, contact Bryan Penkal for database access and installation instructions.",br(),
                                              
                                              h4("Cook's Distance Table"),
                                              "Cook's distance is is used to quantitatively estimate the influence (and likelihood that it's an outlier) of a data point in a regression model.",
                                              "The higher the Cook's distance value is, the more influence that test point likely has on the model.",br(),
                                              "The standard cutoff for outlier removal is 4/n. Points with values above this are generally removed.",br(),
                                              "Click on a row to remove it from the data set. Reselect to include it. Note the recommendation for which rows you might consider removing based on the cutoff value.",br(),
                                              "Click the 'Calculate/Reset button to generate coefficients from the remaining test data.", br(),
                                              
                                              h3("Comparison Tab"),
                                              "The plots rendered in this tab visually compare percent change in two models' performance metrics across the user-defined envelope.",
                                              
                                              h4("Inputs"),
                                              "Change refrigerant, superheat, subcooling, and displacement as needed. There are two choices for envelope using the drop-down menu.",br(),
                                              "Standard envelope: -10/80, -10/100, 40/145, 55/145, 55/80", br(),
                                              "custom: input any number of evaporating and condensing temperatures to create a custom envelope. Conditions need to be in the format Evap/Cond and comma delimited.",br(),
                                              "Import either two sets of coefficients or one set of coefficients and one rating ELT file.",br(),
                                              strong("Always import the left-hand set of coefficients before the other. The other [coef paste or file upload] will render the comparison plots."),br(),
                                              
                                              style='padding-left:30px;'
                                       ))),
                        
                        mainPanel(width=5,
                                  column(12, 
                                         br(),h2("How It Works"),
                                         h3("Units"),
                                         "Units are standard English units and constant throughout.", br(),br(),
                                         column(3,
                                                "Capacity: BTU/hr",br(),
                                                "Power: Watts", br(),
                                                "Current: Amps",
                                                style="padding:'0px"
                                         ),
                                         column(3,
                                                "EER: BTU/W/hr", br(),
                                                "Mass Flow: lbm/hr",
                                                style="padding:'0px"
                                         ),
                                         column(4,
                                                "Temperature: degrees Fahrenheit", br(),
                                                "Displacement: Cu In/rev",
                                                style="padding:'0px"
                                         ),br(),br(),br(),
                                         
                                         h3("Background Calculations"),
                                         "The app utilizes an Emerson REFPROP R library created by Abram Yorde. This library implements CoolProp functionality 
                                 for thermodynamic calculations.",br(),br(),
                                         
                                         strong("Simulated Values"), br(),
                                         "are calculated using the funtion",br(),
                                         "C0 + C1*Te + C2*Tc + C3*(Te^2) + C4*Tc*Te + C5*(Tc^2) + C6*(Te^3) + C7*Tc*(Te^2) + C8*Te*(Tc^2) + C9*(Tc^3)",br(),
                                         "where", br(),
                                         " -- C0-C9 = ten point coefficients",br(),
                                         " -- Te = evaporator temperature",br(),
                                         " -- Tc = condenser temperature",br(),
                                         "This function can be found on any coefficients Excel sheet published to CPIDView.",br(),br(),
                                         
                                         strong("Coeffcients"), br(),
                                         "are calculated by reverse engineering the above function.",br(),
                                         "A linear regression model is created where",br(),
                                         " -- Response variable = known metric (cap, pow, curr..) value", br(),
                                         " -- Predictors = Te, Tc, Tc*Te, Tc^2, ... (from the function above)", br(),
                                         "Coefficients calculated by the linear model are then extracted using an open-souce R function.",br(),br(),
                                         
                                         strong("Calculated mass flow"),br(),
                                         "is calculated using the equation Capacity / H1 - H2 where",br(),
                                         " -- H1 = enthalpy calculated from evap temp + superheat and suction pressure",br(),
                                         " -- H2 = enthalpy calculated from cond temp - subcooling and discharge pressure", br(),br(),
                                         
                                         strong("Isentropic Efficiency"),br(),
                                         "is calculated from the ratio EE/TEER where TEER is calculated using the equation",
                                         "(ReturnGasH - SubcoolLiqH) / (DischargeGasIdealH - ReturnGasH) * 3.412 where", br(),
                                         " -- ReturnGasH = enthalpy calculated using evap temp + superheat and suction pressure",br(),
                                         " -- SubcoolLiqH = enthalpy calculated using discharge temp - subcooling and discharge pressure",br(),
                                         " -- DischargeGasIdealH = enthalpy calculated using inlet entropy and discharge pressure",br(),
                                         " -- 3.412 = fixed unit conversion",br(),br(),
                                         
                                         strong("Volumetric Efficiency"),br(),
                                         "is calculated using the equation Mass Flow / (Density * Displacement * 3500 * 0.034722) where",br(),
                                         " -- Density = calculated from evap temp + superheat and suction pressure, multiplied by 1728 unit conversion factor",br(),
                                         " -- Displacement = input by user",br(),
                                         " -- 3500 = fixed value for RPM",br(),
                                         " -- 0.034722 = fixed unit conversion factor",br(),
                                         
                                         h3("Add Test Points"),
                                         "Using the built in 'Add Test Points' function finds the index of the set of simulated values matching the
                                 input evap and cond temperatures, then replaces them with the input values.",br(),
                                         "Coefficients are then recalculated using the new data set, including the substitution."
                                  )
                        ))),
             
             tabPanel("Create",
                      #makes coefficients from test data or laods coefficients, then allows for manipulation and plotting
                      fluidRow(column(12,
                                      h4("Make Coefficients and Curves"),
                                      "Use this tab to manipulate existing coefficients, create new from rating ELT data, and render performance curves.",br(),br(),
                                      "")),
                      #Sidebar panel with inputs
                      sidebarPanel(width=2,
                                   style="max-height: 585px;",
                                   fluidRow(
                                     column(12,
                                            h4("Inputs"),br(),
                                            radioButtons("choice1", "Import Type",
                                                         c("Coefficients" = "coeffsChoice1",
                                                           "ELT Data" = "testValuesChoice1")),
                                            
                                            "Include .fld or .mix in refrigerant name",br(),
                                            textInput("refrigerant", "Refrigerant", value="R410A.mix", width='115px'),
                                            numericInput("sh", "Superheat", value=20, width='115px'),
                                            numericInput("sc", "Subcooling", value=15, width='115px'),
                                            numericInput("displacement", "Displacement", value=1.54, width='115px'),
                                            style='padding-left:20px; padding-right:0px'
                                     )
                                   )
                      ),
                      
                      mainPanel(
                        #conditional 1
                        conditionalPanel(condition = "input.choice1 == 'coeffsChoice1'",
                                         fluidRow(
                                           column(7,
                                                  h4("Load Coefficients"),br(),
                                                  # tags$head(
                                                  #   tags$style(HTML('#paste{background-color:#98FB98}'))
                                                  # ),
                                                  actionButton("paste", "Paste", style="background-color:#98FB98"),br(),
                                                  strong("To copy:"),
                                                  "Unprotect sheet, select cells, change type to 'number',", br(),
                                                  "increase # decimals (important!) to desired number, press ctrl+c. (See 'About' tab for more details)",br(),br(),
                                                  dataTableOutput("table1A", width='100px'),br(),
                                                  textOutput("reminder"),
                                                  style='padding-left:50px;'
                                           ),
                                           column(5,
                                                  h4("Manipulate Coefficients"),br(),
                                                  tags$head(
                                                    tags$style(HTML('#reset{background-color:#e6b3cc}'))
                                                  ),
                                                  actionButton("reset", "Reset"),br(),
                                                  "Revert working set of coefs back to the original pasted values by pressing this button at any time.",br(),br(),
                                                  
                                                  h4("EER Goal Seek"),
                                                  fluidRow(
                                                    column(3,
                                                           textInput("condition", "Condition", value="50/115", width='120px'),
                                                           style='padding:2px;'),
                                                    column(3,
                                                           numericInput("desired", "Desired EER", value=18, width='120px'),
                                                           style='padding:2px;'),
                                                    column(3, 
                                                           numericInput("adjCap", "Adjust Cap/MF", value=1, width='140px'),
                                                           style='padding:2px;'),
                                                    column(3,
                                                           numericInput("adjPow", "Adjust Pow/Cur", value=1, width='150px'),
                                                           style='padding:2px;')),
                                                  fluidRow(
                                                    column(9,
                                                           htmlOutput("shiftOutput"), style='padding-left:3px;'),
                                                    column(3,
                                                           tags$head(
                                                             tags$style(HTML('#adjust{background-color:#AFEEEE}'))
                                                           ),
                                                           actionButton("adjust", "Shift"))
                                                  ),br(),
                                                  
                                                  #second point match stuff
                                                  checkboxInput("twopoint", "Second Point Match", value=FALSE),
                                                  conditionalPanel(condition = "input.twopoint == 1",
                                                                   fluidRow(
                                                                     column(4,
                                                                            textInput("condit", "Condition", value="10/90", width='120px')),
                                                                     column(4,
                                                                            numericInput("desir", "Desired EER", value=11, width='120px'))),
                                                                   fluidRow(
                                                                     column(6,
                                                                            htmlOutput("SecPointOutput")),
                                                                     column(2,
                                                                            #tags for button background color work inside a conditional panel
                                                                            actionButton("usingPower", "Replace Power", style="background-color: #AFEEEE"))
                                                                     
                                                                   )
                                                  )#############################
                                                  
                                           )
                                         ),br(),br(),
                                         fluidRow(
                                           tags$head(
                                             tags$style(HTML('#plotNew{background-color:#FFA500}'))
                                           ),
                                           actionButton("plotNew", "Plot")
                                         ),
                                         fluidRow(
                                           column(6,
                                                  div(plotOutput("capCurve1A"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("currCurve1A"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("measMFCurve1A"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("percDiffCurve1A"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("volEffyCurve1A"),style="margin-left:-240px; margin-bottom:160px"),
                                                  style='padding-left:0px;'
                                                  
                                           ),
                                           column(6,
                                                  div(plotOutput("powCurve1A"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("EERCurve1A"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("calcMFCurve1A"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("isenEffyCurve1A"),style="margin-right:-500px; margin-bottom:160px"),
                                                  style='padding-right:0px;'
                                           ))),
                        
                        #second conditional panel
                        conditionalPanel(condition = "input.choice1 == 'testValuesChoice1'",
                                         
                                         fluidRow(
                                           column(8,
                                                  h4("Load Data"),
                                                  fluidRow(
                                                    column(6,
                                                           fileInput("upload1B", "File Upload", multiple=F, accept=c(".csv"), width = '600px')),
                                                    column(6,br(),
                                                           "Accepts .csv file type and must contain at least 12 unique test points."
                                                    ))
                                           )),
                                         fluidRow(
                                           column(12,
                                                  textOutput("omitThese"),br(),
                                                  textOutput("click"),br(),
                                                  DT::dataTableOutput("uploadDeleteRows")
                                           )),
                                         br(),
                                         fluidRow(
                                           column(7,
                                                  tags$head(
                                                    tags$style(HTML('#calculate{background-color:#98FB98}'))
                                                  ),
                                                  actionButton("calculate", "Calculate/Reset"),br(),br(),
                                                  div(tableOutput("createCoeffs1B"),style="margin-left:-180px;")
                                           ),
                                           column(1,br(),br(),
                                                  div(h4("EER Goal Seek"),style="margin-left:-140px;"),
                                                  div(textInput("condition2", "Condition", value="50/115", width='120px'),style="margin-left:-160px;"),
                                                  div(numericInput("desired2", "Desired EER", value=18, width='120px'),style="margin-left:-160px;"),
                                                  div(htmlOutput("shiftOutput2", width='160px'),style="margin-left:-160px;"),
                                                  div(actionButton("adjust2", "Shift", style="background-color: #AFEEEE"),style="margin-left:-160px;"),
                                                  style='padding:2px;'),
                                           column(1,
                                                  br(),br(),br(),br(),
                                                  div(numericInput("adjCap2", "Adjust Cap/MF", value=1, width='120px'),style="margin-left:-100px;"),
                                                  div(numericInput("adjPow2", "Adjust Pow/Cur", value=1, width='120px'),style="margin-left:-100px;"),
                                                  style='padding:2px;'),
                                           #second point match
                                           column(2, br(),br(),br(),br(),br(),
                                                  checkboxInput("twopoint2", "Second Point Match", value=FALSE),
                                                  conditionalPanel(condition = "input.twopoint2 == 1",
                                                                   textInput("condit2", "Condition", value="10/90", width='120px'),     
                                                                   numericInput("desir2", "Desired EER", value=11, width='120px'),
                                                                   htmlOutput("SecPointOutput2"),
                                                                   actionButton("usingPower2", "Replace Power", style="background-color: #AFEEEE")
                                                  ))),
                                         actionButton("plotter", "Plot", style="background-color: #FFA500"),
                                         fluidRow(
                                           column(6,
                                                  div(plotOutput("capCurve1B"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("currCurve1B"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("measMFCurve1B"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("percDiffCurve1B"),style="margin-left:-240px; margin-bottom:160px"),
                                                  div(plotOutput("volEffyCurve1B"),style="margin-left:-240px; margin-bottom:160px"),
                                                  style='padding-left:0px'
                                           ),
                                           column(6,
                                                  div(plotOutput("powCurve1B"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("EERCurve1B"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("calcMFCurve1B"),style="margin-right:-500px; margin-bottom:160px"),
                                                  div(plotOutput("isenEffyCurve1B"),style="margin-right:-500px; margin-bottom:160px"),
                                                  style='padding-right:0px;'
                                           )
                                         )
                        ))),
             #########################################################
             #second tab
             tabPanel("Compare",
                      #loads two sets of coefficients or one coef one rating ELT file, compares the two in relevant metrics
                      h4("Compare Compressors"),
                      wellPanel(width = 12,
                                fluidRow(
                                  column(2,
                                         radioButtons("choice2", "Input Type",
                                                      c("Coefficients" = "coeffs2",
                                                        "Coefficients & ELT Data" = "both2")),
                                         style='padding-right:0px'),
                                  column(1,
                                         textInput("refrigerant2", "Refrigerant", value="R410A.mix", width='175px'),
                                         style='padding-left:0px',
                                         textInput("model1Name", "Model 1 Name", value="Mod1")
                                  ),
                                  column(1, 
                                         numericInput("sh2", "Superheat", value=20, width='150px'),
                                         style='padding-left:0px',
                                         textInput("model2Name", "Model 2 Name",value="Mod2")
                                  ),
                                  column(1,
                                         numericInput("sc2", "Subcooling", value=15, width='150px'),
                                         style='padding-left:0px'
                                  ),
                                  column(1,
                                         numericInput("displacement2", "Displacement", value=1.54,width='150px'),
                                         style='padding-left:0px'
                                  ),
                                  column(6,
                                         selectInput("envel", "Envelope:", 
                                                     c("Standard"="standard",
                                                       "Custom"="custom"), width='140px'),
                                         
                                         conditionalPanel(condition = "input.envel == 'custom'",
                                                          column(6,
                                                                 textInput("evapEnvTemps", "Evap (F) Envelope Coords", value = "-20,-20,35,55,55")),
                                                          
                                                          column(6,
                                                                 textInput("condEnvTemps", "Cond (F) Envelope Coords", value = "80,95,145,145,80"))
                                                          
                                         ))
                                )
                      ),#end well panel
                      
                      mainPanel(
                        h4("Load Coefficients"),
                        strong("To copy:"),
                        "Unprotect sheet, select cells, change type to 'number', increase # decimals (important!) to desired number, press ctrl+c. (See 'About' tab for more details)",
                        br(), br(),
                        #first conditional panel
                        conditionalPanel(condition = "input.choice2 == 'coeffs2'",
                                         fluidRow(
                                           column(6,
                                                  "Paste your first set of coefficients here.",br(),br(),
                                                  actionButton("paste2A.1", "Paste", style="background-color: #98FB97"),
                                                  actionButton("pullPrev", "Pull from Create>Coefs", style="background-color: #fffa6b"),
                                                  dataTableOutput("table2A.1"),
                                                  actionButton("compare", "Compare", style="background-color:#FFA500"),
                                                  style = "float:left;"),
                                           column(6,
                                                  "Paste your second set of coefficients here.",br(),br(),
                                                  actionButton("paste2A.2", "Paste",style="background-color: #98FB97"),
                                                  actionButton("pullPrevELT", "Pull from Create>ELT", style="background-color: #fffa6b"),
                                                  dataTableOutput('table2A.2'), br(),br())
                                         ),
                                         fluidRow(selectInput("show", "",
                                                              c("Plots" = "plots",
                                                                "Table" = "table"), width='150px')),br(),
                                         #if plots is selected, show plots
                                         conditionalPanel(condition = "input.show == 'plots'",
                                                          fluidRow(
                                                            
                                                            column(6,
                                                                   div(plotOutput("capDifPlot2A"), style="margin-bottom:160px"),
                                                                   div(plotOutput("currDifPlot2A"), style="margin-bottom:160px"),
                                                                   div(plotOutput("MeasMFDifPlot2A"), style="margin-bottom:160px"),
                                                                   div(plotOutput("isenEffyPlot2A"), style="margin-bottom:160px"),
                                                                   style='padding-left:0px;'
                                                            ),
                                                            column(6,
                                                                   div(plotOutput("powDifPlot2A"), style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("EERDifPlot2A"), style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("CalcMFDifPlot2A"), style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("volEffyPlot2A"), style="margin-left:300px; margin-bottom:160px"),
                                                                   style='padding-right:0px;'
                                                            ))),
                                         #if table is selected, show table
                                         conditionalPanel(condition = "input.show == 'table'",
                                                          fluidRow(
                                                            div(formattableOutput("compareTable2A"), style = "overflow-y: scroll; margin-right:-550px")
                                                          )
                                         )
                                         
                        ), #end conditional panel
                        #second conditional panel
                        conditionalPanel(condition = "input.choice2 == 'both2'",
                                         fluidRow(
                                           column(6,
                                                  "Paste coefficients here first.",br(),br(),
                                                  actionButton("paste2B", "Paste",style="background-color: #98FB97"),
                                                  br(),br(),br(),
                                                  dataTableOutput("table2B"),
                                                  actionButton("compare2", "Compare", style="background-color:#FFA500")
                                           ),
                                           column(6,
                                                  "Accepts .csv file type and must contain at least 12 unique test points.",
                                                  br(),br(),
                                                  fileInput("upload2B", "File Upload", multiple=F, accept=c(".csv"), width = '400px'),
                                                  dataTableOutput("createCoeffs2B")
                                           )),
                                         
                                         fluidRow(selectInput("show2", "",
                                                              c("Plots" = "plots2",
                                                                "Table" = "table2"), width='150px')),br(),
                                         
                                         #if plots is selected, show plots
                                         conditionalPanel(condition = "input.show2 == 'plots2'",
                                                          fluidRow(
                                                            column(6,
                                                                   div(plotOutput("capDifPlot2B"), style="margin-bottom:160px"),
                                                                   div(plotOutput("currDifPlot2B"), style="margin-bottom:160px"),
                                                                   div(plotOutput("MeasMFDifPlot2B"), style="margin-bottom:160px"),
                                                                   div(plotOutput("isenEffyPlot2B"), style="margin-bottom:160px"),
                                                                   style='padding-left:0px;'
                                                            ),
                                                            column(6,
                                                                   div(plotOutput("powDifPlot2B"),style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("EERDifPlot2B"),style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("CalcMFDifPlot2B"),style="margin-left:300px; margin-bottom:160px"),
                                                                   div(plotOutput("volEffyPlot2B"),style="margin-left:300px; margin-bottom:160px"),
                                                                   style='padding-right:0px;'
                                                            ))),
                                         #if table is selected, show table
                                         conditionalPanel(condition = "input.show2 == 'table2'",
                                                          fluidRow(
                                                            div(formattableOutput("compareTable2B"), style = "overflow-y: scroll; margin-right:-550px")
                                                          ))#end conditional panel 
                        )#end conditional panel       
                        
                      ))
  ))#end navbar and ui


####################################################

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
  
  #########################################################
  
  RV <- reactiveValues()
 
  #choice 1, coefficients
  observeEvent(input$paste, {
    #coefficients table output
    pasted1A <- readClipboard()
    pasted1A <- as.numeric(unlist((strsplit(pasted1A, split = "\t"))))
    
    #reset button
    observeEvent(input$reset, {
      
      RV$pastedCoeffs1 <- data.frame(CAP = pasted1A[1:10],
                                     POW = pasted1A[11:20],
                                     CURR = pasted1A[21:30],
                                     MF = pasted1A[31:40])
      #RESET button
      output$table1A = renderDataTable({
        datatable(RV$pastedCoeffs1, rownames=F, selection='none',filter='none', 
                  callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                  options=list(dom='t', ordering=F, digits=10))
      })
    })
    #pastes coefs
    RV$pastedCoeffs1 <- data.frame(CAP = pasted1A[1:10],
                                   POW = pasted1A[11:20],
                                   CURR = pasted1A[21:30],
                                   MF = pasted1A[31:40])
    
    output$table1A = renderDataTable({
      datatable(RV$pastedCoeffs1, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F, digits=10))
    })
    output$reminder = renderText({
      "Reminder: Input correct displacement value before starting manipulation."
    })
    
    #calculate curve % shift (50/115)
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
      str3 <- paste(strong("Shift capacity", shift[1], "OR shift power", shift[2], "to achieve desired EER"))
      HTML(paste(str1,'<br/>',str2,'<br/>',str3))
    })
    
    #adjust coeffs
    observeEvent(input$adjust, {
      RV$pastedCoeffs1$CAP <- RV$pastedCoeffs1$CAP*input$adjCap
      RV$pastedCoeffs1$POW <- RV$pastedCoeffs1$POW*input$adjPow
      RV$pastedCoeffs1$CURR <- RV$pastedCoeffs1$CURR*input$adjPow
      RV$pastedCoeffs1$MF <- RV$pastedCoeffs1$MF*input$adjCap
      
      RV$pastedCoeffs1 <- round(RV$pastedCoeffs1,10)
      
    })
    
    #second point match (10/90)
    observeEvent(input$twopoint, {
      
      output$SecPointOutput = renderUI({
        conditStr <- input$condit
        condition = input$condit %>%
          str_split_fixed(pattern = '/',n = 2) %>%
          as.numeric()
        evap = condition[1]
        cond = condition[2]
        
        RV$one <- evap
        RV$two <- cond
        
        simCAP = mapply(perfCoeff,evap,cond,RV$pastedCoeffs1$CAP[1],RV$pastedCoeffs1$CAP[2],RV$pastedCoeffs1$CAP[3],RV$pastedCoeffs1$CAP[4],RV$pastedCoeffs1$CAP[5],
                        RV$pastedCoeffs1$CAP[6],RV$pastedCoeffs1$CAP[7],RV$pastedCoeffs1$CAP[8],RV$pastedCoeffs1$CAP[9],RV$pastedCoeffs1$CAP[10])
        simPOW = mapply(perfCoeff,evap,cond,RV$pastedCoeffs1$POW[1],RV$pastedCoeffs1$POW[2],RV$pastedCoeffs1$POW[3],RV$pastedCoeffs1$POW[4],RV$pastedCoeffs1$POW[5],
                        RV$pastedCoeffs1$POW[6],RV$pastedCoeffs1$POW[7],RV$pastedCoeffs1$POW[8],RV$pastedCoeffs1$POW[9],RV$pastedCoeffs1$POW[10])
        xpow = (simCAP/input$desir)/simPOW
        secPtPOW <- xpow*simPOW
        RV$secs <- secPtPOW
        str4 <- paste(strong("Adjusted power reaplacement value:", round(secPtPOW,4) ))
        #return
        HTML(paste(str4))
      })
    }) #end observeEvent "two point match" check box
    
    observeEvent(input$usingPower, {
      rval <- reactiveValues(df = data.frame(evap = c(50,55,50,50,50,50,35,30,30,45,10,45,50,35,40),
                                             cond = c(75,90,80,90,100,115,100,100,105,100,90,80,105,80,90)))
      
      rval$df$Capacity <- mapply(perfCoeff,rval$df$evap,rval$df$cond,RV$pastedCoeffs1$CAP[1],
                                 RV$pastedCoeffs1$CAP[2],RV$pastedCoeffs1$CAP[3],RV$pastedCoeffs1$CAP[4],
                                 RV$pastedCoeffs1$CAP[5],RV$pastedCoeffs1$CAP[6],RV$pastedCoeffs1$CAP[7],
                                 RV$pastedCoeffs1$CAP[8],RV$pastedCoeffs1$CAP[9],RV$pastedCoeffs1$CAP[10])
      rval$df$Power <- mapply(perfCoeff,rval$df$evap,rval$df$cond,RV$pastedCoeffs1$POW[1],
                              RV$pastedCoeffs1$POW[2],RV$pastedCoeffs1$POW[3],RV$pastedCoeffs1$POW[4],
                              RV$pastedCoeffs1$POW[5],RV$pastedCoeffs1$POW[6],RV$pastedCoeffs1$POW[7],
                              RV$pastedCoeffs1$POW[8],RV$pastedCoeffs1$POW[9],RV$pastedCoeffs1$POW[10])
      rval$df$Current <- mapply(perfCoeff,rval$df$evap,rval$df$cond,RV$pastedCoeffs1$CURR[1],
                                RV$pastedCoeffs1$CURR[2],RV$pastedCoeffs1$CURR[3],RV$pastedCoeffs1$CURR[4],
                                RV$pastedCoeffs1$CURR[5],RV$pastedCoeffs1$CURR[6],RV$pastedCoeffs1$CURR[7],
                                RV$pastedCoeffs1$CURR[8],RV$pastedCoeffs1$CURR[9],RV$pastedCoeffs1$CURR[10])
      rval$df$MeasMF <- mapply(perfCoeff,rval$df$evap,rval$df$cond,RV$pastedCoeffs1$MF[1],
                               RV$pastedCoeffs1$MF[2],RV$pastedCoeffs1$MF[3],RV$pastedCoeffs1$MF[4],
                               RV$pastedCoeffs1$MF[5],RV$pastedCoeffs1$MF[6],RV$pastedCoeffs1$MF[7],
                               RV$pastedCoeffs1$MF[8],RV$pastedCoeffs1$MF[9],RV$pastedCoeffs1$MF[10])
      test <- round(rval$df,10)
      test[nrow(test)+1,] <- NA
      
      # get indices of extisting conditions that need replaced
      plc1 <- unlist(sapply(test$evap, function(y) match(y,RV$one)))
      index1 <- which(plc1 == 1)
      
      plc2 <- unlist(sapply(test$cond, function(y) match(y,RV$two)))
      index2 <- which(plc2 == 1)
      
      #find the duplicates
      if(length(index1) != 0 & length(index2) != 0){
        indexRem <- Reduce(intersect, list(index1, index2))
        #remove the duplicates
        subAndReplace <- test[indexRem,]
        test <- test[-c(indexRem),]
      }
      
      subAndReplace[,4] <- RV$secs
      #add in new test points at bottom of df
      test[nrow(test),] <- subAndReplace
      
      #make coefficients with lm
      RV$pastedCoeffs1$CAP <- makeCoefficientsWithLM(test$evap, test$cond, test$Capacity)
      RV$pastedCoeffs1$POW <- makeCoefficientsWithLM(test$evap, test$cond, test$Power)
      RV$pastedCoeffs1$CURR <- makeCoefficientsWithLM(test$evap, test$cond, test$Current)
      RV$pastedCoeffs1$MF <- makeCoefficientsWithLM(test$evap, test$cond,  test$MeasMF)
      
      RV$pastedCoeffs1 <- round(RV$pastedCoeffs1,10)
      rval$df <- test
      
      #update coefs table output
      output$table1A = renderDataTable({
        datatable(RV$pastedCoeffs1, rownames=F, selection='none',filter='none', 
                  callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                  options=list(dom='t', ordering=F, digits=10)) 
      })
    }) #end observeEvent "use power"
    
  })#end observe event
  
  observeEvent(input$plotNew, {
    
    #create plot values
    evapPlot <- seq(-10,60,5)
    condPlot <- seq(80,150,10)
    numTotal1A <- length(evapPlot)*length(condPlot)
    
    
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
    }, height = 540, width = 740)#end output
    
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
    }, height = 540, width = 740)#end output
    
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
    }, height = 540, width = 740)#end output
    
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
    }, height = 540, width = 740)#end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
  })#end observe event
  
  
  #########################################################
  #choice 1, tested values
  RVChoice1B <- reactiveValues()
  RVChoice1B$newCoef1B <- data.frame(CAP = numeric(10),
                                     POW = numeric(10),
                                     CURR = numeric(10),
                                     CalcMF = numeric(10),
                                     MF = numeric(10))
  
  observeEvent(input$upload1B, {
    req(input$upload1B)
    RVChoice1B$uploadDF1B <- read_csv(input$upload1B$datapath)
    numRows <- nrow(RVChoice1B$uploadDF1B)
    RVChoice1B$uploadDF1B <- RVChoice1B$uploadDF1B[,c("EvapTemp","CondTemp", "MassFlow", "Capacity", "EER",
                                                      "Power", "Current", "MeasMassFlow", "MassFlow")]
    
    RVChoice1B$newCoef1B$CAP <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Capacity)
    RVChoice1B$newCoef1B$POW <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Power)
    RVChoice1B$newCoef1B$CURR <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Current)
    RVChoice1B$newCoef1B$CalcMF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MassFlow)
    RVChoice1B$newCoef1B$MF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MeasMassFlow)
    
    CAP <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Capacity)
    POW <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Power)
    CURR <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Current)
    MeasMF <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MeasMassFlow)
    CalcMF <- getCooks(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$MassFlow)
    
    RVChoice1B$uploadDF1B = cbind.data.frame(RVChoice1B$uploadDF1B$EvapTemp, RVChoice1B$uploadDF1B$CondTemp, RVChoice1B$uploadDF1B$Capacity, CAP,
                                             RVChoice1B$uploadDF1B$Power, POW, RVChoice1B$uploadDF1B$Current, CURR, RVChoice1B$uploadDF1B$MeasMassFlow,
                                             MeasMF, RVChoice1B$uploadDF1B$MassFlow, CalcMF, stringsAsFactors = F)
    #rouding
    RVChoice1B$uploadDF1B[,c(3,5,7,9,11)] <- round(RVChoice1B$uploadDF1B[,c(3,5,7,9,11)],2)
    RVChoice1B$uploadDF1B[,c(4,6,8,10,12)] <- round(RVChoice1B$uploadDF1B[,c(4,6,8,10,12)],5)
    RVChoice1B$uploadDF1B[,c(1,2)] <- round(RVChoice1B$uploadDF1B[,c(1,2)],1)
    colnames(RVChoice1B$uploadDF1B) <- c("Evap", "Cond", "CAP", "CD_CAP", "POW", "CD_POW", "CURR", "CD_CURR", "MeasMF", "CD_MMF", "CalcMF", "CD_CMF")
    
    #which rows to remove
    cutoffValue <- round(4/(length(RVChoice1B$uploadDF1B[,1])),4)
    delete <- which(RVChoice1B$uploadDF1B[,4] > cutoffValue)
    delete1 <- which(RVChoice1B$uploadDF1B[,6] > cutoffValue)
    delete2 <- which(RVChoice1B$uploadDF1B[,8] > cutoffValue)
    delete3 <- which(RVChoice1B$uploadDF1B[,10] > cutoffValue)
    delete4 <- which(RVChoice1B$uploadDF1B[,12] > cutoffValue)
    
    listoflists <- c(delete, delete1, delete2, delete3, delete4)
    listoflists <- listoflists[!is.na(listoflists)] #get rid of empty lists
    flatList <- toString(sort(unique(unlist(listoflists)))) #flatten list, remove duplciates, sort, and change to string for printing
    
    output$omitThese = renderText({
      str1 <- paste("Cook's distance is used to quantitatively estimate the influence (and likelihood that it's an outlier) of a data point in a regression model.")
      str1.5 <- paste("The higher the Cook's distance value is, the more influence that test point likely has.")
      str2 <- paste("The standard cutoff for outlier removal is", cutoffValue, "(4/n). Rows", flatList, "are recommended for removal.")
      HTML(paste(str1, str1.5, str2))
    })
    
    output$click = renderText({
      "Click to select/deselect rows. Selected rows (blue hightlight) are omitted from coefficient calculations."
    })
    
    output$uploadDeleteRows <- DT::renderDataTable({
      datatable(
        RVChoice1B$uploadDF1B,
        selection = list(mode = "multiple"),
        options = list(ordering=F)
      )
    })
    
  }) #end observe event, upload
  
  #calculate coeffs
  observeEvent(input$calculate, {
    
    ids <- input$uploadDeleteRows_rows_selected
    
    filteredDF_selected <- reactive({
      ids <- input$uploadDeleteRows_rows_selected
      if(length(ids)){
        RVChoice1B$uploadDF1B[-c(ids),]
      }
    })
    
    if(length(ids)){
      RVChoice1B$newCoef1B$CAP <- makeCoefficientsWithLM(filteredDF_selected()$Evap, filteredDF_selected()$Cond, filteredDF_selected()$CAP)
      RVChoice1B$newCoef1B$POW <- makeCoefficientsWithLM(filteredDF_selected()$Evap, filteredDF_selected()$Cond, filteredDF_selected()$POW)
      RVChoice1B$newCoef1B$CURR <- makeCoefficientsWithLM(filteredDF_selected()$Evap, filteredDF_selected()$Cond, filteredDF_selected()$CURR)
      RVChoice1B$newCoef1B$MF <- makeCoefficientsWithLM(filteredDF_selected()$Evap, filteredDF_selected()$Cond, filteredDF_selected()$MeasMF)
      RVChoice1B$newCoef1B$CalcMF <- makeCoefficientsWithLM(filteredDF_selected()$Evap, filteredDF_selected()$Cond, filteredDF_selected()$CalcMF)
    } else {
      RVChoice1B$newCoef1B$CAP <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$Evap, RVChoice1B$uploadDF1B$Cond, RVChoice1B$uploadDF1B$CAP)
      RVChoice1B$newCoef1B$POW <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$Evap, RVChoice1B$uploadDF1B$Cond, RVChoice1B$uploadDF1B$POW)
      RVChoice1B$newCoef1B$CURR <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$Evap, RVChoice1B$uploadDF1B$Cond, RVChoice1B$uploadDF1B$CURR)
      RVChoice1B$newCoef1B$MF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$Evap, RVChoice1B$uploadDF1B$Cond, RVChoice1B$uploadDF1B$MeasMF)
      RVChoice1B$newCoef1B$CalcMF <- makeCoefficientsWithLM(RVChoice1B$uploadDF1B$Evap, RVChoice1B$uploadDF1B$Cond, RVChoice1B$uploadDF1B$CalcMF)
    }
    
    output$createCoeffs1B <- renderTable({
      RVChoice1B$newCoef1B}, digits = 8
    ) #end output
    
    #calculate curve % shift
    output$shiftOutput2 = renderText({
      conditionString <- input$condition2
      condition = input$condition2 %>%
        str_split_fixed(pattern = '/',n = 2) %>%
        as.numeric()
      evap = condition[1]
      cond = condition[2]
      
      shift <- round(curveShift(evap, cond, RVChoice1B$newCoef1B$CAP,  RVChoice1B$newCoef1B$POW, input$desired2),5)
      
      str1 <- paste("Capacity at", conditionString, ":", shift[3])
      str2 <- paste("Power at", conditionString, ":", shift[4])
      str3 <- paste(strong("Shift capacity", shift[1], "OR shift power", shift[2], "to achieve desired EER")) 
      HTML(paste(str1,str2,'<br/>',str3))
    })
    
  })
  
  #option to shift curves up or down by %
  observeEvent(input$adjust2, {
    coefAdjust <- isolate(RVChoice1B$newCoef1B)
    coefAdjust <- coefAdjust %>%
      mutate(CAP = CAP*input$adjCap2,
             POW = POW*input$adjPow2,
             CURR = CURR*input$adjPow2,
             CalcMF = CalcMF*input$adjCap2,
             MF = MF*input$adjCap2)
    RVChoice1B$newCoef1B <- coefAdjust
    
  })
  
  #second point match (10/90)
  observeEvent(input$twopoint2, {
    
    output$SecPointOutput2 = renderUI({
      conditStr2 <- input$condit2
      condition2 = input$condit2 %>%
        str_split_fixed(pattern = '/',n = 2) %>%
        as.numeric()
      evap = condition2[1]
      cond = condition2[2]
      
      RVChoice1B$one2 <- evap
      RVChoice1B$two2 <- cond
      
      simCAP2 = mapply(perfCoeff,evap,cond,RVChoice1B$newCoef1B$CAP[1],RVChoice1B$newCoef1B$CAP[2],RVChoice1B$newCoef1B$CAP[3],RVChoice1B$newCoef1B$CAP[4],RVChoice1B$newCoef1B$CAP[5],
                       RVChoice1B$newCoef1B$CAP[6],RVChoice1B$newCoef1B$CAP[7],RVChoice1B$newCoef1B$CAP[8],RVChoice1B$newCoef1B$CAP[9],RVChoice1B$newCoef1B$CAP[10])
      simPOW2 = mapply(perfCoeff,evap,cond,RVChoice1B$newCoef1B$POW[1],RVChoice1B$newCoef1B$POW[2],RVChoice1B$newCoef1B$POW[3],RVChoice1B$newCoef1B$POW[4],RVChoice1B$newCoef1B$POW[5],
                       RVChoice1B$newCoef1B$POW[6],RVChoice1B$newCoef1B$POW[7],RVChoice1B$newCoef1B$POW[8],RVChoice1B$newCoef1B$POW[9],RVChoice1B$newCoef1B$POW[10])
      xpow2 = (simCAP2/input$desir2)/simPOW2
      secPtPOW2 <- xpow2*simPOW2
      RVChoice1B$secs2 <- secPtPOW2
      str4 <- paste(strong("Adjusted power reaplacement value:", round(secPtPOW2,4) ))
      #return
      HTML(paste(str4))
    })
  }) #end observeEvent "two point match" check box
  
  observeEvent(input$usingPower2, {
    
    rval2 <- reactiveValues(df2 = data.frame(evap = c(50,55,50,50,50,50,35,30,30,45,10,45,50,35,40),
                                             cond = c(75,90,80,90,100,115,100,100,105,100,90,80,105,80,90)))
    
    rval2$df2$Capacity <- mapply(perfCoeff,rval2$df2$evap,rval2$df2$cond,RVChoice1B$newCoef1B$CAP[1],
                                 RVChoice1B$newCoef1B$CAP[2],RVChoice1B$newCoef1B$CAP[3],RVChoice1B$newCoef1B$CAP[4],
                                 RVChoice1B$newCoef1B$CAP[5],RVChoice1B$newCoef1B$CAP[6],RVChoice1B$newCoef1B$CAP[7],
                                 RVChoice1B$newCoef1B$CAP[8],RVChoice1B$newCoef1B$CAP[9],RVChoice1B$newCoef1B$CAP[10])
    rval2$df2$Power <- mapply(perfCoeff,rval2$df2$evap,rval2$df2$cond,RVChoice1B$newCoef1B$POW[1],
                              RVChoice1B$newCoef1B$POW[2],RVChoice1B$newCoef1B$POW[3],RVChoice1B$newCoef1B$POW[4],
                              RVChoice1B$newCoef1B$POW[5],RVChoice1B$newCoef1B$POW[6],RVChoice1B$newCoef1B$POW[7],
                              RVChoice1B$newCoef1B$POW[8],RVChoice1B$newCoef1B$POW[9],RVChoice1B$newCoef1B$POW[10])
    rval2$df2$Current <- mapply(perfCoeff,rval2$df2$evap,rval2$df2$cond,RVChoice1B$newCoef1B$CURR[1],
                                RVChoice1B$newCoef1B$CURR[2],RVChoice1B$newCoef1B$CURR[3],RVChoice1B$newCoef1B$CURR[4],
                                RVChoice1B$newCoef1B$CURR[5],RVChoice1B$newCoef1B$CURR[6],RVChoice1B$newCoef1B$CURR[7],
                                RVChoice1B$newCoef1B$CURR[8],RVChoice1B$newCoef1B$CURR[9],RVChoice1B$newCoef1B$CURR[10])
    rval2$df2$MeasMF <- mapply(perfCoeff,rval2$df2$evap,rval2$df2$cond,RVChoice1B$newCoef1B$MF[1],
                               RVChoice1B$newCoef1B$MF[2],RVChoice1B$newCoef1B$MF[3],RVChoice1B$newCoef1B$MF[4],
                               RVChoice1B$newCoef1B$MF[5],RVChoice1B$newCoef1B$MF[6],RVChoice1B$newCoef1B$MF[7],
                               RVChoice1B$newCoef1B$MF[8],RVChoice1B$newCoef1B$MF[9],RVChoice1B$newCoef1B$MF[10])
    test2 <- round(rval2$df2,10)
    test2[nrow(test2)+1,] <- NA
    
    # get indices of extisting conditions that need replaced
    plc1 <- unlist(sapply(test2$evap, function(y) match(y,RVChoice1B$one2)))
    index1 <- which(plc1 == 1)
    
    plc2 <- unlist(sapply(test2$cond, function(y) match(y,RVChoice1B$two2)))
    index2 <- which(plc2 == 1)
    
    #find the duplicates
    if(length(index1) != 0 & length(index2) != 0){
      indexRem <- Reduce(intersect, list(index1, index2))
      #remove the duplicates
      subAndReplace2 <- test2[indexRem,]
      test2 <- test2[-c(indexRem),]
    }
    
    subAndReplace2[,4] <- RVChoice1B$secs2
    #add in new test2 points at bottom of df
    test2[nrow(test2),] <- subAndReplace2
    
    #make coefficients with lm
    RVChoice1B$newCoef1B$CAP <- makeCoefficientsWithLM(test2$evap, test2$cond, test2$Capacity)
    RVChoice1B$newCoef1B$POW <- makeCoefficientsWithLM(test2$evap, test2$cond, test2$Power)
    RVChoice1B$newCoef1B$CURR <- makeCoefficientsWithLM(test2$evap, test2$cond, test2$Current)
    RVChoice1B$newCoef1B$MF <- makeCoefficientsWithLM(test2$evap, test2$cond,  test2$MeasMF)
    
    RVChoice1B$newCoef1B <- round(RVChoice1B$newCoef1B,10)
    RVChoice1B$df2 <- test2
    
    #update coefs table output
    output$table1B = renderDataTable({
      datatable(RVChoice1B$newCoef1B, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F, digits=10)) 
    })
    
  }) #end observeEvent "use power"
  
  observeEvent(input$plotter, {
    #create plot values
    evapPlot <- seq(-10,60,5)
    condPlot <- seq(80,150,10)
    numTotal <- as.numeric(length(evapPlot)*length(condPlot))
    
    capPlotVals1B <- data.frame(evap = numeric(numTotal),
                                cond = numeric(numTotal),
                                Capacity = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    capPlotVals1B$Capacity <- mapply(perfCoeff,capPlotVals1B$evap,capPlotVals1B$cond,RVChoice1B$newCoef1B$CAP[1],
                                     RVChoice1B$newCoef1B$CAP[2],RVChoice1B$newCoef1B$CAP[3],RVChoice1B$newCoef1B$CAP[4],
                                     RVChoice1B$newCoef1B$CAP[5],RVChoice1B$newCoef1B$CAP[6],RVChoice1B$newCoef1B$CAP[7],
                                     RVChoice1B$newCoef1B$CAP[8],RVChoice1B$newCoef1B$CAP[9],RVChoice1B$newCoef1B$CAP[10])
    
    output$capCurve1B = renderPlot({
      ggplot(capPlotVals1B, aes(x=evap, y=Capacity, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Capacity (Btu / hr)",
             title='Capacity vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 540, width = 740)#end output
    
    powPlotVals1B <- data.frame(evap = numeric(numTotal),
                                cond = numeric(numTotal),
                                Power = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    powPlotVals1B$Power <- mapply(perfCoeff,powPlotVals1B$evap,powPlotVals1B$cond,RVChoice1B$newCoef1B$POW[1],
                                  RVChoice1B$newCoef1B$POW[2],RVChoice1B$newCoef1B$POW[3],RVChoice1B$newCoef1B$POW[4],
                                  RVChoice1B$newCoef1B$POW[5],RVChoice1B$newCoef1B$POW[6],RVChoice1B$newCoef1B$POW[7],
                                  RVChoice1B$newCoef1B$POW[8],RVChoice1B$newCoef1B$POW[9],RVChoice1B$newCoef1B$POW[10])
    
    output$powCurve1B = renderPlot({
      ggplot(powPlotVals1B, aes(x=evap, y=Power, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Power (Watts)", 
             title='Power vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 540, width = 740)#end output
    
    currPlotVals1B <- data.frame(evap = numeric(numTotal),
                                 cond = numeric(numTotal),
                                 Current = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    currPlotVals1B$Current <- mapply(perfCoeff,currPlotVals1B$evap,currPlotVals1B$cond,RVChoice1B$newCoef1B$CURR[1],
                                     RVChoice1B$newCoef1B$CURR[2],RVChoice1B$newCoef1B$CURR[3],RVChoice1B$newCoef1B$CURR[4],
                                     RVChoice1B$newCoef1B$CURR[5],RVChoice1B$newCoef1B$CURR[6],RVChoice1B$newCoef1B$CURR[7],
                                     RVChoice1B$newCoef1B$CURR[8],RVChoice1B$newCoef1B$CURR[9],RVChoice1B$newCoef1B$CURR[10])
    
    output$currCurve1B = renderPlot({
      ggplot(currPlotVals1B, aes(x=evap, y=Current, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Current (Amps)", 
             title='Current vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 540, width = 740)#end output
    
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
    }, height = 540, width = 740)#end output
    
    measMFPlotVals1B <- data.frame(evap = numeric(numTotal),
                                   cond = numeric(numTotal),
                                   MeasMF = numeric(numTotal)) %>%
      mutate(evap = rep_len(evapPlot, numTotal),
             cond = rep_len(condPlot, numTotal))
    
    measMFPlotVals1B$MeasMF <- mapply(perfCoeff,measMFPlotVals1B$evap,measMFPlotVals1B$cond,RVChoice1B$newCoef1B$MF[1],
                                      RVChoice1B$newCoef1B$MF[2],RVChoice1B$newCoef1B$MF[3],RVChoice1B$newCoef1B$MF[4],
                                      RVChoice1B$newCoef1B$MF[5],RVChoice1B$newCoef1B$MF[6],RVChoice1B$newCoef1B$MF[7],
                                      RVChoice1B$newCoef1B$MF[8],RVChoice1B$newCoef1B$MF[9],RVChoice1B$newCoef1B$MF[10])
    
    output$measMFCurve1B = renderPlot({
      ggplot(measMFPlotVals1B, aes(x=evap, y=MeasMF, color=factor(cond))) +
        geom_smooth(method = loess, se = FALSE) +
        labs(x = expression(paste("Evaporating Temperature ( ", degree ~ F, " )")), y = "Mass Flow ( lbm/hr )", 
             title='Measured Mass Flow vs. Evap', color = expression(paste("Cond Temp (", degree ~ F, ")"))) +
        theme_bw() +
        theme(panel.grid.major = element_line(colour = "darkgrey")) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    })#end output
  })#end observe event
  
  #pull from Create tab, if coefs were pasted
  observeEvent(input$pullPrev, {
    RV2$pastedCoeffs2A.1 <- RV$pastedCoeffs1
    
    output$table2A.1 = renderDataTable({
      datatable(RV2$pastedCoeffs2A.1, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
    
  })
  
  #pull from Create tab, if coefs were made from ELT
  observeEvent(input$pullPrevELT, {
    RVChoice1B$newCoef1B <- RVChoice1B$newCoef1B[,c(1,2,3,5)]
    colnames(RVChoice1B$newCoef1B) <- c("CAP", "POW", "CURR", "MF")
    
    RV2$pastedCoeffs2A.2 <- RVChoice1B$newCoef1B
    
    output$table2A.2 = renderDataTable({
      datatable(RV2$pastedCoeffs2A.2, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
    
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
  
  observeEvent(input$compare, {
browser()
    #create envelope
    if(input$envel == 'custom') {
      evapEnv <- input$evapEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$evapEnvTemps,',') +1) %>%
        as.numeric()
      condEnv <- input$condEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$condEnvTemps,',') +1) %>%
        as.numeric()
    }
    if(input$envel == 'standard') {
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
    comparisonDF2A$CAP_Mod1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$CAP[1],
                                      RV2$pastedCoeffs2A.1$CAP[2],RV2$pastedCoeffs2A.1$CAP[3],RV2$pastedCoeffs2A.1$CAP[4],
                                      RV2$pastedCoeffs2A.1$CAP[5],RV2$pastedCoeffs2A.1$CAP[6],RV2$pastedCoeffs2A.1$CAP[7],
                                      RV2$pastedCoeffs2A.1$CAP[8],RV2$pastedCoeffs2A.1$CAP[9],RV2$pastedCoeffs2A.1$CAP[10])
    comparisonDF2A$POW_Mod1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$POW[1],
                                      RV2$pastedCoeffs2A.1$POW[2],RV2$pastedCoeffs2A.1$POW[3],RV2$pastedCoeffs2A.1$POW[4],
                                      RV2$pastedCoeffs2A.1$POW[5],RV2$pastedCoeffs2A.1$POW[6],RV2$pastedCoeffs2A.1$POW[7],
                                      RV2$pastedCoeffs2A.1$POW[8],RV2$pastedCoeffs2A.1$POW[9],RV2$pastedCoeffs2A.1$POW[10])
    comparisonDF2A$EER_Mod1 <- comparisonDF2A$CAP_Mod1 / comparisonDF2A$POW_Mod1
    comparisonDF2A$CURR_Mod1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$CURR[1],
                                       RV2$pastedCoeffs2A.1$CURR[2],RV2$pastedCoeffs2A.1$CURR[3],RV2$pastedCoeffs2A.1$CURR[4],
                                       RV2$pastedCoeffs2A.1$CURR[5],RV2$pastedCoeffs2A.1$CURR[6],RV2$pastedCoeffs2A.1$CURR[7],
                                       RV2$pastedCoeffs2A.1$CURR[8],RV2$pastedCoeffs2A.1$CURR[9],RV2$pastedCoeffs2A.1$CURR[10])
    comparisonDF2A$MeasMF_Mod1 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.1$MF[1],
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
    comparisonDF2A$CalcMF_Mod1 <- comparisonDF2A$CAP_Mod1 / (calcMF1$H1 - calcMF1$H2)
    #calcs isen effy
    isenTemp1 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    isenTemp1$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    isenTemp1$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    isenTemp1$T3_Q0 = mapply(refprope,'T','P',isenTemp1$PDis,'Q',0,input$refrigerant2)
    isenTemp1$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp1$T3_Q0 - input$sc2),'P',isenTemp1$PDis,input$refrigerant2)
    isenTemp1$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp1$PSuc,input$refrigerant2)
    isenTemp1$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp1$PSuc,input$refrigerant2)
    isenTemp1$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp1$PDis,'S',isenTemp1$Inlet_Entropy,input$refrigerant2)
    isenTemp1$EER <- comparisonDF2A$CAP_Mod1 / comparisonDF2A$POW_Mod1
    isenTemp1$TEER <- (isenTemp1$ReturnGasH - isenTemp1$SubcoolLiqH)/(isenTemp1$DischargeGasIdealH - isenTemp1$ReturnGasH) * 3.412
    isenTemp1$isenEffy <- isenTemp1$EER / isenTemp1$TEER
    comparisonDF2A$IsenEffy_Mod1 <- isenTemp1$isenEffy
    #calcs vol effy
    volTemp1 <- data.frame(Evap = comparisonDF2A$Evap)
    volTemp1$PSuc <- mapply(refprope,'P','T',volTemp1$Evap,'Q',1,input$refrigerant2)
    volTemp1$Density <- mapply(refprope, 'D', 'T', volTemp1$Evap+input$sh2, 'P', volTemp1$PSuc,input$refrigerant2)  
    volTemp1$Density <- volTemp1$Density * 1728 #conversion to lbm/ft3
    volTemp1$mf <- comparisonDF2A$MeasMF_Mod1
    volTemp1$volEffy <- volTemp1$mf / (volTemp1$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2A$VolEffy_Mod1 <- volTemp1$volEffy
    
    ##### second set of coefficients
    comparisonDF2A$CAP_Mod2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$CAP[1],
                                      RV2$pastedCoeffs2A.2$CAP[2],RV2$pastedCoeffs2A.2$CAP[3],RV2$pastedCoeffs2A.2$CAP[4],
                                      RV2$pastedCoeffs2A.2$CAP[5],RV2$pastedCoeffs2A.2$CAP[6],RV2$pastedCoeffs2A.2$CAP[7],
                                      RV2$pastedCoeffs2A.2$CAP[8],RV2$pastedCoeffs2A.2$CAP[9],RV2$pastedCoeffs2A.2$CAP[10])
    comparisonDF2A$POW_Mod2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$POW[1],
                                      RV2$pastedCoeffs2A.2$POW[2],RV2$pastedCoeffs2A.2$POW[3],RV2$pastedCoeffs2A.2$POW[4],
                                      RV2$pastedCoeffs2A.2$POW[5],RV2$pastedCoeffs2A.2$POW[6],RV2$pastedCoeffs2A.2$POW[7],
                                      RV2$pastedCoeffs2A.2$POW[8],RV2$pastedCoeffs2A.2$POW[9],RV2$pastedCoeffs2A.2$POW[10])
    comparisonDF2A$EER_Mod2 <- comparisonDF2A$CAP_Mod2 / comparisonDF2A$POW_Mod2
    comparisonDF2A$CURR_Mod2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$CURR[1],
                                       RV2$pastedCoeffs2A.2$CURR[2],RV2$pastedCoeffs2A.2$CURR[3],RV2$pastedCoeffs2A.2$CURR[4],
                                       RV2$pastedCoeffs2A.2$CURR[5],RV2$pastedCoeffs2A.2$CURR[6],RV2$pastedCoeffs2A.2$CURR[7],
                                       RV2$pastedCoeffs2A.2$CURR[8],RV2$pastedCoeffs2A.2$CURR[9],RV2$pastedCoeffs2A.2$CURR[10])
    comparisonDF2A$MeasMF_Mod2 <- mapply(perfCoeff,comparisonDF2A$Evap,comparisonDF2A$Cond,RV2$pastedCoeffs2A.2$MF[1],
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
    comparisonDF2A$CalcMF_Mod2 <- comparisonDF2A$CAP_Mod2 / (calcMF2$H1 - calcMF2$H2)
    #calcs isen effy
    isenTemp2 <- data.frame(PSuc = numeric(length(comparisonDF2A$Evap)))
    isenTemp2$PSuc = mapply(refprope,'P','T',comparisonDF2A$Evap,'Q',1,input$refrigerant2)
    isenTemp2$PDis = mapply(refprope,'P','T',comparisonDF2A$Cond,'Q',1,input$refrigerant2)
    isenTemp2$T3_Q0 = mapply(refprope,'T','P',isenTemp2$PDis,'Q',0,input$refrigerant2)
    isenTemp2$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp2$T3_Q0 - input$sc2),'P',isenTemp2$PDis,input$refrigerant2)
    isenTemp2$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp2$PSuc,input$refrigerant2)
    isenTemp2$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2A$Evap + input$sh2),'P',isenTemp2$PSuc,input$refrigerant2)
    isenTemp2$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp2$PDis,'S',isenTemp2$Inlet_Entropy,input$refrigerant2)
    isenTemp2$EER <- comparisonDF2A$CAP_Mod2 / comparisonDF2A$POW_Mod2
    isenTemp2$TEER <- (isenTemp2$ReturnGasH - isenTemp2$SubcoolLiqH)/(isenTemp2$DischargeGasIdealH - isenTemp2$ReturnGasH) * 3.412
    isenTemp2$isenEffy <- isenTemp2$EER / isenTemp2$TEER
    comparisonDF2A$IsenEffy_Mod2 <- isenTemp2$isenEffy
    #calcs vol effy
    volTemp2 <- data.frame(Evap = comparisonDF2A$Evap)
    volTemp2$PSuc <- mapply(refprope,'P','T',volTemp2$Evap,'Q',1,input$refrigerant2)
    volTemp2$Density <- mapply(refprope, 'D', 'T', volTemp2$Evap+input$sh2, 'P', volTemp2$PSuc,input$refrigerant2)  
    volTemp2$Density <- volTemp2$Density * 1728 #conversion to lbm/ft3
    volTemp2$mf <- comparisonDF2A$MeasMF_Mod2
    volTemp2$volEffy <- volTemp2$mf / (volTemp2$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2A$VolEffy_Mod2 <- volTemp2$volEffy
    #calcualate %difference columns
    comparisonDF2A$Error_CAP <- (comparisonDF2A$CAP_Mod1 - comparisonDF2A$CAP_Mod2) / comparisonDF2A$CAP_Mod1 * 100
    comparisonDF2A$Error_POW <- (comparisonDF2A$POW_Mod1 - comparisonDF2A$POW_Mod2) / comparisonDF2A$POW_Mod1 * 100
    comparisonDF2A$Error_EER <- (comparisonDF2A$EER_Mod1 - comparisonDF2A$EER_Mod2) / comparisonDF2A$EER_Mod1 * 100
    comparisonDF2A$Error_CURR <- (comparisonDF2A$CURR_Mod1 - comparisonDF2A$CURR_Mod2) / comparisonDF2A$CURR_Mod1 * 100
    comparisonDF2A$Error_MMF <- (comparisonDF2A$MeasMF_Mod1 - comparisonDF2A$MeasMF_Mod2) / comparisonDF2A$MeasMF_Mod1 * 100
    comparisonDF2A$Error_CMF <- (comparisonDF2A$CalcMF_Mod1 - comparisonDF2A$CalcMF_Mod2) / comparisonDF2A$CalcMF_Mod1 * 100
    comparisonDF2A$Error_IsenEffy <- (comparisonDF2A$IsenEffy_Mod1 - comparisonDF2A$IsenEffy_Mod2) / comparisonDF2A$IsenEffy_Mod1 * 100
    comparisonDF2A$Error_VolEffy <- (comparisonDF2A$VolEffy_Mod1 - comparisonDF2A$VolEffy_Mod2) / comparisonDF2A$VolEffy_Mod1 * 100
    
    comparisonDF2A <- round(comparisonDF2A[,c(1,2,3,11,19,4,12,20,5,13,21,6,14,22,7,15,23,8,16,24,9,17,25,10,18,26)],2)
    
    output$compareTable2A = renderFormattable({
      
      names(comparisonDF2A) <- gsub(x = names(comparisonDF2A), pattern = "Mod1", replacement = input$model1Name)  
      names(comparisonDF2A) <- gsub(x = names(comparisonDF2A), pattern = "Mod2", replacement = input$model2Name)
      
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
        
      ), 
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
                                 MF = pasted2B[31:40])
    
    RV3$pastedCoeffs2B <- pastedCoeffs2B
    
    output$table2B = renderDataTable({
      datatable(RV3$pastedCoeffs2B, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
  })
  
  # #pull from Create tab, if coefs were pasted
  # observeEvent(input$pullPrev2, {
  #   RV3$pastedCoeffs2B <- RV$pastedCoeffs1
  #   
  #   output$table2B = renderDataTable({
  #     datatable(RV3$pastedCoeffs2B, rownames=F, selection='none',filter='none', 
  #               callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
  #               options=list(dom='t', ordering=F))
  #   })#end output
  #   
  # })
  # 
  # #pull from Create tab, if coefs were made from ELT
  # observeEvent(input$pullPrevELT2, {
  #   RV3$pastedCoeffs2B <- RVChoice1B$newCoef1B
  #   
  #   output$table2B = renderDataTable({
  #     datatable(RV3$pastedCoeffs2B, rownames=F, selection='none',filter='none', 
  #               callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
  #               options=list(dom='t', ordering=F))
  #   })#end output
  # 
  # })
  
  observeEvent(input$upload2B, {
    req(input$upload2B)
    uploadDF2B <- read_csv(input$upload2B$datapath)
    #create coefficients
    RV3$newCoef2B <- data.frame(CAP = numeric(10),
                            POW = numeric(10),
                            CURR = numeric(10),
                            MeasMF = numeric(10))
    
    RV3$newCoef2B$CAP <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Capacity)
    RV3$newCoef2B$POW <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Power)
    RV3$newCoef2B$CURR <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$Current)
    RV3$newCoef2B$MeasMF <- makeCoefficientsWithLM(uploadDF2B$EvapTemp, uploadDF2B$CondTemp, uploadDF2B$MeasMassFlow)
    RV3$newCoef2B <- round(RV3$newCoef2B, 10)
    
    output$createCoeffs2B = renderDataTable({
      datatable(RV3$newCoef2B, rownames=F, selection='none',filter='none', 
                callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                options=list(dom='t', ordering=F))
    })#end output
  })
  
  observeEvent(input$compare2, {
    
    if(input$envel == 'custom') {
      evapEnv <- input$evapEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$evapEnvTemps,',') +1) %>%
        as.numeric()
      condEnv <- input$condEnvTemps %>%
        str_split_fixed(pattern = ',',n = str_count(input$condEnvTemps,',') +1) %>%
        as.numeric()
    }
    if(input$envel == 'standard') {
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
    comparisonDF2B$CAP_Mod1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$CAP[1],
                                      RV3$pastedCoeffs2B$CAP[2],RV3$pastedCoeffs2B$CAP[3],RV3$pastedCoeffs2B$CAP[4],
                                      RV3$pastedCoeffs2B$CAP[5],RV3$pastedCoeffs2B$CAP[6],RV3$pastedCoeffs2B$CAP[7],
                                      RV3$pastedCoeffs2B$CAP[8],RV3$pastedCoeffs2B$CAP[9],RV3$pastedCoeffs2B$CAP[10])
    comparisonDF2B$POW_Mod1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$POW[1],
                                      RV3$pastedCoeffs2B$POW[2],RV3$pastedCoeffs2B$POW[3],RV3$pastedCoeffs2B$POW[4],
                                      RV3$pastedCoeffs2B$POW[5],RV3$pastedCoeffs2B$POW[6],RV3$pastedCoeffs2B$POW[7],
                                      RV3$pastedCoeffs2B$POW[8],RV3$pastedCoeffs2B$POW[9],RV3$pastedCoeffs2B$POW[10])
    comparisonDF2B$EER_Mod1 <- comparisonDF2B$CAP_Mod1 / comparisonDF2B$POW_Mod1
    comparisonDF2B$CURR_Mod1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$CURR[1],
                                       RV3$pastedCoeffs2B$CURR[2],RV3$pastedCoeffs2B$CURR[3],RV3$pastedCoeffs2B$CURR[4],
                                       RV3$pastedCoeffs2B$CURR[5],RV3$pastedCoeffs2B$CURR[6],RV3$pastedCoeffs2B$CURR[7],
                                       RV3$pastedCoeffs2B$CURR[8],RV3$pastedCoeffs2B$CURR[9],RV3$pastedCoeffs2B$CURR[10])
    comparisonDF2B$MeasMF_Mod1 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$pastedCoeffs2B$MF[1],
                                         RV3$pastedCoeffs2B$MF[2],RV3$pastedCoeffs2B$MF[3],RV3$pastedCoeffs2B$MF[4],
                                         RV3$pastedCoeffs2B$MF[5],RV3$pastedCoeffs2B$MF[6],RV3$pastedCoeffs2B$MF[7],
                                         RV3$pastedCoeffs2B$MF[8],RV3$pastedCoeffs2B$MF[9],RV3$pastedCoeffs2B$MF[10])
    #calcs calc mf
    calcMF3 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    calcMF3$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    calcMF3$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    calcMF3$T3_Q0 = mapply(refprope,'T','P',calcMF3$PDis,'Q',0,input$refrigerant2)
    calcMF3$H1 = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',calcMF3$PSuc,input$refrigerant2)
    calcMF3$H2 = mapply(refprope,'H','T',(calcMF3$T3_Q0 - input$sc2),'P',calcMF3$PDis,input$refrigerant2)
    comparisonDF2B$CalcMF_Mod1 <- comparisonDF2B$CAP_Mod1 / (calcMF3$H1 - calcMF3$H2)
    #calcs isen effy
    isenTemp3 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    isenTemp3$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    isenTemp3$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    isenTemp3$T3_Q0 = mapply(refprope,'T','P',isenTemp3$PDis,'Q',0,input$refrigerant2)
    isenTemp3$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp3$T3_Q0 - input$sc2),'P',isenTemp3$PDis,input$refrigerant2)
    isenTemp3$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp3$PSuc,input$refrigerant2)
    isenTemp3$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp3$PSuc,input$refrigerant2)
    isenTemp3$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp3$PDis,'S',isenTemp3$Inlet_Entropy,input$refrigerant2)
    isenTemp3$EER <- comparisonDF2B$CAP_Mod1 / comparisonDF2B$POW_Mod1
    isenTemp3$TEER <- (isenTemp3$ReturnGasH - isenTemp3$SubcoolLiqH)/(isenTemp3$DischargeGasIdealH - isenTemp3$ReturnGasH) * 3.412
    isenTemp3$isenEffy <- isenTemp3$EER / isenTemp3$TEER
    comparisonDF2B$IsenEffy_Mod1 <- isenTemp3$isenEffy
    #calcs vol effy
    volTemp3 <- data.frame(Evap = comparisonDF2B$Evap)
    volTemp3$PSuc <- mapply(refprope,'P','T',volTemp3$Evap,'Q',1,input$refrigerant2)
    volTemp3$Density <- mapply(refprope, 'D', 'T', volTemp3$Evap+input$sh2, 'P', volTemp3$PSuc,input$refrigerant2)  
    volTemp3$Density <- volTemp3$Density * 1728 #conversion to lbm/ft3
    volTemp3$mf <- comparisonDF2B$MeasMF_Mod1
    volTemp3$volEffy <- volTemp3$mf / (volTemp3$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2B$VolEffy_Mod1 <- volTemp3$volEffy
    
    ##### second set, created from uploaded test data
    comparisonDF2B$CAP_Mod2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$newCoef2B$CAP[1],
                                      RV3$newCoef2B$CAP[2],RV3$newCoef2B$CAP[3],RV3$newCoef2B$CAP[4],
                                      RV3$newCoef2B$CAP[5],RV3$newCoef2B$CAP[6],RV3$newCoef2B$CAP[7],
                                      RV3$newCoef2B$CAP[8],RV3$newCoef2B$CAP[9],RV3$newCoef2B$CAP[10])
    comparisonDF2B$POW_Mod2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$newCoef2B$POW[1],
                                      RV3$newCoef2B$POW[2],RV3$newCoef2B$POW[3],RV3$newCoef2B$POW[4],
                                      RV3$newCoef2B$POW[5],RV3$newCoef2B$POW[6],RV3$newCoef2B$POW[7],
                                      RV3$newCoef2B$POW[8],RV3$newCoef2B$POW[9],RV3$newCoef2B$POW[10])
    comparisonDF2B$EER_Mod2 <- comparisonDF2B$CAP_Mod2 / comparisonDF2B$POW_Mod2
    comparisonDF2B$CURR_Mod2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$newCoef2B$CURR[1],
                                       RV3$newCoef2B$CURR[2],RV3$newCoef2B$CURR[3],RV3$newCoef2B$CURR[4],
                                       RV3$newCoef2B$CURR[5],RV3$newCoef2B$CURR[6],RV3$newCoef2B$CURR[7],
                                       RV3$newCoef2B$CURR[8],RV3$newCoef2B$CURR[9],RV3$newCoef2B$CURR[10])
    comparisonDF2B$MeasMF_Mod2 <- mapply(perfCoeff,comparisonDF2B$Evap,comparisonDF2B$Cond,RV3$newCoef2B$MeasMF[1],
                                         RV3$newCoef2B$MeasMF[2],RV3$newCoef2B$MeasMF[3],RV3$newCoef2B$MeasMF[4],
                                         RV3$newCoef2B$MeasMF[5],RV3$newCoef2B$MeasMF[6],RV3$newCoef2B$MeasMF[7],
                                         RV3$newCoef2B$MeasMF[8],RV3$newCoef2B$MeasMF[9],RV3$newCoef2B$MeasMF[10])
    
    #calcs calc mf
    calcMF4 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    calcMF4$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    calcMF4$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    calcMF4$T3_Q0 = mapply(refprope,'T','P',calcMF4$PDis,'Q',0,input$refrigerant2)
    calcMF4$H1 = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',calcMF4$PSuc,input$refrigerant2)
    calcMF4$H2 = mapply(refprope,'H','T',(calcMF4$T3_Q0 - input$sc2),'P',calcMF4$PDis,input$refrigerant2)
    comparisonDF2B$CalcMF_Mod2 <- comparisonDF2B$CAP_Mod2 / (calcMF4$H1 - calcMF4$H2)
    #calcs isen effy
    isenTemp4 <- data.frame(PSuc = numeric(length(comparisonDF2B$Evap)))
    isenTemp4$PSuc = mapply(refprope,'P','T',comparisonDF2B$Evap,'Q',1,input$refrigerant2)
    isenTemp4$PDis = mapply(refprope,'P','T',comparisonDF2B$Cond,'Q',1,input$refrigerant2)
    isenTemp4$T3_Q0 = mapply(refprope,'T','P',isenTemp4$PDis,'Q',0,input$refrigerant2)
    isenTemp4$SubcoolLiqH = mapply(refprope,'H','T',(isenTemp4$T3_Q0 - input$sc2),'P',isenTemp4$PDis,input$refrigerant2)
    isenTemp4$ReturnGasH = mapply(refprope,'H','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp4$PSuc,input$refrigerant2)
    isenTemp4$Inlet_Entropy = mapply(refprope,'S','T',(comparisonDF2B$Evap + input$sh2),'P',isenTemp4$PSuc,input$refrigerant2)
    isenTemp4$DischargeGasIdealH = mapply(refprope,'H','P',isenTemp4$PDis,'S',isenTemp4$Inlet_Entropy,input$refrigerant2)
    isenTemp4$EER <- comparisonDF2B$CAP_Mod2 / comparisonDF2B$POW_Mod2
    isenTemp4$TEER <- (isenTemp4$ReturnGasH - isenTemp4$SubcoolLiqH)/(isenTemp4$DischargeGasIdealH - isenTemp4$ReturnGasH) * 3.412
    isenTemp4$isenEffy <- isenTemp4$EER / isenTemp4$TEER
    comparisonDF2B$IsenEffy_Mod2 <- isenTemp4$isenEffy
    #calcs vol effy
    volTemp4 <- data.frame(Evap = comparisonDF2B$Evap)
    volTemp4$PSuc <- mapply(refprope,'P','T',volTemp4$Evap,'Q',1,input$refrigerant2)
    volTemp4$Density <- mapply(refprope, 'D', 'T', volTemp4$Evap+input$sh2, 'P', volTemp4$PSuc,input$refrigerant2)  
    volTemp4$Density <- volTemp4$Density * 1728 #conversion to lbm/ft3
    volTemp4$mf <- comparisonDF2B$MeasMF_Mod2
    volTemp4$volEffy <- volTemp4$mf / (volTemp4$Density * input$displacement2 * 3500 * 0.034722)
    comparisonDF2B$VolEffy_Mod2 <- volTemp4$volEffy
    #calcualate %difference columns
    comparisonDF2B$Error_CAP <- (comparisonDF2B$CAP_Mod1 - comparisonDF2B$CAP_Mod2) / comparisonDF2B$CAP_Mod1 * 100
    comparisonDF2B$Error_POW <- (comparisonDF2B$POW_Mod1 - comparisonDF2B$POW_Mod2) / comparisonDF2B$POW_Mod1 * 100
    comparisonDF2B$Error_EER <- (comparisonDF2B$EER_Mod1 - comparisonDF2B$EER_Mod2) / comparisonDF2B$EER_Mod1 * 100
    comparisonDF2B$Error_CURR <- (comparisonDF2B$CURR_Mod1 - comparisonDF2B$CURR_Mod2) / comparisonDF2B$CURR_Mod1 * 100
    comparisonDF2B$Error_MMF <- (comparisonDF2B$MeasMF_Mod1 - comparisonDF2B$MeasMF_Mod2) / comparisonDF2B$MeasMF_Mod1 * 100
    comparisonDF2B$Error_CMF <- (comparisonDF2B$CalcMF_Mod1 - comparisonDF2B$CalcMF_Mod2) / comparisonDF2B$CalcMF_Mod1 * 100
    comparisonDF2B$Error_IsenEffy <- (comparisonDF2B$IsenEffy_Mod1 - comparisonDF2B$IsenEffy_Mod2) / comparisonDF2B$IsenEffy_Mod1 * 100
    comparisonDF2B$Error_VolEffy <- (comparisonDF2B$VolEffy_Mod1 - comparisonDF2B$VolEffy_Mod2) / comparisonDF2B$VolEffy_Mod1 * 100
    
    comparisonDF2B <- round(comparisonDF2B[,c(1,2,3,11,19,4,12,20,5,13,21,6,14,22,7,15,23,8,16,24,9,17,25,10,18,26)],2)
    
    output$compareTable2B = renderFormattable({
      formattable(comparisonDF2B, list(
        # style = formatter("span", x ~ style("font-size:10px;")),
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
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
    }, height = 540, width = 740) #end output
    
  })
  
}#end server
####################################################

# Run the application 
shinyApp(ui = ui, server = server)

