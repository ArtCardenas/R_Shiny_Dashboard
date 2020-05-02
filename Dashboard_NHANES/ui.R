#------------------------------------------------------------------------------------------
# Remember:  Data is NOT shared from Server.R  and   ui.R
#            so data has to be loaded twice


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#   http://shiny.rstudio.com/
#
#   
#   https://acarden6.shinyapps.io/Dashboard_NHANES/


#library(devtools)
#   https://www.rdocumentation.org/packages/dashboardthemes/versions/1.0.6
#install_github("nik01010/dashboardthemes")


#-------------------------------------------------------------------------------------------
library(shiny)              # look at page 12/44 from R_Shiny_Tutorial_1.docx
library(shinydashboard)
library(plotly)
library(dashboardthemes)
library(sunburstR)
#library(d3r)
#library(formattable)

myData_Num <- readRDS("data/mydata_numeric.Rda")




# allColumns <- c(    # "SEQN"
#   "Age_In_Years", # Age Years
#   "Income", # income
#   "Heart_Pulse"  , # Pulse
#   "Weight",    # Weight
#   "Height",    # Height
#   "BMI"    # BMI
# )

shinyUI(
  dashboardPage (title = "NHANES Dashboard",
    dashboardHeader(title = "Dashboard Menu",titleWidth = 350),
    dashboardSidebar(
      width = 350,
        sidebarMenu(
            menuItem("Welcome", icon = icon("handshake-o"), # tabname = "Welcome Tab Name"
                     menuSubItem("Welcome Page", tabName = "WelcomeTab")
                    ),
       
            menuItem("Demographics", icon = icon("users"), # tabname = 'Demo Tab Name'
                      menuSubItem("Demographics Page", tabName = "DemoTab"),
                      menuSubItem("Education and Age", tabName = "EduAgesTab")
                     ),

            menuItem("Data Preparation", icon = icon("line-chart"), # , tabName = "Data_Science_Tab"
                     menuSubItem("Height and Weight", tabName = "HeightWeightTab"),
                     menuSubItem("Correlations", tabName = "CorrelationsTab"),
                     menuSubItem("Outliers", tabName = "OutliersTab")
                    ),

            menuItem("Data Science", icon = icon("cogs"), # , tabName = "Data_Science_Tab"
                    menuSubItem("Cluster Analysis", tabName = "ClusterTab"),
                    menuSubItem("Logistic Regression", tabName = "LogisticTab"),
                    menuSubItem("Random Forest", tabName = "RandForestTab"))
            )),
    
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
      tabItems(
        tabItem(tabName = "WelcomeTab",
            fluidPage(
              h1("Arturo Cardenas, UNCC Spring 2020", align = "center"),
              h2("Data Science Academic Project"), align = "center"),
              h3("National Health and Nutrition Survey", align = "center"),
            fluidPage(  
              imageOutput("Image_Concept"), align = "center")),

        tabItem(tabName = "DemoTab",
            h1("Participant Demographics", align= "center"),
            fluidPage(h3(" Total Counts of Participants"),
                      valueBox(4535, "Total Surveyed", width = 3, color = "blue", icon = icon("users")),  
                      valueBox(2477, "Low Income",width = 3, color = "yellow", icon = icon("home")),  
                      valueBox(1750, "High Income",width = 3, color = "yellow", icon = icon("home")), 
                      valueBox(308, "N/A Income",width = 3, color = "yellow", icon = icon("home")), 
                      box(title = "Instructions" , width = 12, 
                          "Please click on the Tabs to choose a different sub-group",
                          collapsible = TRUE, collapsed = TRUE)),
            fluidPage(
              tabBox(title = "Gender", width = 6,
                     tabPanel("Total",       plotlyOutput("demo_gender_pie")),
                     tabPanel("Low Income",  plotlyOutput("demo_gender_pie_LI")),
                     tabPanel("High Income", plotlyOutput("demo_gender_pie_HI"))),
              
              box(title = "Participant Age",
                  #h4("Participant Age", align = "center"),
                  plotlyOutput("age_histogram")
                  #checkboxInput(inputId="chkbxAge",label="Detail Ages")
              )),  # check page 13/41     R_Shiny_Tutorial_2.docx
            
            fluidPage(h3("Race & Marital Status"),
                      tabBox(title = "Race", width = 6,
                             tabPanel("Total",       plotlyOutput("demo_race_pie")),
                             tabPanel("Low Income",  plotlyOutput("demo_race_pie_LI")),
                             tabPanel("High Income", plotlyOutput("demo_race_pie_HI"))),
                      
                      tabBox(title = "Marital status", width = 6,
                             tabPanel("Total",       plotlyOutput("demo_Marital_pie")),
                             tabPanel("Low Income",  plotlyOutput("demo_Marital_pie_LI")),
                             tabPanel("High Income", plotlyOutput("demo_Marital_pie_HI"))))
          
      ),

            

      tabItem(tabName = "EduAgesTab",
           h1 ("Income compared to Education and Ages", align = "center"),
           
           fluidPage(
               valueBox(3, "Income Statuses", width = 4, icon = icon("user"), color = "yellow"),
               valueBox(7, "Education Categories", width = 4, icon = icon("user"), color = "orange"),
               valueBox(9, "Ages Categories", width = 4, icon = icon("user"), color = "light-blue")
           ),
             
           fluidPage(h2("Social Status", align = 'center'),
               box(sund2bOutput("income_edu"), width = 12, title = 'Income and Education')
           ),
      
           fluidPage(h2("Blood Presure Compare",align = "center"),
               #h4("How would you descrive your current physical health?"),
               box(plotOutput("health_likert"), width = 12, title = 'Compare Initial vs. Last, Minor Changes')
           )
           
      ),
      
      
      tabItem(tabName = "HeightWeightTab",
          h1 ("Height and Weight", align = "center"),
          fluidPage(
              box(plotOutput("height_weight"), width = 12, title = 'Geometric Smooth Method')
          )
      ),
      

            
      tabItem(tabName = "CorrelationsTab",
              h1 ("Numeric Variable Correlation", align = "center"),
              fluidPage(
                fluidRow(
                  box(plotOutput("corr_numeric"), width = 12, title = 'Correlations')
                ),
                fluidRow(
                  box(title = "Variables Considered:",
                    width = 12, collapsible = TRUE, collapsed = FALSE,
                    #background = "aqua", # light-blue
                    HTML("<TABLE>"),
                    HTML("<tr><td>RIDRETH3</td><td>Race/Hispanic origin w/ NH Asian</td></tr>"),
                    HTML("<tr><td>EDUCCODE</td><td>Education level</td></tr>"),
                    HTML("<tr><td>DMDHHSIZ</td><td>Total number of people in the Household</td></tr>"),
                    HTML("<tr><td>DMDMARTL</td><td>Marital status</td></tr>"),
                    HTML("<tr><td>BPXSY1 </td><td>Systolic: Blood pres (1st rdg) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXSY3 </td><td>Systolic: Blood pres (third reading) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXDI1 </td><td>Diastolic: Blood pres (1st rdg) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXDI3 </td><td>Diastolic: Blood pressure (third reading) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXPLS</td><td>60 sec. pulse (30 sec. pulse * 2)</td></tr>"),
                    HTML("<tr><td>BPXPTY</td><td>Pulse type</td></tr>"),
                    HTML("<tr><td>BPXPULS</td><td>Pulse regular or irregular?</td></tr>"),
                    HTML("<tr><td>PEASCST1</td><td>Blood Pressure Status</td></tr>"),
                    HTML("<tr><td>PEASCTM1</td><td>Blood Pressure Time in Seconds</td></tr>"),
                    HTML("<tr><td>BMXWAIST</td><td>Waist Circumference (cm)</td></tr>"),
                    HTML("<tr><td>BMXWT</td><td>Weight (kg)</td></tr>"),
                    HTML("<tr><td>LBDBANO</td><td>Basophils number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDEONO</td><td>Eosinophils number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDLYMNO</td><td>Lymphocyte number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDMONO</td><td>Monocyte number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDNENO</td><td>Segmented neutrophils num (1000 cell/uL)</td></tr>"),
                    HTML("<tr><td>LBXBAPCT</td><td>Basophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXEOPCT</td><td>Eosinophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXHCT</td><td>Hematocrit (%)</td></tr>"),
                    HTML("<tr><td>LBXHGB</td><td>Hemoglobin (g/dL)</td></tr>"),
                    HTML("<tr><td>LBXLYPCT</td><td>Lymphocyte percent (%)</td></tr>"),
                    HTML("<tr><td>LBXMC</td><td>Mean cell hemoglobin concentration (g/dL)</td></tr>"),
                    HTML("<tr><td>LBXMCHSI</td><td>Mean cell hemoglobin (pg)</td></tr>"),
                    HTML("<tr><td>LBXMCVSI</td><td>Mean cell volume (fL)</td></tr>"),
                    HTML("<tr><td>LBXMOPCT</td><td>Monocyte percent (%)</td></tr>"),
                    HTML("<tr><td>LBXMPSI</td><td>Mean platelet volume (fL)</td></tr>"),
                    HTML("<tr><td>LBXNEPCT</td><td>Segmented neutrophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXPLTSI</td><td>Platelet count (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBXRBCSI</td><td>Red blood cell count (million cells/uL)</td></tr>"),
                    HTML("<tr><td>LBXRDW</td><td>Red cell distribution width (%)</td></tr>"),
                    HTML("<tr><td>LBXWBCSI</td><td>White blood cell count (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDHDD</td><td>Direct HDL-Cholesterol (mg/dL)</td></tr>"),
                    HTML("<tr><td>LBDHDDSI</td><td>Direct HDL-Cholesterol (mmol/L)</td></tr>"),
                    HTML("<tr><td>LBDTCSI</td><td>Total Cholesterol( mmol/L)</td></tr>"),
                    HTML("<tr><td>LBXTC</td><td>Total Cholesterol( mg/dL)</td></tr>"),
                    HTML("<tr><td>LBXHA</td><td>Hepatitis A antibody</td></tr>"),
                    HTML("<tr><td>LBXHBS</td><td>Hepatitis B Surface Antibody</td></tr>"),
                    HTML("<tr><td>LBDHBG</td><td>Hepatitis B surface antigen</td></tr>"),
                    HTML("<tr><td>LBDHD</td><td>Hepatitis D (anti-HDV)</td></tr>"),
                    HTML("<tr><td>LBXHBC</td><td>Hepatitis B core antibody</td></tr>"),
                    HTML("</TABLE>")
                    
                ) # end box
              ) # end fluidRow
           )# end fluidPage
      ), # end Tabitem
      

      
      tabItem(tabName = "OutliersTab",
              fluidPage(
                h1("Data science", align= "center") #,
                #h3("Outliers Analysis using BoxPlot")
              ),
              
              
              fluidPage(
                headerPanel('Outliers Analysis using BoxPlot'),
                
                sidebarPanel(
                  selectInput('outlierCol', 'Variable to Analyize', names(myData_Num)[1:11], # allColumns,
                              selected = names(myData_Num)[[3]] ),           # allColumns[5]
                ),
                
                # output functions
                mainPanel(
                  plotOutput('plotOutlier1'),  #ID, will be referenced as output$plot1
                )
              ),
              
              fluidPage(
                  verbatimTextOutput('textOutlier')
              )
              
      ),
      
      
            
      
      
      tabItem(tabName = "ClusterTab",
          fluidPage(
            h1("Data science", align= "center"),
            h3("Cluster Analysis")
          ),
                     

          fluidPage(
            headerPanel('K-means Cluster Analysis'),
            
            sidebarPanel( width=3,
              selectInput('xcol', 'X Variable', names(myData_Num)[1:11], # allColumns,
                          selected = names(myData_Num)[[3]] ),           # allColumns[5]
              selectInput('ycol', 'Y Variable', names(myData_Num)[1:11],
                          selected = names(myData_Num)[[11]] ),       # 
              
              sliderInput('numclusters', 'Cluster count', ticks = FALSE,
                          min = 1, max = 9, value = 5, step = 1, round = TRUE),
              
              #numericInput('numclusters', 'Cluster count', 5,
              #             min = 1, max = 9)  # input numeric textbox input$clusters
            ),

            # output functions
            mainPanel( width=9,
              box(title = "Clusters", width = 12,
                  plotOutput('plotClust1')
                  ,collapsible = FALSE, collapsed = FALSE)
              #plotOutput('plotClust1'),  #ID, will be referenced as output$plot1
            )
          ),

          fluidPage(
            fluidRow(
              box( width = 12, collapsible = TRUE, collapsed = FALSE,
                htmlOutput('myHtmlOutId', inline=FALSE)
              )
            ) 
          )
          
      ),
      

      
      tabItem(tabName = "LogisticTab",

              fluidPage(
                headerPanel('Logistic Regression, Predict Is_Diabetic'),
                h4("  Please wait few seconds after clicking button", .noWS="before"),
                box(title = "Variables Considered:",
                    width = 6, collapsible = TRUE, collapsed = TRUE,
                    #background = "aqua", # light-blue
                    HTML("<TABLE>"),
                    HTML("<tr><td>RIDRETH3</td><td>Race/Hispanic origin w/ NH Asian</td></tr>"),
                    HTML("<tr><td>EDUCCODE</td><td>Education level</td></tr>"),
                    HTML("<tr><td>DMDHHSIZ</td><td>Total number of people in the Household</td></tr>"),
                    HTML("<tr><td>DMDMARTL</td><td>Marital status</td></tr>"),
                    HTML("<tr><td>BPXSY1 </td><td>Systolic: Blood pres (1st rdg) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXSY3 </td><td>Systolic: Blood pres (third reading) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXDI1 </td><td>Diastolic: Blood pres (1st rdg) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXDI3 </td><td>Diastolic: Blood pressure (third reading) mm Hg</td></tr>"),
                    HTML("<tr><td>BPXPLS</td><td>60 sec. pulse (30 sec. pulse * 2)</td></tr>"),
                    HTML("<tr><td>BPXPTY</td><td>Pulse type</td></tr>"),
                    HTML("<tr><td>BPXPULS</td><td>Pulse regular or irregular?</td></tr>"),
                    HTML("<tr><td>PEASCST1</td><td>Blood Pressure Status</td></tr>"),
                    HTML("<tr><td>PEASCTM1</td><td>Blood Pressure Time in Seconds</td></tr>"),
                    HTML("<tr><td>BMXWAIST</td><td>Waist Circumference (cm)</td></tr>"),
                    HTML("<tr><td>BMXWT</td><td>Weight (kg)</td></tr>"),
                    HTML("<tr><td>LBDBANO</td><td>Basophils number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDEONO</td><td>Eosinophils number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDLYMNO</td><td>Lymphocyte number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDMONO</td><td>Monocyte number (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDNENO</td><td>Segmented neutrophils num (1000 cell/uL)</td></tr>"),
                    HTML("<tr><td>LBXBAPCT</td><td>Basophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXEOPCT</td><td>Eosinophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXHCT</td><td>Hematocrit (%)</td></tr>"),
                    HTML("<tr><td>LBXHGB</td><td>Hemoglobin (g/dL)</td></tr>"),
                    HTML("<tr><td>LBXLYPCT</td><td>Lymphocyte percent (%)</td></tr>"),
                    HTML("<tr><td>LBXMC</td><td>Mean cell hemoglobin concentration (g/dL)</td></tr>"),
                    HTML("<tr><td>LBXMCHSI</td><td>Mean cell hemoglobin (pg)</td></tr>"),
                    HTML("<tr><td>LBXMCVSI</td><td>Mean cell volume (fL)</td></tr>"),
                    HTML("<tr><td>LBXMOPCT</td><td>Monocyte percent (%)</td></tr>"),
                    HTML("<tr><td>LBXMPSI</td><td>Mean platelet volume (fL)</td></tr>"),
                    HTML("<tr><td>LBXNEPCT</td><td>Segmented neutrophils percent (%)</td></tr>"),
                    HTML("<tr><td>LBXPLTSI</td><td>Platelet count (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBXRBCSI</td><td>Red blood cell count (million cells/uL)</td></tr>"),
                    HTML("<tr><td>LBXRDW</td><td>Red cell distribution width (%)</td></tr>"),
                    HTML("<tr><td>LBXWBCSI</td><td>White blood cell count (1000 cells/uL)</td></tr>"),
                    HTML("<tr><td>LBDHDD</td><td>Direct HDL-Cholesterol (mg/dL)</td></tr>"),
                    HTML("<tr><td>LBDHDDSI</td><td>Direct HDL-Cholesterol (mmol/L)</td></tr>"),
                    HTML("<tr><td>LBDTCSI</td><td>Total Cholesterol( mmol/L)</td></tr>"),
                    HTML("<tr><td>LBXTC</td><td>Total Cholesterol( mg/dL)</td></tr>"),
                    HTML("<tr><td>LBXHA</td><td>Hepatitis A antibody</td></tr>"),
                    HTML("<tr><td>LBXHBS</td><td>Hepatitis B Surface Antibody</td></tr>"),
                    HTML("<tr><td>LBDHBG</td><td>Hepatitis B surface antigen</td></tr>"),
                    HTML("<tr><td>LBDHD</td><td>Hepatitis D (anti-HDV)</td></tr>"),
                    HTML("<tr><td>LBXHBC</td><td>Hepatitis B core antibody</td></tr>"),
                    HTML("</TABLE>")
                    
                ) # end box
              ),
              
              fluidPage(
                
                sidebarPanel(width=3,
                  checkboxGroupInput('cbgID1','Variables to Include', 
                                     c("Age Years" = "Age_Years",
                                       "Education" = "DMDEDUC2",
                                       "BMI" = "BMI",
                                       "Race" = "Race"),
                                     
                                     selected = c('Age_Years',"DMDEDUC2","BMI")), #  selected='Age_Years'
                  actionButton(inputId="myBtnLog", label="Logistic Regression") #click 33/44 tutorial
                
                ),
                
                # output functions
                mainPanel( width=9,
                  box(title = "Table Output, first records", width = 12,
                      tableOutput('tblDataID')
                      ,collapsible = FALSE, collapsed = FALSE)
                )
              ),
              
              fluidPage(
                fluidRow(
                   box( width = 6, title="Model Accuracy",
                      verbatimTextOutput('textAccuracy')
                   ),
                   box( width = 6, title = "Model AUC",
                      verbatimTextOutput('textAUC')
                   ),
                 
                ),
                 
                fluidRow(
                  box( width = 7, title="Model Summary",
                       verbatimTextOutput('textSummary')
                  ),
                  box( width = 5, title = "Anova, Analysis of Variance",
                       selectInput("siLR1", "Anova Test:",
                                   c("CHI Square" = "Chisq",
                                     "Likelihood Ratio Test" = "LRT",
                                     "Rao's (Lagrange) Score" = "Rao"),
                                   selected = "20"),
                       
                       verbatimTextOutput('textAnova')
                  )
                ),

                
                fluidRow(
                  box( width = 5, title="Confusion Matrix",
                       verbatimTextOutput('textConfMatrix')
                  ),
                  box( width=7, title = "plot AUC",
                       plotOutput("plotAUC")
                  )
                )
                
              )
              
      ),   # end Tab Item Logistic Regression
      

      
      
      
      
      
      
      
      
      
      
      #-------------------------------------------------------------------------------------------------
      # Random Forest
      #-------------------------------------------------------------------------------------------------
            
      tabItem(tabName = "RandForestTab",
          fluidPage(
                headerPanel('Random Forest, predict Is_Diabetic'),
                h4("  Please Note: Random Forest takes about 5 minutes, for 20 Trees", .noWS="before")
          ),

          fluidPage(
            
            sidebarPanel(width=3,
                         checkboxGroupInput('cbgRF1','Variables to Include', 
                                            c("Age Years" = "Age_Years",
                                              "Education" = "DMDEDUC2",
                                              "BMI" = "BMI",
                                              "Race" = "Race"),
                                            selected = c('Age_Years',"DMDEDUC2","BMI")), #  selected='Age_Years'
                         
                         selectInput("siRF1", "Max Number of Trees:",
                                     c("20" = 20,
                                       "30" = 30,
                                       "40" = 40,
                                       "50" = 50),
                                     selected = 20),
                         
                         actionButton(inputId="myBtnRF", label="Random Forest") #click 33/44 tutorial
                         
            ),
            
            # output functions
            mainPanel( width=9,
                       box(title = "Table Output, first records", width = 12,
                           tableOutput('tblDataRF')
                           ,collapsible = FALSE, collapsed = FALSE)
            )
          ),
          
          fluidPage(
            fluidRow(
              box( width = 7, title="Random Forest Output",
                   verbatimTextOutput('textOutputRF')
              ),
              box( width = 5, title = "Random Forest Summary",
                   verbatimTextOutput('textSummaryRF')
              )
              
            ),
            fluidRow(
              box( width=7, title = "plot Random Forest",
                   plotOutput("plotRF")
              ),
              box( width = 5, title="Trees Results",
                   verbatimTextOutput('textResultsRF')
              )
            ),
            fluidRow(
              box( width = 6, title="Best Tree",
                   verbatimTextOutput('textBestTreeRF')
              ),
              box( width = 6, title = "Future Use",
                   verbatimTextOutput('textFutureRF')
              ),
              
            )
            
            
          )
              

              
      ) # end TabItem Random Forest

))))
