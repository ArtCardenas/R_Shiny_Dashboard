# https://acarden6.shinyapps.io/Dashboard_NHANES/


library(shiny)
library(plyr)
library(plotly)
library(jpeg)
library(sunburstR)  # for the pie sun burst chart
library(likert)     # for the percentage compare horizontal percent
library(ggplot2)    # ggPlot
library(corrplot)   # correlations



# Logistic Regression libraries
library(car)
library(mlogit)
library(ROCR)
#library(caret)
#library(e1071)

# Random Forest
library(randomForest)
library(caret)
library(e1071)
library(doParallel)


#library(readxl)
#library(xtable)
#library(grid)
#library(d3r)
#library(formattable)

# https://www.r-bloggers.com/load-save-and-rda-files/

######################Importing Data Frames into Project Files################################################# 
###Dataset

myData <- readRDS("data/finalDF.Rda")  #getwd()     File must be in /data
myDataLog <- myData  # make a copy for logistic regression

#--------------------------------------------------------------------------------
# remove Bias variables - as of today Gender is Bias, 
myDataLog$Gender  <- NULL  # remove gender class
myDataLog$RIAGENDR <- NULL # remove gender number
myDataLog$DMDMARTL <- NULL # remove, because not evenly split:  train and test
myDataLog$Marital <- NULL  # remove, because not evenly split:  train and test

# remove variables with very low MDA - Mean Decrease Accuracy
myDataLog$EDUCCODE <- NULL    # somehow is NA
myDataLog$incomeType <- NULL   # somehow is NA
myDataLog$LBXHBC <- NULL
myDataLog$LBDHBG <- NULL
myDataLog$LBDHD <- NULL
myDataLog$LBXHA <- NULL
myDataLog$LBXHBS <- NULL

# Remove High P value > .5
myDataLog$Income_Year <- NULL
myDataLog$DMDHHSIZ <- NULL
myDataLog$LBDEONO  <- NULL
myDataLog$LBDLYMNO <- NULL
myDataLog$LBDNENO  <- NULL
myDataLog$LBXBAPCT <- NULL
myDataLog$LBXEOPCT <- NULL
myDataLog$LBXHCT   <- NULL
myDataLog$LBXLYPCT <- NULL
myDataLog$LBXNEPCT <- NULL
myDataLog$LBXRBCSI <- NULL
myDataLog$LBXRDW   <- NULL
myDataLog$LBXWBCSI <- NULL
myDataLog$LBDHDD   <- NULL
myDataLog$LBDHDDSI <- NULL
myDataLog$LBDTCSI  <- NULL
myDataLog$LBXTC    <- NULL




###Title Page: Conceptual Figure:
img <- readJPEG("data/PicIntro_800x605.jpg", native = TRUE)  # file must be in /data,  check getwd() 

###General Descriptive Data: Resident Demographics:
data_frame_incomeLvl  <- readRDS("data/finalDF_incomeLvl.Rda")
data_frame_gender     <- readRDS("data/finalDF_gender.Rda")
data_frame_gender_LI  <- readRDS("data/finalDF_gender_LI.Rda")
data_frame_gender_HI  <- readRDS("data/finalDF_gender_HI.Rda")


data_frame_race       <- readRDS("data/finalDF_race.Rda")
data_frame_race_LI    <- readRDS("data/finalDF_race_LI.Rda")
data_frame_race_HI    <- readRDS("data/finalDF_race_HI.Rda")

data_frame_Marital    <- readRDS("data/finalDF_Marital.Rda")
data_frame_Marital_LI <- readRDS("data/finalDF_Marital_LI.Rda")
data_frame_Marital_HI <- readRDS("data/finalDF_Marital_HI.Rda")

data_frame_startburst <- readRDS("data/finalDF_starburst.Rda")

likert_frame1b <- readRDS("data/likert_frame1b.Rda")

myData_Num <- readRDS("data/mydata_numeric.Rda")


########################### Creating Plot Outputs for Server##############################################
###colors###

#colors <- c('rgb(52, 152, 219)', 'rgb(231, 76, 60)', 'rgb(241, 196, 15)', 'rgb(26, 188, 156)')

colors_tf <- c('rgb(52, 152, 219)', 'rgb (241, 196, 15)')

# colors_red_yellow <- c('rgb(231, 76, 60)', 'rgb(241, 196, 15)')
# 
# colors_yellow_green_lightblue <- c('rgb(141,198,63)','rgb(153,155,158)','rgb(255, 210, 4)')
# colors_1 <- c('rgb(255, 210, 4)','rgb(141,198,63)','rgb(153,155,158)')
# 
# colors_teal_red_yellow <- c("rgb(26, 188, 156)", "rgb(231, 76, 60)", "rgb(241, 196, 15)")
# 
# colors_grey_yellow <- c("rgb(153,155,158)","rgb(241, 196, 15)")
# 
# colors_grey_lightblue <- c("rgb(153,155,158)","rgb(173,223,231)")
# 
# colors_grey_pink_teal <- c("rgb(150,107,157)", "rgb(153,155,158)", "rgb(201,134,134)" )
# 
# colors_education <- c('rgb(153,155,158)', "rgb(241, 196, 15)", "rgb(173,223,231)",'rgb(201,134,134)',"rgb(150,107,157)")
# 
# colors_education_purple_blue <- c("rgb(173,223,231)",'rgb(150,107,157)')
# 
# colors_education_purple_blue_yellow <- c("rgb(241, 196, 15)","rgb(173,223,231)",'rgb(150,107,157)')






shinyServer(function(input,output){
  
##############################Title Page: Conceptual Figure
  
  output$Image_Concept <- renderImage({
    return(list(
      src = "data/PicIntro_800x605.jpg",
      contentType = "image/jpeg",
      width = 800,
      height = 605,
      alt = "Face"
    ))
  }, deleteFile = FALSE)
  
 
##############################General Descriptive Data: Resident Demographics


    

# Gender: Total
  output$demo_gender_pie <- renderPlotly({
    plot_ly(data_frame_gender, labels = ~gender, values =~freq_gender, 
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker=list(colors = colors_tf,line=list(color = '#FFFFFF', width=1)))%>%
      add_pie(hole = 0.4) %>%
      layout(showlegend = T,
             xaxis = list(title=''),
             yaxis = list(title=''))})

# Gender: RW
  output$demo_gender_pie_LI <- renderPlotly({
    plot_ly(data_frame_gender_LI, labels = ~gender, values =~freq_gender, 
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker=list(colors = colors_tf,line=list(color = '#FFFFFF', width=1)))%>%
      add_pie(hole = 0.4) %>%
      layout(showlegend = T,
             xaxis = list(title=''),
             yaxis = list(title=''))})

# Gender: LR
  output$demo_gender_pie_HI <- renderPlotly({
    plot_ly(data_frame_gender_HI, labels = ~gender, values =~freq_gender, 
    textposition = 'inside',
    textinfo = 'value+label+percent',
    insidetextfont = list(color = '#FFFFFF'),
    marker=list(colors = colors_tf,line=list(color = '#FFFFFF', width=1)))%>%
    add_pie(hole = 0.4) %>%
    layout(showlegend = T,
           xaxis = list(title=''),
           yaxis = list(title=''))})

#Age 
  output$age_histogram <- renderPlotly({
    plot_ly(x = myData$AgeBracket, type = "histogram")}) # RIDAGEYR


  


# Race: Total
  output$demo_race_pie <- renderPlotly({
    plot_ly(data_frame_race, labels=~race, values=~freq_race, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})

# Race: Low Income
  output$demo_race_pie_LI <- renderPlotly({
    plot_ly(data_frame_race_LI, labels=~race, values=~freq_race, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})

# Race: High Income
  output$demo_race_pie_HI <- renderPlotly({
    plot_ly(data_frame_race_HI, labels=~race, values=~freq_race, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})

  
  


# Marital: Total
  output$demo_Marital_pie <- renderPlotly({
    plot_ly(data_frame_Marital, labels=~Marital, values=~freq_Marital, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})

# Marital: Low Income
  output$demo_Marital_pie_LI <- renderPlotly({
    plot_ly(data_frame_Marital_LI, labels=~Marital, values=~freq_Marital, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})

# Marital: High Income
  output$demo_Marital_pie_HI <- renderPlotly({
    plot_ly(data_frame_Marital_HI, labels=~Marital, values=~freq_Marital, type = 'pie',
            textposition = 'inside',
            textinfo = 'value+label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            outsidetextfont = list(color = 'rgb(26, 188, 156)'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)))%>%
      layout(showlegend = T)})


    
# Education and Income
   output$income_edu <- renderSund2b({
     sund2b(data_frame_startburst)})
   
   
# Self-report physical health, Likert- Baseline & 6 Months followup
   output$health_likert <- renderPlot({
     plot(likert_frame1b, plot.percents = TRUE, text.size = 5)})
   


      
# Correlations
   mydata_correlations <- cor(myData_Num)
   
   output$corr_numeric <- renderPlot({
     corrplot(mydata_correlations, method = "circle", tl.cex=.8, win.asp=.7) #plot matrix
   })
   
   
   
      
# Weight vs Height   
   output$height_weight <- renderPlot({
      ggplot(myData, aes(Weight_Kg, Height_Cm)) + 
         geom_point(pch = 21, size = 1, fill = rgb(0.2,0.8,0.6,0.8)) + 
         geom_smooth(method = "auto", color = "red", se = FALSE) + 
         scale_x_continuous("Weight (Kg)", breaks = seq(0,225,15)) + 
         scale_y_continuous("Standing Height (cm)", breaks = seq(79,210,by=10)) +
         labs(title="Scatterplot of Weight vs. Height") + theme_bw()
   })
  
  

# Outlier Analysis
   selectedOutlierCol <- reactive({
     myData_Num[, c(input$outlierCol)]
   })
   

   # creates output, must be a Render function, render a Plot
   output$plotOutlier1 <- renderPlot({
     boxplot(selectedOutlierCol())  # adultData$age
   })
   
   output$textOutlier <- renderPrint({  #renderPrint, renderText
     boxplot.stats(selectedOutlierCol()) # $stats, $n, $conf, $out
   })
   

   
   
   
   
# Cluster Analysis
   selectedData <- reactive({
     myData_Num[, c(input$xcol, input$ycol)]
   })
   
   clusters <- reactive({
     kmeans(selectedData(), input$numclusters)
   })
   
   # creates output, must be a Render function, render a Plot
   output$plotClust1 <- renderPlot({
     # mar = margins (bottom, left, top, right)
     # https://www.r-bloggers.com/setting-graph-margins-in-r-using-the-par-function-and-lots-of-cow-milk/
     #par(mar = c(5.1, 4.1, 0, 1))
     plot(selectedData(),
          col = clusters()$cluster,
          # http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
          # pch = 20,bullet (smaller circle)
          # pch = 4,cross
          # cex : the size of pch symbols
          pch = 20, cex = 3)
     points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
   })

   
   

   
   
   
   
      
   
#---------------------------------------------------------------------   
# logistic Regression
#---------------------------------------------------------------------
   
   
   #  Reactive func() is used when more than 1 output; doc tutorial 28/44
   myDataLogReactive<- reactive ({  
      myDataLog[ ,  # 
                 c("is_diabetic" ,"incomeLvl",
                   "AgeBracket",
                   "Weight_Kg",
                   "Height_Cm",
                   "Systolic_1",
                   "Systolic_3",
                   "Waist_Cm",
                   "Dyastolic_1",
                   "Dyastolic_3",
                   "Heart_Pulse",
                   "PEASCTM1",
                   "BPStage1",
                   "BPStage3",
                   "LBDBANO",
                   "LBDMONO",
                   "LBXHGB",
                   "LBXMC",
                   "LBXMCHSI",
                   "LBXMCVSI",
                   "LBXMOPCT",
                   "LBXMPSI",
                   "LBXPLTSI",
                   input$cbgID1), # columns
                 drop = FALSE]
   })

   
   myDataLogDisplay<- reactive ({  
      myDataLog[ 1:10,  # first 100 records
                 c(input$cbgID1), # columns
                 drop = FALSE]
   })

      
   output$tblDataID <- renderTable({
      myDataLogDisplay()
   }, rownames = TRUE)   
   
   
   # see observe() instead of observeEvent() , see page 34/44
   observeEvent(
      input$myBtnLog,    # input control
      {
         #print(as.numeric(input$myBtnLog))     # only for debugging

                  # create train and test
         # Method 1
         
         set.seed(100)
         trainIndex  <- sample (1:nrow(myDataLogReactive()), .8*nrow(myDataLogReactive()))
         
         train_data  <- myDataLogReactive()[trainIndex, ]
         test_data   <- myDataLogReactive()[-trainIndex, ]
         
         print('Before Logistic Regression')

         myModel.Log1 <- glm(is_diabetic ~ .   # ~ dot means all remaining variables
                             , data = train_data, family = binomial(link='logit')) #
         
         print('Model Ready')

         #summary(myModel.Log1)
         #print(myModel.Log1)
         #anova(myModel.Log1,test="Chisq")
         
         output$textSummary <- renderPrint({  #renderPrint, renderText
            summary(myModel.Log1) 
         })
   
         output$textAnova <- renderPrint({  #renderPrint, renderText
            anova(myModel.Log1,test=input$siLR1)   # "Chisq" default
         })
         
         print('Output values')
         
         
         fitted_prediction<-predict(myModel.Log1,     newdata=test_data         ,type='response')
         fitted_prediction.bin <- ifelse(fitted_prediction > 0.5,1,0)
         
         misClasificError <- mean(fitted_prediction.bin != myData$is_diabetic)
#         print(paste('Accuracy',1-misClasificError))

         output$textAccuracy <- renderPrint({  
            print(paste('Accuracy',1-misClasificError))
         })
         
         output$textConfMatrix <- renderPrint({  
            confusionMatrix(factor(fitted_prediction.bin), test_data$is_diabetic) 
         })
         

         
                  
         pr <- prediction(fitted_prediction, test_data$is_diabetic)
         prf <- performance(pr, measure = "tpr", x.measure = "fpr")
 #        plot(prf)
         
         output$plotAUC <- renderPlot({
            plot(prf)
         })
         
         auc <- performance(pr, measure = "auc")
         auc <- auc@y.values[[1]]
#         auc
         output$textAUC <- renderPrint({  
            auc
         })
         
         
      }
   )

   
   output$myHtmlOutId <-renderUI({
      HTML(paste( c("This Cluster Analysis based on K-Means Algorithm<br>","<br>")))
   })

   
   
   
   

   
   
   
   
      
   
#---------------------------------------------------------------------   
# Random Forest
#---------------------------------------------------------------------
   
   
   #  Reactive func() is used when more than 1 output; doc tutorial 28/44
   myDataRFReactive<- reactive ({  
      myData[ ,   
                 c("is_diabetic" ,
                   'RIAGENDR',
                   # 'Gender',      # 'RIAGENDR'  already
                   #'Age_Years',
                   #'DMDEDUC2',
                   #'BMI',
                   #'Race',
                   'DMDHHSIZ',
                   'Income_Year',
                   'incomeLvl',
                   # 'EDUCCODE',    # somehow is NA in Random Forest 
                   # 'incomeType',  # showhow is NA in Random Forest
                   # 'DMDMARTL',    # remove, because not evenly split:  train and test
                   # 'Marital',     # remove, because not evenly split:  train and test
                   'AgeBracket',
                   'Weight_Kg',
                   'Height_Cm',
                   'Systolic_1',
                   'Systolic_3',
                   'Waist_Cm',
                   'Dyastolic_1',
                   'Dyastolic_3',
                   'Heart_Pulse',
                   'PEASCTM1',
                   'BPStage1',
                   'BPStage3',
                   'LBDBANO',
                   'LBDEONO',
                   'LBDLYMNO',
                   'LBDMONO',
                   'LBDNENO',
                   'LBXBAPCT',
                   'LBXEOPCT',
                   'LBXHCT',
                   'LBXHGB',
                   'LBXLYPCT',
                   'LBXMC',
                   'LBXMCHSI',
                   'LBXMCVSI',
                   'LBXMOPCT',
                   'LBXMPSI',
                   'LBXNEPCT',
                   'LBXPLTSI',
                   'LBXRBCSI',
                   'LBXRDW',
                   'LBXWBCSI',
                   'LBDHDD',
                   'LBDHDDSI',
                   'LBDTCSI',
                   'LBXTC',
                   #'LBXHA',     # remove variables with very low MDA - Mean Decrease Accuracy
                   #'LBXHBS',    # remove variables with very low MDA - Mean Decrease Accuracy
                   #'LBDHBG',    # remove variables with very low MDA - Mean Decrease Accuracy
                   #'LBDHD',     # remove variables with very low MDA - Mean Decrease Accuracy
                   #'LBXHBC',    # remove variables with very low MDA - Mean Decrease Accuracy
                   
                   input$cbgRF1), # columns    #"Age_Years","DMDEDUC2","BMI","Race"
                 drop = FALSE]
   })


      
   
   myDataRFDisplay<- reactive ({  
      myData[ 1:10,  # first 100 records
                 c(input$cbgRF1), # columns
                 drop = FALSE]
   })
   
   output$tblDataRF <- renderTable({
      myDataRFDisplay()
   }, rownames = TRUE)   

      
   
   # see observe() instead of observeEvent() , see page 34/44
   observeEvent(
      input$myBtnRF,    # input control
      {
         #print(as.numeric(input$myBtnRF))   # only for debugging
         
         #--------------------------------------------------------------------------------
         # create train and test
         # Method 1
         
         set.seed(100)
         trainIndex  <- sample (1:nrow(myData), .8*nrow(myData))
         
         train_data  <- myData[trainIndex, ]
         test_data   <- myData[-trainIndex, ]
         
         
         
         
         
         print('Before Random Forest')
         
         
         # method ='rf' =random forest    1 to 14 predictors;  for each parameter run n=500 trees
         # create Model
         myData.rf3<-train(is_diabetic~.,data=train_data,method="rf",metric='Accuracy', #7:40:8:14
                           tuneGrid=expand.grid(.mtry=1:14),ntree=as.numeric(input$siRF1))

         print('Model Ready')
         
         
         # myData.rf3  
         
         #summary(myData.rf3)
         
         #print(myData.rf3)
         #plot(myData.rf3)
         
         output$textOutputRF <- renderPrint({  #renderPrint, renderText
            myData.rf3 
         })

         output$textSummaryRF <- renderPrint({  #renderPrint, renderText
            summary(myData.rf3)
         })
         
         output$plotRF <- renderPlot({
            plot(myData.rf3)
         })
         
         output$textResultsRF <- renderPrint({  #renderPrint, renderText
            myData.rf3$results
         })

         
                  
         best_mtry <- myData.rf3$bestTune$mtry  # will have best model number or 8 in this case

         output$textBestTreeRF <- renderPrint({  #renderPrint, renderText
            print(best_mtry )
         })
         
         

      }
   )
   
   
   
   
   
      
})
   
