# https://carlene-mayfield.shinyapps.io/BUF_Dashboard/
library(lazyeval)
library(colorspace)
library(ggplot2)
library(plotly)
library(plyr)
library(readxl)
library(writexl)
library(xtable)
library(likert)
library(grid)
library(reshape2)
library(lubridate)
library(pastecs)
library(dplyr)
library(tidyr)
library(sunburstR)
library(d3r)
library(stringr)
library(Hmisc)

################
###Import data from Excel
library(readxl)
## "C:/Users/Carlene/Documents/BUF Dashboard/BUF_participantdata_mock.xlsx")

data <- read_excel("C:/Users/Art/OneDrive/UNCC/Internship/Carlene/BUF_participantdata_mock.xlsx")
referrals <- read_excel("C:/Users/Art/OneDrive/UNCC/Internship/Carlene/BUF_referralsdata_mock.xlsx")

################
###Stratify by Community 
data_RW <- subset(data, community == 'RW')
data_LR <- subset(data, community == 'LR')

################
###Create data frame counts

###Enrollment and Baseline Demographics
#Community: Total
data_frame_community <- as.data.frame(table(data$community))
names(data_frame_community)[1] = 'community'
names(data_frame_community)[2] = 'freq_ecommunity'

#Gender: Total
data_frame_gender <- as.data.frame(table(data$gender))
names(data_frame_gender)[1] = 'gender'
names(data_frame_gender)[2] = 'freq_gender'

#Gender: RW
data_frame_gender_RW <- as.data.frame(table(data_RW$gender))
names(data_frame_gender_RW)[1] = 'gender'
names(data_frame_gender_RW)[2] = 'freq_gender'

#Gender: LR
data_frame_gender_LR <- as.data.frame(table(data_LR$race))
names(data_frame_gender_LR)[1] = 'gender'
names(data_frame_gender_LR)[2] = 'freq_gender'

#Race: Total
data_frame_race <- as.data.frame(table(data$race))
names(data_frame_race)[1] = 'race'
names(data_frame_race)[2] = 'freq_race'

#Race: RW
data_frame_race_RW <- as.data.frame(table(data_RW$race))
names(data_frame_race_RW)[1] = 'race'
names(data_frame_race_RW)[2] = 'freq_race'

#Race: LR
data_frame_race_LR <- as.data.frame(table(data_LR$race))
names(data_frame_race_LR)[1] = 'race'
names(data_frame_race_LR)[2] = 'freq_race'

#Ethnicity: Total
data_frame_ethnicity <- as.data.frame(table(data$ethnicity))
names(data_frame_ethnicity)[1] = 'ethnicity'
names(data_frame_ethnicity)[2] = 'freq_eth'

#Ethnicity: RW
data_frame_ethnicity_RW <- as.data.frame(table(data_RW$ethnicity))
names(data_frame_ethnicity_RW)[1] = 'ethnicity'
names(data_frame_ethnicity_RW)[2] = 'freq_eth'

#Ethnicity: LR
data_frame_ethnicity_LR <- as.data.frame(table(data_LR$ethnicity))
names(data_frame_ethnicity_LR)[1] = 'ethnicity'
names(data_frame_ethnicity_LR)[2] = 'freq_eth'

#Summary of Age
summary(data$age)
summary(data_RW$age)
summary(data_LR$age)

#Status: Total
data_frame_status <- as.data.frame(table(data$status))
names(data_frame_status)[1] = 'status'
names(data_frame_status)[2] = 'freq_status'

#Status: RW
data_frame_status_RW <- as.data.frame(table(data_RW$status))
names(data_frame_status_RW)[1] = 'status'
names(data_frame_status_RW)[2] = 'freq_status'

#Status: LR
data_frame_status_LR <- as.data.frame(table(data_LR$status))
names(data_frame_status_LR)[1] = 'status'
names(data_frame_status_LR)[2] = 'freq_status'

###Risk and Referrals
#Risk: Total
data_frame_risk <- as.data.frame(table(data$risk))
names(data_frame_risk)[1] = 'risk'
names(data_frame_risk)[2] = 'freq_risk'

#Risk: RW
data_frame_risk_RW <- as.data.frame(table(data_RW$risk))
names(data_frame_risk_RW)[1] = 'risk'
names(data_frame_risk_RW)[2] = 'freq_risk'

#Risk: LR
data_frame_risk_LR <- as.data.frame(table(data_LR$risk))
names(data_frame_risk_LR)[1] = 'risk'
names(data_frame_risk_LR)[2] = 'freq_risk'

data_frame_RE <- as.data.frame(table(data$status, data$risk))
data_frame_RE_wide <- spread(data_frame_RE, Var2, Freq)

#Referrals 
referral_frame <- as.data.frame(table(referrals$referral,referrals$referral_main,referrals$referral_sub))
names(referral_frame)[1] = 'referral'
names(referral_frame)[2] = 'referral_main'
names(referral_frame)[2] = 'referral_sub'
referral_frame_starburst <- referral_frame%>%unite(referral, -Freq, sep = "-")

#Referrals by Month
month_2019 <- c('January', 'February', 'March', 'April', 'May', 'June', 'July')
Community_2019 <-c(18, 5, 9, 5, 38, 40, 6)      
Workshop_2019 <- c(6, 14, 12, 22, 3, 12, 10)     
All_2019 <-     c(24, 19, 21, 27, 41, 52, 16)       

plot_2019 <- data.frame(month_2019, Community_2019, Workshop_2019, All_2019) 
plot_2019$month_2019 <- factor(plot_2019$month_2019, levels = month_2019)

###Key Drivers: Driver #2 Life Navigators
#Short Term Goals 
short <- as.data.frame(table(data$goals_short,data$goals_short_type))
names(short)[1] = 'goals_short'
names(short)[2] = 'goals_short_type'
short_frame_starburst <- short%>%unite(goals_short, -Freq, sep = "-")

#Long Term Goals 
long <- as.data.frame(table(data$goals_long,data$goals_long_type))
names(long)[1] = 'goals_long'
names(long)[2] = 'goals_long_type'
long_frame_starburst <- long%>%unite(goals_long, -Freq, sep = "-")

###Employment and Education
#Employment_1: Total
data_frame_employment_1 <- as.data.frame(table(data$employment_1))
names(data_frame_employment_1)[1] = 'employment'
names(data_frame_employment_1)[2] = 'freq_employment'

#Employment_1: RW
data_frame_employment_1_RW <- as.data.frame(table(data_RW$employment_1))
names(data_frame_employment_1_RW)[1] = 'employment'
names(data_frame_employment_1_RW)[2] = 'freq_employment'

#Employment_1: LR
data_frame_employment_1_LR <- as.data.frame(table(data_LR$employment_1))
names(data_frame_employment_1_LR)[1] = 'employment'
names(data_frame_employment_1_LR)[2] = 'freq_employment'

#Employment_2
data_frame_employment_2 <- as.data.frame(table(data$employment_2))
names(data_frame_employment_2)[1] = 'employment'
names(data_frame_employment_2)[2] = 'freq_employment'

#Education_1: Total
data_frame_education_1 <- as.data.frame(table(data$education_1))
names(data_frame_education_1)[1] = 'education'
names(data_frame_education_1)[2] = 'freq_education'

#Education_1: RW
data_frame_education_1_RW <- as.data.frame(table(data_RW$education_1))
names(data_frame_education_1_RW)[1] = 'education'
names(data_frame_education_1_RW)[2] = 'freq_education'

#Education_1: LR
data_frame_education_1_LR <- as.data.frame(table(data_LR$education_1))
names(data_frame_education_1_LR)[1] = 'education'
names(data_frame_education_1_LR)[2] = 'freq_education'

#Education_2
data_frame_education_2 <- as.data.frame(table(data$education_2))
names(data_frame_education_2)[1] = 'education'
names(data_frame_education_2)[2] = 'freq_education'

###Health and Wellness
#insurance_1: Total
data_frame_insurance_1 <- as.data.frame(table(data$insurance_1))
names(data_frame_insurance_1)[1] = 'insurance'
names(data_frame_insurance_1)[2] = 'freq_insurance'

#insurance_1: RW
data_frame_insurance_1_RW <- as.data.frame(table(data_RW$insurance_1))
names(data_frame_insurance_1_RW)[1] = 'insurance'
names(data_frame_insurance_1_RW)[2] = 'freq_insurance'

#insurance_1: LR
data_frame_insurance_1_LR <- as.data.frame(table(data_LR$insurance_1))
names(data_frame_insurance_1_LR)[1] = 'insurance'
names(data_frame_insurance_1_LR)[2] = 'freq_insurance'

#insurance_2
data_frame_insurance_2 <- as.data.frame(table(data$insurance_2))
names(data_frame_insurance_2)[1] = 'insurance'
names(data_frame_insurance_2)[2] = 'freq_insurance'

#Self Report Physical Health: Likert Scale
likert_frame1 <- data[c('health_1', 'health_2')]
level <- c('very poor', 'poor', 'fair', 'good', 'very good')
likert_frame1$'health_1' <- factor(likert_frame1$'health_1', levels = level)
likert_frame1$'health_2' <- factor(likert_frame1$'health_2', levels = level)
names(likert_frame1) <- c('health_1'= 'Baseline',
                          'health_2'= '6-Months')

likert_frame1b <- likert(as.data.frame(likert_frame1))

###Trends
employment_trends <- merge(data_frame_employment_1, data_frame_employment_2, by = 'employment')
names(employment_trends)[2] = 'Baseline'
names(employment_trends)[3] = '6-Months'

education_trends <- merge(data_frame_education_1, data_frame_education_2, by = 'education')
names(education_trends)[2] = 'Baseline'
names(education_trends)[3] = '6-Months'

insurance_trends <- merge(data_frame_insurance_1, data_frame_insurance_2, by = 'insurance')
names(insurance_trends)[2] = 'Baseline'
names(insurance_trends)[3] = '6-Months'


####################Save files as R data frames (.Rda files) in the project folder) #######################
#save dataset to project folder
# Old Folder  "C:/Users/Carlene/Documents/BUF Dashboard/.Rproj.user/Final/data/data.Rda"
# New Folder  "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data.Rda"
saveRDS(data, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data.Rda")

#save data frames with counts to project folder
saveRDS(data_frame_community, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_community.Rda")
saveRDS(data_frame_gender, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_gender.Rda")
saveRDS(data_frame_gender_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_gender_RW.Rda")
saveRDS(data_frame_gender_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_gender_LR.Rda")
saveRDS(data_frame_race, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_race.Rda")
saveRDS(data_frame_race_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_race_RW.Rda")
saveRDS(data_frame_race_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_race_LR.Rda")
saveRDS(data_frame_ethnicity, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_ethnicity.Rda")
saveRDS(data_frame_ethnicity_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_ethnicity_RW.Rda")
saveRDS(data_frame_ethnicity_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_ethnicity_LR.Rda")
saveRDS(data_frame_status, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_status.Rda")
saveRDS(data_frame_status_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_status_RW.Rda")
saveRDS(data_frame_status_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_status_LR.Rda")

saveRDS(data_frame_risk, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_risk.Rda")
saveRDS(data_frame_risk_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_risk_RW.Rda")
saveRDS(data_frame_risk_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_risk_LR.Rda")
saveRDS(data_frame_RE_wide, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_RE_wide.Rda")

saveRDS(referral_frame_starburst, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/referral_frame_starburst.Rda")
saveRDS(plot_2019, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/plot_2019.Rda")

saveRDS(short_frame_starburst, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/short_frame_starburst.Rda")
saveRDS(long_frame_starburst, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/long_frame_starburst.Rda")

saveRDS(data_frame_employment_1, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_employment_1.Rda")
saveRDS(data_frame_employment_1_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_employment_1_RW.Rda")
saveRDS(data_frame_employment_1_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_employment_1_LR.Rda")
saveRDS(data_frame_employment_2, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_employment_2.Rda")
saveRDS(data_frame_education_1, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_education_1.Rda")
saveRDS(data_frame_education_1_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_education_1_RW.Rda")
saveRDS(data_frame_education_1_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_education_1_LR.Rda")
saveRDS(data_frame_education_2, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_education_2.Rda")

saveRDS(data_frame_insurance_1, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_insurance_1.Rda")
saveRDS(data_frame_insurance_1_RW, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_insurance_1_RW.Rda")
saveRDS(data_frame_insurance_1_LR, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_insurance_1_LR.Rda")
saveRDS(data_frame_insurance_2, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/data_frame_insurance_2.Rda")
saveRDS(likert_frame1b, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/likert_frame1b.Rda")

saveRDS(employment_trends, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/employment_trends.Rda")
saveRDS(education_trends, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/education_trends.Rda")
saveRDS(insurance_trends, file = "C:/Users/Art/OneDrive/UNCC/Internship/Carlene/data/insurance_trends.Rda")

