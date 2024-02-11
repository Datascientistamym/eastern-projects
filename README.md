#Title: "Final Project"
#Amy Moschos 07-11-2023
#Amy.moschos@Eastern.edu
#2023 Summer 2 Term


rm(list = ls())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(lm.beta))

#setwd("/Users/amymoschos/Desktop/R Code")
brfs<-read_csv("BRFSS2015_650.csv",show_col_types=FALSE)

#create a data frame selecting and filtering pertinent data.
mental<-brfs%>%
  filter((MENTHLTH>=1 & MENTHLTH<=30)|MENTHLTH==88)
mental[mental==88]<-0

mental1<-mental%>%
  select(MENTHLTH,SEX,MARITAL,GENHLTH)%>%
  filter(GENHLTH%in%c(1,2,3,4,5),MARITAL%in%c(1,2,3,4,5,6))
  
mental2<-as.data.frame(mental1)
Q10<-mental2
Q10
#Sex, General Health, and Marital Status will be used as the predictor variables 
#for Mental Health

#identify outliers
# Create a box plot 
Q11<-boxplot(mental2, main="Quartile Plot",  # Title of the plot        
        xlab="Data Sets",      # X-axis label        
        ylab="Values",         # Y-axis label        
        #names=c("Dataset 1", "Dataset 2"),  # Legend labels        
        col=c("blue", "green"), # Colors of the boxes        
        border="black",        # Border color of the boxes        
        notch=FALSE,            # Display notches for confidence intervals        
        outline=FALSE )        # Turn off the outline of the boxes

#Outliers will not be removed as it was decided that they are pertinent part of the data.
#It is significant that the values of Mental Health are considered all 30 days.
#It is significant that the codes for Marital are included for what they represent beyond three 
#because it is still considered a type of status.All General health variables that have been 
#filtered are also considered significant for the analysis.

Q12a<-ggplot(data = mental2) +
  geom_point(mapping = aes(x=GENHLTH,y=MENTHLTH))+
  labs(title = "Plot of Mental Health predicted by General Health", x = "General Health",
       y = "Mental Health")
Q12a
#General Health is a categorical variable. What is shown is the relation of Mental Health 
#and General Health. All the data points lie on x=1,2,3,4,5.
Q12b<-ggplot(data = mental2) +
  geom_point(mapping = aes(x=SEX,y=MENTHLTH))+
  labs(title = "Plot of Mental Health predicted by Sex", x = "Sex",
       y = "Mental Health")
Q12b
#Sex is a categorical variable so it shows the relation of Mental Health between the two sexes.
Q12c<-ggplot(data = mental2) +
  geom_point(mapping = aes(x=MARITAL,y=MENTHLTH))+
  labs(title = "Plot of Mental Health predicted by Marital Status", x = "Marital Status",
       y = "Mental Health")
Q12c
#Marital Status is a categorical variable so this graph shows the relation between 
#Marital Status and Mental Health


#Calculate basic descriptive statistics
summary_sex <- summary(mental2$SEX)
summary_mental_health <- summary(mental2$MENTHLTH)
mean_sex <- mean(mental2$SEX)
mean_mental_health <- mean(mental2$MENTHLTH)
median_sex <- median(mental2$SEX)
median_mental_health <- median(mental2$MENTHLTH)
sd_sex <- sd(mental2$SEX)
sd_mental_health <- sd(mental2$MENTHLTH)
#Calculate additional descriptive statistics 
Q13<-desc_stats<-describe(mental2)

#In this example, Mental Health is being used as the dependent variable while sex, 
#general health, and marital status are being used as the independent variables.
#According to the mean average of sex, since the variables male and female are represented by
#2, 1.575941 would be logical, sharing the status of mental health between the 2.
# The mean average of marital status is 2.210287 indicating that those who were divorced or 
#widowed had an effect on mental health. The mean value of General Health is 2.210287, meaning
#that those who were affected by mental health were in very good general health, but looking at the data,
# it is also the largest population in that category.
#The standard deviation of General Health is 1.080369 making it 1.475141 away from the mean.
#The standard deviation of Marital Status is 1.596545 making it .613742 away from the mean.
#The standard deviation of Sex is .0494200 making it 1.081741 away from the mean.
# This indicates that Sex has the least variation in the data.





mental_lm_1<-lm(MENTHLTH~MARITAL+GENHLTH+SEX, data = mental2)
Q14a<-summary(mental_lm_1)
Q14a
#The adjusted R squared value is very low at .088 which is close to 1 indicating that 
#the predictor variables are good to predict Mental Health.
mental_lm_2<-lm(MENTHLTH~MARITAL+SEX, data = mental2)
Q14b<-summary(mental_lm_2)
Q14b
#The adjusted R squared value is at .01371 which is is not as close to 1 as the 
#previous regression

Q14c<-min(AIC(mental_lm_1, k=2))
Q14d<-min(AIC(mental_lm_2, k=2))

#The better model fit is mental_lm_1 calculated by having a lower AIC value


