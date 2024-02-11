#Title: "Final Project"
#Amy Moschos 07-11-2023
#Amy.moschos@Eastern.edu
#2023 Summer 2 Term


rm(list = ls())
suppressPackageStartupMessages(library(tidyverse))

setwd("/Users/amymoschos/Desktop/R Code")
brfs<-read_csv("BRFSS2015_650.csv",show_col_types=FALSE)


mental<-brfs%>%
  filter((MENTHLTH>=1 & MENTHLTH<=30)|MENTHLTH==88)
mental[mental==88]<-0

mental1<-mental%>%
  select(MENTHLTH,SEX,MARITAL,GENHLTH)%>%
  filter(GENHLTH%in%c(1,2,3,4,5),MARITAL%in%c(1,2,3,4,5,6))%>%
  mutate(MARITAL=as.factor(MARITAL))
Q10<-mental1  
#Sex, General Health, and Marital Status will be used as the predictor variables 
#for Mental Health

#identify outliers
# Create a box plot 
Q11<-boxplot(mental1, main="Quartile Plot",  # Title of the plot        
        xlab="Data Sets",      # X-axis label        
        ylab="Values",         # Y-axis label        
        #names=c("Dataset 1", "Dataset 2"),  # Legend labels        
        col=c("blue", "green"), # Colors of the boxes        
        border="black",        # Border color of the boxes        
        notch=TRUE,            # Display notches for confidence intervals        
        outline=FALSE )        # Turn off the outline of the boxes

#Outliers will not be removed as it was decided that they are pertinent part of data.
#It is significant that the values of Mental Health are considered all 30 days.
#It is significant that the codes for Marital are included for what they represent beyond three 
#because it is still considered a type of status.All General health variables that have been 
#filtered are also considered significant for the analysis.
Q12