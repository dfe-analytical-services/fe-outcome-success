# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("DT")
# install.packages("dplyr")
# install.packages("markdown")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("shinyjs")
# install.packages("plotly")

# load packages
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(markdown)
library(reshape2)
library(stringr)
library(packrat)
library(shinyjs)
library(plotly)
# read in data - obsm is the data displayed in the tables, summ is the data used to create the graphs on the summary page, 
# levs is factor levels of learner category, thresholds is thresholds of quintiles
obsm<-read.csv("Data/obsm.csv",
               check.names=F,stringsAsFactors=F)
summ<-read.csv("Data/summary.csv",
               check.names=F,stringsAsFactors=F)
levs<-read.csv("Data/learner_cat_levels.csv",
               check.names=F,stringsAsFactors=F)
thresholds<-read.csv("Data/thresholds.csv",
                     check.names=F,stringsAsFactors=F)

# combining ukprn and provider name into one variable, then removing those variables and replacing with new variable
# relevelling factor levels of learner category
obsm$Provider<-paste(obsm$UKPRN,"-",obsm$`Provider Name`)
obsm<-obsm %>% select(c(22,3:21))
obsm %>% arrange(factor(`Learner Category`,levels=levs[,1]))
summ$Provider<-paste(summ$UKPRN,"-",summ$`Provider Name`)
summ<-summ %>% select(c(22,3:21))

# adding percent sign to SPDR values that are numbers instead of x or . 
obsm$`Sustained Positive Destination Rate 13/14 BL`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 13/14 BL`)))==F,
                                                            paste0(obsm$`Sustained Positive Destination Rate 13/14 BL`,'%'),
                                                            as.character(obsm$`Sustained Positive Destination Rate 13/14 BL`))
obsm$`Sustained Positive Destination Rate 13/14 All`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 13/14 All`)))==F,
                                                             paste0(obsm$`Sustained Positive Destination Rate 13/14 All`,'%'),
                                                             as.character(obsm$`Sustained Positive Destination Rate 13/14 All`))
obsm$`Sustained Positive Destination Rate 14/15 BL`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 14/15 BL`)))==F,
                                                            paste0(obsm$`Sustained Positive Destination Rate 14/15 BL`,'%'),
                                                            as.character(obsm$`Sustained Positive Destination Rate 14/15 BL`))
obsm$`Sustained Positive Destination Rate 14/15 All`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 14/15 All`)))==F,
                                                             paste0(obsm$`Sustained Positive Destination Rate 14/15 All`,'%'),
                                                             as.character(obsm$`Sustained Positive Destination Rate 14/15 All`))
obsm$`Sustained Positive Destination Rate 15/16 BL`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 15/16 BL`)))==F,
                                                            paste0(obsm$`Sustained Positive Destination Rate 15/16 BL`,'%'),
                                                            as.character(obsm$`Sustained Positive Destination Rate 15/16 BL`))
obsm$`Sustained Positive Destination Rate 15/16 All`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 15/16 All`)))==F,
                                                             paste0(obsm$`Sustained Positive Destination Rate 15/16 All`,'%'),
                                                             as.character(obsm$`Sustained Positive Destination Rate 15/16 All`))
