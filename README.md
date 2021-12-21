# Good-code-for-subseting-
Exampe of code for subseting elements of rows from different columns


#RCaMP Calcium Analysis for Baseline 2 cycles R72R and R72L JMR

#load the library 
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)

#load the data 
filenames <- list.files(pattern="*.csv") #read all the .csv files 


#create a dataframe; Remove missing values
df<-purrr::map_df(filenames, read_csv, .id = 'filename') 
df<- na.omit(df) #remove missing values 

#change the names to lowercase
df$roiName<- tolower(df$roiName)
df$Spot<- tolower(df$Spot)
df$Condition<- tolower(df$Condition)
df$drug<- tolower(df$drug)

#Change nominal and date format
df$filename = factor(df$filename)
df$roiName = factor(df$roiName)
df$Animal = factor(df$Animal)
df$drug = factor(df$drug)
df$Spot = factor(df$Spot)
df$filename = factor(df$filename)
df$date = factor(df$date)
view(df)

#Modifying variables and cleaning data frame
#Short df and create a unique ROIname
df_short <- df[ ,c('date', 'drug', 'Condition', 'Spot', 'Animal', 'roiName', 'amplitude', 'halfWidth','peakAUC')]
#Replace after drugs for baseline
#df_short$drug <- str_replace_all(df_short$drug, 'after drugs', 'baseline')
#adding unique Spot
df_short$Unique_Spot <-paste(df_short$Animal, df_short$Spot, sep= "_")
#adding unique ROI name
df_short$Unique_ROIname <-paste(df_short$Animal, df_short$Spot, df_short$roiName, sep= "_")
#create a new variable for Process, Soma and Sphincter
unique(df_short$roiName)
#replacing "sp" for "t" labeling because it overlaps with "s" and "p" for using grepl function
df_short$roiName <- str_replace_all(df_short$roiName, 'sp1', 't1')
df_short$roiName <- str_replace_all(df_short$roiName, 'sp2', 't2')
df_short$roiName <- str_replace_all(df_short$roiName, 'sp3', 't3')
df_short$roiName <- str_replace_all(df_short$roiName, 'sp2-1', 't2-1')
unique(df_short$roiName)
view(df_short)
#It is better now, create a new variable for Process, Soma and Sphincter
df_short$ROI=0
df_short$ROI[grepl("p",df_short$roiName)]="Process"
df_short$ROI[grepl("s",df_short$roiName)]="Soma"
df_short$ROI[grepl("t",df_short$roiName)]="Sphincter"
view(df_short)
unique(df_short$drug)
unique(df_short$ROI)
unique(df_short$Animal)

#Make another data frame with just baseline that has an extra cycle of the movie 
#to divide the double values
df_baseline <- df_short%>%
  filter(drug == "baseline", Animal!="R97B")
View(df_baseline)
#Dividing parameters peakAUC and halfWidth  of Baseline of R72L and R72R because it is 2 cycles instead of one
df_baseline$peakAUC<- (df_baseline$peakAUC)/2
df_baseline$halfWidth<- (df_baseline$halfWidth)/2
View(df_baseline)

#Make a df substracting the data modified.
df_nshort <- df_short%>%
  filter( drug!="baseline")
View(df_nshort)

#Combining data frames with values of just 1 cycle
df_short <- rbind(df_nshort, df_baseline)
unique(df_short$drug)
unique(df_short$ROI)
unique(df_short$Animal)


df_short <- df_short[ ,c('date', 'Unique_Spot', 'Unique_ROIname', 'ROI', 'Condition', 'drug', 'amplitude', 'Duration','peakAUC')]
view(df_short)
