# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source('header.R')

#Wetland Function Data
#Clean up Plot Function Data
#remove unnecessary columns and transform
EndCol<-ncol(WetPlotFnDataIn)
WetPlotFnData <- WetPlotFnDataIn %>%
  dplyr::select(8:all_of(EndCol)) %>%
  t() %>%
  as.data.frame()

#assign column names
colName <- WetPlotFnDataIn %>%
  dplyr::select(1)

colnames(WetPlotFnData)<-unlist(as.list(colName))

#Clean up names
#WetPlotFnData <- WetPlotFnData[ !duplicated(names(WetPlotFnData)) ]
#Drop un-needed columns, take first row and id all NA then drop those columns
ColsToDrop <- WetPlotFnData[1,]
WetPlotFnData <- WetPlotFnData %>%
  #drop columns that are null
  dplyr::select(-c(colnames(ColsToDrop)[colSums(is.na(ColsToDrop)) > 0])) %>%
  #drop unused/old wetland ids
  dplyr::select(-c("FID", "FID2","newid")) %>%
  #remove '.00' from Wetland_Co
  #mutate(Wetland_Co=gsub('\\.00','',Wetland_Co)) %>%
  #remove all instances of '.0' in Wetland_Co and in Function answers
  mutate_all(funs(gsub('\\.00','',.))) %>%
  mutate_all(funs(gsub('\\.0','',.)))

#Convert all NA to 0
WetPlotFnData[is.na(WetPlotFnData)]<-0

#Clean up row numbers
row.names(WetPlotFnData) <- NULL

#Break function data into 2 data.frames so can write to xlsx (otherwise over 256 columns)
WetPlotFnData1<- WetPlotFnData[,1:187] # to F39 questions
WetPlotFnData2<- WetPlotFnData[,c(1:3,188:ncol(WetPlotFnData))] # F40 questions and greater

#Wetland Stressor Data
#clean up stressor sheet - WetPlotFnStressor
EndCol<-ncol(WetPlotFnStressor)
WetPlotFnStressor1 <- WetPlotFnStressor %>%
  dplyr::select(8:all_of(EndCol)) %>%
  t() %>%
  as.data.frame()

colName <- WetPlotFnStressor %>%
  dplyr::select(1)

colnames(WetPlotFnStressor1)<-unlist(as.list(colName))

#Convert all NA to 0
WetPlotFnStressor1[is.na(WetPlotFnStressor1)]<-0
WetPlotFnStressor1[(WetPlotFnStressor1=='N/A')]<-0

#Clean up row numbers
row.names(WetPlotFnStressor1) <- NULL

WetStress <- WetPlotFnStressor1 %>%
  #drop unused columns
  dplyr::select(-c("Descrip"))

## write out data
#Take processed data make alist and write to multi-tab spreadsheet for use in R course
WetData<-list(WetPlotFnData1,WetPlotFnData2,WetStress)
WetDataNames<-c('WetlandFunction1','WetlandFunction2','WetlandStressors')

WriteXLS(WetData,file.path(dataOutDir,paste('WetPlots.xlsx',sep='')),SheetNames=WetDataNames)
#Read by Wet_03_analy

sis_6_samplePrep.R
