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

source("header.R")

#Read in Paul's Wetland_Co look up table so can assign consistent IDs
SiteID_xtab<-read_excel(file.path(WetMonDir,'WESPprocessing/SiteID_xtab.xlsx'),
                        skip=1,
                        col_names=c('Wetland_Co','Batch_ID'))
Batch_ID_max<-max(SiteID_xtab$Batch_ID)

#Read sampling spreadsheet sheets from WESP-BC_2021_Field_Results_Data_12Oct21.xslx form from BCWF.
#BCWFfile<-'ESI export 11.24 WESP.xlsx'
BCWFfile<-'WESP_06_24_0_ export02.23.xlsx'
WetPlotFnSheets<- excel_sheets(file.path(WetMonDir,paste0('2021FieldData/BCWF_DataDump/',BCWFfile)))

WetPlotFnDataIn<-read_excel(file.path(WetMonDir,paste0('2021FieldData/BCWF_DataDump/',BCWFfile)),
                            sheet = WetPlotFnSheets[1], #skip=1,
                            col_names=TRUE, col_types=c('text'))
#Filter the data for ESI sites
unique(WetPlotFnDataIn$Investigators)
unique(WetPlotFnDataIn$Wetland_Co)
WFormIn <- WetPlotFnDataIn %>%
  dplyr::filter((grepl(('ESI|esi|Training|Witset|witset|Gitanyow|Smithers|GES|Gitxsan'), Investigators)) |
                  (grepl(('ESI|Esi|Wit|Witset|Smithers'), Wetland_Co))) %>%
  dplyr::select(-c(ObjectID,GlobalID,Region,DateIn,
                   CreationDate,Creator,EditDate,Editor,x,y))

#Fix Wetland_Co field and add Batch_ID field
#strings to replace with null characters or with Training designation
wetFix<-c("ESI-","ESI_","ESi-","ESi ","ESI","GES","GES ","GES#","training ")
wetTrain<-data.frame(Wetc=c('ESI-8804','ESi-8804','ESi training 35800','ESI-35800'),
                     NewWet=c('8804','8804Train','35800Train','35800'))
WForm<- WFormIn %>%
  mutate(Wetland_CoIn = Wetland_Co) %>%
  mutate(Wetland_Co=str_squish(mgsub::mgsub(Wetland_Co, wetTrain$Wetc, wetTrain$NewWet))) %>%
  mutate(Wetland_Co=str_squish(mgsub::mgsub(Wetland_Co, wetFix, replicate(length(wetFix),"")))) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Torkelson wetland (frep Smithers district)", 54255, Wetland_Co)) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Wit_002", 8792, Wetland_Co)) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Wit_003_duckbill", 8702, Wetland_Co)) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Wit001", 8769, Wetland_Co)) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Witset_wesp_seaton", 39847, Wetland_Co)) %>%
  mutate(Wetland_Co=ifelse(Wetland_Co=="Witset_wetlandblunt01", 39591, Wetland_Co))  %>%
  dplyr::filter(!(Wetland_Co=='33240' & Investigators=='WFN'),
                !(Wetland_Co=='8804Train' & Investigators=='Training'),
                !(Wetland_Co=='35800Train' & Investigators=='Witset')) %>%
  mutate(Batch_IDc=paste0('data_',Batch_ID_max+row_number()))%>%
  mutate(Batch_ID=Batch_ID_max+row_number())

WFcheck<-WForm %>%
  dplyr::select(Wetland_CoIn, Wetland_Co, Batch_IDc, Batch_ID)

#Read spreadsheet sheets from BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm
#This spreadsheet has been updated by Paul, a 'Questions' column has been added
#for computer readability
WetPlotFnSheets<- excel_sheets(file.path(WetMonDir,'WESPprocessing/BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm'))

WetPlotFnData2020In1<-read_excel(file.path(WetMonDir,'WESPprocessing/BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm'),
                                sheet = 'F',
                                col_names=TRUE, col_types=c('text'))
colnames(WetPlotFnData2020In1)
nSites<-ncol(WetPlotFnData2020In1)-5
cnames<-c('Questions','Q2','Indicator','ConditionChoices','Total',c(paste0('data_',1:(nSites))))
WetPlotFnData2020In<-read_excel(file.path(WetMonDir,'WESPprocessing/BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm'),
                                sheet = 'F', skip=1,
                                col_names=cnames, col_types=c('text'))

WManual2020 <- WetPlotFnData2020In %>%
  dplyr::select(-c(Q2,Indicator, ConditionChoices, Total))

#Added a column in spreadsheet for each indicator
WetPlotFnStressor20201<-read_excel(file.path(WetMonDir,'WESPprocessing/BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm'),
                              sheet = 'S', col_names=TRUE,
                              col_types=c('text'))
colnames(WetPlotFnStressor20201)
nSites<-ncol(WetPlotFnStressor20201)-7
cnames<-c('Questions','Q2','stressor1','stressor2','stressor3','stressor4','Total',c(paste0('data_',1:(nSites))))
WetPlotFnStressor2020<-read_excel(file.path(WetMonDir,'WESPprocessing/BC_BatchCalculator_8April_Fixed+AllData_unformatted_01Dec2021.xlsm'),
                                sheet = 'S', skip=1,
                                col_names=cnames, col_types=c('text'))

WFormStress2020 <- WetPlotFnStressor2020 %>%
  dplyr::select(-c(Q2,stressor1,stressor2,stressor3,stressor4,stressor4, Total))



