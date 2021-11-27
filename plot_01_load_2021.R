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

#Read sampling spreadsheet sheets from WESP-BC_2021_Field_Results_Data_12Oct21.xslx form
WetPlotFnSheets<- excel_sheets(file.path(WetMonDir,'2021FieldData/BCWF_DataDump/ESI export 11.24 WESP.xlsx'))

WetPlotFnDataIn<-read_excel(file.path(WetMonDir,'2021FieldData/BCWF_DataDump/ESI export 11.24 WESP.xlsx'),
                            sheet = WetPlotFnSheets[1], skip=1,
                            col_names=TRUE, col_types=c('text'))
#Filter the data for ESI sites
unique(WetPlotFnDataIn$Investigators)
unique(WetPlotFnDataIn$Wetland_Co)
WForm <- WetPlotFnDataIn %>%
  dplyr::filter((grepl(('ESI|esi|Training|Witset|witset|Gitanyow|Smithers|GES|Gitxsan'), Investigators)) |
                  (grepl(('ESI|Esi|Wit|Witset|Smithers'), Wetland_Co))) %>%
  dplyr::select(-c(ObjectID,GlobalID,Region,DateIn,
                   CreationDate,Creator,EditDate,Editor,x,y))
#Fix Wetland_Co field
WForm<- WForm %>%
  dplyr::rename(Wetland_CoIn = Wetland_Co) %>%
  mutate(Wetland_Co=if_else(is.na(parse_number(Wetland_CoIn,trim_ws=TRUE)) |
                            str_length(parse_number(Wetland_CoIn))==1, Wetland_CoIn,
                            as.character(parse_number(Wetland_CoIn)))) %>%
  mutate(Wetland_Co=gsub("-", "", paste(Wetland_Co)))


#Read spreadsheet sheets from WESP-BC_FieldForms_DataDoneMA.xslx function forms
WetPlotFnSheets<- excel_sheets(file.path(WetMonDir,'2020FieldData/WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021_review.xlsx'))
WetPlotFnData2020In<-read_excel(file.path(WetMonDir,'2020FieldData/WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021_review.xlsx'),
                            sheet = WetPlotFnSheets[2], skip=4,
                            col_names=TRUE, col_types=c('text'))

#Rename columns ...8 to ...77 to Data_#
nSites<-ncol(WetPlotFnData2020In)
oldnames<-sapply(8:nSites, function(x) (paste0('...',x)))
newnames<-c(paste0('data_',1:(nSites-7)))

WManual2020 <- WetPlotFnData2020In %>%
  rename_at(vars(oldnames), ~ newnames) %>%
  dplyr::select(-c(...2,Indicators,Scale,'Condition Choices','Definitions/Explanations','Data'))

#Added a column in spreadsheet for each indicator
WetPlotFnStressor2020<-read_excel(file.path(WetMonDir,'2020FieldData/WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021_review.xlsx'),
                              sheet = WetPlotFnSheets[3], col_names=FALSE,
                              col_types=c('text'))



