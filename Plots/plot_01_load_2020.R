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

#Read spreadsheet sheets from WESP-BC_FieldForms_DataDoneMA.xslx function forms
WetPlotFnSheets<- excel_sheets(file.path(WetMonDir,'WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021.xlsx'))
WetPlotFnDataIn<-read_excel(file.path(WetMonDir,'WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021.xlsx'),
                            sheet = WetPlotFnSheets[2],
                            col_types=c('text'))
#Added a column in spreadsheet for each indicator
WetPlotFnStressor<-read_excel(file.path(WetMonDir,'WESP-BC_FieldForms_DataDoneMA_DM_MS_5_May_2021.xlsx'),
                              sheet = WetPlotFnSheets[3], col_names=FALSE,
                              col_types=c('text'))

sites<-read_excel(file.path(DataDir,'sites visited 2019 and 2020.xlsx'), col_types=c('numeric','numeric','numeric','text','text','text','numeric'))
saveRDS(sites, file = 'tmp/sitesIn')






