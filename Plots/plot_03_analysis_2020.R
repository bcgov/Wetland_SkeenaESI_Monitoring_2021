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

WetPlotFnSheets<- excel_sheets(file.path(dataOutDir,'WetPlots.xlsx'))
WetPlotFnData1<-read_excel(file.path(dataOutDir,'WetPlots.xlsx'),
                           sheet = WetPlotFnSheets[1],
                           col_types=c('text'))
WetPlotFnData2<-read_excel(file.path(dataOutDir,'WetPlots.xlsx'),
                           sheet = WetPlotFnSheets[2],
                           col_types=c('text'))
WetPlotFnData<-WetPlotFnData1 %>%
  full_join(WetPlotFnData2)

site_data <- readRDS(file='tmp/sitesIn')

Burned<-WetPlotFnData %>%
   mutate(RecentFire=as.numeric(F55_1)+as.numeric(F55_2)+as.numeric(F55_3)+
                               as.numeric(F55_4)+as.numeric(F55_5)+as.numeric(F55_6)) %>%
  dplyr::select(Wetland_Co,RecentFire) %>%
  dplyr::filter(RecentFire>0)

site_data<-site_data %>%
  mutate(Wetland_Co=as.character(Field)) %>%
  left_join(Burned)

site_data$RecentFire[is.na(site_data$RecentFire)]<-0
saveRDS(site_data, file = 'tmp/sites')
