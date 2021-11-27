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

#read in FREP blocks
FREPblocks<-readRDS(file='tmp/AOI/FREPblocks')

#identify which FREP blocks are adjacent to wetlands, first buffer FREP blocks
FREPbuf<- st_buffer(FREPblocks, 100) %>%
write_sf(FREPbuf, file.path(spatialOutDir,"FREPbuf.gpkg"))

#Read in clean wetlands
Wetlands<-readRDS(file = 'tmp/AOI/Wetlands2')

#Point in polygon to assign FREP block buffer to wetland picking up adjacent wetlands
FREP_buff_wets <-st_intersection(Wetlands, FREPbuf) %>%
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co, OPENING_ID, DISTRICT_NAME, Order)

FREPwets<-Wetlands %>%
  dplyr::filter(Wetland_Co %in% FREP_buff_wets$Wetland_Co) %>%
  right_join(FREP_buff_wets,by='Wetland_Co') %>%
  #Some wetlands chosen more than once - take one highest in order list, drop the others
  group_by(Wetland_Co) %>%
  slice_min(n=1, order_by=Order, with_ties=FALSE)

Check <-FREPwets %>%
  dplyr::select(Wetland_Co, OPENING_ID, DISTRICT_NAME, Order)

saveRDS(FREPwets, file = 'tmp/AOI/FREPwets')
#write to geo package to check
write_sf(FREPwets, file.path(spatialOutDir,"FREPwets.gpkg"))

saveRDS(Wetlands, file = 'tmp/AOI/Wetlands3')
#write to geo package to check
write_sf(Wetlands, file.path(spatialOutDir,"Wetlands3.gpkg"))


