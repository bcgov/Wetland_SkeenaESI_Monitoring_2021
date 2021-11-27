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

#read in CGL points
Moose<-readRDS(file='tmp/AOI/Moose')

Moosebuf<-Moose %>%
  st_buffer(100)

#Read in clean wetlands
Wetlands3<-readRDS(file = 'tmp/AOI/Wetlands3')

#Select wetlands in Moose buffer and pull out unique ones only
MooseWets <-st_intersection(Wetlands3, Moosebuf) %>%
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co)  %>%
  dplyr::distinct(Wetland_Co) %>%
  mutate(Moose="Moose Sample Site")
saveRDS(MooseWets, file = 'tmp/AOI/MooseWets')

#Add wetland variables back
MooseWetV<-Wetlands3 %>%
  dplyr::filter(Wetland_Co %in% MooseWet$Wetland_Co)

saveRDS(MooseWetV, file = 'tmp/AOI/MooseWetV')
#write to geo package to check
write_sf(MooseWetV, file.path(spatialOutDir,"MooseWetV.gpkg"))


