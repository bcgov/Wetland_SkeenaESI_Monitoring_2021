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

# Analysis generates a Land Type strata
# first, it modifies forested portion and designates as conifer, mixed, deciduous/shrub
# second, first it uses the LandCoverAge coverage and nibbles into water/wetland features and
# assigns to largest neighbourhood

source ('header.R')
ESI<-readRDS(file = 'tmp/ESI')

AOI<-ESI

ESI_OW <-readRDS(file='tmp/AOI/ESI_OW') %>%
  st_set_crs(3005) %>%
  st_union() %>%
  st_intersection(AOI)
ESI_WFN <-readRDS(file='tmp/AOI/ESI_WFN') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
WFN_AOI <-readRDS(file='tmp/AOI/WFN_AOI') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_LBN <-readRDS(file='tmp/AOI/ESI_LBN') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_Gitxsan <-readRDS(file='tmp/AOI/ESI_Gitxsan') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_Gitxsan_wshd <-readRDS(file='tmp/AOI/ESI_Gitxsan_wshd') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_Gitanyow <-readRDS(file='tmp/AOI/ESI_Gitanyow') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)

#Clean up the Nation watersheds and make a single admin layer
# note ‘Lower Skeena’ = ‘Kitwanga’ = ‘Gitwangak’
#OW is not doing wetland sampling, use WFN boundary

#reduce Gitxsan to watershed groups
GitxsanWshd <- ESI_Gitxsan %>%
  group_by(WATERSHED_) %>%
  dplyr::summarise(AreaHa=sum(Hectares), nHouse=n())

ESI_Nations <-readRDS(file='tmp/AOI/FN_boundaries')
#Pull out LBN and erase slivers with Gitxsan and OW
LBN<- ESI_Nations %>%
  dplyr::filter(BOUNDARY_NAME=='Lake Babine Nation') %>%
  st_difference(st_union(GitxsanWshd)) %>%
  st_difference(st_union(ESI_WFN)) %>%
  st_intersection(ESI)  %>%
  st_buffer(0)
mapview(LBN)+mapview(GitxsanWshd)+mapview(WFN_AOI)

#Pull out OW not in WFN
OW<- ESI_OW %>%
  #st_difference(st_union(ESI_WFN))  %>%
  st_difference(st_union(WFN_AOI))  %>% #for WFN only look at AOI instead of entire Territory
  st_difference(st_union(LBN))  %>%
  st_intersection(ESI)%>%
  st_buffer(0)
mapview(LBN)+mapview(OW)+mapview(WFN_AOI)

#Nations<- ESI_Gitanyow %>%
#  st_union(GitxsanWshd) %>%
#  st_union(Wetsuweten)

#Nations2<-Nations %>%
#  st_union(LBN)

waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#join to wetlands
Git_pts <- st_intersection(waterpt, ESI_Gitanyow) %>%
  st_drop_geometry() %>%
  mutate(Nation=paste('Gitanyow_',House_Name,sep='')) %>%
  dplyr::select(Wetland_Co, Nation)
WriteXLS(Git_pts,file.path(dataOutDir,paste('Git_pts.xlsx',sep='')))

OW_pts <-st_intersection(waterpt, OW) %>%
  st_drop_geometry() %>%
  mutate(Nation='Wetsuweten') %>%
dplyr::select(Wetland_Co, Nation)
WriteXLS(OW_pts,file.path(dataOutDir,paste('OW_pts.xlsx',sep='')))

GitxsanWshd_pts <-st_intersection(waterpt, GitxsanWshd) %>%
  st_drop_geometry() %>%
  mutate(Nation=paste('Gitxsan_',WATERSHED_,sep='')) %>%
  dplyr::select(Wetland_Co, Nation)
WriteXLS(GitxsanWshd_pts,file.path(dataOutDir,paste('GitxsanWshd_pts.xlsx',sep='')))

LBN_pts <-st_intersection(waterpt, LBN) %>%
  st_drop_geometry() %>%
  mutate(Nation='LBN') %>%
  dplyr::select(Wetland_Co, Nation)
WriteXLS(LBN_pts,file.path(dataOutDir,paste('LBN_pts.xlsx',sep='')))

#WFN_pts <-st_intersection(waterpt, ESI_WFN) %>%
WFN_pts <-st_intersection(waterpt, WFN_AOI) %>% #Use only AOI not entire Territory
  st_drop_geometry() %>%
  mutate(Nation=paste('WFN_',Name, sep="")) %>% #ability to set target for each sub-AOI
  dplyr::select(Wetland_Co, Nation)
WriteXLS(WFN_pts,file.path(dataOutDir,paste('WFN_pts.xlsx',sep='')))

gc()
