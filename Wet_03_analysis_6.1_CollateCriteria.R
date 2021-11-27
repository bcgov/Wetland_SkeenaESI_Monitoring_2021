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

#Generates SampleStrata file

source ('header.R')

Wetlands<-readRDS(file = 'tmp/AOI/Wetlands3')
#mapview::mapview(Wetlands, maxpoints = 1000000)


#clgeo_IsValid(as(Wetlands,'Spatial'), verbose = FALSE)

#Assemble wetland data with each of the strata as a single field
# bec_pts - wet_id, BECgroup2
bec_pts<-read_xlsx(file.path(dataOutDir,paste('bec_pts.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
WetFlow <- read_xlsx(file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
LandT <- read_xlsx(file.path(dataOutDir,paste('ltls.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
Disturb <- read_xlsx(file.path(dataOutDir,paste('disturb.ls.xlsx',sep='')))
#Gitanyow
Gitanyow <- read_xlsx(file.path(dataOutDir,paste('Git_pts.xlsx',sep='')))
#Gitxsan
GitxsanWshd_pts<- read_xlsx(file.path(dataOutDir,paste('GitxsanWshd_pts.xlsx',sep='')))
#OW
OW_pts<-read_xlsx(file.path(dataOutDir,paste('OW_pts.xlsx',sep='')))
#LBN
LBN_pts<-read_xlsx(file.path(dataOutDir,paste('LBN_pts.xlsx',sep='')))
#WFN
WFN_pts<-read_xlsx(file.path(dataOutDir,paste('WFN_pts.xlsx',sep='')))

#Join strata and select criteria attributes data back to wetlands
SampleStrata1<-Wetlands %>%
  st_drop_geometry() %>%
  left_join(Gitanyow, by='Wetland_Co') %>%
  left_join(GitxsanWshd_pts, by='Wetland_Co') %>%
  left_join(OW_pts, by='Wetland_Co') %>%
  left_join(LBN_pts, by='Wetland_Co') %>%
  left_join(WFN_pts, by='Wetland_Co') %>%
  unite('Nat', c('Nation.x','Nation.y','Nation.x.x','Nation.y.y','Nation'), na.rm = TRUE, remove = FALSE,sep='') %>%
  mutate(House_Name = Nat)

#set up Territory Target
UNation<-unique(SampleStrata1$House_Name)
NatTarg<-data.frame(House_Name=UNation,Territory=gsub("_.*","",UNation))

SampleStrata  <- SampleStrata1 %>%
  left_join(NatTarg, by='House_Name') %>%
  mutate(Gitanyow=ifelse(Territory=='Gitanyow','Yes','No')) %>%
  mutate(Gitxsan=ifelse(Territory=='Gitxsan','Yes','No')) %>%
  mutate(WFN=ifelse(Territory=='WFN','Yes','No')) %>%
  #dplyr::filter(House_Name != "") %>%
  mutate(House_Name = replace_na(House_Name, 'Non-Gitanyow')) %>%
  left_join(WetFlow, by='Wetland_Co') %>%
  left_join(bec_pts, by='Wetland_Co') %>%
  left_join(LandT, by='Wetland_Co') %>%
  left_join(Disturb, by='Wetland_Co') %>%
  left_join(readRDS(file = 'tmp/AOI/MooseWets'), by='Wetland_Co') %>%
  mutate(StrataGroup=as.character(group_indices(.,BEC,FlowCode))) %>% #53108
  #group_by(BEC,FlowCode) %>%
  #mutate(StrataGroup = as.character(cur_group_id())) %>%
  #ungroup() %>%
  #Drop any wetlands that are NA for BEC - 6 cases for some reason
  dplyr::filter(!is.na(BEC)) %>%
  #Drop Landcover NAs - 64 cases? all wetlands should be assigned properly? need to check
  dplyr::filter(!is.na(LanCoverLabel)) %>%
  mutate(Sampled=0) %>%
  mutate(SampleType=0) %>%
  mutate(YearSampled=0) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, YearSampled,StrataGroup, kmRd, House_Name,Dist_to_Road, BEC,
                FlowCode, Verticalflow=Verticalflow.y, Bidirectional=Bidirectional.y,
                Throughflow=Throughflow.y, Outflow=Outflow.y, Inflow=Inflow.y,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID, Moose,Territory,Gitanyow,Gitxsan,WFN)
SampleStrata$Moose[is.na(SampleStrata$Moose)] <- 'NA'


#Load in sits previously surveyed
#Read in cleaned wetland plot data - from plot_02_clean_2020.R
site_data <- readRDS(file='tmp/sites')

#Join wetland spatial with plot data to spatially identify wetlands
#some issues with GPS locations in field card missing or not necessarily correct UTM issues, miss record
#Pull only wetlands that have plots
wet_site2020<- SampleStrata %>%
  #mutate(newid = Wetland_Co) %>%
  inner_join(site_data, by=c('Wetland_Co'='Field')) %>%
  mutate(Sampled=1) %>%
  mutate(SampleType=1) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, YearSampled=Year, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID)

saveRDS(wet_site2020, file = 'tmp/AOI/wet_site2020')

#Set Sampled in SampleStrata where it has been sampled and set others to 0
SampleStrata$Sampled <- wet_site2020[match(SampleStrata$Wetland_Co, wet_site2020$Wetland_Co),2]
SampleStrata$SampleType <- wet_site2020[match(SampleStrata$Wetland_Co, wet_site2020$Wetland_Co),3]
SampleStrata$YearSampled <- wet_site2020[match(SampleStrata$Wetland_Co, wet_site2020$Wetland_Co),4]
SampleStrata[is.na(SampleStrata)] <- 0

#Generate spatial for checking
SampleStrataGeo<- Wetlands %>%
  dplyr::select(Wetland_Co) %>%
  right_join(SampleStrata)
write_sf(SampleStrataGeo, file.path(spatialOutDir,"SampleStrataGeo.gpkg"))

wet_site2020Geo<-Wetlands %>%
  dplyr::select(Wetland_Co) %>%
  right_join(wet_site2020)



#Filter out wetlands that dont have assinged Nation unless they were sampled
SampleStrata <- SampleStrata %>%
  dplyr::filter(House_Name!="" | Sampled==1)

saveRDS(SampleStrata, file = 'tmp/AOI/SampleStrata')




