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

#Generates FREP and CGL and Nation amendments to the SampleStrata file
#SampleType gets set, some cases where 1 (Sampled) gets changed to 2 (FREP) or 3 (CGL), 4 is 'Other'
#Sampled is 66 at the start, bumps to 74 with the 8 Nation specific wetlands that will definitely be sampled

source ('header.R')

SampleStrata<-readRDS(file = 'tmp/AOI/SampleStrata')
wet_site2020<-readRDS(file = 'tmp/AOI/wet_site2020')

## FREP wetlands ##
#Break up by District so get 4 for each using the Order field if filled
FREPwets<-readRDS(file = 'tmp/AOI/FREPwets') %>%
  right_join(SampleStrata)
#table(FREPwets$DISTRICT_NAME)

#Look in sample watersheds first for wetlands especially DSS
WetsInWshd <- FREPwets %>%
  dplyr::filter(Wshd_Sample_Type>0 | SampleType>0)

FREP_DSS<-FREPwets %>%
  dplyr::filter(DISTRICT_NAME=='Skeena Stikine Natural Resource District' &
                  Wetland_Co %in% WetsInWshd$Wetland_Co  &
                  YearSampled==0 &
                  House_Name != "" &
                  kmRd==1) %>%
  dplyr::select(Wetland_Co, Order) %>%
  arrange(Order)
FREPtargetDSS<-FREP_DSS[1:4,]$Wetland_Co

#DCM FREP wetlands
FREP_DCM<-FREPwets %>%
  dplyr::filter(DISTRICT_NAME=='Coast Mountain Natural Resource District' &
                  #Wetland_Co %in% WetsInWshd$Wetland_Co  &
                  YearSampled==0 &
                  House_Name != "" &
                  kmRd==1) %>%
  dplyr::select(Wetland_Co, Order) %>%
  arrange(Order)
FREPtargetDCM<-FREP_DCM[1:4,]$Wetland_Co

#DND FREP wetlands
FREP_DND<-FREPwets %>%
  dplyr::filter(DISTRICT_NAME=='Nadina Natural Resource District' &
                  #Wetland_Co %in% WetsInWshd$Wetland_Co  &
                  YearSampled==0 &
                  House_Name != "" &
                  kmRd==1)
#select 4 random wetlands from the Nadina FREP blocks
FREPtargetDND<-sample(FREP_DND$Wetland_Co,4)

#Aggragate FREP sites
FREPsites<- SampleStrata %>%
  dplyr::filter(Wetland_Co %in% FREPtargetDND | Wetland_Co %in% FREPtargetDSS | Wetland_Co %in% FREPtargetDCM) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,YearSampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID, Moose,Territory,Gitanyow,Gitxsan,WFN)

#Set sample SampleType=2 if a FREP wetland
SampleStrata_wFREP<-SampleStrata %>%
  mutate(YearSampled=ifelse(((Wetland_Co %in% FREPsites$Wetland_Co) & Sampled==0), 2021, YearSampled)) %>%
  mutate(Sampled=ifelse(((Wetland_Co %in% FREPsites$Wetland_Co) & Sampled==0), 1, Sampled)) %>%
  mutate(SampleType=ifelse((Wetland_Co %in% FREPsites$Wetland_Co), 2, SampleType)) %>%
  mutate(DisturbType=ifelse((Wetland_Co %in% FREPsites$Wetland_Co), "FREP", DisturbType))
SampleStrata_wFREP[is.na(SampleStrata_wFREP)] <- 0


## CGL Wetlands ##
#Set sample SampleType= 3 if a CGL wetland
CGLtarget<-c(30878, 30889, 31153, 31225, 4679)
CGLsites<- SampleStrata_wFREP %>%
  dplyr::filter(Wetland_Co %in% CGLtarget & Sampled==0)

CGLsites<- SampleStrata_wFREP %>%
  dplyr::filter(Wetland_Co %in% CGLtarget) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,YearSampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID, Moose,Territory,Gitanyow,Gitxsan,WFN)

SampleStrata_wFREP_CGL<-SampleStrata_wFREP %>%
  mutate(YearSampled=ifelse(((Wetland_Co %in% CGLsites$Wetland_Co)& Sampled==0), 2021, YearSampled)) %>%
  mutate(Sampled=ifelse(((Wetland_Co %in% CGLsites$Wetland_Co) & Sampled==0), 1, Sampled)) %>%
  mutate(SampleType=ifelse((Wetland_Co %in% CGLsites$Wetland_Co), 3, SampleType)) %>%
  mutate(DisturbType=ifelse((Wetland_Co %in% CGLsites$Wetland_Co), "Pipeline", DisturbType))

SampleStrata_wFREP_CGL[is.na(SampleStrata_wFREP_CGL)] <- 0
saveRDS(SampleStrata_wFREP_CGL, file = 'tmp/AOI/SampleStrata_wFREP_CGL')

#Set sample SampleType= 4 if a other wetland-11
#LBNtarget<-c(37788, 54284, 54465, 54255)#LBN drop
Witsettarget<-c(8673, 8702, 8769, 8804, 8792) #Witset
Gitanyowtarget<-c(48507, 46285)
Trainingtarget<-c(35800) #Training sites
WFNtarget<-c(32212) #Maxan wetland
WFNtarget
Deceptiontarget<-c(35771)

Othertarget<-c(Witsettarget,Gitanyowtarget,Trainingtarget,WFNtarget,Deceptiontarget)
Othersites<- SampleStrata_wFREP_CGL %>%
  dplyr::filter(Wetland_Co %in% Othertarget & Sampled==0) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,YearSampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID, Moose,Territory,Gitanyow,Gitxsan,WFN)

SampleStrata_wFREP_CGL_Other<-SampleStrata_wFREP_CGL %>%
  #Set sampled to 1 and year to 2021 since these sites will be sampled this year
  mutate(Sampled=ifelse(((Wetland_Co %in% Othersites$Wetland_Co) & Sampled==0), 1, Sampled)) %>%
  mutate(SampleType=ifelse((Wetland_Co %in% Othersites$Wetland_Co), 4, SampleType)) %>%
  mutate(YearSampled=ifelse(((Wetland_Co %in% Othersites$Wetland_Co)), 2021, YearSampled)) %>%
  mutate(DisturbType=ifelse((Wetland_Co %in% Othersites$Wetland_Co), "Nation Priority", DisturbType))

SampleStrata_wFREP_CGL_Other[is.na(SampleStrata_wFREP_CGL_Other)] <- 0
saveRDS(SampleStrata_wFREP_CGL_Other, file = 'tmp/AOI/SampleStrata_wFREP_CGL_Other')

#Filter out suspect polygons and the
# northern Gitxsan Watersheds & Gitwangak (Kitwanga)
#Filter out far from road unless already sampled, FREP or CGL wetland
#HouseDrop<-c("Gitxsan_ Sustut","Gitxsan_ Nass" ,"Gitxsan_ Upper Skeena" , "Gitxsan_ Kitwanga")
HouseDrop<-c("Wetsuweten","Gitxsan_Sustut","Gitxsan_Suskwa","Gitxsan_Nass" ,"Gitxsan_Upper Skeena" , "Gitxsan_Kitwanga")
WetMappingErros<-c(40237)
SampleStrata_Final<-SampleStrata_wFREP_CGL_Other %>%
  dplyr::filter(!(Wetland_Co %in% WetMappingErros)) %>%
  dplyr::filter(!(House_Name %in% HouseDrop & Sampled==0)) %>%
  dplyr::filter(Wshd_Sample_Type>0 | Sampled>0 | SampleType>0 | Territory=='WFN') %>%
  dplyr::filter(Sampled>0 | SampleType>0 | kmRd==1)

#Pick some moose sites within the constrained SampleStrata
#Set sample SampleType= 5 if a moose wetland
#MooseWets<-readRDS(file = 'tmp/AOI/MooseWets') %>%
#  dplyr::filter(Wetland_Co %in% SampleStrata_PFinal$Wetland_Co)

#MooseTarget<-sample(MooseWets$Wetland_Co,4)
#MooseTarget<-MooseWets$Wetland_Co

#MooseSites<- SampleStrata_PFinal %>%
#  dplyr::filter(Wetland_Co %in% MooseTarget) %>%
#  dplyr::select(Wetland_Co, Sampled, SampleType,YearSampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
#                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
#                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID, Moose)

#Set sample SampleType=5 if a Moose wetland
#SampleStrata_Final<-SampleStrata_PFinal %>%
#  mutate(Sampled=ifelse(((Wetland_Co %in% MooseSites$Wetland_Co) & Sampled==0), 0, Sampled)) %>%
#  mutate(SampleType=ifelse((Wetland_Co %in% MooseSites$Wetland_Co), 5, SampleType)) %>%
#  mutate(Moose=ifelse((Wetland_Co %in% MooseSites$Wetland_Co), "Moose Sample Site", 'NA'))

SampleStrata_Final[is.na(SampleStrata_Final)] <- 0

#Some data checking
table(SampleStrata_Final$SampleType)
table(SampleStrata_Final$Moose, SampleStrata_Final$Sampled)

saveRDS(SampleStrata_Final, file = 'tmp/AOI/SampleStrata_Final')
#Save file for input to the sample draw program
write.csv(SampleStrata_Final, file=file.path(DrawDir,'SampleStrata_ESI.csv'), row.names = FALSE)

SampleStrata_Final_Geo<-Wetlands  %>%
  right_join(SampleStrata_Final, by='Wetland_Co')

SampleStrata_Final_Geo[is.na(SampleStrata_Final_Geo)] <- 0
saveRDS(SampleStrata_Final_Geo, file = 'tmp/AOI/SampleStrata_Final_Geo')
write_sf(SampleStrata_Final_Geo, file.path(spatialOutDir,"SampleStrata_Final_Geo.gpkg"))

SampleStrata_wFREP_CGL_Other_Geo<-Wetlands  %>%
  right_join(SampleStrata_wFREP_CGL_Other, by='Wetland_Co') %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,YearSampled, kmRd=kmRd.x, StrataGroup, House_Name, Dist_to_Road=Dist_to_Road.x, BEC,
                FlowCode, Verticalflow=Verticalflow.x, Bidirectional=Bidirectional.x,Throughflow=Throughflow.x, Outflow=Outflow.x, Inflow=Inflow.x,
                LanCoverLabel, DisturbType, Wshd_Sample_Type=Wshd_Sample_Type.x,WatershedID=WatershedID.x, Moose,Territory,Gitanyow,Gitxsan,WFN)





################
table(SampleStrata_Final$SampleType)

Wetlands<-readRDS(file = 'tmp/AOI/Wetlands3')

SampleStrata_FinalGeo<- Wetlands %>%
  dplyr::select(Wetland_Co) %>%
  right_join(SampleStrata_Final)

SampleStrata_FinalGeo_Pot<- SampleStrata_FinalGeo %>%
  dplyr::filter(Sampled==0)
write_sf(SampleStrata_FinalGeo_Pot, file.path(spatialOutDir,"SampleStrata_FinalGeo_Pot.gpkg"))

SampleWatersheds<- SampleStrata_Final %>%
  distinct(WatershedID) %>%
  dplyr::filter(WatershedID>0) %>%
  mutate(WatershedID=as.character(WatershedID))


Watersheds<-readRDS(file = 'tmp/AOI/Watersheds')

WetWatersheds <- Watersheds %>%
  dplyr::filter(WatershedID %in% as.vector(SampleWatersheds$WatershedID))

write_sf(WetWatersheds, file.path(spatialOutDir,"WetWatersheds.gpkg"))

nrow(subset(SampleStrata_Final, YearSampled>1))

wet_site2020<-SampleStrata_Final %>%
  dplyr::filter(Sampled>0)
saveRDS(wet_site2020, file = 'tmp/AOI/wet_site2020')
WriteXLS(wet_site2020,file.path(dataOutDir,paste('wet_site2020.xlsx',sep='')),SheetNames='wet_site2020')

wet_site2020Geo<-Wetlands %>%
  dplyr::select(Wetland_Co) %>%
  right_join(wet_site2020)

write_sf(wet_site2020Geo, file.path(spatialOutDir,"wet_site2020Geo.gpkg"))

mapview(Watersheds, zcol='SampleType')+
  mapview(wet_site2020Geo, zcol='YearSampled')

StrataGroup<-SampleStrata_Final %>%
  group_by(StrataGroup, BEC, FlowCode) %>%
  dplyr::summarise(nWetlands=n())

saveRDS(StrataGroup, file = 'tmp/AOI/StrataGroup')

#SampleStrata2020<-(subset(SampleStrata, Sampled==1))

#mapview(Watersheds[Watersheds$SampleType>0,], zcol='SampleType')+
#  mapview(SampleStrata_FinalGeo[SampleStrata_FinalGeo$YearSampled>0,], zcol='YearSampled')

table(SampleStrata$Sampled)
table(SampleStrata_Final$Sampled)
table(SampleStrata_Final$SampleType)
table(SampleStrata_Final$YearSampled)

check<-SampleStrata_Final %>%
  dplyr::filter(DisturbType=='FREP')
