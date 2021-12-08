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

#Clean up the Survey123 data
WFormS<-WForm %>%
  dplyr::select(Batch_ID,Wetland_Co, Wetland_CoIn, Investigators, Date,
                starts_with('S')) %>%
    mutate(across(everything(), as.character))

#Make list of variables that require parsing
#S1 S1_3
ParseVars<-c('S1','S2','S3','S4','S5','S6')
#Number of sub-categories for each variable
NparseVars<-c(10,5,9,8,8,2)

#Function to split a Form variable that has multiple entries into
#separate variables
SplitFn1 <- function(i,df) {
  df2<-lapply(1:NparseVars[i], function(j) {
    FormVName<-paste0(ParseVars[i],"_",j)
    df %>%
      mutate(!!FormVName := case_when(
        #is.element(j, VpN) ~ 1,
        is.element(j, VpartsN) ~ 1,
        TRUE ~ 0)) %>%
      dplyr::select(!!rlang::sym(FormVName))
  })
  do.call(cbind, df2)
}
#Loop through each Variable to split out and call the function
#that splits it into separate variables
df3<-lapply(1:length(ParseVars), function(x) {
  df1<-WFormS %>%
    rowwise() %>%
    mutate(Vparts=(strsplit(!!rlang::sym(ParseVars[x]), ","))) %>%
    mutate(VpartsN=list(parse_number(Vparts))) %>%
    dplyr::select((ParseVars[x]),Vparts,VpartsN)
  #SplitFn1(x,df1$VpartsN)
  SplitFn1(x,df1)
})
#Combine generated form sub-variables with original data.frame
WFormS2 <- cbind(WFormS,(do.call(cbind, df3))) %>%
  mutate(across(everything(), as.character))


#Split out form binary variables that are contained in 1 variable
ParseVars<-c('S1_11','S1_12','S1_13','S1_14','S2_6','S2_7','S2_8','S3_10','S3_11','S3_12',
             'S4_10','S4_11','S4_12','S5_9','S5_10','S5_11','S5_12','S6_3','S6_4')

WFormS3<-WFormS2 %>%
  mutate(across(all_of(ParseVars), parse_number)) %>%
  mutate(across(everything(), as.character))

  #Column join 2020 with 2021
#Transform 2020 data so can join with 2021 - then can transpose in excel back to original format
WFormStress2020T<-data.frame(t(WFormStress2020))
#Get the column names from the first row then deletee row
colnames(WFormStress2020T)=WFormStress2020T[c(1),]
WFormStress2020T<-WFormStress2020T[-1,]
#Set all types to character - same as WForm
#Set Batch_ID and get Wetland_Co from Paul's SiteID_xtab file
WFormStress2020T <- WFormStress2020T %>%
  mutate(Batch_ID=parse_number(rownames(WFormStress2020T))) %>%
  left_join(SiteID_xtab) %>%
  mutate(across(everything(), as.character))


#WFormStress2020T$Wetland_Co <- rownames(WFormStress2020T)

#WriteXLS(WManual2020T,file.path(dataOutDir,paste('WManual2020T.xlsx',sep='')),AllText=TRUE)

#Join the data together and write as excel spreadsheet
WetlandStressPlotData<-WFormStress2020T %>%
  dplyr::bind_rows(WFormS3)  %>%
  dplyr::select(Batch_ID,
                S1, starts_with('S1_'),S2, starts_with('S2_'),
                S3, starts_with('S3_'),S4, starts_with('S4_'),
                S5, starts_with('S5_'),S6, starts_with('S6_'))


WriteXLS(WetlandStressPlotData,file.path(dataOutDir,paste('WetlandStressPlotData.xlsx',sep='')),AllText=TRUE)

