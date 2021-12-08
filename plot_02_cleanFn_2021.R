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
#Parse out columns to component parts
#Break up variables
#case 1 - split F2 into c(F2_A1, F2_A2, F2_B1, F2_B2)
WForm1 <- WForm %>% mutate(
  F2_A1 := case_when(
    F2=="A1" ~ 1,
    TRUE ~ 0),
  F2_A2 := case_when(
    F2=="A2" ~ 1,
    TRUE ~ 0),
  F2_B1 := case_when(
    F2=="B1" ~ 1,
    TRUE ~ 0),
  F2_B2 := case_when(
    F2=="B2" ~ 1,
    TRUE ~ 0)
)

#Make list of variables that require parsing
ParseVars<-c('F3','F56','F57','F58','F61')
#Number of sub-categories for each variable
NparseVars<-c(8,3,7,9,4)

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
    df1<-WForm1 %>%
    rowwise() %>%
    mutate(Vparts=(strsplit(!!rlang::sym(ParseVars[x]), ","))) %>%
    mutate(VpartsN=list(parse_number(Vparts))) %>%
    dplyr::select((ParseVars[x]),Vparts,VpartsN)
    #SplitFn1(x,df1$VpartsN)
    SplitFn1(x,df1)
})
#Combine generated form sub-variables with original data.frame
WForm2 <- cbind(WForm1,(do.call(cbind, df3)))

#Split out form binary variables that are contained in 1 variable
ParseVars<-c('F4','F5','F6','F7','F8','F9','F10','F11','F12','F13',
             'F14','F15','F16','F17','F18','F19','F20','F21','F24','F25',
             'F26','F27','F28','F29','F30','F31','F32','F33','F34','F35',
             'F36','F37','F38','F39','F40','F41', 'F43','F44','F47','F48',
             'F50','F51','F52','F53','F54','F55')
#Number of sub-categories for each variable
NparseVars<-c(4,5,5,5,3,4,4,5,4,5,
              5,5,5,5,3,6,5,6,5,5,
              5,6,3,6,6,6,6,6,6,4,
              5,6,3,4,5,4,5,4,3,3,
              5,3,4,6,6,7)

df4<-lapply(1:length(ParseVars), function(x) {
  df1<-WForm2 %>%
    rowwise() %>%
    mutate(VpartsN=parse_number(!!rlang::sym(ParseVars[x]))) %>%
    dplyr::select(ParseVars[x],VpartsN)
  SplitFn1(x,df1)
})
WForm3 <- cbind(WForm2,(do.call(cbind, df4)))

#Modify y/n to 1/0
WForm4<-WForm3 %>%
  mutate(across(c(F22,F23,F42,F49), ~ case_when(. == "yes" ~ "1", TRUE ~ "0"))) %>%
  mutate(across(everything(), as.character))

#Column join 2020 with 2021
#Transform 2020 data so can join with 2021 - then can transpose in excel back to original format
WManual2020T<-data.frame(t(WManual2020))
#Get the column names from the first row then deletee row
colnames(WManual2020T)=WManual2020T[c(1),]
WManual2020T<-WManual2020T[-1,]

#Set Batch_ID and get Wetland_Co from Paul's SiteID_xtab file
WManual2020T <- WManual2020T %>%
  mutate(Batch_ID=parse_number(rownames(WManual2020T))) %>%
  left_join(SiteID_xtab) %>%
  mutate(across(everything(), as.character))
#WriteXLS(WManual2020T,file.path(dataOutDir,paste('WManual2020T.xlsx',sep='')),AllText=TRUE)

#Join the data together and write as excel spreadsheet
WForm5<-WManual2020T %>%
  dplyr::bind_rows(WForm4)

WetlandPlotData <- WForm5 %>%
  dplyr::select(Batch_ID,
              F1, starts_with('F1_'),F2, starts_with('F2_'),F3, starts_with('F3_'),F4, starts_with('F4_'),
              F5, starts_with('F5_'),F6, starts_with('F6_'),F7, starts_with('F7_'),F8, starts_with('F8_'),
              F9, starts_with('F9_'),F10, starts_with('F10_'),F11, starts_with('F11_'),F12, starts_with('F12_'),
              F13, starts_with('F13_'),F14, starts_with('F14_'),F15, starts_with('F15_'),F16, starts_with('F16_'),
              F17, starts_with('F17_'),F18, starts_with('F18_'),F19, starts_with('F19_'),F20, starts_with('F20_'),
              F21, starts_with('F21_'),F22, starts_with('F22_'),F23, starts_with('F23_'),F24, starts_with('F24_'),
              F25, starts_with('F25_'),F26, starts_with('F26_'),F27, starts_with('F27_'),F28, starts_with('F28_'),
              F29, starts_with('F29_'),F30, starts_with('F30_'),F31, starts_with('F31_'),F32, starts_with('F32_'),
              F33, starts_with('F33_'),F34, starts_with('F34_'),F35, starts_with('F35_'),F36, starts_with('F36_'),
              F37, starts_with('F37_'),F38, starts_with('F38_'),F39, starts_with('F39_'),F40, starts_with('F40_'),
              F41, starts_with('F41_'),F42, starts_with('F42_'),F43, starts_with('F43_'),F44, starts_with('F44_'),
              F45,starts_with('F45_'), F46a, F46b,F47, starts_with('F47_'),F48, starts_with('F48_'),
              F49,F50, starts_with('F50_'),F51, starts_with('F51_'),F52, starts_with('F52_'),
              F53, starts_with('F53_'),F54, starts_with('F54_'),F55, starts_with('F55_'),F56, starts_with('F56_'),
              F57, starts_with('F57_'),F58, starts_with('F58_'),F59, starts_with('F59_'),F60, starts_with('F60_'),
              F61,starts_with('F61_'))

WriteXLS(WetlandPlotData,file.path(dataOutDir,paste('WetlandPlotData.xlsx',sep='')),AllText=TRUE)

SiteID_xtab2021<-WForm5 %>%
  dplyr::select(Batch_ID, Wetland_Co)

WriteXLS(SiteID_xtab2021,file.path(dataOutDir,paste('SiteID_xtab2021.xlsx',sep='')),AllText=TRUE)
