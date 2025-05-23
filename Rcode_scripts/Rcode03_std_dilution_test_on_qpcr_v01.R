#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

########################################################################################
# R-code for making sigmoid curve plots from ABI7500 qPCR probe plots

# Make sigmoid curve qPCR plots from excel files with raw data 
# exported from ABI 7500
########################################################################################
########################################################################################
#remove everything in the working environment, without a warning!!
#rm(list=ls())
########################################################################################
# set working directory

library(xlsx)
library(dplyr)
library(ggplot2)
# define working directory
wd00 <- getwd()
# define input directory
wd01 <- "data"
# define out directory
wd03 <- "output03_qpcr_dilution_series"
#paste dirs together
wd00_wd01 <- paste0(wd00,"/",wd01)
wd00_wd03 <- paste0(wd00,"/",wd03)
#Delete any previous versions of the output directory
unlink(wd00_wd03, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd03)
#set the output directory
outdir01 <- wd00_wd03

#install packages
# #get readxl package
# if(!require(readxl)){
#   install.packages("readxl")
# }  
library(readxl)
# #get ggplot package
# if(!require(ggplot2)){
#   install.packages("ggplot2")
# }  
library(ggplot2)

# #get pdp package
# if(!require(pdp)){
#   install.packages("pdp")
# }  
library(pdp)

##########################################################################################
# begin - install packages to be able to do the ggplot below
##########################################################################################
# #get tidyverse package
# if(!require(tidyverse)){
#   install.packages("tidyverse")
# }  
library(tidyverse)

# #get broom package
# if(!require(broom)){
#   install.packages("broom")
# }  
library(broom)

# #get mgcv package
# if(!require(mgcv)){
#   install.packages("mgcv")
# }  
library(mgcv)
# #get tibble package
# if(!require(tibble)){
#   install.packages("tibble")
# }  
library(tibble)

#library(tidyverse)
#library(broom)
#library(mgcv)  #For the gam model
##########################################################################################
# end - install packages to be able to do the ggplot below
##########################################################################################


# make a range of colours for the geom_points in the ggplots
#http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with black:
clBpalt1 <- c("darkolivegreen3",
              "#8B3E2F",
              "darkorchid2",
              "darkslategray1",
              "#EE1289",
              "#458B00",
              "cornsilk4",
              "gold",
              "dodgerblue4",
              "#40E0D0",
              "#FFA54F",
              "slateblue4",
              "#76EE00",
              "#00008B")

##########################################################################################
# Note about the input files for this code
##########################################################################################
# The excel files are prepared in the ABI7500 software as 
# individual raw data needed to reproduce the amplification plots
# Each file can be exported from ABI7500 software as individual 
# excel spreadsheets , remember to export ALL data, both raw data, 
# setup, and sample data
# all spreadsheets can then be zipped together, and 
# transfered to your own computer
# unzip the zip file with all xls-files in a folder that also works as
# working directory
##########################################################################################

#list all files in wd - all the xls-files for which you want to 
# prepare plots from 
ls.fl01 <- list.files(wd00_wd01)
#make a variable with the element you want to search for
id1 <- "xls"
#grep for this variable in the list -  see this example: 
# https://stackoverflow.com/questions/35880242/r-selecting-element-from-list
ls.fl01.xls <- ls.fl01[grep(paste0(id1), ls.fl01)]

files <- ls.fl01.xls
# get the raw data files
qr.dtf <- files[grepl("^qpcr",files)]
qr.dtf <- qr.dtf[grepl("1070",qr.dtf)]
# get the filename withoutthe xls ending
fNm.qr <- gsub("\\.xls","",qr.dtf)
# get the setup  files
st.f <- files[grepl("^setup",files)]
st.f <- st.f[grepl("1070",st.f)]
# get the qpcr numbers
qpcrNos <- gsub("qpcr([0-9]+).*","\\1",qr.dtf)
# get the qpcr rundate
qpcrRundt <- gsub("^qpcr([0-9]+).*_rundate([0-9]+)_.*","\\2",qr.dtf)
qpcrRundt <- gsub("qpcr([0-9]+).*_rundate([0-9]+)_.*","\\2",st.f)
# get the setup no 
setupNo <- gsub("setup_qpcr([0-9]+).*","\\1",st.f)

# read in xls spreadsheet tab with raw flourescense data from ABI7500
rdt <- readxl::read_xls(paste0(wd00_wd01,"/",qr.dtf),
                        sheet="Raw Data",
                        skip=6)
# In the manual for ABI7500 in chapter 1, on page 2-3
# https://assets.thermofisher.com/TFS-Assets/LSG/manuals/cms_050334.pdf
# the filters are specified as monitoring the following colors
# About the Filters The 7500/7500 Fast system uses the following filters:
## Filter 	|1						      |2		  	|3	    			|4				  |	5
## Dye	   	|	 FAM  dye		      | JOE  dye| TAMRA  dye	| ROX  dye	|	Cy5 dye
##          |	 SYBR  Green dye	| VIC dye	| NED dye 		| Texas Red	|
##          |						        |		    	| Cy3 dye 		|				    |

# The present setup makes use of the FAM dye probe monitored dye
# and ROX dye as background dye
# To get these colors in separate columns
rdt$FAM <- rdt$`1`
rdt$ROX <- rdt$`4`
# read in the xlsx file with the setup of the qpcr
#stu.dt <- readxl::read_xlsx(paste0(wd00_wd01,"/",st.f))
stu.dt <- readxl::read_xls(paste0(wd00_wd01,"/",st.f))
# get the row number where the setup of well starts
rwn.wtbs <- which("Well"==stu.dt[,1])
# get a seq from where 'Well' appears and the next 96 rows
setuprws <- seq(rwn.wtbs,(rwn.wtbs+96))
# limit the setup file to only comprise these rows with the setup
qstup <- stu.dt[setuprws,]
# make the setup data frame a data frame instead of a tibble 
df_qs <- as.data.frame(qstup)

# use the 1st row as column names 
colnames(df_qs) <- df_qs[1,]
# get the data frame without the 1st row
df_qs <- df_qs[-1,]
# make a column that has 'WellNo' as column name, in the qpcr setup data frame
df_qs$WellNo <- df_qs$Well
# make a column that has 'WellNo' as column name, in the qpcr rundata data frame
rdt$WellNo  <- rdt$Well
# exclude columns that have NA as column name
ctk <- colnames(df_qs)[!is.na(colnames(df_qs))]
df_qs <- df_qs[ctk]
ctk <- colnames(rdt)[!is.na(colnames(rdt))]
rdt <- rdt[ctk]
# load the library that allows for combining data frames with left_join
library(dplyr)
# use left_join to combine the data frames
# https://cmdlinetips.com/2020/10/4-ways-to-select-columns-from-a-dataframe-with-dplyrs-select/
dfb01 <- dplyr::left_join(rdt,
                          # select all columns to include           
                          df_qs %>% dplyr::select(everything()),
                          by = "WellNo")
# get the difference between probe and the background dye
dfb01$dR <- dfb01$FAM-dfb01$ROX
# I think I need the ratio of the FAM and ROX dye
# consult this:https://assets.thermofisher.com/TFS-Assets/GSD/Application-Notes/co016741-rox-dye-for-qqpcr-app-note.pdf
dfb01$dR <- dfb01$FAM/dfb01$ROX


# use the 'scale' function to normalize the data
# the scaled difference in flourescense represents the 
# difference in the dye monitored
dfb01$ddR <- scale(dfb01$dR)



#https://www.statology.org/how-to-normalize-data-in-r/
#load dplyr package
library(dplyr)
#standardize dR 
# dfb01 <- dfb01 %>% mutate_each_(list(~scale(.) %>% as.vector),
#                                 vars = c("dR"))
# Standardize the ROX and FAM data
dfb01$dAROX <- (dfb01$ROX - mean(dfb01$ROX)) / sd(dfb01$ROX)
dfb01$dAFAM <- (dfb01$FAM - mean(dfb01$FAM)) / sd(dfb01$FAM)
# get the difference between probe and the background dye
dfb01$ddRR <- dfb01$dAFAM/dfb01$dAROX


#dfb01$ddR <- dfb01$ddRR 
# copy the column with primer and probe combination under a different
# name
dfb01$FRP.comb <- dfb01$`Primer and probe combination`
# exclude the rows where the 'FRP.comb' is NA#
# since these are empty wells without reagents added 
dfb01 <- dfb01[!is.na(dfb01$FRP.comb),]
# also exclude if the well name is NA, as these also are empty wells
dfb01 <- dfb01[!is.na(dfb01$`Well Name`),]

# get the mean and standard deviation of the ROX
dfb01mROX<- dfb01 %>%
  group_by(FRP.comb) %>%
  summarise_at(vars(ROX), list(mROX = mean, sdROX = sd))
# get the mean and standard deviation of the FAM
dfb01mFAM<- dfb01 %>%
  group_by(FRP.comb) %>%
  summarise_at(vars(FAM), list(mFAM = mean, sdFAM = sd))
# join the data frames with the mean and standard deviation
dfb01 <- left_join(dfb01, dfb01mROX, by = "FRP.comb")
dfb01 <- left_join(dfb01, dfb01mFAM, by = "FRP.comb")

# Standardize the ROX and FAM data
dfb01$ddAROX <- (dfb01$ROX - dfb01$mROX) / dfb01$sdROX
dfb01$ddAFAM <- (dfb01$FAM - dfb01$mFAM) / dfb01$sdFAM
# get the difference between probe and the background dye
dfb01$ddRR <- dfb01$ddAFAM/dfb01$ddAROX

#dfb01$ddR <- dfb01$ddRR 
#get the unique assays - to use for facet wrap
unq.FRP.comb <- unique(dfb01$FRP.comb)
#make a table of the unique assays, and turn in to a data frame
tu_df <- as.data.frame(table(unq.FRP.comb))
#count the elements
cul <- length(unq.FRP.comb)
#make a sequence of numbers and append to the data frame
tu_df$cul <- 1:cul
#get the number of elements
seq.cul <- tu_df$cul
# copy columns to have the same column names as used for making the MxPro
# plots
dfb01$wllnm2 <- dfb01$`Well Name`
dfb01$Cycles <- dfb01$Cycle
dfb01$well <- dfb01$WellNo

# make a function that can make the text in the legend in italics
# https://stackoverflow.com/questions/59554096/ggplot2-italics-in-the-legend
toexpr <- function(x, plain = NULL) {
  getfun <- function(x) {
    ifelse(x == plain, "plain", "italic")
  }
  as.expression(unname(Map(function(f,v) substitute(f(v), list(f=as.name(f), v=as.character(v))), getfun(x), x)))
}


dfb01$wllnm3 <- dfb01$wllnm2
Nms <- dfb01$wllnm3
# Find third occurrence of a special character and drop everything before that in R
# https://stackoverflow.com/questions/35088337/find-third-occurrence-of-a-special-character-and-drop-everything-before-that-in
Nms <- gsub('^(?:[^_]*_){1}','',Nms)
# also substitute underscore with space
Nms <- gsub('_',' ',Nms)
# also substitute Crefor with Crepidula fornicata
Nms <- gsub("Crefor[0-9]{3}(.*)","Crepidula fornicata\\1",Nms)
Nms <- gsub('(^(?:[^ ]* ){2}).*','\\1',Nms)
Nms <- gsub(' $','',Nms)
dfb01$wllnm3 <-  Nms


# re order the data frame by species names
dfb01 <- dfb01 %>% dplyr::arrange(wllnm3)
# substitute the 'S' with 's' in the well name
dfb01$wllnm2 <- gsub("S","s",dfb01$wllnm2)
dfb01 <- dfb01 %>% arrange(well,wllnm2)
  #View(dfb01)
# split the well name by underscore
well.splt2 <- data.frame(do.call('rbind', strsplit(as.character(dfb01$wllnm2),'_',fixed=TRUE)))
colnames(well.splt2) <- c("dilution","species","PCRsamplestd","PCRsamplestd_wellNo")

# combine the data frames
dfb01 <- cbind(dfb01,well.splt2)
uFRP.comb <- unique(dfb01$FRP.comb)
# get the difference between probe and the background dye
#dfb01$ddR_FAM_ROXl10 <- log10(dfb01$FAM-dfb01$ROX)

#https://stackoverflow.com/questions/31751022/a-function-to-create-multiple-plots-by-subsets-of-data-frame
library(ggplot2)

# Make plots of the amplification
plot01 <- ggplot(dfb01, aes(
  x = Cycles,
  y = ddR, 
  group= well, 
  color = dilution)) +
  geom_point() + 
  theme_minimal() +
  theme_bw() +
  # use a different color scale -  check this webpage for examples : https://sjspielman.github.io/introverse/articles/color_fill_scales.html
  scale_color_viridis_d(option = "inferno") +
  # scale_color_manual(values=clBpalt1,
  #                    labels = toexpr(unique(dfb01$dilution), plain = 'Wt')) +  
  # 
  facet_wrap(~FRP.comb, 
             # use free scales for the y-axis
             # see : https://stackoverflow.com/questions/18046051/setting-individual-axis-limits-with-facet-wrap-and-scales-free-in-ggplot2
              scales = "free",
             nrow = 3) + #'facet_wrap' subsets by column value in dataframe
  # # see : https://r-charts.com/ggplot2/facets/
  theme(strip.text = element_text(#face = "bold",
    color = "black",
    hjust = 0,
    size = 10),
    strip.background = element_rect(fill = c("white"),
                                    #linetype = "solid",
                                    color = "white",
                                    linewidth = 1)) +
  geom_line() + #add lines
  labs(color='Extracted sample') + # change the label for the legend
  labs(color='Ekstraheret prøve fra') + # change the label for the legend
  labs(x = "qPCR cyklusser", y = "ddR") +
  #ggtitle(fNm.qr) # add a title to the plot - here the filename is used
  ggtitle("qpcr1070")
# modify the plot background : https://www.statology.org/ggplot-background-color/


plt01 <- plot01

plt01
# get the qpcr no to include in the output figure
qpcrNo <- qpcrNos
# substitute to remove the xls ending and the qpcr number
tmprundt <- gsub("\\.xls","",qpcrRundt)
tmprundt <- sub("*.*pcr*.*_","",tmprundt)
qpcrRundt<- tmprundt
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = plt01, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outdir01,"/Fig05_qpcrrun",
                           qpcrNo,"rundate",qpcrRundt,".png"),
         width=210*1.6,height=297*0.6,
         #width=297,height=210,
         units="mm",dpi=300)
}

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________


#list all files in wd - all the xls-files for which you want to 
# prepare plots from 
ls.fl01 <- list.files(wd00_wd01)
#make a variable with the element you want to search for
id1 <- "xls"
#grep for this variable in the list -  see this example: 
# https://stackoverflow.com/questions/35880242/r-selecting-element-from-list
ls.fl01.xls <- ls.fl01[grep(paste0(id1), ls.fl01)]

files <- ls.fl01.xls

# define qpcr number to grep for 
qpcrNo.tgrf <- 1071
# get the raw data files
qr.dtf <- files[grepl("^qpcr",files)]
qr.dtf <- qr.dtf[grepl(qpcrNo.tgrf,qr.dtf)]
# get the filename withoutthe xls ending
fNm.qr <- gsub("\\.xls","",qr.dtf)
# get the setup  files
st.f <- files[grepl("^setup",files)]
st.f <- st.f[grepl(qpcrNo.tgrf,st.f)]
# get the qpcr numbers
qpcrNos <- gsub("qpcr([0-9]+).*","\\1",qr.dtf)
# get the qpcr rundate
qpcrRundt <- gsub("^qpcr([0-9]+).*_rundate([0-9]+)_.*","\\2",qr.dtf)
qpcrRundt <- gsub("qpcr([0-9]+).*_rundate([0-9]+)_.*","\\2",st.f)
# get the setup no 
setupNo <- gsub("setup_qpcr([0-9]+).*","\\1",st.f)

# read in xls spreadsheet tab with raw flourescense data from ABI7500
rdt <- readxl::read_xls(paste0(wd00_wd01,"/",qr.dtf),
                        sheet="Raw Data",
                        skip=6)

# read in xls spreadsheet tab Delta-Rn flourescense data from ABI7500
adt <- readxl::read_xls(paste0(wd00_wd01,"/",qr.dtf),
                        sheet="Amplification Data",
                        skip=7)
# In the manual for ABI7500 in chapter 1, on page 2-3
# https://assets.thermofisher.com/TFS-Assets/LSG/manuals/cms_050334.pdf
# the filters are specified as monitoring the following colors
# About the Filters The 7500/7500 Fast system uses the following filters:
## Filter 	|1						      |2		  	|3	    			|4				  |	5
## Dye	   	|	 FAM  dye		      | JOE  dye| TAMRA  dye	| ROX  dye	|	Cy5 dye
##          |	 SYBR  Green dye	| VIC dye	| NED dye 		| Texas Red	|
##          |						        |		    	| Cy3 dye 		|				    |

# The present setup makes use of the FAM dye probe monitored dye
# and ROX dye as background dye
# To get these colors in separate columns
rdt$FAM <- rdt$`1`
rdt$ROX <- rdt$`4`
# read in the xlsx file with the setup of the qpcr
#stu.dt <- readxl::read_xlsx(paste0(wd00_wd01,"/",st.f))
stu.dt <- readxl::read_xls(paste0(wd00_wd01,"/",st.f))
# get the row number where the setup of well starts
rwn.wtbs <- which("Well"==stu.dt[,1])
# get a seq from where 'Well' appears and the next 96 rows
setuprws <- seq(rwn.wtbs,(rwn.wtbs+96))
# limit the setup file to only comprise these rows with the setup
qstup <- stu.dt[setuprws,]
# make the setup data frame a data frame instead of a tibble 
df_qs <- as.data.frame(qstup)

# use the 1st row as column names 
colnames(df_qs) <- df_qs[1,]
# get the data frame without the 1st row
df_qs <- df_qs[-1,]
# make a column that has 'WellNo' as column name, in the qpcr setup data frame
df_qs$WellNo <- df_qs$Well
# make a column that has 'WellNo' as column name, in the qpcr rundata data frame
rdt$WellNo  <- rdt$Well
adt$WellNo  <- adt$Well
# exclude columns that have NA as column name
ctk <- colnames(df_qs)[!is.na(colnames(df_qs))]
df_qs <- df_qs[ctk]
ctk <- colnames(rdt)[!is.na(colnames(rdt))]
actk <- colnames(adt)[!is.na(colnames(adt))]
rdt <- rdt[ctk]
adt <- adt[actk]
# load the library that allows for combining data frames with left_join
library(dplyr)
# use left_join to combine the data frames
# https://cmdlinetips.com/2020/10/4-ways-to-select-columns-from-a-dataframe-with-dplyrs-select/
dfb01 <- dplyr::left_join(rdt,
                          # select all columns to include           
                          df_qs %>% dplyr::select(everything()),
                          by = "WellNo")
# also join the amplification data  
dfa01 <- dplyr::left_join(adt,
                          # select all columns to include           
                          df_qs %>% dplyr::select(everything()),
                          by = "WellNo")

# get the difference between probe and the background dye
dfb01$dR <- dfb01$FAM-dfb01$ROX
#the  dR value should be the 'ΔRn' in the amplication data frame 
dfa01$dR <-dfa01$ΔRn 
# I think I need the ratio of the FAM and ROX dye
# consult this:https://assets.thermofisher.com/TFS-Assets/GSD/Application-Notes/co016741-rox-dye-for-qqpcr-app-note.pdf
dfb01$dR <- dfb01$FAM/dfb01$ROX


# use the 'scale' function to normalize the data
# the scaled difference in flourescense represents the 
# difference in the dye monitored
dfb01$ddR <- scale(dfb01$dR)



#https://www.statology.org/how-to-normalize-data-in-r/
#load dplyr package
library(dplyr)
#standardize dR 
# dfb01 <- dfb01 %>% mutate_each_(list(~scale(.) %>% as.vector),
#                                 vars = c("dR"))
# Standardize the ROX and FAM data
dfb01$dAROX <- (dfb01$ROX - mean(dfb01$ROX)) / sd(dfb01$ROX)
dfb01$dAFAM <- (dfb01$FAM - mean(dfb01$FAM)) / sd(dfb01$FAM)
# get the difference between probe and the background dye
dfb01$ddRR <- dfb01$dAFAM/dfb01$dAROX


#dfb01$ddR <- dfb01$ddRR 
# copy the column with primer and probe combination under a different
# name
dfb01$FRP.comb <- dfb01$`Primer and probe combination`
dfa01$FRP.comb <- dfa01$`Primer and probe combination`
# exclude the rows where the 'FRP.comb' is NA#
# since these are empty wells without reagents added 
dfb01 <- dfb01[!is.na(dfb01$FRP.comb),]
dfa01 <- dfb01[!is.na(dfa01$FRP.comb),]
# also exclude if the well name is NA, as these also are empty wells
dfb01 <- dfb01[!is.na(dfb01$`Well Name`),]
dfa01 <- dfb01[!is.na(dfa01$`Well Name`),]

# get the mean and standard deviation of the ROX
dfb01mROX<- dfb01 %>%
  group_by(FRP.comb) %>%
  summarise_at(vars(ROX), list(mROX = mean, sdROX = sd))
# get the mean and standard deviation of the FAM
dfb01mFAM<- dfb01 %>%
  group_by(FRP.comb) %>%
  summarise_at(vars(FAM), list(mFAM = mean, sdFAM = sd))
# join the data frames with the mean and standard deviation
dfb01 <- left_join(dfb01, dfb01mROX, by = "FRP.comb")
dfb01 <- left_join(dfb01, dfb01mFAM, by = "FRP.comb")

# Standardize the ROX and FAM data
dfb01$ddAROX <- (dfb01$ROX - dfb01$mROX) / dfb01$sdROX
dfb01$ddAFAM <- (dfb01$FAM - dfb01$mFAM) / dfb01$sdFAM
# get the difference between probe and the background dye
dfb01$ddRR <- dfb01$ddAFAM/dfb01$ddAROX

#dfb01$ddR <- dfb01$ddRR 
#get the unique assays - to use for facet wrap
unq.FRP.comb <- unique(dfb01$FRP.comb)
unq.FRP.comb_a <- unique(dfa01$FRP.comb)
#make a table of the unique assays, and turn in to a data frame
tu_df <- as.data.frame(table(unq.FRP.comb))
tu_dfa <- as.data.frame(table(unq.FRP.comb_a))
#count the elements
cul <- length(unq.FRP.comb)
cul_a <- length(unq.FRP.comb_a)
#make a sequence of numbers and append to the data frame
tu_df$cul <- 1:cul
tu_dfa$cul <- 1:cul_a
#get the number of elements
seq.cul <- tu_df$cul
seq.cula <- tu_dfa$cul
# copy columns to have the same column names as used for making the MxPro
# plots
dfb01$wllnm2 <- dfb01$`Well Name`
dfb01$Cycles <- dfb01$Cycle
dfb01$well <- dfb01$WellNo
# also copy columns for the amplification data frame
dfa01$wllnm2 <- dfa01$`Well Name`
dfa01$Cycles <- dfa01$Cycle
dfa01$well <- dfa01$WellNo

# make a function that can make the text in the legend in italics
# https://stackoverflow.com/questions/59554096/ggplot2-italics-in-the-legend
toexpr <- function(x, plain = NULL) {
  getfun <- function(x) {
    ifelse(x == plain, "plain", "italic")
  }
  as.expression(unname(Map(function(f,v) substitute(f(v), list(f=as.name(f), v=as.character(v))), getfun(x), x)))
}


dfb01$wllnm3 <- dfb01$wllnm2
dfa01$wllnm3 <- dfa01$wllnm2
Nms <- dfb01$wllnm3
Nmsa <- dfa01$wllnm3
# Find third occurrence of a special character and drop everything before that in R
# https://stackoverflow.com/questions/35088337/find-third-occurrence-of-a-special-character-and-drop-everything-before-that-in
Nms <- gsub('^(?:[^_]*_){1}','',Nms)
Nmsa <- gsub('^(?:[^_]*_){1}','',Nmsa)
# also substitute underscore with space
Nms <- gsub('_',' ',Nms)
Nmsa <- gsub('_',' ',Nmsa)
# also substitute Crefor with Crepidula fornicata
Nms <- gsub("Crefor[0-9]{3}(.*)","Crepidula fornicata\\1",Nms)
Nms <- gsub('(^(?:[^ ]* ){2}).*','\\1',Nms)
Nms <- gsub(' $','',Nms)
dfa01$wllnm3 <-  Nmsa


# re order the data frame by species names
dfb01 <- dfb01 %>% dplyr::arrange(wllnm3)
dfa01 <- dfa01 %>% dplyr::arrange(wllnm3)
# substitute the 'S' with 's' in the well name
dfb01$wllnm2 <- gsub("S","s",dfb01$wllnm2)
dfb01 <- dfb01 %>% arrange(well,wllnm2)
#View(dfb01)
# split the well name by underscore
well.splt2 <- data.frame(do.call('rbind', strsplit(as.character(dfb01$wllnm2),'_',fixed=TRUE)))
colnames(well.splt2) <- c("dilution","species","PCRsamplestd","PCRsamplestd_wellNo")
# split the well name by underscore
well.splt2a <- data.frame(do.call('rbind', strsplit(as.character(dfa01$wllnm2),'_',fixed=TRUE)))
colnames(well.splt2a) <- c("dilution","species","PCRsamplestd","PCRsamplestd_wellNo")
# combine the data frames
dfb01 <- cbind(dfb01,well.splt2)
dfa01 <- cbind(dfa01,well.splt2a)
uFRP.comb <- unique(dfb01$FRP.comb)
uFRP.comb_a <- unique(dfa01$FRP.comb)
# get the difference between probe and the background dye
#dfb01$ddR_FAM_ROXl10 <- log10(dfb01$FAM-dfb01$ROX)

#https://stackoverflow.com/questions/31751022/a-function-to-create-multiple-plots-by-subsets-of-data-frame
library(ggplot2)

# Make plots of the amplification
plot01 <- ggplot(dfb01, aes(
  x = Cycles,
  y = ddR, 
  group= well, 
  color = dilution)) +
  geom_point() + 
  theme_minimal() +
  theme_bw() +
  # use a different color scale -  check this webpage for examples : https://sjspielman.github.io/introverse/articles/color_fill_scales.html
  scale_color_viridis_d(option = "inferno") +
  # scale_color_manual(values=clBpalt1,
  #                    labels = toexpr(unique(dfb01$dilution), plain = 'Wt')) +  
  # 
  facet_wrap(~FRP.comb, 
             # use free scales for the y-axis
             # see : https://stackoverflow.com/questions/18046051/setting-individual-axis-limits-with-facet-wrap-and-scales-free-in-ggplot2
             scales = "free",
             nrow = 3) + #'facet_wrap' subsets by column value in dataframe
  # # see : https://r-charts.com/ggplot2/facets/
  theme(strip.text = element_text(#face = "bold",
    color = "black",
    hjust = 0,
    size = 10),
    strip.background = element_rect(fill = c("white"),
                                    #linetype = "solid",
                                    color = "white",
                                    linewidth = 1)) +
  geom_line() + #add lines
  labs(color='Extracted sample') + # change the label for the legend
  labs(color='Ekstraheret prøve fra') + # change the label for the legend
  labs(x = "qPCR cyklusser", y = "ddR") +
  #ggtitle(fNm.qr) # add a title to the plot - here the filename is used
  ggtitle(paste0("qpcr",qpcrNo.tgrf))
# modify the plot background : https://www.statology.org/ggplot-background-color/


plt01 <- plot01

plt01


# Make plots of the amplification
plot01 <- ggplot(dfa01, aes(
  x = Cycles,
  y = dR, 
  group= well, 
  color = dilution)) +
  geom_point() + 
  theme_minimal() +
  theme_bw() +
  # use a different color scale -  check this webpage for examples : https://sjspielman.github.io/introverse/articles/color_fill_scales.html
  scale_color_viridis_d(option = "inferno") +
  # scale_color_manual(values=clBpalt1,
  #                    labels = toexpr(unique(dfb01$dilution), plain = 'Wt')) +  
  # 
  facet_wrap(~FRP.comb, 
             # use free scales for the y-axis
             # see : https://stackoverflow.com/questions/18046051/setting-individual-axis-limits-with-facet-wrap-and-scales-free-in-ggplot2
             scales = "free",
             nrow = 3) + #'facet_wrap' subsets by column value in dataframe
  # # see : https://r-charts.com/ggplot2/facets/
  theme(strip.text = element_text(#face = "bold",
    color = "black",
    hjust = 0,
    size = 10),
    strip.background = element_rect(fill = c("white"),
                                    #linetype = "solid",
                                    color = "white",
                                    linewidth = 1)) +
  geom_line() + #add lines
  labs(color='Extracted sample') + # change the label for the legend
  labs(color='kopier i std') + # change the label for the legend
  labs(x = "qPCR cyklusser", y = "dR") +
  #ggtitle(fNm.qr) # add a title to the plot - here the filename is used
  ggtitle(paste0("qpcr",qpcrNo.tgrf))
# modify the plot background : https://www.statology.org/ggplot-background-color/


plt02 <- plot01

plt02

# get the qpcr no to include in the output figure
qpcrNo <- qpcrNos
# substitute to remove the xls ending and the qpcr number
tmprundt <- gsub("\\.xls","",qpcrRundt)
tmprundt <- sub("*.*pcr*.*_","",tmprundt)
qpcrRundt<- tmprundt
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = plt01, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outdir01,"/Fig05_qpcrrun",
                           qpcrNo,"rundate",qpcrRundt,".png"),
         width=210*1.6,height=297*0.6,
         #width=297,height=210,
         units="mm",dpi=300)
}
