#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
#remove everything in the working environment, without a warning!!
#rm(list=ls())

# load the libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
# define working directory
wd00 <- getwd()
#define input file  directory
wd01 <- "data"
# paste together working directory and input file  directory
wd00_01 <- paste0(wd00,"/",wd01)
# define out directory
wd04 <- "output04_ddpcr_results"
#paste dirs together
wd00_wd01 <- paste0(wd00,"/",wd01)
wd00_wd04 <- paste0(wd00,"/",wd04)
#Delete any previous versions of the output directory
unlink(wd00_wd04, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd04)
#set the output directory
outdir01 <- wd00_wd04

#grep for the '.csv' files in the directory, and place in a list
lst_fcsv <- list.files(wd00_01)[grep("\\.csv",list.files(wd00_01))]
# grep only files that begin with ddpcr and 4 digits 
lst_fcsv <- lst_fcsv[grepl("ddpcr[0-9]{4}_",lst_fcsv, ignore.case = F)]
# grep only files that contain the string 'measure_conc_on_filters'
lst_fcsv <- lst_fcsv[grepl("_measure_conc_on_filters",lst_fcsv, ignore.case = F)]

# add wd to all files
lst_inf02 <- wd00_01 %>% paste0("/",lst_fcsv)
# https://statisticsglobe.com/merge-csv-files-in-r
library(readr)
# To read in all files from the list, and also make sure all columns are 
# characters, and the bind rows in to a tibble
# make a name vector  with the file name from the list
names(lst_inf02) <- basename(lst_fcsv)
# use the list of files 
tibl_csvs01 <- lst_inf02 %>%
  # read in the csv files , making sure columns are characte
  lapply(read_csv, col_types=cols(.default = col_character())) %>%
  # bind the rows together, and use the file name as a id-column
  bind_rows(.id="file")
# gsub to get the ddPCR number
tmpNm <- gsub("(.*?)(_.*)", "\\2", tibl_csvs01$file) 
tmpNm <- sub("_", "", tmpNm) 
tibl_csvs01$ddpcrNo <- gsub("(.*?)(_.*)", "\\1", tmpNm) 
# copy the sample description columns
tibl_csvs01$smpldscr01 <- tibl_csvs01$`Sample description 1`
tibl_csvs01$smpldscr02 <- tibl_csvs01$`Sample description 2`
tibl_csvs01$smpldscr03 <- tibl_csvs01$`Sample description 3`
tibl_csvs01$smpldscr04 <- tibl_csvs01$`Sample description 4`
# get the rows in the 'Target' column, that does not have the 6 letter 
# species abbreviation
tibl_csvs01$Target[!grepl("[A-z]",tibl_csvs01$Target)] <- tibl_csvs01$smpldscr01[!grepl("[A-z]",tibl_csvs01$Target)]
# get the 6 letter name for the assay applied , loose any extra characters beoynd 6
tibl_csvs01$smpldscr04<- substring(tibl_csvs01$Target,1,6)
# make the 'MST' samples in the 'smpldscr01' column appear as "unknown"
tibl_csvs01$smpldscr01[grepl("MST",tibl_csvs01$smpldscr01)] <- "unknown"
tibl_csvs01$smpldscr01[grepl("NEKFeb",tibl_csvs01$smpldscr01)] <- "unknown"
# change for the 6 letter sample abbreviation
tibl_csvs01$smpldscr01[grepl("^[A-z]{6}$",tibl_csvs01$smpldscr01)] <- tibl_csvs01$smpldscr03[grepl("^[A-z]{6}$",tibl_csvs01$smpldscr01)]
# replace the NAs
tibl_csvs01$smpldscr01[is.na(tibl_csvs01$smpldscr01)] <- "unknown"
# edit the column for the standard dilution level
tibl_csvs01$smpldscr03 <- NA
tibl_csvs01$smpldscr03[grepl("A01",tibl_csvs01$Well)] <- "1E4"
tibl_csvs01$smpldscr03[grepl("B01",tibl_csvs01$Well)] <- "1E3"
tibl_csvs01$smpldscr03[grepl("C01",tibl_csvs01$Well)] <- "1E2"
tibl_csvs01$smpldscr03[grepl("D01",tibl_csvs01$Well)] <- "1E1"
tibl_csvs01$smpldscr03[grepl("E01",tibl_csvs01$Well)] <- "1E0"
tibl_csvs01$smpldscr03[grepl("F01",tibl_csvs01$Well)] <- "1E-1"
tibl_csvs01$smpldscr03[grepl("G01",tibl_csvs01$Well)] <- "1E-2"
tibl_csvs01$smpldscr03[grepl("H01",tibl_csvs01$Well)] <- "0"
# add a column for vol of template added
tibl_csvs01$smpldscr02 <- "voltempl_10uL"
# make the tibble as a data frame
df_ddP  <- as.data.frame(tibl_csvs01, stringsAsFactors = F)
# copy the 'Well' column to another column named 'WellNumber'
# to allow formusing left_join later on
df_ddP$WellNumber <-  df_ddP$Well
# get the columns with the 'Conc' in the name
clNm <- colnames(df_ddP)
# get the index number for the columns with 'Conc' in the name
idxN <- which(grepl("Conc",clNm))
# use this column to get the concentration in copies per uL
df_ddP$conc.copies.per.uL <-   df_ddP[,idxN]
# make the concentration column numeric
df_ddP$conc.copies.per.uL <- as.numeric(df_ddP$conc.copies.per.uL)

# get a list of files in the input directory
ls.fl01 <- list.files(wd00_wd01)
#make a variable with the element you want to search for
# here the id to search for is 'xls'
id1 <- "xls"
#grep for this variable in the list -  see this example: 
# https://stackoverflow.com/questions/35880242/r-selecting-element-from-list
ls.fl01.xls <- ls.fl01[grep(paste0(id1), ls.fl01)]
# add the list of files to a vector
files <- ls.fl01.xls
# get the setup  files
st.f <- files[grepl("^setup",files)]
# get the only the ddpcr setup  files
st.f <- st.f[grepl("ddpcr",st.f)]
# get the only the 'Gadmor' setup  files
st.f <- st.f[grepl("Gadmor",st.f)]

#  load the readxl library
library(readxl)
# make a name vector  with the file name from the list
names(st.f) <- basename(st.f)
# append the path to the list of files 
xlsfile.lst  <- paste0(wd00_wd01,"/",st.f)
# read in the xlsx files
df.xlslst <- lapply(xlsfile.lst, read_excel)
# make an empty list to add to
lstxlsdf <- list()
# iterate over the list of data frames
for (i in 1:length(df.xlslst)) {
  # get the row number where the setup of well starts
  rwn.wtbs <- which("WellNumber"==(df.xlslst[[i]])[,1])
  # get the file name
  flNm.origin <- st.f[i]
  # get a seq from where 'Well' appears and the next 96 rows
  setuprws <- seq(rwn.wtbs,(rwn.wtbs+96))
  # limit the setup file to only comprise these rows with the setup
  qstup <- (df.xlslst[[i]])[setuprws,]
  # make the setup data frame a data frame instead of a tibble 
  df_qs <- as.data.frame(qstup)
  # use the 1st row as column names 
  colnames(df_qs) <- df_qs[1,]
  # get the data frame without the 1st row
  df_qs <- df_qs[-1,]
  # add the file name as a column
  df_qs$flNm <- flNm.origin
  # add the limited data frame to the empty list
  lstxlsdf[[i]] <- df_qs
}
# bind the data frames in the list together
df_stp <- bind_rows(lstxlsdf, .id = "column_label")
# gsub to get the ddPCR number
tmpNm <- gsub("(.*?)(_.*)", "\\2", df_stp$flNm) 
tmpNm <- sub("_", "", tmpNm) 
df_stp$ddpcrNo <- gsub("(.*?)(_.*)", "\\1", tmpNm) 
# combine the  data frames using 'WellNumber' and 'ddpcrNo' columns
df_ddP02 <- dplyr::left_join(df_stp,df_ddP, by=c("WellNumber","ddpcrNo"))


# copy the 'WellName' column to another column named 'WellNm02' 
df_ddP02$WellNm02 <- df_ddP02$WellName
# limit to 'unknown' well type and welltype tha are not NA
TwN  <- df_ddP02$WellNm02[(df_ddP02$WellType=="unknown" & !is.na(df_ddP02$WellType))]
#pad with zeros to three characters for 
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
TwN <- ifelse(nchar(TwN)<3,stringr::str_pad(TwN, 3, pad = "0"),TwN)
TwN <- as.character(TwN)
# add the modified 'WellNm02' column to the data frame, using the 
# same limitatons as above
df_ddP02$WellNm02[(df_ddP02$WellType=="unknown" & !is.na(df_ddP02$WellType))] <- TwN
# modify the 'WellNm02' column to only contain 'STD' for the 'std' wells
df_ddP02$WellNm02 <- gsub("std.*","STD",df_ddP02$WellNm02)
df_ddP02$unq_fltNmb <- df_ddP02$WellNm02
# define directory with previous table
wd02 <- "output02_figures_for_analysis_of_filters"
#paste dirs together
wd00_wd02 <- paste0(wd00,"/",wd02)
# define the path to the csv file
path_flwd00_wd02_tabl04 <- paste0(wd00_wd02,"/Table04_filters_ekstraheret.csv")
# Write the data frame to a csv file
df_extr01 <- read_csv(path_flwd00_wd02_tabl04)
# make the 'df_extr01' tibble  a data frame
df_extr01 <- as.data.frame(df_extr01)
df_extr01$unq_fltNmb <- as.character(df_extr01$unkFilt.nr)
TwN <- df_extr01$unq_fltNmb
#pad with zeros to three characters for 
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
TwN <- ifelse(nchar(TwN)<3,stringr::str_pad(TwN, 3, pad = "0"),df_extr01$unq_fltNmb)
TwN <- as.character(TwN)
# add the modified 'unq_fltNmb' column to the data frame
df_extr01$unq_fltNmb <- TwN
# combine the  data frames using left_join and the 'unq_fltNmb' column
# as a common column 
df_ddP03 <- left_join(df_ddP02,df_extr01, by="unq_fltNmb")
df_ddP03$filt.pszc <- as.character(df_ddP03$filt.psz)
# make the conc per uL numeric
df_ddP03$ddpcr_copies_uL <-as.numeric(df_ddP03$conc.copies.per.uL)
# get copy number per uL per L of filtered water
df_ddP03$ddpcr_copies_uL_per_L <- df_ddP03$ddpcr_copies_uL/(df_ddP03$fltvol.mL/1000)
library(ggrepel)

# get the filter categories
unq.filt.mbr <- unique(df_ddP03$filt.mbr)
df_ddP03$prv.type <- NA
df_ddP03$prv.type[grepl("NK",df_ddP03$Filt.mbr.pore)] <- "NK"
filt.mbrNm <- unq.filt.mbr[!grepl("No",unq.filt.mbr)]
filt.mbrNm <- filt.mbrNm[!is.na(filt.mbrNm)]
filt.mbrNms <- paste(filt.mbrNm,  collapse="|")
# assiign 'akv' or 'NK' depending on whether sample is negative control or 
# a sample from the aqaurium
df_ddP03$prv.type[grepl(filt.mbrNms,df_ddP03$Filt.mbr.pore)] <-  "akvarie"
df_ddP03$prv.type[grepl("NK",df_ddP03$Filt.mbr.pore)] <- "NK"
# check the copy count in the "3.00" filteres
df_ddP03$ddpcr_copies_uL_per_L[grepl("3.00",df_ddP03$Filt.mbr.pore2)]

# make a scatter plot
p <- ggplot(data = df_ddP03,
       aes(x = extrconc_ngpuL_Pfltvol.L, 
           y = ddpcr_copies_uL_per_L, 
           color= prv.type,
           fill = filt.pszc,
           shape = filt.mbr )) +
  geom_point(size=4) + 
  theme_minimal() + 
  scale_fill_viridis_d(alpha = 0.6, direction = 1) +
  ggrepel::geom_text_repel(size = 2.8,
                           aes(label=Filt.mbr.pore), 
                           max.overlaps = 45,
                           box.padding = 0.5, 
                           point.padding = 0.5,
                           segment.color = 'grey50') +
  scale_shape_manual(values=c(3,seq(21,26,1))) +
  scale_color_manual(values=c("black","darkorange3")) +
  xlab("konc i ekstraktion (ng/uL) per L filtr. vand") +
  ylab("ddPCR fund (kopier/uL) per L filtreret") +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  annotation_logticks(sides = "l")  +
  theme(axis.text.y = element_text(size=12,face="bold")) +
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3,
                          1e2,
                          1e1), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) ) +
  labs(col="prøvetype",
       fill="porestr.\n(um)",
       shape="filtertype")
p


# define the output file name
outfl <- paste0(wd00_wd04,"/Fig04b_copies_per_filtered_liter_compared_with_extract_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         #width=210*1.6,height=297*0.6,
         width=297*0.6,height=210*0.7,
         units="mm",dpi=300)
}


# calculate the copies per liter filtered 
df_ddP03$ddpcr_copies_uL_per_L <- df_ddP03$ddpcr_copies_uL/(df_ddP03$fltvol.mL/1000)
df_ddP03$Filt.mbr.pore2 <- df_ddP03$Filt.mbr.pore
df_ddP03$Filt.mbr.pore2[(df_ddP03$WellNm02=="STD" & !is.na(df_ddP03$WellNm02))]  <- df_ddP03$smpldscr01[(df_ddP03$WellNm02=="STD" & !is.na(df_ddP03$WellNm02))] 
df_ddP03[(is.na(df_ddP03$Filt.mbr.pore2)),]

df_ddP03$Filt.mbr_std <- df_ddP03$Filt.mbr.pore2
# compare in a plot
p <- ggplot(df_ddP03) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=ddpcr_copies_uL_per_L,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("ddPCR find (copies/uL) per L filtered") +
  theme_minimal() + 
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  annotation_logticks(sides = "l")  +
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3,
                          1e2,
                          1e1), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  scale_fill_viridis_d(alpha = 0.6)
  
p

# define the output file name
outfl <- paste0(wd00_wd04,"/Fig04_copies_per_filtered_liter_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         #width=210*1.6,height=297*0.6,
         width=297*0.6,height=210*0.6,
         units="mm",dpi=300)
}


# compare in a plot
p <- ggplot(df_ddP03) +
  geom_boxplot(aes(x=Filt.mbr.pore2, 
                   y=ddpcr_copies_uL,
                   fill=Filt.mbr_std)) +
  
  xlab("filtertype") +
  ylab("ddPCR find (copies/uL) ") +
  theme_minimal() + 
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6)
p


# assign the total copy number per ddpcr tube to a new column
# and make sure it is numeric
clNm <- colnames(df_ddP03)
idxN <- which(grepl("Copies*.*LWell",clNm))
df_ddP03$Copies_20uLWell <- as.numeric(df_ddP03[,idxN])

# compare in a plot
p <- ggplot(df_ddP03) +
  geom_boxplot(aes(x=Filt.mbr.pore2, 
                   y=Copies_20uLWell,
                   fill=Filt.mbr_std)) +
  
  xlab("filtertype") +
  ylab("ddPCR find (copies in 20uL well) ") +
  theme_minimal() + 
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6)
p


df_ddP03$ddpcr_copies_uL_per_mL_per_min <- df_ddP03$ddpcr_copies_uL/df_ddP03$mlpermin
df_ddP03$fltvol.mL <- as.numeric(df_ddP03$fltvol.mL)
df_ddP03$ddpcr_copies_uL_per_L <- df_ddP03$ddpcr_copies_uL/(df_ddP03$fltvol.mL/1000)

# compare in a plot
p <- ggplot(df_ddP03) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=ddpcr_copies_uL_per_L,
                   #y=copies_perLfiltered,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("ddPCR fund (kopier/uL) per L filtreret") +
  theme_minimal() + 
  #scale_y_continuous(trans='log10') +
  
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +

  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.text.y = element_text(size=12,face="bold"),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  annotation_logticks(sides = "l")  +
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3,
                          1e2,
                          1e1), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  scale_fill_viridis_d(alpha = 0.6) 
p


# define the output file name
outfl <- paste0(wd00_wd04,"/Fig06_copies_per_uL_per_filtered_liter_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         #width=210*1.6,height=297*0.6,
         width=297*0.6,height=210*0.6,
         units="mm",dpi=300)
}

voltmpl <- gsub("voltempl_","",df_ddP03$smpldscr02)
voltmpl <- gsub("uL","",voltmpl)
voltmpl <- as.numeric(voltmpl)
df_ddP03$vol_extr_used_inddpcr <- voltmpl
# if 'vol_extr_used_inddpcr' 
# is to represent the volume of water filtered, 
# then it equals being a proportion of the volume of AE buffer used for elution 
prp_of_extr  <- df_ddP03$vol_extr_used_inddpcr/df_ddP03$vol_AE_buffer_for_elutering_uL
# this ratio can then multiplied with the volume of water filtered
# to get the fraction of the filtered volume
frc.fvol <- (prp_of_extr*df_ddP03$fltvol.mL)

# the copy number counted in 'Copies_20uLWell'
# can then be multiplied with the fraction of the filtered volume per 1000 mL
# to get the number of copies  per liter filtered
df_ddP03$copies_perLfiltered <- 1000*(1000/frc.fvol)*df_ddP03$Copies_20uLWell

# check if the different manifold or cartridges have different copy numbers
# It does not seem like it, as the jittered points scattered evenly
df_ddP03$Manifold.og.pumpe.nummer_cat <- as.character(df_ddP03$Manifold.og.pumpe.nummer)
df_ddP03$Cartridge.nummer.I.manifold_cat <- as.character(df_ddP03$Cartridge.nummer.I.manifold)
#
library(ggplot2)
# Included because this is what I used to make the images:
#theme_set(theme_minimal()) 
#install.packages("ggthemes")
library(ggthemes)
# Color # https://stackoverflow.com/questions/19300134/ggplot2-with-colour-blind-ggthemes-except-black
scale_color_colorblind8 = function(.ColorList = 1L:8L, ...){
  scale_color_discrete(..., type = colorblind_pal()(8)[.ColorList])
}
# compare in a plot
p <- ggplot(df_ddP03,
            aes(x=Filt.mbr.pore, 
                #fill=filt.mbr,
                #y=ddpcr_copies_uL_per_L,
                color=Cartridge.nummer.I.manifold_cat,
                y=copies_perLfiltered)) +
  # add jittered points to the boxplot
  # https://cmdlinetips.com/2018/04/how-to-make-boxplot-in-r-with-ggplot2/
  geom_boxplot() +
  geom_jitter(width=0.25, alpha=0.5) +
  xlab("filtertype") +
  ylab("ddPCR fund kopier per L filtreret") +
  theme_minimal() + 
  # modify the y-axis to be log10
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  # add tick marks on th y-axis 
  annotation_logticks(sides = "l")  +
  # add horizontal lines at the log10 values
  geom_hline(yintercept=c(1e11,
                          1e10,
                          1e9,
                          1e8,
                          1e7,
                          1e6,
                          1e5,
                          1e4), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        
        axis.text.y = element_text(size=12,face="bold"), 
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  #scale_fill_viridis_d(alpha = 0.6) +
  scale_color_colorblind8() +
  labs(col="cartridge\n nr")
p

# define the output file name
outfl <- paste0(wd00_wd04,"/Fig07_eDNA_copies_per_filtered_liter_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         #width=210*1.6,height=297*0.6,
         width=297*0.6,height=210*0.6,
         units="mm",dpi=300)
}


# compare in a plot
p <- ggplot(df_ddP03,
            aes(x=Filt.mbr.pore, 
                #y=ddpcr_copies_uL_per_L,
                y=copies_perLfiltered,
                #color=Cartridge.nummer.I.manifold_cat,
                fill=filt.mbr)) +
  # add jittered points to the boxplot
  # https://cmdlinetips.com/2018/04/how-to-make-boxplot-in-r-with-ggplot2/
  geom_boxplot() +
  geom_jitter(width=0.25, alpha=0.5) +
  xlab("filtertype") +
  ylab("ddPCR fund kopier per L filtreret") +
  theme_minimal() + 
  # modify the y-axis to be log10
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  # add tick marks on th y-axis 
  annotation_logticks(sides = "l")  +
  # add horizontal lines at the log10 values
  geom_hline(yintercept=c(1e11,
                          1e10,
                          1e9,
                          1e8,
                          1e7,
                          1e6,
                          1e5,
                          1e4), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.text.y = element_text(size=12,face="bold"), 
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6)
p

# define the output file name
outfl <- paste0(wd00_wd04,"/Fig07_eDNA_copies_per_filtered_liter_v02.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         #width=210*1.6,height=297*0.6,
         width=297*0.6,height=210*0.6,
         units="mm",dpi=300)
}

