#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# SAFIPO:
# Sammenligning af filterporestørrelser

#remove everything in the working environment, without a warning!!
#rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(xlsx)
#install.packages("huxtable")
library(huxtable)
#define working directory
#wd00  <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2024/tilbud_projekt_for_MST_om_forsk_filterpore_str/fig_forsoegs_opsaet"
# filtertype available
# define directory names
wd00 <- getwd()
wd.data <- "data"
wd01 <- "output01_tables_w_experimental_setup"
# make path for data input directory 
wd00_wd.data <- paste(wd00,
                            wd.data,
                           sep="/")
# make path for out put directory
wd00_wd01 <- paste(wd00,
                      wd01,
                      sep="/")
# delete a directory -- must add recursive = TRUE
unlink(wd00_wd01, recursive = TRUE)
#create anew directory
dir.create(wd00_wd01)

#check the working dir
getwd()
# PES Membrane Filter, 0.22 μm Pore Size
# Millipore Express®, filter diam. 47 mm, hydrophilic
# 
# PES Membrane Filter, 0.45 μm Pore Size
# Millipore Express®, filter diam. 47 mm, hydrophilic
# 
# Nylon Membrane Filter, 0.45 μm Pore Size (for at sammenligne med PES 0.45)
# Millipore, filter diam. 47 mm, hydrophilic
# 
# Polycarbonate Membrane Filter, 0.22 μm Pore Size
# Isopore™, filter diam. 47 mm, hydrophilic, white
# 
# Polycarbonate Membrane Filter, 0.4 μm Pore Size
# Isopore™, filter diam. 47 mm, hydrophilic, white
# 
# Polycarbonate Membrane Filter, 0.8 μm Pore Size
# Isopore™, filter diam. 47 mm, hydrophilic
# 
# Polycarbonate Membrane Filter, 1.2 μm Pore Size
# Isopore™, filter diam. 47 mm, hydrophilic
# 
# Polycarbonate Membrane Filter, 3.0 μm Pore Size
# Isopore™, filter diam. 47 mm, hydrophilic

# vectors with filter types
filters <- c("capsule", "disc")
filters_abbr <- c("CPS","DIS")
filter_diam <- c("47 mm", "capsule")
filter_diam_abbr <- c("Ø47mm", "CAPSU")
# combine filter types in to a data frame
df_flTps <- as.data.frame(cbind(filter_diam_abbr,
                        filter_diam,
                        filters_abbr,
                        filters))
# make vectors with mebranes and pore sizes
membrane_matr <- c("polyethersulfone","nylon","Polycarbonate", "glassfiber")
membrane_matr_abbr <- c("PES","NYL","PLC", "GLF")
pore_size_all <- c(0.22, 0.45, 0.4, 0.8, 1.2, 3.0) # in µm
pore_size_STX <- c(0.22, 0.45) # in µm
pore_size_PLC <- pore_size_all[c(1,3:length(pore_size_all))] # in µm
pore_size_NYL <- pore_size_all[c(2)] # in µm
pore_size_GLF <- pore_size_all[c(seq(4,length(pore_size_all),1))] # in µm
pore_size_PSE <- pore_size_all[c(seq(1,2,1))] # in µm
membrane_matr_abbr_STX <- c(membrane_matr_abbr)[1]
membrane_matr_abbr_PLC <- c(membrane_matr_abbr)[3]
membrane_matr_abbr_NYL <- c(membrane_matr_abbr)[2]
membrane_matr_abbr_GLF <- c(membrane_matr_abbr)[4]
membrane_matr_abbr_PSE <- c(membrane_matr_abbr)[1]
replicates <- 16
replNos <- seq(1:replicates)
filtr_rnds <- 4
filtr_rndsNos <- seq(1:filtr_rnds)
df_STX_Ftps <- as.data.frame(cbind("STX",
                                   "sterivex",
                                   "capsule",
                                   membrane_matr_abbr_STX,
                                   pore_size_STX))
df_PLC_Ftps <- as.data.frame(cbind("PLC",
                                   "Polycarbonate",
                                   "disc",
                                   membrane_matr_abbr_PLC,
                                   pore_size_PLC))
df_NYL_Ftps <- as.data.frame(cbind("NYL",
                                   "Nylon",
                                   "disc",
                                   membrane_matr_abbr_NYL,
                                   pore_size_NYL))
df_PSE_Ftps <- as.data.frame(cbind("PSE",
                                   "polyethersulfone",
                                   "disc",
                                   membrane_matr_abbr_PSE,
                                   pore_size_PSE))
df_GLF_Ftps <- as.data.frame(cbind("GLF",
                                   "glassfiber",
                                   "disc",
                                   membrane_matr_abbr_GLF,
                                   pore_size_GLF))

colnames(df_STX_Ftps) <- c("filters_abbr",
                           "filtNm",
                           "filtType",
                           "membrane_matr_abbr",
                           "pore_size_um")

colnames(df_PLC_Ftps) <- c("filters_abbr",
                           "filtNm",
                           "filtType",
                           "membrane_matr_abbr",
                           "pore_size_um")
colnames(df_NYL_Ftps) <- c("filters_abbr",
                           "filtNm",
                           "filtType",
                           "membrane_matr_abbr",
                           "pore_size_um")
colnames(df_PSE_Ftps) <- c("filters_abbr",
                           "filtNm",
                           "filtType",
                           "membrane_matr_abbr",
                           "pore_size_um")


colnames(df_GLF_Ftps) <- c("filters_abbr",
                           "filtNm",
                           "filtType",
                           "membrane_matr_abbr",
                           "pore_size_um")

# combine the data frames with the filter types
df_ALL_Ftps<- rbind(df_NYL_Ftps,
                    df_STX_Ftps,
                    df_PLC_Ftps,
                    df_PSE_Ftps,
                    df_GLF_Ftps)
# ensure the number of decimal places is 2 for
# all pore sizes
df_ALL_Ftps$pore_size_um <- as.character(format(round(
                                  as.numeric(df_ALL_Ftps$pore_size_um)
                                  , 2), nsmall = 2))

#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
replicatNos <- stringr::str_pad(replNos, 2, pad = "0")
FiltRndNos <- stringr::str_pad(filtr_rndsNos, 2, pad = "0")

# only get half the replicate numbers
STXrplNos_p1 <- replicatNos[seq(1,(replicates/2),1)]
STXrplNos_p2 <- replicatNos[ seq((replicates/2)+1,replicates,1)  ]
# make a column with abbraeviations and pore size only
df_ALL_Ftps$abbr_Psz <- paste0(df_ALL_Ftps$filters_abbr,"_",df_ALL_Ftps$pore_size_um)

# Get round 1 filters
df_Rnd01_F01 <- cbind("Rnd01",rep(df_ALL_Ftps$abbr_Psz[2],replicates/2),rep(df_ALL_Ftps$filtType[2],replicates/2),STXrplNos_p1)
df_Rnd01_F02 <- cbind("Rnd01",rep(df_ALL_Ftps$abbr_Psz[4],replicates),rep(df_ALL_Ftps$filtType[4],replicates),replicatNos)
df_Rnd01_F03 <- cbind("Rnd01",rep(df_ALL_Ftps$abbr_Psz[9],replicates),rep(df_ALL_Ftps$filtType[9],replicates),replicatNos)

# Get round 2 filters
df_Rnd02_F01 <- cbind("Rnd02",rep(df_ALL_Ftps$abbr_Psz[2],replicates/2),rep(df_ALL_Ftps$filtType[2],replicates/2),STXrplNos_p2)
df_Rnd02_F02 <- cbind("Rnd02",rep(df_ALL_Ftps$abbr_Psz[6],replicates),rep(df_ALL_Ftps$filtType[6],replicates),replicatNos)
df_Rnd02_F03 <- cbind("Rnd02",rep(df_ALL_Ftps$abbr_Psz[7],replicates),rep(df_ALL_Ftps$filtType[7],replicates),replicatNos)

# Get round 3 filters
df_Rnd03_F01 <- cbind("Rnd03",rep(df_ALL_Ftps$abbr_Psz[3],replicates/2),rep(df_ALL_Ftps$filtType[3],replicates/2),STXrplNos_p1)
df_Rnd03_F02 <- cbind("Rnd03",rep(df_ALL_Ftps$abbr_Psz[1],replicates),rep(df_ALL_Ftps$filtType[1],replicates),replicatNos)
df_Rnd03_F03 <- cbind("Rnd03",rep(df_ALL_Ftps$abbr_Psz[5],replicates),rep(df_ALL_Ftps$filtType[5],replicates),replicatNos)

# Get round 4 filters
df_Rnd04_F01 <- cbind("Rnd04",rep(df_ALL_Ftps$abbr_Psz[3],replicates/2),rep(df_ALL_Ftps$filtType[3],replicates/2),STXrplNos_p2)
df_Rnd04_F02 <- cbind("Rnd04",rep(df_ALL_Ftps$abbr_Psz[10],replicates),rep(df_ALL_Ftps$filtType[10],replicates),replicatNos)
df_Rnd04_F03 <- cbind("Rnd04",rep(df_ALL_Ftps$abbr_Psz[8],replicates),rep(df_ALL_Ftps$filtType[8],replicates),replicatNos)



# set the number of negative controls
nnk <- 4
#pad with zeros to two characters on the negative controls
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
replicatNosNK <- stringr::str_pad(seq(1,nnk,1), 2, pad = "0")

# Get round 5 filters
df_Rnd05_F01 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[2],nnk),"NK"),rep(df_ALL_Ftps$filtType[2],nnk),replicatNosNK)
df_Rnd05_F02 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[4],nnk),"NK"),rep(df_ALL_Ftps$filtType[4],nnk),replicatNosNK)
df_Rnd05_F03 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[9],nnk),"NK"),rep(df_ALL_Ftps$filtType[9],nnk),replicatNosNK)
df_Rnd05_F04 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[6],nnk),"NK"),rep(df_ALL_Ftps$filtType[6],nnk),replicatNosNK)
df_Rnd05_F05 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[7],nnk),"NK"),rep(df_ALL_Ftps$filtType[7],nnk),replicatNosNK)
df_Rnd05_F06 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[3],nnk),"NK"),rep(df_ALL_Ftps$filtType[3],nnk),replicatNosNK)
df_Rnd05_F07 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[1],nnk),"NK"),rep(df_ALL_Ftps$filtType[1],nnk),replicatNosNK)
df_Rnd05_F08 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[5],nnk),"NK"),rep(df_ALL_Ftps$filtType[5],nnk),replicatNosNK)
df_Rnd05_F09 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[10],nnk),"NK"),rep(df_ALL_Ftps$filtType[10],nnk),replicatNosNK)
df_Rnd05_F10 <- cbind("Rnd05",paste0(rep(df_ALL_Ftps$abbr_Psz[8],nnk),"NK"),rep(df_ALL_Ftps$filtType[8],nnk),replicatNosNK)




df_RndAll <- rbind(df_Rnd01_F01,
                   df_Rnd01_F02,
                   df_Rnd01_F03, 
                   
                   df_Rnd02_F01,
                   df_Rnd02_F02,
                   df_Rnd02_F03,
                   
                   df_Rnd03_F01,
                   df_Rnd03_F02,
                   df_Rnd03_F03,
                   
                   df_Rnd04_F01,
                   df_Rnd04_F02,
                   df_Rnd04_F03,
                   
                   df_Rnd05_F01,
                   df_Rnd05_F02,
                   df_Rnd05_F03,
                   df_Rnd05_F04,
                   df_Rnd05_F05,
                   df_Rnd05_F06,
                   df_Rnd05_F07,
                   df_Rnd05_F08,
                   df_Rnd05_F09,
                   df_Rnd05_F10)
# make it a data frame
df_RndAll <- as.data.frame(df_RndAll)
# change the column names
colnames(df_RndAll) <- c("FiltRound","FiltNm","FiltType", "BiolReplNo")
unqFno <- row.names(df_RndAll)
# pad to get an equal number of digits
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
unqFno <- stringr::str_pad(unqFno, 3, pad = "0")
# append this as a new unique column
df_RndAll$unqFno <- unqFno
# match between dataframe to get a shorter name for the filtertype
df_RndAll$FiltType <- df_flTps$filters_abbr[match(df_RndAll$FiltType,df_flTps$filters)]
# get the "xlsx" package to be able to write out an excel file
unqFlTps <- unique(df_RndAll$FiltNm)

# add empty columns
df_RndAll$FltVol_ml <- ""
df_RndAll$date_filt <- ""
df_RndAll$tm_start_filt <- ""
df_RndAll$tm_end_filt <- ""
df_RndAll$tm_froz_filt <- ""
# make some new column names
nwClNms <- c("Filt Rnd",
             "Filt mbr pore",
             "Filt Type",
             "Biol Rpl nr",
             "unkFilt nr",
             "Filtreret volume (mL)",
             "dato for filtrering",
             "Tidspnkt start filt",
             "Tidspnkt slut filt",
             "Tidspnkt Filt indfrosset")

colnames(df_RndAll) <- nwClNms

#require("xlsx")
# write out an xlsx file with the table
xlsx::write.xlsx(df_RndAll, paste0(wd00_wd01 ,
                             "/Tabel_SAFIPO_filt_nummerering_opsaetning_v01.xlsx"),
                 row.names = F)



#dput(unqFlTps)
#
#get ggplot package
# if(!require(huxtable)){
#   library(devtools)
#   install_github("hughjonesd/huxtable")
# }  
library(huxtable)
# see : https://hughjonesd.github.io/huxtable/
# also see: https://hughjonesd.github.io/huxtable/design-principles.html

library(huxtable)
library(stringr)
# subset the mtcar data frame to have a smalller data frame to 
# experiment with
mtct <- mtcars[1:7,1:6]
# color the entire based evaluation of match in specific column
htmtct <- as_hux(mtct,add_rownames = "Model")
htmtct  |>
  set_background_color(row = stringr::str_detect(htmtct$Model,
                                                 "Mazda"), value="green") |>
  set_background_color(row = stringr::str_detect(htmtct$Model, "
                                                 Datsun"), value="orange") |>
  set_background_color(row = stringr::str_detect(htmtct$Model,
                                                 "Hornet"), value="red")
htmtct
# see: for mapping colors by match
# https://hughjonesd.github.io/huxtable/reference/mapping-functions.html

ht_df_RndAll <- as_hux(df_RndAll,add_rownames = "unkFilt nr2")
ht_df_RndAll <- ht_df_RndAll                    |>
  
  #as_huxtable(add_rownames = "unik Prv Nr")    |>
  set_bold(1, everywhere, TRUE)                |>
  set_all_borders(1)                           |>
  
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                "STX_0.22"), value="yellow3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                "PLC_0.22"), value="khaki2") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                "PSE_0.22"), value="lemonchiffon2") |>

  
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "PLC_0.80"), value="seagreen3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "PLC_1.20"), value="palegreen3") |>
  
  
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "STX_0.45"), value="lightblue1") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "NYL_0.45"), value="steelblue3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "PLC_0.40"), value="cornflowerblue") |>
  
  
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "PSE_0.45"), value="orange3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "PLC_3.00"), value="sandybrown") |>
  
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
                                                 "NK"), value="grey45") |>
  
    # map_background_color( by_regex(
    # "STX_0.22" = "yellow3",
    # "PLC_0.22" = "khaki2",
    # "PSE_0.22" = "lemonchiffon2",
    # 
    # "PLC_0.80" = "seagreen3",
    # "PLC_1.20" = "palegreen3",
    # 
    # "STX_0.45" = "lightblue1",
    # "NYL_0.45" = "steelblue3",
    # "PLC_0.40" = "cornflowerblue",
    # 
    # "PSE_0.45" = "orange3",
    # "PLC_3.00" = "sandybrown")) |>
  # 
  map_background_color( by_regex(
    "Rnd01" = "yellow",
    "Rnd02" = "seagreen1",
    "Rnd03" = "deepskyblue1",
    "Rnd04" = "orange1",
    "Rnd05" = "grey34")    )

# get rid of the first column, as this was only for the making of the 
# huxtable
ht_df_RndAll <- ht_df_RndAll[,-1]
# make the huxtable a html table 
  html_ht_df_RndAll <- to_html(ht_df_RndAll)
# save the html table as a file  
  write.table(html_ht_df_RndAll, 
              file=paste0(wd00_wd01 ,"/Table01_filter_opsaetning_v01.html"), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
#_______________________________________________________________________________
# section 02 - start - make html tables that can be printed on 
  # sticker sheets that comprise 3 x 8 stickers
#_______________________________________________________________________________

# concatenate all samples into one vector
lst_all_smplNms <-  paste(df_RndAll$`Filt Rnd`,
  df_RndAll$`Filt mbr pore`,
  df_RndAll$`Filt Type`,
  df_RndAll$`Biol Rpl nr`,
  df_RndAll$`unkFilt nr`, sep="|")
  
nSmpls <- length(lst_all_smplNms)
fTs <- ceiling(nSmpls/24)
sfTs <- seq(1,fTs,1)
# itersate over portions of 24 - which match the sticker sheet
for (e in sfTs)
{
  #see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
  Pe <- stringr::str_pad(e, 2, pad = "0")
    # start the iteration from 1 below the first
  e <- e-1
  # then get every 24th element from the vector
  sta.e <- e*24+1
  stp.e <- e*24+24
  print(paste0(sta.e," to ",stp.e))
lsAsmp24 <- lst_all_smplNms[sta.e:stp.e]
mtxalsA24 <- matrix(lsAsmp24,nrow=8,ncol=3)
huxalsA24 <- as_hux(mtxalsA24)

huxalsA24 <- huxalsA24    |>
    #set_bold(1, everywhere, TRUE)                |>
    set_all_borders(1)                           |>
    map_background_color( by_regex(
    "Rnd01" = "yellow",
    "Rnd02" = "seagreen1",
    "Rnd03" = "deepskyblue1",
    "Rnd04" = "orange1",
    "Rnd05" = "grey74")    )

# make the huxtable a html table 
html_huxalsA24 <- to_html(huxalsA24)
# save the html table as a file  
write.table(html_huxalsA24, 
            file=paste0(wd00_wd01 ,"/Table02_stickers_for_filters_set",Pe,".html"), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

}

#_______________________________________________________________________________
# section 02 - end - make html tables that can be printed on 
# sticker sheets that comprise 3 x 8 stickers
#_______________________________________________________________________________
#_______________________________________________________________________________
# section 03 - start - make html tables that can be printed on 
# sticker sheets that comprise 3 x 8 stickers
#_______________________________________________________________________________

# concatenate all samples into one vector
lst_all_smplNms <-  paste(df_RndAll$`Filt Rnd`,
                          df_RndAll$`Filt mbr pore`,
                          df_RndAll$`Filt Type`, sep="|")

nSmpls <- length(lst_all_smplNms)
fTs <- ceiling(nSmpls/24)
sfTs <- seq(1,fTs,1)
# itersate over portions of 24 - which match the sticker sheet
for (e in sfTs)
{
  #see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
  Pe <- stringr::str_pad(e, 2, pad = "0")
  # start the iteration from 1 below the first
  e <- e-1
  # then get every 24th element from the vector
  sta.e <- e*24+1
  stp.e <- e*24+24
  print(paste0(sta.e," to ",stp.e))
  lsAsmp24 <- lst_all_smplNms[sta.e:stp.e]
  mtxalsA24 <- matrix(lsAsmp24,nrow=8,ncol=3)
  huxalsA24 <- as_hux(mtxalsA24)
  
  huxalsA24 <- huxalsA24    |>
    #set_bold(1, everywhere, TRUE)                |>
    set_all_borders(1)                           |>
    map_background_color( by_regex(
      "Rnd01" = "yellow",
      "Rnd02" = "seagreen1",
      "Rnd03" = "deepskyblue1",
      "Rnd04" = "orange1",
      "Rnd05" = "grey74")    )
  
  # make the huxtable a html table 
  html_huxalsA24 <- to_html(huxalsA24)
  # save the html table as a file  
  write.table(html_huxalsA24, 
              file=paste0(wd00_wd01 ,"/Table03_stickers_for_filters_set",Pe,".html"), 
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)
  
}

#_______________________________________________________________________________
# section 03 - end - make html tables that can be printed on 
# sticker sheets that comprise 3 x 8 stickers
#_______________________________________________________________________________
