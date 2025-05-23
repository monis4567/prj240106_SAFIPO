#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# SAFIPO
#remove everything in the working environment, without a warning!!

#rm(list=ls())

#
library(dplyr)
library(ggplot2)
library(scales)
wd00 <- getwd()

wd10 <- "output10_figures_w_all_filter_comparisons"
# make path for out put directory
wd00_wd10 <- paste(wd00,
                   wd10,
                   sep="/")
# delete a directory -- must add recursive = TRUE
unlink(wd00_wd10, recursive = TRUE)
#create anew directory
dir.create(wd00_wd10)

# make a variable with a directory
wd_out_02 <- "data_output02"
#paste dirs together
wd00_out02  <- paste0(wd00,"/",wd_out_02)
#grep for the '.csv' files in the directory, and place in a list
lst_fcsv <- list.files(wd00_out02)[grep("\\.csv",list.files(wd00_out02))]
# add wd to all files
lst_inf02 <- wd00_out02 %>% paste0("/",lst_fcsv)

# https://statisticsglobe.com/merge-csv-files-in-r
library(readr)
# To read in all files from the list, and also make sure all columns are 
# characters, and the bind rows in to a tibble
# make a name vector  with the file name from the list
names(lst_inf02) <- basename(lst_fcsv)
# use the list of files 
tibl_eDNA01 <- lst_inf02 %>%
  # read in the csv files , making sure columns are characte
  lapply(read_csv, col_types=cols(.default = col_character())) %>%
  # bind the rows together, and use the file name as a id-column
  bind_rows(.id="file")

# find the index number for the column that has the
# droplet count for all 20 uL reaction
clNm <- colnames(tibl_eDNA01)
# and replace the column name with a new name
idxcl.cp20_per_Well <- which(grepl ("20",clNm))
clNm[idxcl.cp20_per_Well] <- "Copies_20uLWell"
colnames(tibl_eDNA01) <- clNm
# see the tragets and file names
unique(tibl_eDNA01$Target)
unique(tibl_eDNA01$file)
# split the file name
fl.splt2 <- data.frame(do.call('rbind',
                      strsplit(as.character(tibl_eDNA01$file)
                          ,'_',fixed=TRUE)))
# get the dilution factor from the 4th colum
dlfct <- gsub("dil(.*)\\.csv","\\1",fl.splt2[,4])
dlfct <- gsub("to","/",dlfct)
# add back as a column with the dilution factor
tibl_eDNA01$dlfct <- as.numeric(as.factor(dlfct))
#
voltmpl <- gsub("voltempl_","",tibl_eDNA01$smpldscr02)
voltmpl <- gsub("uL","",voltmpl)
voltmpl <- as.numeric(voltmpl)
tibl_eDNA01$vol_extr_used_inddpcr <- voltmpl

# the 'tibl_eDNA01' data frame has the 'vol_AE_buffer_for_elutering_uL' 
# column that is a character
# convert this to a numeric value
tibl_eDNA01 <- tibl_eDNA01 %>% dplyr::mutate_at(c(
  'vol_AE_buffer_for_elutering_uL',
  'Copies_20uLWell',
  'dlfct',
  'fltvol.mL'), as.numeric)
# use the 'str' function to check out which are characters 
# and which are numeric
tibl_eDNA01 %>% str()

# if 'vol_extr_used_inddpcr' 
# is to represent the volume of water filtered, 
# then it equals being a proportion of the volume of AE buffer used for elution 
prp_of_extr  <- tibl_eDNA01$vol_extr_used_inddpcr/tibl_eDNA01$vol_AE_buffer_for_elutering_uL
# this ratio can then multiplied with the volume of water filtered
# to get the fraction of the filtered volume
frc.fvol <- (prp_of_extr*tibl_eDNA01$fltvol.mL)

# the copy number counted in 'Copies_20uLWell'
# can then be multiplied with the fraction of the 
# filtered volume per 1000 mL
# to get the number of copies  per liter filtered
tibl_eDNA01$copies_perLfiltered <- 1000*(1000/frc.fvol)*tibl_eDNA01$Copies_20uLWell
# multiply the 'ddpcr_copies_uL_per_L' column by  the dilution factor 
# as the template was diluted, and the dilution factor 
# is a fraction
tibl_eDNA01$copies_perLfiltered <- tibl_eDNA01$copies_perLfiltered*tibl_eDNA01$dlfct

# remove rows with NA values in the 'Filt.mbr.pore' column
tibl_eDNA01 <- tibl_eDNA01[!is.na(tibl_eDNA01$Filt.mbr.pore),]

# paste together the filter type and the target species name
tibl_eDNA01$Filt.mbr.pore_scp <- paste0(tibl_eDNA01$Filt.mbr.pore,
                            "_",
                            tibl_eDNA01$Target)

trgspc <- unique(tibl_eDNA01$Target)
#dput(trgspc)
#
trgspc.abNm <- c("Gadmor", "Neomel", "Plafle", "Myaare","Plepla","Cluhar")
trgspc.lnNm <- c("G. morhua", "N. melanostomus", "P. flesus",
                 "M. arenaria", "P. platessa", "C. harengus")
# make sure the names are orderer in the same way
trgspc.abNm <- trgspc.abNm[order(trgspc.abNm)]
trgspc.lnNm <- trgspc.lnNm[order(trgspc.lnNm)]
trgspc <- trgspc[order(trgspc)]

# combine to a data frame
trgspcNm <- data.frame(trgspc, trgspc.abNm, trgspc.lnNm)
# match the target species name with the abbreviated name
tibl_eDNA01$ln_spcNm <- trgspcNm$trgspc.lnNm[match(tibl_eDNA01$Target,
                                                  trgspcNm$trgspc)]

# In the ggplots , later on, a color is required for each species
# start by making a data frame
# get one range of colors
clr01 <- palette.colors(palette = "Okabe-Ito")
# get another range of colors
clr02 <- palette.colors(palette = "Polychrome")
# combine the two ranges of colors
clr03 <- c(clr01,clr02)
#Get the numberof species
nsp <- length(trgspcNm$trgspc.lnNm)
# make a data frame with the species name and the color
spcClr <- data.frame(trgspcNm$trgspc.lnNm, clr03[1:nsp])
# change the column names
colnames(spcClr) <- c("ln_spcNm","clr")
# reorder the data frame by the species name
spcClr <- spcClr[order(spcClr$ln_spcNm),]
tibl_eDNA01 <- tibl_eDNA01[order(tibl_eDNA01$ln_spcNm),]


# compare in a plot
p <- ggplot(tibl_eDNA01,
            aes(#x=Filt.mbr.pore,
                x=Filt.mbr.pore_scp,
                #y=ddpcr_copies_uL_per_L,
                y=copies_perLfiltered,
                #color=Cartridge.nummer.I.manifold_cat,
                fill=filt.mbr)) +
  # add jittered points to the boxplot
  # https://cmdlinetips.com/2018/04/how-to-make-boxplot-in-r-with-ggplot2/
  geom_boxplot() +
  geom_jitter(width=0.25, alpha=0.5, aes(color= tibl_eDNA01$ln_spcNm) ) +
  scale_color_manual(values=c(spcClr$clr)) +
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
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.text.y = element_text(size=12,face="bold"), 
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6) +
  labs(col="Latinsk \nartsnavn",
       fill="filtertype")
p
#________-
#https://stackoverflow.com/questions/56961744/draw-alternate-rectangles-in-boxplots-with-facets-r-ggplot2
# create a data frame with the tile information
df_tile <- data.frame(carb = c(1, 8, 6), 
                      class = c('class A', 'class C', 'class B'))

ggplot(mtcars) + 
  geom_tile(aes(x = factor(carb), y = 1, 
                height = Inf, width = 1), 
            data = df_tile, alpha = 0.3) + 
  geom_boxplot(aes(x = 
                     factor(carb), 
                   y = 
                     mpg, 
                   fill = 
                     factor(am)), 
               position = position_dodge(0.9)) + 
  facet_grid(cols = vars(class), 
             scales = "free_x", space = "free") + 
  theme(panel.spacing.x = unit(0, "pt"), 
        strip.background = element_rect(
    color="black", size=0.5, linetype="solid"))

#________-
library(ggplot2)
library(tidyverse)
library(scales)
# https://stackoverflow.com/questions/59554096/ggplot2-italics-in-the-legend
toexpr <- function(x, plain = NULL) {
  getfun <- function(x) {
    ifelse(x == plain, "plain", "italic")
  }
  as.expression(unname(Map(function(f,v) substitute(f(v), list(f=as.name(f), v=as.character(v))), getfun(x), x)))
}
#________-

# make a data frame with the filter type and the class
NKfilttps <- unique(tibl_eDNA01$Filt.mbr.pore_scp[(grepl("NK",
                tibl_eDNA01$Filt.mbr.pore_scp))])
# match the filter type with the class
NKfltt <- tibl_eDNA01$filt.mbr[match(NKfilttps,
                                     tibl_eDNA01$Filt.mbr.pore_scp)]
# combine to a data frame
df_tile <- data.frame(Filt.mbr.cat = NKfilttps, 
                      class = NKfltt)

# Make a plot with the tile information
p1 <- ggplot(tibl_eDNA01) + 
  geom_boxplot(aes(x = 
                     factor(Filt.mbr.pore_scp), 
                   y = 
                     copies_perLfiltered, 
                   fill = 
                     factor(filt.mbr)), 
               position = position_dodge(0.9)) + 
  geom_tile(aes(x = factor(Filt.mbr.cat), y = 1, 
                height = Inf, width = 1), 
            data = df_tile, alpha = 0.3) + 
  # add jittered points to the boxplot
  geom_jitter(data= tibl_eDNA01,
              width=0.25, 
              alpha=0.5, 
              aes(x = factor(Filt.mbr.pore_scp), 
                  y = copies_perLfiltered,
                  color = ln_spcNm) ) +
  scale_color_manual(values=c( spcClr$clr
                              
                              ),
  # making the species names in italics
                     labels = toexpr(unique(tibl_eDNA01$ln_spcNm),
                                     plain = 'Wt')) +
  # change the lab labels
  xlab("filtertype") +
  ylab("ddPCR fund kopier per L filtreret") +
  #theme_minimal() + 
  theme_bw() +
# 
#   facet_grid(cols = vars(class),
#              scales = "free_x", space = "free") +

  # modify the y-axis to be log10
  scale_y_continuous(trans='log10',
                     #limits = c(1e2, 1e8),
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))
                     ) +
  # add tick marks on th y-axis 
  annotation_logticks(sides = "l")  +
  # add horizontal lines at the log10 values
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.text.y = element_text(size=12,face="bold"), 
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6) +
  
  labs(col="Latinsk \nartsnavn",
       fill="filtertype") +
  theme(panel.spacing.x = unit(0, "pt"), 
        strip.background = element_rect(
          color="black", size=0.5, linetype="solid"))

p1
# define the output file name
outfl <- paste0(wd00_wd10,"/Fig10_eDNA_copies_per_filtered_liter_allspec_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p1, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         width=297,height=210,
         units="mm",dpi=300)
}

#View(df_ddP03)


str(tibl_eDNA01)

unique(tibl_eDNA01$Filt.mbr.pore)
# make a data frame with the filter type and the class
NKfilttps <- unique(tibl_eDNA01$Filt.mbr.pore[(grepl("NK",
                                                     tibl_eDNA01$Filt.mbr.pore))])
# match the filter type with the class
NKfltt <- tibl_eDNA01$filt.mbr[match(NKfilttps,
                                     tibl_eDNA01$Filt.mbr.pore)]
# combine to a data frame
df_tile <- data.frame(Filt.mbr.cat = NKfilttps, 
                      class = NKfltt)

#View(tibl_eDNA01)
# Make a plot with the tile information
p1 <- ggplot(tibl_eDNA01) + 
  geom_boxplot(aes(x = 
                     factor(Filt.mbr.pore), 
                   y = 
                     copies_perLfiltered, 
                   fill = 
                     factor(filt.mbr)), 
               position = position_dodge(0.9)) + 
  geom_tile(aes(x = factor(Filt.mbr.cat), y = 1, 
                height = Inf, width = 1), 
            data = df_tile, alpha = 0.3) + 
  # add jittered points to the boxplot
  geom_jitter(data= tibl_eDNA01,
              width=0.25, 
              alpha=0.5, 
              aes(x = factor(Filt.mbr.pore), 
                  y = copies_perLfiltered,
                  color = ln_spcNm) ) +
  scale_color_manual(values=c( spcClr$clr
                               
  ),
  # making the species names in italics
  labels = toexpr(unique(tibl_eDNA01$ln_spcNm),
                  plain = 'Wt')) +
  # change the lab labels
  xlab("filtertype") +
  ylab("ddPCR fund kopier per L filtreret") +
  #theme_minimal() + 
  theme_bw() +
  # 
  facet_wrap(~ln_spcNm, 
             #scales = "free_x", 
             ncol = 2) +
  # modify the y-axis to be log10
  scale_y_continuous(trans='log10',
                     #limits = c(1e2, 1e8),
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))
  ) +
  # add tick marks on th y-axis 
  annotation_logticks(sides = "l")  +
  # add horizontal lines at the log10 values
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.text.y = element_text(size=12,face="bold"), 
        #strip.background = element_blank(),
        strip.background = element_rect(fill = "white", linetype = "solid",
                                        color = "white", linewidth = 1, size = 0.5),
        #strip.text.x = element_blank(),
        panel.spacing.x = unit(0, "pt"),
        strip.text.x = element_text(colour = "black", 
                                    face = "italic",
                                    hjust = 0.0) ) +
  #legend.position="bottom",
  #strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.6) +
  labs(col="Latinsk \nartsnavn",
       fill="filtertype") 
# theme(panel.spacing.x = unit(0, "pt"), 
#       strip.background = element_rect(
#         color="black", size=0.5, linetype="solid"))

p1

# define the output file name
outfl <- paste0(wd00_wd10,"/Fig10_eDNA_copies_per_filtered_liter_allspec_v02.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p1, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         width=210,height=297,
         #width=297,height=210,
         units="mm",dpi=300)
}



#tibl_eDNA01$filt.pszc
#tibl_eDNA01$Filt.mbr.pore
# Make sure the columns are numeric
tibl_eDNA01 <- tibl_eDNA01 %>% dplyr::mutate_at(c(
  'vol_AE_buffer_for_elutering_uL',
  'Copies_20uLWell',
  'dlfct',
  'fltvol.mL',
  'extrconc_ngpuL_Pfltvol.L',
  'ddpcr_copies_uL_per_L',
  'filt.pszc'), as.numeric)
# make a scatter plot
p <- ggplot(data = tibl_eDNA01,
            aes(x = extrconc_ngpuL_Pfltvol.L, 
                y = ddpcr_copies_uL_per_L, 
                color= prv.type,
                fill = filt.pszc,
                shape = filt.mbr )) +
  geom_point(size=4) + 
  theme_minimal() + 
  #theme_bw() +
  # 
  facet_wrap(~ln_spcNm, 
             #scales = "free_x", 
             ncol = 2) +
  scale_fill_viridis_c(alpha = 0.6, direction = 1) +
  ggrepel::geom_text_repel(size = 2.8,
                           aes(label=Filt.mbr.pore), 
                           max.overlaps = 45,
                           box.padding = 0.5, 
                           point.padding = 0.5,
                           segment.color = 'grey50') +
  scale_shape_manual(values=c(21,3,seq(22,26,1))) +
  scale_color_manual(values=c("black","darkorange3")) +
  xlab("konc i ekstraktion (ng/uL) per L filtr. vand") +
  ylab("ddPCR fund (kopier/uL) per L filtreret") +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  annotation_logticks(sides = "l")  +
  #theme(axis.text.y = element_text(size=12,face="bold")) +
  theme(#axis.text.x = element_text(angle=90, hjust=1, vjust=1),
    #axis.text.y = element_text(size=12,face="bold"), 
    #strip.background = element_blank(),
    strip.background = element_rect(fill = "white", linetype = "solid",
                                    color = "white", linewidth = 1, size = 0.5),
    #strip.text.x = element_blank(),
    panel.spacing.x = unit(0, "pt"),
    strip.text.x = element_text(colour = "black", 
                                face = "italic",
                                hjust = 0.0) ) +
  geom_hline(yintercept=c(1e1,
                          1e0,
                          1e-1,
                          1e-2,
                          1e-3,
                          1e-4), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  labs(fill="porestr\n(um)") +
  labs(col="type") +
  labs(shape="filter\nmembran") +
  # labs(col="prøvetype",
  #      fill="porestr.\n(um)",
  #      shape="filtertype") +
  guides(fill = guide_legend(override.aes = list(shape = 21) ),
         shape = guide_legend(override.aes = list(fill = "black") ) ) 

p

# define the output file name
outfl <- paste0(wd00_wd10,"/Fig11_eDNA_copies_per_genomicfilter_extract_allspec_v02.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         width=210,height=297,
         #width=297,height=210,
         units="mm",dpi=300)
}


# Compare cartridges in a plot
# check if the different manifold or cartridges have different copy numbers
# It does not seem like it, as the jittered points scattered evenly
tibl_eDNA01$Manifold.og.pumpe.nummer_cat <- as.character(tibl_eDNA01$Manifold.og.pumpe.nummer)
tibl_eDNA01$Cartridge.nummer.I.manifold_cat <- as.character(tibl_eDNA01$Cartridge.nummer.I.manifold)

# make a data frame with the filter type and the class
NKfilttps <- unique(tibl_eDNA01$Filt.mbr.pore[(grepl("NK",
                                                     tibl_eDNA01$Filt.mbr.pore_scp))])
# match the filter type with the class
NKfltt <- tibl_eDNA01$filt.mbr[match(NKfilttps,
                                     tibl_eDNA01$Filt.mbr.pore)]
# combine to a data frame
df_tile <- data.frame(Filt.mbr.cat = NKfilttps, 
                      class = NKfltt)


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
p <- 
  ggplot(tibl_eDNA01) + 
  geom_boxplot(aes(x = 
                     factor(Filt.mbr.pore), 
                   y = 
                     copies_perLfiltered, 
                   color=Cartridge.nummer.I.manifold_cat)) +
  
  geom_tile(aes(x = factor(Filt.mbr.cat), y = 1,
                height = Inf, width = 1),
            data = df_tile, alpha = 0.3) +
  #add jittered points to the boxplot
  # https://cmdlinetips.com/2018/04/how-to-make-boxplot-in-r-with-ggplot2/
  #geom_boxplot() +
  geom_jitter(aes(x = 
                    factor(Filt.mbr.pore), 
                  y = 
                    copies_perLfiltered,
                  color=
                    Cartridge.nummer.I.manifold_cat),
              width=0.25, alpha=0.5) +
  
  xlab("filtertype") +
  ylab("ddPCR fund kopier per L filtreret") +
  theme_minimal() + 
  facet_wrap(~ln_spcNm, 
             #scales = "free_x", 
             ncol = 2) +
  # modify the y-axis to be log10
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  # add tick marks on th y-axis 
  annotation_logticks(sides = "l")  +
  # add horizontal lines at the log10 values
  geom_hline(yintercept=c(1e5,
                          1e4,
                          1e3), linetype='dashed', 
             color=c("grey44"), linewidth=0.35) +
  # adjust the tick labels on the x-axis
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        
        axis.text.y = element_text(size=12,face="bold"), 
        #legend.position="bottom",
        panel.spacing.x = unit(0, "pt"),
        strip.text.x = element_text(colour = "black", 
                                    face = "italic",
                                    hjust = 0.0) ) +
  #scale_fill_viridis_d(alpha = 0.6) +
  scale_color_colorblind8() +
  labs(col="cartridge\n nr")
p


# define the output file name
outfl <- paste0(wd00_wd10,"/Fig12_eDNA_copies_per_cartridge_allspec_v01.png")
#set variable to define if figures are to be saved
bSaveFigures<-T
# save the figure if the above 'bSaveFigures' is TRUE
if(bSaveFigures==T){
  ggsave(plot = p, 
         # define the output filenmae by pasting together 
         # qpcrrunno and qpcrrundate
         filename = paste0(outfl),
         width=210,height=297,
         #width=297,height=210,
         units="mm",dpi=300)
}

#View(df_ddP03)