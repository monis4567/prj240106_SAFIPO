#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-

#____________________________________________________________________________#
# R-code provided for the project:
# SAFIPO:
# Sammenligning af filterporestørrelser

library(xlsx)
library(dplyr)
library(ggplot2)
# define working directory
wd00 <- getwd()
# define input directory
wd01 <- "data"
# define out directory
wd02 <- "output02_figures_for_analysis_of_filters"
#paste dirs together
wd00_wd01 <- paste0(wd00,"/",wd01)
wd00_wd02 <- paste0(wd00,"/",wd02)
#Delete any previous versions of the output directory
unlink(wd00_wd02, recursive=TRUE)
#Create a directory to put resulting output files in
dir.create(wd00_wd02)

# define file name for input file
inpflNm <- paste0(wd00_wd01,"/Tabel01_v01_med_vandfiltreringsdata_fra_akvariet_2024jul.xlsx")
# read in data frame
df_T01 <- xlsx::read.xlsx2(inpflNm,sheetIndex = 1)
#make numeric
df_T01$fltvol.mL <-  as.numeric(df_T01$Filtreret.volume..mL.)
df_T01$flttim.end <- as.numeric(df_T01$Tidspnkt.slut.filt)
df_T01$flttim.sta <- as.numeric(df_T01$Tidspnkt.start.filt)
# make the date for filtration a factor
df_T01$dato.for.filtrering <- as.factor(df_T01$dato.for.filtrering)
df_T01$dt.f.filt <- gsub("(.*)-(.*)-(.*)","\\3\\2\\1",df_T01$dato.for.filtrering)
df_T01$dt.f.filt <- as.Date(df_T01$dt.f.filt, "%d%b%Y")
# get the hours and minutes and paste together with the date
df_T01$flttim.sta_hh <- substr(df_T01$flttim.sta,1,2)
df_T01$flttim.sta_mm <- substr(df_T01$flttim.sta,3,4)
df_T01$flttim.sta_hh_mm <- paste0(df_T01$dt.f.filt," ",df_T01$flttim.sta_hh,":",df_T01$flttim.sta_mm)
df_T01$flttim.end_hh <- substr(df_T01$flttim.end,1,2)
df_T01$flttim.end_mm <- substr(df_T01$flttim.end,3,4)
df_T01$flttim.end_hh_mm <- paste0(df_T01$dt.f.filt," ",df_T01$flttim.end_hh,":",df_T01$flttim.end_mm)
# make the start and end times time values
df_T01$fltm_end <- as.POSIXct(df_T01$flttim.end_hh_mm, format = "%Y-%m-%d %H:%M")
df_T01$fltm_sta <- as.POSIXct(df_T01$flttim.sta_hh_mm, format = "%Y-%m-%d %H:%M")
# find the difference in time
df_T01$diff.time <- difftime(df_T01$fltm_end, df_T01$fltm_sta, units="mins")
df_T01$diff.time

# make numeric
df_T01$extrvl_mL <- as.numeric(df_T01$ekstraktionsudbytte_vol_fra_filter_mL)
df_T01$extrconc_ngpuL <- as.numeric(df_T01$konc_ng_per_uL)
df_T01$volATL_uL <- as.numeric(df_T01$vol_ATL_buffer_uL)
df_T01$volPK_uL <- as.numeric(df_T01$vol_proteinaseK_uL)

# make the date for extraction a factor
df_T01$ekstraktionsdato <- as.factor(df_T01$ekstraktionsdato)
df_T01$dt.f.extr <- gsub("(.*)-(.*)-(.*)","\\3\\2\\1",df_T01$ekstraktionsdato)
df_T01$dt.f.extr <- as.Date(df_T01$dt.f.extr, "%d%b%Y")
# difference in days between filtering and extraction
df_T01$diff.dt.fil.extr <- df_T01$dt.f.extr - df_T01$dt.f.filt

# work out the filtered volume per time
df_T01$mlpermin <- df_T01$fltvol.mL/(as.numeric(df_T01$diff.time))
# work out the volume used for extraction
df_T01$vlf.extrc <- (df_T01$volATL_uL+df_T01$volPK_uL)
# fine the obtained vol per volume used for the extraction
df_T01$vly.rt <- df_T01$extrvl_mL/ (df_T01$vlf.extrc/1000)
# exclude rows if the filter membrane pore is not defined
df_T01 <- df_T01[(df_T01$Filt.mbr.pore!=""),]
# split the string by delimiter
filt.tp  <- strsplit(as.character(df_T01$Filt.mbr.pore), "_")
# and use the splitted string to get each element and add back in a column
filt.mbr <- sapply(filt.tp, "[[", 1)
filt.psz <- sapply(filt.tp, "[[", 2)
filt.psz[(filt.psz=="NK")] <- "0NK" 
df_T01$filt.smpltp <- filt.psz
df_T01$filt.psz <- gsub("NK","",filt.psz)
df_T01$filt.psz <- as.numeric(df_T01$filt.psz)
df_T01$filt.mbr <- filt.mbr
df_T01$filt.smpltp[grepl("NK",df_T01$filt.smpltp)] <- "NK"
df_T01$filt.smpltp[!grepl("NK",df_T01$filt.smpltp)] <- "smpl"
# get a vector with the number of samples 
no.smpls_indf_T01 <- nrow(df_T01)
# get the number of samples that have been extracted and measured for concentration
no.smpls_extr <- !(is.na(df_T01$extrconc_ngpuL))
no.smpls_extr <- length(no.smpls_extr[no.smpls_extr == TRUE]) 
# get the percentage of samples that have been extracted and measured for concentration
no.smpls_extr/no.smpls_indf_T01*100


# compare in a plot
p <- ggplot(df_T01) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                 y=mlpermin,
                 fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("vol filt (mL/min)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.4)
# see the plot
#p <- p + geom_dotplot(stackdir='center', dotsize=1)
p

# define the output file name
outfl <- paste0(wd00_wd02,"/Fig02_compare_filtered_vol_per_time_v01.png")
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
p <- ggplot(df_T01) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=extrconc_ngpuL,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("extract conc (ng/uL)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.4)
# see the plot
#p <- p + geom_dotplot(stackdir='center', dotsize=1)
p


df_T01$extrconc_ngpuL_Pfltvol.L <- df_T01$extrconc_ngpuL/(df_T01$fltvol.mL/1000)
# compare in a plot
p <- ggplot(df_T01) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=extrconc_ngpuL_Pfltvol.L,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("extract conc (ng/uL) per vol filtered (L)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.4)
# see the plot
#p <- p + geom_dotplot(stackdir='center', dotsize=1)
p

# define the output file name
outfl <- paste0(wd00_wd02,"/Fig03_extracted_conc_per_filtered_liter_v01.png")
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
# make sure the filtered volume is in a numeric format 
df_T01$Filtreret.volume..mL. <- as.numeric(df_T01$Filtreret.volume..mL.)
# compare in a plot
p <- ggplot(df_T01) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=Filtreret.volume..mL.,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("volumen filtreret (mL)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.4)
# see the plot
#p <- p + geom_dotplot(stackdir='center', dotsize=1)
p


# define the output file name
outfl <- paste0(wd00_wd02,"/Fig04_volumen_vand_filtreret.png")
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
p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=mlpermin,
                 fill=Filt.mbr.pore,
                 shape=Filt.Type) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_d() +
  scale_shape_manual(values= c(21,22,23) ) +
  xlab("conc extract (ng/uL)") +
  ylab("vol filt (mL/min)")
# see the plot
p


# compare in a plot
p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=mlpermin,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("conc extract (ng/uL)") +
  ylab("vol filt (mL/min)")
# see the plot
p

#
df_T01$covl <- (df_T01$extrconc_ngpuL*1000)/(df_T01$fltvol.mL/1000)
# compare in a plot
p <- ggplot(df_T01) +
  geom_boxplot(aes(x=Filt.mbr.pore, 
                   y=covl,
                   fill=filt.mbr)) +
  
  xlab("filtertype") +
  ylab("konc i ekstrakt per volumen filtreret ( (ng/mL) / (L) )") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        #legend.position="bottom",
        strip.text = element_text(hjust = 0)) +
  scale_fill_viridis_d(alpha = 0.4)
# see the plot
p
# define the output file name
outfl <- paste0(wd00_wd02,"/Fig05_konc_ekstrakt_per_vol_vand_filtreret.png")
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
p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=fltvol.mL,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("conc extract (ng/uL)") +
  ylab("filtered vol (mL)")
# see the plot
p


# compare in a plot
p <- ggplot(df_T01) +
  geom_point(aes(y=extrconc_ngpuL, 
                 x=diff.dt.fil.extr,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  ylab("conc extract (ng/uL)") +
  xlab("days between filt and extraction")
# see the plot
p

# compare in a plot
p <- ggplot(df_T01) +
  geom_point(aes(y=extrconc_ngpuL_Pfltvol.L, 
                 x=diff.dt.fil.extr,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  ylab("conc extract (ng/uL) per filtered vol (L)") +
  xlab("days between filt and extraction")
# see the plot
p


hh.flt.frz <- substr(df_T01$Tidspnkt.Filt.indfrosset,1,2)
mm.flt.frz <- substr(df_T01$Tidspnkt.Filt.indfrosset,3,4)
hh.flt.end <- substr(df_T01$Tidspnkt.slut.filt,1,2)
mm.flt.end <- substr(df_T01$Tidspnkt.slut.filt,3,4)

tmflt_end <- paste0(df_T01$dt.f.filt," ",hh.flt.end,":",mm.flt.end )
tmflt_frz <- paste0(df_T01$dt.f.filt," ",hh.flt.frz,":",mm.flt.frz )
# make the start and end times time values
df_T01$tmflt_end <- as.POSIXct(tmflt_end, format = "%Y-%m-%d %H:%M")
df_T01$tmflt_frz <- as.POSIXct(tmflt_frz, format = "%Y-%m-%d %H:%M")
# make the time points numeric, and get the time between filtration
# and freezing 
# find the difference in time
df_T01$tm.btw_fil_freez  <- difftime(df_T01$tmflt_frz, df_T01$tmflt_end, units="mins")


# compare in a plot
p <- ggplot(df_T01) +
  geom_point(aes(y=extrconc_ngpuL, 
                 x=tm.btw_fil_freez,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  ylab("conc extract (ng/uL)") +
  xlab("time between end of filtration and freeze (minutes)")
# see the plot
p



p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=filt.psz,
                 fill=fltvol.mL,
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("conc extract (ng/uL)") +
  ylab("filter por size (um)")
# see the plot
p


p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=ekstraktionsudbytte_vol_fra_filter_mL,
                 fill=filt.psz,
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("conc extract (ng/uL)") +
  ylab("ekstraktionsudbytte_vol_fra_filter (mL)")
# see the plot
p

p <- ggplot(df_T01) +
  geom_point(aes(x=filt.psz, 
                 y=extrconc_ngpuL,
                 fill=volATL_uL,
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("filter pore size (um)") +
  ylab("conc extract (ng/uL)")
# see the plot
p

p <- ggplot(df_T01) +
  geom_point(aes(x=extrconc_ngpuL, 
                 y=fltvol.mL/filt.psz,
                 fill=fltvol.mL,
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  xlab("conc extract (ng/uL)") +
  ylab(" filter vol(mL) / filter pore size (um)")
# see the plot
p
# #
df_T01$ekstraktionsudbytte_vol_fra_filter_mL <- as.numeric(df_T01$ekstraktionsudbytte_vol_fra_filter_mL)
p <- ggplot(df_T01) +
  geom_point(aes(y=extrconc_ngpuL, 
                 x=fltvol.mL/ekstraktionsudbytte_vol_fra_filter_mL,
                 fill=log10(filt.psz),
                 shape=filt.mbr) ,
             size =3) +
  theme_minimal() + 
  scale_fill_viridis_c(direction = -1) +
  scale_shape_manual(values= c(21,22,23,24,25) ) +
  ylab("conc extract (ng/uL)") +
  xlab(" filter vol(mL) / extract yield (mL)")
# see the plot
p


# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
library("plot3D")
#install.packages("plot3D")
extrconc_ngpuL <- df_T01$extrconc_ngpuL
filt.psz <- df_T01$filt.psz
fltvol.mL <- df_T01$fltvol.mL
#dev.off()
plot3D::scatter3D(filt.psz,
                  extrconc_ngpuL,
                  fltvol.mL, clab = c("poresize", 
                                      "filtered vol (mL)"),
                  phi = 0, bty = "g",
                  pch = 20, cex = 2, 
                  ticktype = "detailed",
                  main = "filter data",
                  xlab ="poresize (um)",
                  ylab = "extrc conc (ng/uL)",
                  zlab = "flt vol (mL)")


# https://stackoverflow.com/questions/48589924/add-grid-over-3d-surface-using-persp3d-plot3d-package
#persp3D(z=volcano, border="black", lwd=0.3)

# make the "ESXNR","subESXNr" columns numeric
# see: https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
cols.num <- c("ESXNR","subESXNr")
df_T01[cols.num] <- sapply(df_T01[cols.num],as.numeric)
# reorder the data frame
library(dplyr)
# using arrange from dplyr
df_T01 <- df_T01 %>% dplyr::arrange(ESXNR,subESXNr,ekstraktionsdato)
# make the data frame a huxtable
library(huxtable)
library(stringr)
# subset the mtcar data frame to have a smalller data frame to 
# experiment with
# color the entire based evaluation of match in specific column
ht_df_RndAll <- as_hux(df_T01,add_rownames = "rwNo")
# color the entire row based on the value in the column "Filt mbr pore"
ht_df_RndAll  |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "STX_0.22"), value="yellow3") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PLC_0.22"), value="khaki2") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PSE_0.22"), value="lemonchiffon2") |>
  # 
  # 
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PLC_0.80"), value="seagreen3") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PLC_1.20"), value="palegreen3") |>
  # 
  # 
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "STX_0.45"), value="lightblue1") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "NYL_0.45"), value="steelblue3") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PLC_0.40"), value="cornflowerblue") |>
  # 
  # 
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PSE_0.45"), value="orange3") |>
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "PLC_3.00"), value="sandybrown") |>
  # 
  # set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt mbr pore',
  #                                                "NK"), value="grey45") |>
  
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
            file=paste0(wd00_wd02 ,"/Table03_filters_ekstraheret.html"), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)
# define the path to the csv file
path_flwd00_wd02_tabl04 <- paste0(wd00_wd02,"/Table04_filters_ekstraheret.csv")
# Write the data frame to a csv file
write.csv(df_T01,path_flwd00_wd02_tabl04)
#
# define a vector with the columns to keep
ckeep <- c( "Filt.mbr.pore",  "unkFilt.nr", 
  "Filtreret.volume..mL.", "dato.for.filtrering", "Tidspnkt.start.filt", 
  "Tidspnkt.slut.filt", "Tidspnkt.Filt.indfrosset",
  "ekstraktionsdato", "konc_ng_per_uL")
# keep only columns listed in the vector
df_T02 <- df_T01[ckeep]
# make the unique identifier number a character
Pe <- as.character(df_T02$unkFilt.nr)
# the volume filtered need to be presented as characters, not as
# scientific numbers
df_T02$Filtreret.volume..mL. <- as.character(df_T02$Filtreret.volume..mL.)
# pad the unique identifier number with zeroes
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
Pe <- stringr::str_pad(Pe, 3, pad = "0")
# and dd back into the data frame
df_T02$unkFilt.nr <- Pe 
# reorder the data frame by the unique sample number
df_T02 <- df_T02 %>% dplyr::arrange(unkFilt.nr)
# color the entire based evaluation of match in specific column
ht_df_RndAll <- as_hux(df_T02,add_rownames = "rwNo")
# color the entire row based on the value in the column "Filt mbr pore"
ht_df_RndAll  |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "STX_0.22"), value="yellow3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PLC_0.22"), value="khaki2") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PSE_0.22"), value="lemonchiffon2") |>


  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PLC_0.80"), value="seagreen3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PLC_1.20"), value="palegreen3") |>


  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "STX_0.45"), value="lightblue1") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "NYL_0.45"), value="steelblue3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PLC_0.40"), value="cornflowerblue") |>


  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PSE_0.45"), value="orange3") |>
  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "PLC_3.00"), value="sandybrown") |>

  set_background_color(row = stringr::str_detect(ht_df_RndAll$'Filt.mbr.pore',
                                                 "NK"), value="grey45") |>
  
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
            file=paste0(wd00_wd02 ,"/Table05_filters_ekstraheret.html"), 
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)

