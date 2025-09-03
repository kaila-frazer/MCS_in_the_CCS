#1. Libraries ####

library(dplyr)
library(glue)
library(lubridate)
library(ggplot2)
library(tidyterra)
library(terra)
library(patchwork)
library(cowplot)
library(vroom)
library(ragg)

#2. Load foundational data ####

# Here, I retrieve the dataframes of species' responses to marine cold-spells generated in a the Measure Habitat.R script.

setwd("/Volumes/One Touch/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/")

# Detrended habitat
dt_gfdl <- vroom("Habitat Detected/contemp_newthresh_gfdl.csv")
dt_had <- vroom("Habitat Detected/contemp_newthresh_had.csv")
dt_ipsl <- vroom("Habitat Detected/contemp_newthresh_ipsl.csv")
# Normal habitat
nor_gfdl <- vroom("Habitat Detected/contemp_newthresh_gfdl_normal.csv")
nor_had <- vroom("Habitat Detected/contemp_newthresh_had_normal.csv")
nor_ipsl <- vroom("Habitat Detected/contemp_newthresh_ipsl_normal.csv")

#3. A little data manipulation ####

# Define a function to summarize and decade check
decchek <- function(csv) {
  # Separate CSV into June and not June dates
  juncsv <- csv %>% dplyr::filter(month=="06")
  juloctcsv <- csv %>% dplyr::filter(month!="06")
  # Preliminary summarizing
  junsmrz <- juncsv %>% group_by(sanctuary, decade) %>% reframe(jun_blwh_sd=sd(blwh_reldl), jun_lbst_sd=sd(lbst_reldl), jun_blwh_reldl=mean(blwh_reldl), jun_lbst_reldl=mean(lbst_reldl))
  juloctsmrz <- juloctcsv %>% group_by(sanctuary, decade) %>% reframe(juloct_blwh_sd=sd(blwh_reldl), juloct_lbst_sd=sd(lbst_reldl), juloct_blwh_reldl=mean(blwh_reldl), juloct_lbst_reldl=mean(lbst_reldl))
  smrz <- full_join(junsmrz, juloctsmrz)
  # Fill empty decade
  for (s in c("gf", "cb", "mb", "ch", "ci")) {
    for (d in c(1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090)) {
      if (length(which(filter(smrz, sanctuary==s)$decade==d))==0) {
        smrz <- rbind(smrz, data.frame(sanctuary=s, decade=as.numeric(d), jun_blwh_sd=as.double(0), jun_lbst_sd=as.double(0), jun_blwh_reldl=as.double(0), jun_lbst_reldl=as.double(0), juloct_blwh_sd=as.double(0), juloct_lbst_sd=as.double(0), juloct_blwh_reldl=as.double(0), juloct_lbst_reldl=as.double(0)))
      }
    }
  }
  # Return
  return(smrz)
}

# Join data
dt <- full_join(decchek(dt_gfdl), decchek(dt_had)) %>% full_join(decchek(dt_ipsl))
nor <- full_join(decchek(nor_gfdl), decchek(nor_had)) %>% full_join(decchek(nor_ipsl))

# Final summary
dt <- dt %>% group_by(sanctuary, decade) %>% reframe(jun_blwh_reldl=mean(jun_blwh_reldl), juloct_blwh_reldl=mean(juloct_blwh_reldl), jun_lbst_reldl=mean(jun_lbst_reldl), juloct_lbst_reldl=mean(juloct_lbst_reldl), jun_blwh_sd=mean(jun_blwh_sd), juloct_blwh_sd=mean(juloct_blwh_sd), jun_lbst_sd=mean(jun_lbst_sd), juloct_lbst_sd=mean(juloct_lbst_sd))
nor <- nor %>% group_by(sanctuary, decade) %>% reframe(jun_blwh_reldl=mean(jun_blwh_reldl), juloct_blwh_reldl=mean(juloct_blwh_reldl), jun_lbst_reldl=mean(jun_lbst_reldl), juloct_lbst_reldl=mean(juloct_lbst_reldl), jun_blwh_sd=mean(jun_blwh_sd), juloct_blwh_sd=mean(juloct_blwh_sd), jun_lbst_sd=mean(jun_lbst_sd), juloct_lbst_sd=mean(juloct_lbst_sd))

#4. Misc prep for plotting ####

# Set NMS information
snct_names <- data.frame(sanctuary=c("gf", "cb", "mb", "ch", "ci"), snct_name=c("Greater Farallones", "Cordell Bank", "Monterey Bay", "Chumash Heritage", "Channel Islands"))
# Order NMSs within dataframes
dt <- right_join(dt, snct_names); dt$order = factor(dt$snct_name, ordered=T, levels=c("Channel Islands", "Chumash Heritage", "Monterey Bay", "Cordell Bank", "Greater Farallones"))
nor <- right_join(nor, snct_names); nor$order = factor(nor$snct_name, ordered=T, levels=c("Channel Islands", "Chumash Heritage", "Monterey Bay", "Cordell Bank", "Greater Farallones"))

# Theme :)
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8), legend.title=element_text(size=10,face="bold"), plot.title=element_text(size=12,face="bold",hjust=0),panel.grid.major=element_blank(),panel.border=element_rect(color="black",fill=NA,linewidth=1),axis.line=element_blank()))

#5. Plot :) ####

{nor_blwh_juloct <- ggplot(nor) + geom_tile(aes(x=decade, y=order, fill=juloct_blwh_reldl), color="white")+coord_fixed(ratio=11)+scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", limits=c(-20,20), midpoint=0) + labs(x="", y="", title="A. Blue Whale Response\nto Fixed Cold-spells") + theme_classic()+theme_get() + theme(axis.text.x=element_blank())

dt_blwh_juloct <- ggplot(dt) + geom_tile(aes(x=decade, y=order, fill=juloct_blwh_reldl), color="white")+coord_fixed(ratio=11)+scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", limits=c(-20,20), midpoint=0) + labs(x="Decade", y="", title="B. Blue Whale Response\nto Detrended Cold-spells") + scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080)) + theme_classic()+theme_get()}

{nor_blwh_jun <- ggplot(nor) + geom_tile(aes(x=decade, y=order, fill=jun_blwh_reldl), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", midpoint=0) + labs(x="", y="", title="A. Blue Whale Response\nto Fixed Cold-spells") + theme_classic()+theme_get() + theme(axis.text.x=element_blank())

dt_blwh_jun <- ggplot(dt) + geom_tile(aes(x=decade, y=order, fill=jun_blwh_reldl), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", midpoint=0) + labs(x="Decade", y="", title="A. Blue Whale Response\nto Detrended Cold-spells\nin June") + scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080)) + theme_classic()+theme_get()}

{nor_lbst_juloct <- ggplot(nor) + geom_tile(aes(x=decade, y=order, fill=juloct_lbst_reldl), color="white")+coord_fixed(ratio=11)+scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", limits=c(-20,20), midpoint=0) + labs(x="", y="", title="B. Leatherback Response\nto Fixed Cold-spells") + theme_classic()+theme_get() + theme(axis.text.y=element_blank(), axis.text.x=element_blank())
  
dt_lbst_juloct <- ggplot(dt) + geom_tile(aes(x=decade, y=order, fill=juloct_lbst_reldl), color="white")+coord_fixed(ratio=11)+scale_fill_gradient2("Relative Δ\nCore Habitat (%)", high="#cf4446", low="#31688e", mid="white", na.value="white", limits=c(-20,20), midpoint=0) + labs(x="Decade", y="", title="D. Leatherback Response\nto Detrended Cold-spells") + theme_classic()+theme_get() + theme(axis.text.y=element_blank())}

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig6.tiff", width = 7.5, height = 4, units = "in", res = 300)

(nor_blwh_juloct | nor_lbst_juloct) / (dt_blwh_juloct | dt_lbst_juloct) + plot_layout(guides="collect") # 1400 x 1000 works for insets

dev.off()

#6. Plot standard deviations ####

norsd_blwh <- ggplot(nor) + geom_tile(aes(x=decade, y=order, fill=juloct_blwh_sd), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient("SD Δ\nCore\nHabitat", high="#fde725", low="#21918c", na.value="#21918c") + labs(x="", y="", title="A. Blue Whale Response\nto Fixed Cold-spells") + theme_classic()+ theme_get() + theme(axis.text.x=element_blank())

dtsd_blwh <- ggplot(dt) + geom_tile(aes(x=decade, y=order, fill=juloct_blwh_sd), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient("SD Δ\nCore\nHabitat", high="#fde725", low="#21918c", na.value="#21918c") + labs(x="Decade", y="", title="C. Blue Whale Response\nto Detrended Cold-spells") + scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080)) + theme_classic()+theme_get()

dtsd_lbst <- ggplot(dt) + geom_tile(aes(x=decade, y=order, fill=juloct_lbst_sd), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient("SD Δ\nCore\nHabitat", high="#fde725", low="#21918c", na.value="#21918c") + labs(x="Decade", y="", title="D. Leatherback Response\nto Detrended Cold-spells") + scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080)) + theme_classic()+theme_get() +theme(axis.text.y = element_blank())

norsd_lbst <- ggplot(nor) + geom_tile(aes(x=decade, y=order, fill=juloct_lbst_sd), color="white")+coord_fixed(ratio=11)+ scale_fill_gradient("SD Δ\nCore\nHabitat", high="#fde725", low="#21918c", na.value="#21918c") + labs(x="", y="", title="B. Leatherback Response\nto Fixed Cold-spells") +theme_classic()+ theme_get() +theme(axis.text.y=element_blank(), axis.text.x=element_blank())

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("S5Fig.tiff", width = 7.5, height = 3.5, units = "in", res = 300)

(norsd_blwh | norsd_lbst) / (dtsd_blwh | dtsd_lbst) #1200  700

dev.off()

#7. Gather some stats ####

past <- c(1980, 1990, 2000)
fut <- c(2070, 2080, 2090)

dtp <- dplyr::filter(dt, decade %in% past); dtf <- dplyr::filter(dt, decade %in% fut) # hahahahaha "detrended future"
dtf_blwh <- mean(dtf$juloct_blwh_reldl); dtf_lbst <- mean(dtf$juloct_lbst_reldl)
dtp_blwh <- mean(dtp$juloct_blwh_reldl); dtp_lbst <- mean(dtp$juloct_lbst_reldl)

norp <- nor %>% filter(decade %in% past); norf <- nor %>% filter(decade %in% fut)
norp_blwh <- mean(norp$juloct_blwh_reldl); norp_lbst <- mean(norp$juloct_lbst_reldl)
norf_blwh <- mean(norf$juloct_blwh_reldl); norf_lbst <- mean(norf$juloct_lbst_reldl)
