#### Libraries ####
library(dplyr)
library(gbm)
library(rlang)
library(lubridate)
library(glue)
library(ggplot2)
library(ncdf4)
library(terra)
library(raster)
library(rts)
library(remotes)
library(heatwaveR)
library(mgcv)
library(patchwork)
library(ggpattern)
library(vroom)
library(ragg)
library(ggside)
setwd("/Users/kailafrazer/")

#### Gather climatological SSTs -- preliminary files ####

# Get Sanct cells
uqid <- rast("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/roms_unique_id.grd")
mb <- vect("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 GitHub/NMS Shapefiles/mb/mbnms_py.shp")
mb_cells <- terra::extract(uqid, mb) %>% na.omit() %>% dplyr::select(layer) %>% rename(pixel_id=layer) %>% mutate(sanct_id="mb")

#### Gather climatological SSTs -- IPSL -- one time run ####

# Read in IPSL MB SST time series - generated in "Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/Time Series/Creating Time Series.R"
i_ts = readRDS("C:/Users/kaila/Desktop/Marine Cold-Spell Manuscript/Figures/Fig 7/Time Series/ipsl_mb_ts.rds")
# Gather relevant SST windows
is = dplyr::filter(i_ts, doy >= 152, doy <= 304)
is_win2020 = dplyr::filter(is, year(date_flag) >= 2010, year(date_flag) <= 2039)
is_only2020 = dplyr::filter(is, year(date_flag) >= 2020, year(date_flag) <= 2029)
is_win2080 = dplyr::filter(is, year(date_flag) >= 2070, year(date_flag) <= 2089)
is_only2080 = dplyr::filter(is, year(date_flag) >= 2080, year(date_flag) <= 2089)
is_fixedwin = dplyr::filter(is, year(date_flag) >= 1980, year(date_flag) <= 2009)

# Get detrended MCS days
is_dtmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_ipsl.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
is_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(is_dtmcs)), function(x){seq(as.Date(is_dtmcs[x,]$date_start), as.Date(is_dtmcs[x,]$date_end), by="days")})))
is_mcs_dates = is_mcs_dates %>% as.data.frame() 
is_mcs_dates = dplyr::rename(is_mcs_dates, date_flag=.) %>% distinct()
is_dtmcs_2020 = inner_join(is_only2020, is_mcs_dates)
is_dtmcs_2080 = inner_join(is_only2080, is_mcs_dates)

# Get fixed MCS days
is_fmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_ipsl_normal.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
is_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(is_fmcs)), function(x){seq(as.Date(is_fmcs[x,]$date_start), as.Date(is_fmcs[x,]$date_end), by="days")})))
is_mcs_dates = is_mcs_dates %>% as.data.frame() 
is_mcs_dates = dplyr::rename(is_mcs_dates, date_flag=.) %>% distinct()
is_fmcs_2020 = inner_join(is_only2020, is_mcs_dates)
is_fmcs_2080 = inner_join(is_only2080, is_mcs_dates)

#### Gather climatological SSTs -- HAD -- one time run ####

# Read in HAD MB SST time series - generated in "Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/Time Series/Creating Time Series.R"
h_ts = readRDS("C:/Users/kaila/Desktop/Marine Cold-Spell Manuscript/Figures/Fig 6/Time Series/had_mb_ts.rds")
# Gather relevant SST windows
hs = dplyr::filter(h_ts, doy >= 152, doy <= 304)
hs_win2020 = dplyr::filter(hs, year(date_flag) >= 2010, year(date_flag) <= 2039)
hs_only2020 = dplyr::filter(hs, year(date_flag) >= 2020, year(date_flag) <= 2029)
hs_win2080 = dplyr::filter(hs, year(date_flag) >= 2070, year(date_flag) <= 2089)
hs_only2080 = dplyr::filter(hs, year(date_flag) >= 2080, year(date_flag) <= 2089)
hs_fixedwin = dplyr::filter(hs, year(date_flag) >= 1980, year(date_flag) <= 2009)

# Get detrended MCS days
hs_dtmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_had.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
hs_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(hs_dtmcs)), function(x){seq(as.Date(hs_dtmcs[x,]$date_start), as.Date(hs_dtmcs[x,]$date_end), by="days")})))
hs_mcs_dates = hs_mcs_dates %>% as.data.frame() 
hs_mcs_dates = dplyr::rename(hs_mcs_dates, date_flag=.) %>% distinct()
hs_dtmcs_2020 = inner_join(hs_only2020, hs_mcs_dates)
hs_dtmcs_2080 = inner_join(hs_only2080, hs_mcs_dates)

# Get fixed MCS days
hs_fmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_had_normal.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
hs_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(hs_fmcs)), function(x){seq(as.Date(hs_fmcs[x,]$date_start), as.Date(hs_fmcs[x,]$date_end), by="days")})))
hs_mcs_dates = hs_mcs_dates %>% as.data.frame() 
hs_mcs_dates = dplyr::rename(hs_mcs_dates, date_flag=.) %>% distinct()
hs_fmcs_2020 = inner_join(hs_only2020, hs_mcs_dates)
hs_fmcs_2080 = inner_join(hs_only2080, hs_mcs_dates)

#### Gather climatological SSTs -- GFDL -- one time run ####

# Read in GFDL MB SST time series - generated in "Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/Time Series/Creating Time Series.R"
g_ts = readRDS("C:/Users/kaila/Desktop/Marine Cold-Spell Manuscript/Figures/Fig 6/Time Series/gfdl_mb_ts.rds")
# Gather relevant SST windows
gs = dplyr::filter(g_ts, doy >= 152, doy <= 304)
gs_win2020 = dplyr::filter(gs, year(date_flag) >= 2010, year(date_flag) <= 2039)
gs_only2020 = dplyr::filter(gs, year(date_flag) >= 2020, year(date_flag) <= 2029)
gs_win2080 = dplyr::filter(gs, year(date_flag) >= 2070, year(date_flag) <= 2089)
gs_only2080 = dplyr::filter(gs, year(date_flag) >= 2080, year(date_flag) <= 2089)
gs_fixedwin = dplyr::filter(gs, year(date_flag) >= 1980, year(date_flag) <= 2009)

# Get detrended MCS days
gs_dtmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_gfdl.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
gs_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(gs_dtmcs)), function(x){seq(as.Date(gs_dtmcs[x,]$date_start), as.Date(gs_dtmcs[x,]$date_end), by="days")})))
gs_mcs_dates = gs_mcs_dates %>% as.data.frame() 
gs_mcs_dates = dplyr::rename(gs_mcs_dates, date_flag=.) %>% distinct()
gs_dtmcs_2020 = inner_join(gs_only2020, gs_mcs_dates)
gs_dtmcs_2080 = inner_join(gs_only2080, gs_mcs_dates)

# Get fixed MCS days
gs_fmcs <- vroom(glue("D:/Dropbox Backup [MCS Stuff] 1-11-25/Kaila_newMCSs/MCSs Detected/mcs_gfdl_normal.csv")) %>% inner_join(mb_cells) %>% dplyr::filter(month(date_start) >= 07, month(date_start) <= 10, month(date_end) <= 10, month(date_end) >= 07, year(date_start) <= 2099)
gs_mcs_dates <- as.Date(unlist(lapply(seq(1:nrow(gs_fmcs)), function(x){seq(as.Date(gs_fmcs[x,]$date_start), as.Date(gs_fmcs[x,]$date_end), by="days")})))
gs_mcs_dates = gs_mcs_dates %>% as.data.frame() 
gs_mcs_dates = dplyr::rename(gs_mcs_dates, date_flag=.) %>% distinct()
gs_fmcs_2020 = inner_join(gs_only2020, gs_mcs_dates)
gs_fmcs_2080 = inner_join(gs_only2080, gs_mcs_dates)

#### Ensemble SSTs - all values :) ####

win2020 = full_join(gs_win2020, is_win2020) %>% full_join(hs_win2020)
win2080 = full_join(gs_win2080, is_win2080) %>% full_join(hs_win2080)
fixedwin = full_join(gs_fixedwin, is_fixedwin) %>% full_join(hs_fixedwin)

dtmcs_2020 = full_join(gs_dtmcs_2020, is_dtmcs_2020) %>% full_join(hs_dtmcs_2020)
dtmcs_2080 = full_join(gs_dtmcs_2080, is_dtmcs_2080) %>% full_join(hs_dtmcs_2080)

fmcs_2020 = full_join(gs_fmcs_2020, is_fmcs_2020) %>% full_join(hs_fmcs_2020)
fmcs_2080 = full_join(gs_fmcs_2080, is_fmcs_2020) %>% full_join(hs_fmcs_2080)

saveRDS(win2020, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/win2020_ensemble_spread.rds")
saveRDS(win2080, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/win2080_ensemble_spread.rds")
saveRDS(fixedwin, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/fixedwin_ensemble_spread.rds")
saveRDS(dtmcs_2020, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/dtmcs_2020_ensemble_spread.rds")
saveRDS(dtmcs_2080, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/dtmcs_2080_ensemble_spread.rds")
saveRDS(fmcs_2020, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/fmcs_2020_ensemble_spread.rds")
saveRDS(fmcs_2080, "C:/Users/kaila/Desktop/Marine Cold-spell Manuscript/Figures/Fig 6/fmcs_2080_ensemble_spread.rds")

#### Read in ensemble SSTs ####

win2020 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/win2020_ensemble_spread.rds")
win2080 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/win2080_ensemble_spread.rds")
fixedwin <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/fixedwin_ensemble_spread.rds")
dtmcs_2020 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/dtmcs_2020_ensemble_spread.rds")
dtmcs_2080 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/dtmcs_2080_ensemble_spread.rds")
fmcs_2020 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/fmcs_2020_ensemble_spread.rds")
fmcs_2080 <- readRDS("Desktop/MCS/Marine Cold-spell Manuscript/Original Submission Figures/Fig 7/fmcs_2080_ensemble_spread.rds")

#### Blue whale seasonal response curves ####

#1. Read in four blue whale models
#2. Set up data for prediction
#3. Predict models
#4. Average BRT and GAM predictions
#5. Find seasonal ensemble model response for June-October
#6. Plot
#7. Find order of importance of variables in model

#1. Read in four blue whale models
setwd("Desktop/MCS/Honors/BLWH Models/")
blwh.sf.gam=readRDS("GAMs/blwh.res1.gam.sf.mod1.rds")
blwh.ws.gam=readRDS("GAMs/blwh.res1.gam.ws.mod1.rds")

summary.gam(blwh.sf.gam)

# set up data for GAM prediction based on BRT prediction
gam.dat = {expand.grid(ptt = 723029, # levels(blwh.sf$model$ptt), 
                       sst_mean_0.1 = seq(min(blwh.sf.gam$model$sst_mean_0.1), 
                                          max(blwh.sf.gam$model$sst_mean_0.1), length = 100), 
                       z_0.1 = mean(blwh.sf.gam$model$z_0.1), 
                       ssh_sd_1 = mean(blwh.sf.gam$model$ssh_sd_1),
                       zsd_1 = mean(blwh.sf.gam$model$zsd_1),
                       ild_mean_0.1 = mean(blwh.sf.gam$model$ild_mean_0.1),
                       EKE_0.1 = mean(blwh.sf.gam$model$EKE_0.1))}
# GAM prediction
sf.gam.pred = cbind(gam.dat, predict(blwh.sf.gam, newdata = gam.dat, se.fit = T, type = "link"))
ws.gam.pred = cbind(gam.dat, predict(blwh.ws.gam, newdata = gam.dat, se.fit = T, type = "link"))
# back transform because i'm converting in the link space. the link is "logit" because i'm using binomial presabs data. "plogis" is the inverse of "logit"
sf.gam.pred$est = plogis(sf.gam.pred$fit)
ws.gam.pred$est = plogis(ws.gam.pred$fit)

sf.gam.pred$lower = plogis(sf.gam.pred$fit - 1.96*sf.gam.pred$se.fit) 
sf.gam.pred$upper = plogis(sf.gam.pred$fit + 1.96*sf.gam.pred$se.fit)
ws.gam.pred$lower = plogis(ws.gam.pred$fit - 1.96*ws.gam.pred$se.fit) 
ws.gam.pred$upper = plogis(ws.gam.pred$fit + 1.96*ws.gam.pred$se.fit)
# Plot
ggplot(data=sf.gam.pred, aes(x=sst_mean_0.1, y=est)) + geom_line() + geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey70", alpha = 0.5) + xlab("SST") + ylab("Predicted Habitat Suitability") + ggtitle("SF GAM Response")
ggplot(data=ws.gam.pred, aes(x=sst_mean_0.1, y=est)) + geom_line() + geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey70", alpha=0.5) + xlab("SST") + ylab("Predicted Habitat Suitability") + ggtitle("WS GAM Response")

# Combine results of predictions
blwh.ws = dplyr::select(ws.gam.pred,sst_mean_0.1,est, lower, upper) %>% rename(sst=sst_mean_0.1,ws.est=est,ws.low=lower,ws.up=upper)
blwh.sf = dplyr::select(sf.gam.pred,sst_mean_0.1,est, lower, upper) %>% rename(sst=sst_mean_0.1,sf.est=est,sf.low=lower,sf.up=upper)
blwh.pred = full_join(blwh.ws, blwh.sf)

#5. Find seasonal ensemble model response for June-October
# weightings between SF WS models - weightings for SF model are 1 - WS weightings
ws_weightings=c(rep(1,22), .8,.6,.4,.2, rep(0,17),.2,.4,.6,.8, rep(1,6))
june_weights=ws_weightings[week("01-06-2000"):week("30-06-2000")]
july_weights=ws_weightings[week("01-07-2000"):week("31-07-2000")]
aug_weights=ws_weightings[week("01-08-2000"):week("31-08-2000")]
sep_weights=ws_weightings[week("01-09-2000"):week("30-09-2000")]
oct_weights=ws_weightings[week("01-10-2000"):week("31-10-2000")]
# turns out July-October all have the same weightings (100% SF model)
jul_oct_response=blwh.sf
jun_response=mutate(blwh.pred, ws.weight=ws.est*june_weights) %>% 
  mutate(sf.weight=sf.est*(1-june_weights)) %>% 
  mutate(pred=ws.weight+sf.weight) %>%
  mutate(ws.low=ws.low*june_weights, ws.up=ws.up*june_weights, sf.low=sf.low*(1-june_weights), sf.up=sf.up*(1-june_weights)) %>%
  mutate(low=ws.low+sf.low, up=ws.up+sf.up)

#6. Plot model response

ggplot() + 
  geom_line(data=jul_oct_response,aes(x=sst,y=sf.est,color="July-October"))+
  geom_line(data=jun_response, aes(x=sst,y=pred,color="June"))+
  xlab("SST")+ylab("Habitat Response")+
  scale_color_manual("Month", values=c("June"="#0d0887", "July-October"="#f89540"))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#7. Find order of importance of environmental variables

summary.gam(blwh.sf.gam)
summary.gam(blwh.ws.gam)

#### Leatherback turtle response curves ####

#1. Read in model
#2. Predict model
#3. Plot
#4. Find importance of environmental variables

#1. Read in four blue whale models
setwd("/Volumes/One Touch/"); lbst.brt=readRDS("Dropbox Backup [MCS Stuff] 1-11-25/Models/Kaila/lbst_noSSH.res1.tc3.lr01.single.rds")

summary.gbm(lbst.brt) # gaining info on variable importance

#2. Set up data for prediction and #3. Predict models
# plot marginal effects of BRTs
lbst.dat=plot.gbm(lbst.brt,i.var = "sst",return.grid = T,smooth=T,type="response")# plot.gbm() plots marginal effect
# add warmer ssts
brt.ssts=c(lbst.dat$sst,seq(24,45)) %>% as.data.frame()
brt.ssts=brt.ssts[order(brt.ssts$.),]
brt.ssts=as.data.frame(brt.ssts) %>% dplyr::rename("sst" = "brt.ssts")
# fill in predictions for full set of SST values
lbst.dat=left_join(brt.ssts,lbst.dat,by="sst")
for (i in 0:(nrow(lbst.dat)-1)) {
  if (is.na(lbst.dat$y[i+1])) {
    lbst.dat$y[i+1] = lbst.dat$y[i]
  }
}

#3. Plot

# lbst <- ggplot() +
#   geom_line(data=lbst.dat,aes(x=sst,y=y))+
#   xlab("SST")+ylab("Habitat Response")+
#   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#4. Find importance of environmental variables
summary.gbm(lbst.brt)
#### Compile final plots ####

# Set theme pre-plotting
theme <- theme_set(theme_classic())
theme_set(theme(axis.title=element_text(size=10,face="bold"), axis.text=element_text(size=8), legend.text=element_text(size=8), legend.title=element_text(size=10,face="bold"), plot.title=element_text(size=12,face="bold",hjust=0),panel.grid.major=element_blank(),legend.background=element_blank(),ggside.axis.text.y=element_blank(),ggside.axis.ticks.y=element_blank(),ggside.axis.line.y=element_blank(),plot.subtitle=element_text(size=8)))

blwh_2020_juloct_dt <- {ggplot() + 
  # blue whale response curves
  geom_line(data=jul_oct_response, aes(x=sst,y=sf.est))+
  geom_ribbon(data=jul_oct_response, aes(x=sst,y=sf.est,ymin=sf.low, ymax=sf.up), fill="grey80", alpha=0.5)+
  xlab("SST (°C)")+ylab("Habitat suitability")+
  # MB histogram
  geom_vline(xintercept=mean(dtmcs_2020$mean_value), color="#586F7C")+
  geom_vline(xintercept=mean(win2020$mean_value), color="#FB4B4E")+
  # themes
  labs(title="A. Blue Whale\nResponse Curve", subtitle="Model. Ensemble\nSanct. MB\nDecade. 2020\nType. Detrended MCS")+ scale_x_continuous(limits=c(7,24)) +
  # sst
  geom_xsidedensity(data=dtmcs_2020,aes(x=mean_value,y=after_stat(ndensity), fill="2020 Detrended\nMCS SSTs"), color="#586F7C",linewidth=0,alpha=.3) +
  geom_xsidedensity(data=win2020,aes(x=mean_value,y=after_stat(ndensity), fill="2010-2039\nTypical SSTs"), color="#FB4B4E", linewidth=0, alpha=0.3) +
  scale_fill_manual("", values=c("2010-2039\nTypical SSTs" = "#FB4B4E", "2020 Detrended\nMCS SSTs" = "#586F7C")) +
  theme_classic()+theme_get()}

lbst_2080_juloct_dt <- {ggplot() + 
    # Response curve
    geom_line(data=lbst.dat, aes(x=sst,y=y))+xlim(7,28)+
    xlab("SST (°C)")+ylab("")+
    geom_vline(xintercept=mean(dtmcs_2080$mean_value), color="#586F7C")+
    geom_vline(xintercept=mean(win2080$mean_value), color="#FB4B4E")+
    scale_x_continuous(limits=c(7,24)) + labs(title="B. Leatherback Sea Turtles\nResponse Curve", subtitle="Model. Ensemble\nSanct. MB\nDecade. 2080\nType. Detrended MCS") +
    # MB histogram
    geom_xsidedensity(data=dtmcs_2080,aes(x=mean_value,y=after_stat(ndensity), fill="2080 Detrended\nMCS SSTs"), color="#586F7C",linewidth=0,alpha=.3)+
    geom_xsidedensity(data=win2080,aes(x=mean_value,y=after_stat(ndensity), fill="2070-2099\nTypical SSTs"), color="#FB4B4E", linewidth=0, alpha=0.3)+
    # colors
    scale_fill_manual("", values=c("2070-2099\nTypical SSTs" = "#FB4B4E", "2080 Detrended\nMCS SSTs" = "#586F7C")) +
    # themes
    theme_classic()+theme_get()+theme()}

setwd("/Users/kailafrazer/Desktop/MCS/Marine Cold-spell Manuscript/PLOS Submission 2 Final Materials/300dpi Figures/")

ragg::agg_tiff("Fig7.tiff", width = 7.5, height = 4, units = "in", res = 300)

blwh_2020_juloct_dt + lbst_2080_juloct_dt

dev.off()
