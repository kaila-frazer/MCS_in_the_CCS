# This code was written in collaboration between Heather Welch and Kaila Frazer. Kaila Frazer did the final editing.

#### Leatherbacks ####

library(tidyverse)
library(glue)
library(gbm)
library(mgcv)
library(dismo)
library(lubridate)

lbst_model=readRDS("/Volumes/One Touch/Dropbox Backup [MCS Stuff] 1-11-25/Models/Kaila/lbst_noSSH.res1.tc3.lr01.single.rds") # Read in leatherback model

fit.dat=readRDS("/Users/kailafrazer/Desktop/MCS/Honors/Habitat/lbst_bstrp_list.rds")[[1]] # Read in data used to fit the leatherback model

pred=as.data.frame(predict.gbm(lbst_model, fit.dat, n.trees=lbst_model$gbm.call$best.trees, type="response"))[,1]

glimpse(pred) # Contains predictions from the model at each row of fit.dat

master_dat <- dplyr::select(fit.dat, PresAbs) %>% mutate(pred=pred)

pres=master_dat %>% filter(PresAbs==1) %>% pull(pred)

abs=master_dat %>% filter(PresAbs==0) %>% pull(pred)

e <- dismo::evaluate(p=pres, a=abs) 

dismo::threshold(e) # Find leatherback equal_sens_spec threshold

#### Blue whales ####

## steps
#1. read in 4 blue whale models
#2. read in blue whale data
#3. predict each of the blue whale models onto data
#4. ensemble predictions using weighted day of week formula
#5. run evaluate()
#6. run threshold()

library(tidyverse)
library(glue)
library(gbm)
library(mgcv)
library(dismo)
library(lubridate)


gam_sf=readRDS("/Users/kailafrazer/Desktop/MCS/Honors/BLWH Models/GAMs/blwh.res1.gam.sf.mod1.rds")
gam_ws=readRDS("/Users/kailafrazer/Desktop/MCS/Honors/BLWH Models/GAMs/blwh.res1.gam.ws.mod1.rds")
# Read in blue whale GAMs

fit.dat=readRDS("/Volumes/One Touch/Dropbox Backup [MCS Stuff] 1-11-25/Models/blwh_ROMSvar_list.rds")[[1]]%>%  
  mutate(ptt = 723029) # Read in data used to predict the blue whale GAMs

pred_gam_ws=as.data.frame(predict.gam(gam_ws,newdata=fit.dat, type = 'response'))[,1]
pred_gam_sf=as.data.frame(predict.gam(gam_sf,newdata=fit.dat, type = 'response'))[,1]

ws_mod_preds <- pred_gam_ws #cbind(pred_gam_ws, pred_brt_ws); ws_mod_preds <- rowMeans(ws_mod_preds, na.rm=T) # Averaging ws models
sf_mod_preds <- pred_gam_sf #cbind(pred_gam_sf, pred_brt_sf); sf_mod_preds <- rowMeans(sf_mod_preds, na.rm=T) # Averaging sf models

# Assign winter/spring model weightings for each week of year
# Weightings for summer/fall model are 1 - winter/spring weightings
ws_weightings <- c(rep(1,22), .8,.6,.4,.2, rep(0,17),.2,.4,.6,.8, rep(1,6)) 

master_dat=fit.dat 
master_dat <- dplyr::select(master_dat, dt,presabs)
master_dat <- mutate(master_dat, ws_mod_preds2=ws_mod_preds)
master_dat <- mutate(master_dat, sf_mod_preds2=sf_mod_preds) 
master_dat <- mutate(master_dat, week=week(dt)) # Ensembling works on weeks
master_dat <- mutate(master_dat, weighting=ws_weightings[week]) # Find out what the weightings are for each week of year
master_dat <- mutate(master_dat, ws_mod_weighted=ws_mod_preds2*weighting) # Weight the ws model by week of year
master_dat <- mutate(master_dat, sf_mod_weighted=sf_mod_preds2*(1-weighting)) # Weight the sf model by day of year
master_dat <- mutate(master_dat, mod_preds=ws_mod_weighted+sf_mod_weighted) # Combine the weighted ws and sf model predictions

# Take a look at what you just did for fun
test=master_dat %>% 
  dplyr::select(c(week,ws_mod_weighted,sf_mod_weighted,mod_preds)) %>% 
  group_by(week) %>% 
  summarise(ws_mod_weighted=mean(ws_mod_weighted,na.rm=T),
            sf_mod_weighted=mean(sf_mod_weighted,na.rm=T),
            mod_preds=mean(mod_preds,na.rm=T)
  ) %>% 
  gather(model, predictions,-c(week))

ggplot(test,aes(x=week,y=predictions,fill=model))+
  geom_bar(stat="identity",position="dodge")

# Filter out presence data
pres=master_dat %>% 
  filter(presabs==1) %>% 
  pull(mod_preds)

# Filter out absence data
abs=master_dat %>% 
  filter(presabs==0) %>% 
  pull(mod_preds)

e <- dismo::evaluate(p=pres, a=abs) 

dismo::threshold(e) # Find blue whale equal_sens_spec threshold
