rm(list=ls())

### Estimation of univariate HAR-RV model proposed by Corsi.

# We will also estimate the model on the log(RV)

# Libraries
library(HARModel)
library(readxl)

# Reads in data of the Open-to-close returns, and the daily realized variance
Returns <- read_excel("ECTR_CASE_RV_4_assets.xlsx", sheet = "OTC returns")
RV <- read_excel("ECTR_CASE_RV_4_assets.xlsx", sheet = "RV") 

# Transform the Date column, such that we can use this for plotting
Returns <- transform(Returns, Tijd = as.Date(as.character(Tijd), "%Y%m%d"))

# Define model
Size.in.sample <- 1500
RV.in <- RV$AA[1:Size.in.sample]
RVol_simple_day_t1d<-RV.in[-(1:22)]
RVol_simple_day_t<- RV.in[-c(1:21,length(RV.in))]
RVol_simple_week_t<-rollmean(RV.in[-c(1:17,length(RV.in))],5)
RVol_simple_month_t<-rollmean(RV.in[-length(RV.in)],22)

# Estimate HAR-RV model on RV and log RV
HAR <-lm(RVol_simple_day_t1d~ RVol_simple_day_t + RVol_simple_week_t + RVol_simple_month_t )
Log_HAR <-lm(log(RVol_simple_day_t1d)~ log(RVol_simple_day_t) + log(RVol_simple_week_t) + log(RVol_simple_month_t))

# Results
summary(HAR)
summary(Log_HAR)

# Check with package
FitHAR = HAREstimate(RM = RV$AA[1:Size.in.sample], periods = c(1,5,22))
FitHAR
