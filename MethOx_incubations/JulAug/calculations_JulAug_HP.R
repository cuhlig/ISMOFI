#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------

#HP range JulAug
#processing bags data for each seperate bag: 
#calculation of first order constant k from concentration and isotope ratios


#empty workspace
rm(list=ls())

#set work directory to source file location
#R: Session -> Set Working Directory -> To Source File Location

mainDir <- getwd()
subDir <- "outdir"

#make out directory if it does not exist yet
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
}

#set path to out directory
newdir <- paste(mainDir,
                "/",
                subDir,
                "/",
                sep=""
)

setwd(newdir)


#calibration table for HP range JulAug
cali.HP.JulAug <- read.table("../JulAug.cali.HP.final.txt", header=TRUE, sep="\t", comment.char="", stringsAsFactors=FALSE)

# 1x Barrow cubitainer; 4 bags 62, 63, 64_BES, 65_NaOH; 
# 1x Narragansett Bay; 4 bags 50, 51, 52_BES, 53_NaOH; 
# 50x Barrow cubitainer; 4 bags 66, 67, 68_BES, 69_NaOH; 
# 50x Narragansett Bay; 4 bags 56, 58, 59_BES, 61_NaOH; 



filelist <- c(
      "Bcubi.oxi.62.txt"
    )


file_names <- as.list(filelist)


for (file_name in file_names) {
  
   data.oxi <- read.table(paste(file_name))
  #print(data.oxi)
  
  #merge daily calibration values
  data.oxi <- merge(data.oxi, cali.HP.JulAug, by.x = "Date", by.y = "date", all.x = TRUE)
  
  #calculate time since start of experiment in days from timestamp
  source("../../../R_lib/daysfromts.R")
  data.oxi$time.days <- daysfromts(data.oxi$Timestamp)
  
  #calculate calibrated CH4 concentration and delta
  source("../../../R_lib/calibrateCH4deltappm.R") #  calCH4deltappm <- function(Picarro.ppm, b.ppm, m.ppm, data.oxi$SSIM.dil.factor, Picarro.delta, b.delta, m.delta){
  calibrated.CH4.HP <- calCH4deltappm(data.oxi$HP.CH4.Mean, data.oxi$b.ppm, data.oxi$m.ppm, data.oxi$SSIM.dil.factor, data.oxi$HP.Delta.iCH4.Mean.corr, data.oxi$b.delta, data.oxi$m.delta)

  #print(calibrated.CH4.HP)
  data.oxi <- cbind(data.oxi, calibrated.CH4.HP)  
   
  #calculate sw density with swdens function
  source("../../../R_lib/swdens.R")
  data.oxi$rho.sw <- swdens(data.oxi$T.equil, data.oxi$sal)
  
  #calculate volume of sea water in bag in L
  data.oxi$V.sw = data.oxi$m.water / data.oxi$rho.sw
  
  # calculate mass and concentations of methane in solution with function (Magen 2014)
  #       in water
  #       headspace
  #       total
  data.oxi$V.hs <-
    data.oxi$V.hs.initial - data.oxi$hs.removed * 0.001 #in L
  data.oxi$V.gas <-
    data.oxi$V.hs #in L for bags V.gas is eaqual to V.hs since the bags are flexible
  
  source("../../../R_lib/CH4solubility.R") #CH4sol <- function(temp, sal, V.hs, V.gas, V.sw, hs.ppm)
  solubility.values <-
    CH4solubility(
      data.oxi$T.equil,
      data.oxi$sal,
      data.oxi$V.hs,
      data.oxi$V.gas,
      data.oxi$V.sw,
      data.oxi$hs.ppm
    )

  #print(solubility.values)
  data.oxi <- cbind(data.oxi, solubility.values)


  
  # calculate mass of methane which was removed for sampling and correct the total mass
  
  data.oxi$V.discarded = 0 # no correction for discarded volumen necessary since this was already included in the hs.removed amount
  
    data.oxi$V.sampled <-
    data.oxi$vol.mL + data.oxi$V.discarded # total volume which was removed by sampling, comprises columns V.mL and V.discarded, i.e. sampled volumes used for measurement including the ones we had to discard because they were bad
  
  data.oxi$V.leftover <- data.oxi$V.hs.initial - data.oxi$hs.removed*10^(-3)
  
  V.leftover.day_n <- unique(data.oxi$V.leftover)
  
  sample_day_ids <- unique(data.oxi$Date.ID)
  
  
  
  # Create arrays that hold the daywise total amount 
  mean.n.CH4.hs <- rep(0.0, times=length(sample_day_ids))
  daysum.V.sampled <- rep(0.0, times=length(sample_day_ids))  
  
  for (sample_day_id in sample_day_ids) {
    
    # get the index of the reduced (day-wise) arrays
    sample_day_id_index <- match(sample_day_id, sample_day_ids)
    
    # get a list of all samples indices for the given day
    sample_indices <- which(data.oxi$Date.ID == sample_day_id)
    
    # Mean if more than one sample
    mean.n.CH4.hs[sample_day_id_index] <- mean(data.oxi$n.CH4.hs[c(sample_indices)])
  
    # sum of sampled volumes for replicate samples at respective sampling day in mL
    daysum.V.sampled[sample_day_id_index] <- sum(data.oxi$V.sampled[c(sample_indices)]*10^-3)
    
    
  }
  n.CH4.sampled <- mean.n.CH4.hs*(daysum.V.sampled/V.leftover.day_n)

  n.CH4.sampled.cumsum <- cumsum(n.CH4.sampled)

  
  
  for (i in 1:nrow(data.oxi)) {
    
    j <- match(data.oxi$Date.ID[i], sample_day_ids)-1
    
    if (j==0) {
      data.oxi$n.CH4.total.corr[i] <- data.oxi$n.CH4.total[i]

    } else {
      
      data.oxi$n.CH4.total.corr[i] <- data.oxi$n.CH4.total[i] + n.CH4.sampled.cumsum[j]
    }
  }
  
## calculate y-axis for high and low epsilon to calculate oxidation rate from isotopic ratios
#isotopic fractionation factors alpha(product/substrate)=1+10^-3*epsilon(product/substrate)
epsilon.PS.low=7 # fractionation factor epsilon (product/substrate) lower end
epsilon.PS.high=25 # fractionation factor epsilon (product/substrate) higher end
alpha.PS.low=1.007 # fractionation factor alpha (product/substrate) lower end
alpha.PS.high=1.025 # fractionation factor alpha (product/substrate) higher end

source("../../../R_lib/yaxis.R")
  all.yaxis <- yaxis(
    data.oxi$calibrated.delta,
    alpha.PS.low,
    alpha.PS.high
    )
  #print(all.yaxisHP)
  data.oxi <- cbind(data.oxi, all.yaxis)


# calculate oxidation rate by concentration 
# calculate log of 
# data.oxi$n.CH4.total.log <-  log(data.oxi$n.CH4.total)
  data.oxi$n.CH4.total.corr.log <-  log(data.oxi$n.CH4.total.corr)

  
#subset data if some days should not be used for fits
data.oxi.sel <- subset(data.oxi, time.days > 7)

reg.oxi.n.CH4.log <- lm(data.oxi$n.CH4.total.corr.log ~data.oxi$time.days)
reg.oxi.n.CH4.log.sel <- lm(data.oxi.sel$n.CH4.total.corr.log ~data.oxi.sel$time.days)

lm.ppm <- summary(reg.oxi.n.CH4.log) # details on linear regression
lm.ppm.sel <- summary(reg.oxi.n.CH4.log.sel) # details on linear regression

# linear regression for k from isotope ratio 

reg.oxi.delta.low <- lm(data.oxi$yaxis.low ~data.oxi$time.days)
lm.delta.low <- summary(reg.oxi.delta.low) # details on linear regression
reg.oxi.delta.high <- lm(data.oxi$yaxis.high ~data.oxi$time.days)
lm.delta.high <- summary(reg.oxi.delta.high) # details on linear regression
reg.oxi.delta.sel.low <- lm(data.oxi.sel$yaxis.low ~data.oxi.sel$time.days)
lm.delta.low.sel <- summary(reg.oxi.delta.sel.low) # details on linear regression
reg.oxi.delta.sel.high <- lm(data.oxi.sel$yaxis.high ~data.oxi.sel$time.days)
lm.delta.high.sel <- summary(reg.oxi.delta.sel.high) # details on linear regression


require(broom)    

  print(paste(file_name))
  print(lm.ppm)
  print(lm.ppm.sel)
  print(lm.delta.low)
  print(lm.delta.high)
  print(lm.delta.low.sel)
  print(lm.delta.high.sel)
  
  n.CH4.log <- tidy(reg.oxi.n.CH4.log)
  n.CH4.log.sel <- tidy(reg.oxi.n.CH4.log.sel)
  delta.low <- tidy(reg.oxi.delta.low)
  delta.high <- tidy(reg.oxi.delta.high)
  delta.low.sel <- tidy(reg.oxi.delta.sel.low)
  delta.high.sel <- tidy(reg.oxi.delta.sel.high)
  
  
  all.fits <- rbind(n.CH4.log, 
                    n.CH4.log.sel, 
                    delta.low, 
                    delta.high, 
                    delta.low.sel, 
                    delta.high.sel)

  
  all.fits$info <- c("lm.ppm", "lm.ppm", "lm.ppm.sel", "lm.ppm.sel", "lm.delta.low", "lm.delta.low", "lm.delta.high", "lm.delta.high", "lm.delta.low.sel", "lm.delta.low.sel", "delta.high.sel", "delta.high.sel")

  all.fits$sample.ID <- data.oxi$sample.ID[1]
  
  all.fits$N <- ifelse(grepl("sel", all.fits$info), length(data.oxi.sel$sample.ID), length(data.oxi$sample.ID))
  
  write.table(all.fits, file=paste(paste(sub(".txt" , "" ,file_name)), ".fits.txt", sep =""))
  
  
  
#calclate mean and sd of c.CH4.water  
  
  # library(plyr)
  # info.cH4.water <- ddply(data.oxi, c("Date.ID"), summarise,
  #                         N    = length(c.CH4.water),
  #                         mean = mean(c.CH4.water),
  #                         sd   = sd(c.CH4.water),
  #                         se   = sd / sqrt(N),
  #                         sdpercent = sd/mean*100,
  #                         sepercent = se/mean*100,
  #                         mean.time = mean(time.days),
  #                         sample.ID = mean(sample.ID)
  # )
  # info.cH4.water
  # 
  # write.table(info.cH4.water, file=paste(paste(file_name), ".info.water.txt", sep =""), append=TRUE)  

#write file into table with respective data.ID in file name

write.table(data.oxi, file=paste(paste(sub(".txt" , "" ,file_name)), ".final.txt", sep =""))


}

