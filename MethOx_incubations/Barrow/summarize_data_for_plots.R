#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------

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

filelist <- c(
  "Barrow.oxi.23.final.txt"
)
  
  
file_names <- as.list(filelist)


for (file_name in file_names) {
  data.oxi <- read.table(paste(file_name))

  
  # calculate mean, sd, se, and sd_percent of parameters used for plot
  library(plyr)
  summary.data.oxi <- ddply(data.oxi, c("Date.ID"), summarise,
                       N.time.days    = length(time.days),
                       mean.time.days = mean(time.days),
                       sd.time.days   = sd(time.days),

                       mean.c.CH4.water = mean(c.CH4.water),
                       sd.c.CH4.water   = sd(c.CH4.water),

                       mean.n.CH4.total.corr.log = mean(n.CH4.total.corr.log),
                       sd.n.CH4.total.corr.log   = sd(n.CH4.total.corr.log),

                       mean.calibrated.delta = mean(calibrated.delta),
                       sd.calibrated.delta   = sd(calibrated.delta),

                       mean.yaxis.high.x = mean(yaxis.high.x),
                       sd.yaxis.high.x   = sd(yaxis.high.x),
                       
                       mean.yaxis.low.x = mean(yaxis.low.x),
                       sd.yaxis.low.x   = sd(yaxis.low.x)

  )
  summary.data.oxi
  
  summary.data.oxi$sample.ID <- data.oxi$sample.ID[1]
  

 write.table(summary.data.oxi, file=paste(paste(sub(".final.txt" , "" ,file_name)), ".summary.txt", sep = ""))
}
  

