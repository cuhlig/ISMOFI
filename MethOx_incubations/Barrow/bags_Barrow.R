#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------


### corrections for removed mass of CH4 during injections 
### add ancillary information
### subsetting to separate bag files

#empty workspace
rm(list=ls())

#set work directory to source file location
#R menue: Session -> Set Working Directory -> To Source File Location

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

# #load addNewData
# library(devtools, quietly=TRUE)
# source_gist("https://gist.github.com/dfalster/5589956")

source("../../../R_lib/5589956-master/addNewData.R")


#read file with sample data
data.samples <- read.table("../Barrow_20161116.data.samples.txt", header=TRUE, sep="\t", comment.char="", stringsAsFactors=FALSE)
data.samples <- data.samples[order(data.samples$Timestamp.cor),]

data.samples <- data.samples[,c(1:5,7,9,11,13,15,17,32:42, 45:50)] # select only columns with useful information for better readability

### add column to merge with calibration table
data.samples$cali.date <- data.samples$Date
data.samples$cali.date[c(48:81)] <- "04/13.14/16"
data.samples$cali.date[c(82:148)] <- "04/14.15.16/16"


#add columns with weight of water in bags, depth of sampling in m, sampling date and treatment
lookup1 <- read.table("../../Lookup.csv", header=TRUE, sep=",") # be carefull to have "," as separation in the lookup table, otherwise the function does not recognize the names in the lookup table

allowedVars <- c("m.water","treatment","sample.date","sample.depth")
data.samples <- addNewData("../../Lookup.csv", data.samples, allowedVars) #the lookup table can contain more variables, e.g. other parameters can also be added this way

data.samples$m.water <- as.numeric(data.samples$m.water) #set column as numeric
data.samples$sample.depth <- as.numeric(data.samples$sample.depth) 

#add equilibration temperature for experimental bags,salinity, initial headspace volume in L 
##### SALINITY should be added here when finally available
##equilibration temperature: oxidation experiment bags in Barrow 0degC (all April), oxidation experiment bags in Rhode Island +1degC (all May)
lookup2 <- read.table("../../Lookup_2_Barrow.csv", header=TRUE, sep=",") 

allowedVars <- c("T.equil","V.hs.initial","sal")
data.samples <- addNewData("../../Lookup_2_Barrow.csv", data.samples, allowedVars) 

data.samples$T.equil <- as.numeric(data.samples$T.equil) #set column as numeric
data.samples$V.hs.initial <- as.numeric(data.samples$V.hs.initial) 
data.samples$sal <- as.numeric(data.samples$sal) 


####SUBSET to oxidation bags and in situ measurements and write into file #####
#subset to in situ 
data.insitu <- subset(data.samples, treatment=="in situ")

#add column with headspace removed before measurement to match oxidaton bag columns 0mL
data.insitu$hs.removed <- 0
write.table(data.insitu, 
            file=paste("Barrow.insitu.txt", sep = "")
            , sep = "\t")

data.oxi <- subset(data.samples, treatment!="in situ") 
write.table(data.oxi, 
            file=paste("Barrow.oxi.txt", sep = "")
            , sep = "\t")


##subset to seperate bags for further processing
##add column with headspace taken out of bag for measurement up to this date - hs.removed in mL!!
#value is the same for one duplicate measurements at one sample date, because bags were not equilibrated before 2nd measurement 
#add column with additionally removed volume, these samples were removed from the bag but data had to be discarded because it was faulty V.discarded (in mL)



#0.1x
data.oxi.7 <- subset(data.samples, sample.ID ==7)
data.oxi.7$hs.removed <- c(0,0,20,20,20,50,50,70,70,70,70)
data.oxi.7$V.discarded <- 0
write.table(data.oxi.7, file=paste("Barrow.oxi.7.txt", sep = ""), sep = "\t")

data.oxi.10 <- subset(data.samples, sample.ID ==10)
data.oxi.10$hs.removed <- c(0,0,20,20,40,40,60,60)
data.oxi.10$V.discarded <- 0
write.table(data.oxi.10, file=paste("Barrow.oxi.10.txt", sep = ""), sep = "\t")

data.oxi.13 <- subset(data.samples, sample.ID ==13)
data.oxi.13$hs.removed <- c(0,0,20,20,40,40,60,60)
data.oxi.13$V.discarded <- 0
write.table(data.oxi.13, file=paste("Barrow.oxi.13.txt", sep = ""), sep = "\t")

data.oxi.16 <- subset(data.samples, sample.ID ==16)
data.oxi.16$hs.removed <- c(0,10,10,10,40,40,60,60)
data.oxi.16$V.discarded <- 0
write.table(data.oxi.16, file=paste("Barrow.oxi.16.txt", sep = ""), sep = "\t")

data.oxi.19 <- subset(data.samples, sample.ID ==19)
data.oxi.19$hs.removed <- c(0,0,20,20,40,40,60,60)
data.oxi.19$V.discarded <- 0
write.table(data.oxi.19, file=paste("Barrow.oxi.19.txt", sep = ""), sep = "\t")

data.oxi.22 <- subset(data.samples, sample.ID ==22)
data.oxi.22$hs.removed <- c(0,0,20,20,20,50,50,70,70)
data.oxi.22$V.discarded <- 0
write.table(data.oxi.22, file=paste("Barrow.oxi.22.txt", sep = ""), sep = "\t")

#50x
data.oxi.8 <- subset(data.samples, sample.ID ==8)
data.oxi.8$hs.removed <- c(0,0,10,10,10,17,17,23,23,29,29,29,42,42,48,48)
data.oxi.8$V.discarded <- c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
write.table(data.oxi.8, file=paste("Barrow.oxi.8.txt", sep = ""), sep = "\t")

data.oxi.11 <- subset(data.samples, sample.ID ==11)
data.oxi.11$hs.removed <- c(0,0,10,20,20,20,29,29,35,35,41,41,47,47)
data.oxi.11$V.discarded <- c(0,0,5,0,0,0,0,0,0,0,0,0,0,0)
write.table(data.oxi.11, file=paste("Barrow.oxi.11.txt", sep = ""), sep = "\t")

data.oxi.14 <- subset(data.samples, sample.ID ==14)
data.oxi.14$hs.removed <- c(0,0,10,10,10,19,19,28,28,34,34,40,40,46,46)
data.oxi.14$V.discarded <- c(0,0,0,0,0,3,0,0,0,0,0,0,0,0,0)
write.table(data.oxi.14, file=paste("Barrow.oxi.14.txt", sep = ""), sep = "\t")

data.oxi.17 <- subset(data.samples, sample.ID ==17)
data.oxi.17$hs.removed <- c(0,0,10,10,10,19,19,25,25,34,34,34,40,40,46,46)
data.oxi.17$V.discarded <- 0
write.table(data.oxi.17, file=paste("Barrow.oxi.17.txt", sep = ""), sep = "\t")

data.oxi.20 <- subset(data.samples, sample.ID ==20)
data.oxi.20$hs.removed <- c(0,0,10,10,16,16,22,22,28,28,34,34,40,40)
data.oxi.20$V.discarded <- 0
write.table(data.oxi.20, file=paste("Barrow.oxi.20.txt", sep = ""), sep = "\t")

data.oxi.23 <- subset(data.samples, sample.ID ==23)
data.oxi.23$hs.removed <- c(0,0,10,10,16,16,22,22,22)
data.oxi.23$V.discarded <- 0
write.table(data.oxi.23, file=paste("Barrow.oxi.23.txt", sep = ""), sep = "\t")

#1x
data.oxi.26 <- subset(data.samples, sample.ID ==26)
data.oxi.26$hs.removed <- c(0,0,10,10,20,20,30,30)
data.oxi.26$V.discarded <- 0
write.table(data.oxi.26, file=paste("Barrow.oxi.26.txt", sep = ""), sep = "\t")

data.oxi.29 <- subset(data.samples, sample.ID ==29)
data.oxi.29$hs.removed <- c(0,0,10,10,10,25,25,35,35)
data.oxi.29$V.discarded <- 0
write.table(data.oxi.29, file=paste("Barrow.oxi.29.txt", sep = ""), sep = "\t")

data.oxi.33 <- subset(data.samples, sample.ID ==33)
data.oxi.33$hs.removed <- c(0,0,10,10,20,20,30,30,30)
data.oxi.33$V.discarded <- 0
write.table(data.oxi.33, file=paste("Barrow.oxi.33.txt", sep = ""), sep = "\t")

data.oxi.37 <- subset(data.samples, sample.ID ==37)
data.oxi.37$hs.removed <- c(0,0,10,10,20,20,30,30)
data.oxi.37$V.discarded <- 0
write.table(data.oxi.37, file=paste("Barrow.oxi.37.txt", sep = ""), sep = "\t")

#500x Peri
data.oxi.27 <- subset(data.samples, sample.ID ==27)
data.oxi.27$hs.removed <- c(0,0,1.2,1.2,3.2,3.2,3.2,6.2,6.2)
data.oxi.27$V.discarded <- 0
write.table(data.oxi.27, file=paste("Barrow.oxi.27.txt", sep = ""), sep = "\t")

data.oxi.30 <- subset(data.samples, sample.ID ==30)
data.oxi.30$hs.removed <- c(0,0,0,1.8,1.8,3.8,3.8,5.8,5.8,7.8,7.8,9.8,9.8)
data.oxi.30$V.discarded <- 0
write.table(data.oxi.30, file=paste("Barrow.oxi.30.txt", sep = ""), sep = "\t")

data.oxi.34 <- subset(data.samples, sample.ID ==34)
data.oxi.34$hs.removed <- c(0,0,1.2,1.2,3.2,3.2,5.2,5.2)
data.oxi.34$V.discarded <- 0
write.table(data.oxi.34, file=paste("Barrow.oxi.34.txt", sep = ""), sep = "\t")

data.oxi.38 <- subset(data.samples, sample.ID ==38)
data.oxi.38$hs.removed <- c(0,0,1.2,1.2,3.2,3.2,5.2,5.2,7.2,7.2,9.2,9.2)
data.oxi.38$V.discarded <- 0
write.table(data.oxi.38, file=paste("Barrow.oxi.38.txt", sep = ""), sep = "\t")

data.oxi.44 <- subset(data.samples, sample.ID ==44)
data.oxi.44$hs.removed <- c(0,0,0,2.4,2.4,4.4,4.4,6.4,6.4)
data.oxi.44$V.discarded <- 0
write.table(data.oxi.44, file=paste("Barrow.oxi.44.txt", sep = ""), sep = "\t")

#500x Submers
data.oxi.25 <- subset(data.samples, sample.ID ==25)
data.oxi.25$hs.removed <- c(0,0,1.2,1.2,3.2,3.2,5.2,5.2)
data.oxi.25$V.discarded <- 0
write.table(data.oxi.25, file=paste("Barrow.oxi.25.txt", sep = ""), sep = "\t")

data.oxi.28 <- subset(data.samples, sample.ID ==28)
data.oxi.28$hs.removed <- c(0,0,1.8,1.8,3.8,3.8,5.8,5.8)
data.oxi.28$V.discarded <- c(0.6,0,0,0,0,0,0,0)
write.table(data.oxi.28, file=paste("Barrow.oxi.28.txt", sep = ""), sep = "\t")

data.oxi.32 <- subset(data.samples, sample.ID ==32)
data.oxi.32$hs.removed <- c(0,0,1.2,1.2,3.2,3.2,3.2,6.2,6.2)
data.oxi.32$V.discarded <- 0
write.table(data.oxi.32, file=paste("Barrow.oxi.32.txt", sep = ""), sep = "\t")

data.oxi.35 <- subset(data.samples, sample.ID ==35)
data.oxi.35$hs.removed <- c(0,0,0,1.8,1.8,3.8,3.8,5.8,5.8)
data.oxi.35$V.discarded <- 0
write.table(data.oxi.35, file=paste("Barrow.oxi.35.txt", sep = ""), sep = "\t")

data.oxi.36 <- subset(data.samples, sample.ID ==36)
data.oxi.36$hs.removed <- c(0,0,1.2,1.2,4.2,4.2)
data.oxi.36$V.discarded <- c(0,0,1,0,0,0)
write.table(data.oxi.36, file=paste("Barrow.oxi.36.txt", sep = ""), sep = "\t")
