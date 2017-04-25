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
data.samples <- read.table("../JulAug_20161116.data.samples.txt", header=TRUE, sep="\t", comment.char="", stringsAsFactors=FALSE)
data.samples <- data.samples[order(data.samples$Timestamp),]

data.samples <- data.samples[,c(1:5,7,9,11,13,15,17,32:41, 44:49)] # select only columns with useful information for better readability



#add columns with weight of water in bags, depth of sampling in m, sampling date and treatment
lookup1 <- read.table("../../Lookup.csv", header=TRUE, sep=",") # be carefull to have "," as separation in the lookup table, otherwise the function does not recognize the names in the lookup table

allowedVars <- c("m.water","treatment","sample.date","sample.depth")
data.samples <- addNewData("../../Lookup.csv", data.samples, allowedVars) #the lookup table can contain more variables, e.g. I can also add the salinities and temperatures this way

data.samples$m.water <- as.numeric(data.samples$m.water) #set column as numeric
data.samples$sample.depth <- as.numeric(data.samples$sample.depth) 

#initial heaspace volume
#Nbay + Bcubi 500x = 100mL
#Nbay + Bcubi 50x = 150mL
#Nbay + Bcubi 1x = 110mL
#Nbay in situ = 100mL

data.samples$V.hs.initial <- ifelse(grepl("50x", data.samples$treatment), "0.15", 
                                ifelse(grepl("1x", data.samples$treatment), "0.11", "0.10") )



####SUBSET to oxidation bags and in situ measurements and write into file #####
#subset to in situ 
data.insitu <- subset(data.samples, treatment=="Nbay in situ")
#add column with headspace removed before measurement to match oxidaton bag columns 0mL
data.insitu$hs.removed <- 0
#add colum with salinity
data.insitu$sal <- 33.1
#add.column with equilibration temperature
data.insitu$T.equil <- 18 #in situ concentration bags 18°C

write.table(data.insitu, file=paste("JulAug.insitu.txt", sep = ""), sep = "\t")


data.oxi <- subset(data.samples, treatment!="Nbay in situ") 
data.oxi$sal <- ifelse(grepl("Nbay", data.oxi$treatment), "33.1", "34.8")
data.oxi$T.equil <- ifelse(grepl("Nbay", data.oxi$treatment), "18", "0")

write.table(data.oxi, file=paste("JulAug.oxi.txt", sep = ""), sep = "\t")


##subset to seperate bags for further processing
##add column with headspace taken out of bag for measurement up to this date - hs.removed in mL!!
#value is the same for one duplicate measurements at one sample date, because bags were not equilibrated before 2nd measurement 
#add column with additionally removed volume, these samples were removed from the bag but data had to be discarded because it was faulty V.discarded (in mL)

#Narragansett Bay 1x incubations 50, 51, 52, 53
data.oxi.50 <- subset(data.oxi, sample.ID ==50)
data.oxi.50$hs.removed <- c(0,0,0,15,15,25,25,35,35,43,43,43,50,50,54,54)
write.table(data.oxi.50, file=paste("Nbay.oxi.50.txt", sep = ""), sep = "\t")

data.oxi.51 <- subset(data.oxi, sample.ID ==51)
data.oxi.51$hs.removed <- c(0,0,0,15,15,25,25,35,35,41,41,51,51,61,61)
write.table(data.oxi.51, file=paste("Nbay.oxi.51.txt", sep = ""), sep = "\t")

data.oxi.52 <- subset(data.oxi, sample.ID ==52)
data.oxi.52$hs.removed <- c(0,0,15,15,25,25,35,35,41,51,51,61,61)
write.table(data.oxi.52, file=paste("Nbay.oxi.52_BES.txt", sep = ""), sep = "\t")

data.oxi.53 <- subset(data.oxi, sample.ID ==53)
data.oxi.53$hs.removed <- c(0,0,10,10,20,20,26,26,36,36,46,46)
write.table(data.oxi.53, file=paste("Nbay.oxi.53_NaOH.txt", sep = ""), sep = "\t")


#Narragansett Bay 50x incubations 56 ,58, 59, 61
data.oxi.56 <- subset(data.oxi, sample.ID ==56)
data.oxi.56$hs.removed <- c(0,0,0,0,14,14,20,20,26,26,32,32,38,38)
write.table(data.oxi.56, file=paste("Nbay.oxi.56_NaOH.txt", sep = ""), sep = "\t")

data.oxi.58 <- subset(data.oxi, sample.ID ==58)
data.oxi.58$hs.removed <- c(0,0,0,9,9,9,20,20,26,26,32,32,38,38,44,44)
write.table(data.oxi.58, file=paste("Nbay.oxi.58.txt", sep = ""), sep = "\t")

data.oxi.59 <- subset(data.oxi, sample.ID ==59)
data.oxi.59$hs.removed <- c(0,0,6,6,12,12,18,18,24,24,30,30,36,36)
data.oxi.59$vol.mL <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3) # injection volumes in info file were wrong with 5ml in the 3 middle days
data.oxi.59$SSIM.dil.factor <- data.oxi.59$Pssim/((data.oxi.59$vol.mL/20)*data.oxi.59$Patm.torr) # has to be updated after change of injection volume 
write.table(data.oxi.59, file=paste("Nbay.oxi.59.txt", sep = ""), sep = "\t")

data.oxi.61 <- subset(data.oxi, sample.ID ==61)
data.oxi.61$hs.removed <- c(0,0,0,7,7,13,13,19,19,25,25,31,31,37,37)
data.oxi.61$vol.mL <- c(1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3) # injection volumes in info file were wrong with 5ml in the 3 middle days
data.oxi.61$SSIM.dil.factor <- data.oxi.61$Pssim/((data.oxi.61$vol.mL/20)*data.oxi.61$Patm.torr) # has to be updated after change of injection volume 
write.table(data.oxi.61, file=paste("Nbay.oxi.61_BES.txt", sep = ""), sep = "\t")

#Narragansett Bay 500x incubations 57,60
data.oxi.57 <- subset(data.oxi, sample.ID ==57)
data.oxi.57$hs.removed <- c(0,0,0,8.2,8.2,17.6,17.6,19.8,19.8,22,22,41.2,41.2)
write.table(data.oxi.57, file=paste("Nbay.oxi.57.txt", sep = ""), sep = "\t")

data.oxi.60 <- subset(data.oxi, sample.ID ==60)
data.oxi.60$hs.removed <- c(0,0,0,19.4,19.4,21.6,21.6,23.8,23.8,26,26,28.3,28.3)
write.table(data.oxi.60, file=paste("Nbay.oxi.60.txt", sep = ""), sep = "\t")


#Barrow cubitainer 1x 62,63,64,65
data.oxi.62 <- subset(data.oxi, sample.ID ==62)
data.oxi.62$hs.removed <- c(0,0,10,10,10,25,25,35,35,45,45,55,55,65,65)
write.table(data.oxi.62, file=paste("Bcubi.oxi.62.txt", sep = ""), sep = "\t")

data.oxi.63 <- subset(data.oxi, sample.ID ==63)
data.oxi.63$hs.removed <- c(0,0,0,15,15,25,25,35,35,45,45,55,55,65,65)
write.table(data.oxi.63, file=paste("Bcubi.oxi.63.txt", sep = ""), sep = "\t")

data.oxi.64 <- subset(data.oxi, sample.ID ==64)
data.oxi.64$hs.removed <- c(0,0,10,10,20,20,30,30,40,40,50,50,60,60)
write.table(data.oxi.64, file=paste("Bcubi.oxi.64_BES.txt", sep = ""), sep = "\t")

data.oxi.65 <- subset(data.oxi, sample.ID ==65)
data.oxi.65$hs.removed <- c(0,0,0,15,20,20,30,30,40,40,50,50,60,60)
write.table(data.oxi.65, file=paste("Bcubi.oxi.65_NaOH.txt", sep = ""), sep = "\t")


#Barrow cubitainer 50x 66,67,68,69
data.oxi.66 <- subset(data.oxi, sample.ID ==66)
data.oxi.66$hs.removed <- c(0,0,0,9,9,9,18,18,24,24,30,30,36,36,42,42)
write.table(data.oxi.66, file=paste("Bcubi.oxi.66.txt", sep = ""), sep = "\t")

data.oxi.67 <- subset(data.oxi, sample.ID ==67)
data.oxi.67$hs.removed <- c(0,0,6,6,12,12,18,18,24,24,30,30,36,36)
write.table(data.oxi.67, file=paste("Bcubi.oxi.67.txt", sep = ""), sep = "\t")

data.oxi.68 <- subset(data.oxi, sample.ID ==68)
data.oxi.68$hs.removed <- c(0,0,6,6,12,12,18,27,27,27,36,36,42,42)
write.table(data.oxi.68, file=paste("Bcubi.oxi.68_BES.txt", sep = ""), sep = "\t")

data.oxi.69 <- subset(data.oxi, sample.ID ==69)
data.oxi.69$hs.removed <- c(0,0,6,6,12,12,18,18,24,24,30,30)
write.table(data.oxi.69, file=paste("Bcubi.oxi.69_NaOH.txt", sep = ""), sep = "\t")


#Barrow cubitainer 500x 70,71,72,73
data.oxi.70 <- subset(data.oxi, sample.ID ==70)
data.oxi.70$hs.removed <- c(0,0,2.2,2.2,4.4,4.4,6.6,6.6,8.8,8.8,11,11,13.2,13.2)
write.table(data.oxi.70, file=paste("Bcubi.oxi.70.txt", sep = ""), sep = "\t")

data.oxi.71 <- subset(data.oxi, sample.ID ==71)
data.oxi.71$hs.removed <- c(0,0,0,3.3,3.3,5.5,5.5,7.7,7.7,9.9,9.9,12.1,12.1,14.3,14.3)
write.table(data.oxi.71, file=paste("Bcubi.oxi.71.txt", sep = ""), sep = "\t")

data.oxi.72 <- subset(data.oxi, sample.ID ==72)
data.oxi.72 <- data.oxi.72[-c(3),]
data.oxi.72$hs.removed <- c(0,0,5.2,5.2,7.4,7.4,9.6,9.6,11.8,11.8,15.1,15.1,18.4,18.4)
write.table(data.oxi.72, file=paste("Bcubi.oxi.72_BES.txt", sep = ""), sep = "\t")

data.oxi.73 <- subset(data.oxi, sample.ID ==73)
data.oxi.73$hs.removed <- c(0,0,0,3.3,3.3,3.3,6.6,6.6,8.8,8.8,11,11,13.2,13.2)
write.table(data.oxi.73, file=paste("Bcubi.oxi.73_NaOH.txt", sep = ""), sep = "\t")
