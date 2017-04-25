#------------------
# author: Christiane Uhlig, Graduate School of Oceanography, URI
# Version: 1.0 April 2017
# Email: cuhlig@uri.edu
#------------------

#------------------

# Uhlig, C., Loose, B., Using stable isotopes and gas concentrations for independent constraints on microbial methane oxidation at Arctic Ocean temperatures,
# Limnology & Oceanography Methods (under review)

#------------------

###sensitivity study to estimate detection limit of ISMOFI method


#empty workspace
rm(list=ls())

#set work directory to source file location
#R menue: Session -> Set Working Directory -> To Source File Location


library(plyr)

#parameters 
T <- 0  # °C
S <- 34.8 # salinity in PSU
V.hs <- 0.1 #L
V.w <- 0.75 #L

#calculate ideal gas constant (IGC)
source("../R_lib/IGC.R")
IGC <- idealgasconstant(T)

source("../R_lib/BunsenCoeff.R")
bunsen.coeff <- bunsen(T, S)
bunsen.coeff.L.L <- bunsen.coeff[1,1]
bunsen.coeff.mol.L <- bunsen.coeff[1,2]

#change of isotope ratio and concentration

delta.CH4.dt <- function (t, delta.CH4.ini, c.CH4.ini, k.ox.ppm, k.ox.delta) {
  
  n.CH4.total.ini <- (c.CH4.ini/(bunsen.coeff.mol.L*10^9))*(V.w*bunsen.coeff.mol.L+V.hs*IGC)*10^9 # in nmol
  

  delta.CH4.t <- (exp(-k.ox.delta*t*(1/alpha.S.P-1)))*(delta.CH4.ini+1000)-1000
  
  
  n.CH4.total.t <- exp(-k.ox.ppm*t)*n.CH4.total.ini
  
  list(c(t, delta.CH4.t, n.CH4.total.t))
  
  }

#constant concentration
#constant istotope ratio
#different k

delta.ini.1 = -35
c.CH4.ini.1 = 120
k.ox.ppm.1 = 0.1
k.ox.delta.1 = 0.1
k.ox.ppm.2 = 0.01
k.ox.delta.2 = 0.01
k.ox.ppm.3 = 0.001
k.ox.delta.3 = 0.001
k.ox.ppm.3a = 0.0001
k.ox.delta.3a = 0.0001


#different concentration
#constant istotope ratio
#constant k

delta.ini.4 = -35
c.CH4.ini.4 = 3000
c.CH4.ini.5 = 300
c.CH4.ini.6 = 30
c.CH4.ini.7 = 3
k.ox.ppm.4 = 0.01
k.ox.delta.4 = 0.01

#constant concentration
#different istotope ratio
#constant k

c.CH4.ini.8 = 120
delta.ini.8 = -23
delta.ini.9 = -35
delta.ini.10 = -70
k.ox.ppm.8 = 0.01
k.ox.delta.8 = 0.01


alpha.S.P <- 1.025

t <- as.data.frame(seq(0, 45, 1))


out1 <- as.data.frame(delta.CH4.dt(t, delta.ini.1, c.CH4.ini.1, k.ox.ppm.1, k.ox.delta.1))
out1 <- rename(out1, c("seq.0..45..1."="t", "seq.0..45..1..1"="delta", "seq.0..45..1..2"="n.CH4.delta"))

out2 <- as.data.frame(delta.CH4.dt(t, delta.ini.1, c.CH4.ini.1, k.ox.ppm.2, k.ox.delta.2))
out2 <- rename(out2, c("seq.0..45..1."="t", "seq.0..45..1..1"="delta", "seq.0..45..1..2"="n.CH4.delta"))

out3 <- as.data.frame(delta.CH4.dt(t, delta.ini.1, c.CH4.ini.1, k.ox.ppm.3, k.ox.delta.3))
out3 <- rename(out3, c("seq.0..45..1."="t", "seq.0..45..1..1"="delta", "seq.0..45..1..2"="n.CH4.delta"))

out3a <- as.data.frame(delta.CH4.dt(t, delta.ini.1, c.CH4.ini.1, k.ox.ppm.3a, k.ox.delta.3a))
out3a <- rename(out3a, c("seq.0..45..1."="t", "seq.0..45..1..1"="delta", "seq.0..45..1..2"="n.CH4.delta"))


#####not normalized

filename <- "varying_k_only"

pdf(file = paste(
  filename,
  #    file,
  ".pdf"),
  width = 6,
  height = 3
)

# png(file = paste(
#   filename,
#   #    file,
#   ".png"),
#   width = 6,
#   height = 3,
#   units = "in",
#   res = 300
# )

par(
  mfrow = c(1, 2),
  mgp = c(2.2, 0.9, 0),
  mar = c(4, 5, 0, 0),
  oma = c(0, 0, 1, 1),
  cex = 1.0,
  lwd = 2.5
)


plot(
  out1[,1], out1[,2], 
  type="l", 
  lty = 1, 
  #lwd = 2, 
  col = "black",
  #main=paste(filename), 
  xlim = c(),
  ylim = c(-35.5, -30), 
  xlab="time (days)" ,
  ylab=expression(paste(delta^{13}, CH[4], " (\u2030)"))
)
lines(out2[,1], out2[,2], lty=2, col = "red")
lines(out3[,1], out3[,2], lty=6, col = "blue", lwd =3)
lines(out3a[,1], out3a[,2], lty=4, col = "darkgreen")

abline(h = delta.ini.1 - (-0.45*3), col="magenta", lty=5) # sensitivity set as 3*absolute mean standard deviation of replicates
abline(h = delta.ini.1-(0.0073*3*delta.ini.1), col="darkgrey", lty=5) # sensitivity set as 3*relative mean standard deviation of replicates

legend(20, -31, 
       c(k.ox.ppm.1, k.ox.ppm.2, k.ox.ppm.3, "0.0001" ), 
       title = (expression(paste(k["ox"]," ",(day^-1)))), 
       col=c("black", "red", "blue", "darkgreen"), 
       lty = c(1, 2, 5, 4), 
       bty="n", 
       cex=0.8)

text(0, 10, "a", font = 2)


plot(
  out1[,1], out1[,3], 
  type="l", 
  lty = 1, 
  col = "black",
  #main="k.ox = 0.01", 
  xlim = c(),
  ylim = c(300, 360), 
  xlab="time (days)", 
  ylab=expression(paste(n(CH["4"])["total"]," (nmol)"))
)
lines(out2[,1], out2[,3], lty=2, col = "red")
lines(out3[,1], out3[,3], lty=6, col = "blue", lwd = 3)
lines(out3a[,1], out3a[,3], lty=4, col = "darkgreen")

abline(h = out1[1,3]-(3.59*3), col="magenta", lty=5) # sensitivity set as 3*absolute mean standard deviation of replicates
abline(h = out1[1,3]-(0.0202*3*out1[1,3]), col="darkgrey", lty=5) # sensitivity set as 3*relative mean standard deviation of replicates


text(44, -20, "b", font = 2)


dev.off()

