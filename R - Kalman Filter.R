
require(astsa)
library(MASS)
setwd("C:\\Users\\SONY\\Documents\\My Dropbox\\DIEBOLD\\PS3")
rm(list=ls())
set.seed(90)

gdpE = read.csv(paste("data.csv", sep="") , header=FALSE)[,1]*4
gdpI = read.csv(paste("data.csv", sep="") , header=FALSE)[,2]*4
gdpboth = read.csv(paste("data.csv", sep="") , header=FALSE)*4
gdpplus = read.csv(paste("gdp_2.csv", sep="") , header=FALSE)*4

periods=length(gdpE)

# INITIAL VALUES FOR THE PARAMETERS
mu=3; rho=0.6; sigmag=5; sigmae=4; sigmai=3; sigmaei=3/2
T=rho; R=1; Z=rbind(1,1); Q=sigmag; H=cbind(rbind(sigmae, sigmaei),rbind(sigmaei,sigmai))

# I CREATE MATRICES FOR STORING THE PARAMETERS IN EACH ITERATION OF MY MULTIMOVE GIBBS SAMPLING
Thistory=T
Qhistory=Q
muhistory=mu
Hhistory=list()
Hhistory[[1]]=H

# VALUES FOR THE PRIORS
meanpar=rbind(1.2,0.6)
varpar=diag(c(100,100),2,2)

psi=1/3
m=15
psimea=matrix(c(4/15,3/30,3/30,3/15),2,2)
mmea=15

for (round in 1:1000){
    # Matrix Ricatti equation calculation
    error=10
    P0=0
    while (error>10^(-2)){
      P1=T%*%P0%*%t(T)-T%*%P0%*%t(Z)%*%solve(Z%*%P0%*%t(Z)+H)%*%Z%*%P0%*%t(T)+R%*%Q%*%t(R)
      error=P1-P0
      P0=P1
    }
    
    g=mu; gpred=0; Ppred=0; F=list(matrix(0,2,2)); K=list(0); v=list(matrix(0,2,1))
    
    for (i in 1:periods){
      
      # PREDICTION RECURSIONS
      gpred[i+1]=mu*(1-T)+T*g[i]
      Ppred[i+1]=T%*%P0[i]%*%t(T)+R%*%Q%*%t(R)
      
      # UPDATING RECURSIONS
      F[[i]]=(Z%*%Ppred[i+1]%*%t(Z))+H
      K[[i]]=Ppred[i+1]%*%t(Z)%*%solve(F[[i]])
      
      g[i+1]=gpred[i+1]+(K[[i]]%*%(t(gdpboth[i,])-Z%*%gpred[i+1]))
      P0[i+1]=Ppred[i+1]-Ppred[i+1]%*%t(Z)%*%solve(F[[i]])%*%Z%*%Ppred[i+1]
    }
    
    a=g[2:209]
    P=P0[2:209]
    
    # I START THE MULTIMOVE GIBBS SAMPLING HERE
    # 1. DRAWS FOR THE LAST ALPHA_t
    # I NAME THE VECTOR OF ALPHA_j DRAWS "alpha"
    
    alpha=matrix(0,208,1)
    varalpha=matrix(0,208,1)
    meanalpha=matrix(0,208,1)
    
    meanalpha[208]=a[208]
    varalpha[208]=P[208]
    
    alpha[208]=rnorm(1,meanalpha[208],sqrt(varalpha[208]))
    
    for (k in 207:1){
      meanalpha[k]=a[k]+P[k]%*%t(T)%*%solve(T%*%P[k]%*%t(T)+Q)%*%(alpha[k+1]-T%*%a[k])
      varalpha[k]=P[k]-P[k]%*%t(T)%*%solve(T%*%P[k]%*%t(T)+Q)%*%T%*%P[k]
      alpha[k]=rnorm(1,meanalpha[k],sqrt(varalpha[k]))
    }
    
    # PARAMETERS OF TRANSITION EQUATION
    lagalpha=matrix(cbind(1,c(0,alpha[1:207])),208,2)
    
    Bhat=(solve(t(lagalpha)%*%lagalpha))%*%t(lagalpha)%*%alpha
    
    postvar=solve((kronecker(solve(Q),(t(lagalpha)%*%lagalpha),FUN="*")+solve(varpar)))
    postmean=postvar%*%(((kronecker(solve(Q),t(lagalpha)%*%lagalpha,FUN="*"))%*%Bhat)+(solve(varpar)%*%meanpar))
    
    T2=mvrnorm(1,postmean,postvar)
    T=T2[2]
    mu=T2[1]/(1-T)
    
    # VARIANCE OF STATE EQUATION    
    psipost=t(alpha-(lagalpha%*%T2))%*%(alpha-(lagalpha%*%T2))+solve(psi)
    mpost=208+m
    
    Q2=rWishart(1,mpost, solve(psipost))
    Q=solve(Q2)
    
    # VARIANCE OF MEASUREMENT EQUATION    
    gdpboth2 = cbind(gdpE,gdpI)
    
    psipostmea=t(gdpboth2-alpha%*%t(Z))%*%(gdpboth2-alpha%*%t(Z))+solve(psimea)
    mpostmea=208+mmea
    
    H2=rWishart(1,mpostmea, solve(psipostmea))
    H=solve(matrix(H2,2,2))
    
    Thistory=cbind(Thistory,T)
    Qhistory=cbind(Qhistory,Q)
    muhistory=cbind(muhistory,mu)
    Hhistory[[round+1]]=H
}
 
length=length(Thistory)

for (i in 1:length){
  sigmae[i]=Hhistory[[i]][1]
  sigmai[i]=Hhistory[[i]][4]
  sigmaei[i]=Hhistory[[i]][2]
}
  
start=100

meanrho=round(mean(Thistory[start:length]),digits=3)
meanmu=round(mean(muhistory[start:length]),digits=3)
meang=round(mean(Qhistory[start:length]),digits=3)
meane=round(mean(sigmae[start:length]),digits=3)
meani=round(mean(sigmai[start:length]),digits=3)
meanei=round(mean(sigmaei[start:length]),digits=3)
start=1
jpeg(paste("parameters.jpg",sep=""),width = 1000, height = 500)
old.par <- par(mfrow=c(2, 2))
plot(Thistory[start:length], main=c("Rho",meanrho))
plot(muhistory[start:length], main=c("Mu",meanmu))
plot(Qhistory[start:length], main=c("Sigma_g",meang))
par(old.par)
dev.off()

jpeg(paste("parameters2.jpg",sep=""),width = 1000, height = 500)
old.par <- par(mfrow=c(2, 2))
plot(sigmae[start:length], main=c("Sigma_e",meane))
plot(sigmai[start:length], main=c("Sigma_i",meani))
plot(sigmaei[start:length], main=c("Sigma_ei",meanei))
par(old.par)
dev.off()


# Now, with the maximizer values, I estimate kalman filter and smoother
rho=meanrho; mu0=meanmu; sigmag=meang; 
sigmae=meane; sigmai=meani; sigmaei=meanei

T=rho; R=1; Z=rbind(1,1); Q=sigmag; H=cbind(rbind(sigmae, sigmaei),rbind(sigmaei,sigmai))


# Matrix Ricatti equation calculation
error=10
P0=0
while (error>10^(-4)){
  P1=T%*%P0%*%t(T)-T%*%P0%*%t(Z)%*%ginv(Z%*%P0%*%t(Z)+H)%*%Z%*%P0%*%t(T)+R%*%Q%*%t(R)
  error=P1-P0
  P0=P1
}

# KALMAN FILTER
g=mu0; gpred=0; Ppred=0; F=list(matrix(0,2,2)); K=list(0); v=list(matrix(0,2,1))
for (i in 1:periods){
  
  # PREDICTION RECURSIONS
  gpred[i+1]=mu0*(1-T)+T*g[i]
  Ppred[i+1]=T%*%P0[i]%*%t(T)+R%*%Q%*%t(R)
  
  # UPDATING RECURSIONS
  F[[i]]=(Z%*%Ppred[i+1]%*%t(Z))+H
  K[[i]]=Ppred[i+1]%*%t(Z)%*%ginv(F[[i]])
  
  g[i+1]=gpred[i+1]+(K[[i]]%*%(t(gdpboth[i,])-Z%*%gpred[i+1]))
  P0[i+1]=Ppred[i+1]-Ppred[i+1]%*%t(Z)%*%ginv(F[[i]])%*%Z%*%Ppred[i+1]
  v[[i+1]]=t(gdpboth[i,])-Z%*%gpred[i+1]
}

g=g[2:209]
P0=P0[2:209]

# KALMAN SMOOTHER
a=matrix(0,1,periods); P2=matrix(0,1,periods); J=matrix(0,1,periods)
a[periods]=g[periods]; P2[periods]=P0[periods]; 

for (i in (periods-1):1){
  # PREDICTION RECURSIONS
  J[i]=P0[i]%*%t(T)%*%solve(Ppred[i+1])
  a[i]=g[i]+J[i]%*%(a[i+1]-gpred[i+1])
  P2[i]=P0[i]+J[i]%*%(P2[i+1]-Ppred[i+1])%*%t(J[i])
}

write.csv(cbind(t(a),g), file = "Results.csv")

a_past = read.csv(paste("C://Users//SONY//Documents//Doctorado//Econometria//Diebold//PS2//Results.csv", sep="") , header=TRUE)[,2]
g_past = read.csv(paste("C://Users//SONY//Documents//Doctorado//Econometria//Diebold//PS2//Results.csv", sep="") , header=TRUE)[,3]


# WITH ZOOM ON THE LAST YEARS, TO SEE THE DIFFERENCE/SIMILARITY

jpeg(paste("JUST FILTERED.jpg",sep=""),width = 1000, height = 350)
plot(ts(g_past[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="FILTERED: PS3 (dashed) VS PS2 (solid)", xlab="Year", ylab="Logarithmic growth")
lines(ts(g[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()

jpeg(paste("JUST SMOOTHED.jpg",sep=""),width = 1000, height = 350)
plot(ts(a_past[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="SMOOTHED: PS3 (dashed) VS PS2 (solid)", xlab="Year", ylab="Logarithmic growth")
lines(ts(a[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()

jpeg(paste("filteredvsgdp.jpg",sep=""),width = 1000, height = 350)
plot(ts(g[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="FILTERED (solid) VS GDP-E AND GDP-I SERIES", xlab="Year", ylab="Logarithmic growth")
lines(ts(gdpE[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
lines(ts(gdpI[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()

jpeg(paste("smoothedvsgdp.jpg",sep=""),width = 1000, height = 350)
plot(ts(a[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="SMOOTHED (solid) VS GDP-E AND GDP-I SERIES", xlab="Year", ylab="Logarithmic growth")
lines(ts(gdpE[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
lines(ts(gdpI[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()

jpeg(paste("filteredvsplus.jpg",sep=""),width = 1000, height = 350)
plot(ts(g[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="FILTERED (solid) VS GDP-PLUS SERIES", xlab="Year", ylab="Logarithmic growth")
lines(ts(gdpplus[161:periods,1], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()

jpeg(paste("smoothedvsplus.jpg",sep=""),width = 1000, height = 350)
plot(ts(a[161:periods], frequency=4, start=cbind(2000,1), deltat=1/4), main="SMOOTHED (solid) VS GDP-PLUS SERIES", xlab="Year", ylab="Logarithmic growth")
lines(ts(gdpplus[161:periods,1], frequency=4, start=cbind(2000,1), deltat=1/4),  lty=2)
dev.off()
