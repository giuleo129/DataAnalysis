#"""""streams"""""""
summary(streams)
sd(streams)
skewness(streams)
kurtosis(streams)

hist(streams, breaks = 20, col="lightblue",freq = FALSE, main = "Histogram of streams")
box()
lines(density(streams), col="red")
boxplot(streams, main = "streams")
hist(scale(streams))

#FITTING MODEL
fit.GA <- histDist(streams, family=GA, nbins=20, main="Gamma")
fit.EXP <- histDist(streams, family=EXP, nbins = 20, main="Exponential")
fit.IG <- histDist(streams, family=IG, nbins = 20, main="Inverse Gaussian distribution") 
fit.LN <- histDist(streams, family=LOGNO, nbins = 20, main="Log-Normal distribution") 
fit.WEI <- histDist(streams, family=WEI, nbins = 20, main="Weibull distribution")

data.frame(row.names = c("Gamma","Exponential", "Inverse Gaussian", "Log-Normal", "Weibull"), AIC=c(AIC(fit.GA), AIC(fit.EXP), AIC(fit.IG), AIC(fit.LN), AIC(fit.WEI)), SBC=c(fit.GA$sbc, fit.EXP$sbc, fit.IG$sbc, fit.LN$sbc, fit.WEI$sbc))
LR.test(fit.GA,fit.WEI)


#""""""""""""""""bpm"""""""""""""""""""""""
summary(bpm)
sd(bpm)
skewness(bpm)
kurtosis(bpm)

hist(bpm, breaks = 30, col="lightblue",freq = FALSE, main = "Histogram of BPM per song")
box()
lines(density(bpm), col="red")

#FITTING MODEL
fit.GA <- histDist(bpm, family=GA, nbins=30, main="Gamma")
fit.EXP <- histDist(bpm, family=EXP, nbins = 30, main="Exponential")
fit.IG <- histDist(bpm, family=IG, nbins = 30, main="Inverse Gaussian distribution") 
fit.LN <- histDist(bpm, family=LOGNO, nbins = 30, main="Log-Normal distribution") 
fit.WEI <- histDist(bpm, family=WEI, nbins = 30, main="Weibull distribution")

data.frame(row.names = c("Gamma","Exponential", "Inverse Gaussian", "Log-Normal", "Weibull"), AIC=c(AIC(fit.GA), AIC(fit.EXP), AIC(fit.IG), AIC(fit.LN), AIC(fit.WEI)), SBC=c(fit.GA$sbc, fit.EXP$sbc, fit.IG$sbc, fit.LN$sbc, fit.WEI$sbc))
LR.test(fit.WEI,fit.IG)

#MIXTURE MODEL (LOGNO WITH K=2)
set.seed(123)
fit.IG.2 <- gamlssMX(formula = bpm~1, family = GA, K = 2, data = NULL)

# estimate of mu and sigma in group 1
mu.hat1 <- exp(fit.IG.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.IG.2[["models"]][[1]][["sigma.coefficients"]])

# estimate of mu and sigma in group 2
mu.hat2 <- exp(fit.IG.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.IG.2[["models"]][[2]][["sigma.coefficients"]])

hist(bpm, breaks = 20,freq = FALSE)
lines(seq(min(bpm),max(bpm),length=length(bpm)),fit.IG.2[["prob"]][1]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(bpm),max(bpm),length=length(bpm)),fit.IG.2[["prob"]][2]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(bpm),max(bpm),length=length(bpm)),
      fit.IG.2[["prob"]][1]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.IG.2[["prob"]][2]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

#MIXTURE MODEL (IG WITH K=3)
set.seed(123)
fit.IG.3 <- gamlssMX(formula = bpm~1, family = GA, K = 3, data = NULL)

# estimate of mu and sigma in group 1
mu.hat1 <- exp(fit.IG.3[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.IG.3[["models"]][[1]][["sigma.coefficients"]])

# estimate of mu and sigma in group 2
mu.hat2 <- exp(fit.IG.3[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.IG.3[["models"]][[2]][["sigma.coefficients"]])

# estimate of mu and sigma in group 3
mu.hat3 <- exp(fit.IG.3[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(fit.IG.3[["models"]][[3]][["sigma.coefficients"]])
hist(bpm, breaks = 20,freq = FALSE)

lines(seq(min(bpm),max(bpm),length=length(bpm)),fit.IG.2[["prob"]][1]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(bpm),max(bpm),length=length(bpm)),fit.IG.2[["prob"]][2]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(bpm),max(bpm),length=length(bpm)),fit.IG.2[["prob"]][2]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col=4)
lines(seq(min(bpm),max(bpm),length=length(bpm)),
      fit.IG.3[["prob"]][1]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.IG.3[["prob"]][2]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat2, sigma = sigma.hat2) + 
        fit.IG.3[["prob"]][3]*dGA(seq(min(bpm),max(bpm),length=length(bpm)), mu = mu.hat3, sigma = sigma.hat3),
      lty = 1, lwd = 3, col = 1)

data.frame(row.names=c('K=2','K=3'),AIC=c(fit.IG.2$aic,fit.IG.3$aic),SBC=c(fit.IG.2$sbc,fit.IG.3$sbc))
