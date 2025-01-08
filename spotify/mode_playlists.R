#""""""""""""""""mode"""""""""""""""""""""""""
table(mode)
freqMode <- table(mode)/length(mode)*100
freqMode
pie(freqMode, col = c("orange","black"))



#""""""""""""""""in_spotify_playlists"""""""""""""""""""""""
summary(in_spotify_playlists)
sd(in_spotify_playlists)
skewness(in_spotify_playlists)
kurtosis(in_spotify_playlists)

hist(in_spotify_playlists, breaks = 30, col="lightblue",freq = FALSE, main = "Histogram of presence in playlists")
box()
lines(density(in_spotify_playlists), col="red")

#FITTING MODEL
fit.GA <- histDist(in_spotify_playlists, family=GA, nbins=30, main="Gamma")
fit.EXP <- histDist(in_spotify_playlists, family=EXP, nbins = 30, main="Exponential")
fit.IG <- histDist(in_spotify_playlists, family=IG, nbins = 30, main="Inverse Gaussian distribution") 
fit.LN <- histDist(in_spotify_playlists, family=LOGNO, nbins = 30, main="Log-Normal distribution") 
fit.WEI <- histDist(in_spotify_playlists, family=WEI, nbins = 30, main="Weibull distribution")

data.frame(row.names = c("Gamma","Exponential", "Inverse Gaussian", "Log-Normal", "Weibull"), AIC=c(AIC(fit.LOGNO), AIC(fit.EXP), AIC(fit.IG), AIC(fit.LN), AIC(fit.WEI)), SBC=c(fit.LOGNO$sbc, fit.EXP$sbc, fit.IG$sbc, fit.LN$sbc, fit.WEI$sbc))
LR.test(fit.WEI,fit.LN)

#MIXTURE MODEL (LOGNO WITH K=2)
fit.LOGNO.2 <- gamlssMX(formula = in_spotify_playlists~1, family = LOGNO, K = 2, data = NULL)

# estimate of mu and sigma in group 1
mu.hat1 <- exp(fit.LOGNO.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.LOGNO.2[["models"]][[1]][["sigma.coefficients"]])

# estimate of mu and sigma in group 2
mu.hat2 <- exp(fit.LOGNO.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.LOGNO.2[["models"]][[2]][["sigma.coefficients"]])

hist(in_spotify_playlists, breaks = 50,freq = FALSE)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),fit.LOGNO.2[["prob"]][1]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),fit.LOGNO.2[["prob"]][2]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),
      fit.LOGNO.2[["prob"]][1]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.LOGNO.2[["prob"]][2]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

#MIXTURE MODEL (LOGNO WITH K=3)
set.seed(123)
fit.LOGNO.3 <- gamlssMX(formula = in_spotify_playlists~1, family = LOGNO, K = 3, data = NULL)

# estimate of mu and sigma in group 1
mu.hat1 <- exp(fit.LOGNO.3[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.LOGNO.3[["models"]][[1]][["sigma.coefficients"]])

# estimate of mu and sigma in group 2
mu.hat2 <- exp(fit.LOGNO.3[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.LOGNO.3[["models"]][[2]][["sigma.coefficients"]])

# estimate of mu and sigma in group 3
mu.hat3 <- exp(fit.LOGNO.3[["models"]][[3]][["mu.coefficients"]])    
sigma.hat3 <- exp(fit.LOGNO.3[["models"]][[3]][["sigma.coefficients"]])
hist(in_spotify_playlists, breaks = 50,freq = FALSE)

lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),fit.LOGNO.2[["prob"]][1]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),fit.LOGNO.2[["prob"]][2]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),fit.LOGNO.2[["prob"]][2]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat3, sigma = sigma.hat3),lty=2,lwd=3,col=4)
lines(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)),
      fit.LOGNO.3[["prob"]][1]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.LOGNO.3[["prob"]][2]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat2, sigma = sigma.hat2) + 
          fit.LOGNO.3[["prob"]][3]*dGA(seq(min(in_spotify_playlists),max(in_spotify_playlists),length=length(in_spotify_playlists)), mu = mu.hat3, sigma = sigma.hat3),
      lty = 1, lwd = 3, col = 1)

data.frame(row.names=c('K=2','K=3'),AIC=c(fit.LOGNO.2$aic,fit.LOGNO.3$aic),SBC=c(fit.LOGNO.2$sbc,fit.LOGNO.3$sbc))



