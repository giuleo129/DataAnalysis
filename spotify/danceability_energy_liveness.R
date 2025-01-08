#"""""""danceability""""""""""""

round(summary(danceability),2)
sd(danceability)
skewness(danceability)
kurtosis(danceability)

hist(danceability, col="lightblue",freq = FALSE, main = "Histogram of danceability")
box()
lines(density(danceability), col="red")

#FITTING MODEL
fit.BE <- histDist(danceability, family=BE, nbins=10, main="Beta")
fit.BEo <- histDist(danceability, family=BEo, nbins = 10, main="Beta Original")
fit.LOGITNO <- histDist(danceability, family=LOGITNO, nbins = 10, main="Logit-Normal")
fit.GB1 <- histDist(danceability, family=GB1, nbins = 10, main="Generalized Beta")

data.frame(row.names = c("Beta","Beta Or", "Gen Beta", "logit Norm"), AIC=c(AIC(fit.BE), AIC(fit.BEo), AIC(fit.GB1), AIC(fit.LOGITNO)), SBC=c(fit.BE$sbc, fit.BEo$sbc, fit.GB1$sbc, fit.LOGITNO$sbc))


#MIXTURE MODEL (logitno WITH K=2)
set.seed(123)
fit.LOGITNO.2 <- gamlssMX(formula = danceability~1, family = LOGITNO, K = 2, data = NULL)

# estimate of mu and sigma in group 1
mu.hat1 <- exp(fit.LOGITNO.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.LOGITNO.2[["models"]][[1]][["sigma.coefficients"]])

# estimate of mu and sigma in group 2
mu.hat2 <- exp(fit.LOGITNO.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.LOGITNO.2[["models"]][[2]][["sigma.coefficients"]])

hist(danceability, breaks = 20,freq = FALSE)
lines(seq(min(danceability),max(danceability),length=length(danceability)),fit.LOGITNO.2[["prob"]][1]*dGA(seq(min(danceability),max(danceability),length=length(danceability)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(danceability),max(danceability),length=length(danceability)),fit.LOGITNO.2[["prob"]][2]*dGA(seq(min(danceability),max(danceability),length=length(danceability)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(danceability),max(danceability),length=length(danceability)),
      fit.LOGITNO.2[["prob"]][1]*dGA(seq(min(danceability),max(danceability),length=length(danceability)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.LOGITNO.2[["prob"]][2]*dGA(seq(min(danceability),max(danceability),length=length(danceability)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)
fit.LOGITNO.2$aic
fit.LOGITNO.2$sbc


#""""""""energy""""""""
round(summary(energy),2)
sd(energy)
skewness(energy)
kurtosis(energy)

hist(energy,breaks = 15, col="lightblue",freq = FALSE, main = "Histogram of energy")
box()
lines(density(energy), col="red")

#FITTING MODEL
fit.BE <- histDist(energy, family=BE, nbins=15, main="Beta")
fit.LOGITNO <- histDist(energy, family=LOGITNO, nbins = 15, main="Logit-Normal")
fit.GB1 <- histDist(energy, family=GB1, nbins = 15, main="Generalized Beta")

data.frame(row.names = c("Beta", "Gen Beta", "logit Norm"), AIC=c(AIC(fit.BE), AIC(fit.GB1), AIC(fit.LOGITNO)), SBC=c(fit.BE$sbc, fit.GB1$sbc, fit.LOGITNO$sbc))


#BONUS

danceability_fact <- cut(danceability, breaks = c(0.01,0.25,0.50,0.75,1), labels = c("not danceable","more no than yes","more yes than no","danceable"), include.lowest = TRUE)
energy_fact <- cut(energy, breaks = c(0.001,0.25,0.50,0.75,1), labels = c("no energy","more no than yes","more yes than no","full of energy"), include.lowest = TRUE)
liveness_fact <- cut(liveness, breaks = c(0.001,0.25,0.50,0.75,1), labels = c("no live element","more no than yes","more yes than no","live"), include.lowest = TRUE)

df <- cbind(df, dance_f = danceability_fact, energy_f = energy_fact, live_f = liveness_fact)
view(df)
attach(df)

pie(table(dance_f), main = "Danceability", col = rainbow(length(table(dance_f))))
pie(table(energy_f), main = "Energy", col = rainbow(length(table(energy_f))))
pie(table(live_f), main = "Live Elements", col = rainbow(length(table(live_f))))