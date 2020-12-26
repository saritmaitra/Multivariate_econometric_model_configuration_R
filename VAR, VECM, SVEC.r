# remove everything from the environment
rm(list=ls())
install.packages("Quandl")
library(Quandl)
library(tidyverse)
install.packages("alfred")
library(alfred)

wti = as_tibble(get_fred_series("DCOILWTICO", "WTI", observation_start="2000-01-02"))
brent = as_tibble(get_fred_series("DCOILBRENTEU", "Brent", observation_start="2000-01-02"))
hh = as_tibble(get_fred_series("MHHNGSP", 'HenryHub', observation_start="2000-01-02"))

oil_prices = wti %>% left_join(brent, by="date")
prices = hh %>% left_join(oil_prices, by="date")
head(prices)

prices

# identify count of NAs in data frame
sum(is.na(prices))
colSums(is.na(prices))

install.packages("VIM")
library(VIM)
install.packages('mice')
#Loading the mice package
library(mice)

#Loading the following package for looking at the missing values
install.packages('lattice')
library(lattice)


# First look at the data
str(prices)

#understand the missing value pattern
md.pattern(prices)

#Drawing margin plot
marginplot(prices[, c("WTI", "Brent")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

# Imputing missing values
imputes = mice(prices, m=5, maxit = 40)
# methods used for imputing
imputes

#Imputed dataset
data = complete(imputes,5)
head(data)

# Plotting and comparing values with xyplot()
xyplot(imputes, WTI ~ Brent | .imp, pch = 20, cex = 1.4)

#make a density plot
densityplot(imputes)

 summary(data)

str(data)

# converting to time -series
henryhub = ts(data$HenryHub, start = c(2000,02,01), frequency = 252)
wti = ts(data$WTI, start = c(2000,02,01), frequency = 252)
brent = ts(data$Brent, start = c(2000,02,01), frequency = 252)

par(mfrow=c(3,1), mar=c(0,3.5,0,3), oma=c(3.5,0,2,0), mgp=c(2,.6,0), cex.lab=1.1, tcl=-.3, las=1)
plot(henryhub, ylab=expression('HenryHub price'), xaxt="no", type='n')
  grid(lty=1, col=gray(.9))
  lines(henryhub, col=rgb(0,0,.9))
   
plot(wti, ylab=expression('WTI crude price'), xaxt="no", yaxt='no', type='n')
  grid(lty=1, col=gray(.9))
  lines(wti, col=rgb(.9,0,.9)) 
  axis(4) 
plot(brent, ylab=expression('Brent Crude price'))
  grid(lty=1, col=gray(.9))
  lines(brent, col=rgb(0,.7,0))
title(xlab="Date", outer=TRUE)

library('urca')

library("tseries")
adf.test(log(data[, "HenryHub"]))
adf.test(log(data[, "WTI"]))
adf.test(log(data[, "Brent"]))

pp.test(log(data[, "HenryHub"]), type = "Z(t_alpha)")
pp.test(log(data[, "WTI"]), type = "Z(t_alpha)")
pp.test(log(data[, "Brent"]), type = "Z(t_alpha)")

kpss.test(log(data[, "HenryHub"]))
kpss.test(log(data[, "WTI"]))
kpss.test(log(data[, "Brent"]))

df = cbind(henryhub, wti, brent)
po.test(log(df))

ts.plot(df)

par(mfrow=c(3,1))
plot(diff(data[, "HenryHub"], type='l', main="differenced data (HenryHub)", ylim=c(-0.4, 0.4)))
plot(diff(data[, "WTI"], type='l', main="differenced data (HenryHub)", ylim=c(-0.4, 0.4)))
plot(diff(data[, "Brent"], type='l', main="differenced data (HenryHub)", ylim=c(-0.4, 0.4)))

install.packages('vars')
library(vars)

VARselect(log(df), lag.max = 10, type="const")

# Creating a new dataframe with the differenced variables

diff.data = cbind(diff(df))
VARselect(diff.data, lag.max = 10, type="const")

plot.ts(diff.data, main = "diff(data)")

install.packages('egcm')

library(egcm)
# procedure selects the appropriate values for α, β, and ρ that best fit the following model:
fit_egcm = egcm(diff.data)
summary(fit_egcm)

#data <- df[, c("HenryHub", "WTI", "Brent")]
var_mod <- VAR(df, p = 3, type = "both")
summary(var_mod)

plot(var_mod, names = "HenryHub")

residuals = serial.test(var_mod, lags.pt=3, type="PT.asymptotic") # residuals
residuals$serial

norm <- normality.test(var_mod)
norm$jb.mul

plot(norm, names = "HenryHub")

arch <- arch.test(var_mod, lags.multi = 5, multivariate.only = TRUE)
arch$arch.mul

## Stability
plot(stability(var_mod, type = "Rec-CUSUM"))


causality(var_mod, cause = 'wti')
causality(var_mod, cause = 'brent')
causality(var_mod, cause = 'henryhub')

## IRF
plot(irf(var_mod, impulse = "wti", response = c("henryhub"), n_ahead=15, boot = TRUE))

## IRF
plot(irf(var_mod, impulse = "brent", response = c("henryhub"), n_ahead=15, boot = TRUE))

plot(fevd(var_mod, n.ahead = 15))

forecast <- predict(var_mod, n.ahead = 15, ci = 0.95)
fanchart(forecast, names = "henryhub", main = "HenryHub price forecast", 
         xlab = "Horizon", ylab = "Price", colors = "red")
forecast

library(urca)
coin = ca.jo(df, type = "eigen", ecdet = "none", K = 3, spec = "transitory")
summary(coin)

coin = ca.jo(df, type = "trace", ecdet = "none", K = 3, spec = "transitory")
# endet = 'none' means there is a linear trend in data
summary(coin)

s = 1*henryhub - 0.7788110 * wti + 0.6723858*brent + 0.4075001
plot(s, type ='l')
adf.test(s)

adf.test(s)

vecm <- cajorls(coin)
vecm
# extract error correction term co-efficient (ECT)

install.packages('tsDyn')
library(tsDyn)

model_vecm = VECM(df, lag=3, r=1, estim = 'ML')
summary(model_vecm)

# VAR reprecsentation of VECM
VARrep(model_vecm)

fcast = predict(model_vecm, n.ahead = 15)
fcast


