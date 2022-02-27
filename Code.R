
# install.packages(c("readxl","zoo","urca","sandwich","vars",
#                    "svars","tseries","tsDyn","dplyr","minqa","HI","mvnfast",
#                    "VARsignR","ggplot2","plotly","hrbrthemes","xtable","stargazer",
#                    "forecast", "stats", "shiny","devtools", "shinydashboard",
#                    "flexdashboard", "DT", "rmarkdown", "tinytex", "tidyverse",
#                    "lubridate", "plotly", "knitr", "roll", "Rcpp","Quandl",
#                    "KFAS","GetBCBData","seasonal","openxlsx","writexl"))
library(readr)
library(readxl)
library(vars)
library(svars)
library(tseries)
library(VARsignR)
library(ggplot2)
library(plotly)
library(forecast)
library(stats)
library(shiny)
library(devtools)
library(tidyverse)
library(lubridate)
library(Quandl)

rm(list=ls()) #clean the workspace.

cat("\014")



##puxando dados
Base_FX <- read_excel("dados.xlsx",  sheet = "Sheet2")
#Base_FX <- read_csv("C:/Users/faust/Desktop/R codes/Project-Exchange/dados3.csv")
## Definindo variaveis 
Data <- Base_FX$Dates
n <- length(Data)
BRL <- Base_FX$`BRL Curncy`
CLP <- Base_FX$`CLP Curncy`
COP <- Base_FX$`COP Curncy`
MXN <- Base_FX$`MXN Curncy`
ZAR <- Base_FX$`ZAR Curncy`

#plot Graphall
plot(Data, scale(BRL), type = "l", col = "green3", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(Data, scale(CLP), type = "l", col = "red3", ylim = c(-2,3), ylab = "", xlab = "Exchange Rates from 2000-2022")
par(new = T)
plot(Data, scale(COP), type = "l", col = "yellow3", ylim = c(-2,3), ylab = "Std Deviation from the Mean", xlab = "")
par(new = T)
plot(Data, scale(MXN), type = "l", col = "grey", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(Data, scale(ZAR), type = "l", col = "orange3", ylim = c(-2,3), ylab = "", xlab = "")
abline(h = 0, col = "black", lty = 5)
legend("topleft", c("USD.BRL","USD.CLP", "USD.COP","USD.MXN","USD.ZAR"),lwd=1,
       col=c("green3","red3","yellow3","grey","orange3"), cex = 1,bty = "n",xjust=1,
       lty = c(1))


#modelo de fator
P <- data.frame(scale(CLP),scale(COP),scale(MXN),scale(ZAR))
P_f <- prcomp(P)
f1 <- P_f$x[,1]



plot(Data, scale(BRL), type = "l", col = "green3", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(Data, scale(f1), type = "l", col = "black", ylim = c(-2,3), ylab = "", xlab = "")



## equilibrio
ols <- lm(BRL ~ CLP + COP + MXN + ZAR)
summary(ols)
ect <-scale(ols$residuals)



plot(Data, ect, type = "l", col = "red", ylim = c(-3,4), ylab = "Std Deviation from the Mean", xlab = "")
abline(h = -2, col = "black", lty = 2)
abline(h = 0, col = "black", lty = 3)
abline(h = 2, col = "black", lty = 2)
#abline(h = scale(ect2_BRL)[n-i+1], col = "brown2", lty = 2)
legend("topleft", c("ECT = BRL - BRL(pairs) "),lwd=1,
              col=c("red"), cex = 0.72,bty = "n",xjust=1, lty = c(1))


BRL_hat <- scale(ols$fitted.values)
plot(Data, scale(BRL), type = "l", col = "green3", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)

plot(Data, scale(BRL_hat), type = "l", col = "blue", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(Data, scale(f1), type = "l", col = "black", ylim = c(-2,3), ylab = "", xlab = "")
legend("topleft", c("BRL","hat BRL", "1st factor"),lwd=1,
       col=c("green3","blue","black"), cex = 1,bty = "n",xjust=1,
              lty = c(1))



# pHILLIPS peRRON tEST FOR UNIT ROOT
pp.test(BRL)
pp.test(CLP)
pp.test(COP)
pp.test(MXN)
pp.test(ZAR)
pp.test(ect) # rejeita h0 de raiz unitaria


hist(ect, main = "Histogram of ECT", xlab = "Std. Deviation of ECT")

