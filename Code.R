library(readr)
library(readxl)
library(vars)
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
inicial <- Sys.time()
cat("\014")


##puxando dados
Base_FX <- read_excel("dados.xlsx",  sheet = "Sheet2")
# Base_FX <- read_csv("C:/Users/faust/Desktop/R codes/Project-Exchange/dados5.csv", 
#                     col_types = cols(`Dates` = col_character(),
#                                      `BRL Curncy` = col_number(),
#                                      `CLP Curncy` = col_number(),
#                                      `COP Curncy` = col_number(),
#                                      `MXN Curncy` = col_number(),
#                                      `ZAR Curncy` = col_number()))
#Base_FX <- read.csv("C:/Users/faust/Desktop/R codes/Project-Exchange/dados5.csv", sep = ";")

## Definindo variaveis 
Data <- Base_FX$Dates
n <- length(Data)
BRL <- Base_FX$`BRL Curncy`
CLP <- Base_FX$`CLP Curncy`
COP <- Base_FX$`COP Curncy`
MXN <- Base_FX$`MXN Curncy`
ZAR <- Base_FX$`ZAR Curncy`


nova <- filter(Base_FX, Dates > "2010-01-01")
datan <- nova$Dates
BRLn <- nova$`BRL Curncy`
CLPn <- nova$`CLP Curncy`
COPn <- nova$`COP Curncy`
MXNn <- nova$`MXN Curncy`
ZARn <- nova$`ZAR Curncy`

#gráfico inicial
plot(datan, scale(BRLn), type = "l", col = "green3", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(datan, scale(CLPn), type = "l", col = "red3", ylim = c(-2,3), ylab = "", xlab = "Exchange Rates from 2010-2022")
par(new = T)
plot(datan, scale(COPn), type = "l", col = "yellow3", ylim = c(-2,3), ylab = "Std Deviation from the Mean", xlab = "")
par(new = T)
plot(datan, scale(MXNn), type = "l", col = "grey", ylim = c(-2,3), ylab = "", xlab = "")
par(new = T)
plot(datan, scale(ZARn), type = "l", col = "orange3", ylim = c(-2,3), ylab = "", xlab = "")
abline(h = 0, col = "black", lty = 5)
legend("topleft", c("USD.BRL","USD.CLP", "USD.COP","USD.MXN","USD.ZAR"),lwd=1,
       col=c("green3","red3","yellow3","grey","orange3"), cex = 1,bty = "n",xjust=1,
       lty = c(1))


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


## equilibrio
ols <- lm(BRL ~ CLP + COP + MXN + ZAR)
summary(ols)
ect <-scale(ols$residuals)
BRL_hat <- scale(ols$fitted.values)


#grafico
plot(Data, ect, type = "l", col = "red", ylim = c(-3,4), ylab = "Std Deviation from the Mean", xlab = "Cointegration Error of the ECM model")
abline(h = -2, col = "black", lty = 2)
abline(h = 0, col = "black", lty = 3)
abline(h = 2, col = "black", lty = 2)
#abline(h = scale(ect2_BRL)[n-i+1], col = "brown2", lty = 2)
legend("topleft", c("ECT = BRL - BRL(pairs) "),lwd=1,
              col=c("red"), cex = 0.72,bty = "n",xjust=1, lty = c(1))


#grafico
plot(Data, scale(BRL), type = "l", col = "green3", ylim = c(-2,3), ylab = "Std Deviation from the Mean", xlab = "")
par(new = T)
plot(Data, scale(BRL_hat), type = "l", col = "blue", ylim = c(-2,3), ylab = "", xlab = "Principal Component Analysis and Fitted Value of ECM model")
par(new = T)
plot(Data, scale(f1), type = "l", col = "black", ylim = c(-2,3), ylab = "", xlab = "")
abline(h = 0, col = "black", lty = 5)
legend("topleft", c("BRL","hat BRL", "1st factor"),lwd=1,
       col=c("green3","blue","black"), cex = 1,bty = "n",xjust=1,
              lty = c(1))

# Phillips-perron Test Unit Root
pp.test(BRL)
pp.test(CLP)
pp.test(COP)
pp.test(MXN)
pp.test(ZAR)
pp.test(ect) # rejeita h0 de raiz unitaria


hist(ect, main = "Histogram of ECT", xlab = "Std. Deviation of ECT")

final <- Sys.time()
final - inicial

