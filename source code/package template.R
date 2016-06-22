install.packages("tseries")
install.packages("quantmod")
install.packages("FitARMA")
install.packages("PerformanceAnalytics")
install.packages("WriteXLS")
install.packages(c("zoo","forecast","FinTs","reguarch"))
install.packages("sampling")
install.packages("fGarch")
install.packages("TSA")

library(tseries);library(FitARMA);library(quantmod);library(PerformanceAnalytics);library(sampling)
require(TTR);require(graphics) ;library(fGarch);library(TSA);require(TSA)

rm(list = ls(all =TRUE))
