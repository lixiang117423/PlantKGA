library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)

datos <-read.csv("www/dataset.csv",dec = ",")

shinyServer(function(input, output){
  source("main/server/server.home.R", local = TRUE, encoding = "UTF-8")
})
