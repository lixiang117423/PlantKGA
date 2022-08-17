library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(colourpicker)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("PlantKGA: KEGG, GO, and GSEA for plant genes"),
  navbarPage(
    "Let's get started",
    # home page
    source("main/ui/ui.home.R", local = TRUE, encoding = "UTF-8")$value,
    # go page
    source("main/ui/ui.go.R", local = TRUE, encoding = "UTF-8")$value
  )
))
