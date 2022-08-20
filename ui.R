library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(colourpicker)

# in shiny server, please run the next code
#library(dbplyr,lib.loc = "/home/lixiang/miniconda3/envs/Rpackages/lib/R/library/")


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
