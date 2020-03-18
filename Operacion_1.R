library(shinydashboard)
library(shinyWidgets)
library(dtplyr)
library(dplyr)
library(data.table)
library(DT)
library(readxl)
library(tidyr)

MesaDeControlUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("kps"))
  
}

MesaDeControl <- function(input, output, session, usf, direcc, baseOPP){
  ns <- session$ns
  
  MCompleto <- fread(paste0(direcc,"insumos/MDControl/comentaMDC.csv"), sep = ",", colClasses = c(rep("character",15)))
  MCompleto$fecha <- as.Date(MCompleto$fecha)
  MCompleto <- MCompleto[order(-fecha,-hora)]
  
  
} #Cierra función "server"