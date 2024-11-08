# data camera e pipae Bio
########################################################

pkg <- c('googlesheets4',"lubridate", "timetk","dplyr","corrplot",
         "tidyr",'stringr',"viridis")

pkg <- pkg[!pkg%in%installed.packages()] 
pkg
install.packages (pkg)
#########################
library (googlesheets4)
library(lubridate)
library(timetk)
library(dplyr)
library(tidyr)
library(corrplot)
library (stringr)
library(tibble)
library(viridis)


