#'================== Introduction ==============================================
#' This script for transforming and reformatting data from motile invertebrate
#' surveys done as part of the Hakai Institute's ongoing Rocky Intertidal 
#' survey. The transformed data will be used as the basis for an OBIS dataset.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-01
#' Last Updated:
#' ============================================================================
#================== Preamble ==================================================
# Load required libraries
library(tidyverse)
library(googlesheets4)

# Load most up to date QCd data
mi <- read_csv('./data/motile_invertebrates-surveys.csv')

# Download RI intervals google sheet
rii <- read_sheet(ss = '1Jlbt_-rvoGA6V6EQtIdmaxYnlaru0fZMS52EcN5pIGE',
                  sheet = 'Sheet1',
                  col_types = 'cDc')

# Join intervals to survey data
mi <- left_join(mi, rii)

#================== Event Core ================================================
# Event table for expeditions--------------------------------------------------
mi.e.int <- distinct(rii, interval) # Get distinct intervals
names(mi.e.int) <- c('eventID') # Rename column

# Add other event core columns
mi.e.int$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.int$parentEventID <- NA_character_
mi.e.int$eventDate <- NA_Date_
mi.e.int$eventRemarks <- 'expedition'

# Reorder columns
mi.e.int <- mi.e.int %>% 
  select(datasetName, eventID, parentEventID, eventDate, eventRemarks)

# Event table for site visits--------------------------------------------------
mi.e.sv <- rii # Copy interval data

# Add abbreviated site names column
mi.e.sv$site_code <- NA_character_

for (i in 1:length(mi.e.sv$interval)){
  if (mi.e.sv$site_name[i] == 'North Beach'){
    mi.e.sv$site_code[i] <- 'NB'
  } else{
    if (mi.e.sv$site_name[i] == 'West Beach'){
      mi.e.sv$site_code[i] <- 'NB'
    } else{
      mi.e.sv$site_code[i] <- 'FB'
    }
  }
}

# Build eventID column
mi.e.sv$eventID <- paste(mi.e.sv$interval, 
                         mi.e.sv$site_code,
                         mi.e.sv$date,
                         sep = '_')

# Add other event core columns
mi.e.sv$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.sv$eventRemarks <- 'station visit'

# Select relevant columns
mi.e.sv <- mi.e.sv %>% 
  select(datasetName, eventID, interval, date, eventRemarks)

names(mi.e.sv) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                    'eventRemarks')

# Event table for quadrats-----------------------------------------------------
mi.e.quad <- mi %>% 
  subset(subplot_size == 'whole plot') %>% 
  select(interval, date, site_name, plot_type, plot)
