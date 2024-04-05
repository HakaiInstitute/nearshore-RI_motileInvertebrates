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
mi.e.int$sampleSizeValue <- NA_real_
mi.e.int$sampleSizeUnit <- NA_character_
mi.e.int$eventRemarks <- 'expedition'

# Reorder columns
mi.e.int <- mi.e.int %>% 
  select(datasetName, eventID, parentEventID, eventDate, sampleSizeUnit,
         sampleSizeValue, eventRemarks)

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
mi.e.sv$sampleSizeValue <- NA_real_
mi.e.sv$sampleSizeUnit <- NA_character_
mi.e.sv$eventRemarks <- 'station visit'

# Select relevant columns
mi.e.sv <- mi.e.sv %>% 
  select(datasetName, eventID, interval, date, sampleSizeUnit, sampleSizeValue,
         eventRemarks)

names(mi.e.sv) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                    'sampleSizeUnit', 'sampleSizeValue', 'eventRemarks')

# Event table for quadrats-----------------------------------------------------
# Copy survey data and find distinct quadrats
mi.e.quad <- mi %>% 
  distinct(date, site_name, plot_type, plot)

# Add plot type code column
mi.e.quad$type.code <- substring(mi.e.quad$plot_type, 1, 1)

# Build quadrat code column
mi.e.quad$quad.code <- paste(mi.e.quad$type.code, 'Q', mi.e.quad$plot, 
                             sep = '')

# Rename columns
names(mi.e.quad) <- c('eventDate', 'site_name', 'plot_type', 'plot', 
                      'type.code', 'quad.code')

# Join with site visit data
mi.e.quad <- left_join(mi.e.quad, mi.e.sv)

# Select relevant columns
mi.e.quad <- mi.e.quad %>% 
  select(datasetName, quad.code, eventID)

# Build the quad event column
mi.e.quad$quad.code <- paste(mi.e.quad$eventID, mi.e.quad$quad.code, sep = '_')


# Add missing columns
mi.e.quad$eventDate <- NA_Date_
mi.e.quad$sampleSizeValue <- 0.38
mi.e.quad$sampleSizeUnit <- 'square meter'
mi.e.quad$eventRemarks <- 'quadrat'

# Rename columns
names(mi.e.quad) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                      'sampleSizeUnit', 'sampleSizeValue', 'eventRemarks')

# Event table for subplots-----------------------------------------------------
# Copy survey data and find distinct subplots
mi.e.sp <- mi %>% 
  drop_na(subplot_location) %>% 
  distinct(date, site_name, plot_type, plot, subplot_location)

# Build subplot code column
mi.e.sp$sp_code <- NA_character_

for (i in 1:length(mi.e.sp$date)){
  if (mi.e.sp$subplot_location[i] == 'top left (1)'){
    mi.e.sp$sp_code[i] <- 'S1'
  } else {
    if (mi.e.sp$subplot_location[i] == 'middle (2)'){
      mi.e.sp$sp_code[i] <- 'S2'
    } else{
      mi.e.sp$sp_code[i] <- 'S3'
    }
  }
}

# Rename columns
names(mi.e.sp) <- c('eventDate', 'site_name', 'plot_type', 'plot', 
                    'subplot_location', 'sp_code')

# Join with site visit data
mi.e.sp <- left_join(mi.e.sp, mi.e.quad)

# Select relevant columns
mi.e.sp <- mi.e.sp %>% 
  select(datasetName, sp_code, eventID)

# Build the quad event column
mi.e.quad$quad.code <- paste(mi.e.quad$eventID, mi.e.quad$quad.code, sep = '_')


# Add missing columns
mi.e.quad$eventDate <- NA_Date_
mi.e.quad$sampleSizeValue <- 0.38
mi.e.quad$sampleSizeUnit <- 'square meter'
mi.e.quad$eventRemarks <- 'quadrat'

# Rename columns
names(mi.e.quad) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                      'sampleSizeUnit', 'sampleSizeValue', 'eventRemarks')