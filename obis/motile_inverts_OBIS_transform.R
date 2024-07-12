#'================== Introduction ==============================================
#' This script for transforming and reformatting data from motile invertebrate
#' surveys done as part of the Hakai Institute's ongoing Rocky Intertidal 
#' survey. The transformed data will be used as the basis for an OBIS dataset.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-01
#' Last Updated: 2024-04-11
#' ============================================================================
#================== Preamble ==================================================
# Load required libraries
library(tidyverse)
library(googlesheets4)
library(rairtable)

# Load most up to date QCd data
mi <- read_csv('./data/motile_invertebrates-surveys.csv')

# Download RI intervals google sheet
rii <- read_sheet(ss = '1Jlbt_-rvoGA6V6EQtIdmaxYnlaru0fZMS52EcN5pIGE',
                  sheet = 'Sheet1',
                  col_types = 'cDc')

# Join intervals to survey data
mi <- left_join(mi, rii)

# Source taxonomic data
source('./obis/source_taxonomy.R')

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
  select(datasetName, eventID, parentEventID, eventDate, sampleSizeValue,
         sampleSizeUnit, eventRemarks)

# Event table for site visits--------------------------------------------------
mi.e.sv <- rii # Copy interval data

# Add abbreviated site names column
mi.e.sv$site_code <- NA_character_

for (i in 1:length(mi.e.sv$interval)){
  if (mi.e.sv$site_name[i] == 'North Beach'){
    mi.e.sv$site_code[i] <- 'NB'
  } else{
    if (mi.e.sv$site_name[i] == 'West Beach'){
      mi.e.sv$site_code[i] <- 'WB'
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
  select(datasetName, eventID, interval, date, sampleSizeValue, sampleSizeUnit,
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
                      'sampleSizeValue', 'sampleSizeUnit', 'eventRemarks')

# Join events together into single dataset
event <- rbind(mi.e.int, mi.e.sv, mi.e.quad)

# Remove duplicated rows
event <- unique(event)

# Remove unneeded elements
rm(list = c('mi.e.int', 'mi.e.quad', 'mi.e.sv', 'i'))

#================== Occurrence Extension ======================================
# Copy survey data
occurrence <- mi

# Join with taxonomic data
occurrence <- left_join(occurrence, ns.taxa.full)

# Measured occurrences---------------------------------------------------------
occurrence.m <- occurrence %>%         # split off whole plot observations
  subset(count_type == 'Individual - measured')

# Join with interval data
occurrence.m <- left_join(occurrence.m, rii)

# Add abbreviated site names column
occurrence.m$site_code <- NA_character_

for (i in 1:length(occurrence.m$interval)){
  if (occurrence.m$site_name[i] == 'North Beach'){
    occurrence.m$site_code[i] <- 'NB'
  } else{
    if (occurrence.m$site_name[i] == 'West Beach'){
      occurrence.m$site_code[i] <- 'WB'
    } else{
      occurrence.m$site_code[i] <- 'FB'
    }
  }
}

# Add plot type code column
occurrence.m$type.code <- substring(occurrence.m$plot_type, 1, 1)

# Build quadrat code column
occurrence.m$quad.code <- paste(occurrence.m$type.code, 'Q', 
                                 occurrence.m$plot, sep = '')

# Build event ID column to match with event table
occurrence.m$eventID <- paste(occurrence.m$interval, 
                               occurrence.m$site_code,
                               occurrence.m$date,
                               occurrence.m$quad.code,
                               sep = '_')

# Join with event table
occurrence.m <- left_join(occurrence.m, event)

# Add occurrence ID column
occurrence.m$observation <- 1:nrow(occurrence.m) 
occurrence.m$occurrenceID <- paste('indMeasured', occurrence.m$observation,
                                    sep = '-')

# Add other missing columns
occurrence.m$organismQuantityType <- 'individuals measured'

occurrence.m$decimalLatitude <- NA_real_
occurrence.m$decimalLongitude <- NA_real_

for (i in 1:length(occurrence.m$interval)){
  if (occurrence.m$site_name[i] == 'North Beach'){
    occurrence.m$decimalLatitude[i] <- 51.665080
    occurrence.m$decimalLongitude[i] <- -128.134700
  } else{
    if (occurrence.m$site_name[i] == 'West Beach'){
      occurrence.m$decimalLatitude[i] <- 51.657423
      occurrence.m$decimalLongitude[i] <- -128.148814
    } else{
      occurrence.m$decimalLatitude[i] <- 51.640680
      occurrence.m$decimalLongitude[i] <- -128.156779
    }
  }
}

occurrence.m$basisOfRecord <- 'HumanObservation'
occurrence.m$occurrenceStatus <- 'present'

# Unmeasured occurrences-------------------------------------------------------
occurrence.nm <- occurrence %>%         # split off whole plot observations
  subset(count_type == 'Individual - not measured'
         | count_type == 'Large limpets (>15mm)')

# Join with interval data
occurrence.nm <- left_join(occurrence.nm, rii)

# Add abbreviated site names column
occurrence.nm$site_code <- NA_character_

for (i in 1:length(occurrence.nm$interval)){
  if (occurrence.nm$site_name[i] == 'North Beach'){
    occurrence.nm$site_code[i] <- 'NB'
  } else{
    if (occurrence.nm$site_name[i] == 'West Beach'){
      occurrence.nm$site_code[i] <- 'WB'
    } else{
      occurrence.nm$site_code[i] <- 'FB'
    }
  }
}

# Add plot type code column
occurrence.nm$type.code <- substring(occurrence.nm$plot_type, 1, 1)

# Build quadrat code column
occurrence.nm$quad.code <- paste(occurrence.nm$type.code, 'Q', 
                                occurrence.nm$plot, sep = '')

# Build event ID column to match with event table
occurrence.nm$eventID <- paste(occurrence.nm$interval, 
                              occurrence.nm$site_code,
                              occurrence.nm$date,
                              occurrence.nm$quad.code,
                              sep = '_')

# Join with event table
occurrence.nm <- left_join(occurrence.nm, event)

# Add occurrence ID column
occurrence.nm$observation <- 1:nrow(occurrence.nm) 
occurrence.nm$occurrenceID <- paste('ind', occurrence.nm$observation,
                                   sep = '-')

# Add other missing columns
occurrence.nm$organismQuantityType <- 'individuals'

occurrence.nm$decimalLatitude <- NA_real_
occurrence.nm$decimalLongitude <- NA_real_

for (i in 1:length(occurrence.nm$interval)){
  if (occurrence.nm$site_name[i] == 'North Beach'){
    occurrence.nm$decimalLatitude[i] <- 51.665080
    occurrence.nm$decimalLongitude[i] <- -128.134700
  } else{
    if (occurrence.nm$site_name[i] == 'West Beach'){
      occurrence.nm$decimalLatitude[i] <- 51.657423
      occurrence.nm$decimalLongitude[i] <- -128.148814
    } else{
      occurrence.nm$decimalLatitude[i] <- 51.640680
      occurrence.nm$decimalLongitude[i] <- -128.156779
    }
  }
}

occurrence.nm$basisOfRecord <- 'HumanObservation'
occurrence.nm$occurrenceStatus <- 'present'

# Join occurrence tables-------------------------------------------------------
occurrence <- rbind(occurrence.m, occurrence.nm)

# Select relevant columns
occurrence <- occurrence.nm %>% 
  select(eventID, occurrenceID, scientific_name, total_count, 
         organismQuantityType, date, decimalLatitude, decimalLongitude,
         rank, LSID, basisOfRecord, occurrenceStatus)

# Rename columns
names(occurrence) <- c('eventID', 'occurrenceID', 'scientificName',
                       'organismQuantity', 'organismQuantityType',
                       'eventDate', 'decimalLatitude', 'decimalLongitude',
                       'taxonRank', 'scientificNameID', 'basisOfRecord',
                       'occurrenceStatus')
                          
# Remove unneeded elements
rm(list = c('occurrence.nm', 'i'))

#================== Measurement or Fact Extension =============================
# Size Measurements------------------------------------------------------------
mof.s <- mi %>% 
  subset(!is.na(size))
