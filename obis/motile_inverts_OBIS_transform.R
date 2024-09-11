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
mi.e.int$eventType <- 'expedition'
mi.e.int$eventRemarks <- NA_character_
mi.e.int$decimalLatitude <- NA_real_
mi.e.int$decimalLongitude <- NA_real_
mi.e.int$coordinateUncertaintyInMeters <- NA_real_

# Reorder columns
mi.e.int <- mi.e.int %>% 
  select(datasetName, eventID, parentEventID, eventDate, decimalLatitude,
         decimalLongitude, coordinateUncertaintyInMeters, sampleSizeValue,
         sampleSizeUnit, eventType, eventRemarks)

# Event table for site visits--------------------------------------------------
mi.e.sv <- rii # Copy interval data

# Add abbreviated site names column and lat/long columns
mi.e.sv$site_code <- NA_character_
mi.e.sv$decimalLatitude <- NA_real_
mi.e.sv$decimalLongitude <- NA_real_
mi.e.sv$coordinateUncertaintyInMeters <- 5

for (i in 1:length(mi.e.sv$interval)){
  if (mi.e.sv$site_name[i] == 'North Beach'){
    mi.e.sv$site_code[i] <- 'NB'
    mi.e.sv$decimalLatitude[i] <- 51.665080
    mi.e.sv$decimalLongitude[i] <- -128.134700
  } else{
    if (mi.e.sv$site_name[i] == 'West Beach'){
      mi.e.sv$site_code[i] <- 'WB'
      mi.e.sv$decimalLatitude[i] <- 51.657423
      mi.e.sv$decimalLongitude[i] <- -128.148814
    } else{
      mi.e.sv$site_code[i] <- 'FB'
      mi.e.sv$decimalLatitude[i] <- 51.640680
      mi.e.sv$decimalLongitude[i] <- -128.156779
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
mi.e.sv$eventType <- 'station visit'
mi.e.sv$eventRemarks <- NA_character_

# Select relevant columns
mi.e.sv <- mi.e.sv %>% 
  select(datasetName, eventID, interval, date, decimalLatitude,
         decimalLongitude, coordinateUncertaintyInMeters, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.sv) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                    'decimalLongitude', 'decimalLatitude', 
                    'coordinateUncertaintyInMeters', 'sampleSizeUnit',
                    'sampleSizeValue', 'eventType', 'eventRemarks')

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

# Add lat/long columns
mi.e.quad.coord <- read_csv('obis/quadrat_locations.csv')
mi.e.quad <- left_join(mi.e.quad, mi.e.quad.coord)
mi.e.quad$coordinateUncertaintyInMeters <- 0.1

# Select relevant columns
mi.e.quad <- mi.e.quad %>% 
  select(datasetName, eventDate, quad.code, eventID, quadratLatitude, 
         quadratLongitude, coordinateUncertaintyInMeters)

# Build the quad event column
mi.e.quad$quad.code <- paste(mi.e.quad$eventID, mi.e.quad$quad.code, sep = '_')

# Add missing columns
mi.e.quad$sampleSizeValue <- 0.38
mi.e.quad$sampleSizeUnit <- 'square meter'
mi.e.quad$eventType <- 'quadrat'
mi.e.quad$eventRemarks <- 'Only Lottia spp. with length > 15mm were counted'

# Select relevant columns
mi.e.quad <- mi.e.quad %>% 
  select(datasetName, eventID, quad.code, eventDate, quadratLatitude,
         quadratLongitude, coordinateUncertaintyInMeters, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.quad) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                    'decimalLongitude', 'decimalLatitude', 
                    'coordinateUncertaintyInMeters', 'sampleSizeUnit',
                    'sampleSizeValue', 'eventType', 'eventRemarks')

# Event table for sub-quadrats-------------------------------------------------
mi.e.subquad <- mi %>% 
  subset(count_type == 'Littorines') %>% 
  distinct(date, site_name, plot_type, plot, subplot_location, subplot_size)

# Add plot type code column
mi.e.subquad$type.code <- substring(mi.e.subquad$plot_type, 1, 1)

# Build quadrat code column
mi.e.subquad$quad.code <- paste(mi.e.subquad$type.code, 'Q', mi.e.subquad$plot, 
                                sep = '')

# Build sub-quadrat code column
mi.e.subquad$subquad.code <- NA_character_

for (i in 1:length(mi.e.subquad$date)){
  if (mi.e.subquad$subplot_location[i] == 'top left (1)'){
    mi.e.subquad$subquad.code[i] <- '1'
  } else{
    if (mi.e.subquad$subplot_location[i] == 'middle (2)'){
      mi.e.subquad$subquad.code[i] <- '2'
    } else{
      mi.e.subquad$subquad.code[i] <- '3'
    }
  }
}

# Join to intervals dataset
mi.e.subquad <- left_join(mi.e.subquad, rii)

# Build parentEventID column
mi.e.subquad$site_code <- NA_character_

for (i in 1:length(mi.e.subquad$interval)){
  if (mi.e.subquad$site_name[i] == 'North Beach'){
    mi.e.subquad$site_code[i] <- 'NB'
  } else{
    if (mi.e.subquad$site_name[i] == 'West Beach'){
      mi.e.subquad$site_code[i] <- 'WB'
    } else{
      mi.e.subquad$site_code[i] <- 'FB'
    }
  }
}

mi.e.subquad$parentEventID <- paste(mi.e.subquad$interval, 
                                    mi.e.subquad$site_code,
                                    mi.e.subquad$date,
                                    mi.e.subquad$quad.code, 
                                    mi.e.subquad$subquad.code,
                                    sep = '_')

# Build event ID column
mi.e.subquad$eventID <- paste(mi.e.subquad$parentEventID, 
                              mi.e.subquad$subquad.code,
                              sep = '_')

# Add sample size column
mi.e.subquad$sampleSizeValue <- NA_real_

for (i in 1:length(mi.e.subquad$interval)){
  if (mi.e.subquad$subplot_size[i] == '10cm x 10cm'){
    mi.e.subquad$sampleSizeValue[i] <- 0.01
    } else{
      mi.e.subquad$sampleSizeValue[i] <- 0.04
    }
  }

# Add other event core columns
mi.e.subquad$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.subquad$sampleSizeUnit <- 'square meter'
mi.e.subquad$eventType <- 'sub-quadrat'
mi.e.subquad$eventRemarks <- NA_character_
mi.e.subquad$decimalLatitude <- NA_real_
mi.e.subquad$decimalLongitude <- NA_real_
mi.e.subquad$coordinateUncertaintyInMeters <- NA_real_

# Select relevant columns
mi.e.subquad <- mi.e.subquad %>% 
  select(datasetName, eventID, parentEventID, date, decimalLatitude,
         decimalLongitude, coordinateUncertaintyInMeters, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.subquad) <- c('datasetName', 'eventID', 'parentEventID', 'eventDate',
                         'decimalLongitude', 'decimalLatitude', 
                         'coordinateUncertaintyInMeters', 'sampleSizeUnit',
                         'sampleSizeValue', 'eventType', 'eventRemarks')
                      

# Join events together into single dataset-------------------------------------
event <- rbind(mi.e.int, mi.e.sv, mi.e.quad, mi.e.subquad)

# Add sampling protocol column
event$samplingProtocol <- 'https://github.com/HakaiInstitute/nearshore-RI_motileInvertebrates/blob/eaebf4b2b252b48ba79eae92e32e5f8b6d6f2ac1/protocols/rocky_intertidal-protocol.pdf'

# Remove duplicated rows
event <- unique(event)

# Remove unneeded elements
rm(list = c('mi.e.int', 'mi.e.quad', 'mi.e.quad', 'i'))

#================== Occurrence Extension ======================================
# Copy survey data
occurrence <- mi

# Join with taxonomic data
occurrence <- left_join(occurrence, ns.taxa.full)

# Measured occurrences---------------------------------------------------------
occurrence.m <- occurrence %>%         # split off whole plot observations
  subset(count_type == 'Individual - measured' & !is.na(size))

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
occurrence <- occurrence %>% 
  select(eventID, occurrenceID, scientific_name, count, organismQuantityType, 
         date, decimalLatitude, decimalLongitude, rank, LSID, basisOfRecord, 
         occurrenceStatus)

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
mof.s <- occurrence.m

# Add measurement id to size measurement table
mof.s$measurementID <- paste(mof.s$eventID, mof.s$occurrenceID, "length",
                             sep = '_')

# Select relevant columns
mof.s <- mof.s %>% 
  select(eventID, occurrenceID, measurementID, size)

# Add missing columns
mof.s$measurementType <- 'Length'
mof.s$measurementMethod <- 'In the field'
mof.s$measurementTypeID <- NA_character_
mof.s$measurementUnit <- 'mm'
mof.s$measurementUnitID <- NA_character_
mof.s$measurementValueID <- NA_character_

# Rearrange columns
mof.s <- mof.s %>% 
  select(eventID, occurrenceID, measurementID, measurementMethod,
         measurementType, measurementTypeID, measurementUnit, 
         measurementUnitID, size, measurementValueID)

# Rename size column
names(mof.s)[names(mof.s) == 'size'] <- 'measurementValue'

# YSI Measurements-------------------------------------------------------------
# Download data from portal
client <- hakaiApi::Client$new()

ysi <- client$get("https://hecate.hakai.org/api//eims/views/output/ysi?survey=ROCKY&limit=-1")

# Remove irrelevant sites
ysi <- ysi %>% 
  subset(site_id == 'ROCKY03' | site_id == 'ROCKY3' | site_id == 'ROCKY06'
         | site_id == 'ROCKY6' | site_id == 'ROCKY07' | site_id == 'ROCKY7')

# Translate site codes to site names
ysi$site_name <- NA_character_

for (i in 1:length(ysi$site_id)){
  if (ysi$site_id[i] == 'ROCKY07' | ysi$site_id[i] == 'ROCKY7'){
    ysi$site_name[i] <- 'North Beach'
  } else{
    if (ysi$site_id[i] == 'ROCKY06' | ysi$site_id[i] == 'ROCKY6'){
      ysi$site_name[i] <- 'West Beach'
    } else{
      ysi$site_name[i] <- 'Fifth Beach'
    }
  }
}

# Select columns and join to intervals
ysi <- ysi %>% 
  select(date, site_name, temperature, salinity)

ysi <- left_join(ysi, rii)

# Remove surveys with no data (+ missing data - TEMPORARY!!)
ysi <- ysi %>% 
  drop_na(interval, salinity)

# Remove duplicates
ysi <- ysi %>% 
  distinct(date, site_name, temperature, salinity, interval)

# Add abbreviated site names column
ysi$site_code <- NA_character_

for (i in 1:length(ysi$interval)){
  if (ysi$site_name[i] == 'North Beach'){
    ysi$site_code[i] <- 'NB'
  } else{
    if (ysi$site_name[i] == 'West Beach'){
      ysi$site_code[i] <- 'WB'
    } else{
      ysi$site_code[i] <- 'FB'
    }
  }
}

# Build eventID column
ysi$eventID <- paste(ysi$interval, ysi$site_code, ysi$date, sep = '_')

# Rearrange columns and pivot longer
ysi.l <- ysi %>% 
  select(eventID, temperature, salinity) %>% 
  pivot_longer(c(temperature, salinity), names_to = 'measurementType',
               values_to = 'measurementValue')

# Change measurement type values
for (i in 1:length(ysi.l$eventID)){
  if (ysi.l$measurementType[i] == 'temperature'){
    ysi.l$measurementType[i] <- 'sea_surface_temperature'
  } else{
      ysi.l$measurementType[i] <- 'sea_surface_salinity'
  }
}

# Add missing columns
ysi.l$occurrenceID <- NA_character_
ysi.l$measurementID <- paste(ysi.l$eventID, ysi.l$measurementType, sep = '_')
ysi.l$measurementMethod <- NA_character_
ysi.l$measurementTypeID <- NA_character_
ysi.l$measurementUnit <- NA_character_
ysi.l$measurementUnitID <- NA_character_
ysi.l$measurementValueID <- NA_character_

# Rearrange columns
ysi.l <- ysi.l %>% 
  select(eventID, occurrenceID, measurementID, measurementMethod,
         measurementType, measurementTypeID, measurementUnit, 
         measurementUnitID, measurementValue, measurementValueID)

# Join measurements together
mof <- rbind(mof.s, ysi.l)
