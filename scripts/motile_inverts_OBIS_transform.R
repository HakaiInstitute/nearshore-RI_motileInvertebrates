#'================== Introduction ==============================================
#' This script for transforming and reformatting data from motile invertebrate
#' surveys done as part of the Hakai Institute's ongoing Rocky Intertidal 
#' survey. The transformed data will be used as the basis for an OBIS dataset.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-01
#' Last Updated: 2024-04-10
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
source('./scripts/source_taxonomy.R')

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

# Event table for subplots-----------------------------------------------------
# Copy survey data
mi.e.sl <- mi

# Add plot type code column
mi.e.sl$type.code <- substring(mi.e.sl$plot_type, 1, 1)

# Add abbreviated site names column
mi.e.sl$site_code <- NA_character_

for (i in 1:length(mi.e.sl$interval)){
  if (mi.e.sl$site_name[i] == 'North Beach'){
    mi.e.sl$site_code[i] <- 'NB'
  } else{
    if (mi.e.sl$site_name[i] == 'West Beach'){
      mi.e.sl$site_code[i] <- 'WB'
    } else{
      mi.e.sl$site_code[i] <- 'FB'
    }
  }
}

# Build quadrat code column
mi.e.sl$quad.code <- paste(mi.e.sl$interval, mi.e.sl$site_code,
                           mi.e.sl$date, sep = '_')
mi.e.sl$quad.code <- paste(mi.e.sl$quad.code, '_', mi.e.sl$type.code, 'Q', 
                           mi.e.sl$plot, sep = '')

# Build subplot code column
mi.e.sl$sp_code <- NA_character_

for (i in 1:length(mi.e.sl$date)){
  if (is.na(mi.e.sl$subplot_location[i])){
    mi.e.sl$sp_code[i] <- NA
  } else {
    if (mi.e.sl$subplot_location[i] == 'top left (1)'){
      mi.e.sl$sp_code[i] <- 'S1'
    } else {
      if (mi.e.sl$subplot_location[i] == 'middle (2)'){
        mi.e.sl$sp_code[i] <- 'S2'
      } else{
        mi.e.sl$sp_code[i] <- 'S3'
      }
    }
  }
}

mi.e.sl$sp_code <- paste(mi.e.sl$quad.code, mi.e.sl$sp_code, sep = '_')

# Find distinct subplots
mi.e.sl <- mi.e.sl %>% 
  drop_na(subplot_location) %>%
  subset(count_type == 'Small limpets (<5mm)' 
         | count_type == 'Medium limpets (5-15mm)'
         | count_type == 'Littorines') %>% 
  distinct(subplot_size, quad.code, sp_code, count_type)

# Make simplified count type code
mi.e.sl$count_code <- NA_character_

for (i in 1:length(mi.e.sl$quad.code)){
  if (is.na(mi.e.sl$count_type[i])){
    mi.e.sl$count_code[i] <- NA
  } else {
    if (mi.e.sl$count_type[i] == 'Small limpets (<5mm)'){
      mi.e.sl$count_code[i] <- 'SL'
    } else {
      if (mi.e.sl$count_type[i] == 'Medium limpets (5-15mm)'){
        mi.e.sl$count_code[i] <- 'ML'
      } else{
        mi.e.sl$count_code[i] <- 'L'
      }
    }
  }
}

mi.e.sl$sp_code <- paste(mi.e.sl$sp_code, mi.e.sl$count_code, sep = '')

# Select relevant columns
mi.e.sl <- mi.e.sl %>% 
  select(quad.code, sp_code, subplot_size)

# Calculate subplot size
mi.e.sl$sampleSizeValue <- NA_real_

for (i in 1:length(mi.e.sl$quad.code)){
  if (mi.e.sl$subplot_size[i] == '10cm x 10cm'){
    mi.e.sl$sampleSizeValue[i] <- 0.01
  } else {
    mi.e.sl$sampleSizeValue[i] <- 0.04
  }
}

# Rename columns
names(mi.e.sl) <- c('parentEventID', 'eventID', 'subplot_size',
                    'sampleSizeValue')

# Add missing columns
mi.e.sl$eventDate <- NA_Date_
mi.e.sl$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.sl$sampleSizeUnit <- 'square meter'
mi.e.sl$eventRemarks <- 'sub quadrat'

# Select relevant columns
mi.e.sl <- mi.e.sl %>% 
  select(datasetName, eventID, parentEventID, eventDate, sampleSizeValue,
         sampleSizeUnit, eventRemarks)

# Join events together into single dataset
event <- rbind(mi.e.int, mi.e.sv, mi.e.quad, mi.e.sl)

# Remove unneeded elements
rm(list = c('mi.e.int', 'mi.e.quad', 'mi.e.sl', 'mi.e.sv', 'i'))

#================== Occurrence Extension ======================================
# Copy survey data
occurrence <- mi

# Join with taxonomic data
occurrence <- left_join(occurrence, ns.taxa.full)

# Whole plot occurrences-------------------------------------------------------
occurrence.wp <- occurrence %>%         # split off whole plot observations
  subset(subplot_size == 'whole plot' | subplot_size == '75cm x 50cm') %>% 
  group_by(date, site_name, plot_type, plot, rank, scientific_name) %>% 
  summarise(total_count = sum(count)) %>% 
  ungroup()

# Join with interval data
occurrence.wp <- left_join(occurrence.wp, rii)

# Add abbreviated site names column
occurrence.wp$site_code <- NA_character_

for (i in 1:length(occurrence.wp$interval)){
  if (occurrence.wp$site_name[i] == 'North Beach'){
    occurrence.wp$site_code[i] <- 'NB'
  } else{
    if (occurrence.wp$site_name[i] == 'West Beach'){
      occurrence.wp$site_code[i] <- 'WB'
    } else{
      occurrence.wp$site_code[i] <- 'FB'
    }
  }
}

# Add plot type code column
occurrence.wp$type.code <- substring(occurrence.wp$plot_type, 1, 1)

# Build quadrat code column
occurrence.wp$quad.code <- paste(occurrence.wp$type.code, 'Q', 
                                 occurrence.wp$plot, sep = '')