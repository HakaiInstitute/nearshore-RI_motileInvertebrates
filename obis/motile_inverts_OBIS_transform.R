#'================== Introduction ==============================================
#' This script for transforming and reformatting data from motile invertebrate
#' surveys done as part of the Hakai Institute's ongoing Rocky Intertidal 
#' survey. The transformed data will be used as the basis for an OBIS dataset.
#' 
#' Author: Tyrel Froese
#' Project: Nearshore
#' Survey: Rocky Intertidal
#' Date Created: 2024-04-01
#' ============================================================================
#================== Preamble ==================================================
# Load required libraries
library(tidyverse)
library(googlesheets4)
library(rairtable)
library(worrms)
library(obistools)

# Load most up to date QCd data
mi <- read_csv('./data/motile_invertebrates-surveys.csv')

# Download RI intervals google sheet and subset by max survey date
rii <- read_sheet(ss = '1Jlbt_-rvoGA6V6EQtIdmaxYnlaru0fZMS52EcN5pIGE',
                  sheet = 'Sheet1',
                  col_types = 'cDc') %>% 
  subset(date <= max(mi$date))

# Add year to intervals
rii$year <- year(rii$date)

# Join intervals to survey data
mi <- left_join(mi, rii)

# Source taxonomic data
source('./obis/source_taxonomy.R')

#================== Event Core ================================================
# Event table for expeditions--------------------------------------------------
# Get distinct intervals and years
mi.e.int <- distinct(rii, interval, year) # Get distinct intervals

names(mi.e.int) <- c('eventID', 'year') # Rename columns

# Add other event core columns
mi.e.int$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.int$parentEventID <- NA_character_
mi.e.int$verbatimLocality <- NA_character_
mi.e.int$eventDate <- NA_Date_
mi.e.int$month <- NA_real_
mi.e.int$day <- NA_real_
mi.e.int$sampleSizeValue <- NA_real_
mi.e.int$sampleSizeUnit <- NA_character_
mi.e.int$eventType <- 'expedition'
mi.e.int$eventRemarks <- NA_character_
mi.e.int$decimalLatitude <- NA_real_
mi.e.int$decimalLongitude <- NA_real_
mi.e.int$coordinateUncertaintyInMeters <- NA_real_
mi.e.int$minimumDistanceAboveSurfaceInMeters <- NA_real_
mi.e.int$maximumDistanceAboveSurfaceInMeters <- NA_real_
mi.e.int$habitat <- NA_character_

# Reorder columns
mi.e.int <- mi.e.int %>% 
  select(datasetName, eventID, parentEventID, verbatimLocality, eventDate,
         year, month, day, decimalLatitude, decimalLongitude, 
         coordinateUncertaintyInMeters, minimumDistanceAboveSurfaceInMeters, 
         maximumDistanceAboveSurfaceInMeters, habitat, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

# Start building event table
event <- mi.e.int

# Event table for site visits--------------------------------------------------
mi.e.sv <- rii # Copy interval data

# Add abbreviated site names column and lat/long columns
mi.e.sv$site_code <- NA_character_
mi.e.sv$decimalLatitude <- NA_real_
mi.e.sv$decimalLongitude <- NA_real_
mi.e.sv$coordinateUncertaintyInMeters <- 5

mi.e.sv <- mi.e.sv %>%
  mutate(site_code = ifelse(site_name == "North Beach", "NB",
                            ifelse(site_name == "West Beach", "WB", "FB")),
         decimalLatitude = ifelse(site_name == "North Beach", 51.665080,
                                  ifelse(site_name == "West Beach", 51.657423,
                                         51.640680)),
         decimalLongitude = ifelse(site_name == "North Beach", -128.134700,
                                   ifelse(site_name == "West Beach", -128.148814,
                                          -128.156779)))

# Build eventID column
mi.e.sv$eventID <- paste(mi.e.sv$interval, 
                         mi.e.sv$site_code,
                         mi.e.sv$date,
                         sep = '_')

# Add other event core columns
mi.e.sv$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.sv$verbatimLocality <- mi.e.sv$site_name
mi.e.sv$month <- month(mi.e.sv$date)
mi.e.sv$day <- day(mi.e.sv$date)
mi.e.sv$sampleSizeValue <- NA_real_
mi.e.sv$sampleSizeUnit <- NA_character_
mi.e.sv$eventType <- 'station visit'
mi.e.sv$eventRemarks <- NA_character_
mi.e.sv$minimumDistanceAboveSurfaceInMeters <- -0.1
mi.e.sv$maximumDistanceAboveSurfaceInMeters <- 5
mi.e.sv$habitat <- NA_character_

# Select relevant columns
mi.e.sv <- mi.e.sv %>% 
  select(datasetName, eventID, interval, verbatimLocality, date, year, month, 
         day, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters,
         minimumDistanceAboveSurfaceInMeters, 
         maximumDistanceAboveSurfaceInMeters, habitat, sampleSizeValue,
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.sv) <- c('datasetName', 'eventID', 'parentEventID', 
                    'verbatimLocality', 'eventDate', 'year', 'month', 'day',
                    'decimalLatitude', 'decimalLongitude', 
                    'coordinateUncertaintyInMeters',
                    'minimumDistanceAboveSurfaceInMeters',
                    'maximumDistanceAboveSurfaceInMeters', 'habitat',
                    'sampleSizeUnit', 'sampleSizeValue', 'eventType', 
                    'eventRemarks')

# Add to event table
event <- rbind(event, mi.e.sv)

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
         quadratLongitude, quadrat_elevation, coordinateUncertaintyInMeters)

# Build the quad event column
mi.e.quad$quad.code <- paste(mi.e.quad$eventID, mi.e.quad$quad.code, sep = '_')

# Add missing columns
mi.e.quad$year <- NA_real_
mi.e.quad$month <- NA_real_
mi.e.quad$day <- NA_real_
mi.e.quad$verbatimLocality <- NA_character_
mi.e.quad$sampleSizeValue <- 0.38
mi.e.quad$sampleSizeUnit <- 'square meter'
mi.e.quad$eventType <- 'quadrat'
mi.e.quad$eventRemarks <- 'Only Lottia spp. with length > 15mm were counted'
mi.e.quad$minimumDistanceAboveSurfaceInMeters <- mi.e.quad$quadrat_elevation
mi.e.quad$maximumDistanceAboveSurfaceInMeters <- mi.e.quad$quadrat_elevation

mi.e.quad$habitat <- ifelse(grepl('BQ', mi.e.quad$quad.code), 
                            'rocky intertidal dominated by barnacle species',
                            ifelse(grepl('FQ', mi.e.quad$quad.code),
                                   'rocky intertidal dominated by Fucus distichus',
                                   'rocky intertidal dominated by Mytilus californianus'))

# Select relevant columns
mi.e.quad <- mi.e.quad %>% 
  select(datasetName, quad.code, eventID, verbatimLocality, eventDate,
         year, month, day, quadratLatitude, quadratLongitude, 
         coordinateUncertaintyInMeters, minimumDistanceAboveSurfaceInMeters, 
         maximumDistanceAboveSurfaceInMeters, habitat, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.quad) <- c('datasetName', 'eventID', 'parentEventID',
                      'verbatimLocality', 'eventDate', 'year', 'month', 'day', 
                      'decimalLatitude', 'decimalLongitude', 
                      'coordinateUncertaintyInMeters', 
                      'minimumDistanceAboveSurfaceInMeters',
                      'maximumDistanceAboveSurfaceInMeters', 'habitat',
                      'sampleSizeUnit', 'sampleSizeValue', 'eventType', 
                      'eventRemarks')

# Add to event table and flatten events
event <- rbind(event, mi.e.quad)

event <- flatten_event(event = event, fields = c('verbatimLocality',
                                                 'eventDate', 'year', 'month',
                                                 'day'))

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

mi.e.subquad$subquad.code <- ifelse(mi.e.subquad$subplot_location == 'top left (1)', '1',
                                    ifelse(mi.e.subquad$subplot_location == 'middle (2)', 
                                           '2', '3'))

# Join to intervals dataset
mi.e.subquad <- left_join(mi.e.subquad, rii)

# Build parentEventID column
mi.e.subquad$site_code <- NA_character_

mi.e.subquad$site_code <- ifelse(mi.e.subquad$site_name == "North Beach", "NB",
                                 ifelse(mi.e.subquad$site_name == "West Beach", 
                                        "WB", "FB"))

mi.e.subquad$parentEventID <- paste(mi.e.subquad$interval, 
                                    mi.e.subquad$site_code,
                                    mi.e.subquad$date,
                                    mi.e.subquad$quad.code,
                                    sep = '_')

# Build event ID column
mi.e.subquad$eventID <- paste(mi.e.subquad$parentEventID, 
                              mi.e.subquad$subquad.code,
                              sep = '_')

# Add sample size column
mi.e.subquad$sampleSizeValue <- NA_real_

mi.e.subquad$sampleSizeValue <- ifelse(mi.e.subquad$subplot_size == '10cm x 10cm',
                                       0.01, 0.04)

# Add other event core columns
mi.e.subquad$datasetName <- 'Hakai Institute Rocky Intertidal Invertebrates'
mi.e.subquad$verbatimLocality <- NA_character_
mi.e.subquad$year <- NA_real_
mi.e.subquad$month <- NA_real_
mi.e.subquad$day <- NA_real_
mi.e.subquad$sampleSizeUnit <- 'square meter'
mi.e.subquad$eventType <- 'sub-quadrat'
mi.e.subquad$eventRemarks <- NA_character_
mi.e.subquad$decimalLatitude <- NA_real_
mi.e.subquad$decimalLongitude <- NA_real_
mi.e.subquad$coordinateUncertaintyInMeters <- NA_real_
mi.e.subquad$minimumDistanceAboveSurfaceInMeters <- NA_real_
mi.e.subquad$maximumDistanceAboveSurfaceInMeters <- NA_real_
mi.e.subquad$habitat <- NA_character_

# Select relevant columns
mi.e.subquad <- mi.e.subquad %>% 
  select(datasetName, eventID, parentEventID, verbatimLocality, date, year,
         month, day, decimalLatitude, decimalLongitude, 
         coordinateUncertaintyInMeters,
         minimumDistanceAboveSurfaceInMeters,
         maximumDistanceAboveSurfaceInMeters, habitat, sampleSizeValue, 
         sampleSizeUnit, eventType, eventRemarks)

names(mi.e.subquad) <- c('datasetName', 'eventID', 'parentEventID', 
                         'verbatimLocality', 'eventDate', 'year', 'month', 
                         'day', 'decimalLatitude', 'decimalLongitude', 
                         'coordinateUncertaintyInMeters', 
                         'minimumDistanceAboveSurfaceInMeters',
                         'maximumDistanceAboveSurfaceInMeters', 'habitat',
                         'sampleSizeUnit', 'sampleSizeValue', 'eventType', 
                         'eventRemarks')
                      

# Join events together into single dataset-------------------------------------
event <- rbind(event, mi.e.subquad)
                        
# Add sampling protocol column
event$samplingProtocol <- 'https://github.com/HakaiInstitute/nearshore-RI_motileInvertebrates/blob/eaebf4b2b252b48ba79eae92e32e5f8b6d6f2ac1/protocols/rocky_intertidal-protocol.pdf'

# Add language column
event$language <- 'en'

# Add license column
event$license <- 'https://github.com/HakaiInstitute/nearshore-RI_motileInvertebrates/blob/main/LICENSE'

# Add citation column
event$bibliographicCitation <- 'Froese, T., Sadlier-Brown, G., Hessing-Lewis, M., & Gehman, A.-L. (2024). Motile Invertebrate Surveys - BC Central Coast (3.2.0) [Data set]. Hakai Institute. https://doi.org/10.21966/0052-wk15'

# Add rights holder column
event$rightsHolder <- 'Hakai Institute'

# Add institution code
event$institutionCode <- 'https://edmo.seadatanet.org/report/5148'

# Add country & country code
event$country <- 'Canada'
event$countryCode <- 'CA'

#Add geodetic column
event$geodeticDatum <- 'WGS84'

# Add modified column
event$modified <- lubridate::today()

# Save event file
write_csv(event, file = '/obis/obis_outputs/event.csv')

#================== Occurrence Extension ======================================
# Copy survey data
occurrence <- mi

# Join with taxonomic data
occurrence <- left_join(occurrence, ns.taxa)

# Add vitality column
occurrence$vitality <- ifelse(grepl('dead', occurrence$notes), 'dead', 'alive')

# Measured occurrences---------------------------------------------------------
occurrence.m <- occurrence %>%         # split off whole plot observations
  subset(count_type == 'Individual - measured' & !is.na(size))

# Join with interval data
occurrence.m <- left_join(occurrence.m, rii)

# Add abbreviated site names column
occurrence.m$site_code <- NA_character_

occurrence.m$site_code <- ifelse(occurrence.m$site_name == "North Beach", "NB",
                                 ifelse(occurrence.m$site_name == "West Beach", 
                                        "WB", "FB"))

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

# Add other missing columns
occurrence.m$basisOfRecord <- 'HumanObservation'
occurrence.m$occurrenceStatus <- 'present'
occurrence.m$tag <- 'measured individuals'

# Select required columns and change names
occurrence.m <- occurrence.m %>% 
  select(eventID, scientific_name, rank, LSID, common_name, vitality, count, 
         basisOfRecord, occurrenceStatus, notes, tag, size)

names(occurrence.m) <- c('eventID', 'scientificName', 'taxonRank', 
                         'scientificNameID', 'vernacularName', 'vitality',
                         'individualCount', 'basisOfRecord', 
                         'occurrenceStatus', 'occurrenceRemarks', 'tag', 
                         'size')

# Unmeasured occurrences-------------------------------------------------------
occurrence.nm <- occurrence %>%         # split off whole plot observations
  subset(count_type == 'Individual - not measured'
         | count_type == 'Large limpets (>15mm)')

# Join with interval data
occurrence.nm <- left_join(occurrence.nm, rii)

# Add abbreviated site names and coordinate columns
occurrence.nm$site_code <- NA_character_

occurrence.nm <- occurrence.nm %>%
  mutate(site_code = ifelse(site_name == "North Beach", "NB",
                            ifelse(site_name == "West Beach", "WB", "FB")))

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

# Add other missing columns
occurrence.nm$basisOfRecord <- 'HumanObservation'
occurrence.nm$occurrenceStatus <- 'present'
occurrence.nm$tag <- 'counted individuals'
occurrence.nm$size <- NA_real_

# Select required columns and change names
occurrence.nm <- occurrence.nm %>% 
  select(eventID, scientific_name, rank, LSID, common_name, vitality, count, 
         basisOfRecord, occurrenceStatus, notes, tag, size)

names(occurrence.nm) <- c('eventID', 'scientificName', 'taxonRank', 
                          'scientificNameID', 'vernacularName', 'vitality',
                          'individualCount', 'basisOfRecord', 
                          'occurrenceStatus', 'occurrenceRemarks', 'tag', 
                          'size')
                         
# Littorine occurrences--------------------------------------------------------
occurrence.l <- occurrence %>%         # split off littorine observations
  subset(count_type == 'Littorines')

# Join with interval data
occurrence.l <- left_join(occurrence.l, rii)

# Add abbreviated site names and coordinate columns
occurrence.l$site_code <- NA_character_

occurrence.l <- occurrence.l %>%
  mutate(site_code = ifelse(site_name == "North Beach", "NB",
                            ifelse(site_name == "West Beach", "WB", "FB")))

# Add plot type code column
occurrence.l$type.code <- substring(occurrence.l$plot_type, 1, 1)

# Build quadrat code column
occurrence.l$quad.code <- paste(occurrence.l$type.code, 'Q', 
                                 occurrence.l$plot, sep = '')

# Build sub-quadrat code column
occurrence.l$subquad.code <- NA_character_

occurrence.l$subquad.code <- ifelse(occurrence.l$subplot_location == 'top left (1)', '1',
                                    ifelse(occurrence.l$subplot_location == 'middle (2)', 
                                           '2', '3'))

# Build event ID column to match with event table
occurrence.l$eventID <- paste(occurrence.l$interval, 
                              occurrence.l$site_code,
                              occurrence.l$date,
                              occurrence.l$quad.code,
                              occurrence.l$subquad.code,
                              sep = '_')
                               
# Add other missing columns
occurrence.l$basisOfRecord <- 'HumanObservation'
occurrence.l$occurrenceStatus <- 'present'
occurrence.l$tag <- 'counted individuals'
occurrence.l$size <- NA_real_

# Select required columns and change names
occurrence.l <- occurrence.l %>% 
  select(eventID, scientific_name, rank, LSID, common_name, vitality, count, 
         basisOfRecord, occurrenceStatus, notes, tag, size)

names(occurrence.l) <- c('eventID', 'scientificName', 'taxonRank',
                         'scientificNameID', 'vernacularName', 'vitality',
                         'individualCount', 'basisOfRecord', 
                         'occurrenceStatus', 'occurrenceRemarks', 'tag', 
                         'size')
                          
# Join biological occurrences--------------------------------------------------
authority <- read_csv('./obis/authority.csv')

# Join life observations and add missing columns
occurrence <- rbind(occurrence.nm, occurrence.m, occurrence.l)

occurrence$occurrenceID <- paste(occurrence$eventID,
                                 rownames(occurrence),
                                 sep = '_')
                                       
occurrence <- left_join(occurrence, authority,
                        by = c('scientificName' = 'scientific_name'))

# Select columns
occurrence <- occurrence %>% 
  select(eventID, occurrenceID, scientific_name_full, scientificNameID, 
         taxonRank, vernacularName, vitality, individualCount, basisOfRecord, 
         occurrenceStatus, occurrenceRemarks, tag, size)
 
# Change authority column name
names(occurrence)[names(occurrence) == 'scientific_name_full'] <- 'scientificName'

# Make tagged version of occurrence
occurrence.tag <- occurrence

occurrence <- occurrence %>% select(-c(tag, size))

# Save occurrence file
write_csv(occurrence, file = '/obis/obis_outputs/occurrence.csv')

#================== Measurement or Fact Extension =============================
# CMECS descriptors------------------------------------------------------------
# Copy survey data and find distinct quadrats
mof.cmec <- mi %>% 
  distinct(date, site_name, plot_type, plot)

mof.cmec$site_code <- ifelse(mof.cmec$site_name == "North Beach", "NB",
                                    ifelse(mof.cmec$site_name == "West Beach", 
                                           "WB", "FB"))
# Add plot type code column
mof.cmec$type.code <- substring(mof.cmec$plot_type, 1, 1)

# Build quadrat code column
mof.cmec$quad.code <- paste(mof.cmec$type.code, 'Q', 
                                   mof.cmec$plot, 
                                   sep = '')

# Join to interval data
mof.cmec <- left_join(mof.cmec, rii)

# Add eventID column
mof.cmec$eventID <- paste(mof.cmec$interval,
                          mof.cmec$site_code,
                          mof.cmec$date,
                          sep = '_')
                                 

# CMECS Tidal Zone ------------------------------------------------------------
cmecs.tidal <- mof.cmec

# Add measurement columns
cmecs.tidal$occurrenceID <- NA_character_
cmecs.tidal$measurementID <- paste(mof.cmec$eventID, 'cmecs-tidal', sep = '_')
cmecs.tidal$measurementMethod <- 'https://www.fgdc.gov/standards/projects/cmecs-folder/CMECS_Version_06-2012_FINAL.pdf'
cmecs.tidal$measurementType <- 'tidal zone as defined by CMECS'
cmecs.tidal$measurementTypeID <- 'https://w3id.org/CMECS/CMECS_00000034'
cmecs.tidal$measurementValue <- 'Marine Nearshore Intertidal'
cmecs.tidal$measurementValueID <- 'https://w3id.org/CMECS/CMECS_00000495'
cmecs.tidal$measurementUnit <- 'not applicable'
cmecs.tidal$measurementUnitID <- 'https://vocab.nerc.ac.uk/collection/P06/current/XXXX/'

# CMECS Substrate -------------------------------------------------------------
cmecs.sub <- mof.cmec

# Add measurement columns
cmecs.sub$occurrenceID <- NA_character_
cmecs.sub$measurementID <- paste(mof.cmec$eventID, 'cmecs-substrate', sep = '_')
cmecs.sub$measurementMethod <- 'https://www.fgdc.gov/standards/projects/cmecs-folder/CMECS_Version_06-2012_FINAL.pdf'
cmecs.sub$measurementType <- 'substrate component as defined by CMECS'
cmecs.sub$measurementTypeID <- 'https://w3id.org/CMECS/CMECS_00000803'
cmecs.sub$measurementValue <- 'Bedrock'
cmecs.sub$measurementValueID <- 'https://w3id.org/CMECS/CMECS_00000094'
cmecs.sub$measurementUnit <- 'not applicable'
cmecs.sub$measurementUnitID <- 'https://vocab.nerc.ac.uk/collection/P06/current/XXXX/'

# CMECS Geoform ---------------------------------------------------------------
cmecs.geo <- mof.cmec

# Add measurement columns
cmecs.geo$occurrenceID <- NA_character_
cmecs.geo$measurementID <- paste(mof.cmec$eventID, 'cmecs-geoform', sep = '_')
cmecs.geo$measurementMethod <- 'https://www.fgdc.gov/standards/projects/cmecs-folder/CMECS_Version_06-2012_FINAL.pdf'
cmecs.geo$measurementType <- 'Geoform Type as defined by CMECS'
cmecs.geo$measurementTypeID <- 'https://w3id.org/CMECS/CMECS_00000387'
cmecs.geo$measurementValue <- 'Tide-dominated beach'
cmecs.geo$measurementValueID <- 'https://w3id.org/CMECS/CMECS_00000837 '
cmecs.geo$measurementUnit <- 'not applicable'
cmecs.geo$measurementUnitID <- 'https://vocab.nerc.ac.uk/collection/P06/current/XXXX/'

# Bind CMECS back together
mof.cmec <- rbind(cmecs.tidal, cmecs.geo, cmecs.sub)

# Select required columns and change names
mof.cmec <- mof.cmec %>% 
  select(eventID, occurrenceID, measurementID, measurementMethod,
         measurementType, measurementTypeID, measurementUnit,
         measurementUnitID, measurementValue, measurementValueID)

# remove duplicated rows
mof.cmec <- mof.cmec %>% distinct()

# Size Measurements------------------------------------------------------------
mof.s <- occurrence.tag %>% subset(tag == 'measured individuals')

# Add measurement id to size measurement table
mof.s$measurementID <- paste(mof.s$occurrenceID, "length", sep = '_')

# Select relevant columns
mof.s <- mof.s %>% 
  select(eventID, occurrenceID, measurementID, size)

# Add missing columns
mof.s$measurementType <- 'Length'
mof.s$measurementMethod <- 'Calipers to nearest mm'
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
ysi.l$measurementMethod <- 'YSI'
ysi.l$measurementTypeID <- NA_character_
ysi.l$measurementUnit <- ifelse(ysi.l$measurementType == 'sea_surface_temperature',
                                'degree celsius', 'practical salinity scale')
ysi.l$measurementUnitID <- ifelse(ysi.l$measurementType == 'sea_surface_temperature',
                                  'https://vocab.nerc.ac.uk/collection/P06/current/UPAA/',
                                  'https://vocab.nerc.ac.uk/collection/A05/current/EV_SALIN/')
ysi.l$measurementValueID <- NA_character_

# Rearrange columns
ysi.l <- ysi.l %>% 
  select(eventID, occurrenceID, measurementID, measurementMethod,
         measurementType, measurementTypeID, measurementUnit, 
         measurementUnitID, measurementValue, measurementValueID)

# Join measurements together
mof <- rbind(mof.cmec, mof.s, ysi.l)

# Save measurement file
write_csv(mof, file = '/obis/obis_outputs/eMoF.csv')
