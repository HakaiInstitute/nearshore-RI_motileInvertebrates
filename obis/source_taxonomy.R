#================== Introduction ==============================================
# This script is meant to download and reformat Rocky Intertidal taxonomic data
# for the purposes of summarization in separate scripts and reports.
#
# Outputs: 
# ri.codes - All Rocky Intertidal taxonomic codes
# ns.taxa - Full taxonomic classification for all observed taxa
# ns.taxa.code <- Concatenated taxa codes for various uses
#
#================== Taxon Data ================================================
# Classifications--------------------------------------------------------------
ns.taxa <- read_csv('obis/nearshore_taxa.csv',
                    col_types = 'ccccncccccccccccccccccccccccccccccccccc')

# Nearshore Codes--------------------------------------------------------------
ri.codes <- read_csv('obis/ri_codes.csv')

# join taxa data to codes
ri.codes <- left_join(ri.codes, ns.taxa)

# Taxonomy Codes---------------------------------------------------------------
# Nearshore taxa codes
ns.taxa.codes <- distinct(ri.codes, scientific_name, description)

ns.taxa.codes$taxon_code <- paste(ns.taxa.codes$scientific_name, ' (', 
                                  ns.taxa.codes$description, ')')