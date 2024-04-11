#================== Introduction ==============================================
# This script is meant to download and reformat Rocky Intertidal taxonomic data
# for the purposes of summarization in separate scripts and reports.
#
# Outputs: 
# ri.codes - All Rocky Intertidal taxonomic codes
# ns.taxa.full - Full taxonomic classification for all observed RI taxa
#
#================== Taxon Data ================================================
# Classifications--------------------------------------------------------------
table <- airtable('Taxa', 'appoo8TNz4FSn4260')

ns.taxa <- read_airtable(table, id_to_col = TRUE, max_rows = 50000) %>%
  select(airtable_record_id, scientific_name, common_name, rank, kingdom, 
         subkingdom, infrakingdom, phylum, subphylum, infraphylum, superclass, 
         class, subclass, infraclass, subterclass, superorder, order, suborder, 
         infraorder, section, subsection, superfamily, family, subfamily, genus, 
         species)

colnames(ns.taxa)[colnames(ns.taxa) == 'airtable_record_id'] <- 'taxonID'

# Codes------------------------------------------------------------------------
table <- airtable('Codes', 'appoo8TNz4FSn4260')

ns.codes <- read_airtable(table, id_to_col = TRUE, max_rows = 50000)

# Projects---------------------------------------------------------------------
table <- airtable('Projects', 'appoo8TNz4FSn4260')

ns.projects <- read_airtable(table, id_to_col = TRUE, max_rows = 50000)

# Subset Rocky Intertidal taxa*************************************************

# join project data to codes
colnames(ns.projects)[colnames(ns.projects) == 'airtable_record_id'] <- 'project'
ns.codes$project <- unlist(ns.codes$project) #unlist projects column
ns.codes <- inner_join(ns.codes, ns.projects, by = 'project')

# Build full taxonomic classification data frame*******************************
ns.taxa$kingdom[sapply(ns.taxa$kingdom, is.null)] <- NA
ns.taxa$kingdom <- unlist(ns.taxa$kingdom)
ns.taxa$subkingdom[sapply(ns.taxa$subkingdom, is.null)] <- NA
ns.taxa$subkingdom <- unlist(ns.taxa$subkingdom)
ns.taxa$infrakingdom[sapply(ns.taxa$infrakingdom, is.null)] <- NA
ns.taxa$infrakingdom <- unlist(ns.taxa$infrakingdom)
ns.taxa$phylum[sapply(ns.taxa$phylum, is.null)] <- NA
ns.taxa$phylum <- unlist(ns.taxa$phylum)
ns.taxa$subphylum[sapply(ns.taxa$subphylum, is.null)] <- NA
ns.taxa$subphylum <- unlist(ns.taxa$subphylum)
ns.taxa$infraphylum[sapply(ns.taxa$infraphylum, is.null)] <- NA
ns.taxa$infraphylum <- unlist(ns.taxa$infraphylum)
ns.taxa$superclass[sapply(ns.taxa$superclass, is.null)] <- NA
ns.taxa$superclass <- unlist(ns.taxa$superclass)
ns.taxa$class[sapply(ns.taxa$class, is.null)] <- NA
ns.taxa$class <- unlist(ns.taxa$class)
ns.taxa$subclass[sapply(ns.taxa$subclass, is.null)] <- NA
ns.taxa$subclass <- unlist(ns.taxa$subclass)
ns.taxa$infraclass[sapply(ns.taxa$infraclass, is.null)] <- NA
ns.taxa$infraclass <- unlist(ns.taxa$infraclass)
ns.taxa$subterclass[sapply(ns.taxa$subterclass, is.null)] <- NA
ns.taxa$subterclass <- unlist(ns.taxa$subterclass)
ns.taxa$superorder[sapply(ns.taxa$superorder, is.null)] <- NA
ns.taxa$superorder <- unlist(ns.taxa$superorder)
ns.taxa$order[sapply(ns.taxa$order, is.null)] <- NA
ns.taxa$order <- unlist(ns.taxa$order)
ns.taxa$suborder[sapply(ns.taxa$suborder, is.null)] <- NA
ns.taxa$suborder <- unlist(ns.taxa$suborder)
ns.taxa$infraorder[sapply(ns.taxa$infraorder, is.null)] <- NA
ns.taxa$infraorder <- unlist(ns.taxa$infraorder)
ns.taxa$section[sapply(ns.taxa$section, is.null)] <- NA
ns.taxa$section <- unlist(ns.taxa$section)
ns.taxa$subsection[sapply(ns.taxa$subsection, is.null)] <- NA
ns.taxa$subsection <- unlist(ns.taxa$subsection)
ns.taxa$superfamily[sapply(ns.taxa$superfamily, is.null)] <- NA
ns.taxa$superfamily <- unlist(ns.taxa$superfamily)
ns.taxa$family[sapply(ns.taxa$family, is.null)] <- NA
ns.taxa$family <- unlist(ns.taxa$family)
ns.taxa$subfamily[sapply(ns.taxa$subfamily, is.null)] <- NA
ns.taxa$subfamily <- unlist(ns.taxa$subfamily)
ns.taxa$genus[sapply(ns.taxa$genus, is.null)] <- NA
ns.taxa$genus <- unlist(ns.taxa$genus)
ns.taxa$species[sapply(ns.taxa$species, is.null)] <- NA
ns.taxa$species <- unlist(ns.taxa$species)

# join taxa data to codes
ns.codes <- ns.codes %>% 
  select(-c(codes, project_status, parent))

ns.codes$scientific_name <- unlist(ns.codes$scientific_name)
ns.codes$taxon_ID <- unlist(ns.codes$taxon_ID)
ns.codes$taxonomic_rank[sapply(ns.codes$taxonomic_rank, is.null)] <- NA
ns.codes$taxonomic_rank <- unlist(ns.codes$taxonomic_rank)
ns.codes$kingdom[sapply(ns.codes$kingdom, is.null)] <- NA
ns.codes$kingdom <- unlist(ns.codes$kingdom)
ns.codes$phylum[sapply(ns.codes$phylum, is.null)] <- NA
ns.codes$phylum <- unlist(ns.codes$phylum)
ns.codes$class[sapply(ns.codes$class, is.null)] <- NA
ns.codes$class <- unlist(ns.codes$class)
ns.codes$order[sapply(ns.codes$order, is.null)] <- NA
ns.codes$order <- unlist(ns.codes$order)
ns.codes$family[sapply(ns.codes$family, is.null)] <- NA
ns.codes$family <- unlist(ns.codes$family)
ns.codes$genus[sapply(ns.codes$genus, is.null)] <- NA
ns.codes$genus <- unlist(ns.codes$genus)

colnames(ns.codes)[colnames(ns.codes) == 'scientific_name'] <- 'taxonID' #change name
colnames(ns.taxa)[colnames(ns.taxa) == 'taxon_ID'] <- 'taxonID' #change name

ns.codes <- ns.codes %>% 
  select(taxonID, name, taxon_code, taxonomic_rank, description)

ns.codes <- left_join(ns.codes, ns.taxa)

# subset RI codes from overall nearshore codes
ri.codes <- ns.codes %>% 
  subset(name == 'Rocky Intertidal') %>% 
  select(taxon_code, rank, scientific_name, description)

ns.taxa.full <- ns.taxa %>% 
  select(taxonID, scientific_name, common_name, rank)

ns.taxa.full$kingdom <- NA_character_
ns.taxa.full$subkingdom <- NA_character_
ns.taxa.full$infrakingdom <- NA_character_
ns.taxa.full$phylum <- NA_character_
ns.taxa.full$subphylum <- NA_character_
ns.taxa.full$infraphylum <- NA_character_
ns.taxa.full$superclass <- NA_character_
ns.taxa.full$class <- NA_character_
ns.taxa.full$subclass <- NA_character_
ns.taxa.full$infraclass <- NA_character_
ns.taxa.full$subterclass <- NA_character_
ns.taxa.full$superorder <- NA_character_
ns.taxa.full$order <- NA_character_
ns.taxa.full$suborder <- NA_character_
ns.taxa.full$infraorder <- NA_character_
ns.taxa.full$section <- NA_character_
ns.taxa.full$subsection <- NA_character_
ns.taxa.full$superfamily <- NA_character_
ns.taxa.full$family <- NA_character_
ns.taxa.full$subfamily <- NA_character_
ns.taxa.full$genus <- NA_character_
ns.taxa.full$species <- NA_character_

# match and replace for each column
ns.taxa.full$kingdom <- ns.taxa$scientific_name[match(ns.taxa$kingdom, ns.taxa.full$taxonID)]
ns.taxa.full$subkingdom <- ns.taxa$scientific_name[match(ns.taxa$subkingdom, ns.taxa.full$taxonID)]
ns.taxa.full$infrakingdom <- ns.taxa$scientific_name[match(ns.taxa$infrakingdom, ns.taxa.full$taxonID)]
ns.taxa.full$phylum <- ns.taxa$scientific_name[match(ns.taxa$phylum, ns.taxa.full$taxonID)]
ns.taxa.full$subphylum <- ns.taxa$scientific_name[match(ns.taxa$subphylum, ns.taxa.full$taxonID)]
ns.taxa.full$infraphylum <- ns.taxa$scientific_name[match(ns.taxa$infraphylum, ns.taxa.full$taxonID)]
ns.taxa.full$superclass <- ns.taxa$scientific_name[match(ns.taxa$superclass, ns.taxa.full$taxonID)]
ns.taxa.full$class <- ns.taxa$scientific_name[match(ns.taxa$class, ns.taxa.full$taxonID)]
ns.taxa.full$subclass <- ns.taxa$scientific_name[match(ns.taxa$subclass, ns.taxa.full$taxonID)]
ns.taxa.full$infraclass <- ns.taxa$scientific_name[match(ns.taxa$infraclass, ns.taxa.full$taxonID)]
ns.taxa.full$subterclass <- ns.taxa$scientific_name[match(ns.taxa$subterclass, ns.taxa.full$taxonID)]
ns.taxa.full$superorder <- ns.taxa$scientific_name[match(ns.taxa$superorder, ns.taxa.full$taxonID)]
ns.taxa.full$order <- ns.taxa$scientific_name[match(ns.taxa$order, ns.taxa.full$taxonID)]
ns.taxa.full$suborder <- ns.taxa$scientific_name[match(ns.taxa$suborder, ns.taxa.full$taxonID)]
ns.taxa.full$infraorder <- ns.taxa$scientific_name[match(ns.taxa$infraorder, ns.taxa.full$taxonID)]
ns.taxa.full$section <- ns.taxa$scientific_name[match(ns.taxa$section, ns.taxa.full$taxonID)]
ns.taxa.full$subsection <- ns.taxa$scientific_name[match(ns.taxa$subsection, ns.taxa.full$taxonID)]
ns.taxa.full$superfamily <- ns.taxa$scientific_name[match(ns.taxa$superfamily, ns.taxa.full$taxonID)]
ns.taxa.full$family <- ns.taxa$scientific_name[match(ns.taxa$family, ns.taxa.full$taxonID)]
ns.taxa.full$subfamily <- ns.taxa$scientific_name[match(ns.taxa$subfamily, ns.taxa.full$taxonID)]
ns.taxa.full$genus <- ns.taxa$scientific_name[match(ns.taxa$genus, ns.taxa.full$taxonID)]
ns.taxa.full$species <- ns.taxa$scientific_name[match(ns.taxa$species, ns.taxa.full$taxonID)]

for(i in 1:length(ns.taxa.full$taxonID)){
  if(is.na(ns.taxa.full$rank[i])){
    ns.taxa.full$species[i] <- NA
  } else if (ns.taxa.full$rank[i] == 'SPECIES'){
    ns.taxa.full$species[i] <- ns.taxa.full$scientific_name[i]
    }
  }

# remove unneeded data sets
rm(list = c('ns.codes', 'ns.projects', 'ns.taxa', 'i', 'table'))
   