# Header ----------------------------------------------------------------
# Project: SAmMammals
# File name: 02_prepare_occ.R
# Last updated: 2023-11-13
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/SAmMammals

# Load libraries --------------------------------------------------------
library(palaeoverse)
library(CoordinateCleaner)
library(httr)
library(tidyverse)

# Load data -------------------------------------------------------------
# Download data from the PBDB using API
occdf <- read.csv(paste0("https://paleobiodb.org/data1.2/occs/list.csv?",
                         "base_name=Eutheria&interval=Paleogene,Pleistocene",
                         "&cc=SOA&pgm=gplates,scotese,seton&envtype=terr",
                         "&show=full,genus,acconly"))
# Save locally for a permanent record
write.csv(occdf, paste0("./data-raw/pbdb_data_",
                        gsub("-", "", Sys.Date()),
                        ".csv"))

# Prepare data ----------------------------------------------------------
# Remove basin scale data and non-determined
occdf <- occdf %>% filter(!geogscale %in% c("", "basin"))
# Retain body fossils only
occdf <- occdf[str_detect(occdf$pres_mode, "body"), ]
# Remove bats
occdf <- occdf %>% filter(order != "Chiroptera")
# Set subspecies and subgenus names to species and genus names
# Subspecies to species
subspecies <- occdf$accepted_name[which(occdf$accepted_rank == "subspecies")]
subspecies <- str_split_fixed(subspecies, " ", 3)
subspecies <- paste(subspecies[, 1], subspecies[, 2])
# Subgenera to genera
subgenus <- occdf$accepted_name[which(occdf$accepted_rank == "subgenus")]
subgenus <- str_split_i(subgenus, " ", 1)
# Update names/rank
occdf$accepted_name[which(occdf$accepted_rank == "subspecies")] <-
  subspecies
occdf$accepted_rank[which(occdf$accepted_rank == "subspecies")] <- "species"
occdf$accepted_name[which(occdf$accepted_rank == "subgenus")] <-
  subgenus
occdf$accepted_rank[which(occdf$accepted_rank == "subgenus")] <- "genus"

## Handle age data ------------------------------------------------------
# Add late interval data if empty (equal to early interval)
occdf$late_interval[which(occdf$late_interval == "")] <-
  occdf$early_interval[which(occdf$late_interval == "")]

### Update SALMA ages ---------------------------------------------------
# South American Land Mammal Ages (SALMA) are out-of-date in the PBDB and
# require updating.
# Load updated ages
salma <- read.csv("./data/SALMA.csv")
pbdb_early_salma <- salma[match(x = occdf$early_interval,
                                table = salma$salma_pbdb), ]
pbdb_late_salma <- salma[match(x = occdf$late_interval,
                               table = salma$salma_pbdb), ]
# Update early intervals
occdf$early_interval[which(!is.na(pbdb_early_salma$salma_pbdb))] <-
  pbdb_early_salma$salma_updated[which(!is.na(pbdb_early_salma$salma_pbdb))]
# Update max ma
occdf$max_ma[which(!is.na(pbdb_early_salma$salma_pbdb))] <-
  pbdb_early_salma$max_ma[which(!is.na(pbdb_early_salma$max_ma))]
# Update late intervals
occdf$late_interval[which(!is.na(pbdb_late_salma$salma_pbdb))] <-
  pbdb_late_salma$salma_updated[which(!is.na(pbdb_late_salma$salma_pbdb))]
# Update min ma
occdf$min_ma[which(!is.na(pbdb_late_salma$salma_pbdb))] <-
  pbdb_late_salma$min_ma[which(!is.na(pbdb_late_salma$min_ma))]

# Check coordinates -----------------------------------------------------
# Which samples are flagged?
flag <- clean_fossils(x = occdf, lon = "lng", lat = "lat",
                      min_age = "min_ma", max_age = "max_ma",
                      taxon = "accepted_name", value = "flagged")
# Remove flagged occurrences
occdf <- occdf[flag, ]

# Temporal binning ------------------------------------------------------


# Palaeorotations -------------------------------------------------------


# Spatial binning -------------------------------------------------------


