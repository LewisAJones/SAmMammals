# Header ----------------------------------------------------------------
# Project: SAmMammals
# File name: 01_prepare_climate.R
# Last updated: 2023-07-03
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/SAmMammals

# Load libraries --------------------------------------------------------
library(palaeoverse)
library(terra)
library(ncdf4)

# Process data ----------------------------------------------------------

# Month temperature and precipitation
# Layer: pdcl
# Temperature: temp_mm_srf (units: kelvin)
# Precipitation: precip_mm_srf (units: kg/m^2/s)
# Outputs: Monthly

# Generate bins
bins <- time_bins(interval = "Cenozoic")
# Get available time slices
available <- as.numeric(sub(pattern = "Ma",
                            replacement = "",
                            x = list.files("./data-raw/")))
# Remove unrelated folders
available <- na.omit(available)
# Add model age column
bins$model_age <- NA
# Create folder
dir.create("./data/palaeoclimate/")
# Run across intervals
for (i in 1:nrow(bins)) {
  # Which time slices best fits data?
  val <- available[which.min(abs(bins$mid_ma[i] - available))]
  # Assign value
  bins$model_age[i] <- val
  # Get files
  files <- list.files(paste0("./data-raw/", val, "Ma", "/"),
                      full.names = TRUE)
  # Handle names
  names <- sub(pattern = ".*pdcl", replacement = "", x = files)
  names <- sub(pattern = "_180.nc", replacement = "", x = names)
  # Load precipitation data
  precip <- rast(files, subds = "precip_mm_srf")
  # Assign names
  names(precip) <- names
  # Convert from kg/m2/s to mm per day
  precip <- (precip * 86400)
  # Convert from mm per dat to mm per month
  precip <- precip * 30
  # Load temperature data
  temp <- rast(files, subds = "temp_mm_srf")
  names <- sub(pattern = ".*pdcl", replacement = "", x = files)
  names <- sub(pattern = "_180.nc", replacement = "", x = names)
  # Assign names
  names(temp) <- names
  # Convert kelvin to celsius
  temp <- temp - 273.15
  # Load mask
  msk <- rast(paste0("./data-raw/masks/mask_deg1/", val, "Ma.tiff"))
  # Set values for masking
  msk[msk > 0] <- 1
  msk[msk != 1] <- NA
  # Resample climate data and mask
  temp <- resample(x = temp, y = msk)
  temp <- mask(x = temp, mask = msk)
  precip <- resample(x = precip, y = msk)
  precip <- mask(x = precip, mask = msk)
  # Save data
  dir.create(paste0("./data/palaeoclimate/", bins$interval_name[i]))
  writeRaster(x = temp, filename = paste0("./data/palaeoclimate/",
                                          bins$interval_name[i], "/temp_",
                                          names(temp),
                                          ".tiff"),
              overwrite = TRUE)
  writeRaster(x = precip, filename = paste0("./data/palaeoclimate/",
                                          bins$interval_name[i], "/precip_",
                                          names(precip),
                                          ".tiff"),
              overwrite = TRUE)
}
# Save bins
write.csv(x = bins, "./data/intervals.csv")
