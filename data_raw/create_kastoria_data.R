
# Load necessary packages
library(sf)
library(dplyr)
library(units)
library(devtools) # For use_data

devtools::load_all()


# Read and prepare the data using the read_data function
kastoria <- read_data(shp_path = "./data_raw/cad1925.shp",
                      gcp_path = "./data_raw/homologous.csv",
                      crs = 2100)

# Save the 'kastoria' object as internal package data
use_data(kastoria, overwrite = TRUE)

message("kastoria.rdata created successfully in the data/ directory.")
