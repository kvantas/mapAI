
# Load necessary packages
library(sf)
library(dplyr)
library(units)
library(devtools) # For use_data

devtools::load_all()


# Read and prepare the data using the read_data function
gcps <- read_gcps(gcp_path = "./data_raw/homologous.csv", crs = 2100)
parcels <- read_map(shp_path = "./data_raw/cad1925.shp",
                      crs = 2100)

# Save the objects as internal package data
use_data(parcels, overwrite = TRUE)
use_data(gcps, overwrite = TRUE)

message("data were created successfully in the data/ directory.")
