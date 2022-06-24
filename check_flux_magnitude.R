# 3/31/2022
# Alberto Rovellini

# opening Hydro file for GOA and for CalCurrent and comparing the magnitude of the fluxes per time step
# Try to do this relative to the box area and / or face length
# Will need depth breaks from CalCurrent

library(rbgm)
library(ncdf4)
library(tidyverse)
library(sf)

select <- dplyr::select

# read hydro data
goa_hydro <- nc_open('C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/monthly/forcings/hydro/goa_hydro_2017.nc')
# cc_hydro <- nc_open()

goa_bgm <- read_bgm('C:/Users/Alberto Rovellini/Documents/GOA/ROMS/data/atlantis/GOA_WGS84_V4_final.bgm')
goa_box <- goa_bgm %>% box_sf()
goa_face <- goa_bgm$faces # remember that with face_sf() the face indices come out jumbled

exchange <- ncvar_get(goa_hydro, 'exchange')
dist_b <- ncvar_get(goa_hydro, 'dest_b')
dist_k <- ncvar_get(goa_hydro, 'dest_k')

max(exchange, na.rm = T)
