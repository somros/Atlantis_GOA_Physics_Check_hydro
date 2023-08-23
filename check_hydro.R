# Alberto Rovellini

# List of packages for session
.packages = c("devtools","dtplyr","stringi","data.table","tidyverse","stringr","R.utils","ReactiveAtlantis","shinyrAtlantis","rbgm")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session
lapply(.packages, require, character.only=TRUE)

# install_github('Atlantis-Ecosystem-Model/ReactiveAtlantis', force=TRUE, dependencies=TRUE)
# devtools::install_github("shanearichards/shinyrAtlantis")

# Load packages into session
lapply(.packages, require, character.only=TRUE)

select <- dplyr::select

#check files using shinyrAtlantis

# monthyear <- '1_2017'

this_year <- 2017

# salinity.file    <- paste0("../../outputs/complete_from_Emily/Script1/",this_year,"/monthly/forcings/salt/goa_salt_",this_year,".nc")      
# temperature.file <- paste0("../../outputs/complete_from_Emily/Script1/",this_year,"/monthly/forcings/temp/goa_temp_",this_year,".nc")
# exchange.file <- paste0("../../outputs/complete_from_Emily/Script1/",this_year,"/monthly/forcings/hydro/goa_hydro_",this_year,".nc")
salinity.file    <- 'check_08232023/goa_roms_salt_1999.nc'      
temperature.file <- 'check_08232023/goa_roms_temp_1999.nc'
exchange.file <- 'check_08232023/goa_hydro_1999.nc'

bgm.file         <- "../../data/atlantis/GOA_WGS84_V4_final.bgm" 
#cum.depth <- c(0,30,70,100,300,500,2969)  # cumulative water layer depths
cum.depth <- c(0,30,100,200,500,1000,3969)

input.object <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)

sh.forcings(input.object)


# Checking that final fluxes make sense -----------------------------------

# Proceed as follows:
# Choose 1 random exchange from Shane's viewer
# Work out time step, layer, source, and destination boxes for the exchange
# Open BGM and write list of faces for the exchange (the faces between source and destination box)
# open pre-interp files and extract flows across the relevant faces
# add them up, divide by area of the DESTINATION box from point 2 (I think?), multiply by 43200
# you should get the same value as the start
# Repeat 3 times

# the problem here is that we interpolate time steps in script 2, so maybe post-interp data would work better

# start simple
source_b <- 34
dest_b <- 36
source_k <- 0 # THIS IS SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
dest_k <- 0 # THIS IS SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
exchange <- -0.994379985862203 # this is the target
tstep <- 1

# read bgm
atlantis_bgm <- read_bgm(bgm.file)
# faces of interest
faces <- atlantis_bgm$faces %>% filter(left %in% c(source_b, dest_b), right %in% c(source_b, dest_b)) %>% pull(.fx0)
# dest area
dest_area <- atlantis_bgm$boxes %>% filter(.bx0==dest_b) %>% pull(area)

# read in depth reconstruction information - a now useless twirl since hydroconstruct is not used
# number these from the surface down for ease here
depth_key <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/depth_layer.csv")

depth_key1 <- depth_key %>% 
  mutate(across(everything(), ~str_replace(.,'_','NA')),
         across(where(is.character), as.numeric)) %>%
  pivot_longer(-box_id, names_to = 'layer', values_to = 'depth_m') %>%
  mutate(lyr = substr(layer, nchar(layer), nchar(layer))) %>%
  select(-layer) %>%
  filter(lyr!=6) %>%
  arrange(box_id, desc(lyr)) %>%
  drop_na() %>%
  group_by(box_id) %>%
  mutate(cumdepth = cumsum(depth_m)) %>%
  slice_min(cumdepth) %>% # this takes surface - shallowest layer
  ungroup() %>%
  filter(box_id %in% c(source_b, dest_b)) %>%
  mutate(lyr = as.numeric(lyr))

# read pre-interp file
fluxes <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/monthly/post-interp/1_2017_flux.dat",sep='\t',header = T)
# subset to our fluxes
these_fluxes <- fluxes %>% 
  filter(FaceID %in% faces, Time.Step..12.hr==unique(fluxes$Time.Step..12.hr)[tstep]) %>%
  left_join(depth_key1, by = c('Polygon.number'='box_id', 'Depth.Layer'='lyr')) %>%
  drop_na() %>%
  pull(Flux..m3.s.)

these_fluxes1 <- these_fluxes[1:3]

# add up, divide by area of dest, multiply by 43200
sum(these_fluxes1) / dest_area * 60*60*12 - (-exchange)

# same as target! And the sign should get flipped later (script 3) so this should make sense

# once more
source_b <- 71
dest_b <- 74
source_k <- 2 # THIS IS FROM SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
dest_k <- 2 # THIS IS FROM SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
exchange <- -4.88586275385411 # this is the target
tstep <- 10

# read bgm
atlantis_bgm <- read_bgm(bgm.file)
# faces of interest
faces <- atlantis_bgm$faces %>% filter(left %in% c(source_b, dest_b), right %in% c(source_b, dest_b)) %>% pull(.fx0)
# dest area
dest_area <- atlantis_bgm$boxes %>% filter(.bx0==dest_b) %>% pull(area)

# read in depth reconstruction information - a now useless twirl since hydroconstruct is not used
# number these from the surface down for ease here
depth_key <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/depth_layer.csv")

depth_key1 <- depth_key %>% 
  mutate(across(everything(), ~str_replace(.,'_','NA')),
         across(where(is.character), as.numeric)) %>%
  pivot_longer(-box_id, names_to = 'layer', values_to = 'depth_m') %>%
  mutate(lyr = substr(layer, nchar(layer), nchar(layer))) %>%
  select(-layer) %>%
  filter(lyr!=6) %>%
  arrange(box_id, desc(lyr)) %>%
  drop_na() %>%
  group_by(box_id) %>%
  mutate(cumdepth = cumsum(depth_m)) %>%
  filter(cumdepth==100) %>% # this takes surface - shallowest layer
  ungroup() %>%
  filter(box_id %in% c(source_b, dest_b)) %>%
  mutate(lyr = as.numeric(lyr))

# read pre-interp file
fluxes <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/monthly/post-interp/1_2017_flux.dat",sep='\t',header = T)
# subset to our fluxes
these_fluxes <- fluxes %>% 
  filter(FaceID %in% faces, Time.Step..12.hr==unique(fluxes$Time.Step..12.hr)[tstep]) %>%
  left_join(depth_key1, by = c('Polygon.number'='box_id', 'Depth.Layer'='lyr')) %>%
  drop_na() %>%
  pull(Flux..m3.s.)

these_fluxes1 <- these_fluxes[1:(length(these_fluxes)/2)]

# add up, divide by area of dest, multiply by 43200
sum(these_fluxes1) / dest_area * 60*60*12 - (-exchange)

# one last time

source_b <- 102
dest_b <- 103
source_k <- 2 # THIS IS FROM SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
dest_k <- 2 # THIS IS FROM SURFACE AND IT WILL BE A DIFFERENT LAYER IN DAT
exchange <- -12.9424348981901 # this is the target
tstep <- 50

# read bgm
atlantis_bgm <- read_bgm(bgm.file)
# faces of interest
faces <- atlantis_bgm$faces %>% filter(left %in% c(source_b, dest_b), right %in% c(source_b, dest_b)) %>% pull(.fx0)
# dest area
dest_area <- atlantis_bgm$boxes %>% filter(.bx0==dest_b) %>% pull(area)

# read in depth reconstruction information - a now useless twirl since hydroconstruct is not used
# number these from the surface down for ease here
depth_key <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/depth_layer.csv")

depth_key1 <- depth_key %>% 
  mutate(across(everything(), ~str_replace(.,'_','NA')),
         across(where(is.character), as.numeric)) %>%
  pivot_longer(-box_id, names_to = 'layer', values_to = 'depth_m') %>%
  mutate(lyr = substr(layer, nchar(layer), nchar(layer))) %>%
  select(-layer) %>%
  filter(lyr!=6) %>%
  arrange(box_id, desc(lyr)) %>%
  drop_na() %>%
  group_by(box_id) %>%
  mutate(cumdepth = cumsum(depth_m)) %>%
  filter(cumdepth==100) %>% # this takes surface - shallowest layer
  ungroup() %>%
  filter(box_id %in% c(source_b, dest_b)) %>%
  mutate(lyr = as.numeric(lyr))

# read pre-interp file
fluxes <- read.csv("C:/Users/Alberto Rovellini/Documents/GOA/ROMS/outputs/2017/monthly/post-interp/1_2017_flux.dat",sep='\t',header = T)
# subset to our fluxes
these_fluxes <- fluxes %>% 
  filter(FaceID %in% faces, Time.Step..12.hr==unique(fluxes$Time.Step..12.hr)[tstep]) %>%
  left_join(depth_key1, by = c('Polygon.number'='box_id', 'Depth.Layer'='lyr')) %>%
  drop_na() %>%
  pull(Flux..m3.s.)

these_fluxes1 <- these_fluxes[1:(length(these_fluxes)/2)]

# add up, divide by area of dest, multiply by 43200
sum(these_fluxes1) / dest_area * 60*60*12 - (-exchange)

# Good
