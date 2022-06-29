# Alberto Rovellini
# 6/24/2022
# Produce plots of water transport

# List of packages for session
.packages = c("dplyr", "ggplot2", "purrr", "tidync", "sf", "ggh4x", "rbgm", "cowplot", "maps", "mapdata")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session
lapply(.packages, require, character.only=TRUE)

select <- dplyr::select

exchange.file <- paste0("../../outputs/complete_from_Emily/Script1/all/hydro/goa_hydro_1995-2004.nc")
bgm.file <- "../../data/atlantis/GOA_WGS84_V4_final.bgm" 
cum.depth <- c(1,30,100,200,500,1000,3969)

# make directory for plots
dir.create('flux_plots_1995-2004')

# Atlantis model spatial domain
atlantis_bgm <- bgm.file %>% read_bgm()
atlantis_box <- atlantis_bgm %>% 
  box_sf() %>%
  st_transform(crs = 4326) %>%
  mutate(botz = -1*botz)

# Get exchanges from NetCDF file ------------------------------------------
exchange <- tidync(exchange.file) 

these_vars <- hyper_grids(exchange) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    exchange %>% activate(x) %>% hyper_vars() %>% 
      mutate(grd=x)
  })

grids <- these_vars %>% filter(name=="dest_b") %>% pluck('grd')

dat <- exchange %>% activate(grids) %>% hyper_tibble()

# numbering of b and z starts from 1 - change
dat <- dat %>% 
  mutate(b = b - 1, z = z - 1) %>%
  select(-dest) # remove dest, not needed

# here 0 is the bottom

# Loop for all boxes ------------------------------------------------------
all_boxes <- atlantis_box %>% pull(box_id) 

make_fluxplot <- function(source_b){
  
  # get neighbors
  neighbors <- atlantis_bgm$facesXboxes %>%
    filter(.bx0 == source_b) %>%
    pull(ibox) %>%
    unique()
  
  dat1 <- dat %>% 
    filter(b == source_b) %>%
    drop_na() # this step drops some neighbors - fluxes only show up once, but we want to view all of them for any one box
  
  # missing neighbors
  missing_neighbors <- setdiff(neighbors, unique(dat1$dest_b))
  
  dat2 <- dat %>% filter(b %in% missing_neighbors, dest_b == source_b) %>%
    rename(b = dest_b, z = dest_k, dest_b = b, dest_k = z) %>%
    mutate(exchange = -1*exchange) %>%
    select(exchange, dest_b, dest_k, z, b, t) %>%
    drop_na()
  
  dat3 <- rbind(dat1, dat2)
  
  # flip layers so that 0 becomes the surface, for visualisation purposes
  dat3 <- dat3 %>%
    group_by(t,b) %>%
    mutate(z_flip = max(z)-z) %>%
    group_by(dest_b) %>%
    mutate(dest_k_flip = max(dest_k)-dest_k)
  
  # Plot exchanges ----------------------------------------------------------
  p1 <- dat3 %>%
    mutate(dest = paste0(dest_b, ':', dest_k_flip),
           t = as.POSIXct(t, origin = '1995-01-01', tz = 'UTC')) %>%
    rowwise() %>%
    mutate(sign = ifelse(exchange > 0, 'Outgoing from focal', 'Incoming into focal')) %>%
    ungroup() %>% 
    ggplot(aes(x = t, y = exchange, fill = factor(sign)))+
    geom_bar(stat='identity')+
    scale_fill_manual(values = c('red','blue'))+
    geom_hline(yintercept = 0, color = 'darkgrey', size = .8)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x = 'Time', 
         y = expression(paste('Flux (', m^3, '/ time step)')), 
         fill = 'Direction',
         title = paste('Fluxes from focal box', source_b, '(0 denotes surface)'))+
    ggh4x::facet_grid2(dest ~ z_flip, scales = "free_y", independent = "y")
  
  
  # Plot box and neighbors --------------------------------------------------
  
  this_bbox <- st_bbox(atlantis_box %>% filter(box_id == source_b))
  # add a small buffer around it?
  this_bbox[1] <- this_bbox[1] - 0.1 #xmin
  this_bbox[2] <- this_bbox[2] - 0.1 #ymin
  this_bbox[3] <- this_bbox[3] + 0.1 #xmax
  this_bbox[4] <- this_bbox[4] + 0.1 #ymax
  
  this_atlantis_box <- atlantis_box %>% 
    st_crop(this_bbox)%>% 
    mutate(lyrs = findInterval(botz, cum.depth)) 
  
  cc <- scales::seq_gradient_pal("yellow", "blue", "Lab")(seq(0,1,length.out = (max(this_atlantis_box$lyrs)+1)))
  
  coast <- maps::map("worldHires", regions = c("Canada", "USA"), plot = FALSE, fill = TRUE)
  coast_sf <- coast %>% st_as_sf() %>% st_transform(crs = 4326) %>% st_combine()
  
  p2 <- this_atlantis_box %>%
    ggplot()+
    geom_sf(aes(fill = factor(lyrs)), color = 'white')+
    scale_fill_manual(values = cc) +
    geom_sf(data=coast_sf)+
    coord_sf(xlim = c(this_bbox$xmin,
                      this_bbox$xmax),
             ylim = c(this_bbox$ymin,
                      this_bbox$ymax))+
    geom_sf_label(aes(label = box_id))+
    labs(x = '', y = '', fill = 'Depth layers')+
    theme_bw()
  
  # Produce a graphic output ------------------------------------------------
  
  p3 <- plot_grid(p2, p1, ncol = 1, nrow = 2, rel_heights = c(0.3,1))
  save_plot(paste0('flux_plots_1995-2004/focal_box_', source_b, '.png'), p3, base_height = 20, base_width = 12)
  
}

purrr::map(all_boxes, possibly(make_fluxplot,NA))
