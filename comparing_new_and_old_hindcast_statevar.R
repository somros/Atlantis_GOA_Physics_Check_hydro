# produce plots from Altantis runs (e.g. entire hindcast)
# doing this for hindcast comparison August 2023

.packages = c("dplyr", "ggplot2", "purrr", "tidyr", "tidync", "sf", "ggh4x", "rbgm", "cowplot", "maps", "mapdata")
lapply(.packages, require, character.only=TRUE)


# Geography: read bgm
fl <- 'data/GOA_WGS84_V4_final.bgm'
bgm <- read_bgm(fl)
goa_sf <- box_sf(bgm)
st_crs(goa_sf) <- st_crs(attr(goa_sf$geometry, "crs")$proj)
boundary_boxes <- goa_sf %>% filter(boundary == TRUE) %>% pull(box_id) # get boundary boxes

run_old <- 1277
run_new <- 1350

dir_old <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_',run_old,'/')
dir_new <- paste0('C:/Users/Alberto Rovellini/Documents/GOA/Parametrization/output_files/data/out_',run_new,'/')

# Plot time series of temperature -----------------------------

# netcdf output file from old run
out_fl_old <- paste0(dir_old, 'outputGOA0', run_old, '_test.nc')
out_fl_new <- paste0(dir_new, 'outputGOA0', run_new, '_test.nc')

out_old <- tidync(out_fl_old)
this_nc_old <- ncdf4::nc_open(out_fl_old)

out_new <- tidync(out_fl_new)
this_nc_new <- ncdf4::nc_open(out_fl_new)

# we want to get the areas to divide surface and bottom temperatures
volumes <- out_old %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="volume") %>% dplyr::select(-t)

# # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
areas <- volumes %>% filter(z==max(z)) %>% dplyr::select(b,volume) %>% rename(area=volume)

# get temp
temp_array_old <- ncdf4::ncvar_get("Temp", nc=this_nc_old)
temp_array_new <- ncdf4::ncvar_get("Temp", nc=this_nc_new)

# get dimensions (will be the same for old and new only if the runtime was identical)
# old
dim_lyr_old <- dim(temp_array_old)[1]
dim_box_old <- dim(temp_array_old)[2]
dim_t_old <- dim(temp_array_old)[3]

# new
dim_lyr_new <- dim(temp_array_new)[1]
dim_box_new <- dim(temp_array_new)[2]
dim_t_new <- dim(temp_array_new)[3]

# turn the array to a 2D data frame, add box and time dimensions, etc.
dim(temp_array_old) <- c(dim_lyr_old, dim_box_old * dim_t_old)
dim(temp_array_new) <- c(dim_lyr_new, dim_box_new * dim_t_new)

temp_df_old <- temp_array_old %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box_old, dim_t_old),
         ts = rep(1:dim_t_old, each = dim_box_old)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5, # change to years
         run = 'old') 

colnames(temp_df_old) <-   c(paste0('lyr',6:1), 'sed', 'box_id', 'ts', 'run')

temp_df_new <- temp_array_new %>%
  t() %>%
  data.frame() %>%
  mutate(box_id = rep(1:dim_box_new, dim_t_new),
         ts = rep(1:dim_t_new, each = dim_box_new)) %>%
  mutate(box_id = box_id - 1) %>%
  filter(ts > 1) %>% # here 1 is the initial conditions, which immdiately get replaced
  mutate(ts = ts - 1,
         ts = ts / 5, # change to years
         run = 'new') 

colnames(temp_df_new) <-   c(paste0('lyr',6:1), 'sed', 'box_id', 'ts', 'run')

# bind
temp_df <- rbind(temp_df_old, temp_df_new)

# plot time series bottom
p_ts_bottom <- temp_df %>%
  dplyr::select(ts,box_id,lyr1,sed, run) %>%
  pivot_longer(-c(ts,box_id,run), names_to = 'lyr', values_to = 'temp') %>%
  rowwise() %>%
  mutate(temp = ifelse(box_id %in% boundary_boxes, NA, temp)) %>% # turn boundary boxes to NA temp, we do not want those to confuse the average plot
  ungroup() %>%
  left_join(areas, by = c('box_id'='b')) %>% # add areas for weighting
  group_by(run, ts, lyr) %>%
  summarise(temp = weighted.mean(temp, area, na.rm = T)) %>%
  ungroup() %>%
  filter(lyr == 'sed',
         ts > 8) %>%
  ggplot()+
  geom_line(aes(x = ts+(1991-8), y = temp, color = run), linewidth = 0.9)+
  theme_bw()+
  labs(x = 'Year', y = 'SBT (\u00B0C)')
p_ts_bottom
ggsave('sbt_comp.png', p_ts_bottom, width = 7, height = 4)

# plot time series surface
p_ts_surface <- temp_df %>%
  dplyr::select(ts,box_id,lyr1,sed, run) %>%
  pivot_longer(-c(ts,box_id,run), names_to = 'lyr', values_to = 'temp') %>%
  rowwise() %>%
  mutate(temp = ifelse(box_id %in% boundary_boxes, NA, temp)) %>% # turn boundary boxes to NA temp, we do not want those to confuse the average plot
  ungroup() %>%
  left_join(areas, by = c('box_id'='b')) %>% # add areas for weighting
  group_by(run, ts, lyr) %>%
  summarise(temp = weighted.mean(temp, area, na.rm = T)) %>%
  ungroup() %>%
  filter(lyr == 'lyr1',
         ts > 8) %>%
  ggplot()+
  geom_line(aes(x = ts+(1991-8), y = temp, color = run), linewidth = 0.9)+
  theme_bw()+
  labs(x = 'Year', y = 'SST (\u00B0C)')
p_ts_surface
ggsave('sst_comp.png', p_ts_surface, width = 7, height = 4)
