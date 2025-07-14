# ====================================================
# Spatial LGCP Script for Cetaceans in the Azores
# Author: Diego Fernández-Fernández
# Date: 14/07/2025
# Description: Full workflow to prepare spatial data,
# interpolate covariates, estimate effort, and fit LGCP models
# ====================================================

# ===============================
# 1. Load Required Libraries
# ===============================
library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(ggplot2)
library(inlabru)
library(INLA)
library(fmesher)
library(readxl)
library(viridis)
library(concaveman)
library(sp)

# ===============================
# 2. Load Study Area and Observations
# ===============================
setwd("..")
study_area <- read_sf("./Study Area/ECS/study_area_ECS.shp") %>% st_transform(4326)
cetacean_data <- read.csv("data_ecs_env.csv")

# Filter target species
dde <- cetacean_data %>% filter(scientificName == "Delphinus delphis")
pma <- cetacean_data %>% filter(scientificName == "Physeter macrocephalus")

# ===============================
# 3. Crop Study Area to Azores
# ===============================
crs_utm28n_km <- 'PROJCRS["unknown",
    BASEGEOGCRS["unknown",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]],
            ID["EPSG",6326]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8901]]],
    CONVERSION["UTM zone 28N",
        METHOD["Transverse Mercator",
            ID["EPSG",9807]],
        PARAMETER["Latitude of natural origin",0,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8801]],
        PARAMETER["Longitude of natural origin",-15,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8802]],
        PARAMETER["Scale factor at natural origin",0.9996,
            SCALEUNIT["unity",1],
            ID["EPSG",8805]],
        PARAMETER["False easting",500000,
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8807]],
        ID["EPSG",16028]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["kilometre",1000,
                ID["EPSG",9036]]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["kilometre",1000,
                ID["EPSG",9036]]]]'

clip_box <- st_polygon(list(matrix(c(-35, 43, -21, 43, -21, 34, -35, 34, -35, 43),
                                   ncol = 2, byrow = TRUE))) %>%
  st_sfc(crs = crs_utm28n_km)

study_area_crop <- st_intersection(study_area, clip_box)
study_area_km <- study_area_crop

st_write(study_area_km, "./Study Area/ECS/study_area_azores.shp", delete_layer = TRUE)

# ===============================
# 4. Mesh Construction
# ===============================
bdry <- inla.sp2segment(study_area_km)
mesh <- fm_mesh_2d_inla(
  boundary = list(bdry), 
  max.edge = c(10, 50),
  offset = c(10, 50), 
  cutoff = 10, 
  crs = fm_crs(study_area_km)
)
saveRDS(mesh, "./Spatial Point Patern Analysis/mesh_inla_azores.rds")

# ===============================
# 5. Create Integration Points
# ===============================
xx <- range(mesh$loc[,1])
yy <- range(mesh$loc[,2])
new_xy <- expand.grid(seq(xx[1], xx[2], 1), seq(yy[1], yy[2], 1))
A <- inla.spde.make.A(mesh = mesh, loc = new_xy)
ips <- fm_int(mesh)
saveRDS(ips, "../Environmental Variables/cmems/seasonal_ips/ips_azores.rds")

# ===============================
# 6. SPDE Model Definition
# ===============================
spde <- inla.spde2.pcmatern(
  mesh = mesh, 
  prior.range = c(800, 0.8), 
  prior.sigma = c(6, 0.01)
)

# ===============================
# 7. Covariate Interpolation (example: mlotst summer)
# ===============================
var_name <- "mlotst"
season <- "summer"
raster_path <- paste0("../Environmental Variables/cmems/seasonal_means/", var_name, "_", season,"_mean.nc")
r <- rast(raster_path)
r_proj <- project(r, crs_utm28n_km)

r_raster <- raster::raster(r_proj)
r_spdf <- as(r_raster, "SpatialPixelsDataFrame")
proj4string(r_spdf) <- CRS(crs_utm28n_km)

vals <- inlabru:::eval_spatial(data = r_spdf, where = ips)
vals_filled <- bru_fill_missing(data = r_spdf, where = ips, values = vals)
interpolated <- A %*% vals_filled
interpolated_scaled <- scale(as.vector(interpolated), center = TRUE, scale = TRUE)

raster_ips <- terra::rast(SpatialPixelsDataFrame(
  points = new_xy,
  data = data.frame(var_interp = as.vector(interpolated_scaled)),
  proj4string = CRS(crs_utm28n_km)
))

assign(paste0(var_name, "_", season, "_azores"), raster_ips)

# Save interpolated rasters
dir.create("../Environmental Variables/cmems/seasonal_ips", showWarnings = FALSE)

guardar_raster_rds <- function(obj_name) {
  rast_obj <- get(obj_name, envir = .GlobalEnv)
  rds_path <- file.path("../Environmental Variables/cmems/seasonal_ips", paste0(obj_name, ".rds"))
  saveRDS(rast_obj, file = rds_path)
  message("✅ Saved: ", rds_path)
}

vars <- c("mlotst", "so", "to", "zo")
seasons <- c("winter", "summer")

for (v in vars) {
  for (s in seasons) {
    obj_name <- paste0(v, "_", s, "_azores")
    if (exists(obj_name)) guardar_raster_rds(obj_name)
  }
}

# ===============================
# 8. Bathymetry Interpolation
# ===============================
bathy_path <- "C:/Users/ichu3/Dropbox/Diego/REDUCE/Environmental Variables/gebco_2024_tid_geotiff/gebco_2024_ecs.tif"
bathy <- rast(bathy_path)
bathy_proj <- project(bathy, crs_utm28n_km)
bathy_crop <- crop(bathy_proj, vect(study_area_km))
bathy_masked <- mask(bathy_crop, vect(study_area_km), overwrite = TRUE)
bathy_masked[bathy_masked > 0] <- 0
saveRDS(bathy_masked, "./bathy_masked_ecs.rds")

# Interpolate to integration points
bathy_r <- raster::raster(bathy_masked)
bathy_spdf <- as(bathy_r, "SpatialPixelsDataFrame")
proj4string(bathy_spdf) <- CRS(crs_utm28n_km)

bathy_vals <- inlabru:::eval_spatial(data = bathy_spdf, where = ips)
bathy_vals_filled <- bru_fill_missing(data = bathy_spdf, where = ips, values = bathy_vals)
bathy_interp_t <- A %*% bathy_vals_filled
bathy_vals_scaled <- scale(bathy_interp_t, center = TRUE, scale = TRUE)[, 1]

bathy_raster_ips <- terra::rast(SpatialPixelsDataFrame(
  points = new_xy,
  data = data.frame(bathymetry = as.vector(bathy_vals_scaled)),
  proj4string = CRS(crs_utm28n_km)
))
saveRDS(bathy_raster_ips, "C:/Users/ichu3/Dropbox/Diego/REDUCE/Environmental Variables/cmems/seasonal_ips/bathymetry_rast_centscaled_azores.rds")

# ===============================
# 9. Format Observations as sf
# ===============================
dde_sf <- st_as_sf(dde, coords = c("lon", "lat"), crs = 4258) %>%
  st_transform(crs_utm28n_km) %>%
  st_intersection(study_area_km)

dde_sf$lon <- st_coordinates(dde_sf)[,1]
dde_sf$lat <- st_coordinates(dde_sf)[,2]
dde_sf <- st_drop_geometry(dde_sf)

write.csv(dde_sf, "./Spatial Point Patern Analysis/dde_azores.csv")

dde_sf <- read.csv("./Spatial Point Patern Analysis/dde_azores.csv")
dde_sf <- st_as_sf(dde_sf, coords = c("lon", "lat"), crs = crs_utm28n_km)

# ===============================
# 10. Define Seasonal Subsets
# ===============================
winter_months <- c(12, 1, 2, 10, 11, 3)
summer_months <- c(4, 5, 6, 7, 8, 9)

dde_longwinter <- dde_sf %>% filter(month %in% winter_months)
dde_longsummer <- dde_sf %>% filter(month %in% summer_months)

# repeat for PMA

# ===============================
# 11. Generate Samplers and Estimate Effort (Winter)
# ===============================

# Define full seasonal windows
longsummer <- c(4, 5, 6, 7, 8, 9)
longwinter <- c(10, 11, 12, 1, 2, 3)

# Load all observations and convert to sf
observations_sf <- readRDS("./Spatial Point Patern Analysis/observations_azores_sf.rds") %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(study_area_km))

# Subset by season
observations_longwinter <- st_as_sf(data.frame(geometry = st_geometry(observations_sf %>% filter(month %in% longwinter))))
observations_longsummer <- st_as_sf(data.frame(geometry = st_geometry(observations_sf %>% filter(month %in% longsummer))))

# Create non-convex hull (winter)
coords_winter <- st_coordinates(observations_longwinter)
hull_raw_winter <- inla.nonconvex.hull(coords_winter, convex = -0.02, resolution = c(55, 40))
hull_sf_winter <- st_as_sf(fm_as_sfc(hull_raw_winter))
st_crs(hull_sf_winter) <- st_crs(study_area_km)
hull_winter <- st_intersection(hull_sf_winter, st_geometry(study_area_km))

# Fix geometries
hull_lines <- st_geometry(hull_winter)
line_polys <- lapply(hull_lines, function(ln) {
  coords <- st_coordinates(ln)
  coords <- coords[!duplicated(coords[,1:2]), ]
  if (!all(coords[1,1:2] == coords[nrow(coords),1:2])) {
    coords <- rbind(coords, coords[1,])
  }
  if (nrow(coords) >= 4) {
    return(st_polygon(list(coords[,1:2])))
  } else {
    return(NULL)
  }
})
line_polys <- Filter(Negate(is.null), line_polys)
hull_sfc <- st_sfc(line_polys, crs = st_crs(hull_winter))
hull_winter <- st_sf(geometry = hull_sfc)

saveRDS(hull_winter, file = "./hull_winter.rds")
# Repeat for Summer season 

# ===============================
# 12. Load Environmental Covariates and Detection Function
# ===============================

# Directory where .rds rasters are stored
rds_dir <- "../Environmental Variables/cmems/seasonal_ips"

# Helper function to load rasters from directory
load_rds_rast <- function(filename) {
  readRDS(file.path(rds_dir, filename))
}

# Load effort rasters (scaled LGCP predictions for each season)
effort_summer_rast <- rast("../Environmental Variables/cmems/seasonal_ips/effort_summer_rast_azores.rds")
effort_winter_rast <- rast("../Environmental Variables/cmems/seasonal_ips/effort_winter_rast_azores.rds")

# Select effort raster to use (can switch by season)
effort_density <- effort_winter_rast

# Load bathymetry 
bathy_azores    <- rast(file.path(rds_dir, "bathymetry_rast_centscaled_azores.rds"))

# Load seasonal CMEMS covariates
mlotst_winter_rast <- load_rds_rast("mlotst_winter_azores.rds")
mlotst_summer_rast <- load_rds_rast("mlotst_summer_azores.rds")
to_winter_rast     <- load_rds_rast("to_winter_azores.rds")
to_summer_rast     <- load_rds_rast("to_summer_azores.rds")
so_winter_rast     <- load_rds_rast("so_winter_azores.rds")
so_summer_rast     <- load_rds_rast("so_summer_azores.rds")
zo_winter_rast     <- load_rds_rast("zo_winter_azores.rds")
zo_summer_rast     <- load_rds_rast("zo_summer_azores.rds")

# Custom helper: Quantile-based exponential transformation
qexppnorm <- function(x, rate) {
  qexp(pnorm(x, lower.tail = FALSE), rate = rate, lower.tail = FALSE)
}

# Detection function (Panunzi & Martino style)
log_detect <- function(xy, t1, t2) {
  dens <- eval_spatial(effort_density, xy)
  pnorm(dens / qexppnorm(t1, rate = 0.5) + t2, log.p = TRUE)
}

# Load seasonal sampling hulls
hull_winter <- readRDS("./hull_winter.rds")
hull_summer <- readRDS("./hull_summer.rds")


# ===============================
# 13. Fit LGCP Model for Sampling Effort (Winter)
# ===============================
cmp_effort <- ~ 0 + Intercept(1, mean.linear = 0, prec.linear = 0.1) +
  SPDE(main = geometry, model = spde)

formula_effort <- geometry ~ Intercept + SPDE
likelihood_effort <- like("cp",
                          formula = formula_effort,
                          domain = list(geometry = mesh),
                          samplers = hull_winter,
                          data = observations_longwinter)

fit_effort <- bru(components = cmp_effort,
                  likelihood_effort,
                  options = list(verbose = TRUE, bru_max_iter = 20))

# Predict sampling intensity
pxl_eff <- fm_pixels(mesh, mask = hull_winter, dim = c(500, 500))
pred_intensity <- predict(fit_effort, newdata = pxl_eff, formula = ~ exp(Intercept + SPDE))

# Convert prediction to raster
effort_sf <- st_as_sf(pred_intensity)
coords <- st_coordinates(effort_sf)
vals <- effort_sf$median

raw_rast <- terra::rast(data.frame(x = coords[,1], y = coords[,2], z = vals), type = "xyz", crs = st_crs(effort_sf)$wkt)
cropped_rast <- terra::crop(raw_rast, study_area_km)
reproj_rast <- terra::project(cropped_rast, crs(study_area_km))
masked_rast <- terra::mask(reproj_rast, study_area_km)

# Interpolate effort surface to integration points
vals_interp <- inlabru:::eval_spatial(data = masked_rast, where = ips)
vals_filled <- bru_fill_missing(data = masked_rast, where = ips, values = vals_interp)
interpolated <- A %*% vals_filled

# Create raster over IPS
effort_rast_ips <- terra::rast(SpatialPixelsDataFrame(
  points = new_xy,
  data = data.frame(effort = as.vector(interpolated)),
  proj4string = CRS(st_crs(study_area_km)$wkt)
))
saveRDS(effort_rast_ips, "../Environmental Variables/cmems/seasonal_ips/effort_winter_rast_azores.rds")
# Repeat for Summer season 

# ===============================
# 14. LGCP Model Fit (Winter Example)
# ===============================
cmp <- ~ 0 + Intercept(1, 
                       mean.linear = 0, 
                       prec.linear = 0.1) +
              SPDE(main = geometry, 
                   model = spde) +
              bathy(main = bathy_scaled, 
                    mean.linear = 0,
                    prec.linear = 1) +
              zo(main = zo_summer_rast, 
                 mean.linear = 0, 
                 prec.linear = 1) +
              mlotst(main = mlotst_summer_rast, 
                     mean.linear = 0, 
                     prec.linear = 1) +
              to(main = to_summer_rast, 
                 mean.linear = 0, 
                 prec.linear = 1)
            # Optional detection function parameters:
            # + effort_scale(1, mean.linear = 0, prec.linear = 1) +
            #   effort_shift(1, mean.linear = 0, prec.linear = 1)

form <- geometry ~ Intercept + SPDE + bathy + zo + mlotst + to # + log_detect(geometry, effort_scale, effort_shift)

fit_lgcp <- lgcp(components = cmp,
                 formula = form,
                 data = dde_longwinter,
                 domain = list(geometry = mesh),
                 samplers = study_area_km,
                 options = list(
                   verbose = TRUE,
                   bru_max_iter = 20,
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
                 ))

saveRDS(fit_lgcp, "./Spatial Point Patern Analysis/models/model_lgcp_winter.RDS")

# ===============================
# 15. Prediction Grid and Extract Covariates
# ===============================
pxl <- fm_pixels(mesh, dim = c(500, 500), mask = study_area_km)
pxl$bathy <- terra::extract(bathy, vect(pxl))[,2]

pred <- predict(fit_lgcp, newdata = pxl, formula = ~ exp(Intercept + SPDE + bathy))

# ===============================
# 16. Plot Predicted Intensity
# ===============================
if (requireNamespace("patchwork", quietly = TRUE)) library(patchwork)

ggplot() +
  geom_sf(data = study_area_km, fill = NA, color = "black") +
  geom_sf(data = dde_longwinter, color = "black", size = 1) +
  gg(tidyr::pivot_longer(pred, c(q0.025, q0.5, q0.975, sd),
                         names_to = "quantile", values_to = "value"),
     aes(fill = value), geom = "tile", alpha = 0.9) +
  facet_wrap(~quantile) +
  scale_fill_viridis_c(option = "plasma", name = "Predicted Intensity",
                       oob = scales::squish) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey90", color = NA),
        strip.text = element_text(color = "black", face = "bold"),
        panel.grid = element_line(color = "grey90"),
        panel.spacing = unit(1, "lines"))

# =================================
# 17. Plot Posterior Distribution
# =================================
marginals <- list(
  Intercept = fit_lgcp$marginals.fixed$Intercept,
  bathy = fit_lgcp$marginals.fixed$bathy,
  range = fit_lgcp$marginals.hyperpar$`Range for SPDE`,
  sigma = fit_lgcp$marginals.hyperpar$`Stdev for SPDE`
)

marg_df <- imap_dfr(marginals, ~{
  as.data.frame(.x) %>%
    rename(x = x, y = y) %>%
    mutate(parameter = .y)
})

marg_df <- marg_df %>%
  mutate(parameter = factor(parameter, levels = c("Intercept", "bathy", "to", "range", "sigma")))
colores <- c("Intercept" = "#F8766D", "bathy" = "#7CAE00", "range" = "#00BFC4", "sigma" = "#C77CFF")


ggplot(marg_df, aes(x = x, y = y, color = parameter, fill = parameter)) +
  geom_line(size = 1) +
  geom_area(alpha = 0.3) +
  facet_wrap(~ parameter, scales = "free", ncol = 2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Parameter value",
    y = "Density"
  ) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  theme(legend.position = "none")

