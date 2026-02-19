# =========================
# Updated + corrected version (no rgdal/rgeos)
# =========================

library(sf)
library(rayshader)
library(elevatr)
library(raster)
library(leaflet)
library(magick)
library(gifski)
library(png)

# needed by your functions
library(httr)
library(glue)
library(jsonlite)
library(rlang)
library(purrr)
library(rgl)

# -----------------------------
# Functions
# -----------------------------
define_image_size <- function(bbox, major_dim = 400) {
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  img_width  <- ifelse(aspect_ratio > 1, major_dim, major_dim * aspect_ratio) |> round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim / aspect_ratio) |> round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

bbox_to_3857 <- function(bbox4326) {
  pts <- sf::st_as_sf(
    data.frame(
      lon = c(bbox4326$p1$long, bbox4326$p2$long),
      lat = c(bbox4326$p1$lat,  bbox4326$p2$lat)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  ptsm <- sf::st_transform(pts, 3857)
  xy <- sf::st_coordinates(ptsm)
  
  list(
    p1 = list(long = xy[1, 1], lat = xy[1, 2]),
    p2 = list(long = xy[2, 1], lat = xy[2, 2])
  )
}

get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL,
                                 width = 400, height = 400, sr_bbox = 4326) {
  
  url <- httr::parse_url(
    "https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute"
  )
  
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue::glue(
          "https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
          map_type = map_type
        )))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- httr::GET(
    url,
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param, auto_unbox = TRUE)
    )
  )
  httr::stop_for_status(res)
  
  body <- httr::content(res, type = "application/json")
  
  if (is.null(file)) file <- tempfile("overlay_img", fileext = ".png")
  
  # In ArcGIS responses, the "results" list contains an URL to the exported image
  img_url <- body$results[[1]]$value$url
  img_res <- httr::GET(img_url)
  httr::stop_for_status(img_res)
  
  img_bin <- httr::content(img_res, "raw")
  writeBin(img_bin, file)
  
  message(paste("image saved to file:", normalizePath(file, winslash = "/")))
  invisible(file)
}

plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}

save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 &
                                 !(names(dots) %in% var_exception_list)]
  
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- rlang::exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  if (length(gif_var_lengths) == 0) stop("No animated variables provided (e.g. theta=vector).")
  if (length(unique(gif_var_lengths)) > 1) stop("all gif input vectors must be the same length")
  
  n_frames <- unique(gif_var_lengths)
  
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames), add = TRUE)
  
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames),
                          path = file, delay = duration / n_frames)
  message("Done!")
  invisible(file)
}

transition_values <- function(from, to, steps = 10, one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin"))) stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range) / 2
  
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)),
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y
  }
  
  middle - half_width * scaling
}

# =========================
# Bounding box (Hamburg area in your example)
# =========================
N <- 28.794488
S <- 28.608625
W <- -17.984549
E <- -17.798781

bbox <- list(
  p1 = list(long = W, lat = N),
  p2 = list(long = E, lat = S)
)

# Check BBOX
leaflet() %>%
  addTiles() %>%
  addRectangles(
    lng1 = W, lat1 = N,
    lng2 = E, lat2 = S,
    fillColor = "transparent"
  ) %>%
  fitBounds(lng1 = W, lat1 = N, lng2 = E, lat2 = S)

# Image size for overlay
image_size <- define_image_size(bbox, major_dim = 2000)
print(image_size)

# =========================
# Build sf bbox polygon for elevatr (WGS84)
# =========================
bb <- st_bbox(c(xmin = W, ymin = S, xmax = E, ymax = N), crs = st_crs(4326))
my_box_sf <- st_as_sfc(bb)  # polygon

# =========================
# DEM download (elevatr)
# =========================
Sub <- elevatr::get_elev_raster(
  locations = my_box_sf,
  z = 10,
  neg_to_na = FALSE
)

# Crop to bbox extent (Sub is RasterLayer)
SubEL <- raster::crop(Sub, raster::extent(W, E, S, N))

# Optional: clamp negatives
SubEL[SubEL <= 0] <- 0

# To matrix
ElevM <- rayshader::raster_to_matrix(SubEL)

# =========================
# Rayshader layers
# =========================
zscale_dem <- 9
ambmat <- rayshader::ambient_shade(ElevM, zscale = zscale_dem)
raymat <- rayshader::ray_shade(ElevM, zscale = zscale_dem, lambert = TRUE)
watermap <- rayshader::detect_water(ElevM)

# 2D map
ElevM %>%
  rayshader::sphere_shade(texture = "imhof1") %>%
  rayshader::add_shadow(ambmat, max_darken = 0.5) %>%
  rayshader::add_shadow(raymat, max_darken = 0.5) %>%
  rayshader::plot_map()

# =========================
# Overlay download (FIXED: use EPSG:3857 extent for ArcGIS basemap)
# =========================
overlay_file <- "overlay_map.png"

bbox_3857 <- bbox_to_3857(bbox)

get_arcgis_map_image(
  bbox_3857,
  map_type = "World_Imagery",
  file = overlay_file,
  width = image_size$width,
  height = image_size$height,
  sr_bbox = 3857
)

overlay_img <- png::readPNG(overlay_file)

# 2D with overlay
ElevM %>%
  rayshader::sphere_shade(texture = "imhof4") %>%
  rayshader::add_water(watermap, color = "imhof4") %>%
  rayshader::add_shadow(raymat, max_darken = 0.5) %>%
  rayshader::add_shadow(ambmat, max_darken = 0.5) %>%
  rayshader::add_overlay(overlay_img, alphalayer = 0.9) %>%
  rayshader::plot_map()

# =========================
# 3D render
# =========================
zscale_3d <- 30

rgl::clear3d()
ElevM %>%
  rayshader::sphere_shade(texture = "desert") %>%
  rayshader::add_water(watermap, color = "imhof1") %>%
  rayshader::add_overlay(overlay_img, alphalayer = 1) %>%
  rayshader::add_shadow(raymat, max_darken = 0.5) %>%
  rayshader::add_shadow(ambmat, max_darken = 0.5) %>%
  rayshader::plot_3d(
    ElevM, zscale = zscale_3d, windowsize = c(1200, 1000),
    water = FALSE, waterdepth = 150,
    soliddepth = -max(ElevM, na.rm = TRUE) / zscale_3d,
    theta = 25, phi = 25,
    zoom = 0.65, fov = 60
  )

rayshader::render_snapshot()