library(rgdal)
library(sf)
library(mapview)
library(rayshader)
library(elevatr)
library(raster)
library(leaflet)
library(rgeos)
library(magick)
library(gifski)

#Defining Functions needed for this process
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
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
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}

transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
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

save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done player !")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}

#____________________________
# define bounding box with longitude/latitude coordinates

N = 28.58138
S = 28.52170
W = -16.32866
E = -16.24096

bbox <- list(
  p1 = list(long = W, lat = N),
  p2 = list(long = E, lat = S))

# Check BBOX

leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent"
  ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  )

#________________________________
# Define Image Size, Funktion von:
# https://github.com/wcmbishop/rayshader-demo/blob/master/R/image-size.R
# je höher major_dim, desto genauer die Auflösung des Overlays

image_size <- define_image_size(bbox, major_dim = 7500); image_size
#________________________________
# BBOX in spatial polygon umwandeln für elev_raster funktion
# Nord , Ost , Süd, und West Boundaries zu beginn festgelegt

my_box <- rgeos::bbox2SP(n = N, s = S, w = W, e = E,
                         proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#_______________________________
# DEM Runterladen

Sub <- get_elev_raster(my_box,
                       z= 12,
                       neg_to_na = F)
SubEL <- crop(Sub, my_box)
SubEL[SubEL <= 0] <- 0 # Rastervalues utner 0 auf null wenn nötig

# Für Rayshader DEM in Matrix transformieren
ElevM <- raster_to_matrix(SubEL)

#_______________________________

#   Rayshader variablen berechnen

zs = 13 # hier zscale festlegen

ambmat <- ambient_shade(ElevM, zscale = zs)
raymat <- ray_shade(ElevM, zscale = zs, lambert = TRUE)
watermap <- detect_water(ElevM)

#_______________________________
# 2D Map plotten

ElevM %>%
  sphere_shade(texture = "imhof1") %>%
  #add_water(watermap, color ="imhof4") %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  plot_map()

#________________________________
# Download Overlay_image
# Optionen für map_type sind:

# NatGeo_World_Map
# USA_Topo_Maps 
# World_Imagery 
# World_Physical_Map 
# World_Shaded_Relief 
# World_Street_Map 
# World_Terrain_Base 
# World_Topo_Map 

overlay_file <- "overlay_map.png"

get_arcgis_map_image(bbox, map_type = "World_Imagery", file = overlay_file,
                     width = image_size$width, height = image_size$height, 
                     sr_bbox = 4326)

overlay_img <- png::readPNG(overlay_file)

#_______________________________

# 2D plot with map overlay
ElevM %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.9) %>%
  plot_map()



#_______________________________

zscale <- zs
rgl::clear3d()
ElevM %>% 
  sphere_shade(texture = "desert") %>% 
  #add_water(watermap, color = "imhof1") %>%
  add_overlay(overlay_img, alphalayer = 1) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_3d(ElevM, zscale = zscale, windowsize = c(1200, 1000),
          water = F ,waterdepth = 350, soliddepth = -max(ElevM)/zscale,
          wateralpha = 0.5, watercolor = "lightblue",
          theta = 25, phi = 25,
          zoom = 0.65, fov = 60)
render_snapshot()



#____________________________________
# Make it a GIF

# Animationsvariablen Festlegen
n_frames <- 180
thetas <- transition_values(from = 0, to = 359, steps = n_frames, one_way = T)

ElevM %>% 
  sphere_shade(texture = "imhof1", zscale = zscale) %>%
  add_shadow(ambient_shade(ElevM, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(ElevM, zscale = zscale, lambert = TRUE), 0.5) %>%
  add_overlay(overlay_img, alphalayer = 0.8) %>%
  save_3d_gif(ElevM, file = "ElevM.gif", duration = 9,
              solid = TRUE,
              soliddepth = -max(ElevM)/zscale,
              shadow = TRUE,
              water = TRUE,
              zscale = zscale,
              theta = thetas, phi = 25, zoom = 0.8, fov = 60)


