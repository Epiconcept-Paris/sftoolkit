internal_removeIslands <- function(geom, min_area = 4) {
  # -- local variables
  # -------------------------------------------------------------
  l_class_sfc <- NULL   # Class of geometry
  l_class_sfg <- NULL   # Class of polygones
  l_del <- 0            # Numer of deleted polygones

  # -- locales variables from parameters
  # -------------------------------------------------------------
  p_area <- as.numeric(min_area * 1000000); units(p_area) <- "m^2"

  .G <- geom
  l_L1 <- length(.G)

  # ---Gets classes for sfc & sfg
  # ------------------------------
  l_class_sfc <- class(.G)
  l_class_sfg <- class(.G[[1]])

  # --- Slow mode (areasize))
  # -------------------------
  .G <- lapply(.G, function(L){
    l_len <- length(L)
    if (l_len == 1) {
      return(L)
    }
    L <- lapply(L, function(L){
      l_matrix <- L[[1]]
      l_polygon <- sf::st_sfc(sf::st_polygon(list(l_matrix)), crs=sf::st_crs(4326))
      l_area <- sf::st_area(l_polygon)
      if (l_area <= p_area) {
        L <- list()
        l_del <<- l_del + 1
      }
      L
    })
    # --- Removes empty geometries in the region. NEEDS: package 'rlist'
    # ------------------------------------------------------------------
    L <- rlist::list.clean(L, function(x) length(x) == 0L, TRUE)
    # --- Set class of region polygones
    # ---------------------------------
    class(L) <- l_class_sfg
    return(L)
  })

  .G <- sf::st_sfc(.G, crs=4326)
  .msg <- sprintf("%d polygons have been removed", l_del)
  message(.msg)

  return(.G)
}


# sft_removeIslands.sf ----------------------------------------------------
# =========================================================================
sft_removeIslands.sf <- function(x, min_area = 4) {
  .df <- sf::st_drop_geometry(x)
  .g  <- sf::st_geometry(x)
  .g  <- internal_removeIslands(.g, min_area)
  .sf <- sf::st_set_geometry(.df, .g)
  .sf
}

# sft_removeIslands.sfc ---------------------------------------------------
# =========================================================================
sft_removeIslands.sfc <- function(x, min_area = 4) {
  geometry <- x
  .x <- st_sf(geometry, A=1)
  .g  <- sf::st_geometry(.x)
  .g <- internal_removeIslands(.g, min_area)
  .g
}
