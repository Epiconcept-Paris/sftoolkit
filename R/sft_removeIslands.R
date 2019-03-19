internal_removeIslands <- function(geom, mode = "keeplarger", min_area = 4, keep = 1) {
  # -- local variables
  # -------------------------------------------------------------
  l_class_sfc <- NULL   # Class of geometry
  l_class_sfg <- NULL   # Class of polygones
  l_del <- 0            # Numer of deleted polygones
  l_L1  <- 0            # Numberof regions
  l_L2  <- 0            # Numberof polygons in a region
  l_matindex <- 0       #
  l_maxmatsize <- 0     #
  l_minpolygons <- 0

  # -- locales variables from parameters
  # -------------------------------------------------------------
  # .pmin <- floor(min_vertices +1)
  p_mode <- mode
  # p_minlines <- min_vertex + 1
  p_keep <- as.numeric(min_area * 1000000); units(p_keep) <- "m^2"
  p_loglines <- ceiling(1.5 * log2(min_area * 1000000))
  p_minkeep <- keep + 1

  #.spdf <- x
  # .keep = as.numeric(keep * 1000000)
  #units(.keep) <- "m^2"

  # .df <- sf::st_drop_geometry(.spdf)
  # .G <- sf::st_geometry(.spdf)
  .G <- geom
  l_L1 <- length(.G)
  # cat("L1 : ", l_L1, "\n")

  # ---Gets classes for sfc & sfg
  # ------------------------------
  l_class_sfc <- class(.G)
  l_class_sfg <- class(.G[[1]])

  #
  # cat("Class de G : ", class(.G), "\n")
  # cat("Class de G(i) : ", class(.G[[1]]), "\n")

  for (i in 1:l_L1) {
    l_L2 <- length(.G[[i]])
    if (l_L1 == 1 & mode == "keeplarger") {
      l_bufindex <- integer(l_L2)
      l_bufmatsize <- integer(l_L2)
    }

    # --- Walking through the list of matri
    # -------------------------------------
    l_matindex <- 0
    l_maxmatsize <- 0

    for (j in 1:l_L2) {
      l_matrix <- .G[[i]][[j]][[1]]

      # --- Mode brutal (keeplarger)
      # ----------------------------
      if (p_mode == "keeplarger") {
        l_matsize <- dim(l_matrix)[1]
        if (l_L1 == 1) {
          l_bufindex[j] <- j
          l_bufmatsize[j] <-l_matsize
          next
        }

        if (j == 1) {
          l_maxmatsize <- l_matsize
          l_matindex <- j
          next
        }
        if (l_maxmatsize < l_matsize) {
          .G[[i]] [[l_matindex]] <- list()
          l_del <- l_del + 1
          l_maxmatsize <- l_matsize
          l_matindex <- j
        } else {
          .G[[i]] [[j]] <- list()
          l_del <- l_del + 1
        }
      }
      # --- Slow mode (areasize))
      # -------------------------
      if (p_mode == "areasize") {
        l_matsize <- dim(l_matrix)[1]
        if (l_matsize < p_loglines) {
          .G[[i]] [[j]] <- list()
          l_del <- l_del + 1
          next
        }
        l_polygon <- sf::st_sfc(sf::st_polygon(list(l_matrix)), crs=sf::st_crs(4326))
        l_area <- sf::st_area(l_polygon)
        if (l_area < p_keep) {
          .G[[i]] [[j]] <- list()
          l_del <- l_del + 1
        }
      }


    } ## END loop j

    if (l_L1 == 1 & p_mode == "keeplarger") {
      l_df <- data.frame(l_bufindex, l_bufmatsize)
      colnames(l_df) <- c("J", "A")
      l_df <- l_df[order(-l_df$A),]
      l_LDF <- nrow(l_df)
      for (index in p_minkeep:l_LDF) {
        j_index <- l_df[index, 1]
        .G[[1]][[j_index]]  <- list()
        l_del <- l_del + 1
      }
    }

    # --- Removes empty geometries in the region. NEEDS: package 'rlist'
    # ------------------------------------------------------------------
    .G[[i]] <- rlist::list.clean(.G[[i]], function(x) length(x) == 0L, TRUE)

    # --- Set class of region polygones
    # ---------------------------------
    class(.G[[i]]) <- l_class_sfg

  } ## END loop i

  .G <- sf::st_sfc(.G, crs=4326)
  return(.G)
  .sf <- sf::st_set_geometry(.df, .G)
  # .spdf <- sf::st_geometry(.spdf) <- .G

  .msg <- sprintf("%d polygons have been removed", l_del)
  message(.msg)

  # --- Return the new geometry
  # ---------------------------
  .sf
}

sft_removeIslands.sf <- function(x, mode = "keeplarger", min_area = 4, keep = 1) {
  .df <- sf::st_drop_geometry(x)
  .g  <- sf::st_geometry(x)
  .g  <- internal_removeIslands(.g, mode, min_area, keep)
  .sf <- sf::st_set_geometry(.df, .g)
  .sf
}

sft_removeIslands.sfc <- function(x, mode = "keeplarger", min_area = 4, keep = 1) {
  geometry <- x
  .x <- st_sf(geometry, A=1)
  .g  <- sf::st_geometry(.x)
  .g <- internal_removeIslands(.g, mode, min_area, keep)
  .g
}
