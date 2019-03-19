# internal_getIslands -----------------------------------------------------
# =========================================================================
internal_getIslands <- function(x, forget = 0) {
  # -- local variables
  # -------------------------------------------------------------
  l_class_sfc <- NULL   # Class of geometry
  l_class_sfg <- NULL   # Class of polygones
  l_ret <- 0            # Number of retained polygones
  l_L1  <- 0            # Number of regions
  l_L2  <- 0            # Number of polygons in a region
  l_matindex <- 0       #
  l_maxmatsize <- 0     #
  # l_minpolygons <- 0

  # -- locales variables from parameters
  # -------------------------------------------------------------
  p_forget <- forget

  .df <- sf::st_drop_geometry(x)
  .G <- sf::st_geometry(x)
  l_L1 <- length(.G)

  # ---Gets classes for sfc & sfg
  # ------------------------------
  l_class_sfc <- class(.G)
  l_class_sfg <- class(.G[[1]])

  for (i in 1:l_L1) {
    l_L2 <- length(.G[[i]])
    if (l_L1 == 1) {
      l_bufindex <- integer(l_L2)
      l_bufmatsize <- integer(l_L2)
    }

    # --- Walking through the list of matrix
    # --------------------------------------
    l_matindex <- 0
    l_maxmatsize <- 0
    for (j in 1:l_L2) {
      l_matrix <- .G[[i]][[j]][[1]]

      # --- Mode brutal (keeplarger)
      # ----------------------------
      l_matsize <- dim(l_matrix)[1]
      if (l_L1 == 1) {
        l_bufindex[j] <- j
        l_bufmatsize[j] <-l_matsize
        l_ret <- l_ret + 1
        next
      }
      if (j == 1) {
        l_maxmatsize <- l_matsize
        l_matindex <- j
        next
      }
      if (l_maxmatsize < l_matsize) {
        l_ret <- l_ret + 1
        l_maxmatsize <- l_matsize
        l_matindex <- j
      } else {
        l_ret <- l_ret + 1
      }

    } ## END loop j

    if (l_L1 == 1) {
      l_df <- data.frame(l_bufindex, l_bufmatsize)
      colnames(l_df) <- c("J", "A")
      l_df <- l_df[order(-l_df$A),]
      l_LDF <- nrow(l_df)
      if (p_forget > 0) {
        for (index in 1:p_forget) {
          j_index <- l_df[index, 1]
          .G[[1]][[j_index]]  <- list()
        }
        l_ret <- l_LDF - p_forget
      }
    } else {
      .G[[i]] [[l_matindex]] <- list()
    }

    # --- Removes empty geometries in the region. NEEDS: package 'rlist'
    # ------------------------------------------------------------------
    .G[[i]] <- rlist::list.clean(.G[[i]], function(x) length(x) == 0L, TRUE)

    # --- Set class of region polygones
    # ---------------------------------
    class(.G[[i]]) <- l_class_sfg

  } ## END loop i

  .G <- sf::st_sfc(.G, crs=4326)
  .sf <- sf::st_set_geometry(.df, .G)

  .msg <- sprintf("%d polygons were retained", l_ret)
  message(.msg)

  # --- Return the new geometry
  # ---------------------------
  .sf
}


# sft_getIslands.sf -------------------------------------------------------
# =========================================================================
sft_getIslands.sf <- function(x, forget = 0) {
  x <- internal_getIslands(x, forget)
  x
}


# sft_getIslands.sfc ------------------------------------------------------
# =========================================================================
sft_getIslands.sfc <- function(x, forget = 0) {
  x <- sf::st_sf(x)
  x <- internal_getIslands(x, forget)
  x
}
