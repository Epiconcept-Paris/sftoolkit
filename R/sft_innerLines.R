#' Create a line layer consisting of shared boundaries with no attribute data
#'
#' @param x sf or sfc polygons object
#'
#' @return multilines in the same class as the input layer, but without attributes
#' @author Jean Pierre Decorps <jp.decorps@epiconcept.fr>
#'
#' @examples
#' library(sftoolkit)
#' data("Tasmania")
#' innerlines <- sftk_innerlines(Tasmania)
#' plot(innerlines)
sft_innerLines <- function(x) {
  .G <- sf::st_geometry(x)
  .M <- sf::st_intersects(x, sparse = FALSE) * 1
  l_LM <- dim(.M)[1]
  l_list <- list()
  l_index <- 0
  for (i in 1:(l_LM-1)) {
    for (j in (i+1):l_LM) {
      if (.M[i,j] == 1) {
        l_index <- l_index +1
        l_inter <- sf::st_intersection(.G[[i]], .G[[j]])
        l_list[[l_index]] <- l_inter
      }
    }
  }

  class(l_list) <- c("sfc_MULTILINESTRING", "sfc")
  .sfc <- sf::st_sfc(l_list, crs=4326)
  .sfc
}

