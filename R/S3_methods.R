#' Get small detached polygons (islands)
#'
#' @param x sf or sfc object
#' @param forget Number of areas not considered as islands
#'
#' @return same type than x. sf or sfc object
#'
#' @examples
#' library(sftoolkit)
#' library(ggplot2)
#' data("Tasmania")
#'
#' Islands <- sft_getIslands(Tasmania)
#' ggplot() + geom_sf(data=Tasmania, fill=NA, color="black", size = 0.25)+theme_light() +
#'  geom_sf(data=Islands, fill=NA, color="red", size = 0.25)+theme_light()
sft_getIslands <- function(x,
                           forget = 0) UseMethod("sft_getIslands", x)



#' Remove small detached polygons (islands)
#'
#' @description Remove small detached polygons, keeping those with a minimum area
#'
#' @param x spatial object to filter of sf or sfc
#' @param min_area minimum area to keep (square kilometer)
#'
#' @return like x, sf or sfc
#'
#' @examples
sft_removeIslands <- function(x,
                              min_area = 4) UseMethod("sft_removeIslands", x)
