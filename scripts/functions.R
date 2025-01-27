#' Create consecutive panels within a regular shape 
#' 
#' @description Create consecutive panels in either the x or y dimension. 
#' @param x Numeric vector, length 5. The outer corners (5th position being a repeat of the first corner), of the first panel; x dimension. 
#' @param y Numeric vector, length 5. The outer corners (5th position being a repeat of the first corner), of the first panel y dimension. 
#' @param xORy Character. Whether the panels will be consecutive vertically, or horizontally. 'x' indicates vertical. 
#' @param n Numeric. The number of panels to generate. 
#' @param add Numeric. The amount of area which needs to be added to have the panels border (see example).
#' @results a list of panels, as sf polygons, for extracting as you see fit. 
#' @examples 
#' 
#' # first define the corners of the shape you are going to panel. NOte that the
#' # coordinates must go in ONE DIRECTION around the object, clockwise or counter
#' # clockwise should not matter (to you). 
#' x <- c(0.2, 1.8, 1.8, 0.2, 0.2) 
#' y <- c(0.2, 0.2, 1.6, 1.6, 0.2)
#' 
#' # you can take a quick look to see how everything is.. 
#' data.frame(
#' x = x, y = y
#'  ) |>  sf::st_as_sf(coords = c('x', 'y')) |> 
#'   ggplot2::ggplot() + ggplot2::geom_sf()
#'   
#' # note that generally... the amount of `add` will be the difference between the
#' # two edges of your quadrilateral (yes this code will work for non-square -
#' # but I am a square and so is my work). In this example, we are going to 
#' # vertical, and our add is 1.4 (x[2] - x[1]). However we don't use this as a default
#' # because there are a few different ways folks can supply these values. 
#' 
#' panels <- panelR(x, y, xORy= 'Y', n = 4, add = 1.4)
#' ggplot2::ggplot(panels) + 
#'   ggplot2::geom_sf()
#' 
#' @export 
panelR <- function(x, y, xORy, n, add){
  
  if(xORy=='X'){addX <- add; addY <- 0} else {addY <- add; addX <- 0} 
  
  mkPanel <- function(x, y, addX, addY){
    data.frame(x = x + addX, y = y + addY) |>  
      sf::st_as_sf(coords = c('x', 'y')) |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
      sf::st_cast("POLYGON") |>
      sf::st_as_sf()
  }
  
  panels <- vector(mode = 'list', length = n)
  panels[1] <- mkPanel(x, y, addX = 0, addY = 0)
  
  for (i in 2:n){
    panels[i] <- mkPanel(x, y, addX = addX*(i-1), addY = addY*(i-1))
  }
  
  panels <- sf::st_combine(do.call("c", panels)) |>
    sf::st_cast('POLYGON') |>
    sf::st_as_sf() |>
    dplyr::mutate(
      Panel = 1:dplyr::n(), .before = 1) |>
    dplyr::rename(geometry = x)
  
  return(panels)
}


#' snap generated grids to a polygon and number the cells by rows and column
#' 
#' @description After adding grids to the output of `panelR` ensure that they do 
#' not 'overflow' from the space, by clipping them, assign a panel ID to them, and
#' define their positions in X and Y space. 
#' @param gr an sf object. grids created over a set of panels 
#' @param panels an sf object. polygons created from `panelR`
grids2poly <- function(panels, gr){
  
 # sf::st_agr(gr) = "constant"#; sf::st_agr(panels) = "constant"
  samps <- sf::st_intersects(gr, panels)
  samps <-  unlist(lapply(samps, function(x){y <- x[sample(1:length(x), size = 1)]}))
  
  gr <- dplyr::bind_cols(
    Panel = samps,
    geometry = st_as_sf(gr)
  ) |>
    sf::st_as_sf()

  gr <- st_intersection(gr, st_union(panels))
  gr <- gr[sf::st_is(gr, 'POLYGON'),]
  gr <- gr |> 
    dplyr::mutate(gr, ID = 1:dplyr::n(), .before = 1) |>
    dplyr::rename(geometry = x)
  
  gr2 <- gr %>% 
    sf::st_centroid() %>% 
    dplyr::mutate(
      x = sf::st_coordinates(.)[,'X'],
      y = sf::st_coordinates(.)[,'Y']
    ) |>
   dplyr::arrange(y) |>
   dplyr::group_by(y) |>
    dplyr::mutate(row = dplyr::cur_group_id()) |>
    dplyr::arrange(x) |> 
    dplyr::group_by(x) |> 
    dplyr::mutate(col = dplyr::cur_group_id()) |>
    dplyr::select(ID,  col, row) |>
    sf::st_drop_geometry() |>
    dplyr::arrange(ID)
  
  gr <- dplyr::left_join(gr, gr2, by = 'ID') 
  
}
