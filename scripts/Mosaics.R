library(sf)
library(ggplot2)

# create the border of the bookmark. 
outer_border <- data.frame(
  x = c(0, 2, 2, 0, 0), 
  y = c(0, 0, 6, 6, 0)
) |> sf::st_as_sf(coords = c('x', 'y'))

# now an inner border, 2 mm of width. this will be outside of our art, 
# adjacent to the outer border. 

x <- c(0.2, 1.8, 1.8, 0.2, 0.2) 
inner_border <- data.frame(
  x = x, 
  y = c(0.2, 0.2, 5.8, 5.8, 0.2)
) |>  sf::st_as_sf(coords = c('x', 'y')) 


y_pan <- c(0.2, 0.2, 1.6, 1.6, 0.2)

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
#' x <- c(0.2, 1.8, 1.8, 0.2, 0.2) 
#' y <- c(0.2, 0.2, 1.6, 1.6, 0.2)
#' 
#' # you can take a quick look to see how everything is.. 
#' data.frame(
#' x = x, y = y
#'  ) |>  sf::st_as_sf(coords = c('x', 'y')) |> 
#'   ggplot2::ggplot() + ggplot2::geom_sf()
#' 
#' @export 
panelR <- function(x, y, xORy, n, add){
  
  if(xORy=='x'){addX <- add; addY <- 1} else {addY <- add; add <- 1} 
  
  
}

data.frame(
  x = x, y = y
) |>  sf::st_as_sf(coords = c('x', 'y')) |> 
  ggplot2::ggplot() + ggplot2::geom_sf()

panel2 <- data.frame(
  x = x, y = y_pan+1.4
) |>  sf::st_as_sf(coords = c('x', 'y')) 

panel3 <- data.frame(
  x = x, y = y_pan+2.8
) |>  sf::st_as_sf(coords = c('x', 'y')) 

panel4 <- data.frame(
  x = x, y = y_pan+4.2
) |>  sf::st_as_sf(coords = c('x', 'y')) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  st_cast("POLYGON") 

ggplot() + 
  geom_sf(data = outer_border) + 
  geom_sf(data = inner_border) + 
  geom_sf(data = panel1) + 
  geom_sf(data = panel2) + 
  geom_sf(data = panel3) + 
  geom_sf(data = panel4)
#  theme_void()



