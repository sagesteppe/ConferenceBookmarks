library(sf)
library(ggplot2)

# create the border of the bookmark. 
outer_border <- data.frame(
  x = c(0, 2, 2, 0, 0), 
  y = c(0, 0, 6, 6, 0)
) |> sf::st_as_sf(coords = c('x', 'y')) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
  sf::st_cast("POLYGON") |>
  sf::st_as_sf()

# now an inner border, 2 mm of width. this will be outside of our art, 
# adjacent to the outer border. 

x <- c(0.2, 1.8, 1.8, 0.2, 0.2) 
inner_border <- data.frame(
  x = x, 
  y = c(0.2, 0.2, 5.8, 5.8, 0.2)
) |>  sf::st_as_sf(coords = c('x', 'y')) 


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



panels <- panelR(x, y, xORy= 'Y', n = 4, add = 1.4)
ggplot2::ggplot(panels) + 
  ggplot2::geom_sf()

# now we want to add grids into each of these panels. our printing goal will be
# about 1.8 inches wide for each section and 1.6 long... We want same spaced grids. 
# so this looks about right
1.8/23
1.6/20


# create all mosaics for all panels at once, and then assign
# creating by panel will result in some ugly clipping of them. 

gr <- sf::st_make_grid(panels, n = c(20, 90), square = FALSE)
gr <- sf::st_intersection(panels, gr)
gr <- gr[ sf::st_is(gr, 'POLYGON'), ]


??billow
fractals <- list(billow, fbm, ridged)
generators <- list(gen_simplex, gen_perlin, gen_worley)

??fbm

gr |>
  dplyr::mutate(
    fill = 10 * value + fracture(
      x = x * sample(-3:3, 1),
      y = y * sample(-3:3, 1),
      noise = sample(generators, 1)[[1]],
      fractal = sample(fractals, 1)[[1]],
      octaves = sample(10, 1),
      frequency = sample(10, 1) / 20,
      value = "distance2"
    ) |>
      normalise(to = c(1, nshades)) |> 
      round()
  )


ggplot() + 
  geom_sf(data = outer_border, fill = '#BDC4A7') + 
  geom_sf(data = inner_border, fill = NA) + 
  geom_sf(data = panels, fill = NA) + 
 geom_sf(data = gr)
#  theme_void()


?geom_sf()
