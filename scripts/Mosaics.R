library(sf)
library(ggplot2)
source('functions.R')

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


x <- c(0.2, 1.8, 1.8, 0.2, 0.2) 
y <- c(0.2, 0.2, 1.6, 1.6, 0.2)

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

gr <- grids2poly(panels, gr)

ggplot() + 
  geom_sf(data=gr, aes(fill = col))
# understanding how to generate the fillzzz this lady has mad skillzzz (but does not
# document code the way others would...)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

fractals <- list(ambient::billow, ambient::fbm, ambient::ridged)
generators <- list(ambient::gen_simplex, ambient::gen_perlin, ambient::gen_worley)

value <- 0.25
nshades = 50

gr <- gr |>
  dplyr::mutate(
    fill = 10 * value + ambient::fracture(
      x = col * sample(-4:3, 1),
      y = row * sample(-4:3, 1),
      noise = sample(generators, 1)[[1]],
      fractal = sample(fractals, 1)[[1]],
      octaves = sample(10, 1), 
      frequency = sample(10, 1) / 20, # lower than 15 no good, above 25 too random
      value = "distance2"
    ) |>
      ambient::normalise(to = c(1, nshades)) |> 
      round()
  )


pal1 = c('#5AAA95', '#095256')
pal2 <- c('#9C7A97', '#C6D4FF')
pal4 = c('#D5A021', '#BF3100')
pal3 <- c('#EE7674', '#F9B5AC')


ggplot() + 
  geom_sf(data = outer_border, fill = '#BDC4A7') + 
  
  geom_sf(data = dplyr::filter(gr, Panel == 1), 
          aes(fill = fill), color = "#BDC4A7") + 
  scale_fill_gradientn(colours = pal4) + 
  
  
  # will need to perform this set of fns for each level value in the 'Panel' field
  
  ggnewscale::new_scale_fill() + 
  geom_sf(data = dplyr::filter(gr, Panel == 2), 
          aes(fill = fill), color = "#BDC4A7") + 
  scale_fill_gradientn(colours = pal3) + 
  
  ggnewscale::new_scale_fill() + 
  geom_sf(data = dplyr::filter(gr, Panel == 3), 
          aes(fill = fill), color = "#BDC4A7") + 
  scale_fill_gradientn(colours = pal1) + 
  
  ggnewscale::new_scale_fill() + 
  geom_sf(data = dplyr::filter(gr, Panel == 4), 
          aes(fill = fill), color = "#BDC4A7") + 
  scale_fill_gradientn(colours = pal2) + 
  
  # subdue the colors just slightly. ds
  geom_sf(data = panels, fill = adjustcolor("white", alpha.f = 0.2)) + 
  
  theme_void() + 
  theme(legend.position = 'none')

