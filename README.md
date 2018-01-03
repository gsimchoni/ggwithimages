<!-- README.md is generated from README.Rmd. Please edit that file -->
ggwithimages - Drawing Basic Plots with Images
==============================================

This package allows you to plot basic plots (currently lines and histograms) using images. You kinda have to see it to get it...

Install:

``` r
devtools::install_github("gsimchoni/ggwithimages")
```

Load:

``` r
library(ggwithimages)
```

Draw a line plot with the Annapurna mountains:

``` r
annapurna <- png::readPNG(system.file("extdata", "annapurna.png", package = "ggwithimages"))
sky <- png::readPNG(system.file("extdata", "sky.png", package = "ggwithimages"))
kathmandu_hourly_aqi <- readr::read_csv(system.file("extdata", "kathmandu_hourly_aqi.csv", package = "ggwithimages"))
#> Parsed with column specification:
#> cols(
#>   hour = col_integer(),
#>   aqi = col_double()
#> )

library(ggplot2)
ggplot(kathmandu_hourly_aqi, aes(hour, aqi)) +
  geom_line_with_image(annapurna, sky) +
  labs(title = "Air Quality Index in the Thamel, Kathmandu, Nepal",
       subtitle = "Measured in PM2.5 by the US Embassy in Kathmandu",
       y = "Hourly Mean AQI [PM2.5]",
       x = "Hour") +
  ylim(c(50, 200)) +
  theme(text = element_text(family="mono"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
#> Warning in geom_line_with_image(annapurna, sky): bottomImg and topImg have
#> different sizes, will take the minimum in each dimension
```

![](README-Annapurna-1.png)

Draw a histogram with NYC buildings:

``` r
nyc <- png::readPNG(system.file("extdata", "nyc.png", package = "ggwithimages"))
night_sky <- png::readPNG(system.file("extdata", "night_sky.png", package = "ggwithimages"))
nyc_accidents <- readr::read_csv(system.file("extdata", "nyc_accidents.csv", package = "ggwithimages"))
#> Parsed with column specification:
#> cols(
#>   date = col_character(),
#>   n_accidents = col_integer()
#> )

library(ggplot2)
ggplot(nyc_accidents, aes(n_accidents)) +
  geom_hist_with_image(nyc, night_sky) +
  labs(title = "NYC Daily No. of Accidents Distribution",
       subtitle = "Data Obtained on January 3, 2018 from NYC OpenData",
       y = "Frequency",
       x = "Daily No. of Accidents") +
  ylim(c(0, 700)) +
  xlim(c(0, 1300)) +
  theme(text = element_text(family="mono"),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))
#> Warning in geom_hist_with_image(nyc, night_sky): bottomImg and topImg have
#> different sizes, will take the minimum in each dimension
```

![](README-NYC-1.png)

See more in [this](TODO) blog post.
