#' Line Plot Using Images
#'
#' This function allows you to draw a simple line plot of y vs. x, using one or
#' two images
#'
#' @param bottomImg The image "below" the line. This should be an image object
#' as obtained by \code{png::readPNG}, \code{jpeg::readJPEG},
#' \code{magick::image_read} etc. You must specify at least this image.
#' @param topImg The image "above" the line.
#' @param bgColor Background color above the line in case you don't specify
#' \code{topImg}, defaults to white.
#' @param lineColor Line color as accepted by R in the \code{col} parameter, defaults to black.
#' @param lineWidth Line width as accepted by R in the \code{lwd} parameter, defaults to 1.
#' @param lineType Line type as accepted by R in the \code{lty} parameter, defaults to "solid".
#' @param ... Other standard ggplot2 parameters such as mapping, data, stat etc.
#'
#' @note By default \code{geom_line_with_image} will not add padding in the x and y axes.
#' This default behavior of ggplot2 does not make sense when it comes to plotting with images.
#' You can however override this by adding \code{+ xlim(c(0, 100))}. Make sure
#' the result is meaningful though.
#'
#' @examples
#' annapurna <- png::readPNG(system.file("extdata", "annapurna.png", package = "ggwithimages"))
#' sky <- png::readPNG(system.file("extdata", "sky.png", package = "ggwithimages"))
#' kathmandu_hourly_aqi <- readr::read_csv(system.file("extdata", "kathmandu_hourly_aqi.csv", package = "ggwithimages"))
#' library(ggplot2)
#' ggplot(kathmandu_hourly_aqi, aes(hour, aqi)) +
#'   geom_line_with_image(annapurna, sky)
#'
#' @export
geom_line_with_image <- function(bottomImg = NULL, topImg = NULL,
                                 mapping = NULL, data = NULL, stat = "identity",
                                 position = "identity", na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, bgColor = "white",
                                 lineColor = 1, lineWidth = 1, lineType = "solid", ...) {
  if (is.null(bottomImg)) {
    stop("You must specify at least a 'bottom' image")
  } else {
    if (!is.null(topImg)) {
      bottomImg <- as.raster(bottomImg)
      topImg <- as.raster(topImg)

      if (!identical(dim(bottomImg), dim(topImg))) {
        warning("bottomImg and topImg have different sizes, will take the minimum in each dimension")

        minD1 <- min(dim(bottomImg)[1], dim(topImg)[1])
        minD2 <- min(dim(bottomImg)[2], dim(topImg)[2])
        bottomImg <- bottomImg[1:minD1, 1:minD2]
        topImg <- topImg[1:minD1, 1:minD2]
      }
    } else {
      bottomImg <- as.raster(bottomImg)
    }
  }

  list(
    ggplot2::layer(
      geom = GeomLineWithImage, mapping = mapping,  data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, bottomImg = bottomImg, topImg = topImg, bgColor = bgColor,
                    lineColor = lineColor, lineWidth = lineWidth, lineType = lineType, ...)
    ),
    ggplot2::coord_cartesian(expand = FALSE)
  )
}

#' helper function to rotate a 2D matrix 90 degrees clock-wise, n times
rotate = function(mat, n = 1) {
  for (i in 1:n) mat <- t(mat[nrow(mat):1, , drop = FALSE])
  mat
}

#' @rdname geom_line_with_image
#' @format NULL
#' @usage NULL
#' @export
GeomLineWithImage <- ggplot2::ggproto("GeomLineWithImage", ggplot2::Geom,
                                      required_aes = c("x", "y"),
                                      default_aes = ggplot2::aes(),
                                      draw_key = ggplot2::draw_key_blank,
                                      draw_panel = function(data, panel_params, coord,
                                                            bottomImg, topImg, bgColor,
                                                            lineColor, lineWidth, lineType) {
                                        coords <- coord$transform(data, panel_params)
                                        ordX <- order(coords$x)

                                        polyX <- c(coords$x[ordX])
                                        polyY <- c(coords$y[ordX])
                                        if (polyY[length(polyY)] < 0.5) {
                                          polyX <- c(polyX, 1)
                                          polyY <- c(polyY, 0)
                                        }
                                        polyX <- c(polyX, 1, 0)
                                        polyY <- c(polyY, 1, 1)
                                        if (polyY[1] < 0.5) {
                                          polyX <- c(polyX, 0)
                                          polyY <- c(polyY, 0)
                                        }

                                        if (!is.null(topImg)) {
                                          gridDF <- expand.grid(
                                            x = 1:dim(bottomImg)[2] / dim(bottomImg)[2],
                                            y = 1:dim(bottomImg)[1] / dim(bottomImg)[1])
                                          pIpoly <- sp::point.in.polygon(gridDF$x, gridDF$y,
                                                                         polyX,
                                                                         polyY)
                                          gridDF$pIpoly <- pIpoly > 0
                                          mask <- tidyr::spread(gridDF, y, pIpoly)
                                          mask <- as.matrix(mask[, -1])
                                          mask <- rotate(mask, 3)

                                          bottomImg[mask] <- topImg[mask]

                                          poly_grob <- NULL
                                        } else {
                                          poly_grob <- grid::polygonGrob(
                                            x = polyX,
                                            y = polyY,
                                            gp = grid::gpar(fill = bgColor, lty = 0)
                                          )
                                        }

                                        image_grob <- grid::rasterGrob(
                                          image = bottomImg,
                                          width = grid::unit(1, "npc"),
                                          height = grid::unit(1, "npc")
                                        )

                                        lines_grob <- grid::linesGrob(
                                          x = coords$x[ordX],
                                          y = coords$y[ordX],
                                          gp = grid::gpar(col = lineColor, lwd = lineWidth,
                                                          lty = lineType)
                                        )

                                        return(grid::grobTree(image_grob, poly_grob, lines_grob))
                                      }
)

#' Histogram Using Images
#'
#' This function allows you to draw a simple histogram of a single variable,
#' using one or two images
#'
#' @param bottomImg The image "inside" the histogram. This should be an image object
#' as obtained by \code{png::readPNG}, \code{jpeg::readJPEG},
#' \code{magick::image_read} etc. You must specify at least this image.
#' @param topImg The image "outside" the histogram.
#' @param bgColor Background color outside the histogram in case you don't specify
#' \code{topImg}, defaults to white.
#' @param lineColor Line color as accepted by R in the \code{col} parameter, defaults to black.
#' @param lineWidth Line width as accepted by R in the \code{lwd} parameter, defaults to 1.
#' @param lineType Line type as accepted by R in the \code{lty} parameter, defaults to "solid".
#' @param breaks See the \code{breaks} parameter in \code{?hist}, defaults to "Sturges".
#' @param freq See the \code{freq} parameter in \code{?hist}.
#' @param ... Other standard ggplot2 parameters such as mapping, data, stat etc.
#'
#' @note By default \code{geom_hist_with_image} will not add padding in the x and y axes.
#' This default behavior of ggplot2 does not make sense when it comes to plotting with images.
#' You can however override this by adding \code{+ xlim(c(0, 100))}. Make sure
#' the result is meaningful though.
#'
#' @examples
#' nyc <- png::readPNG(system.file("extdata", "nyc.png", package = "ggwithimages"))
#' night_sky <- png::readPNG(system.file("extdata", "night_sky.png", package = "ggwithimages"))
#' nyc_accidents <- readr::read_csv(system.file("extdata", "nyc_accidents.csv", package = "ggwithimages"))
#' library(ggplot2)
#' ggplot(nyc_accidents, aes(n_accidents)) +
#'   geom_hist_with_image(nyc, night_sky) +
#'   ylim(c(0, 700))
#'
#' @export
geom_hist_with_image <- function(bottomImg = NULL, topImg = NULL,
                                 mapping = NULL, data = NULL, stat = "identity",
                                 position = "identity", na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE, bgColor = "white",
                                 lineColor = 1, lineWidth = 2, lineType = "solid",
                                 breaks = "Sturges", freq = NULL, ...) {
  if (is.null(bottomImg)) {
    stop("You must specify at least one image object for the 'bottom' image")
  } else {
    if (!is.null(topImg)) {
      bottomImg <- as.raster(bottomImg)
      topImg <- as.raster(topImg)

      if (!identical(dim(bottomImg), dim(topImg))) {
        warning("bottomImg and topImg have different sizes, will take the minimum in each dimension")

        minD1 <- min(dim(bottomImg)[1], dim(topImg)[1])
        minD2 <- min(dim(bottomImg)[2], dim(topImg)[2])
        bottomImg <- bottomImg[1:minD1, 1:minD2]
        topImg <- topImg[1:minD1, 1:minD2]
      }
    } else {
      bottomImg <- as.raster(bottomImg)
    }
  }

  list(
    ggplot2::layer(
      geom = GeomHistWithImage, mapping = mapping,  data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, bottomImg = bottomImg, topImg = topImg,
                    bgColor = bgColor, lineColor = lineColor,
                    lineWidth = lineWidth, lineType = lineType,
                    breaks = breaks, freq = freq, ...)
    ),
    ggplot2::coord_cartesian(expand = FALSE)
  )
}

#' @rdname geom_hist_with_image
#' @format NULL
#' @usage NULL
#' @export
GeomHistWithImage <- ggplot2::ggproto("GeomHistWithImage", GeomLineWithImage,
                                      required_aes = c("x"),
                                      setup_data = function(data, params) {
                                        histogram <- suppressWarnings(
                                          hist(data$x, breaks = params$breaks,
                                               freq = params$freq, plot = FALSE)
                                        )
                                        data <- data.frame(x = rep(histogram$breaks, each = 2))
                                        yField <- ifelse(
                                          is.null(params$freq) || params$freq,
                                          "counts", "density")
                                        data$y <- c(0,
                                                    rep(histogram[[yField]], each = 2),
                                                    0)
                                        data$PANEL <- rep(1, nrow(data))
                                        data$group <- rep(-1, nrow(data))
                                        data
                                      },
                                      draw_panel = function(self, data, panel_params, coord,
                                                            bottomImg, topImg, bgColor,
                                                            lineColor, lineWidth, lineType,
                                                            breaks, freq) {
                                        ggproto_parent(GeomLineWithImage, self)$draw_panel(
                                          data, panel_params, coord,
                                          bottomImg, topImg, bgColor,
                                          lineColor, lineWidth, lineType
                                        )
                                      }
)
