#' @rdname scale_colour_nsw
#' @export
scale_fill_nsw <- function(palette = "default", discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "nsw", pal_nsw(palette = palette, ...), ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal_nsw(palette = palette, ...)(100), ...)
  }
}

