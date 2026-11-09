#' NSW colour and fill scales for ggplot2
#'
#' @param palette Name of palette (e.g. "default", "brand_default", "core"). See [pal_nsw()]
#' @param discrete Logical indicating whether aesthetic is discrete (TRUE) or continuous (FALSE)
#' @param ... Additional arguments passed to [pal_nsw()] or [ggplot2::discrete_scale()]
#' @export
scale_colour_nsw <- function(palette = "default", discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("colour", "nsw", pal_nsw(palette = palette, ...), ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal_nsw(palette = palette, ...)(100), ...)
  }
}

# Export UK and US spelling aliases
#' @rdname scale_colour_nsw
#' @export
scale_color_nsw <- scale_colour_nsw
