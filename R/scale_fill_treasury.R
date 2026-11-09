#' @rdname scale_colour_nsw
#' @export
scale_fill_treasury <- function(variant = 'treasury', tone = 'normal', discrete = TRUE, ...) {
  #pal_nsw(variant = 'treasury', tone = 'normal')(4)
  if (discrete) {
    ggplot2::discrete_scale("fill", "treasury", pal_nsw(variant = variant, tone = tone, ...), ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal_nsw(variant = variant, tone = tone, ...)(100), ...)
  }
}

