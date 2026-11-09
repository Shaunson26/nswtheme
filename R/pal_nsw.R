#' NSW Design System colour palettes
#'
#' Palettes created using the [NSW Design System](https://designsystem.nsw.gov.au/docs/content/design/theming.html).
#' To use the Aboriginal colour grid, specify `variant = "aboriginal"`.\cr\cr
#' \if{html}{\figure{nsw_palette.svg}{options: width=95%}}
#' `r svglite::svglite("man/figures/nsw_palette.svg"); display_pal_nsw(); invisible(dev.off())`
#'
#' @export
#'
#' @param palette name of a predefined palette: `r rev(names(nsw_named_palettes))`.
#' @param hue name or index of the hue - see below.
#'   Ignored if `palette` is specified.
#' @param tone name or index of the tone - see below.
#'   Ignored if `palette` is specified.
#' @param variant name of palette variant.
#'   Available options are: `r rev(names(nsw_colour_grids))`.
#'   Ignored unless `hue` or `tone` is specified.
#' @param direction set to -1 to reverse the order of colours in the palette,
#'   or 1 for the original order.
#' @returns A palette object (see [palette constructors][scales::new_continuous_palette])
#'
#' @details
#' To use palettes based on the NSW Design System colour grids, either
#' specify `hue` and allow the tone to vary, or specify `tone` to allow the
#' hue to vary.
#' The recommendation is to use the first two tonal rows going one colour at a
#' time from a set of colours; this can be achieved by specifying `tone = 1:2`.
#'
#' There are several named palettes which can be specified with `palette`.
#' To create custom combinations of named colours from the design system, use
#' `pal_nsw_manual()`.
#'
#' @inheritSection col_nsw Colour columns and tonal rows
#' @family palettes
#' @seealso [col_nsw()]
#'
#' @examples
#' library(scales)
#'
#' pal_nsw() |> show_col()
#' pal_nsw(hue = "blues") |> show_col()
#' pal_nsw(tone = 1:2, variant = "corporate") |> show_col()
#' pal_nsw(tone = "light") |> show_col()
#' pal_nsw(tone = "normal", variant = "aboriginal") |> show_col()
#' pal_nsw_manual(c("blue_02", "red_01", "green_03")) |> show_col()
#'
#' # you can interpolate colours by converting to a continuous scale
#' pal_nsw(hue = "blues") |> as_continuous_pal() |> show_col(labels = FALSE)
pal_nsw <- function(
    palette = waiver(),
    hue = NA,
    tone = NA,
    variant = getOption("nswtheme.colour_theme", default = "base"),
    direction = 1
) {

  # Argument checking ----
  if (all(identical(hue, NA), identical(tone, NA))) {
    if (!missing(variant)) {
      cli::cli_warn(
        "{.arg variant} is ignored unless used with {.arg hue} or {.arg tone}"
      )
    }
  }
  if (
    !is_waiver(palette) &&
    any(!missing(variant), !identical(hue, NA), !identical(tone, NA))
  ) {
    cli::cli_warn(
      "{.arg variant}, {.arg hue} and {.arg tone} are ignored when {.arg palette} is specified"
    )
  }


  if (!identical(hue, NA) && !identical(tone, NA)) {
    colours <- col_nsw(hue = hue, tone = tone, variant = variant)
  } else if (!identical(hue, NA)) {
    colours <- col_nsw(hue = hue, variant = variant)
  } else if (!identical(tone, NA)) {
    colours <- col_nsw(tone = tone, variant = variant)
  } else if (!is_waiver(palette)) {
    colours <- get(palette, envir = nsw_named_palettes)
  } else {
    colours <- get("default", envir = nsw_named_palettes)
  }

  if (direction < 0) {
    colours <- rev(colours)
  }

  scales::pal_manual(unlist(colours), type = "colour")
}

#' @rdname pal_nsw
#'
#' @param colours vector of colour names corresponding to [nsw_colours].
#'
#' @export
pal_nsw_manual <- function(colours) {
  colours <- rlang::env_get_list(colours, env = as.environment(nsw_colours))
  scales::pal_manual(unlist(unname(colours)), type = "colour")
}

nsw_named_palettes <-
  rlang::new_environment(
    list(
      default = c(
        nsw_colours$blue_01,
        nsw_colours$red_02,
        nsw_colours$blue_04,
        nsw_colours$red_04,
        nsw_colours$blue_02,
        nsw_colours$teal_02,
        nsw_colours$fuchsia_01,
        nsw_colours$orange_02,
        nsw_colours$yellow_02,
        nsw_colours$red_03
        # nsw_colours$blue_01,
        # nsw_colours$red_02,
        # nsw_colours$teal_02,
        # nsw_colours$fuchsia_01,
        # nsw_colours$orange_02,
        # nsw_colours$yellow_02,
        # nsw_colours$red_03
      ),
      brand_default = c(
        nsw_colours$blue_01,
        nsw_colours$red_02,
        nsw_colours$blue_04,
        nsw_colours$blue_02

      ),
      core = c(
        nsw_colours$blue_01,
        nsw_colours$red_02
      )
    )
  )


