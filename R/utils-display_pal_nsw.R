#' Display the palette colour of nswtheme
#'
#' Display the colours in the colour environments: `nsw_named_palettes` and `nsw_colour_grids`
display_pal_nsw <- function() {

  #colour_grid_names <- names(nsw_colour_grids) |> dput()
  #colour_grid_names <- c("base","aboriginal", "treasury", "corporate",)

  pals <-
    c(
      rev(names(nsw_named_palettes)),
      # colour grids
      " ",
      " v=base",
      paste0("h=", colnames(nsw_colour_grids$base)),
      paste0("t=", rownames(nsw_colour_grids$base)),
      " ",
      " v=aboriginal",
      paste0("h=", colnames(nsw_colour_grids$aboriginal)),
      paste0("t=", rownames(nsw_colour_grids$aboriginal)),
      " ",
      " v=treasury",
      paste0("h=", colnames(nsw_colour_grids$treasury)),
      paste0("t=", rownames(nsw_colour_grids$treasury)),
      " ",
      " v=corporate",
      paste0("h=", colnames(nsw_colour_grids$corporate)),
      paste0("t=", rownames(nsw_colour_grids$corporate))
    )

  cols <-
    unname(
      c(
        rev(as.list(nsw_named_palettes)),
        NA,
        NA,
        Map(
          \(x) unname(unlist(col_nsw(hue = x, variant = "base"))),
          colnames(nsw_colour_grids$base)
        ),
        Map(
          \(x) unname(unlist(col_nsw(tone = x, variant = "base"))),
          rownames(nsw_colour_grids$base)
        ),
        NA,
        NA,
        Map(
          \(x) unname(unlist(col_nsw(hue = x, variant = "aboriginal"))),
          colnames(nsw_colour_grids$aboriginal)
        ),
        Map(
          \(x) unname(unlist(col_nsw(tone = x, variant = "aboriginal"))),
          rownames(nsw_colour_grids$aboriginal)
        ),
        NA,
        NA,
        Map(
          \(x) unname(unlist(col_nsw(hue = x, variant = "treasury"))),
          colnames(nsw_colour_grids$treasury)
        ),
        Map(
          \(x) unname(unlist(col_nsw(tone = x, variant = "treasury"))),
          rownames(nsw_colour_grids$treasury)
        ),
        NA,
        NA,
        Map(
          \(x) unname(unlist(col_nsw(hue = x, variant = "corporate"))),
          colnames(nsw_colour_grids$corporate)
        ),
        Map(
          \(x) unname(unlist(col_nsw(tone = x, variant = "corporate"))),
          rownames(nsw_colour_grids$corporate)
        )
      ))

  n <- lengths(cols)
  nr <- length(pals)
  nc <- max(n)

  ylim <- c(0, nr)
  oldpar <- graphics::par(mgp = c(0, 0, 0), mar = c(0, 4, 0, 0))
  on.exit(graphics::par(oldpar))
  plot(
    1,
    1,
    xlim = c(0, nc),
    ylim = ylim,
    type = "n",
    axes = FALSE,
    bty = "n",
    xlab = "",
    ylab = ""
  )
  for (i in 1:nr) {
    nj <- n[[i]]
    if (startsWith(pals[[i]], " ")) {
      next
    }
    shadi <- cols[[i]]
    graphics::rect(
      xleft = 0:(nj - 1),
      ytop = nr - i,
      xright = 1:nj,
      ybottom = nr - i + 0.8,
      col = shadi,
      border = "light grey"
    )
  }
  graphics::text(
    rep(-0.1, nr),
    rev(1:nr) - 0.6,
    labels = pals,
    xpd = TRUE,
    adj = 1,
    cex = 0.7
  )
}
