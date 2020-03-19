#' University of Manchester Colour Palette.
#'
#' This is a convenience function that makes it easier for me to make plots that conform to the University of Manchester's corporate colour scheme.
#' @export
#'

uom_colours <- function(...) {

  choice <- c(...)

  cols <- c(
    `purple` = "#660099",
    `purple2` = "#7317A2",
    `purple3` = "#812EAB",
    `purple4` = "#8F45B4",
    `purple5` = "#9D5CBE",
    `purple6` = "#AB73C7",
    `purple7` = "#B98BD0",
    `purple8` = "#C7A2D9",
    `purple9` = "#D5B9E3",
    `purple10` = "#E3D0EC",
    `gold` = "#FFCC33",
    `gold2` = "#FFD045",
    `gold3` = "#FFD558",
    `gold4` = "#FFD96A",
    `gold5` = "#FFDE7D",
    `gold6` = "#FFE38F",
    `gold7` = "#FFE7A2",
    `gold8` = "#FFECB4",
    `gold9` = "#FFF1C7",
    `gold10` = "#FFF5D9",
    `grey` = "#999999",
    `grey2` = "#A2A2A2",
    `grey3` = "#ABABAB",
    `grey4` = "#B4B4B4",
    `grey5` = "#BEBEBE",
    `grey6` = "#C7C7C7",
    `grey7` = "#D0D0D0",
    `grey8` = "#D9D9D9",
    `grey9` = "#E3E3E3",
    `grey10` = "#ECECEC",
    `black` = "#000000",
    `white` = "#FFFFFF"
  )

  if (is.null(choice)) return(cols)

  c(cols[choice])
}
