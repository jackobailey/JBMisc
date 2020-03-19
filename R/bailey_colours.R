#' Jack Bailey's Colour Palette.
#'
#' This is a convenience function that makes it easier for me to make plots that use colours I like.
#' @export
#'

bailey_colours <- function(...) {

  choice <- c(...)

  cols <- c(
    `red` = "#C1292E",
    `red2` = "#C63C41",
    `red3` = "#CC4F54",
    `red4` = "#D16367",
    `red5` = "#D7767A",
    `red6` = "#DD8A8D",
    `red7` = "#E29DA0",
    `red8` = "#E8B1B3",
    `red9` = "#EEC4C6",
    `red10` = "#F3D8D9",
    `blue` = "#235789",
    `blue2` = "#376693",
    `blue3` = "#4B759E",
    `blue4` = "#5F84A9",
    `blue5` = "#7394B3",
    `blue6` = "#87A3BE",
    `blue7` = "#9BB2C9",
    `blue8` = "#AFC1D4",
    `blue9` = "#C3D1DE",
    `blue10` = "#D7E0E9",
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
    `black` = "#020100",
    `white` = "#FDFFFC"
  )

  if (is.null(choice)) return(cols)

  `names<-`(cols[choice], NULL)
}
