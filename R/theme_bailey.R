#' Jack Bailey's Custom ggplot Theme.
#'
#' This is a convenience function that makes it easier for me to make nice plots. It is an extension of theme_minimal() in ggplot2. It removes some lines, adds others, and changes the font to Cabin.
#' @export
#'

theme_bailey <- function(){
  ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1)),
                   text = ggplot2::element_text(family = "Cabin", color = "black", size = 8),
                   plot.title = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1.4), hjust = 0),
                   plot.subtitle = ggplot2::element_text(family = "Cabin", size = ggplot2::rel(1), hjust = 0, margin = ggplot2::margin(b = 10)),
                   axis.line = ggplot2::element_line(lineend = "round"),
                   axis.title.x = ggplot2::element_text(family = "Cabin", face = "bold", margin = ggplot2::margin(t = 10), size = ggplot2::rel(1)),
                   axis.text.x = ggplot2::element_text(color = "black", size = ggplot2::rel(1)),
                   axis.ticks.x = ggplot2::element_line(lineend = "round"),
                   axis.title.y = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1)),
                   axis.text.y = ggplot2::element_text(color = "black", size = ggplot2::rel(1)),
                   strip.text = ggplot2::element_text(family = "Cabin", face = "bold", size = ggplot2::rel(1)),
                   panel.spacing = ggplot2::unit(.3, "cm"),
                   panel.grid.major.y = ggplot2::element_line(size = .5, lineend = "round"),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank()
    )
}
