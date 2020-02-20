#' Jack Bailey's Custom ggplot Theme.
#'
#' This is a convenience function that makes it easier for me to make nice plots. It is an extension of theme_minimal() in ggplot2. It removes some lines, adds others, and changes the font to Cabin.
#' @export

theme_bailey <- function(){
  theme_minimal() +
    theme(text = element_text(family = "Cabin", color = "black", size = 8),
          plot.title = element_text(family = "Cabin", face = "bold", size = 10, hjust = 0),
          plot.subtitle = element_text(family = "Cabin", size = 8, hjust = 0),
          axis.line = element_line(),
          axis.title.x = element_text(family = "Cabin", face = "bold", margin = margin(t = 8), size = 6),
          axis.text.x = element_text(color = "black", size = 6),
          axis.ticks.x = element_line(color = "black"),
          axis.title.y = element_text(family = "Cabin", face = "bold", margin = margin(r = 8), size = 6),
          axis.text.y = element_text(color = "black", size = 6),
          strip.text = element_text(family = "Cabin", face = "bold", size = 8),
          panel.spacing = unit(.3, "cm"),
          panel.grid.major.y = element_line(size = .5),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )
}
