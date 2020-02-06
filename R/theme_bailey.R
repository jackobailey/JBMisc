#' Jack Bailey's Custom Theme.
#'
#' This is a convenience function that makes it easier for me to make nice plots. It is an extension of theme_minimal() in ggplot2. It removes some lines, adds others, and changes the font to Cabin.
#' @export

theme_bailey <- function(){
  theme_minimal() +
    theme(text = element_text(family = "Cabin", color = "black", size = 8),
          plot.title = element_text(family = "Cabin SemiBold", hjust = 0.5),
          axis.line = element_line(),
          axis.title.x = element_text(family = "Cabin SemiBold", margin = margin(t = 8),),
          axis.text.x = element_text(color = "black"),
          axis.ticks.x = element_line(),
          axis.title.y = element_text(family = "Cabin SemiBold", margin = margin(r = 8)),
          axis.text.y = element_text(color = "black"),
          strip.text = element_text(family = "Cabin SemiBold"),
          panel.spacing = unit(.3, "cm"),
          panel.grid.major.y = element_line(size = .5),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )
}
