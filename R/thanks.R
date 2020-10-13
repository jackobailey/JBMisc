#' Thanks for Replicating!
#'
#' A convenience function that thanks the user for replicating and provides contact details.
#'
#' @examples
#' thanks()
#' @export

thanks <- function(){

  cat(
    "-------------------------------------------------",
    "",
    "  THANKS FOR REPLICATING!\n",
    "  For any further questions about this project:",
    "",
    "  Website: www.jack-bailey.co.uk",
    "  Email: jack.bailey@manchester.ac.uk",
    "  Twitter: @PoliSciJack",
    "",
    "-------------------------------------------------",
    sep = "\n"
  )

}
