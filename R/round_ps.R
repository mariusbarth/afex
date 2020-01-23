#' Helper function which rounds p-values
#'
#' p-values are rounded in a sane way: .99 - .01 to two digits, < .01 to three digits, < .001 to four digits.
#'
#' @usage round_ps(x)
#'
#' @param x a numeric vector
#'
#' @return A character vector with the same length of x.
#'
#' @author Henrik Singmann
#'
#' @encoding UTF-8
#'
#' @export round_ps
#' @examples
#' round_ps(runif(10))
#' 
#' round_ps(runif(10, 0, .01))
#' print_ps(runif(10, 0, .01))
#' round_ps(runif(10, 0, .001))
#' 
#' round_ps(0.0000000099)
#' 

round_ps <- function(x) {
  substr(as.character(ifelse(x < 0.0001, " <.0001", ifelse(x < 0.001, formatC(x, digits = 4, format = "f"), ifelse(x < 0.01, formatC(x, digits = 3, format = "f"), ifelse(round(x, 2) == 1, " >.99", formatC(x, digits = 2, format = "f")))))), 2, 7)
}


#' @keywords internal

print_ps <- function(x) {
  
  y <- ifelse(
    x < .001
    , "< .001"
    , ifelse(
      x > .999
      , "> .999"
      , format(round(x, digits = 3L), nsmall = 3L)
    )
  )
  
  format(gsub("0\\.", "\\.", y), justify = "right")
}
