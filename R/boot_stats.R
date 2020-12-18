#' Basic bootstrap function statistics
#'
#' @param x vector of numeric values
#' @param d vector of integrs representing indices
#'
#' @return outputs summary statistics for the values in x sleected by d
#' @export
#'
#' @examples bootstats(mtcars$cyl, c(1,2,3,4,5,6))
boot_stats <- function(x, d) {
  return(c(mean(x[d]),
           median(x[d]),
           sd(x[d])))
}
