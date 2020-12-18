#' Percent NA
#'
#' @param x this is a dataframe
#' @param count this is a logical
#'
#' @return Returns a value between 0 and 1 representing the percentage of each column that is NA, and an optional output with the total number of rows that have
#' at least one NA
#'
#' @examples
#' perc_NA(mtcars, count = FALSE)
perc_NA = function(x, count = TRUE){
  if(count == TRUE){
    count_na = sum(!complete.cases(x))
    print(paste(count_na,"rows with NA  values out of", nrow(x)))
  }
  colna = sapply(x, function(y) sum(is.na(y)))
  colna/nrow(x)*100
}
