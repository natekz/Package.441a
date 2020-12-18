#' Numeric Histograms
#'
#' @param x this is a dataframe
#' @param rug logical value
#' @param dim length 2 vector for the dimensions of the output
#'
#' @return outputs histograms for each numeric variable in a data frame, optionally includes a rug plot,
#' @export
#'
#' @examples data_hist(mtcars, rug = FALSE, dim = c(2,2))
data_hist = function(x, rug = TRUE, dim = NULL){
  par(mfrow = dim)
  for(i in names(x)){
    z = i
    if(class(x[,z]) == "numeric"){
      x = na.omit(x)
      plt_title = paste("Histogram of", i)
      hist(x[,z],breaks ="FD",col="skyblue2", freq = FALSE, ylab = "Density", main = plt_title)
      lines(density(x[,z]),lwd = 2, col = "red", type ='l')
      rug(x[,z])
    }
  }
}
