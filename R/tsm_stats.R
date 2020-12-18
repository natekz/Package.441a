#' Time Series Model Statistics
#'
#' @param model_list list object containing regression objects
#'
#' @return outputs a table of statistics for each model in the list
#' @export
#'
#' @examples x = list(model1, model2, model3)
#' tsm_stats(x)
tsm_stats = function(model_list){
  diagnostics = c()
  nms = c('F-value', 'R-sqr', 'MSE', 'Log-Lik')
  for(i in list){
    MSE = deviance(i)/i$df.residual
    summ = summary(i)
    Rsq = if(is.null(summ$r.squared)) NA else summ$r.squared
    fvalue = if(is.null(summ$fstatistic)) NA else summ$fstatistic[1]
    Loglik = logLik(i)
    diagnostics = rbind(diagnostics,setNames(c(fvalue, Rsq, MSE, Loglik),nms))
  }
  print(diagnostics)
}
