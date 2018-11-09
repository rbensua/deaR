#' @title RTS
#'   
#'   
#' @description Extract the returns to scale. 

#' @usage rts(deamodel, thr = 1e-4)
#' @param deamodel Object of class dea obtained with some of the dea functions.
#' @param thr Threshold for 
#' @author 
#' \strong{Vicente Coll-Serrano} (\email{vicente.coll@@uv.es}).
#' \emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}
#' 
#' \strong{Vicente Bolós} (\email{vicente.bolos@@uv.es}).
#' \emph{Department of Business Mathematics}
#'
#' \strong{Rafael Benítez} (\email{rafael.suarez@@uv.es}).
#' \emph{Department of Business Mathematics}
#'
#' University of Valencia (Spain)
#'
#' @examples 
#'  data("Coll_Blasco_2006")
#'  data_example <- read_data(Coll_Blasco_2006,
#'                            dmus=1, 
#'                            ni=2, 
#'                            no=2)
#'  result <- model_basic(data_example, 
#'                        orientation="io", 
#'                        rts="crs")
#'  rts(result)
#'  
#' @export

rts <- function(deamodel, thr =  1e-4){
  if (!is.dea(deamodel)) {
    stop("Input should be a dea class object!")
  }
  if(deamodel$modelname != "multiplier"){
    lamb <- lambdas(deamodel)
    lambsum <- rowSums(lamb)
    if(deamodel$rts == "crs"){
      rts <- ifelse(lambsum > 1 + thr , "Decreasing",
                    ifelse(abs(lambsum - 1) < thr, "Constant", "Increasing"))
      res <- data.frame(lambsum = lambsum, rts = rts)
    } else {
      res <- data.frame(lambsum = round(lambsum,5))
      warning("rts function with variable returns to scale does not make much sense!")
    }
  }else {
    k <- do.call(rbind, lapply(deamodel$DMU, function(x) x$multiplier_rts))
    rts <- ifelse(k > 1 + thr , "Decreasing",
                  ifelse(abs(k - 1) < thr, "Constant", "Increasing"))
    res <- data.frame(k = unname(k), rts = rts)
  }
  
  return(res)
}