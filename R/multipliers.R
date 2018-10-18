#' @title Multipliers
#'   
#' @description Extract the multipliers of the DMUs from a dea solution.
#' 
#' @usage multipliers(deasol)
#' 
#' @param deasol Object of class dea obtained with some of the dea model functions.
#'   
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
#'  result <- model_multiplier(data_example,
#'                             orientation="io", 
#'                             rts="crs")
#'  multipliers(result)
#'  
#' @export

multipliers <- 
  function(deasol) {
    
    if (is.dea(deasol)) {
      
      if (any(grepl("multiplier", names(deasol$DMU[[1]])))) {
        
        multiplier_input <- NULL
        if ("multiplier_input" %in% names(deasol$DMU[[1]])) {
          multiplier_input <- do.call(rbind, lapply(deasol$DMU, function(x)
            x$multiplier_input))
        }
        
        multiplier_output <- NULL
        if ("multiplier_output" %in% names(deasol$DMU[[1]])) {
          multiplier_output <- do.call(rbind, lapply(deasol$DMU, function(x)
            x$multiplier_output))
        }
        
        multiplier_rts <- NULL
        if ("multiplier_rts" %in% names(deasol$DMU[[1]])) {
          multiplier_rts <- unlist(lapply(deasol$DMU, function(x)
            x$multiplier_rts))
        }
        
        return(list(input = round(multiplier_input,6), 
                    output = round(multiplier_output,6), 
                    rts = round(multiplier_rts),4))
        
      } else {
        stop("No multiplier parameters in this solution!")
      }
      
    } else {
      
      stop("Input should be a dea class object!")
      
    }
    
  }