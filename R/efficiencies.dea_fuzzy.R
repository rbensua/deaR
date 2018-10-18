#' @title Efficiencies
#'   
#' @description Extract the efficiencies of the DMUs from a dea or dea_fuzzy solution.
#' 
#' @usage efficiencies(deasol)
#' 
#' @param deasol Object of class dea or dea_fuzzy obtained with some of the dea model functions.
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
#' Example. Replication results DEA1 in Tomkins and Green (1988)
#' data("Departments")
#' # Calculate Total income
#' Departments$Total_income <- Departments[,5]+Departments[,6]+Departments[,7] 
#' data_DEA1 <- read_data(Departments,
#'                        inputs=9,
#'                        outputs=c(2,3,4,12))
#' result <- model_basic(data_DEA1, 
#'                       orientation="io",
#'                       rts="crs")
#' efficiencies(result) # Table 3 (p.156) 
#'  
#' @export


efficiencies.dea_fuzzy <-
  function(deasol) {
  
  if (is.dea(deasol)) {
    
    if ("efficiency" %in% names(deasol$DMU[[1]])) {
      eff <- unlist(lapply(deasol$DMU, function(x)
        x$efficiency))
      if (length(deasol$DMU[[1]]$efficiency) > 1) {
        eff  <- do.call(rbind, lapply(deasol$DMU, function(x)
          x$efficiency))
        mean_eff <- unlist(lapply(deasol$DMU, function(x) x$mean_efficiency))
        eff <- cbind(eff, mean_eff)
      }
    } else if ("beta" %in% names(deasol$DMU[[1]])) {
      eff <- unlist(lapply(deasol$DMU, function(x)
        x$beta))
    } else if ("delta" %in% names(deasol$DMU[[1]])) {
      eff <- unlist(lapply(deasol$DMU, function(x)
        x$delta))
    } else if ("objval" %in% names(deasol$DMU[[1]])) {
      eff <- unlist(lapply(deasol$DMU, function(x)
        x$objval))
    } else {
      stop("No efficiency/beta/delta/objval parameters in this solution!")
    }
    
    return(eff)
    
  } else if (is.dea_fuzzy(deasol)) {
    
    dmunames_eval <- names(deasol$dmu_eval)
    nde <- length(deasol$dmu_eval)
    nalpha <- length(deasol$alpha)
    
    if (grepl("kaoliu", deasol$modelname)) {
      
      if ("efficiency" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {
        
        neff <- length(deasol$alphacut[[1]]$DMU$Worst[[1]]$efficiency)
        
        if (neff == 1) {
          
          eff.W <- matrix(0, nrow = nde, ncol = nalpha)
          rownames(eff.W) <- dmunames_eval
          colnames(eff.W) <- names(deasol$alphacut)
          eff.B <- eff.W
          
          for (i in 1:nalpha) {
            eff.W[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
              x$efficiency))
            eff.B[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
              x$efficiency))
          }
          
        } else {
          
          eff.W <- array(0,
                         dim = c(nde, neff + 1, nalpha),
                         dimnames = list(dmunames_eval,
                                         c(names(deasol$alphacut[[1]]$DMU$Worst[[1]]$efficiency), "mean_efficiency"),
                                         names(deasol$alphacut)))
          eff.B <- eff.W
          
          for (i in 1:nalpha) {
            eff.W[, , i] <- cbind(
              do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
                x$efficiency)),
              unlist(lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
                x$mean_efficiency))
            )
            eff.B[, , i] <- cbind(
              do.call(rbind, lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
                x$efficiency)),
              unlist(lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
                x$mean_efficiency))
            )
          }
          
          #for (k in 1:nalpha) {
          #  for (i in 1:nde) {
          #    for (j in 1:neff) {
          #      eff.W[i, j, k] <- deasol$alphacut[[k]]$DMU$Worst[[i]]$efficiency[j]
          #      eff.B[i, j, k] <- deasol$alphacut[[k]]$DMU$Best[[i]]$efficiency[j]
          #    }
          #    eff.W[i, neff + 1, k] <- deasol$alphacut[[k]]$DMU$Worst[[i]]$mean_efficiency
          #    eff.B[i, neff + 1, k] <- deasol$alphacut[[k]]$DMU$Best[[i]]$mean_efficiency
          #  }
          #}
          
        }
        
      } else if ("beta" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {
        
        eff.W <- matrix(0, nrow = nde, ncol = nalpha)
        rownames(eff.W) <- dmunames_eval
        colnames(eff.W) <- names(deasol$alphacut)
        eff.B <- eff.W
        
        for (i in 1:nalpha) {
          eff.W[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$beta))
          eff.B[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$beta))
        }
        
      } else if ("delta" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {
        
        eff.W <- matrix(0, nrow = nde, ncol = nalpha)
        rownames(eff.W) <- dmunames_eval
        colnames(eff.W) <- names(deasol$alphacut)
        eff.B <- eff.W
        
        for (i in 1:nalpha) {
          eff.W[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$delta))
          eff.B[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$delta))
        }
        
      } else if ("objval" %in% names(deasol$alphacut[[1]]$DMU$Worst[[1]])) {
        
        eff.W <- matrix(0, nrow = nde, ncol = nalpha)
        rownames(eff.W) <- dmunames_eval
        colnames(eff.W) <- names(deasol$alphacut)
        eff.B <- eff.W
        
        for (i in 1:nalpha) {
          eff.W[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Worst, function(x)
            x$objval))
          eff.B[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU$Best, function(x)
            x$objval))
        }
        
      } else {
        stop("No efficiency/beta/delta/objval parameters in this solution!")
      }
      
      return(list(Worst = eff.W, Best = eff.B))
      
    } else if (grepl("possibilistic", deasol$modelname)) {
      
      if ("efficiency" %in% names(deasol$alphacut[[1]]$DMU[[1]])) {
        
        neff <- length(deasol$alphacut[[1]]$DMU[[1]]$efficiency)
        
        if (neff == 1) {
          
          eff <- matrix(0, nrow = nde, ncol = nalpha)
          rownames(eff) <- dmunames_eval
          colnames(eff) <- names(deasol$alphacut)
          
          for (i in 1:nalpha) {
            eff[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU, function(x)
              x$efficiency))
          }
          
        } else {
          
          eff <- array(0,
                       dim = c(nde, neff + 1, nalpha),
                       dimnames = list(dmunames_eval,
                                       c(names(deasol$alphacut[[1]]$DMU[[1]]$efficiency), "mean_efficiency"),
                                       names(deasol$alphacut)))
          
          for (i in 1:nalpha) {
            eff[, , i] <- cbind(
              do.call(rbind, lapply(deasol$alphacut[[i]]$DMU, function(x)
                x$efficiency)),
              unlist(lapply(deasol$alphacut[[i]]$DMU, function(x)
                x$mean_efficiency))
            )
          }
          
        }
        
      } else if ("beta" %in% names(deasol$alphacut[[1]]$DMU[[1]])) {
        
        eff <- matrix(0, nrow = nde, ncol = nalpha)
        rownames(eff) <- dmunames_eval
        colnames(eff) <- names(deasol$alphacut)
        
        for (i in 1:nalpha) {
          eff[, i] <- unlist(lapply(deasol$alphacut[[i]]$DMU, function(x)
            x$beta))
        }
      
      } else {
        stop("No efficiency/beta parameters in this solution!")
      }
      
      return(eff)
      
    } else if (grepl("guotanaka", deasol$modelname)) {
      
      eff <- array(0,
                   dim = c(nde, 3, nalpha),
                   dimnames = list(dmunames_eval,
                                   names(deasol$alphacut[[1]]$DMU[[1]]$efficiency),
                                   names(deasol$alphacut)))
      
      for (i in 1:nalpha) {
        eff[, , i] <- cbind(
          do.call(rbind, lapply(deasol$alphacut[[i]]$DMU, function(x)
            x$efficiency)),
          unlist(lapply(deasol$alphacut[[i]]$DMU, function(x)
            x$mean_efficiency))
        )
        
      }
      
      return(eff)
      
    }
    
  } else {
    stop("Input should be a dea or dea_fuzzy class object!")
  }
  
}