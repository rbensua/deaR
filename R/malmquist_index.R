#' @title Malmquist index
#'   
#' @description This function calculates the conventional input/output oriented Malmquist index under variable return-to-scale.
#' 
#' @note In the results: EC = Efficiency Change, PTEC = Pure Technical Efficiency Change, SEC = Scale Efficiency Change, TC = Technological Change, MI = Malmquist Index 
#' @usage malmquist_index(datadealist,
#'                        dmu_eval = NULL,
#'                        dmu_ref = NULL,
#'                        orientation = c("io", "oo"),
#'                        type1 = c("cont", "seq", "glob"),
#'                        type2 = c("fgnz", "rd", "gl"))
#' 
#' @param datadealist A list with the data at different times, including DMUs, inputs and outputs.
#' @param dmu_eval A numeric vector containing which DMUs have to be evaluated.
#' @param dmu_ref A numeric vector containing which DMUs are the evaluation reference set.
#' @param orientation A string, equal to "io" (input oriented) or "oo" (output oriented).
#' @param type1 A string, equal to "cont" (contemporary), "seq" (sequential) or "glob"
#' (global).
#' @param type2 A string, equal to "fgnz" (Färe et al. 1994), "rd" (Ray and Desli 1997) or
#' "gl" (generalized).
#'   
#' @return A numeric list with Malmquist index and efficiencies.
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
#' @references 
#' Caves, D.W.; Christensen, L. R. y Diewert, W.E. (1982). “The Economic Theory of Index Numbers and the Measurement of Input, Output, and Productivity”. Econometrica, 50(6), 1393-1414. 
#'  
#' Färe, R.; Grosskopf, S.; Lindgren, B.; Roos, P. (1989). “Productivity Developments in Swedish Hospitals: A Malmquist Output Index Approach”. Discussion paper nº 89-3. Southern Illinois University. Illinois.
#' 
#' Färe, R.; Grosskopf, S.; Lindgren, B.; Roos, P. (1992). “Productivity changes in Swedish Pharmacies 1980-89: A nonparametric Malmquist Approach”. Journal of productivity Analysis, 3(3), 85-101. 
#' 
#' Färe, R.; Grosskopf, S.; Norris, M. y Zhang, Z. (1994). “Productivity Growth, Technical Progress, and Efficiency Change in Industrialized Countries”. American Economic Review, 84(1), 66-83. 

#' @examples 
#' # Example 1. With dataset in wide format.
#' # Replication of results in Wang and Lan (2011, p. 2768)
#' data("Economy")
#' data_example <- read_malmquist(datadea = Economy,
#'                                nper = 5, 
#'                                arrangement = "horizontal",
#'                                ni = 2, 
#'                                no = 1)
#' result <- malmquist_index(data_example, orientation = "io")
#' mi <- result$mi
#' effch <- result$ec
#' tech <- result$tc
#' 
#' # Example 2. With dataset in long format.
#' # Replication of results in Wang and Lan (2011, p. 2768)
#' data("EconomyLong")
#' data_example2 <- read_malmquist(EconomyLong,
#'                                 percol = 2, 
#'                                 arrangement = "vertical",
#'                                 inputs = 3:4, 
#'                                 outputs = 5)
#' result2 <- malmquist_index(data_example2, orientation = "io")
#' mi2 <- result2$mi
#' effch2 <- result2$ec
#' tech2 <- result2$tc
#' 
#' @import lpSolve
#' 
#' @export
  
malmquist_index <- function(datadealist,
                            dmu_eval = NULL,
                            dmu_ref = NULL,
                            orientation = c("io", "oo"),
                            type1 = c("cont", "seq", "glob"),
                            type2 = c("fgnz", "rd", "gl")) {
  
  nt <- length(datadealist)
  
  if (nt < 2) {
    stop("Number of times should be >= 2.")
  }
  
  # Cheking data
  for (t in 1:nt) {
    if (!is.deadata(datadealist[[t]])) {
      stop("Data should be of class deadata. Run read_data function first!")
    }
  }
  
  dmunames <- datadealist[[1]]$dmunames
  nd <- length(dmunames) # number of dmus
  for (t in 2:nt) {
    if (nd != length(datadealist[[t]]$dmunames)) {
      stop("Number of DMUs does not coincide.")
    }
  }
  
  if (is.null(dmu_eval)) {
    dmu_eval <- 1:nd
  } else if (all(dmu_eval %in% (1:nd)) == FALSE) {
    stop("Invalid set of DMUs to be evaluated (dmu_eval).")
  }
  names(dmu_eval) <- dmunames[dmu_eval]
  nde <- length(dmu_eval)
  
  if (is.null(dmu_ref)) {
    dmu_ref <- 1:nd
  } else if (all(dmu_ref %in% (1:nd)) == FALSE) {
    stop("Invalid set of reference DMUs (dmu_ref).")
  }
  names(dmu_ref) <- dmunames[dmu_ref]
  ndr <- length(dmu_ref)
  
  ni <- nrow(datadealist[[1]]$input) # number of inputs
  no <- nrow(datadealist[[1]]$output) # number of outputs
  for (t in 2:nt) {
    if (ni != nrow(datadealist[[t]]$input)) {
      stop("Number of inputs does not coincide.")
    }
    if (no != nrow(datadealist[[t]]$output)) {
      stop("Number of outputs does not coincide.")
    }
  }
  
  # Checking types
  type1 <- tolower(type1)
  type1 <- match.arg(type1)
  type2 <- tolower(type2)
  type2 <- match.arg(type2)
  
  # Checking orientation
  orientation <- tolower(orientation)
  orientation <- match.arg(orientation)
  if (orientation == "io") {
    input <- array(0, dim = c(ni, nd, nt))
    output <- array(0, dim = c(no, nd, nt))
    for (t in 1:nt) {
      input[, , t] <- datadealist[[t]]$input
      output[, , t] <- datadealist[[t]]$output
    }
    obj <- "min"
  } else {
    ni <- nrow(datadealist[[1]]$output)
    no <- nrow(datadealist[[1]]$input)
    input <- array(0, dim = c(ni, nd, nt))
    output <- array(0, dim = c(no, nd, nt))
    for (t in 1:nt) {
      input[, , t] <- -datadealist[[t]]$output
      output[, , t] <- -datadealist[[t]]$input
    }
    obj <- "max"
  }
  
  mi <- matrix(0, nrow = nt - 1, ncol = nde)
  colnames(mi) <- dmunames[dmu_eval]
  eff <- matrix(0, nrow = nt, ncol = nde)
  colnames(eff) <- dmunames[dmu_eval]
  effv <- eff
  
  if (type2 == "fgnz") {
    eff12 <- mi # DMU adelantada
    eff21 <- mi # Frontera adelantada
  } else if (type2 == "rd") {
    eff12 <- mi # DMU adelantada
    effv12 <- mi # DMU adelantada vrs
  } else if (type2 == "gl") {
    eff12y <- mi # DMU output adelantado
    effv12 <- mi # DMU adelantada vrs
    effv12y <- mi # DMU output adelantado vrs
  }
  
  if (type1 == "cont") {
    
    f.obj <- c(1, rep(0, ndr))
    f.dir <- c(rep("<=", ni), rep(">=", no))
    f.dirv <- c(f.dir, "=")
    f.con.vrs <- cbind(0, matrix(1, nrow = 1, ncol = ndr))
    
    for (i in 1:nde) {
      
      ii <- dmu_eval[i]
      
      f.con.1 <- cbind(-input[, ii, 1], 
                       matrix(input[, dmu_ref, 1], nrow = ni))
      f.con.2 <- cbind(matrix(0, nrow = no, ncol = 1),
                       matrix(output[, dmu_ref, 1], nrow = no))
      f.con1 <- rbind(f.con.1, f.con.2)
      
      f.rhs1 <- c(rep(0, ni), output[, ii, 1])
      
      f.con1v <- rbind(f.con1, f.con.vrs)
      f.rhs1v <- c(f.rhs1, 1)
      
      eff[1, i] <- lp(obj, f.obj, f.con1, f.dir, f.rhs1)$objval
      effv[1, i] <- lp(obj, f.obj, f.con1v, f.dirv, f.rhs1v)$objval
      
      for (t in 2:nt) {
        
        f.con.1 <- cbind(-input[, ii, t],
                         matrix(input[, dmu_ref, t], nrow = ni))
        f.con.2 <- cbind(matrix(0, nrow = no, ncol = 1),
                         matrix(output[, dmu_ref, t], nrow = no))
        f.con2 <- rbind(f.con.1, f.con.2)
        
        f.rhs2 <- c(rep(0, ni), output[, ii, t])
        
        f.con2v <- rbind(f.con2, f.con.vrs)
        f.rhs2v <- c(f.rhs2, 1)
        
        eff[t, i] <- lp(obj, f.obj, f.con2, f.dir, f.rhs2)$objval
        effv[t, i] <- lp(obj, f.obj, f.con2v, f.dirv, f.rhs2v)$objval
        
        # Intertemporal scores
        
        f.con12 <- cbind(f.con2[, 1], f.con1[, -1])
        if (type2 == "fgnz") {
          f.con21 <- cbind(f.con1[, 1], f.con2[, -1])
          eff12[t - 1, i] <- lp(obj, f.obj, f.con12, f.dir, f.rhs2)$objval
          eff21[t - 1, i] <- lp(obj, f.obj, f.con21, f.dir, f.rhs1)$objval
          f.rhs1 <- f.rhs2
        } else if (type2 == "rd") {
          f.con12v <- rbind(f.con12, f.con.vrs)
          eff12[t - 1, i] <- lp(obj, f.obj, f.con12, f.dir, f.rhs2)$objval
          effv12[t - 1, i] <- lp(obj, f.obj, f.con12v, f.dirv, f.rhs2v)$objval
        } else if (type2 == "gl") {
          f.con12v <- rbind(f.con12, f.con.vrs)
          eff12y[t - 1, i] <- lp(obj, f.obj, f.con1, f.dir, f.rhs2)$objval
          effv12[t - 1, i] <- lp(obj, f.obj, f.con12v, f.dirv, f.rhs2v)$objval
          effv12y[t - 1, i] <- lp(obj, f.obj, f.con1v, f.dirv, f.rhs2v)$objval
          f.con1v <- f.con2v
        }
        f.con1 <- f.con2
        
      }
      
    }
    
  } else if (type1 == "seq") {
    
    f.dir <- c(rep("<=", ni), rep(">=", no))
    f.dirv <- c(f.dir, "=")
    
    for (i in 1:nde) {
      
      ii <- dmu_eval[i]
      f.obj1 <- c(1, rep(0, ndr))
      inputfront1 <- matrix(input[, dmu_ref, 1], nrow = ni)
      outputfront1 <- matrix(output[, dmu_ref, 1], nrow = no)
      
      f.con.1 <- cbind(-input[, ii, 1], inputfront1)
      f.con.2 <- cbind(matrix(0, nrow = no, ncol = 1), outputfront1)
      f.con1 <- rbind(f.con.1, f.con.2)
      
      f.rhs1 <- c(rep(0, ni), output[, ii, 1])
      
      f.con1v <- rbind(f.con1, cbind(0, matrix(1, nrow = 1, ncol = ndr)))
      f.rhs1v <- c(f.rhs1, 1)
      
      eff[1, i] <- lp(obj, f.obj1, f.con1, f.dir, f.rhs1)$objval
      effv[1, i] <- lp(obj, f.obj1, f.con1v, f.dirv, f.rhs1v)$objval
      
      for (t in 2:nt) {
        
        f.obj2 <- c(f.obj1, rep(0, ndr))
        inputfront2 <- cbind(inputfront1, matrix(input[, dmu_ref, t], nrow = ni))
        outputfront2 <- cbind(outputfront1, matrix(output[, dmu_ref, t], nrow = no))
        
        f.con.1 <- cbind(-input[, ii, t], inputfront2)
        f.con.2 <- cbind(matrix(0, nrow = no, ncol = 1), outputfront2)
        f.con2 <- rbind(f.con.1, f.con.2)
        
        f.rhs2 <- c(rep(0, ni), output[, ii, t])
        
        f.con2v <- rbind(f.con2, cbind(0, matrix(1, nrow = 1, ncol = t * ndr)))
        f.rhs2v <- c(f.rhs2, 1)
        
        eff[t, i] <- lp(obj, f.obj2, f.con2, f.dir, f.rhs2)$objval
        effv[t, i] <- lp(obj, f.obj2, f.con2v, f.dirv, f.rhs2v)$objval
        
        # Intertemporal scores
        
        f.con12 <- cbind(f.con2[, 1], f.con1[, -1])
        if (type2 == "fgnz") {
          f.con21 <- cbind(f.con1[, 1], f.con2[, -1])
          eff12[t - 1, i] <- lp(obj, f.obj1, f.con12, f.dir, f.rhs2)$objval
          eff21[t - 1, i] <- lp(obj, f.obj2, f.con21, f.dir, f.rhs1)$objval
          f.rhs1 <- f.rhs2
        } else if (type2 == "rd") {
          f.con12v <- rbind(f.con12, cbind(0, matrix(1, nrow = 1, ncol = (t - 1) * ndr)))
          eff12[t - 1, i] <- lp(obj, f.obj1, f.con12, f.dir, f.rhs2)$objval
          effv12[t - 1, i] <- lp(obj, f.obj1, f.con12v, f.dirv, f.rhs2v)$objval
        } else if (type2 == "gl") {
          f.con12v <- rbind(f.con12, cbind(0, matrix(1, nrow = 1, ncol = (t - 1) * ndr)))
          eff12y[t - 1, i] <- lp(obj, f.obj1, f.con1, f.dir, f.rhs2)$objval
          effv12[t - 1, i] <- lp(obj, f.obj1, f.con12v, f.dirv, f.rhs2v)$objval
          effv12y[t - 1, i] <- lp(obj, f.obj1, f.con1v, f.dirv, f.rhs2v)$objval
          f.con1v <- f.con2v
        }
        f.con1 <- f.con2
        f.obj1 <- f.obj2
        inputfront1 <- inputfront2
        outputfront1 <- outputfront2
        
      }
      
    }
    
  } else if (type1 == "glob") {
    
    f.obj <- c(1, rep(0, nt * ndr))
    f.dir <- c(rep("<=", ni), rep(">=", no))
    f.dirv <- c(f.dir, "=")
    f.con.vrs <- cbind(0, matrix(1, nrow = 1, ncol = nt * ndr))
    inputfront <- matrix(0, nrow = ni, ncol = nt * ndr)
    outputfront <- matrix(0, nrow = no, ncol = nt * ndr)
    for (t in 1:nt) {
      inputfront[,((t - 1) * ndr + 1):(t * ndr)] <- matrix(input[, dmu_ref, t], nrow = ni)
      outputfront[,((t - 1) * ndr + 1):(t * ndr)] <- matrix(output[, dmu_ref, t], nrow = no)
    }
    f.con.2 <- cbind(matrix(0, nrow = no, ncol = 1), outputfront)
    
    for (i in 1:nde) {
      
      ii <- dmu_eval[i]
      
      f.con.1 <- cbind(-input[, ii, 1], inputfront)
      f.con1 <- rbind(f.con.1, f.con.2)
      
      f.rhs1 <- c(rep(0, ni), output[, ii, 1])
      
      f.con1v <- rbind(f.con1, f.con.vrs)
      f.rhs1v <- c(f.rhs1, 1)
      
      eff[1, i] <- lp(obj, f.obj, f.con1, f.dir, f.rhs1)$objval
      effv[1, i] <- lp(obj, f.obj, f.con1v, f.dirv, f.rhs1v)$objval
      
      for (t in 2:nt) {
        
        f.con.1 <- cbind(-input[, ii, t], inputfront)
        f.con2 <- rbind(f.con.1, f.con.2)
        
        f.rhs2 <- c(rep(0, ni), output[, ii, t])
        
        f.con2v <- rbind(f.con2, f.con.vrs)
        f.rhs2v <- c(f.rhs2, 1)
        
        eff[t, i] <- lp(obj, f.obj, f.con2, f.dir, f.rhs2)$objval
        effv[t, i] <- lp(obj, f.obj, f.con2v, f.dirv, f.rhs2v)$objval
        
        # Intertemporal scores
        
        if (type2 == "fgnz") {
          f.con21 <- cbind(f.con1[, 1], f.con2[, -1])
          eff12[t - 1, i] <- eff[t, i]
          eff21[t - 1, i] <- lp(obj, f.obj, f.con21, f.dir, f.rhs1)$objval
          f.rhs1 <- f.rhs2
        } else if (type2 == "rd") {
          eff12[t - 1, i] <- eff[t, i]
          effv12[t - 1, i] <- effv[t, i]
        } else if (type2 == "gl") {
          eff12y[t - 1, i] <- lp(obj, f.obj, f.con1, f.dir, f.rhs2)$objval
          effv12[t - 1, i] <- effv[t, i]
          effv12y[t - 1, i] <- lp(obj, f.obj, f.con1v, f.dirv, f.rhs2v)$objval
          f.con1v <- f.con2v
        }
        f.con1 <- f.con2
        
      }
      
    }
    
  }
  
  if (orientation == "oo") {
    eff <- 1 / eff
    effv <- 1 / effv
    if (type2 == "fgnz") {
      eff12 <- 1 / eff12
      eff21 <- 1 / eff21
    } else if (type2 == "rd") {
      eff12 <- 1 / eff12
      effv12 <- 1 / effv12
    } else if (type2 == "gl") {
      eff12y <- 1 / eff12y
      effv12 <- 1 / effv12
      effv12y <- 1 / effv12y
    }
  }
  
  if (type2 == "fgnz") {
    mi <- sqrt((eff12 * eff[-1, ]) / (eff[-nt, ] * eff21))
    ec <- eff[-1, ] / eff[-nt, ]
    tc <- mi / ec
    pech <- effv[-1, ] / effv[-nt, ]
    sech <- ec / pech
  } else if (type2 == "rd") {
    tc <- effv12 / effv[-1, ]
    pech <- effv[-1, ] / effv[-nt, ]
    sech <- (effv[-nt, ] / eff[-nt, ]) / (effv12 / eff12) 
    mi <- tc * pech * sech
  } else if (type2 == "gl") {
    tc <- effv12 / effv[-1, ]
    pech <- effv[-1, ] / effv[-nt, ]
    sech <- (effv[-nt, ] / eff[-nt, ]) / (effv12y / eff12y) 
    mi <- tc * pech * sech
  }
  
  deaOutput <- list(mi = mi,
                    tc = tc,
                    pech = pech,
                    sech = sech,
                    #eff = eff,
                    #effv = effv,
                    datadealist = datadealist,
                    dmu_eval = dmu_eval,
                    dmu_ref = dmu_ref,
                    orientation = orientation,
                    type1 = type1,
                    type2 = type2,
                    modelname = "malmquist")
  return(structure(deaOutput, class = "dea"))
  
}