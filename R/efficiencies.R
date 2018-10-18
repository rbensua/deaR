
#' @export
efficiencies <- function(x,...){
  UseMethod("efficiencies",x)
}

efficiencies.default <- function(x,...){
  "Unknown class"
}
