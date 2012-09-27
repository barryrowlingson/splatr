
#' three-parameter distance-decay function
#'
#' this returns a function that when bound to data returns a
#' function of i and a vector of length 3 minus number of fixed params
#'
#' @param f a formula on the data to return a 2-column matrix
#' @param fixed named vector of fixed parameters
#' @param starting named vector of starting parameters
#' @param names vector of length 3 of alternate variable names
#'
expDecay <- function(f,fixed,starting=NULL, names=c("phi","rho","kappa")){
  force(f);force(fixed);force(starting);force(names)

  makeED <- function(data){
    if(f[[2]]=="."){ # formula was ~.
      coords = coordinates(data)
    }else{
      coords = model.frame(f,data)
    }
    parameters = paramInfo(
      list(name=names[1],label=names[1]),
      list(name=names[2],label=names[2]),
      list(name=names[3],label=names[3]),
      fixed = fixed,
      starting=starting
      )
      
    F = function(i,j,variables){
      ## return distance-decay function from i to all of j
      d = sqrt((coords[i,1]-coords[j,1])^2 + (coords[i,2]-coords[j,2])^2)
      P = mergeFixed(parameters,variables)
      return(exp(-(d/P[1])^P[3])+P[2])
    }
    F
  }
  makeED
}
