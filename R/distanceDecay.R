
#' four-parameter distance-decay function
#'
#' this returns a function that when bound to data returns a
#' function of i and a vector of length 4 minus number of fixed params
#'
#' note this is the:
#'
#' f = nu*exp(-(d/phi)^kappa + rho
#'
#' formula used in the FnM paper. Hence f(0)->rho+1
#'
#' @param f a formula on the data to return a 2-column matrix
#' @param fixed named vector of fixed parameters
#' @param starting named vector of starting parameters
#' @param names vector of length 4 of alternate variable names
#'
expDecay <- function(f,fixed=NULL,starting=NULL, names=c("nu","phi","rho","kappa")){
  force(f);force(fixed);force(starting);force(names)

  parameters = paramInfo(
    list(name=names[1],label=names[1]), # nu
    list(name=names[2],label=names[2]), # phi
    list(name=names[3],label=names[3]), # rho
    list(name=names[4],label=names[4]), # kappa
    fixed = fixed,
    starting=starting
    )
  
  makeED <- function(data){
    if(f[[2]]=="."){ # formula was ~.
      coords = coordinates(data)
    }else{
      coords = model.frame(f,data)
    }
     
    F = function(i,j,variables){
      stopifnot(length(i)==1)
      ## return distance-decay function from i to all of j
      d = sqrt((coords[i,1]-coords[j,1])^2 + (coords[i,2]-coords[j,2])^2)
      P = mergeFixed(parameters,variables)
      return(P[1]*exp(-(d/P[2])^P[4])+P[3])
    }
    class(F)=c("pfunc","function")
    F
  }
  class(makeED)=c("pfuncmaker","function")
  makeED
}
