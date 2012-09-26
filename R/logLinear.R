#' log-linear terms
#'
#' @param f formula
#' @param starting list of starting values
#' @param prefixes prefix of the variable name
#'
#'
#' @export

logLinear <- function(f, starting=NULL,prefixes="beta"){
###
### note fixed parameters are not allowed, use offset(z) in the formula
###
  force(f);force(starting);

  makeF <- function(data){
  
    mm = model.matrix(f,data)
    mf = model.frame(f,data)

    offset = model.offset(mf)
    if(is.null(offset)){
      offset=rep(0,nrow(data))
    }
  
    if(any(attr(mm,"assign")==0)){
      intercept=TRUE
    }else{
      intercept=FALSE
    }
  
    if(!is.null(attr(mm,"contrasts"))){
      stop("Covariates must be numeric, not factors (for now). Convert by hand to binary variables or help rewrite the code so it does!")
    }

    labels = colnames(mm)

    if(!is.null(labels)){
      labels = paste(prefixes,labels,sep=".")
    }
  
    parameters = paramInfo(
      list(name=prefixes, label=labels),
      fixed=NULL,
      starting=starting
      )
    
    F = function(i, variables){
      P = mergeFixed(parameters,variables)
      return(offset[i]+as.vector(mm[i,,drop=FALSE] %*% P))
    }
    F
  }
  makeF
}
