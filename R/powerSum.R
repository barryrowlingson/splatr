#' a powered sum covariate
#'
#' @param f formula - first covariate is baseline
#' @param fixed list of fixed parameter values
#' @param starting starting values for optimiser
#' @param prefixes names to give the parameters
#'
#' @return a function of a data frame. Calling this creates a function of the parameters.
#' @export

powerSum <- function(f, fixed=NULL, starting=NULL, prefixes=c("beta","gamma")){

  force(f);force(fixed);force(starting)

  makeF <- function(data){

    mm = model.matrix(f,data)
    if(ncol(mm)<=2){
      stop("Need more than one covariate for power-sum")
    }

    if(!is.null(attr(mm,"contrasts"))){
      stop("Covariates must be numeric, not factors")
    }

    ## drop the intercept term
    mm = mm[,-1,drop=FALSE]
  
    ##paramLabels = c(paste(prefix,colnames(mm)[-1],sep="."),"gamma")

    parameters = paramInfo(
      list(name=prefixes[1],label=paste(prefixes[1],colnames(mm)[-1],sep=".")), # the beta parameter(s)
      list(name=prefixes[2],label=prefixes[2]),                                 # the gamma parameter
      fixed=fixed,
      starting=starting
      )

  ## checks:
  ## fixed is a list with names from prefixes, lengths equal to lengths of labels
  ## starting is similar
  ## names can't appear in starting and fixed

    
    ## this is a function of the variable parameters only
    F = function(i, variables){
      P = mergeFixed(parameters,variables)
      gamma = P[prefixes[2]]
      betas = exp(P[-match(prefixes[2],names(P))]) # betas constrained positive
      return(as.vector((mm[i,,drop=FALSE]^gamma) %*% c(1,betas))) # 1 for the baseline variable
    }
    attr(F,"formula") <- "powered sum"
    class(F)=c("pfunc","function")
    F
  }
  class(makeF)=c("pfuncmaker","function")
  makeF
  
}
