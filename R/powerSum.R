#' a powered sum covariate
#'
#' @export

powerSum <- function(data, f, fixed=NULL, starting=NULL, prefixes=c("beta","gamma")){

  force(data);force(f);force(fixed);force(starting)

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
  F
}
