#' log-linear terms
#'
#' @param data data frame for evaluation
#' @param f formula
#' @param fixed list of fixed parameters
#' @param starting list of starting values
#' @param prefixes prefix of the variable name
#' 
#' @export

logLinear <- function(data, f, fixed=NULL, starting=NULL,prefixes="beta"){

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
    stop("Covariates must be numeric, not factors (for now)")
  }

  labels = colnames(mm)

  if(!is.null(labels)){
    labels = paste(prefixes,labels,sep=".")
  }
  
  parameters = paramInfo(
    list(name=prefixes, label=labels),
    fixed=fixed,
    starting=starting
    )
  
  F = function(i, variables){
    P = mergeFixed(parameters,variables)
    return(offset[i]+as.vector(mm[i,,drop=FALSE] %*% P))
  }
  F
  
}
