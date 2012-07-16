#' compare fixed list and paramNames, return param info
#'

.checkFixed <- 
function(fixed,paramNames,unnamed=0){

  fixNames = .rnames(fixed)
  
  ## the number of unnamed parameters in fixed must either be none or equal to unnamed
  
  nUns = sum(fixNames=="")
  if(nUns != 0  & nUns != unnamed){
    stop("Wrong number of unnamed parameters - should be zero or ",unnamed)
  }
  ## all non-"" names in fixed must be in paramNames
  givenFixNames = fixNames[fixNames!=""]
  checkNames = givenFixNames %in% paramNames
  if(!all(checkNames)){
    stop("Fixed parameters not in parameter names: ",paste(givenFixNames[!checkNames],collapse=","))
  }

  if(nUns!=0){
    fixNames[1:nUns]=paramNames[1:nUns]
  }
  freeVector = !(paramNames %in% fixNames)

  placeHolder=rep(NA,length(paramNames))
  names(placeHolder)=paramNames
  placeHolder[fixNames]=fixed
  
  return(list(pnames=paramNames,free=freeVector, values=placeHolder))
  
}

.rnames <- 
function(x){
  if(length(x)==0){
    return(character(0))
  }
  if(is.null(names(x))){
    return(rep("", length(x)))
  }
  return(names(x))
}


.fixParamInfo <- function(paramInfo,fixed,starting){
  ##
  ##
  ## paramInfo is the description of the parameters for the generated functions
  ##
  ## fixed is a list with names from pnames, lengths equal to lengths of labels
  ## starting is similar
  ## names can't appear in starting and fixed

  if(!all(names(fixed) %in% pnames)){
    stop("Fixed parameter name not in parameters")
  }

  if(!all(names(starting) %in% pnames)){
    stop("Starting value name not in parameters")
  }


  
}

#' collate the parameter info
#'
#' paramInfo's main job is to compute the mask for fixed/variable values
#'
paramInfo <- function(...,fixed=NULL,starting=NULL){
  p = list(...)

  ## parameters need a name...
  if(any(unlist(lapply(p,function(x){is.null(x$name)})))){
    stop("All parameters need a name")
  }
  
  pnames = unlist(lapply(p,function(x){x$name})) # parameter names
  pvnames = unlist(lapply(p,function(x){rep(x$name,length(x$label))}))
  nvalues = sum(unlist(lapply(p,function(x){length(x$label)*length(x$null)}))) # how many values...
  nparams = length(p) # how many parameters (some of which may be vectors)

  ## check fixed names are all in the parameter names
  checkNa = is.na(match(names(fixed),pnames))
  if(any(checkNa)){
    stop("Unknown fixed parameter: ",paste(names(fixed)[checkNa],collapse=",")," not in ",paste(pnames,collapse=","))
  }

  ## don't bother checking the starting values, that's the business of the fitting function, it may have some
  ## sensible defaults.
  
  variableMask = logical(nvalues)
  pos = 1
  fixedValues = NULL
  for(ip in 1:nparams){
    if(!is.null(p[[ip]]$label)){
      if(p[[ip]]$name %in% names(fixed)){
        variableMask[pos:(pos+length(p[[ip]]$label)-1)]=FALSE
        fixedValues = c(fixedValues,fixed[[p[[ip]]$name]])
      }else{
        variableMask[pos:(pos+length(p[[ip]]$label)-1)]=TRUE
      }
      pos = pos + length(p[[ip]]$label)
    }
    
  }

  
  
  pi = list(params=p,
    pvnames=pvnames,
    fixed=fixed,
    starting=starting,
    nvalues=nvalues,
    variableMask = variableMask,
    fixedValues=fixedValues)
  
  class(pi) <- c("paramInfo","list")
  pi
}

pnames <- function(pi){
  UseMethod("pnames")
}

pnames.paramInfo <- function(pi){
  unlist(lapply(pi$params,function(x){x$name}))
}

mergeFixed <- function(pi,variables){
  if(length(variables)!=sum(pi$variableMask)){
    stop("Wrong number of values")
  }
  values = numeric(pi$nvalues)
  values[pi$variableMask]=variables
  values[!(pi$variableMask)]=pi$fixedValues
  names(values)=pi$pvnames
  return(values)
}
