###
### SIR2 class models
###
### canonical example, FMD
###

#' SIR2 models
#'
#' returns a likelihood function
#'
#' @export
#'
likSIR2 = function(data,times,cov,distDecay){
  ncase = nrow(data)
  if(inherits(times,"formula")){
    IRtimes = model.frame(times,data)
  }else{
    IRtimes = times
  }
  if(ncol(IRtimes)!=2){
    stop("Incorrect specification of Infection/Removal times")
  }
  if(nrow(IRtimes)!=ncase){
    stop("Wrong number of Infection/Removal times - ",nrow(IRtimes), " times with ",ncase," data")
  }

  Ifunc = infectivity(IRtimes)
  Sfunc = susceptible(IRtimes)
  
  ## using na.last=NA means the non-cases don't appear.
  caseOrder = order(IRtimes[,1],na.last=NA)
  
  f = distDecay(data)
  covFlist = sapply(cov,function(A){A(data)})
  nVarsEach = c(nvariables(f),sapply(covFlist,nvariables))
  totalVars = sum(nVarsEach)
  nPars = length(nVarsEach)
  breakMap = makeBreakMap(nVarsEach)
  
  F = function(params=numeric(0)){
    if(length(params)!=totalVars){
      stop("Incorrect number of parameters")
    }
    ##
    ## loop over all the cases
    ##
    logP = rep(0,length(caseOrder))
    for(icase in 1:length(caseOrder)){
      ## case is data index
      case = caseOrder[icase]
      ## get this case's infection time:
      t = IRtimes[case,1]
      ## what else is infective at this point:
      Infs = Ifunc(t)
      ## current case can't be infective:
      Infs[case]=FALSE

      if(!any(Infs)){
        next ## no infecting cases, logP[case] = 0
      }

      ## get all susceptibles at this time
      Suss = Sfunc(t)
      ## our case must be susceptible
      Suss[case]=TRUE
      ## compute lambda for all the susceptibles,
      ## then P is lambda[the case]/sum(lambdas)
      
      susCases = which(Suss) ## data indices of susceptibles
      lambda = rep(NA,length(susCases))
      for(isus in 1:length(lambda)){
        lambda[isus]=1
      }
      logP[isus]=lambda[icase]/sum(lambda)
    }
    return(sum(logP))
  }
  F

}
