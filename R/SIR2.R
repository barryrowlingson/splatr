###
### SIR2 class models
###
### canonical example, FMD
###

#' SIR2 models
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
}
