##' time-series plot for SIR epidemic
##'
##' displays counts of cases in each of the three states over time
##' @title SIR Time-series plot
##' @param data data frame with infection and removal times
##' @param times formula of infection and removal times, or 2-column matrix
##' @param tlim lower and upper limit of plot
##' @param nt number of time steps
##' @param colours colour for S, I, and R states
##' @author Barry Rowlingson

plotSIR <- function(data,times,
                    tlim=range(IRtimes,na.rm=TRUE),nt=50,
                    colours =c("#00FF00","#FF0000","#000000"),
                    pos="topright"){
  if(inherits(times,"formula")){
    IRtimes = model.frame(times,data,na.action=na.pass)
  }else{
    IRtimes = times
  }

  tseq = seq(min(tlim),max(tlim),length=nt)
  ssf = SIRt(IRtimes)(tseq)

  matplot(tseq,t(ssf),type="l",lty=1,lwd=c(6,3,1),xlab="Time",ylab="Count",col=colours)
  legend(x=pos,legend=c("S","I","R"),lty=c(1,1,1),col=colours,lwd=c(6,6,6))
  invisible(0)

}

##' map an SIR model at a given time
##'
##' produce a map of an SIR model at a given time
##' @title SIR map
##' @param data data frame with infection and removal times
##' @param Ifunc infectivity function
##' @param Sfunc susceptibility function
##' @param t time point
##' @author Barry Rowlingson
plotSIRat <- function(data,Ifunc,Sfunc,t){
  if(inherits(data,"Spatial")){
    xy = coordinates(data)
  }else{
    xy = cbind(data$x,data$y)
  }

  plot(xy,asp=1,type="n")
  Is = Ifunc(t)
  Ss = Sfunc(t)
  Rs = !Is & !Ss
  points(xy[Ss,],col="black",pch=19,cex=0.26)
  points(xy[Is,],col="red",pch=19)
  points(xy[Rs,],col="red",pch=4)
  
  
  
}
##'
##' make an animation of an SIR model
##'
##' produces a web page animation of an SIR model
##' @title SIR Animation
##' @param data data frame with infection and removal times
##' @param times  formula of infection and removal times, or 2-column matrix
##' @param tlim  tlim lower and upper limit of plot
##' @param nt number of time steps
##' @author Barry Rowlingson
animateSIR <- function(data,times,
                       tlim=range(IRtimes,na.rm=TRUE),nt=25
                       ){
  if(inherits(times,"formula")){
    IRtimes = model.frame(times,data,na.action=na.pass)
  }else{
    IRtimes = times
  }
  
  Ifunc = infectivity(IRtimes)
  Sfunc = susceptible(IRtimes)

  ani.options(verbose=FALSE)
  saveHTML({
    dev.control("enable")
    for(t in seq(min(tlim),max(tlim),length=nt)){
      plotSIRat(data,Ifunc,Sfunc,t)
      ani.record(reset=TRUE,replay.cur=TRUE)
    }
  })
  
    
  
}
