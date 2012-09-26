#' create the infectivity function
#'
#' @param times a 2-column matrix of infection/removal times
#'

infectivity <- function(times){
  t1 = times[,1]
  t2 = times[,2]
  I = function(t){
    (!is.na(t1) & t1 < t) & (t < t2 | is.na(t2))
  }
  I
}

#' create the susceptibility function
#'
#' @param times a 2-columnd matrix of infection/removal times
#'

susceptible <- function(times){
  t1 = times[,1]
  t2 = times[,2]
  S = function(t){
    (is.na(t2) & is.na(t1)) |
    (is.na(t1) & !is.na(t2) & (t2 >= t)) |
    (!is.na(t1)  & (t1>=t))
  }
  S
}

SIR2 <- function(times){
  Sf = susceptible(times)
  If = infectivity(times)
  SIRf = function(t){
    infectives = If(t)
    susceptibles = Sf(t)
    removed = !infectives & !susceptibles
    state = rep(NA,length(infectives))
    state[infectives]="I"
    state[susceptibles]="S"
    state[removed]="R"
    state = factor(state,levels=c("S","I","R"))
    return(state)
  }
  SIRf
}
