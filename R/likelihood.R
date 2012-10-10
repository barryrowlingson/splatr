print.likelihood <- function(x,...){
  cat("\nA log-likelihood function\n")
  cat("\n")
  cat("With parameter vector: ",paste(with(environment(x),varnames),collapse=", "),"\n")
  cat("Data summary:\n")
  print(summary(with(environment(x),data)))
  invisible(x)
}
