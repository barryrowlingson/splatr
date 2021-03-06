##  data,times=~I(report-5)+cull,
##          cov=list(
##             powerSum(~nsheep+ncattle,gamma=1),
##             logLinear(~area)
##             ),
##          distF = expDecay(~x+y,kappa=0.5)


set.seed(310366)
data=data.frame(
  x = as.integer(runif(10,1000,1100)),
  y = as.integer(runif(10,2000,2100)),
  report = as.integer(runif(10,100,200)),
  sheep=sample(1:10),cows=5*sample(1:10),pigs = 2*sample(1:10),
  area=runif(10,100,200),
  type=factor(sample(c("Hill","Lowland","Small"),10,replace=TRUE))
  )
data$cull = data$report + 5

test_that("SIR2 checking", {

  expect_error(likSIR2(),"missing")
  expect_error(likSIR2(data,~x))
  expect_error(likSIR2(data,1:5))
  expect_error(likSIR2(data,cbind(runif(nrow(data)+1),runif(nrow(data)+1))))
  
}
          )


test_that("SIR2 running",{
  L = likSIR2(data,
          times=~I(report-5)+cull,
          cov=list(
            powerSum(~sheep+cows,fixed=list(gamma=1)), # 1 parameter
            logLinear(~area) # 2 parameters
            ),
          distDecay = expDecay(~x+y,fixed=c(kappa=0.5)) # 3 parameters
          )
  expect_error(L(),"Incorrect number of parameters")
  expect_error(L(c(0,0,0)),"Incorrect number of parameters")
  expect_error(L(c(0,0,0,0,0,0,0)),"Incorrect number of parameters")
  
})



##         cov = powerSum(data,~sheep+cows,fixed=list(gamma=1))
##       distF = expDecay(data,~x+y,fixed=list(kappa=0.5))
