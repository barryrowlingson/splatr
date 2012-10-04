set.seed(310366)
data=data.frame(
  x = as.integer(runif(10,1000,1100)),
  y = as.integer(runif(10,2000,2100)),
  sheep=sample(1:10),cows=5*sample(1:10),pigs = 2*sample(1:10),
  area=runif(10,100,200),
  type=factor(sample(c("Hill","Lowland","Small"),10,replace=TRUE))
  )

f = expDecay(~x+y,fixed=c(kappa=0.5))(data) # 3 params

l1 = logLinear(~cows)(data)        # 2 params
l2 = logLinear(~pigs+sheep)(data)   # 3 params

covFlist = list(
  l1,
  l2
  )

nVarsEach = c(nvariables(f),sapply(covFlist,nvariables))

expect_equal(nVarsEach,c(3,2,3))

breakMap = makeBreakMap(nVarsEach)

A <- makeAfunction(covFlist,breakMap)

expect_error(A(1:10,numeric(0)))
expect_error(A(1:10,c(1,2,3)))

params = c(1,2,3,4,5,6,7,8)

expect_equal(breakVariable(params,1,breakMap),params[1:3])
expect_equal(breakVariable(params,2,breakMap),params[4:5])
expect_equal(breakVariable(params,3,breakMap),params[6:8])

expect_equivalent(A(1:10,params),l1(1:10,params[4:5])*l2(1:10,params[6:8]))
expect_equivalent(A(1:5,params),l1(1:5,params[4:5])*l2(1:5,params[6:8]))
expect_equivalent(A(3,params),l1(3,params[4:5])*l2(3,params[6:8]))


# single covariance function
covFlist = list(
  l1
  )

A <- makeAfunction(covFlist,breakMap)

expect_equivalent(A(1:10,params),l1(1:10,params[4:5]))

