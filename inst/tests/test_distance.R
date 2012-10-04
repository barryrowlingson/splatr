
data=data.frame(
  x = 1:10,
  y = 1:10,
  area=runif(10,100,200)
  )

coordinates(data)=~x+y

test_that("distance functions",{

  expect_error(expDecay())
  expDecay(~.) # this is okay, we can have all free variables

  ## there's no foo parameter...
  expect_error(expDecay(~x+y,fixed=c(foo=1)))

  ## ..unless I call it that
  efoo = expDecay(~x+y,fixed=c(foo=1),names=c("phi","kappa","foo"))
  edfoo = efoo(data)
  
  
  e = expDecay(~x+y,fixed=c(kappa=0.5))
  e2 = expDecay(~.,fixed=c(kappa=0.5))
  ed = e(data)
  ed2 = e2(data)

  expect_error(ed(1:10,1:10,c(1,2,3))) # first parameter must be an integer

  expect_equal(ed(1,1:10,c(1.2,2.3,3.4)),1.2*exp(-((0:9)*sqrt(2)/2.3)^0.5)+3.4)
  expect_equal(ed(1,3:7,c(1.2,2.3,3.4)),1.2*exp(-((2:6)*sqrt(2)/2.3)^0.5)+3.4)

  e = expDecay(~x+y)
  ed = e(data)
  expect_error(ed(1,1:10,c(1.2,2.3,3.4)),"Wrong number of values")
  expect_equal(ed(1,3:7,c(1.2,2.3,3.4,0.6)),1.2*exp(-((2:6)*sqrt(2)/2.3)^0.6)+3.4)
  
  
})
