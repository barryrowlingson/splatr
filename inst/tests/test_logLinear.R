set.seed(310366)
data=data.frame(
  x = as.integer(runif(10,1000,1100)),
  y = as.integer(runif(10,2000,2100)),
  sheep=sample(1:10),cows=5*sample(1:10),pigs = 2*sample(1:10),
  area=runif(10,100,200),
  type=factor(sample(c("Hill","Lowland","Small"),10,replace=TRUE))
  )


test_that("logLinear",{
  l1 = logLinear(~cows)(data)
  p1 = getParamInfo(l1)
  expect_equal(l1(1:10,c(2.2,3.1)),2.2+data$cows*3.1)
  expect_equal(l1(4,c(2.2,3.1)),(2.2+data$cows*3.1)[4])
  expect_equal(l1(c(4,7),c(2.2,3.1)),(2.2+data$cows*3.1)[c(4,7)])
  
  expect_error(l1(1:10,2),"Wrong number")
  expect_error(l1(1:100,c(2,3)),"out of bounds")

  l2 = logLinear(~cows-1)(data)
  p2 = getParamInfo(l2)
  expect_equal(l2(1:10,1.2),1.2*data$cows)
  expect_error(l2(1:10,c(1.1,2.2)),"Wrong number")

  l3 = logLinear(~cows+pigs)(data)
  p3 = getParamInfo(l3)
  expect_equal(l3(1:10,c(1.2,2.3,3.4)),1.2+2.3*data$cows + 3.4*data$pigs)
  l4 = logLinear(~cows+pigs-1)(data)
  p4 = getParamInfo(l4)
  expect_equal(l4(1:10,c(2.3,3.4)),2.3*data$cows + 3.4*data$pigs)

  ## test transformations

  l5 = logLinear(~sin(cows))(data)
  p5 = getParamInfo(l5)
  expect_equal(l5(1:10,c(1.1,2.2)),1.1+2.2*sin(data$cows))

  ## todo: fixed parameters

  ## todo: offsets

  l6 = logLinear(~cows+offset(pigs))(data)
  p6 = getParamInfo(l6)  
  expect_equal(l6(1:10,c(2.2,3.1)),2.2+data$cows*3.1+data$pigs)
  expect_equal(l6(4,c(2.2,3.1)),(2.2+data$cows*3.1+data$pigs)[4])
  expect_equal(l6(c(4,7),c(2.2,3.1)),(2.2+data$cows*3.1+data$pigs)[c(4,7)])

  l7 = logLinear(~cows-1+offset(pigs))(data)
  p7 = getParamInfo(l7)
  expect_error(l7(1:10,c(2.2,3.1)),"Wrong number")
  expect_equal(l7(1:10,c(3.1)),data$cows*3.1+data$pigs)

  # just an intercept, no variables
  l8 = logLinear(~offset(pigs))(data)
  p8 = getParamInfo(l8)
  expect_equal(l8(1:10,1.2),data$pigs + 1.2)

  l9 = logLinear(~offset(pigs)-1)(data)
  p9 = getParamInfo(l9)
  expect_equal(l9(1:10,NULL),data$pigs)
  
}
          )
