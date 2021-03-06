test_that("powerSum data",{
  set.seed(310366)
  data=data.frame(
    x = as.integer(runif(10,1000,1100)),
    y = as.integer(runif(10,2000,2100)),
    sheep=sample(1:10),cows=5*sample(1:10),pigs = 2*sample(1:10),
    area=runif(10,100,200),
    type=factor(sample(c("Hill","Lowland","Small"),10,replace=TRUE))
    )

# need more than one covariate (since the effect is relative)
expect_error(powerSum(~cows,fixed=list(gamma=2),starting=list(beta=c(1,1)))(data),"more than one")

# this works because we don't validate starting at this point.
p1 = powerSum(~cows+sheep,fixed=list(gamma=2),starting=list(beta=c(1,1)))(data)
p2 = powerSum(~cows+sheep)(data)

expect_error(p1(1:10,c(3,4)),"Wrong number of values")

# 
expect_equal(p1(1:10,3.4),data$cows^2 + exp(3.4)*data$sheep^2)
expect_equal(p1(c(3,6,5,9),3.4),(data$cows^2 + exp(3.4)*data$sheep^2)[c(3,6,5,9)])

# 
expect_equal(p2(1:10,c(1.1,2.1)),data$cows^(2.1)+ exp(1.1)*data$sheep^(2.1))

# testing prefixes:

expect_error(powerSum(~cows+sheep,fixed=list(gamma=2),prefixes=c("zeta","lamma"))(data),"Unknown fixed")

p3 = powerSum(~cows+sheep,fixed=list(lamma=2),prefixes=c("zeta","lamma"))(data)
expect_equal(p3(1:10,2.3),data$cows^2+ exp(2.3)*data$sheep^2)

# testing boolean selection

  expect_equal(p3(1:5,2.3),p3(c(rep(TRUE,5),rep(FALSE,5)),2.3))
  expect_equal(p3(c(1,3,4,5,9),2.3),p3(c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE),2.3))
  
  
}
          )

