set.seed(310366)
data=data.frame(
  x = as.integer(runif(10,1000,1100)),
  y = as.integer(runif(10,2000,2100)),
  sheep=sample(1:10),cows=5*sample(1:10),pigs = 2*sample(1:10),
  area=runif(10,100,200),
  type=factor(sample(c("Hill","Lowland","Small"),10,replace=TRUE))
  )
coordinates(data)=~x+y

test_that("distance functions",{

  
  
})
