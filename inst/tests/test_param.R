test_that("params",{

prefixes=c("beta","gamma")

# no fixed params
pi1 = paramInfo(
  list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
  list(name=prefixes[2],label=prefixes[2])
  )


# fixed gamma
pi2 = paramInfo(
  list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
  list(name=prefixes[2],label=prefixes[2]),
  fixed = list(gamma=1)
  )

# fixed betas
pi3 = paramInfo(
  list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
  list(name=prefixes[2],label=prefixes[2]),
  fixed=list(beta=c(2,3))
  )

# everything fixed...
pi4 = paramInfo(
  list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
  list(name=prefixes[2],label=prefixes[2]),
  fixed=list(beta=c(2,3),gamma=1)
  )

pi4 = paramInfo(
  list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
  list(name=prefixes[2],label=prefixes[2]),
  fixed=list(beta=c(2,3),gamma=1)
  )

expect_error(
             paramInfo(
                       list(name=prefixes[1],label=paste(prefixes[1],c("cattle","sheep"),sep=".")),
                       list(name=prefixes[2],label=prefixes[2]),
                       fixed=list(zapzap=c(2,3),gamma=1)
                       ) , "Unknown fixed" 
             )

expect_equal(mergeFixed(pi1,c(1,2,3)),c(beta=1,beta=2,gamma=3))
expect_error(mergeFixed(pi1,c(1,2)),"Wrong number")
expect_error(mergeFixed(pi1,c(1)))
expect_error(mergeFixed(pi1,c(1,2,3,4,5)))
#expect_error(mergeFixed(pi1,c("a","b","c")))

expect_equal(mergeFixed(pi2,c(2,3)),c(beta=2,beta=3,gamma=1))
expect_error(mergeFixed(pi2,c(1)))
expect_error(mergeFixed(pi2,c(1,3,4)))

expect_equal(mergeFixed(pi3,1),c(beta=2,beta=3,gamma=1))

expect_equal(mergeFixed(pi4,NULL),c(beta=2,beta=3,gamma=1))

}
          )

test_that("null param functions",
          {
            expect_error(paramInfo(list(name=NULL,label="beta.")),"All parameters need a name")
            pnull2 = paramInfo(list(name="beta",label=NULL))
          }
          )
