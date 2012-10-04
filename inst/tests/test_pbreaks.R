test_that("breaking parameter vectors",
   {
     expect_equivalent(makeBreakMap(c(1,2)),c(1,2,2))
     expect_equivalent(makeBreakMap(c(2,0)),c(1,1))
     expect_equivalent(makeBreakMap(c(0)),integer(0))
     expect_equivalent(makeBreakMap(c(0,0)),integer(0))
     expect_equivalent(makeBreakMap(c(0,0,0)),integer(0))
     
     expect_equivalent(makeBreakMap(c(3,0,0)),c(1,1,1))
     expect_equivalent(makeBreakMap(c(0,3,0)),c(2,2,2))
     expect_equivalent(makeBreakMap(c(0,0,3)),c(3,3,3))

     expect_equivalent(makeBreakMap(c(3,2,0)),c(1,1,1,2,2))
     expect_equivalent(makeBreakMap(c(0,3,2)),c(2,2,2,3,3))
     expect_equivalent(makeBreakMap(c(2,0,3)),c(1,1,3,3,3))

     expect_equal(attr(makeBreakMap(c(1)),"npars"),1)
     expect_equal(attr(makeBreakMap(c(3,2)),"npars"),2)
     expect_equal(attr(makeBreakMap(c(5,4,3)),"npars"),3)

     expect_equal(attr(makeBreakMap(c(0,0,0)),"npars"),3)
     expect_equal(attr(makeBreakMap(c(0,0,1)),"npars"),3)
     expect_equal(attr(makeBreakMap(c(2,0,0)),"npars"),3)
   }
          )


