
times = rbind(
  c(NA,NA),    # never infected, never culled
  c(NA,1.2),   # never infected, culled
  c(0.5,NA),   # infected, never culled
  c(0.5,1.2))  # infected, then culled

If = infectivity(times)
Sf = susceptible(times)
SIRstate = SIR2(times)

test_that("infectivity",{
  expect_equal(If(-10),rep(FALSE,4))
  expect_equal(If(1),c(FALSE,FALSE,TRUE,TRUE))
  expect_equal(If(2),c(FALSE,FALSE,TRUE,FALSE))
})

test_that("susceptibility",{
  expect_equal(Sf(0),rep(TRUE,4))
  expect_equal(Sf(1),c(TRUE,TRUE,FALSE,FALSE))
  expect_equal(Sf(2),c(TRUE,FALSE,FALSE,FALSE))
})

levs = c("S","I","R")
mkfac = function(s){factor(strsplit(s,"")[[1]],levels=levs)}
test_that("SIR state",{
  expect_equal(SIRstate(0),mkfac("SSSS"))
  expect_equal(SIRstate(1),mkfac("SSII"))
  expect_equal(SIRstate(2),mkfac("SRIR"))
})
