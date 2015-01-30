context('quickhash')

test_that("quickhash works", {
  lsm='test.lsm'
  expect_equal(quickhash(lsm), digest(lsm, file=T), 
               "quickhash defaults to file digest")
  expect_equal(quickhash.lsm(lsm), "e5309f6725f4a7b3d6af9e9aac77f5ef",
               "quickhash.lsm gives expected result")
  
})
