context('lsm_metadata')

test_that("we can extract lsm metadata", {
  lsm='test.lsm'
  expect_match(lsm_metadata(lsm), "VoxelSizeZ: 1.0E-13", all = FALSE)
})
