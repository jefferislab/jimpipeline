context('lsm_metadata')

test_that("we can extract lsm metadata", {
  lsm='test.lsm'
  expect_match(lsm_metadata(lsm), "VoxelSizeZ: 1.0E-13", all = FALSE)
})

test_that("we can parse key lsm metadata", {
  lsm='test.lsm'
  expect_equal(parse_key_lsm_metadata(lsm)$dim[['DimensionX']], 512)
  expect_equal(parse_key_lsm_metadata(lsm)$dim[['DimensionChannels']], 1)
})
