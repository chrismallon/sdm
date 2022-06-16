# Tests
library(testthat)
source('../../R/sdm_helpers.R')

test_that("Test raster is created.", {
  to_test = make_test_raster(50, 50, "test_raster_1", "test_raster")
  on.exit(file.remove(to_test))
  on.exit(unlink('test_raster', recursive=TRUE))

  expect_true(file.exists(to_test)) # file is written

  trast <- raster::raster(to_test)

  expect_equal(trast@ncols, 50) # file conforms to parameters
  expect_equal(trast@nrows, 50)
})



testthat::test_that("Function successfuly loads raster.", {
  test_path <- make_test_raster(50, 50, "test_raster_0002", "test_raster_2")
  on.exit(file.remove(test_path))
  on.exit(unlink('test_raster_2', recursive=TRUE))
  trast2 <- load_disturbance_raster(file_path = 'test_raster_2', base_file_name = 'test_raster_',year = 2,file_name_end_and_type = '.tif')
  expect_equal(trast2@ncols, 50) # file is successfully loaded
  expect_equal(trast2@nrows, 50)
})

testthat::test_that("List of fire sizes is correctly created", {
  test_path3 <- make_test_raster(50, 50, "test_raster_0003", "test_raster_3")
  on.exit(file.remove(test_path3))
  on.exit(unlink('test_raster_3', recursive=TRUE))
  trast3 <- load_disturbance_raster(file_path = 'test_raster_3', base_file_name = 'test_raster_',year = 3,file_name_end_and_type = '.tif')
  fire_size_list3 <- list()
  fire_size_list3 <- add_list_of_fire_sizes(fire_size_list3, trast3)

  expect_equal(sum(fire_size_list3[[1]]$size), sum(trast3[]))
})
