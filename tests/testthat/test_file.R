# test make_filename
test_that("make_filename generates the file name if it exists", {
  expect_equal(make_filename(2012), "")
})

# test fars_read_years
test_that("fars_read_years return a table with the information", {
  expect_equal(nrow(fars_read_years(2013)[[1]]), 30202)
})
