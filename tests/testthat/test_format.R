library(testthat)

test_that(
  "Checking if myquad function works",
  {
    expect_equal(myquad(0), 6)
  }
)
