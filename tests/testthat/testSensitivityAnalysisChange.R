library(testthat)

context("Test the construction of sensitivity analysis changes")

test_that("Initialise a few sensitivity analysis changes", {
  # Change on theta
  change1 <- Change("VC", down=2, up=2, where="theta")
  expect_equal(change1@parameter, "VC")
  expect_equal(change1@where, "theta")
  expect_equal(change1@down, 2)
  expect_equal(change1@up, 2)
  
  # Testing default 'where' argument
  change1b <- Change("VC", down=2, up=2)
  expect_equal(change1, change1b)
  
  # Change on equation
  change2 <- Change("VC", down=2, up=2, where="equation")
  expect_equal(change2@where, "equation")

  # Unknown 'where' argument
  expect_error(Change("VC", down=2, up=2, where="wrong"), regexp="where must be either 'theta' or 'equation'")
})
