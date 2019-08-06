library(testthat)
source("./algorithms.R")

###############################################################################
#-----------------------------BEGINING context.get----------------------------#
###############################################################################
test_that("context.get returns rigth for sentences ended with '.'",{
  context <- context.get("Life is what happens when you are busy making plans. Get busy living or get busy dying.")
  expect_equal(context$length, 0)
  
  expect_true(is.null(context$context))
})

test_that("context.get returns rigth sentence vector",{
  context <- context.get("Life is what happens when")
  expect_equal(context$length, 2)
  expect_setequal(context$context, c("happen", "when"))
})


test_that("context.get returns empty sentence vector",{
  context <- context.get()
  expect_equal(context$length, 0)
  expect_true(is.null(context$sentence))
})

test_that("context.get returns the right number of words",{
  context <-context.get("Life is what happens when you are busy making plans. Get")
  expect_true(context$length == 1)
  expect_setequal(c("get"), context$context)
  
  context <-context.get("Life is what happens when you are busy making plans. Get Busy")
  expect_true(context$length == 2)
  expect_setequal(c("get", "busi"), context$context)
  
  
  context <-context.get("Life is what happens when you are busy making plans.")
  expect_true(context$length == 0)
  
  context <-context.get("Life is what happens when you are busy making plans?")
  expect_true(context$length == 0)
  
  context <-context.get("Life is what happens when you are busy making plans!")
  expect_true(context$length == 0)
})
###############################################################################
#-------------------------------END context.get-------------------------------#
###############################################################################

