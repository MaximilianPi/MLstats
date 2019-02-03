context("ann_lm")
skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}

x = matrix(runif(1000), nrow = 100, ncol = 10)
w = runif(10,-1,1)
y = x %*% w + 2 + rnorm(100,0,0.5)
data = list(x=x, y=y)

testthat::test_that("ann_lm functionality", {
  skip_if_no_tensorflow()

  testthat::expect_error({
    m = ANN_lm$new(data= data)
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data= data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data= data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$predict()
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data= data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$predict()
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data= data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$reset_fit()
    m$predict()
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data= data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$reset_fit()
    m$train()
    m$predict()
  }, NA)

  testthat::expect_error({
    m = ANN_lm$new(data = data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$reset_all()
    m$train()
  })

  testthat::expect_error({
    m = ANN_lm$new(data = data, epochs = 1L, architecture = 1L)
    m$build_lm()
    m$compile()
    m$train()
    m$reset_all()
    m$build_lm()
    m$compile()
    m$train()
  }, NA)

})

