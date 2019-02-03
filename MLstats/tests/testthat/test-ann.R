testthat::context("ann functionality")
skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}

x = matrix(runif(1000), nrow = 100, ncol = 10)
w = runif(10,-1,1)
y = x %*% w + 2 + rnorm(100,0,0.5)
data = list(x=x, y=y)
# parameter = NULL
# formula = y~.
# cv = NULL
# method = "ann"
testthat::test_that("ann parameters", {
  skip_if_no_tensorflow()
  testthat::expect_error({ANN$new()})
  testthat::expect_error({ANN$new(data = data, batch_size = 20)}, NA)
  testthat::expect_error({ANN$new(data = data, epochs = 0)})
  testthat::expect_error({ANN$new(data = data, learning_rate = -1)})
  testthat::expect_error({ANN$new(data = data, dropout_rate  = -1)})
  testthat::expect_error({ANN$new(data = data, architecture = NULL)})
  testthat::expect_error({ANN$new(data = data, architecture = 10L)}, NA)
  testthat::expect_error({ANN$new(data = data, activation_function = "egal")})
})


testthat::test_that("ann functionality", {
  skip_if_no_tensorflow()
  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
  }, NA)

  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
    m$predict()
  }, NA)

  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
    m$predict(data$x)
  }, NA)

  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
    m$reset_fit()
    m$train()
  }, NA)

  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
    m$reset_all()
    m$train()
  })

  testthat::expect_error({
    m = ANN$new(data = data, epochs = 1L, architecture = 1L)
    m$build()
    m$compile()
    m$train()
    m$reset_all()
    m$build()
    m$compile()
    m$train()
  }, NA)
})

