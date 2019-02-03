testthat::context("lm")

skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}

x = matrix(runif(1000), nrow = 100, ncol = 10)
w = runif(10,-1,1)
y = x %*% w + 2 + rnorm(100,0,0.5)
data = data.frame(x, y=y)
# parameter = NULL


testthat::test_that("lm functionality",{
  testthat::expect_error(lmML("egal"))
  testthat::expect_error(lmML())
  testthat::expect_error(lmML(y~., data = data, parameter = list(hi = "hi")))
  testthat::expect_error(lmML(y~., data = data, parameter = list(hi = "hi"), method = "rf"))
  testthat::expect_error(lmML(y~., data = data, parameter = list(epochs = "hi"), method = "ann"))

  skip_if_no_tensorflow()
  testthat::expect_error(lmML(y~., data = data, parameter = list(epochs = 1L, architecture = 1L), method = "ann"), NA)
  testthat::expect_error({
    fit = lmML(y~., data = data, parameter = list(epochs = 1L, architecture = 1L), method = "ann")
    p = predict(fit,data)
    }, NA)
  testthat::expect_error({
    fit = lmML(y~., data = data, parameter = list(epochs = 1L, architecture = 1L), method = "ann")
    p = predict(fit,data)
  }, NA)
})
