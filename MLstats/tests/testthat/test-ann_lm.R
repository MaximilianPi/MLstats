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


})

