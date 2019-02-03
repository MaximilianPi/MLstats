#' Base ANN object
#'
#' \code{ANN} object holds a basic artifical neural network model
#'
#' @format \code{\link{R6Class}} object.
#' @name ANN
#' @section Usage:
#' \preformatted{
#' model = ANN$new()
#' }
#'
#' @section Arguments:
#' \describe{
#' \item{model: }{(any)\cr keras model}
#' \item{data: }{(list)\cr List of data with x and y as matrices }
#' \item{data: }{(list)\cr fit history list}
#' \item{build: }{(function)\cr build function}
#'
#'
#' }
#'
#' @section Details:
#' comming soon
#'
#'
#'
#' @author Maximilian Pichler
#' @export
NULL

#'@export
ANN = R6::R6Class("ANN",
 public = list(
   model = NULL,
   data = NULL,
   fit_history = NULL,
   k = NULL,
   initialize = function(architecture = c(10,10),
                         activation_function = "relu",
                         bias = TRUE,
                         regularization = "batch_normalization",
                         dropout_rate = 1.,
                         batch_size = 25L,
                         epochs = 100L,
                         learning_rate = 0.001,
                         data = NULL){
     self$k = keras::backend()
     self$reset_all()

     if(length(architecture) <= 0) stop("For linear regression, switch to lm()",call. = FALSE)
     if(!is.numeric(architecture)) stop("Numerical vector for architecture is needed", call. = FALSE)
     if(any(sapply(architecture, function(x) x == 0))) stop("Nodes with 0 units not allowed", call. = FALSE)
     if(!activation_function %in% c("relu", "tanh", "elu")) stop("activation_function must be one of (relu, tanh, elu)", call. = FALSE)
     if(dropout_rate > 1 || dropout_rate < 0) stop("dropout_rate must be in [0,1]", call. = FALSE)
     if(batch_size > nrow(data$x)) {
       batch_size = nrow(data$x)
       warning("Setting batch_size to nrow(data) because batch size cannot be larger than data", call. = FALSE)
     }
     if(epochs <=0 || !is.numeric(epochs)) stop("Epochs must be > 0 and numeric", call. = FALSE)
     if(learning_rate <= 0 || !is.numeric(learning_rate)) stop("learning_rate must be > 0 and numeric", call. = FALSE)
     if(is.null(data)) warning("Data must contain data or model cannot be built and trained", call. = FALSE)

     private$architecture = architecture
     private$activation_function = activation_function
     private$bias = bias
     private$regularization = regularization
     private$dropout_rate = dropout_rate
     private$batch_size = batch_size
     private$epochs = epochs
     private$learning_rate = learning_rate

     if(is.null(dim(data$y))) {
       warning("Y is a vector, changing to matrix with ncol == 1",call. = FALSE)
       data$y = matrix(data$y, ncol = 1, byrow = TRUE)
     }

     self$data = data
     self$model = keras::keras_model_sequential()
     private$built = FALSE

   },
   build = function(){
     if(!private$built){
       regularization =
         if(private$regularization=="batch_normalization") {
           function(model) return(keras::layer_batch_normalization(model))
         } else {
           function(model) return(keras::layer_dropout(model, rate = self$dropout_rate))
         }

       for(i in 1:length(private$architecture)){
         if(i == 1) {
           keras::layer_dense(self$model, input_shape = dim(self$data$x[2]),
                              activation = private$activation_function, units = private$architecture[i])
         }else {
           keras::layer_dense(self$model,
                              activation = private$activation_function, units = private$architecture[i])
         }
         regularization(self$model)
       }

       private$built = TRUE
       return(TRUE)
     } else {
       warning("already built, reset first", call. = FALSE)
       return(FALSE)
     }
   },

   build_output = function(output_activation = "relu"){
       keras::layer_dense(self$model, units = dim(self$data$y)[2], activation = output_activation)
   },

   compile = function(){
     keras::compile(self$model, loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_rmsprop(lr = private$learning_rate))
   },

   train = function(){
     fit_history = keras::fit(self$model, x = self$data$x, y = self$data$y, epochs = private$epochs, batch_size = private$batch_size, shuffle = TRUE)
     self$fit_history = c(self$fit_history, fit_history$metrics$loss)
   },

   predict = function(newdata = NULL){
     require(keras)
     if(is.null(newdata)) newdata = self$data$x
     predict(self$model, newdata)
   },

   serialize = function(){
     keras::serialize_model(model, include_optimizer = TRUE)
   },

   reset_fit = function(){
     self$k$clear_session()
     self$model = keras::keras_model_sequential()
     self$build()
     self$compile()
   },

   reset_all = function(){
     self$k$clear_session()
     self$model = keras::keras_model_sequential()
   }


 ),
 private = list(
   architecture = NULL,
   activation_function = NULL,
   bias = NULL,
   regularization = NULL,
   dropout_rate = NULL,
   batch_size = NULL,
   epochs = NULL,
   learning_rate = NULL,
   output_activation = NULL,
   built = NULL
))



#' Base ANN_lm object
#'
#' \code{ANN_lm} object holds an artifical neural network model with a normal distribution likelihood
#'
#' @format \code{\link{R6Class}} object.
#' @name ANN_lm
#' @section Usage:
#' \preformatted{
#' model = ANN$new()
#' }
#'
#' @section Arguments:
#' \describe{
#' \item{model: }{(any)\cr keras model}
#' \item{data: }{(list)\cr List of data with x and y as matrices }
#' \item{data: }{(list)\cr fit history list}
#' \item{build: }{(function)\cr build function}
#'
#'
#' }
#'
#' @section Details:
#' comming soon
#'
#'
#'
#' @author Maximilian Pichler
#' @export
NULL

#'@export
ANN_lm = R6::R6Class("ANN_lm",
  inherit = ANN,
  public = list(

    eps = NULL,
    tf = NULL,
    dist = NULL,
    sigma = NULL,

    initialize = function(...){
      super$initialize(...)
      self$tf = reticulate::import("tensorflow")
      self$dist = reticulate::import("tensorflow_probability")$distributions
      self$eps = self$tf$constant(.Machine$double.eps)
    },

    build_lm = function(){
      if(self$build()){
        suppressMessages({self$model$layers[[length(self$model$layers)]]$add_weight(name = 'sigma',
                                                                  shape = list(),
                                                                  initializer = initializer_constant(0.5),
                                                                  trainable = TRUE)
        })
      }
    },

    compile = function(){


      normal_likelihood = function(y_true, y_pred){
        sigma = self$tf$get_default_graph()$get_tensor_by_name("sigma:0")
        ll = self$dist$Normal(y_pred, scale = sigma+self$eps)$log_prob(y_true)
        return(self$k$mean(-ll))
      }

      keras::compile(self$model, loss = normal_likelihood, optimizer = keras::optimizer_rmsprop(lr = private$learning_rate))
    },

    train = function(){
      super$train()

      self$update_sigma()
    },

    update_sigma = function() {
      self$sigma = self$k$get_value(self$tf$get_default_graph()$get_tensor_by_name("sigma:0"))
    }


))

# m = ANN_lm$new(data = data)
#
# m$build_lm()
# m$compile()
# m$train()
# m$model
#
# m$reset_all()
