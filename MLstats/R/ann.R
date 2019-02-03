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
#' \item{data: }{(data.frame)\cr The data }
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
   initialize = function(architecture = c(10,10),
                         activation_function = "relu",
                         bias = TRUE,
                         regularization = "batch_normalization",
                         dropout_rate = 1.,
                         batch_size = 25L,
                         epochs = 100L,
                         learning_rate = 0.001,
                         data = NULL){

     self$architecture = architecture
     self$activation_function = activation_function
     self$bias = bias
     self$regularization = regularization
     self$dropout_rate = dropout_rate
     self$batch_size = batch_size
     self$epochs = epochs
     self$learning_rate = learning_rate
     self$data = data
     self$model = keras::keras_model_sequential()

   },
   build = function(){

     regularization =
       if(self$regularization=="batch_normalization") {
         function(model) return(keras::layer_batch_normalization(model))
       } else {
         function(model) return(keras::layer_dropout(model, rate = self$dropout_rate))
       }

     for(i in 1:length(self$architecture)){
       if(i == 1) {
         keras::layer_dense(self$model, input_shape = dim(self$data$x[2]),
                            activation = self$activation_function, units = architecture[i])
       }else {
         keras::layer_dense(self$model,
                            activation = self$activation_function, units = architecture[i])
       }
       regularization(self$model)
     }
   },

   build_output = function(output_activation = "relu"){
       keras::layer_dense(self$model, units = dim(self$data$y)[2], activation = output_activation)
   },

   compile = function(){
     keras::compile(self$model, loss = keras::loss_mean_squared_error, optimizer = keras::optimizer_rmsprop(lr = self$learning_rate))
   },

   train = function(){
     fit_history = keras::fit(self$model, x = self$data$x, y = self$data$y, epochs = self$epochs, batch_size = self$batch_size, shuffle = TRUE)
   },

   predict = function(newdata = NULL){
     if(is.null(newdata)) newdata = self$data$x
     predict(self$model, newdata)
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
   output_activation = NULL

 ))












#' Helper function for ANN build
#'
#' @author Maximilian Pichler
#' @param parameter parameterlist
#' @param input_dim input dimension
#' @param output_dim output dimension for last layer
#' @param output_activation activation function in last layer

build_base_ann = function( parameter, input_dim, output_dim, output_activation = "linear"){

  require(keras, quietly = TRUE)

  return(function(model){

    layer = length(parameter$architecture)

    architecture = parameter$architecture

    activation = parameter$activation

    regularization =
      if(parameter$regularization=="batch_normalization") {
        function(model) return(layer_batch_normalization(model))
      } else {
        function(model) return(layer_dropout(model, rate = parameter$dropout_rate))
      }

    for(i in 1:layer){
      if(i == 1) {
        model %>% layer_dense(input_shape = input_dim, activation = activation, units = architecture[i])
      }else {
        model %>% layer_dense( activation = "relu", units = architecture[i])
      }
      model %>% regularization
    }

    model %>%
      layer_dense(units = output_dim, activation = output_activation)

    return(model)
  })

}




