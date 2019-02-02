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
