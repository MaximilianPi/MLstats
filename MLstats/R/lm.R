#'Fit a linear regression with a normal distributed error
#'@author Maximilian Pichler
#'
#'@param  formula an object of class formula
#'@param data data frame
#'@param subset indicies on which model is fitted
#'@param method support methods are random forest, deep neural networks
#'
#'@export



# x = matrix(runif(1000), nrow = 100, ncol = 10)
# w = runif(10,-1,1)
# y = x %*% w + 2 + rnorm(100,0,0.5)
# data = data.frame(x, y=y)

lmML = function(formula, data = NULL, subset, na.action, method = "ann", scale, parameter = NULL, cv = NULL){

  if (!inherits(formula, "formula")) stop("method is only for formula objects")

  if(is.null(parameter)) parameter = get_default_parameter(method = method)

  # not elegant...change later
  # ......................... #
  y = model.response(model.frame(formula, data))
  x = model.matrix(formula, data)
  x = x[,!colnames(x) %in% "(Intercept)"]
  # ......................... #

  data_prepared = list(x = x, y = y)

  model = create_model_object(method, parameter)

  model_fit = fit_model(model, cv, data_prepared)

  importance = get_importance(model_fit)

}


#' Create model object for useage of methods
#'
#' @author Maximilian Pichler
#' @param method default ann
#' @param parameter parameter list

create_model_object = function(method, parameter){
  out = list()
  if(method == "ann"){
    out$model = method
    out$parameter = parameter
    class(out) = "lm_ann"
    return(out)
  }
}


#' Get default parameter function, internal
#' @author MaximilianPi
#' @param method pars for method, default ann
get_default_parameter = function(method = "ann") {
  if(method == "ann"){
    out = list()
    out$architecture = c(20L, 20L, 20L)
    out$lr = 0.001
    out$regularization = "batch_normalization"
    out$activation = "relu"
    out$batch = 25L
    out$epochs = 50L
  }
  return(out)
}




#' Model fit function for the different models
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param cv type
#' @export
model_fit = function(model, cv, data){
  UseMethod("model_fit", model)
}


#' Model fit lm for ANN
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param cv cv type
#' @param data data..
#' @export
model_fit.lm_ann = function(model, cv, data){

  .error = tryCatch({
    require(keras)
    require(tensorflow)
    tf_probability = reticulate::import("tensorflow_probability")
    dist = tf_probability$distributions
  }, error = function(err) return(err))
  if(inherits(.error, "error")) stop(error)

  model = keras_model_sequential()

  build = build_base_ann(parameter, input_dim = , output_dim = 1L, output_activation = "linear")

  .null = build(model)

  # hack to add an additional variable to the keras trainable weights

  model$layers[[length(model$layers)]]$add_weight(name = 'sigma',
                                                  shape = list(),
                                                  initializer = initializer_constant(0.5),
                                                  trainable = TRUE)

  eps = tf$constant(.Machine$double.eps)

  k = backend()

  # likelihood normal distribution
  normal_likelihood = function(y_true, y_pred){

    sigma = tf$get_default_graph()$get_tensor_by_name("sigma:0")

    ll = dist$Normal(y_pred, scale = sigma+eps)$log_prob(y_true)

    return(k_mean(-ll))
  }

  model %>%
    compile(
      loss = normal_likelihood,
      optimizer = optimizer_rmsprop(lr = parameter$lr)
    )

  fit_history =
    model %>%
      fit(
        x = data$x,
        y = matrix(data$y, ncol = 1L, byrow = T),
        batch_size = parameter$batch,
        epochs = parameter$epoch
      )

  ### Prepare output and serilization

  model$set_weights

  sigma = k_get_value(tf$get_default_graph()$get_tensor_by_name("sigma:0"))

}


#' Model fit lm for ANN
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param cv cv type
#' @export
model_fit.lm_rf = function(model, cv, data){

}
