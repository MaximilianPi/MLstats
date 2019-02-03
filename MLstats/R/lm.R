#'Fit a linear regression with a normal distributed error
#'@author Maximilian Pichler
#'
#'@param  formula an object of class formula
#'@param data data frame
#'@param subset indicies on which model is fitted
#'@param method support methods are random forest, deep neural networks
#'
#'@export



x = matrix(runif(1000), nrow = 100, ncol = 10)
w = runif(10,-1,1)
y = x %*% w + 2 + rnorm(100,0,0.5)
data = data.frame(x, y=y)
parameter = NULL
formula = y~.
cv = NULL

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

  model_fit = model_fit(model, cv, data_prepared)

  #importance = get_importance(model_fit)

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
    out$activation_function = "relu"
    out$batch = 25L
    out$epochs = 50L
    out$learning_rate = 0.001
    out$bias = TRUE
  }
  return(out)
}


#' Model predict function for the different models
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param data predict for data
#' @export
predict = function(model, data){
  UseMethod("predict", model)
}

#' Model predict function for lm_ann
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param data predict for data
#' @export
predict.lm_ann = function(model, data){
  return(model$lm$predict(data))
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

  parameter = model$parameter

  model$lm = ANN_lm$new(architecture = parameter$architecture,
                        activation_function = parameter$activation_function,
                        bias = parameter$bias,
                        regularization = parameter$regularization,
                        batch_size = parameter$batch,
                        epochs = parameter$epochs,
                        learning_rate = parameter$learning_rate,
                        data = data)

  model$lm$build_lm()
  model$lm$compile()
  model$lm$train()
  return(model)

}


#' Model fit lm for ANN
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param cv cv type
#' @export
model_fit.lm_rf = function(model, cv, data){

}
