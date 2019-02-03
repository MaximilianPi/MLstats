#'Fit a linear regression with a normal distributed error
#'@author Maximilian Pichler
#'
#'@param  formula an object of class formula
#'@param data data frame
#'@param subset indicies on which model is fitted
#'@param na.action how to handle nas
#'@param method support methods are random forest, deep neural networks
#'@param scale how to handle unscales values
#'@param parameter list of parameter for method
#'@param cv cross validation strategy
#'
#'@importFrom stats model.frame model.matrix dnorm
#'@export

# TO DO: implement cv + subset + na.action

lmML = function(formula, data = NULL, subset = NULL, na.action = NULL, method = "ann", scale = FALSE, parameter = NULL, cv = NULL){

  if(!inherits(formula, "formula")) stop("method is only for formula objects")
  if(method != "ann") stop("At the moment, only method = 'ann' is supported")
  if(is.null(parameter)) parameter = get_default_parameter(method = method)
  parameter = check_parameter(parameter, method)
  if(is.logical(parameter) && !parameter) stop("Unkown parameters in parameter list!")

  # not elegant...change later
  # ......................... #
  y = model.response(model.frame(formula, data))

  x = model.matrix(formula, data)

  x = x[,!colnames(x) %in% "(Intercept)"]
  # ......................... #

  data_prepared = list(x = x, y = y)

  model = create_model_object(method, parameter)

  model_fitted = model_fit(model, cv, data_prepared)

  predictions = predict(model_fitted, data_prepared$x)

  residuals = data_prepared$y - predictions

  output = list()

  output$predictions = predictions

  output$residuals = residuals

  output$deviance = -2*sum(dnorm(data_prepared$y, predictions, sd = model_fitted$lm$sigma, log = T))

  output$model = model_fitted$lm

  output$formula = formula

  class(output) = paste0("MLstats.", class(model_fitted))

  return(output)

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



#' check parameter list
#' @author Maximilian Pichler
#' @param parameter parameter list to check
#' @param method method, only ann supported

check_parameter = function(parameter, method){
  get_default = get_default_parameter()
  if(!any(names(parameter) %in% names(get_default))) return(FALSE)
  else {
    for(p in names(parameter)) get_default[[p]] = parameter[[p]]
    return(get_default)
  }
}





#' Get default parameter function, internal
#' @author MaximilianPi
#' @param method pars for method, default ann
get_default_parameter = function(method = "ann") {
  if(method == "ann"){
    out = list()
    out$architecture = c(10,10)
    out$lr = 0.001
    out$regularization = "batch_normalization"
    out$activation_function = "relu"
    out$batch = 25L
    out$epochs = 100L
    out$learning_rate = 0.001
    out$bias = TRUE
    out$dropout_rate = 1.
  }
  return(out)
}


#' Model predict function for lm_ann
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param data predict for data
#' @importFrom stats predict
#' @export
predict.lm_ann = function(model, data){
  return(model$lm$predict(data))
}



#' Model predict function for MLstats.lm_ann
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param data predict for data
#' @importFrom stats predict
#' @export
predict.MLstats.lm_ann = function(model, data){

  y = model.response(model.frame(model$formula, data))

  x = model.matrix(model$formula, data)

  x = x[,!colnames(x) %in% "(Intercept)"]

  return(model$model$predict(as.matrix(x)))
}

#' Model fit function for the different models
#'
#' @author Maximilian Pichler
#' @param model model type
#' @param cv type
#' @param data data for fitting
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
#' @param data data ...
#' @export
model_fit.lm_rf = function(model, cv, data){

}
