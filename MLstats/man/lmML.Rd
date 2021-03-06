% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{lmML}
\alias{lmML}
\title{Fit a linear regression with a normal distributed error}
\usage{
lmML(formula, data = NULL, subset = NULL, na.action = NULL,
  method = "ann", scale = FALSE, parameter = NULL, cv = NULL)
}
\arguments{
\item{formula}{an object of class formula}

\item{data}{data frame}

\item{subset}{indicies on which model is fitted}

\item{na.action}{how to handle nas}

\item{method}{support methods are random forest, deep neural networks}

\item{scale}{how to handle unscales values}

\item{parameter}{list of parameter for method}

\item{cv}{cross validation strategy}
}
\description{
Fit a linear regression with a normal distributed error
}
\author{
Maximilian Pichler
}
