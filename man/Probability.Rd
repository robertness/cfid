% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability.R
\name{Probability}
\alias{Probability}
\title{Probability}
\usage{
Probability(
  val = NULL,
  var = NULL,
  do = NULL,
  sumset = NULL,
  summand = NULL,
  terms = NULL,
  numerator = NULL,
  denominator = NULL
)
}
\arguments{
\item{val}{An integer value of either 0 or 1 for almost sure events.}

\item{var}{A list of objects of class \code{CounterfactualVariable}
(without interventions and with value assignments).
\code{var} defines the observations \eqn{y} in \eqn{p(y|...)}.}

\item{do}{A list of objects of class \code{CounterfactualVariable}
(without interventions and with value assignments).
If an interventional probability is defined, these depict the \eqn{do(.)}
variables.}

\item{sumset}{A list of objects of class \code{CounterfactualVariable}
(without interventions and with value assignments).
If the probability depicts marginalization, \code{sumset}
defines the set of variable to be marginalized over.}

\item{summand}{An object of class \code{Probability}. If \code{sumset}
is not \code{NULL}, this defines the probability being marginalized.}

\item{terms}{A list of \code{Probability} objects if the object in question
is meant to represent a product of terms.}

\item{numerator}{An object of class \code{Probability}. If the probability
depicts a conditional probability that cannot be expressed simply
in terms of the set of inputs \eqn{P*}, this is the numerator
of the quotient representation.}

\item{denominator}{An object of class \code{Probability}. The denominator
of the quotient representation.}
}
\value{
An object of class \code{Probability}, which is a list containing
all of the arguments of the constructor.
}
\description{
Defines an interventional or observational probability \eqn{p(y|do(x))}.
}
\details{
For formatting options, see \link{format.Probability}.
When formatted via \code{print} or \code{format}, the  arguments are
prioritized in the following order if conflicting definitions are given:
\code{val}, (\code{var}, \code{do}), (\code{sumset}, \code{summand}), \code{terms},
(\code{numerator}, \code{denominator})
}
\seealso{
\link{CounterfactualVariable}
}
