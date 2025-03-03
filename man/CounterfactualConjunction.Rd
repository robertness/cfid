% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf_conjunction.R
\name{CounterfactualConjunction}
\alias{CounterfactualConjunction}
\alias{conj}
\title{Counterfactual Conjunction}
\usage{
CounterfactualConjunction(...)

conj(...)
}
\arguments{
\item{...}{Objects of class \code{CounterfactualVariable}.}
}
\value{
An object of class \code{CounterfactualConjunction}.
}
\description{
Defines a conjunction of counterfactual statements (variables).
}
\details{
A counterfactual conjunction is a conjunction (or a set in some contexts)
of counterfactual statements that are assumed to hold simultaneously.

For example, the statement "The value of \eqn{Y} was observed to
be \eqn{y}, and the value of \eqn{Y} was observed to be \eqn{y'}
under the intervention \eqn{do(X = x)}" consists of two variables:
variable \eqn{Y} without intervention, and \eqn{Y} under the intervention
\eqn{do(X = x)} (which is \eqn{Y_x}). This conjunction can be succinctly
written as \eqn{y \wedge y'_x}.

Conjunctions can also be constructed
via the alias \code{conj} or iteratively from \code{CounterfactualVariable}
objects (see examples).
}
\examples{
# The conjunction described under 'details'
v1 <- cf("Y", 0)
v2 <- cf("Y", 1, c("X" = 0))
c1 <- conj(v1, v2)

# Alternative construction
c1 <- v1 + v2

# Adding further variables
v3 <- cf("X", 1)
c2 <- c1 + v3

# A specific variable (a unique combination of `var` and `int`)
# can only appear once in a given conjunction,
# otherwise the conjunction would be trivially inconsistent
v4 <- cf("Y", 0, c("X" = 0))
v5 <- cf("Y", 1, c("X" = 0))
c3 <- try(conj(v4, v5))
}
\seealso{
\link{CounterfactualVariable}
}
