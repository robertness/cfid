#' Counterfactual Conjunction
#'
#' Defines a conjunction of counterfactual statements (variables).
#'
#' @param ... Objects of class `CounterfactualVariable`.
#'
#' @details
#' A counterfactual conjunction is a conjunction (or a set in some contexts)
#' of counterfactual statements that are assumed to hold simultaneously.
#'
#' For example, the statement "The value of \eqn{Y} was observed to
#' be \eqn{y}, and the value of \eqn{Y} was observed to be \eqn{y'}
#' under the intervention \eqn{do(X = x)}" consists of two variables:
#' variable \eqn{Y} without intervention, and \eqn{Y} under the intervention
#' \eqn{do(X = x)} (which is \eqn{Y_x}). This conjunction can be succinctly
#' written as \eqn{y \wedge y'_x}.
#'
#' Conjunctions can also be constructed
#' via the alias `conj` or iteratively from `CounterfactualVariable`
#' objects (see examples).
#'
#' @return An object of class `CounterfactualConjunction`.
#'
#' @seealso [cfid::CounterfactualVariable]
#'
#' @examples
#' # The conjunction described under 'details'
#' v1 <- cf("Y", 0)
#' v2 <- cf("Y", 1, c("X" = 0))
#' c1 <- conj(v1, v2)
#'
#' # Alternative construction
#' c1 <- v1 + v2
#'
#' # Adding further variables
#' v3 <- cf("X", 1)
#' c2 <- c1 + v3
#'
#' # A specific variable (a unique combination of `var` and `int`)
#' # can only appear once in a given conjunction,
#' # otherwise the conjunction would be trivially inconsistent
#' v4 <- cf("Y", 0, c("X" = 0))
#' v5 <- cf("Y", 1, c("X" = 0))
#' c3 <- try(conj(v4, v5))
#' @export
CounterfactualConjunction <- function(...) {
    dots <- list(...)
    if (all(sapply(dots, is.CounterfactualVariable))) {
        check_conflicts(dots)
        structure(unique(dots), class = "CounterfactualConjunction")
    } else {
        stop_("All arguments must be of class `CounterfactualVariable`")
    }
}

as.CounterfactualConjunction <- function(x) {
    UseMethod("as.CounterfactualConjunction")
}

as.CounterfactualConjunction.list <- function(x) {
    do.call(CounterfactualConjunction, x)
}

as.CounterfactualConjunction.default <- function(x) {
    if (is.CounterfactualConjunction(x)) {
        x
    } else {
        stop_("Cannot object to class `CounterfactualConjunction`", x)
    }

}

is.CounterfactualConjunction <- function(x) {
    inherits(x, "CounterfactualConjunction")
}

#' @export
format.CounterfactualConjunction <- function(x, varsep = " \u2227 ", ...) {
    cf <- sapply(x, function(y) format.CounterfactualVariable(y, ...))
    paste0(cf, collapse = varsep)
}

#' @export
print.CounterfactualConjunction <- function(x, ...) {
    cat(format(x, ...), "\n")
}

#' @export
`+.CounterfactualConjunction` <- function(e1, e2) {
    if (is.CounterfactualConjunction(e1)) {
        if (is.CounterfactualConjunction(e2)) {
            y <- c(e1, e2)
            check_conflicts(y)
            out <- structure(unique(y),
                             class = "CounterfactualConjunction")
        } else if (is.CounterfactualVariable(e2)) {
            x <- list(e2)
            if (x %in% e1) {
                y <- e1
            } else {
                check_conflicts(e2, e1)
                y <- c(e1, x)
            }
            out <- structure(y, class = "CounterfactualConjunction")
        } else {
            stop_("Unable to add object of class `", class(e2),
                  "` to a counterfactual conjunction")
        }
    } else if (is.CounterfactualVariable(e1)) {
        if (is.CounterfactualConjunction(e2)) {
            x <- list(e1)
            if (x %in% e2) {
                y <- e2
            } else {
                check_conflicts(e1, e2)
                y <- c(x, e2)
            }
            out <- structure(y, class = "CounterfactualConjunction")
        } else if (is.CounterfactualVariable(e2)) {
            out <- CounterfactualConjunction(e1, e2)
        } else {
            stop_("Unable to add object of class `", class(e2),
                  "` to a counterfactual variable")
        }
    } else {
        stop_("Unsupported input for method `+.CounterfactualConjunction`")
    }
    out
}

#' @export
`[.CounterfactualConjunction` <- function(x, i) {
    as.CounterfactualConjunction(NextMethod())
}

#' @export
`+.CounterfactualVariable` <- `+.CounterfactualConjunction`


#' @rdname CounterfactualConjunction
#' @export
conj <- CounterfactualConjunction
