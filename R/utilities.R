collapse <- function(...) {
    paste0(..., collapse = "")
}

comma_sep <- function(...) {
    paste0(..., collapse = ",")
}

rep_char <- function(x, n) {
    paste0(rep(x, n), collapse = "")
}

reg_match <- function(x, pattern, perl = TRUE) {
    m <- gregexec(pattern, x, perl)
    regmatches(x, m)[[1L]]
}

try_type <- function(..., type) {
    if (missing(type)) {
        stop_("Argument `type` must be given")
    }
    dots <- list(...)
    arg_name <- names(dots)[1L]
    out <- try(do.call(paste0("as.", type), list(dots[[1L]])), silent = TRUE)
    if ("try-error" %in% class(out)) {
        stop_("Unable to coerce argument `", arg_name, "` to `", type, "`")
    }
    names(out) <- names(dots[[1L]])
    out
}

val_consistent <- function(x, y) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (is.null(y)) {
        return(TRUE)
    }
    if (identical(x, y)) {
        return(TRUE)
    }
    return(FALSE)
}

stop_ <- function(...) {
    stop(..., call. = FALSE)
}

check_conflicts <- function(x, y) {
    if (missing(y)) {
        conf <- trivial_conflicts(x)
    } else {
        conf <- trivial_conflict(x, y)
    }
    if (length(conf)) {
        conf_form <- comma_sep(sapply(conf, format))
        stop_("Inconsistent definitions given for variables: ", conf_form)
    }
}

get_nodes <- function(g){
  nodes <- attr(g, "labels")
  if(class(nodes) != "character"){
    nodes <- sapply(nodes, format)
  }
  return(nodes)
}

get_edgelist <- function(g){
  nodes <- get_nodes(g)
  mat <- g
  attr(mat, "class") <- "matrix"
  colnames(mat) <- nodes
  rownames(mat) <- nodes
  el <- reshape2::melt(mat)
  el <- dplyr::filter(el, value==1)
  el <- dplyr::select(el, -value)
  colnames(el) <- c("from", "to")
  el <- dplyr::mutate(el, from=as.character(from), to=as.character(to))
  return(el)
}
