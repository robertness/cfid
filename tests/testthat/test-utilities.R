# Utility tests

test_that("get_edgestring is working (no intervention terms).", {
  # Returns a string of edges compatible with dagitty
  g <- cfid::dag("X -> W -> Y <- Z <- D x <-> Y")
  actual <- get_edgestring(g)
  expected <- "U_XY -> X X -> W W -> Y Z -> Y U_XY -> Y D -> Z"
  expect_equal(actual, expected)
})

test_that("get_edgestring returns string compatible with dagitty.", {
  g <- cfid::dag("X -> W -> Y <- Z <- D x <-> Y")
  actual <- get_edgestring(g)
  dagitty_format <- paste0("dag {", actual, "}")
  output <- dagitty::dagitty(dagitty_format)
  expect_equal(class(output), "dagitty")
})

test_that("edgelist is working.", {
  g <- cfid::dag("X -> W -> Y <- Z <- D x <-> Y")
  actual <- get_edgelist(g)
  expected = data.frame(
    from=c("U[X,Y]", "X", "W", "Z", "U[X,Y]", "D"),
    to=c("X", "W", "Y", "Y", "Y", "Z")
  )
  expect_equal(actual, expected)
})
