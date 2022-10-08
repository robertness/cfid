# Utility tests

test_that("edgelist is working.", {
  g <- cfid::dag("X -> W -> Y <- Z <- D x <-> Y")
  actual <- get_edgelist(g)
  expected = data.frame(
    from=c("U[X,Y]", "X", "W", "Z", "U[X,Y]", "D"),
    to=c("X", "W", "Y", "Y", "Y", "Z")
  )
  expect_equal(actual, expected)
})
