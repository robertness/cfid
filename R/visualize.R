g1 <- dag("X -> W -> Y <- Z <- D X <-> Y")
v1 <- cf("Y", 0, c(X = 0))
v2 <- cf("X", 1)
v3 <- cf("Z", 0, c(D = 0))
v4 <- cf("D", 0)
c1 <- conj(v1, v2, v3, v4)
identifiable(g1, c1)

tmp <- make_cg(g1, c1)
adj_mat <- tmp$graph
names_ <- unlist(lapply(attr(adj_mat, "label"), function(x) format(x)))
rownames(adj_mat) <- names_
colnames(adj_mat) <- names_
ig <- igraph::graph_from_adjacency_matrix(adj_mat)
plot(ig, layout=layout_as_tree)

