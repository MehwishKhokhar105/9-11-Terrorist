library(igraph)

network <- read.csv("E:\\My Python Projects\\My All codes\\R\\Assigment\\facebook_combined-csv.csv", header=TRUE)

g <- graph_from_data_frame(network, directed=FALSE)
g <- simplify(g, remove.multiple=TRUE, remove.loops=TRUE)

comp <- components(g)
giant_vids <- which(comp$membership == which.max(comp$csize))
g_giant <- induced_subgraph(g, vids = giant_vids)

plot(g_giant, vertex.size=3, vertex.label=NA, edge.arrow.size=0.2,
     layout=layout_with_fr(g_giant))
