library(tnet)
library(igraph)

getwd()

network <- read.csv("E:/My Python Projects/My All codes/R/lab8/911-weighted.csv", header=TRUE)
nodes <- read.csv("E:/My Python Projects/My All codes/R/lab8/node-attribs.csv", header=TRUE)

Degree <- degree_w(network)
Degree_df <- data.frame(Node = 1:length(Degree), Degree = as.numeric(Degree))
write.csv(Degree_df, file="E:/My Python Projects/My All codes/R/lab8/degree_results.csv", row.names=FALSE)

hist(Degree, xlab="Degree", main="Histogram of Degree")
hist(Degree, breaks=20, xlab="Degree", main="Histogram of Degree with 20 breaks")

NG <- graph.data.frame(network, directed=FALSE)
Strength <- strength(NG, vids=V(NG), mode="all", weights=E(NG)$weight)
Strength_df <- data.frame(Node = names(Strength), Strength = as.numeric(Strength))
write.csv(Strength_df, file="E:/My Python Projects/My All codes/R/lab8/strength_results.csv", row.names=FALSE)

Betweenness <- betweenness_w(network)
Betweenness_df <- data.frame(Node = 1:length(Betweenness), Betweenness = as.numeric(Betweenness))
write.csv(Betweenness_df, file="E:/My Python Projects/My All codes/R/lab8/betweenness_results.csv", row.names=FALSE)

Closeness <- closeness_w(network)
Closeness_df <- data.frame(Node = 1:length(Closeness), Closeness = as.numeric(Closeness))
write.csv(Closeness_df, file="E:/My Python Projects/My All codes/R/lab8/closeness_results.csv", row.names=FALSE)

Clustering <- clustering_w(network)
Clustering_df <- data.frame(Node = 1:length(Clustering), Clustering = as.numeric(Clustering))
write.csv(Clustering_df, file="E:/My Python Projects/My All codes/R/lab8/clustering_results.csv", row.names=FALSE)

Density <- edge_density(NG)
Density_df <- data.frame(Density = Density)
write.csv(Density_df, file="E:/My Python Projects/My All codes/R/lab8/density_results.csv", row.names=FALSE)

plot(NG, layout=layout.fruchterman.reingold(NG), vertex.size=5, vertex.label=NA, main="Network Visualization")

top_degree <- Degree_df[order(-Degree_df$Degree), ][1:5, ]
top_strength <- Strength_df[order(-Strength_df$Strength), ][1:5, ]
top_betweenness <- Betweenness_df[order(-Betweenness_df$Betweenness), ][1:5, ]
top_closeness <- Closeness_df[order(-Closeness_df$Closeness), ][1:5, ]

print("Top 5 nodes by Degree:")
print(top_degree)
print("Top 5 nodes by Strength:")
print(top_strength)
print("Top 5 nodes by Betweenness:")
print(top_betweenness)
print("Top 5 nodes by Closeness:")
print(top_closeness)

broker_node <- Betweenness_df[which.max(Betweenness_df$Betweenness), ]
print("Broker node (highest betweenness):")
print(broker_node)

print("Degree Distribution Summary:")
print(summary(Degree))
print("Strength Distribution Summary:")
print(summary(Strength))

hist(Degree, breaks=20, xlab="Degree", main="Degree Distribution")
hist(Strength, breaks=20, xlab="Strength", main="Strength Distribution")

report <- "
Network Analysis Report:

- The network has N nodes and M edges (replace with actual counts).
- Density: [Density value] indicates how connected the network is.
- The most central actors (by degree and strength) are nodes: [list top nodes].
- The broker (highest betweenness) is node: [broker_node].
- Degree and strength distributions show that most nodes have few connections, while some nodes act as hubs with high connectivity.
- Global clustering values indicate the presence of clusters in the network.
- Overall, the network exhibits [describe: centralized / decentralized / clustered] structure.
"

cat(report)
nrow(network)
