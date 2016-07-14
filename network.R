#Libraries

library(scholar)
library(scholarnetwork)
library(ggplot2)
library(igraph)

#Loading dataset
d <- extractNetwork(id="jGLKJUoAAAAJ", n=500)

#Cleaning network data
network <- graph_from_data_frame(d$edges, directed = FALSE)
set.seed(123)
l <- layout.fruchterman.reingold(network, niter = 1500) # layout
fc <- walktrap.community(network) # community detection

#Node locations
nodes <- data.frame(l); names(nodes) <- c("x", "y")
nodes$cluster <- factor(fc$membership)
nodes$label <- fc$names
nodes$degree <- degree(network)

#Edge locations
edgelist <- get.edgelist(network, names = FALSE)
edges <- data.frame(nodes[edgelist[,1],c("x", "y")], nodes[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")

#and now visualizing it...
p <- ggplot(nodes, aes(x = x, y = y, color = cluster, label = label, size = degree))
pq <- p + geom_text(color = "black", aes(label = label, size = degree),show.legend = FALSE) +
  # nodes
  geom_point(color = "grey20", aes(fill = cluster), shape = 21, show.legend = FALSE, alpha = 1/2) +
  # edges
  geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2, label = NA),
                data = edges, size = 0.25, color = "grey20", alpha = 1/5) +
				## note that here I add a border to the points
  scale_fill_discrete(labels = labels) +
  scale_size_continuous(range  =  c(5, 8)) +
  theme(
    panel.background  =  element_rect(fill  =  "white"),
    plot.background  =  element_rect(fill = "white"),
    axis.line  =  element_blank(), axis.text  =  element_blank(),
    axis.ticks  =  element_blank(),
    axis.title  =  element_blank(), panel.border  =  element_blank(),
    panel.grid.major  =  element_blank(),
    panel.grid.minor  =  element_blank(),
    legend.background  =  element_rect(colour  =  F, fill  =  "black"),
    legend.key  =  element_rect(fill  =  "black", colour  =  F),
    legend.title  =  element_text(color = "white"),
    legend.text  =  element_text(color = "white")
  ) +
  ## changing size of points in legend
  guides(fill  =  guide_legend(override.aes  =  list(size = 5)))

pq