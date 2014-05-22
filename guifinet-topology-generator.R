##
## Example of the topology generator proposed in
##
## Cerdà-Alabern L.  2012.  On the Topology Characterization of
## Guifi.net. 2012 IEEE 8th International Conference on Wireless and
## Mobile Computing, Networking and Communications (WiMob'2012).
##
## (c) Llorenç Cerdà, March 2013

library(igraph)

connection.preference <- function(term.nodes, i, j) {
  x <- term.nodes[i]
  y <- term.nodes[j]
  res <- max(x * y, 1)
  return(res)
}
##
## Build the synthetic graph
##
## Set full=TRUE to get the graph with the hidden terminals.
##
build.synthetic.graph <- function(nodes, links, shape, rate, seed=1, full=FALSE) {
  set.seed(seed)
  num.core.nodes <- nodes
  num.core.links <- links
  ##
  term.nodes <- sort(round(rgamma(num.core.nodes, rate=rate, shape=shape)))
  g.sim = graph.empty(num.core.nodes, directed=FALSE)
  v.pairs <- combn(1:num.core.nodes,2)
  ##
  ## First create a link for each node
  ##
  ## connect.nodes <- max(which(term.nodes<1)) # floor(num.core.nodes/2)
  connect.nodes <- num.core.nodes
  for(i in 1:connect.nodes) {
    while(1) {
      idx.to <- (1:connect.nodes)[-i]
      v.to <- sample(idx.to, size=1, prob=term.nodes[idx.to])
      if(!are.connected(g.sim, i, v.to)) {
        g.sim <- add.edges(g.sim, c(i, v.to))
        rm.pair <- which((v.pairs[1,]==i) & (v.pairs[2,]==v.to))
        if(length(rm.pair) == 0) {
          rm.pair <- which((v.pairs[2,]==i) & (v.pairs[1,]==v.to))
        }
        if(length(rm.pair) == 0) {
          cat("pair not found:", i, ",", v.to, "\n")
          break
        }
        v.pairs <- v.pairs[,-rm.pair]
        break
      }
    }
  }
  ##
  ## connect the remaining edges choosing the connection preference
  ##
  v.paris.prob <-
    sapply(1:ncol(v.pairs),
           function(n) connection.preference(term.nodes, v.pairs[1,n], v.pairs[2,n]))
  ##
  connect.pairs <-
    sample.int(n=ncol(v.pairs), size=num.core.links-connect.nodes,
               replace=FALSE, prob=v.paris.prob)
  g.sim <- add.edges(g.sim, c(v.pairs[,connect.pairs]))
  ##
  ## Add the hidden terminals
  ##
  if(full) {
    for(node in 1:length(V(g.sim))) {
      if(term.nodes[node] > 0) {
        v.from <- node
        for(term in 1:term.nodes[node]) {
          v.to <- length(V(g.sim))
          g.sim <- add.vertices(g.sim, 1)
          g.sim <- add.edges(g.sim, c(v.from, v.to))
        }
      }
    }
  }
  return(g.sim)
}

## Values of Osona zone
n.core.nodes <- 266
n.core.links <- 398
rate.o <- 0.01147359
shape.o <- 0.2521602

g.sim <- build.synthetic.graph(nodes=n.core.nodes, links=n.core.links, rate=rate.o, shape=shape.o)

l <- layout.drl(g.sim, options=list(simmer.attraction=0.1))
plot(g.sim, layout=l, vertex.size=3, edge.arrow.size=0.5,
     vertex.label=rep("",vcount(g.sim)))


