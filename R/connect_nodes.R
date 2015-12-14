#' Create a connection between nodes
#' @description Create a connection between two different nodes in the graph
#' space.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from the node from which a connection will initiate.
#' @param to the node to which a connection will terminate.
#' @return a graph object of class \code{dgr_graph}.
#' @export connect_nodes

connect_nodes <- function(graph, from, to){
  
  graph <- add_edge(graph, from = from, to = to, rel = "connection")
  
  return(graph)
  
}
