#' Create graph space
#' @description Initialize a space for the graph.
#' @param output the rendering engine to use. Currently defaults to
#' \code{visNetwork}.
#' @return a graph object of class \code{dgr_graph}.
#' @export create_graph_space

create_graph_space <- function(output = "visNetwork"){
  
  # Create the graph space
  graph <-
    create_graph(graph_attrs = "output = visNetwork")
  
  return(graph)
}
