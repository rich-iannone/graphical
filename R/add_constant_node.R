#' Add a constant node
#' @description Add a constant value to the graph, which can either be a
#' number, TRUE/FALSE, or a string. 
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param value the value to use as a constant; can be either \code{numeric},
#' \code{character}, or \code{logical}.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_constant_node

add_constant_node <- function(graph, value){
  
  # Get the number of constants currently in the graph
  if (node_count(graph) == 0){
    constants_in_graph <- 0
  } else {
    node_df <- get_node_df(graph)
    node_id_in_graph <- node_df$nodes
    if (!any(grepl("_c_[0-9][0-9][0-9]$", node_id_in_graph)) == TRUE){
      constants_in_graph <- 0
    } else {
      constants_in_graph <-
        length(grep("_c_[0-9][0-9][0-9]$",
                    node_id_in_graph,
                    value = TRUE))
    }
  }
  
  # Create the node
  constant_node <-
    create_nodes(nodes = paste0("_c_",
                                formatC(constants_in_graph + 1,
                                        width = 3,
                                        format = "d",
                                        flag = "0")),
                 type = "constant",
                 label = paste0("C\n", value),
                 data_value = value,
                 shape = "circle")
  
  # Insert the node into the graph
  graph <- add_node_df(graph, constant_node)
  
  return(graph)
}
