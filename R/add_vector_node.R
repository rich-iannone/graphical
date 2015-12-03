#' Add a vector node
#' @description Add a vector to the graph, which can either consist of 2 or more
#' numbers, TRUE/FALSE values, or strings. 
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param value the vector to be used; can be either composed of \code{numeric},
#' \code{character}, or \code{logical} values.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_vector_node

add_vector_node <- function(graph, value){
  
  # Get the number of vectors currently in the graph
  if (node_count(graph) == 0){
    vectors_in_graph <- 0
  } else {
    node_df <- get_node_df(graph)
    node_id_in_graph <- node_df$nodes
    
    if (!any(grepl("_v_[0-9][0-9][0-9]$", node_id_in_graph)) == TRUE){
      vectors_in_graph <- 0
    } else {
      vectors_in_graph <-
        length(grep("_v_[0-9][0-9][0-9]$",
                    node_id_in_graph,
                    value = TRUE))
    }
  }
  
  # Create the node
  vector_node <-
    create_nodes(nodes = paste0("_v_",
                                formatC(vectors_in_graph + 1,
                                        width = 3,
                                        format = "d",
                                        flag = "0")),
                 type = "vector",
                 label = paste0("vect(num): ",
                                value[1], " ... ",
                                value[length(value)]),
                 data_value = paste0("vect(num): ",
                                     value[1], " ... ",
                                     value[length(value)]),
                 shape = "circle")
  
  # Insert the vector into the graph
  graph <- add_node_df(graph, vector_node)
  
  return(graph)
}
