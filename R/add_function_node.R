#' Add a function node
#' @description Add a function to the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param name the name of the function as a character object.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_function_node

add_function_node <- function(graph, name){
  
  # Get the number of functions currently in the graph
  if (node_count(graph) == 0){
    functions_in_graph <- 0
  } else {
    node_df <- get_node_df(graph)
    node_id_in_graph <- node_df$nodes
    
    if (!any(grepl("_f_[0-9][0-9][0-9]$", node_id_in_graph)) == TRUE){
      functions_in_graph <- 0
    } else {
      functions_in_graph <-
        length(grep("_f_[0-9][0-9][0-9]$",
                    node_id_in_graph,
                    value = TRUE))
    }
  }
  
  # Create the node
  function_node <-
    create_nodes(nodes = paste0("_f_",
                                formatC(functions_in_graph + 1,
                                        width = 3,
                                        format = "d",
                                        flag = "0")),
                 type = "function",
                 label = paste0("F -- ", name),
                 function_name = name,
                 shape = "circle")
  
  # Insert the function into the graph
  graph <- add_node_df(graph, function_node)
  
  # Classes for arguments with defaults
  for (i in 1:length(eval(call("formals", name)))){
    if (i == 1) classes <- vector(mode = "character")
    
    classes <- c(classes, class(eval(call("formals", name))[[i]]))
    
    if (i == length(eval(call("formals", name)))){
      classes[which(classes == "name")] <- ""
    }
  }
  
  # Create the node data frame for function's arguments
  argument_nodes <-
    create_nodes(nodes = paste0("_f_",
                                formatC(functions_in_graph + 1,
                                        width = 3,
                                        format = "d",
                                        flag = "0"),
                                "_",
                                names(eval(call("formals", name)))),
                 label = paste0(names(eval(call("formals", name)))),
                 type = "argument",
                 function_name = name,
                 data_value = as.character(eval(call("formals", name))),
                 data_types = classes,
                 shape = "circle")
  
  # Count number of function nodes of this type in the graph
  instance_number <-
    length(which(get_node_df(graph)$label %in% paste0("F -- ", name)))
  
  # Create node which has the instance number
  instance_no_node <-
    create_nodes(nodes = paste0("_f_", formatC(functions_in_graph + 1,
                                               width = 3, format = "d", flag = "0"),
                                "__no"),
                 type = "fcn_inst_no",
                 label = instance_number,
                 function_name = name,
                 shape = "diamond")
  
  # Add the argument nodes to the graph
  graph <- add_node_df(graph, argument_nodes)
  
  # Add the instance number for the function to the graph
  graph <- add_node_df(graph, instance_no_node)
  
  # Add edges from the argument nodes to the function itself
  for (i in 1:length(get_nodes(argument_nodes))){
    graph <- 
      add_edge(graph,
               from = get_nodes(argument_nodes)[i],
               to = get_nodes(function_node),
               rel = "argument_for")
  }
  
  # Add an edge from the instance number to the function itself
  graph <- 
    add_edge(graph,
             from = get_nodes(instance_no_node),
             to = get_nodes(function_node),
             rel = "inst_no")
  
  return(graph)
}
