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
  
  # Count number of function nodes of this type in the graph
  if (is.null(graph$nodes_df)){
    instance_number <- 1
  } else {
  instance_number <-
    length(which(get_node_df(graph)$label %in% paste0("F\n", name)))
  }
  
  # Create the node
  function_node <-
    create_nodes(nodes = paste0("_f_",
                                formatC(functions_in_graph + 1,
                                        width = 3,
                                        format = "d",
                                        flag = "0")),
                 type = "function",
                 label = paste0("F\n", name, "\n(", instance_number, ")"),
                 function_name = name,
                 shape = "circle")
  
  # Insert the function into the graph
  graph <- add_node_df(graph, function_node)
  
  function_args_df <- 
    data.frame(mat.or.vec(nr = length(names(eval(call("formals", name)))),
                          nc = 3),
               stringsAsFactors = FALSE)
  
  colnames(function_args_df) <- c("arg_name", "arg_required", "dflt_val")
  
  function_args_df$arg_name <- names(eval(call("formals", name)))
  
  # Get argument list
  arg_list <- as.list(eval(call("formals", name)))
  
  for (i in 1:length(arg_list)){
    
    if (i == 1) args_required <- vector(mode = "character")
    
    if (is.character(as.list(eval(call("formals", name)))[i][[1]]) |
                             is.logical(as.list(eval(call("formals", name)))[i][[1]])){
      arg_required <- FALSE
    }
    
    arg_required <- ifelse(is.null(as.list(eval(call("formals", name)))[[i]]), FALSE, TRUE)
    
    arg_required <- ifelse(nchar(as.character(eval(call("formals", name)))[i]) > 0,
                           FALSE, TRUE)
    
    if (formalArgs(name)[i] == "...") arg_required <- FALSE
    
    if (is.character(arg_list[[i]])) arg_required <- FALSE
    
    args_required <- c(args_required,
                       arg_required)
  }
  
  function_args_df$arg_required <- args_required
  
  function_args_df$dflt_val <- as.character(eval(call("formals", name)))
  
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
  # Create default argument nodes for each function argument
  for (i in which(function_args_df$arg_required == FALSE)){
    
    #graph <- add_constant_node(graph, value = function_args_df[i,3])
    
    def_arg_node <-
      create_nodes(nodes = paste0("_default_f",
                                  instance_number,i),
                   type = "default_val",
                   label = paste0("D\n", function_args_df[i,3]),
                   data_value = function_args_df[i,3],
                   shape = "circle")
    graph <- add_node_df(graph, def_arg_node)
    graph <- select_last_node(graph)
    graph <- add_edge(graph, from = get_selection(graph)[[1]],
             to = paste0("_f_",
                         formatC(functions_in_graph + 1,
                                 width = 3,
                                 format = "d",
                                 flag = "0"),
                         "_",
               function_args_df$arg_name[i]),
             rel = "default_val")
  }
  
  return(graph)
}
