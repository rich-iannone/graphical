# Create simple graph

#devtools::install_github("rich-iannone/DiagrammeR")
library(DiagrammeR)
library(magrittr)

# Creation of graph
#simple_graph <-
#  create_graph_space() %>%
#  add_n_nodes(2) %>% select_nodes %>%
#  add_n_nodes_to_selection(5) %>% clear_selection %>%
#  add_n_nodes(2) %>% add_edge(1, 13) %>% add_edge(2, 13) %>%
#  add_edge(13, 14) %>%
#  set_node_attr(nodes = c(3:12), node_attr = "constant", values = 3) %>%
#  set_node_attr(nodes = c(1, 2, 13), node_attr = "function", values = "sum") %>%
#  set_node_attr(nodes = 14, node_attr = "function", values = "print")

simple_graph <-
  create_graph_space() %>%
  add_constant_node(3) %>% add_constant_node(7) %>%
  add_function_node("sum") 

render_graph(simple_graph)

#
# Evaluation of graph
#

# Get vector of nodes with no predecessors and set their type as 'initial'
initial_nodes <-
  simple_graph %>% select_nodes_by_degree(degree_type = "in", degree_values = 0) %>%
  get_selection() %>% unlist

names(initial_nodes) <- NULL

simple_graph %<>% select_nodes(nodes = initial_nodes) %>%
  set_node_attr_with_selection(node_attr = "type", value = "initial") %>%
  clear_selection

# Get vector of nodes with no successors
final_nodes <-
  simple_graph %>% select_nodes_by_degree(degree_type = "out", degree_values = 0) %>%
  get_selection() %>% unlist

names(final_nodes) <- NULL

simple_graph %<>% select_nodes(nodes = final_nodes) %>%
  set_node_attr_with_selection(node_attr = "type", value = "final") %>%
  clear_selection

# Set the type of all other nodes as 'operation'
simple_graph %<>% select_nodes(nodes = initial_nodes) %>%
  select_nodes(nodes = final_nodes) %>% invert_selection %>%
  set_node_attr_with_selection(node_attr = "type", value = "function") %>%
  clear_selection

# Modify labels
simple_graph %<>% select_nodes(node_attr = "type", search = "initial") %>%
  set_node_attr_with_selection(node_attr = "label", value = "initial") %>%
  clear_selection

simple_graph %<>% select_nodes(node_attr = "type", search = "final") %>%
  set_node_attr_with_selection(node_attr = "label", value = "final") %>%
  clear_selection

simple_graph %<>% select_nodes(node_attr = "type", search = "function") %>%
  set_node_attr_with_selection(node_attr = "label", value = "function") %>%
  clear_selection

# Display the graph
render_graph(simple_graph)

# From each of the starting nodes, push values to their outbound edges
for (i in initial_nodes){
  simple_graph %<>% select_nodes(nodes = i) %>%
    deposit_node_attr_from_selection(node_attr = "constant") %>%
    trav_out_edge %>% set_edge_attr_with_selection(edge_attr = "output",
                                                   value = withdraw_values(.)) %>%
    clear_selection
}

# Initialize index of nodes that have already performed evaluations
receivers_evaluated <- vector(mode = "character")

while (!(all(final_nodes %in% receivers_evaluated))){

  # Determine which nodes are potentially receiving inputs in this iteration
  receivers <-
    simple_graph %>% select_nodes_by_degree(degree_type = "in", degree_values = ">0") %>%
    get_selection %>% unlist

  names(receivers) <- NULL

  # Remove those receivers that have already performed evaluations
  receivers <- setdiff(receivers, receivers_evaluated)

  # For all receivers with pending evaluations, perform those evaluations and push
  # outputs to outward edges
  for (i in receivers){

    # Only perform evaluations for those receivers that have all
    # inputs available
    if (!any(simple_graph %>% select_nodes(nodes = i) %>% trav_in_edge %>%
             deposit_edge_attr_from_selection(edge_attr = "output") %>%
             withdraw_values == "")){

      inputs_to_node <-
        simple_graph %>% select_nodes(nodes = i) %>% trav_in_edge() %>%
        deposit_edge_attr_from_selection(edge_attr = "output") %>% withdraw_values

      function_for_node <-
        simple_graph %>% select_nodes(nodes = i) %>%
        deposit_node_attr_from_selection(node_attr = "function") %>% withdraw_values

      if (!(i %in% final_nodes)){

        simple_graph %<>% select_nodes(nodes = i) %>% trav_out_edge() %>%
          set_edge_attr_with_selection(edge_attr = "output",
                                       value = eval(call(function_for_node,
                                                         as.numeric(inputs_to_node)))) %>%
          clear_selection

      } else {

        simple_graph %<>% select_nodes(nodes = i) %>%
          set_node_attr_with_selection(node_attr = "constant",
                                       value = inputs_to_node) %>%
          clear_selection

        value_for_call <- simple_graph %>% select_nodes(nodes = i) %>%
          deposit_node_attr_from_selection(node_attr = "constant") %>%
          withdraw_values

        function_for_call <- simple_graph %>% select_nodes(nodes = i) %>%
          deposit_node_attr_from_selection(node_attr = "function") %>%
          withdraw_values

        eval(call(function_for_call,
                  as.numeric(value_for_call)))
      }

      receivers_evaluated <- c(receivers_evaluated, i)
    }
  }
}
