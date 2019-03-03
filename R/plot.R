#' @export
plot_decision_tree <- function(x, ...) {
  nodes <- rbind(data.frame(node = 'root'), x$df %>% select(node))
  edges <- x$df %>% select(parent, node, formula) %>% mutate(parent = ifelse(is.na(parent), 'root', parent))
  the_graph <- tbl_graph(nodes, edges)
  ggraph(the_graph, layout = 'tree') +
    geom_node_point() +
    geom_edge_link(aes(label = formula), angle_calc = 'along', label_dodge = unit(2.5, 'mm')) +
    geom_node_label(aes(label = node)) +
    theme_void() +
    scale_x_continuous(expand = expand_scale(mult = 0.2))
}
