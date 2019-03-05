#' Define a decision Tree
#' 
#' @param df a data.frame representing the decision trees.
#' 
#' @export
define_decision_trees <- function(ns) {
  
  # Make and return a variables list containing the decision trees
  if (!is.null(ns$env$.trees)) {
    unique(ns$env$.trees$tree) %>%
      set_names(.) %>%
      map(function(x) {
        
        # subset the master tree list to the entries for the given tree.
        tree_df <- filter(ns$env$.trees, .data$tree == x)
        
        # Extract from each node the variables referenced and collpase to a
        # character vector.
        vars <- ns$env$.trees$formula %>%
          map(~all.vars(parse(text = .), functions = T)) %>%
          flatten_chr()
        
        # Define a variable which will create the decision tree object.
        hero_var <- define_variable(paste0('decision_tree(.trees, "', x, '")'))
        
        # Add the variables from the tree itself to the dependencies so that it
        # will be evaluated only after those variables.
        hero_var$vars <- c(hero_var$vars, vars)
  
        hero_var
      }) %>%
      as.heRovar_list
  } else {
    list()
  }
}

parse_csl <- function(string) {
  gsub('\\s', '', string) %>%
    strsplit(',') %>%
    flatten_chr()
}

check_tree_df <- function(df, name) {
  
  # Check that it has the right columns
  tree_colnames <- colnames(df)
  missing <- setdiff(tree_def_columns, tree_colnames)
  if (length(missing) != 0) {
    missing_str <- paste(missing, collapse = ', ')
    error_msg <- glue('Error in tree "{name}", missing columns: {missing_str}.')
    stop(error_msg, call. = F)
  }
  
  # Extract the node names
  node_names <- df$node
  
  # Check that node names are unique
  if (length(node_names) != length(unique(node_names))) {
    error_msg <- glue('Error in tree "{name}", node names must be unique.')
    stop(error_msg, call. = F)
  }
  
  # Extract the tag names
  tag_names <- tryCatch(parse_csl(df$tags), err = function(e) {
    error_msg <- glue('Error in tree "{name}", tags field must be comma-separated list.')
    stop(error_msg, call. = F)
  })
  
  # Check for overlap between tag and node names
  intersection <- intersect(tag_names, node_names)
  if (length(intersection) != 0) {
    inter_str <- paste(intersection, collapse = ', ')
    error_msg <- glue('Error in tree "{name}", node names {inter_str} may not appear in tags list')
    stop(error_msg, call. = F)
  }
  
}

#' @export
decision_tree <- function(df, name) {
  
  the_env <- parent.frame()
  
  # Pull out tree from trees df and check it
  tree_df <- filter(df, .data$tree == name)
  check_tree_df(tree_df, name)
  tree_df$parent <- ifelse(is.na(tree_df$parent), '', tree_df$parent)
  
  parent_names <- unique(tree_df$parent)
  terminal_node_names <- tree_df$node[tree_df$node %in% parent_names]
  
  # Calculate the conditional probabilities level-by-level
  cond_prob <- parent_names %>%
    map(function(x) {
      # Get the subtree
      subtree <- filter(tree_df, parent == x)
      # Evaluate the formulas
      values <- subtree$formula %>%
        map(~as.lazy(., env = the_env) %>%
            interp(C = -pi) %>%
            lazy_eval())
      # Put into a matrix
      mat <- do.call(cbind, values)
      # Calculate complementary probabilities
      colnames(mat) <- subtree$node
      c_index <- mat == -pi
      mat[c_index] <- 0
      if (any(rowSums(c_index) > 1)) {
        error_msg <- glue('Error in tree "{name}", "C" may be used only once per level.')
        stop(error_msg, call. = F)
      }
      mat[c_index] <- 1 - rowSums(mat)[which(c_index, arr.ind = TRUE)[, -2]] 
      as.list(as.data.frame(mat))
    }) %>%
    flatten() %>%
    do.call(data.frame, .)

  # Isolate to the terminal nodes
  terminal_nodes <- filter(tree_df, !.data$node %in% .data$parent) %>%
    rowwise() %>%
    group_split() %>%
    map(function(x) {
      prob <- cond_prob[[x$node]]
      tags <- c(x$node, parse_csl(x$tags))
      parent <- x$parent
      while (!is.empty(parent)) {
        parent_prob <- cond_prob[[parent]]
        parent_df <-  tree_df[tree_df$node == parent,]
        parent <- parent_df$parent
        parent_tags <- parent_df$tags[1]
        parent_node <- parent_df$node[1]
        prob <- prob * parent_prob
        tags <- c(tags, parent_node, parse_csl(parent_tags))
      }
      list(node = x$node, prob = prob, tags = tags)
    })
  
  tag_names <- unique(map(terminal_nodes, ~.$tags) %>% flatten_chr())
  
  subtrees <- tag_names %>%
    set_names(.) %>%
    map(function(tag) {
      subtree <- keep(terminal_nodes, ~tag %in% .$tags)
      define_object_(subtree, class = 'subtree')
    })
  
  define_object(
    df = df,
    terminal_nodes = terminal_nodes,
    cond_prob = cond_prob,
    subtrees = subtrees,
    all = define_object_(terminal_nodes, class = 'subtree'),
    class = 'eval_decision_tree'
  )
}

#' @export
p <- function(statement, tree) {
  
  # Create & populate environment in which to evaluate probability statement
  my_env <- new.env(parent =  parent.frame())
  assign('|', `%given%`, envir = my_env)
  assign('%and%', `%and%`, envir = my_env)
  assign('%or%', `%or%`, envir = my_env)
  n_subtrees <- length(tree$subtrees)
  for (i in seq_len(n_subtrees)) {
    subtree <- tree$subtrees[[i]]
    name <- names(tree$subtrees)[i]
    assign(name, subtree, envir = my_env)
  }
  assign('.all', tree$all, envir = my_env)
  
  # Evaluate probability statement
  lazy_statement <- as.lazy(interp(lazy(statement), `-` = `not`))
  lazy_statement$env <- my_env
  res <- lazy_eval(lazy_statement)
  
  # Extract probability from result
  get_prob(res)
}

check_subtree <- function(x) {
  
  # Check classes
  if (!'subtree' %in% class(x)) {
    error_msg <- 'Error, argument must be of type "subtree"'
    stop(error_msg)
  }
  
}

`%and%` <- function(a, b) {
  
  # Check arguments
  check_subtree(a)
  check_subtree(b)
  
  # Extract node names and find intersection
  nodes_a <- map_chr(a, ~.$node)
  nodes_b <- map_chr(b, ~.$node)
  intersection <- intersect(nodes_a, nodes_b)
  
  # Return intersection
  res <- keep(a, ~.$node %in% intersection)
  define_object_(res, 'subtree')
  
}

`%or%` <- function(a, b) {
  
  # Check arguments
  check_subtree(a)
  check_subtree(b)
  
  # Extract node names and find difference
  nodes_a <- map_chr(a, ~.$node)
  nodes_b <- map_chr(b, ~.$node)
  difference <- setdiff(nodes_a, nodes_b)
  
  # Return the union, careful to avoid duplicates
  # (items in a not in b plus all items in b)
  the_union <- c(keep(a, ~.$node %in% difference), b)
  define_object_(the_union, 'subtree')
}

`%given%` <- function(a, b) {
  
  # Check arguments
  check_subtree(a)
  check_subtree(b)

  # Sum up numerator & denominator probabilities
  res <- list(
    numerator = a %and% b,
    denominator = b
  )
  
  define_object_(res, class = 'cond_prob')
}

not <- function(a) {
  .all <- get('.all', envir = parent.frame())
  check_subtree(a)
  a_nodes <- map_chr(a, ~.$node)
  complement <- keep(.all, ~!(.$node %in% a_nodes))
  define_object_(complement, 'subtree')
}

#' @export
get_prob <- function(x) {
  UseMethod('get_prob', x)
}

#' @export
get_prob.subtree <- function(x) {
  map(x, ~.$prob) %>% reduce(., `+`)
}

#' @export
get_prob.cond_prob <- function(x) {
  get_prob(x$numerator) / get_prob(x$denominator)
}
