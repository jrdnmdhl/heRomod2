#' Define Decision Trees
parse_tree_vars <- function(trees) {
  
  # Make and return a variables list containing the decision trees
  if (!is.null(trees)) {
    
    # Check that tree specification is valid
    check_trees_df(trees)
    
    unique(trees$name) %>%
      set_names(.) %>%
      map(function(x) {
        # Subset the master tree list to the entries for the given tree.
        tree_df <- filter(trees, name == x)
        
        # Extract the dependencies for each node
        vars_by_node <- tree_df %>%
          rowwise() %>%
          group_split() %>%
          set_names(tree_df$node) %>%
          map(function(y) {
            vars <- all.vars(parse(text = y$formula), functions = T)
            # If a node variable references C, effectively its dependencies include
            # the dependencies of all other nodes at with the same parent in the tree.
            if ('C' %in% vars) {
              vars <- filter(tree_df, parent == y$parent) %>%
                .$formula %>%
                map(~all.vars(parse(text = .), functions = T)) %>%
                flatten_chr() %>%
                unique()
            }
            list(
              node = y$node,
              tags = c(y$node, parse_csl(y$tags)),
              depends = vars
            )
          })
        
        # Collapse to a character vector
        vars <- unique(flatten_chr(map(vars_by_node, ~.$depends)))
        
        # Define a variable which will create the decision tree object.
        hero_var <- define_formula(paste0('decision_tree(.trees, "', x, '")'))
        
        # Add the variables from the tree itself to the 'after' field to ensure the
        # tree is evaluated after all variables referenced by it
        hero_var$after <- vars
        
        # Add the list of dependencies by node so that calls that references nodes can
        # inherit those dependencies
        hero_var$node_depends <- vars_by_node
        
        # Return the row
        tibble(
          name = x,
          display_name = x,
          description = x,
          formula = list(hero_var)
        )
      }) %>%
      bind_rows()
  } else {
    tibble()
  }
}

check_trees_df <- function(trees) {
  
  # Check that it has the right columns
  tree_colnames <- colnames(trees)
  missing <- setdiff(tree_def_columns, tree_colnames)
  if (length(missing) != 0) {
    missing_str <- err_name_string(missing)
    error_msg <- paste0(
      'Error in decision tree specification, missing columns: ',
      missing_str,
      '.'
    )
    stop(error_msg, call. = F)
  }
  
  group_by(trees, name) %>%
    group_split() %>%
    map(check_tree_df)
}

check_tree_df <- function(df) {
  
  # Extract the node names
  node_names <- df$node
  
  # Check that node names are unique
  dupes <- duplicated(node_names)
  if (any(dupes)) {
    dupes_msg <- err_name_string(unique(node_names[dupes]))
    error_msg <- paste0(
      'Error in decision tree specification, tree "',
      df$name[1],
      '" contained duplicate node names: ',
      dupes_msg,
      '.'
    )
    stop(error_msg, call. = F)
  }
  
  # Extract the tag names
  tag_names_list <- parse_csl(df$tags, flatten = F)
  bad_tags <- map_lgl(tag_names_list, function(x) any(!(is.na(x) | is_valid_name(x))))
  if (any(bad_tags)) {
    bad_nodes <- node_names[bad_tags]
    error_msg <- paste0(
      'Error in decision tree specification, tree "',
      df$name[1],
      '" contained invalid tag names for nodes: ',
      err_name_string(bad_nodes),
      ". Tag names must be provided in a comma-separated list, start with a letter and contain only letters, numbers, and underscores."
    )
    stop(error_msg, call. = F)
  }
  
  tag_names <- flatten_chr(tag_names_list)
  
  # Check for overlap between tag and node names
  intersection <- intersect(tag_names, node_names)
  if (length(intersection) != 0) {
    inter_str <- err_name_string(intersection)
    error_msg <- paste0(
      'Error in decision tree "',
      df$name[1],
      '", tag names were duplicates of node names: ',
      inter_str,
      '.'
    )
    stop(error_msg, call. = F)
  }
  
}

#' @export
decision_tree <- function(df, name) {
  
  the_env <- parent.frame()
  
  # Pull out tree from trees df
  tree_df <- filter(df, name == name)
  tree_df$parent <- ifelse(is.na(tree_df$parent), '', tree_df$parent)
  
  parent_names <- unique(tree_df$parent)
  terminal_node_names <- tree_df$node[tree_df$node %in% parent_names]
  
  # Calculate the conditional probabilities level-by-level
  cond_prob <- parent_names %>%
    lapply(function(x) {
      # Get the subtree
      subtree <- filter(tree_df, parent == x)
      # Parse subtree as variables
      subtree_vars <- subtree %>%
        mutate(name = node, display_name = node, description = node) %>%
        parse_variables()
      
      # Create a namespace from parent environment
      ns <- define_namespace(the_env, data.frame(the_env$cycle))
      
      # Evaluate the variables
      res <- eval_variables(subtree_vars, ns, T, 'trees')
      
      # Put into a matrix
      mat <- as.matrix(res$df[subtree_vars$name])

      # Calculate complementary probabilities
      c_index <- mat == -pi
      mat[c_index] <- 0
      if (any(rowSums(c_index) > 1)) {
        error_msg <- paste0('Error in calculating complementary probabilities, "C" may be used only once per level.')
        stop(error_msg, call. = F)
      }
      mat[c_index] <- 1 - rowSums(mat)[which(c_index, arr.ind = TRUE)[, -2]] 
      as.list(as.data.frame(mat))
    }) %>%
    flatten() %>%
    do.call(data.frame, .)

  # Isolate to the terminal nodes
  terminal_nodes <- filter(tree_df, !node %in% parent) %>%
    rowwise() %>%
    group_split() %>%
    map(function(x) {
      prob <- cond_prob[[x$node]]
      tags <- c(x$node, x$tags)
      parent <- x$parent
      while (!is.empty(parent)) {
        parent_prob <- cond_prob[[parent]]
        parent_df <-  tree_df[tree_df$node == parent,]
        parent <- parent_df$parent
        parent_tags <- parent_df$tags[1]
        parent_node <- parent_df$node[1]
        prob <- prob * parent_prob
        tags <- c(tags, parent_node, parent_tags)
      }
      list(node = x$node, prob = prob, tags = parse_csl(tags))
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
