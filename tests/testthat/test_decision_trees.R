context("Decision Trees")
library(tibble)

test_that("Calculate conditional & unconditional probabilities", {
  
  # Create a namespace in which to evaluate variables
  ns <- heRomod2:::define_namespace(data.frame(model_time = c(1,2,3)), new.env())
  
  # Make sure trees table is populated in namespace
  ns$env$.trees <- tribble(
    ~tree, ~node,~tags, ~parent, ~formula,
    'tree', 'had_event', 'event', NA, '0.15 + 0.005 * model_time',
    'tree', 'had_surgery', 'event, surgery', 'had_event', '0.34',
    'tree', 'died_surgery', 'event, died, surgery', 'had_surgery', 'p_death_surgery',
    'tree', 'survived_surgery', 'event, survived, surgery', 'had_surgery', 'C',
    'tree', 'no_surgery', 'event', 'had_event', 'C',
    'tree', 'died_event', 'event, died', 'no_surgery', 'p_death_event',
    'tree', 'survived_event', 'event, survived', 'no_surgery', 'C',
    'tree', 'no_event', '', NA, 'C',
    'tree', 'died_no_event', 'died', 'no_event', 'p_death_no_event',
    'tree', 'survived_no_event', 'survived', 'no_event', 'C'
  )
  
  # Define the variables list
  vars <- tribble(
    ~name, ~formula,
    'p_death_no_event', '0.01',
    'p_death_event', '0.15',
    'p_death_surgery', '0.05',
    'p_event', 'p(event, tree)',
    'p_died', 'p(died, tree)',
    'p_died_given_event', 'p(died | event, tree)',
    'p_event_given_died', 'p(event | died, tree)',
    'p_died_or_surgery', 'p(died %or% surgery, tree)',
    'p_died_and_surgery', 'p(died %and% surgery, tree)',
    'p_died_or_survived_and_had_event_given_surgery', 'p(((died %or% survived) %and% had_event) | surgery, tree)',
    'p_died_or_survived_and_event_given_surgery', 'p(((died %or% survived) %and% event) | surgery, tree)',
    'p_died_or_survived_and_surgery_given_event', 'p(((died %or% survived) %and% surgery) | event, tree)'
  )
  var_list <- heRomod2:::define_variable_list(vars) %>%
    append(define_decision_trees(ns), .) %>%
    as.heRovar_list %>%
    sort()
  
  # Evaluate
  var_res <- heRomod2:::evaluate_variable_list(var_list, ns)
  
  # Check results
  expect_equal(var_res['p_event'], 0.15 + 0.005 * c(1,2,3))
  expect_equal(
    var_res['p_died'],
    (0.15 + 0.005 * c(1,2,3)) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * c(1,2,3))) * 0.01
  )
  expect_equal(var_res['p_died_given_event'], rep((0.34 * 0.05) + ((1 - 0.34) * 0.15), 3))
  expect_equal(
    var_res['p_event_given_died'],
    ((0.15 + 0.005 * c(1,2,3)) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15))) / ((0.15 + 0.005 * c(1,2,3)) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * c(1,2,3))) * 0.01)
  )
  expect_equal(
    var_res['p_died_or_surgery'],
    (0.15 + 0.005 * c(1,2,3)) * (0.34 + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * c(1,2,3))) * 0.01
  )
  expect_equal(
    var_res['p_died_and_surgery'],
    (0.15 + 0.005 * c(1,2,3)) * 0.34 * 0.05
  )
  expect_equal(
    var_res['p_died_or_survived_and_had_event_given_surgery'],
    rep(1, 3)
  )
  expect_equal(
    var_res['p_died_or_survived_and_event_given_surgery'],
    rep(1, 3)
  )
  expect_equal(
    var_res['p_died_or_survived_and_surgery_given_event'],
    rep(0.34, 3)
  )
  
})

