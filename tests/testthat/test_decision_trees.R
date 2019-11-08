context("Decision Trees")
library(tibble)

model <- system.file("models","checkimab", package="heRomod2") %>%
  read_model() %>%
  heRomod2:::parse_model()
segment <- tibble(group = "male_age_lt_35", strategy = "chemo")
cycles <- rep(seq_len(360), 12)
rows <- length(cycles)
ns <- heRomod2:::create_namespace(
  model,
  segment
)

test_that("Calculate conditional & unconditional probabilities", {
  
  # Make sure trees table is populated in namespace
  ns$env$.trees <- tribble(
    ~name, ~node,~tags, ~parent, ~formula,
    'tree', 'had_event', 'event', NA, '0.15 + 0.005 * cycle',
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
    'p_survived', 'p(-died, tree)',
    'p_surgery', 'p(surgery, tree)',
    'p_died_given_event', 'p(died | event, tree)',
    'p_event_given_died', 'p(event | died, tree)',
    'p_died_or_surgery', 'p(died %or% surgery, tree)',
    'p_died_or_not_surgery', 'p(died %or% -surgery, tree)',
    'p_died_and_surgery', 'p(died %and% surgery, tree)',
    'p_died_or_survived_and_had_event_given_surgery', 'p(((died %or% survived) %and% had_event) | surgery, tree)',
    'p_died_or_survived_and_event_given_surgery', 'p(((died %or% survived) %and% event) | surgery, tree)',
    'p_died_or_survived_and_surgery_given_event', 'p(((died %or% survived) %and% surgery) | event, tree)'
  )
  vars$strategy <- segment$strategy
  vars$group <- segment$group
  vars$display_name <- ''
  vars$description <- ''
  var_list <- heRomod2:::parse_variables(
      vars,
      segment,
      trees = ns$env$.trees
    )
  
  # Evaluate
  var_res <- heRomod2:::eval_variables(var_list, ns)
  plot_decision_tree(var_res['tree'])
  
  # Check results
  expect_equal(var_res['p_event'], 0.15 + 0.005 * cycles)
  expect_equal(
    var_res['p_died'],
    (0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01
  )
  expect_equal(
    1 - var_res['p_died'],
    var_res['p_survived']
  )
  expect_equal(var_res['p_died_given_event'], rep((0.34 * 0.05) + ((1 - 0.34) * 0.15), rows))
  expect_equal(
    var_res['p_event_given_died'],
    ((0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15))) / ((0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01)
  )
  expect_equal(
    var_res['p_died_or_surgery'],
    (0.15 + 0.005 * cycles) * (0.34 + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01
  )
  expect_equal(
    var_res['p_died_or_not_surgery'],
    1 - var_res['p_surgery'] + var_res['p_died_and_surgery']
  )
  expect_equal(
    var_res['p_died_and_surgery'],
    (0.15 + 0.005 * cycles) * 0.34 * 0.05
  )
  expect_equal(
    var_res['p_died_or_survived_and_had_event_given_surgery'],
    rep(1, rows)
  )
  expect_equal(
    var_res['p_died_or_survived_and_event_given_surgery'],
    rep(1, rows)
  )
  expect_equal(
    var_res['p_died_or_survived_and_surgery_given_event'],
    rep(0.34, rows)
  )
  
})

