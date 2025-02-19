source("utilities.R")
d <- load_data()
g <- get_dag()
test_for_cycles(g)

# Visualise Data
visualise_data()

# Test for too few edges
run_independence_tests(g, d, 1, 10, "Test5", 600)

# Test for too many edges
get_superfluous_edges()

