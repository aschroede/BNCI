# Bring in the utilities file to access all functions
source("utilities.R")

# Load dataset with preprocessing
d <- load_data()
# get the dag from utilities
g <- get_dag()
# test for cycles
test_for_cycles(g)


# Extract polychoric correlation matrix using lavaan
M <- lavCor(d)

# Use the data, polychoric correlation matrix, and the dag to fit our model
fit <- sem(toString(g, "lavaan"), sample.cov=M, sample.nobs=nrow(d))

# Test for too many edges with fitted path coeficients
# Only use during testing of network structure!
#get_superfluous_edges(fit, 0.01, "superfluous_edges.csv")

# Convert the fitted sem model in lavaan to a daggity dag
fg <- lavaanToGraph(fit, digits=2)

# Save fitted dag with path coefficients
save_to_txt(fg, "FittedDAG.txt")

# Extract coordinates from daggity dag
cg <- coordinates(g)
# Apply coordinates to fitted daggity dag
coordinates(fg) <-cg
# Draw resutling daggity dag wtih coefficients
plot(fg, show.coefficients=TRUE)

# Number of edges don't match up
edges(g)
edges(fg)

# Generate all the modified dags the result from the do operator
get_interventional_dags("FittedDAG.txt", d)

# Using the generated graphs, apply do operator with target variable
causal_effects = get_causal_effects("doDags", "Diabetes_binary")
# Write results to file
write.csv(causal_effects, "causal_effects.csv")




