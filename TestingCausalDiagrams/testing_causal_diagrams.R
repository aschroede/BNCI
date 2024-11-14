
#plot(g)

# Takes forever to run
# localTests(g, d, type="cis.chisq")

# Model testing using polychoric correlations
# TODO: Do we need to specify the type of each variable and the order
# Like in R-companion? Depending on the type of tests (polychoric, polyserial, canonical )
# we may or may not need to specify order of variables and types. Canonical works with everything
# but is less interpretable

# The difficult data is stuff like job title, categorical. We don't have any of that kind of data in
# our sets

# Limit the data conditional sets to smaller ones, focus on the smaller ones first, fix those problems
# Then condition on 2 or 3 variables later
#


# Load the data

# Preprocess the data for polychoric correlations
# For binary variables, assume that it is inferred that 0 
# means first item in variable description on dataset website

# Assuming that all nominal binary data with (0, 1) values is already considered numeric
# and that R does not need to be explicitly told this. If this is an issue in future could do

# data$Diabetes_binary <- factor(data$Diabetes_binary,
#                               levels = c(0, 1),
#                               labels = c("No", "Yes"))


source("utilities.R")
d <- load_data()
g <- get_dag()
test_for_cycles(g)

run_independence_tests(g, d, 1, 20, "Test6")


# Generate the polychoric correlation matrix
#polychoric_matrix <- lavCor(d)
#print(polychoric_matrix)
#polychoric_tests <- localTests(g, sample.cov = polychoric_matrix, sample.nobs=nrow(d), max.conditioning.variables = 1)
#plotLocalTestResults(polychoric_tests, n=20)
# canonical <- localTests(g, d, type='cis.pillai', max.conditioning.variables = 1)
