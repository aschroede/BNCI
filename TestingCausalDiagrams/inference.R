source("utilities.R")
d <- load_data()


# Summary stats
# stat.desc(d)
# summary(d)
# head(d)
# str(d)
# mean(d$Age)
# mean(d$Sex)
# var(d$Age)
# var(d$Sex)



g <- get_dag()
test_for_cycles(g)

debug(get_interventional_dags)
get_interventional_dags("Test5/DAG5.txt", d)

plot(g)
# Extract polychoric correlation matrix
M <- lavCor(d)
print(toString(g, "lavaan"))

# One way to get path coefficients
fit1 <- sem(toString(g, "lavaan"), sample.cov=M, sample.nobs=nrow(d))

# Second way to get path coefficients
# Results in warnings
#Warning messages:
  # 1: lavaan->lav_data_full():  
  # exogenous variable(s) declared as ordered in data: "Education" 
# 2: lavaan->lav_lavaan_step11_estoptim():  
  # Model estimation FAILED! Returning starting values. 
#fit2 <- sem(toString(g, "lavaan"), d)

summary(fit1)
#summary(fit2)
cg <- coordinates(g)
fg <- lavaanToGraph(fit, digits=2)
print(fg)
coordinates(fg) <-cg
# Plot with path coefficients
plot(fg, show.coefficients=TRUE)

# Number of edges don't match up
edges(g)
edges(fg)



# Is the impliedCovarianceMatrix the same as the polychoric correlation matrix?
# Shouldn't be... they are not the same when printed.
# Polychoric correlation matrix used to get path coefficients, which rae used to get implied covariance
# The impliedCovarianceMatrix has the causal effects in the observation regime 

# Here we assume that all residual variances are set to values that generate a variance of 1 for all observed variables (need to scale all data??)
implied_cov_matrix = impliedCovarianceMatrix(fg, standardized = TRUE)
education_on_diabates = implied_cov_matrix["Education", "Diabetes_binary"]
