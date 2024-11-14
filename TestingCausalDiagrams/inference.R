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


# Fit and 
summary(fit1)
#summary(fit2)
cg <- coordinates(g)
fg <- lavaanToGraph(fit, digits=2)
# Save fitted dag with beta coefficients
save_to_txt(fg, "FittedDAG.txt")

# Plot with path coefficients
print(fg)
coordinates(fg) <-cg
plot(fg, show.coefficients=TRUE)

# Number of edges don't match up
edges(g)
edges(fg)

# Causl Effect analysis with do operator
#debug(get_interventional_dags)
get_interventional_dags("FittedDAG.txt", d)



# Is the impliedCovarianceMatrix the same as the polychoric correlation matrix?
# Shouldn't be... they are not the same when printed.
# Polychoric correlation matrix used to get path coefficients, which rae used to get implied covariance
# The impliedCovarianceMatrix has the causal effects in the observation regime 

# Here we assume that all residual variances are set to values that generate a variance of 1 for all observed variables (need to scale all data??)
debug(get_causal_effects)
causal_effects = get_causal_effects("doDags", "Diabetes_binary")



