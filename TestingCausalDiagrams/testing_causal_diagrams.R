if (!require(dagitty)){
  install.packages("dagitty")
  librarty(dagitty)
}

if (!require(lavaan)){
  install.packages("lavaan")
  library(lavaan)
}

if (!require(bayesianNetworks)){
  install.packages("bayesianNetworks")
  librarty(bayesianNetworks)
}


g <- dagitty('dag {
bb="-9.145,-7.211,8.532,7.668"
Age [pos="-3.330,-3.637"]
AnyHealthcare [pos="4.357,2.087"]
BMI [pos="-1.539,2.944"]
CholCheck [pos="-0.010,2.105"]
Diabetes_binary [pos="-3.422,5.834"]
DiffWalk [pos="4.106,3.718"]
Education [pos="1.125,-6.001"]
Fruits [pos="-2.025,-4.985"]
GenHlth [pos="-2.271,6.471"]
HeartDiseaseorAttack [pos="-5.421,5.869"]
HighBP [pos="-5.097,-1.021"]
HighChol [pos="-2.608,-1.339"]
HvyAlcoholConsump [pos="-7.514,0.498"]
Income [pos="2.962,-3.599"]
MentHlth [pos="2.442,4.215"]
NoDocbcCost [pos="5.886,0.427"]
PhysActivity [pos="1.603,3.085"]
PhysHlth [pos="2.044,6.467"]
Sex [pos="-5.695,-4.862"]
Smoker [pos="-6.469,-1.233"]
Stroke [pos="-6.909,6.862"]
Veggies [pos="-0.361,-4.959"]
Age -> Diabetes_binary
Age -> DiffWalk
Age -> GenHlth
Age -> HighBP
Age -> HighChol
Age -> Income
AnyHealthcare -> CholCheck
AnyHealthcare -> GenHlth
AnyHealthcare -> MentHlth
AnyHealthcare -> PhysActivity
AnyHealthcare -> PhysHlth
BMI -> Diabetes_binary [pos="-2.512,3.968"]
BMI -> HighBP [pos="-4.052,-0.880"]
BMI -> HighChol [pos="-1.086,-2.072"]
Diabetes_binary -> HeartDiseaseorAttack
DiffWalk -> MentHlth
DiffWalk -> PhysHlth
DiffWalk <-> PhysActivity
Education -> BMI
Education -> CholCheck
Education -> Fruits
Education -> Income
Education -> PhysActivity [pos="1.174,0.410"]
Education -> Smoker [pos="-3.594,-6.421"]
Education -> Veggies
Fruits -> BMI
Fruits -> HighBP
Fruits <-> Veggies
GenHlth -> Diabetes_binary
HeartDiseaseorAttack -> Stroke
HighBP -> Diabetes_binary [pos="-4.656,0.436"]
HighBP -> HeartDiseaseorAttack
HighChol -> CholCheck
HighChol -> Diabetes_binary
HighChol -> HeartDiseaseorAttack
HighChol -> Stroke
HvyAlcoholConsump -> HeartDiseaseorAttack
HvyAlcoholConsump -> Stroke
Income -> AnyHealthcare
Income -> BMI [pos="1.657,-3.273"]
Income -> DiffWalk
Income -> NoDocbcCost
Income -> PhysActivity
MentHlth -> GenHlth
MentHlth <-> PhysHlth [pos="2.316,5.082"]
NoDocbcCost -> AnyHealthcare
NoDocbcCost -> PhysHlth [pos="5.693,4.613"]
PhysActivity -> BMI [pos="-0.663,3.200"]
PhysActivity -> Diabetes_binary
PhysActivity -> GenHlth
PhysActivity -> PhysHlth [pos="1.804,4.687"]
PhysHlth -> GenHlth
Sex -> BMI
Sex -> HeartDiseaseorAttack
Sex -> HighBP
Sex -> HighChol
Smoker -> Diabetes_binary
Smoker -> GenHlth [pos="-2.647,2.976"]
Smoker -> HeartDiseaseorAttack
Smoker -> HighBP
Smoker -> Stroke
Veggies -> BMI
Veggies -> HighBP
}
')

# Check that we have an actual DAG
if (!isAcyclic(g)) {
  print("The Bayesian network contains cycles.")
} else {
  print("The Bayesian network is acyclic.")
}

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
d <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# Preprocess the data for polychoric correlations
# For binary variables, assume that it is inferred that 0 
# means first item in variable description on dataset website

# Assuming that all nominal binary data with (0, 1) values is already considered numeric
# and that R does not need to be explicitly told this. If this is an issue in future could do

# data$Diabetes_binary <- factor(data$Diabetes_binary,
#                               levels = c(0, 1),
#                               labels = c("No", "Yes"))

# Ordinal Variables
d$GenHlth <- factor(d$GenHlth, levels = 1:5, ordered = TRUE)
# TODO: might need to reduce number of levels to 12 or lower.
d$Age <- factor(d$Age, levels = 1:13, ordered = TRUE)
d$Education <- factor(d$Education, levels = 1:6, ordered = TRUE)
d$Income <- factor(d$Income, levels = 1:8, ordered = TRUE)

# Scale Numeric Variables to std=1
d$BMI <- scale(d$BMI)
d$MentHlth <- scale(d$MentHlth)
d$PhysHlth <- scale(d$PhysHlth)

# Fix lavaan warning: some ordered categorical variable(s) have more than 12 levels: "Age". 
# Fix by treating age as a continuous variable (assuming ranges are equally spaced)
d$Age <- as.numeric(d$Age)

get_independence_tests <- function(dag, data, max_conditioning_vars, top_n = Inf, folder){
    
  # Generate the polychoric correlation matrix
  polychoric_matrix <- lavCor(data)
  polychoric_tests <- localTests(dag, sample.cov = polychoric_matrix, sample.nobs=nrow(d), max.conditioning.variables = max_conditioning_vars)
  
  # Save results
  if (!dir.exists(folder)){
    dir.create(folder, recursive = TRUE)
  }
  write.csv(polychoric_matrix, file.path(folder, "polychor_cor_matrix.csv"))
  write.csv(polychoric_tests, file.path(folder, "poly_cor_indep_tests.csv"))
  
  # Save image
  png(file=file.path(folder, "plot_results.png"), width=900, height=900)
  plotLocalTestResults(polychoric_tests, n = top_n)
  dev.off()
}

get_independence_tests(g, d, 1, 20, "TestingCausalDiagrams/Test5")

# Generate the polychoric correlation matrix
#polychoric_matrix <- lavCor(d)
#print(polychoric_matrix)
#polychoric_tests <- localTests(g, sample.cov = polychoric_matrix, sample.nobs=nrow(d), max.conditioning.variables = 1)
#plotLocalTestResults(polychoric_tests, n=20)
# canonical <- localTests(g, d, type='cis.pillai', max.conditioning.variables = 1)
