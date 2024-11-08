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

d <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")
nodes = colnames(d)

# Define the nodes in the network
#nodes <- c("Age", "Diabetes_binary", "HighBP", "HighChol", "AnyHealthcare", 
#           "GenHlth", "MentHlth", "NoDocbcCost", "PhysHlth", "BMI", 
#           "CholCheck", "DiffWalk", "Education", "Income", "PhysActivity", 
#           "Fruits", "HeartDiseaseorAttack", "Stroke", "HvyAlcoholConsump", 
#           "Sex", "Smoker", "Veggies")

# Define the directed edges (arcs) between the nodes as per your network structure
# dag <- empty.graph(nodes)

# Add arcs between the nodes based on your provided structure
arcs(dag) <- matrix(c(
  "Age", "Diabetes_binary",
  "Age", "HighBP",
  "Age", "HighChol",
  "AnyHealthcare", "GenHlth",
  "AnyHealthcare", "MentHlth",
  "AnyHealthcare", "NoDocbcCost",
  "AnyHealthcare", "PhysHlth",
  "BMI", "Diabetes_binary",
  "BMI", "HighBP",
  "BMI", "HighChol",
  "CholCheck", "HighChol",
  "DiffWalk", "PhysHlth",
  "Education", "BMI",
  "Education", "Income",
  "Education", "PhysActivity",
  "Fruits", "BMI",
  "GenHlth", "Diabetes_binary",
  "GenHlth", "PhysActivity",
  "HeartDiseaseorAttack", "Diabetes_binary",
  "HighBP", "Diabetes_binary",
  "HighBP", "HeartDiseaseorAttack",
  "HighBP", "Stroke",
  "HighChol", "Diabetes_binary",
  "HighChol", "HeartDiseaseorAttack",
  "HighChol", "Stroke",
  "HvyAlcoholConsump", "HeartDiseaseorAttack",
  "HvyAlcoholConsump", "Stroke",
  "Income", "AnyHealthcare",
  "Income", "BMI",
  "Income", "NoDocbcCost",
  "Income", "PhysActivity",
  "MentHlth", "GenHlth",
  "NoDocbcCost", "PhysHlth",
  "PhysActivity", "BMI",
  "PhysActivity", "Diabetes_binary",
  "PhysHlth", "GenHlth",
  "Sex", "BMI",
  "Sex", "HeartDiseaseorAttack",
  "Sex", "HighBP",
  "Sex", "HighChol",
  "Smoker", "Diabetes_binary",
  "Smoker", "HeartDiseaseorAttack",
  "Smoker", "HighBP",
  "Smoker", "Stroke",
  "Stroke", "Diabetes_binary",
  "Stroke", "HeartDiseaseorAttack",
  "Veggies", "BMI"
), byrow = TRUE, ncol = 2)

# Plot the Bayesian Network to visualize the structure
plot(dag)

# Check that we have an actual DAG
if (!isAcyclic(dag)) {
  print("The Bayesian network contains cycles.")
} else {
  print("The Bayesian network is acyclic.")
}

# Optionally, check the structure of the Bayesian Network
#print(dag)




# Get conditional independence 
# impliedConditionalIndependencies(dag)
print(colnames(d))
print(nodes(dag))

g <- dagitty('dag {
  Age
  AnyHealthcare
  BMI
  CholCheck
  Diabetes_binary
  DiffWalk
  Education
  Fruits
  GenHlth
  HeartDiseaseorAttack
  HighBP
  HighChol
  HvyAlcoholConsump
  Income
  MentHlth
  NoDocbcCost
  PhysActivity
  PhysHlth
  Sex
  Smoker
  Stroke
  Veggies
  Age -> Diabetes_binary
  Age -> HighBP
  Age -> HighChol
  AnyHealthcare -> GenHlth
  AnyHealthcare -> MentHlth
  AnyHealthcare -> NoDocbcCost
  AnyHealthcare -> PhysHlth
  BMI -> Diabetes_binary
  BMI -> HighBP
  BMI -> HighChol
  CholCheck -> HighChol
  DiffWalk -> PhysHlth
  Education -> BMI
  Education -> Income
  Education -> PhysActivity
  Fruits -> BMI
  GenHlth -> Diabetes_binary
  GenHlth -> PhysActivity
  HeartDiseaseorAttack -> Diabetes_binary
  HighBP -> Diabetes_binary
  HighBP -> HeartDiseaseorAttack
  HighBP -> Stroke
  HighChol -> Diabetes_binary
  HighChol -> HeartDiseaseorAttack
  HighChol -> Stroke
  HvyAlcoholConsump -> HeartDiseaseorAttack
  HvyAlcoholConsump -> Stroke
  Income -> AnyHealthcare
  Income -> BMI
  Income -> NoDocbcCost
  Income -> PhysActivity
  MentHlth -> GenHlth
  NoDocbcCost -> PhysHlth
  PhysActivity -> BMI
  PhysActivity -> Diabetes_binary
  PhysHlth -> GenHlth
  Sex -> BMI
  Sex -> HeartDiseaseorAttack
  Sex -> HighBP
  Sex -> HighChol
  Smoker -> Diabetes_binary
  Smoker -> HeartDiseaseorAttack
  Smoker -> HighBP
  Smoker -> Stroke
  Stroke -> Diabetes_binary
  Stroke -> HeartDiseaseorAttack
  Veggies -> BMI
}')

plot(g)

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
M <- lavCor(d)
polychoric <- localTests(g, sample.cov = M, sample.nobs=nrow(d))
canonical <- localTests(g, d, type='cis.pillai', max.conditioning.variables = 1)
plotLocalTestResults(canonical)
