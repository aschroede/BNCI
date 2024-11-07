# Install the bnlearn package if you don't have it already
if (!require(bnlearn)) {
  install.packages("bnlearn")
  library(bnlearn)
}

if (!require(bnstruct)) {
  install.packages("bnstruct")
  library(bnstruct)
}

data_csv <- read.csv("C:/Users/svenm/Documents/Radboud/Bayesian Networks/BNCI/data/diabetes_binary_health_indicators_BRFSS2015.csv")
st_data_csv <- scale(data_csv)

# Define the nodes in the network
nodes <- c("Age", "Diabetes_binary", "HighBP", "HighChol", "AnyHealthcare", 
           "GenHlth", "MentHlth", "NoDocbcCost", "PhysHlth", "BMI", 
           "CholCheck", "DiffWalk", "Education", "Income", "PhysActivity", 
           "Fruits", "HeartDiseaseorAttack", "Stroke", "HvyAlcoholConsump", 
           "Sex", "Smoker", "Veggies")

# Define the directed edges (arcs) between the nodes as per your network structure
dag <- empty.graph(nodes)

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

# Optionally, check the structure of the Bayesian Network
print(dag)

# Load lavaan package
install.packages("lavaan")
library(lavaan)

# Define the SEM model string based on your DAG structure
sem_model <- '
  # Direct effects specified by arcs in your DAG
  Diabetes_binary ~ Age + BMI + GenHlth + HeartDiseaseorAttack + HighBP + HighChol + PhysActivity + Smoker + Stroke
  HighBP ~ Age + BMI + Sex + Smoker
  HighChol ~ Age + BMI + CholCheck + Sex + Smoker
  GenHlth ~ AnyHealthcare + MentHlth + PhysHlth
  MentHlth ~ AnyHealthcare
  NoDocbcCost ~ AnyHealthcare + Income
  PhysHlth ~ AnyHealthcare + DiffWalk + NoDocbcCost
  BMI ~ Education + Fruits + Income + PhysActivity + Sex + Veggies
  PhysActivity ~ Education + GenHlth + Income
  HeartDiseaseorAttack ~ HighBP + HighChol + Sex + Smoker + Stroke + HvyAlcoholConsump
  Stroke ~ HighBP + HighChol + HvyAlcoholConsump + Smoker
  AnyHealthcare ~ Income
  CholCheck ~ HighChol
'

# Fit the SEM model
fit <- sem(sem_model, data = st_data_csv)
varTable(fit)

# Display the summary of the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)

install.packages("dagitty")
library(dagitty)

g <- lavaanToGraph( fit, digits=4 )
print(g)

imp <- impliedCovarianceMatrix( g )

imp['Diabetes_binary', 'Education']