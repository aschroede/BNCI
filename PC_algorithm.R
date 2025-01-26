BiocManager::install("Rgraphviz")
BiocManager::install("RBGL")
library(Rgraphviz)

install.packages("BiocManager")
library(BiocManager)

# Install package for PC algorithm
if (!require(pcalg)){
  install.packages("pcalg")
}
library(pcalg)

# Install bnlearn
if (!require(bnlearn)){
  install.packages("bnlearn")
}
library(bnlearn)
library(igraph)


load_data <- function(){
  # Load data
  d <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")
  
  
  # Ordinal Variables
  d$GenHlth <- factor(d$GenHlth, levels = 1:5, ordered = TRUE)
  d$Age <- factor(d$Age, levels = 1:13, ordered = TRUE)
  d$Education <- factor(d$Education, levels = 1:6, ordered = TRUE)
  d$Income <- factor(d$Income, levels = 1:8, ordered = TRUE)
  
  # Fix lavaan warning: some ordered categorical variable(s) have more than 12 levels: "Age". 
  # Fix by treating age as a continuous variable (assuming ranges are equally spaced)
  d$Age <- factor(d$Age, 
                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
                  labels = c("18-34", "18-34", "18-34", "35-49", "35-49", "35-49", 
                             "50-64", "50-64", "50-64", "65+", "65+", "65+", "65+"), 
                  ordered = TRUE)
  d$Age <- as.numeric(d$Age)
  
  #Scale Numeric Variables to std=1
  d$Age <- scale(d$Age)
  d$BMI <- scale(d$BMI)
  d$MentHlth <- scale(d$MentHlth)
  d$PhysHlth <- scale(d$PhysHlth)
  
  return(d)
}


# Load data with pre-processing
data <- load_data()

# Load data without pre-processing
data <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# Can't train on the whole dataset (takes forever) --> so just take subset of 5000-10000 samples
subset_data <- head(data, 7500)


# White list; relations between items that MUST be in the final DAG (based of domain knowledge)
#white_list <- data.frame(from = c("Age", "Age", "Age", "Age", "Age", 
#                                  "AnyHealthcare",
#                                  "BMI", "BMI", "BMI", 
#                                  "Fruits","Fruits",
#                                  "Veggies",
#                                  "GenHlth", 
#                                  "HeartDiseaseorAttack",
#                                  "HighBP",
#                                  "HighChol",
#                                  "Income", "Income","Income",
#                                  "PhysActivity", "PhysActivity", "PhysActivity", "PhysActivity", "PhysActivity",
#                                  "DiffWalk", "DiffWalk",
#                                  "Smoker"
#                                  ), 
#                         to = c("Diabetes_binary", "DiffWalk", "GenHlth", "HighBP", "HighChol",
#                                "GenHlth",
#                                "Diabetes_binary", "HighBP", "HighChol", 
#                                "BMI","Veggies",
#                                "Fruits",
#                                "Diabetes_binary", 
#                                "Stroke", 
#                                "HeartDiseaseorAttack",
#                                "CholCheck",
#                                "AnyHealthcare", "NoDocbcCost","DiffWalk",
#                                "BMI", "Diabetes_binary", "GenHlth", "PhysHlth","DiffWalk",
#                                "MentHlth", "PhysActivity",
#                                "GenHlth"
#                                ))

# Black list; relation between items that must NOT be in the final DAG (based
# of domain knowledge and don't want to create cycles and preserve causality (so 
# no outputs --> inputs based on white list))
#black_list <- data.frame(from = c(
#                                "Diabetes_binary", "Diabetes_binary", "Diabetes_binary", "Diabetes_binary",
#                                "Stroke",
#                                "GenHlth", "GenHlth",
#                                "HighBP",
#                                "HighChol",
#                                "BMI", "BMI", "BMI",
#                                "PhysHlth", "PhysHlth",
#                                "PhysActivity",
#                                "Income", "Income",
#                                "CholCheck"
#                              ),
#                              to = c(
#                                "Age", "GenHlth", "HighBP", "HighChol",
#                                "HeartDiseaseorAttack",
#                                "Age", "AnyHealthcare",
#                                "Age",
#                                "Age",
#                                "Education", "Age", "PhysActivity",
#                                "Age", "Income",
#                                "Age",
#                                "Age", "Education",
#                                "HighBP"
#                              ))

white_list <- data.frame(from = c("HighBP"),
                         to = c("Diabetes_binary"))

black_list <- data.frame(from = c(
    "AnyHealthcare", "BMI", "CholCheck", "Diabetes_binary", "DiffWalk", "Fruits", 
    "GenHlth", "HeartDiseaseorAttack", "HighBP", "HighChol", "HvyAlcoholConsump", 
    "Income", "MentHlth", "NoDocbcCost", "PhysActivity", "PhysHlth", "Smoker", 
    "Stroke", "Veggies",
    "AnyHealthcare", "BMI", "CholCheck", "Diabetes_binary", "DiffWalk", "Fruits", 
    "GenHlth", "HeartDiseaseorAttack", "HighBP", "HighChol", "HvyAlcoholConsump", 
    "Income", "MentHlth", "NoDocbcCost", "PhysActivity", "PhysHlth", "Smoker", 
    "Stroke", "Veggies"),
  to = c(
    rep("Age", 19),
    rep("Sex", 19)
  ))

# Run PC algorithm
# DAG from the PC algorithm without lists
pc_model_without_lists <- pc.stable(subset_data, alpha = 0.01)
print(pc_model_without_lists)
graph <- graphviz.plot(pc_model_without_lists, layout = "dot")
plot(graph)
arcs <- arcs(pc_model_without_lists)
dagitty_format_without_lists <- paste("dag {", 
                        paste0(arcs[, 1], " -> ", arcs[, 2], collapse = "; "),
                        "}")
print(dagitty_format_without_lists)
adj_matrix <- as(graph, "matrix")
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
is_acyclic(g)

# DAG from the PC algorithm with lists
pc_model_with_lists <- pc.stable(subset_data, black_list = black_list, white_list = white_list, alpha = 0.01)
graph <- graphviz.plot(pc_model_with_lists, layout = "dot")
plot(graph)
arcs <- arcs(pc_model_with_lists)
dagitty_format_with_lists <- paste("dag {", 
                                      paste0(arcs[, 1], " -> ", arcs[, 2], collapse = "; "),
                                      "}")
print(dagitty_format_with_lists)
adj_matrix <- as(graph, "matrix")
g <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
is_acyclic(g)














