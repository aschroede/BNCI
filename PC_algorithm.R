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



# Load data
data <- read.csv("/Users/daankersten/Desktop/Study/Bayesian/BNCI/data/diabetes_binary_health_indicators_BRFSS2015.csv")
subset_data <- head(data, 5000)


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
pc_model_without_lists <- pc.stable(subset_data, alpha = 0.01)
pc_model_with_lists <- pc.stable(subset_data, black_list = black_list, white_list = white_list, alpha = 0.01)
print(pc_model)

# Visualize the DAG from the PC algorithm without lists
graph <- graphviz.plot(pc_model_without_lists, layout = "dot")
plot(graph)
arcs <- arcs(pc_model_without_lists)
dagitty_format_without_lists <- paste("dag {", 
                        paste0(arcs[, 1], " -> ", arcs[, 2], collapse = "; "),
                        "}")
print(dagitty_format_without_lists)

# Visualize the DAG from the PC algorithm with lists
graph <- graphviz.plot(pc_model_with_lists, layout = "dot")
plot(graph)
arcs <- arcs(pc_model_with_lists)
dagitty_format_with_lists <- paste("dag {", 
                                      paste0(arcs[, 1], " -> ", arcs[, 2], collapse = "; "),
                                      "}")
print(dagitty_format_with_lists)















