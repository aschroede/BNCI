if (!require(Dict)){
  install.packages("Dict")
  library(Dict)
}

if (!require(pastecs)){
  install.packages("pastecs")
  library(pastecs)
}

if (!require(dagitty)){
  install.packages("dagitty")
  library(dagitty)
}

if (!require(lavaan)){
  install.packages("lavaan")
  library(lavaan)
}

if (!require(bayesianNetworks)){
  install.packages("bayesianNetworks")
  library(bayesianNetworks)
}


save_to_txt <- function(object, filename){
  writeLines(object, filename)
  message("Created file: ", filename)
}

get_interventional_dags <- function(dag_filepath, d, output_folder="doDags"){
  
  # Save results
  if (!dir.exists(output_folder)){
    dir.create(output_folder, recursive = TRUE)
  }
  
  variables = colnames(d)
  dag_lines <- readLines(dag_filepath)
  
  for (variable in variables){
    modified_dag <- dag_lines
    
    pattern <- paste0("-> ", variable)
    modified_dag <- modified_dag[!grepl(pattern, modified_dag)]
    
    output_file <- paste0("do_", variable, ".txt")
    writeLines(modified_dag, file.path(output_folder, output_file))
    
    message("Created file: ", output_file)
  }
}


get_causal_effects <- function(doDAG_path, target_var){
  
  causal_effects <- list()
  
  # Get all doDAGs
  files <- list.files(path=doDAG_path, pattern="*.txt", full.names=FALSE, recursive=FALSE)
  
  for (filename in files){
    
    #doDAG_string <- readChar(filename, file.info(filename)$size)
    
    filepath = file.path(doDAG_path, filename)
    doDAG_string <- readChar(filepath, file.info(filepath)$size)
    doDAG <- dagitty(doDAG_string)
    
    variable <- sub("do_(.*)\\.txt", "\\1", filename)
    
    # extract variable name from file name
    implied_cov_matrix = impliedCovarianceMatrix(doDAG, standardized = TRUE)
    implied_cov = implied_cov_matrix[variable, target_var]
    causal_effects[[variable]] <- implied_cov
  }
  
  return (causal_effects)
}



load_data <- function(){
  # Load data
  d <- read.csv("../data/diabetes_binary_health_indicators_BRFSS2015.csv")
  
  # Ordinal Variables
  d$GenHlth <- factor(d$GenHlth, levels = 1:5, ordered = TRUE)
  #d$Age <- factor(d$Age, levels = 1:13, ordered = TRUE)
  d$Education <- factor(d$Education, levels = 1:6, ordered = TRUE)
  d$Income <- factor(d$Income, levels = 1:8, ordered = TRUE)
  
  
  # Fix lavaan warning: some ordered categorical variable(s) have more than 12 levels: "Age". 
  # Fix by treating age as a continuous variable (assuming ranges are equally spaced)
  d$Age <- as.numeric(d$Age)
  
  # This scales and centers numeric data to have mean 0 and variance 1
  #d <- scale(d)
  
  #Scale Numeric Variables to std=1
  d$Age <- scale(d$Age)
  d$BMI <- scale(d$BMI)
  d$MentHlth <- scale(d$MentHlth)
  d$PhysHlth <- scale(d$PhysHlth)
  return(d)
}



run_independence_tests <- function(dag, data, max_conditioning_vars, top_n = Inf, folder){
  
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

test_for_cycles <- function(g){
  # Check that we have an actual DAG
  if (!isAcyclic(g)) {
    print("The Bayesian network contains cycles.")
  } else {
    print("The Bayesian network is acyclic.")
  }
}

get_dag <- function(){
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
}')
  return(g)
}