install.packages("MatchIt")
install.packages("BayesianNetwork")
install.packages("lmtest")
install.packages("sandwich")
install.packages("forestplot")
install.packages("stddiff")
install.packages("cobalt")

#library(dplyr)
library(MatchIt)
library(lmtest)
library(sandwich)
library(forestplot)
library(stddiff)
library(cobalt)
library(dagitty)


source("C:/Users/svenm/Documents/Radboud/Bayesian Networks/BNCI/src/utilities.R")

data <- load_data()
cls <- c("Diabetes_binary", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")

# PART 1: Perform regression analysis on the dataset to look for the effect of HighBP on Diabetes_binary
summary( glm ( Diabetes_binary ~ HighBP, data, family="binomial" ) )

#test for covariate imbalance between the treatment and the control arm
t.test( Diabetes_binary ~ HighBP, data )

#assess covariate imbalance more systematically and plot it using a forest plot
base_imb <- as.data.frame(stddiff.numeric(data, vcol=cls, gcol="HighBP"))

forestplot(
  labeltext = cls,
  mean=base_imb$stddiff,
  lower=base_imb$stddiff.l,
  upper=base_imb$stddiff.u
)


# PART 2: Computing and using of propensity scores to improve the covariate balance in the dataset and reperform regression analysis on the new dataset to look for the effect of HighBP on Diabetes_binary
dag <- get_dag()
covariates <- dagitty::parents(dag, "HighBP")

# Not sure which variables to include as covariates, currently only including parents because these do directly influence the outcome (diabetes)
mdl <- glm( HighBP ~ Age + BMI + Fruits + Sex + Smoker + Veggies, # + Diabetes_binary + HeartDiseaseorAttack,
            data, family="binomial"
 )
summary(mdl)

#obtain propensity scores (probabilities to receive the treatment given the covariates) and visualize the distribution of the propensity scores
pscore_logit <- predict( mdl )
pscore <- exp(pscore_logit) / (1+exp(pscore_logit))
boxplot( pscore ~ data$HighBP )

#use the propensity scores to estimate our treatment effect
wgt <- 1/pscore
wgt[!data$HighBP] <- 1/(1-pscore[!data$HighBP])
summary( lm( Diabetes_binary ~ HighBP, data, weights=pscore ) )

# TODO: Test multiple caliper values
#match on the propensity scores
caliper_value <- sd(pscore) * 0.02
mm <- matchit( data$HighBP ~ pscore, caliper=caliper_value )$match.matrix

#remove redundant treatments from dataset
mm <- mm[!is.na(mm),,drop=FALSE]
trt <- as.integer(rownames(mm))
ctrl <- as.integer(mm[,1])
dmtch <- data[c(trt,ctrl),]
#perform regression analysis on the new dataset to look for the effect of HighBP on Diabetes_binary
summary( glm( Diabetes_binary ~ HighBP, dmtch, family="binomial") )

#test for covariate imbalance between the treatment and the control arm
t.test( Diabetes_binary ~ HighBP, dmtch )

#assess covariate imbalance more systematically and plot it using a forest plot
match_imb <- as.data.frame(stddiff.numeric(dmtch, vcol=cls, gcol="HighBP"))
forestplot(
  labeltext = cls,
  mean=match_imb$stddiff,
  lower=match_imb$stddiff.l,
  upper=match_imb$stddiff.u )



# PART 3: This still needs to be done but will be the same as part, the only difference is that we will be using different covariates when fitting the logistic regression model

