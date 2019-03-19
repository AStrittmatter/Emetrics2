
########################  Load Packages  ########################

# List of required packages
pkgs <- c('psych', 'ggplot2', 'dplyr', 'corrplot', 'gmm', 'sandwich', 'lmtest', 'sem')

# Load packages
for(pkg in pkgs){
    library(pkg, character.only = TRUE)
}

set.seed(1001) # set starting value for random number generator

print('All packages successfully installed and loaded.')

############## Load Data ##############
setwd("C:/Users/user/Dropbox/Emetrics2/Exercise 3")
# Load data frame
df <- read.csv("gotv.csv",header=TRUE, sep=",")

# Outcome
vote02 <- as.matrix(df[,1])

# Called Voter by Phone
call <- as.matrix(df[,2])
# Reached Voter by Phone
contact <- as.matrix(df[,3])

# Covariates
covariates <- as.matrix(df[,c(4:ncol(df))])

print('Data is loaded.')

############## Descriptive Statistics ##############

# Use the describe() function


#####################################################

############## Correlation ##############

# Use the cor() and corrplot() functions


#########################################

############## Univariate OLS estimator ##############

# Use the lm() and coeftest( ,vcov. = vcovHC) functions


######################################################

############## Multivariate OLS estimator ##############


########################################################

############## Probit without Covariates ##############

# Use the glm() funstion with family = binomial(link = "probit")


#######################################################

############## Average Marginal Effects ##############

# The function pnorm() returns the cdf of the normal distribution


######################################################

############## Multivariate Probit Model ##############

## Probit with Covariates


########################################################

############## Multivariate Logit Model ##############

# The function plogis() returns the cdf of the logistic distribution


########################################################

############## First Stage ##############

# Univeriate OLS


# Multivariate OLS


##########################################

############## GMM estimator ##############

# Replicate OLS using the gmm() package

############################################

############## 2SLS Estimator ##############


#############################################


