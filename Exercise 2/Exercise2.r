
########################  Load Packages  ########################

# List of required packages
pkgs <- c('psych', 'ggplot2', 'dplyr', 'sampling')

# Load packages
for(pkg in pkgs){
    library(pkg, character.only = TRUE)
}

print('All packages successfully installed and loaded.')

############## Data Generating Process (DGP) ##############
set.seed(1001)

N <- 200 # sample size

# Generate variables
x0 <- matrix(1, nrow = N, ncol = 1) # intercept (vector of one'S)
x1 <- matrix(rnorm(N), nrow = N, ncol = 1) # standard normal distributed covariate
X <- cbind(x0,x1) # matrix of covariates
u <- matrix(rnorm(N), nrow = N, ncol = 1) # standard normal distributed error term
y <- x1 + u # outcome variable
# the true effect of x1 on y is 1
# the true intercept is 0

dataset <- as.data.frame(cbind(y,x1)) # dataframe will be needed later

print('Data is generated.')

############## Descriptive Statistics ##############

round(describe(dataset), digits=3)

############## Scatter Plot ##############

dataset %>%
 ggplot(aes(x = x1, y = y)) +
 geom_point(colour = "red") 

############## Off-the-Shelf OLS estimator ##############

# data has to be in a dataframe (and not matrix) to use the lm command
lmodel <- lm(y ~ x1, data = dataset)
summary(lmodel)

############## Put your code here ##############

# Apply OLS formula


#################################################

############## Put your code here ##############

# Calulate the error term

# Calculate Sigma-squared with degrees-of-freedom adjustment

# Inverse design matrix

#################################################

############## Put your code here ##############

# Calulate diagonal matrix of squared error (using a loop)


# Variance calulation

#################################################

############## Put your code here ##############
set.seed(1001)
rep = 9999

# Loop with boostrap resamples

#################################################


