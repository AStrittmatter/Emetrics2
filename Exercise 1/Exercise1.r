
############## Define Input Factors ##############

N1 <- 20 # sample size
rep <- 2000 # replications

print('Input factors defined.')

############## Data Generating Process (DGP) ##############
set.seed(1001)

# Generate matrix of integers
x1 <- matrix(floor(runif(rep*N1 ,min = 0.25, max = 1.25)), nrow = N1, ncol = rep)
# Rows correspond to different observations
# Columns corresponds to different samples (replications of the DGP)
# Example: x could be a dummy for females

print('Data is generated.')

############## Estimation of Sample Mean ##############

# Generate vector to store the results
shares <- matrix(NA, nrow = rep, ncol = 10)

# Make a loop
for (i in c(1:rep)) {
   shares[i,1] <- mean(x1[,i]) # calculate mean for each replication
}

print('Means are estimated')

############## Visualization of Results ##############

# Histogram
hist(shares[,1])
abline(v=.25,col="red")

############## Performance Measures ##############







##################################################


