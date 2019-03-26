
########################  Load Packages  ########################

# List of required packages
pkgs <- c('fBasics', 'corrplot', 'psych', 'rpart',
          'rpart.plot', 'treeClust', 'rlang', 'readr', 'devtools',
          'tidyverse', 'reshape2', 'caret', 'plotmo',
          'RandomFieldsUtils', 'rms')


# Load packages
for(pkg in pkgs){
    library(pkg, character.only = TRUE)
}

print('All packages successfully installed and loaded.')

########################  Load Data Frame  ########################

set.seed(100239) # set starting value for random number generator

# Load data frame
data_raw <- read.csv("Data/used_cars.csv",header=TRUE, sep=",")

# Outcome Variables
outcomes <- c("first_price", "final_price", "overprice")

# Covariates/Features
baseline_covariates_bin <- c("bmw_320", "opel_astra", "mercedes_c", "vw_golf", "vw_passat", 
                              "diesel", "private_seller", "guarantee", "maintenance_cert",  "pm_green") # binary
baseline_covariates_cont <- c("mileage", "age_car_years", "other_car_owner", "inspection",
                              "co2_em", "euro_norm") # continuous/ordered discrete
baseline_covariates <- c(baseline_covariates_cont,baseline_covariates_bin)
all_variables <- c(outcomes, baseline_covariates)

# Selection of Subsample size, max. 104,719 observations
# Select smaller subsample to decrease computation time
n_obs <- 10000
df <- data_raw %>%
  dplyr::sample_n(n_obs) %>%
  dplyr::select(all_variables)

print('Data frame successfully loaded and sample selected.')

########################  Table with Descriptive Statistics  ########################

desc <- fBasics::basicStats(df) %>% t() %>% as.data.frame() %>% 
  select(Mean, Stdev, Minimum, Maximum, nobs)
print(round(desc, digits=1))

########################  Correlation Matrix  ########################

corr = cor(df)
corrplot(corr, type = "upper", tl.col = "black")

########################  Extract Dataset  ########################

# Extracting continuous variables
baseline_covariates_cont <- df %>%
  dplyr::select(baseline_covariates_cont) 

# Extracting indicator variables
baseline_covariates_bin <- df %>%
  dplyr::select(baseline_covariates_bin)

# Extracting outcome 
outcomes <- df %>% dplyr::select(outcomes)

# Setting up the data, renaming columns and discarding rows with NA (if any)
df <- bind_cols(outcomes, baseline_covariates_cont, baseline_covariates_bin) %>%
  na.omit()

print('Data successfully extracted.')

########################  Take Hold-Out-Sample  ########################

df_part <- modelr::resample_partition(df, c(obs = 0.8, hold_out = 0.2))
df_obs <- as.data.frame(df_part$obs) # Training and estimation sample
df_hold_out <- as.data.frame(df_part$hold_out) # Hold-out-sample

# Outcomes
first_price_obs <- as.matrix(df_obs[,1])
final_price_obs <- as.matrix(df_obs[,2])
overprice_obs <- as.matrix(df_obs[,3])

first_price_hold_out <- as.matrix(df_hold_out[,1])
final_price_hold_out <- as.matrix(df_hold_out[,2])
overprice_hold_out <- as.matrix(df_hold_out[,3])

## Covariates/Features
baseline_covariates_cont_obs <- as.matrix(df_obs[,c(4:9)])
baseline_covariates_bin_obs <- as.matrix(df_obs[,c(10:19)])
baseline_covariates_hold_cont_out <- as.matrix(df_hold_out[,c(4:9)])
baseline_covariates_hold_bin_out <- as.matrix(df_hold_out[,c(10:19)])

# Standardise continuous covariates
preProcValues <- preProcess(baseline_covariates_cont_obs, method = c("center", "scale")) # Take means and standard deviations from training sample
ObsTransformed <- predict(preProcValues, baseline_covariates_cont_obs) # Apply the transformation to trainings sample
HoldOutTransformed <- predict(preProcValues, baseline_covariates_hold_cont_out) # Apply the transformation to hold-out-sample (based on means and standard deviations from training sample)
# Note: Outcome variables are not rescaled

baseline_covariates_obs <- as.matrix(cbind(ObsTransformed,baseline_covariates_bin_obs)) 
baseline_covariates_hold_out <- as.matrix(cbind(HoldOutTransformed,baseline_covariates_hold_bin_out)) 
                  
print('The data is now ready for your first analysis!')

########################  Build Trees with different Leave Sizes  ########################                         

# Prepare data for tree estimator
tree_data_obs <-  data.frame(final_price_obs, baseline_covariates_obs)

# Setup the formula of the linear regression model
sumx <- paste(baseline_covariates, collapse = " + ")  
linear <- paste("final_price_obs",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build the tree
linear.singletree_1 <- rpart(formula = linear, data = tree_data_obs , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=500))
# Note: 'minbucket=500' imposes the restriction that each terminal leave should contain at least 500 used cars. Algorithm 'rpart' stops growing trees when either one leave has less than 500 observations or the MSE gain of addidng one addidtional leave is below cp=0.00002.

print('Relative CV-MSE for different tree sizes')
print(linear.singletree_1$cptable)

# Plot CV-MSE
plotcp(linear.singletree_1)

# Save CV-MSE as png-file
png(filename= "cp_tree1.png", units="in", width=5, height=4, pointsize=12, res=72)
    plotcp(linear.singletree_1)
dev.off()

########################  Select the Tree that Minimises CV-MSE  ######################## 

op.index_1 <- which.min(linear.singletree_1$cptable[, "xerror"])
print(paste0("Optimal number final leaves: ", op.index_1))

# Get cp-value that corresponds to optimal tree size
cp.vals_1 <- linear.singletree_1$cptable[op.index_1, "CP"]

########################  Select the Optimal Tree and Assess Out-of-Sample Performance  ######################## 

# Prune the tree
treepruned.linearsingle_1 <- prune(linear.singletree_1, cp = cp.vals_1)

extr_covariates_obs <- as.data.frame(baseline_covariates_obs)
extr_covariates_hold_out <- as.data.frame(baseline_covariates_hold_out)

# Predict final price in the observed and hold-out-samples
pred_tree_hold_out_1 <- as.matrix(predict(treepruned.linearsingle_1, newdata=extr_covariates_hold_out))
pred_tree_obs_1 <- as.matrix(predict(treepruned.linearsingle_1, newdata=extr_covariates_obs))

## Assess performance of tree estimator
# In-sample RMSE
rmse_obs_1 <- round(sqrt(mean((final_price_obs - pred_tree_obs_1)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_1 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_1)^2)),digits=3)
# In-sample R-squared
r2_obs_1 <- round(1-mean((final_price_obs - pred_tree_obs_1)^2)/mean((final_price_obs - mean(final_price_obs))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_1 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_1)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("In-Sample RMSE: ", rmse_obs_1))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_1))
print(paste0("In-Sample R-squared: ", r2_obs_1))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_1))

########################  Visulatisation of tree  ######################## 

## Plot tree structure
rpart.plot(treepruned.linearsingle_1,digits=3)
# Note: All continuous variables are standardised.

# Save tree structure as png-file
png(filename= "full_tree1.png",units="in", width=9, height=9, pointsize=12,res=72)
    rpart.plot(treepruned.linearsingle_1,digits=3)
dev.off()

######################## Average Value of Each Covariate by Leaf  ######################## 
## Code from Susan Athey and Guido Imbens AEA lecture

# Take hold-out data only
tree_data_out <- data.frame(pred_tree_hold_out_1, baseline_covariates_hold_out)

# Map to each individual row the leaf number and add the covariates
individual_leaf <- treeClust::rpart.predict.leaves(treepruned.linearsingle_1, tree_data_out)  %>% 
  as_tibble()  %>% 
  dplyr::rename(leaf=value) 
leaf_covariates <- cbind(individual_leaf, tree_data_out[baseline_covariates])

# Get predicted final price of each leaf 
leaf_price <- treepruned.linearsingle_1$frame %>% as_tibble() %>%
  dplyr::mutate(row = 1:nrow(.)) %>% 
  dplyr::filter(var == "<leaf>") %>% 
  dplyr::rename(leaf=row, pred_price=yval) %>% 
  dplyr::select(leaf, pred_price) 

# Merge all the information on leaf level
leaf_data <- left_join(leaf_covariates, leaf_price, by="leaf")

# Mean of each covariate on each leaf, 
# Leafs sorted and renumbered by predicted prive
leaf_mean <- leaf_data %>% 
  dplyr::group_by(leaf) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::arrange(desc(pred_price)) %>%
  dplyr::mutate(leaf = 1:nrow(.)) 

# Plot
plt <- leaf_mean %>% 
  dplyr::select(leaf, baseline_covariates[c(16:1)]) %>%
  melt(id="leaf") %>%
  ggplot(aes(x=factor(leaf), y=variable, fill=value)) +
  geom_raster() +
  scale_fill_gradient2() + 
  scale_x_discrete(breaks=seq_along(leaf_mean$pred_price),      
                   labels=round(leaf_mean$pred_price, 1)) +
  # From here on, all the code is optional styling
  geom_tile(colour="white",size=0.25) +            # white cell border
  labs(x="Predicted final price",
       y="", title="Average covariate value by leaf") +# axis labels 
  coord_fixed()+                                   # square cells
  theme_grey(base_size=8)+                         # basic hue 
  theme(
    axis.text=element_text(face="bold"),      # axis font style
    plot.background=element_blank(),          # cleaner background
    panel.border=element_blank(),             # cleaner panel
    legend.key.width=grid::unit(0.2,"cm"),    # slim legend color bar
    axis.ticks=element_line(size=0.4),        # tick style
    axis.text.x=element_text(size=7,          # tick label style
                             colour="grey40",
                             angle = 60,
                             hjust = 1),
    plot.title=element_text(colour="grey40",  # plot title style
                            hjust=.5,size=7,
                            face="bold")
  )

plot(plt)

# Save average covariate values as png-file
png(filename= "regressor_values.png",units="in", width=6, height=6, pointsize=38,res=300)
    plt
dev.off()

######################## Deep tree estimator  ########################                         

# Build deep Tree
linear.singletree_2 <- rpart(formula = linear, data = tree_data_obs , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=5))

# Find tree size that minimises CV-MSE
op.index_2 <- which.min(linear.singletree_2$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_2)
abline(v = op.index_2, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_2 <- linear.singletree_2$cptable[op.index_2, "CP"]

# Prune the tree
treepruned.linearsingle_2 <- prune(linear.singletree_2, cp = cp.vals_2)

# Plot tree structure
rpart.plot(treepruned.linearsingle_2,digits=3)
treepruned.linearsingle_2_short <- prune(linear.singletree_2, cp = 150*cp.vals_2)
rpart.plot(treepruned.linearsingle_2_short,digits=3, main = "First few leaves",fallen.leaves=FALSE)


# Predict final price in the observed and hold-out-samples
pred_tree_hold_out_2 <- as.matrix(predict(treepruned.linearsingle_2, newdata=extr_covariates_hold_out))
pred_tree_obs_2 <- as.matrix(predict(treepruned.linearsingle_2, newdata=extr_covariates_obs))

## Assess performance of tree estimator
# In-sample RMSE
rmse_obs_2 <- round(sqrt(mean((final_price_obs - pred_tree_obs_2)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_2 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_2)^2)),digits=3)
# In-sample R-squared
r2_obs_2 <- round(1-mean((final_price_obs - pred_tree_obs_2)^2)/mean((final_price_obs - mean(final_price_obs))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_2 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_2)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("In-Sample RMSE: ", rmse_obs_2))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_2))
print(paste0("In-Sample R-squared: ", r2_obs_2))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_2))

######################## Honest deep tree estimator ########################                        

# Create tratining and estimation sample
df_obs_part <- modelr::resample_partition(df_obs, c(train = 0.5, est = 0.5))
df_train <- as.data.frame(df_obs_part$train)
df_est <- as.data.frame(df_obs_part$est)

# Outcomes
final_price_train <- as.matrix(df_train[,2])
final_price_est <- as.matrix(df_est[,2])

# Covariates/Features
baseline_covariates_hold_cont_out <- as.matrix(df_hold_out[,c(4:9)])
baseline_covariates_cont_train <- as.matrix(df_train[,c(4:9)]) 
baseline_covariates_cont_est <- as.matrix(df_est[,c(4:9)])
baseline_covariates_hold_bin_out <- as.matrix(df_hold_out[,c(10:19)])
baseline_covariates_bin_train <- as.matrix(df_train[,c(10:19)]) 
baseline_covariates_bin_est <- as.matrix(df_est[,c(10:19)])

baseline_covariates_hold_out <- as.matrix(cbind(baseline_covariates_hold_cont_out,baseline_covariates_hold_bin_out)) 
baseline_covariates_train <- as.matrix(cbind(baseline_covariates_cont_train,baseline_covariates_bin_train)) 
baseline_covariates_est <- as.matrix(cbind(baseline_covariates_cont_est,baseline_covariates_bin_est)) 

# Prepare data for tree estimator
tree_data_train <-  data.frame(final_price_train, baseline_covariates_train)
tree_data_est <-  data.frame(final_price_est, baseline_covariates_est)
tree_data_hold_out <-  data.frame( baseline_covariates_hold_out)

# Setup the formula of the linear regression model
linear <- paste("final_price_train",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build deep tree
linear.singletree_3 <- rpart(formula = linear, data = tree_data_train , method = "anova", xval = 10,
                             control = rpart.control(cp = 0.00002, minbucket=5), model = TRUE)

# Find tree size that minimises CV-MSE
op.index_3 <- which.min(linear.singletree_3$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_3)
abline(v = op.index_3, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_3 <- linear.singletree_3$cptable[op.index_3, "CP"]

# Prune the tree
treepruned.linearsingle_3 <- prune(linear.singletree_3, cp = cp.vals_3)

# Extrapolate Tree to Estimation Sample
extr_covariates_train <-  as.data.frame(baseline_covariates_train)
extr_covariates_est <-  as.data.frame(baseline_covariates_est)
extr_covariates_hold_out <-  as.data.frame(baseline_covariates_hold_out)

leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_3, newdata= extr_covariates_train, type = "where"))
dummies_train = model.matrix(~leaves )

leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_3, newdata= extr_covariates_est, type = "where"))
dummies_est = model.matrix(~leaves )

leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_3, newdata= extr_covariates_hold_out, type = "where"))
dummies_out = model.matrix(~leaves )

nam_train <- as.vector(colnames(dummies_train))
nam_est <- as.vector(colnames(dummies_est))
nam_out <- as.vector(colnames(dummies_out))

for (i in c(1:ncol(dummies_train))) {
    if (nam_train[i] != nam_est[i]) {
        s = i -1
        dummies_est <- as.matrix(cbind(dummies_est[,c(1:s)],rep(0,times=nrow(dummies_est)), 
                                       dummies_est[,c(i:ncol(dummies_est))]))
        nam_est <- as.vector(c(nam_est[c(1:s)],"NA", nam_est[c(i:length(nam_est))]))
    }
    
}

for (i in c(1:ncol(dummies_train))) {
    if (nam_train[i] != nam_out[i]) {
        s = i -1
        dummies_out <- as.matrix(cbind(dummies_out[,c(1:s)],rep(0,times=nrow(dummies_out)), 
                                       dummies_out[,c(i:ncol(dummies_out))]))
        nam_out <- as.vector(c(nam_out[c(1:s)],"NA", nam_out[c(i:length(nam_out))]))
    }
    
}


res <- lm.fit(dummies_est,final_price_est)
coef <- as.matrix(res$coefficients)
coef[is.na(coef)] <- 0

# Prediction
pred_tree_est_3 <- as.matrix(res$fit)
pred_tree_hold_out_3  <- as.matrix(dummies_out) %*% as.matrix(coef)
pred_tree_train_3 <- as.matrix(dummies_train) %*% as.matrix(coef)

## Assess performance of tree estimator
# Training-sample RMSE
rmse_train_3 <- round(sqrt(mean((final_price_train - pred_tree_train_3)^2)),digits=3)
# Estimation-sample RMSE
rmse_est_3 <- round(sqrt(mean((final_price_est - pred_tree_est_3)^2)),digits=3)
# Hold-out-sample RMSE
rmse_hold_out_3 <- round(sqrt(mean((final_price_hold_out - pred_tree_hold_out_3)^2)),digits=3)
# Training-sample R-squared
r2_train_3 <- round(1-mean((final_price_train - pred_tree_train_3)^2)/mean((final_price_train - mean(final_price_train))^2),digits=3)
# Estimation-sample R-squared
r2_est_3 <- round(1-mean((final_price_est - pred_tree_est_3)^2)/mean((final_price_est - mean(final_price_est))^2),digits=3)
# Hold-out-sample R-squared
r2_hold_out_3 <- round(1-mean((final_price_hold_out - pred_tree_hold_out_3)^2)/mean((final_price_hold_out - mean(final_price_hold_out))^2),digits=3)

print(paste0("Training-Sample RMSE: ", rmse_train_3))
print(paste0("Estimation-Sample RMSE: ", rmse_est_3))
print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_3))
print(paste0("Training-Sample R-squared: ", r2_train_3))
print(paste0("Estimation-Sample R-squared: ", r2_est_3))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_3))


########################  Crossfitted honest deep tree estimator  ########################                        

# Setup the formula of the linear regression model for the first tree
linear <- paste("final_price_est",paste(sumx, sep=" + "), sep=" ~ ")
linear <- as.formula(linear)

# Build first deep tree
linear.singletree_4 <- rpart(formula = linear, data = tree_data_est , method = "anova", xval = 10,
                             y = TRUE, control = rpart.control(cp = 0.00002, minbucket=5))

# Find tree size that minimises CV-MSE
op.index_4 <- which.min(linear.singletree_4$cptable[, "xerror"])

# Plot CV-MSE
plotcp(linear.singletree_4)
abline(v = op.index_4, lty = "dashed")

# Get cp-value that corresponds to optimal tree size
cp.vals_4 <- linear.singletree_4$cptable[op.index_4, "CP"]

# Prune the tree
treepruned.linearsingle_4 <- prune(linear.singletree_4, cp = cp.vals_4)

# Extrapolate Tree to Estimation Sample
leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_4, newdata= extr_covariates_est, type = "where"))
dummies_train = model.matrix(~leaves )

leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_4, newdata= extr_covariates_train, type = "where"))
dummies_est = model.matrix(~leaves )

leaves <- as.factor(rpart.predict.leaves(treepruned.linearsingle_4, newdata= extr_covariates_hold_out, type = "where"))
dummies_out = model.matrix(~leaves )

nam_train <- as.vector(colnames(dummies_train))
nam_est <- as.vector(colnames(dummies_est))
nam_out <- as.vector(colnames(dummies_out))

for (i in c(1:ncol(dummies_train))) {
    if (nam_train[i] != nam_est[i]) {
        s = i -1
        dummies_est <- as.matrix(cbind(dummies_est[,c(1:s)],rep(0,times=nrow(dummies_est)), 
                                       dummies_est[,c(i:ncol(dummies_est))]))
        nam_est <- as.vector(c(nam_est[c(1:s)],"NA", nam_est[c(i:length(nam_est))]))
    }
    
}

for (i in c(1:ncol(dummies_train))) {
    if (nam_train[i] != nam_out[i]) {
        s = i -1
        dummies_out <- as.matrix(cbind(dummies_out[,c(1:s)],rep(0,times=nrow(dummies_out)), 
                                       dummies_out[,c(i:ncol(dummies_out))]))
        nam_out <- as.vector(c(nam_out[c(1:s)],"NA", nam_out[c(i:length(nam_out))]))
    }
    
}

res <- lm.fit(dummies_est,final_price_train)
coef <- as.matrix(res$coefficients)
coef[is.na(coef)] <- 0

# Prediction

pred_tree_hold_out_4  <- as.matrix(dummies_out) %*% as.matrix(coef)

#####

## Assess performance of tree estimator
# Hold-out-sample RMSE
rmse_hold_out_4 <- round(sqrt(mean((final_price_hold_out - 0.5*(pred_tree_hold_out_3 
                            + pred_tree_hold_out_4))^2)),digits=3)
# Hold-out-sample R-squared
r2_hold_out_4 <- round(1-mean((final_price_hold_out - 0.5*(pred_tree_hold_out_3 
                            + pred_tree_hold_out_4))^2)/mean((final_price_hold_out 
                            - mean(final_price_hold_out))^2),digits=3)

print(paste0("Hold-out-Sample RMSE: ", rmse_hold_out_4))
print(paste0("Hold-out-Sample R-squared: ", r2_hold_out_4))


