# Load library with dataset
library(MASS)
library(ISLR2)

# PENALIZED LINEAR REGRESSION

# Check out the first 5 entries in the dataset
head(Boston)

# Add Boston to the searh path, so its columns can be referenced without
# having to suffix them with Boston$ every time
attach(Boston)

# Run a simple linear regression of medv against lstat in Boston
fitted = lm(medv ~ lstat, data = Boston)

# Using the fact that Boston has been added to the search path, it can be omitted
fitted = lm(medv ~ lstat)

# The fitted object is of type list, class lm. Check its available attributes
names(fitted)

# Obtain a summary of the fit; it is of type list, class summary.
summary(fitted)

# Obtain 95% confidence intervals for the coefficients of the fitted linear model
confint(fitted)

# Do inference for three values of lstat (5, 10, 15)
predict(fitted, data.frame(lstat = c(5, 10, 15)))

# Do inference and obtain confidence intervals for the predictions
predict(fitted, data.frame(lstat = c(5, 10, 15)), interval = 'confidence')

# Do inference and obtain prediction intervals for the predictions
predict(fitted, data.frame(lstat = c(5, 10, 15)), interval = 'prediction')

# Make a scatterplot of two variables from the dataset
plot(lstat, medv)

# Draw the linear regression line over the scatterplot
abline(fitted)

# Plot diagnostic charts for residuals of the linear regression, organize them in 4 quadrants
par(mfrow = c(2, 2))
plot(fitted)

# Reset plots to a single pane
par(mfrow = c(1, 1))

# Remove all plots and reset to a single pane
dev.off()

# Plot the residuals of a linear regression
plot(predict(fitted), residuals(fitted))

# Plot the studentized residuals of a linear regression
plot(predict(fitted), rstudent(fitted))

# Plot leverage statistics
plot(hatvalues(fitted))

# Fit a linear regression against multiple variables.
# Providing data is mandatory even if the source is in the search path
fitted = lm(medv ~ lstat + age, data = Boston)
summary(fitted)

# Fit a linear regression against all the variables
fitted = lm(medv ~ ., data = Boston)

# Fit a linear regression against all variables but one
fitted = lm(medv ~ . - age, data = Boston)

# Update a linear regression removing previously fitted variables
refitted = update(fitted, ~ . - age)

# Update a linear regression adding variables to be fitted
refitted = update(refitted, ~ . + age)

# Compute variance inflation factors
library(car)
vif(fitted)


# Display the coding used for dummy variables (e.g. categorical variables in linear regression)
attach(Carseats)
contrasts(ShelveLoc)

# Plot residuals and their normal density
res = summary(fitted)$residuals
range = seq(min(res), max(res), length = 200)
the_norm = dnorm(range, mean = mean(res), sd = sd(res))
plot(density(summary(fitted)$residuals))
lines(range, the_norm, col = 'blue')

# Make a matrix of scatterplots
pairs(Auto)

# Make a scatterplot matrix, with bivariate scatterplots, histograms and Pearson correlation
pairs.panels(
  Auto,
  method = "pearson",
  # correlation method
  hist.col = "#00AFBB",
  density = TRUE,
  # show density plots
  ellipses = TRUE # show correlation ellipses
)

# Make a correlation matrix of all variables in Auto except `name`
cor(Auto[, -which(names(Auto) == 'name')], method = 'pearson')

# PRINCIPAL COMPONENTS ANALYSIS (PCA)

# Compute variance by column in a data frame
apply(USArrests, 2, var)

# Perform PCA, centering the data and scaling their variance to 1
pca.result = prcomp(USArrests, scale = TRUE)

# Obtain mean and std. dev. used to scale the data before PCA
pca.result$center
pca.result$scale

# Fetch the transformation matrix for the PCA, its columns are the PCs (eigenvectors, a.k.a. loading vectors).
# Multiplying it with the dataset gives the dataset in the PCs space, i.e. the principal component scores
pca.result$rotation

# Project the dataset into the space of the PCA eigenvectors (obtain the score vectors)
# ((as.matrix(USArrests) - pca.result$center) / pca.result$scale) %*% pca.result$rotation
scale(as.matrix(USArrests)) %*% pca.result$rotation 
pca.result$x

# Make a biplot of the first 2 PC and the loadings
biplot(pca.result, scale = 0)

# Obtain the standard deviation of each PC
pca.result$sdev

# Get the variance explained, aka the eigenvalues
pca.result$sdev ^ 2

# Proportion of variance explained by each PC
pca.result$sdev ^ 2 / sum(pca.result$sdev ^ 2)

# Plot the proportion of variance explained (scree plot) and its cumulative values
par (mfrow = c (1 , 2))
plot (
  pve ,
  xlab = "Principal Component" ,
  ylab = "Proportion of Variance Explained" ,
  ylim = c (0 , 1) ,
  type = "b"
)
plot (
  cumsum (pve) ,
  xlab = "Principal Component" ,
  ylab = "Cumulative Proportion of Variance Explained" ,
  ylim = c (0 , 1) ,
  type = "b"
)

# A quick scree plot (of the first couple PC only)
plot(pca.result)

# Do PCA with eigenvalues/eigenvectors of the covariance matrix
cov.mat = cov(scale(as.matrix(USArrests), center=TRUE))
eigen.stuff = eigen(cov.mat, symmetric=TRUE)

# The eigenvectors (by column)
eigen.stuff$vectors

# The eigenvalues, aka the variance explained
eigen.stuff$values
