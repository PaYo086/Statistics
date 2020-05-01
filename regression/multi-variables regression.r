# Author: Po-Yu Lin, GitHub: https://github.com/PaYo086
######                                                                      ######
###### Multiple linear regression - with more than one explanatory variable ######
######                                                                      ######
### Intro: If you want to do the regression of a dependent variable on two or more independent variablees, there are three methods. 1) Simple regression, 2) Regression of residuals, 3) Multiple regression.
# Simple regression: y ~ x1
#                    y ~ x2
#                    y ~ x3
#                    ...
#                    y ~ xn
# Regression of residuals: resid(y ~ X2 + X3 + ... + xn) ~ x1
#                          resid(y ~ X1 + X3 + ... + xn) ~ x2
#                          resid(y ~ x1 + x2 + ... + xn) ~ x3
#                          ...
#                          resid(y ~ x1 + x2 + ... + xn-1) ~ xn
# Multiple regression: y ~ b0 + b1*x1 + b2*x2 + ... + bn*xn
# Ref: Freckleton, R. P. (2002). On the misuse of residuals in ecology: regression of residuals vs. multiple regression. Journal of Animal Ecology, 71(3), 542-545.

windows()
par(mfrow = c(3,2))

# Example using the 'trees' dataset in R; Volume as dependent and Height + Girth as explanatory variables
trees <- as.data.frame(scale(trees)) # standardize first, so the slope (coefficient) is equal to the correlation between the independent and dependent variable.
# 1) make the model separately (without removing any effect of other explanatory variables).
# Model 1: Volume = 0.9671*Girth
# Model 2: Volume = 0.5982*Height
plot(Volume ~ Girth, data = trees)
abline(lm(formula = Volume ~ Girth, data = trees), col = "red")
plot(Volume ~ Height, data = trees)
abline(lm(formula = Volume ~ Height, data = trees), col = "red")

# 2) make the model separately ,a.k.a Regression of residuals
# (removing the effct of other explanatory variables "only" on dependent variable).
# Model 1: Residual of Volume = 0.6565*Girth
# Model 2: Residual of Volume = 0.09604*Height
plot(resid(lm(Volume ~ Height, trees)) ~ Girth, trees, ylab = "Residual of Volume")
abline(lm(resid(lm(Volume ~ Height, trees)) ~ Girth, data = trees), col = "red")
plot(resid(lm(Volume ~ Girth, trees)) ~ Height, trees, ylab = "Residual of Volume")
abline(lm(resid(lm(Volume ~ Girth, trees)) ~ Height, data = trees), col = "red")

# 3) Multiple regression
# Model: Volume = 0.8988*Girth + 0.1315*Height
# Visualized by Partial regression plots
plot(resid(lm(Volume ~ Height, trees)) ~ resid(lm(Girth ~ Height, trees)), ylab = "Residual of Volume", xlab = "Residual of Girth")
abline(lm(resid(lm(Volume ~ Height, trees)) ~ resid(lm(Girth ~ Height, trees))), col = "red")
plot(resid(lm(Volume ~ Girth, trees)) ~ resid(lm(Height ~ Girth, trees)), ylab = "Residual of Volume", xlab = "Residual of Height")
abline(lm(resid(lm(Volume ~ Girth, trees)) ~ resid(lm(Height ~ Girth, trees))), col = "red")

### Summary: Multiple regreesion is the best, because it is not influenced by the correlation between independent (explanatory) variables. Sorry that I don't know how to simulate the correlation condditions... Please chekch the Freckleton 2002 paper.


######                               ######
###### Visualize multiple regression ######
######                               ######
### Multiple regression is unlike other methods. It needs special way to visualize it. 1) Single variable regression plots, 2) Partial residual plots, Partial regression plots.
# Single variable regression plots vs. Partial regression plots vs. Partial residual plots
# Partial regression plot is better.
# Ref: Moya-Larano, J. and Corcobado, G. 2008. Plotting partial correlation and regression in ecological studies. V Web Ecol. 8: 35.V46.

windows()
par(mfrow = c(3,2))

# 1) Single variable regression plots
# The plots without remove the effect of other explanatory variable.
# Model 1: Volume = 0.9671*Girth (Simple regression)
# Model 2: Volume = 0.5982*Height (Simple regression)
plot(Volume ~ Girth, data = trees)
abline(lm(Volume ~ Girth, trees), col = "red")
plot(Volume ~ Height, data = trees)
abline(lm(Volume ~ Height, trees), col = "red")

# 2) Partial residual plots
# This kind of plot shows the true slope of each explanatory variable, but dooesn't show the true scatter. In other words, the correlation coefficient of the plot is bigger than true partial correlation coefficient.
# Model: Volume = 0.8988*Girth + 0.1315*Height (multiple regression)
lm <- lm(Volume ~  Girth + Height, trees)
plot(resid(lm) + lm$coefficients["Girth"]*trees$Girth ~ Girth, trees, ylab = "Residual volume + b*girth")
abline(lm(resid(lm) + lm$coefficients["Girth"]*trees$Girth ~ Girth, trees), col = "red")
plot(resid(lm) + lm$coefficients["Height"]*trees$Height ~ Height, trees, ylab = "Residual volume + b*height")
abline(lm(resid(lm) + lm$coefficients["Height"]*trees$Height ~ Height, trees), col = "red")

# 3) Partial regression plots
# This kind of plot shows the true slope and true scatter of each explanatory variable.
# Model: Volume = 0.8988*Girth + 0.1315*Height (multiple regression)
plot(resid(lm(Volume ~ Height, trees)) ~ resid (lm(Girth ~ Height, trees)), xlab = "Residual girth", ylab = "Residual volume")
abline(lm(resid(lm(Volume ~ Height, trees)) ~ resid (lm(Girth ~ Height, trees))), col = "red")
plot(resid(lm(Volume ~ Girth, trees)) ~ resid (lm(Height ~ Girth, trees)), xlab = "Residual height", ylab = "Residual volume")
abline(lm(resid(lm(Volume ~ Girth, trees)) ~ resid (lm(Height ~ Girth, trees))), col = "red")

### Summary: Partial regression is the best, because it shows the true slopes and true scatters. Please check reference for more details.

### Advertisement ###
# I wrote a function for plotting partial regression plot.
# source("https://raw.githubusercontent.com/PaYo086/My-function/master/part_regre_plot.r")
# Please check my GitHub for more details: https://github.com/PaYo086/My-function

# Examples:
# part.regre.plot(trees, y = 3, x = 2)
# part.regre.plot(trees, y = "Volume", x = "Height")
