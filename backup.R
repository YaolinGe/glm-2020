# Another one -------------------------------------------------------------
library(dplyr)
library(magrittr)

n.groups <- 4 # number of groups
n.repeats <- 2 # samples per group
groups <- rep(1:n.groups, each = n.repeats) %>% as.factor
print(groups)

n <- length(groups)
z0 <- rnorm(n.groups, 0, 10)
(z <- z0[as.numeric(groups)]) # generate and inspect random group effects

print(n)
print(z0)
print(z)

epsilon <- rnorm(0, 1) # generate measurement error
beta0 <- 2 # this is the actual parameter of interest! The intercept
y <- beta0 + z + epsilon # sample from an LMM


# fit a linear model assuming independence
lm.5 <- lm(y~1)
# fit a mixed_model that deals with the group dependence
library(lme4)
lme.5 <- lmer(y~1|groups)

summary.lm.5 <- summary(lm.5)
print(summary.lm.5)



<!-- $$ -->
    <!-- y_{ij} = \begin{bmatrix} 1 & x_{ij}\end{bmatrix} \begin{bmatrix} \beta_0 \\ \beta_1\end{bmatrix} + \begin{bmatrix} 1 & x_{ij}\end{bmatrix} \begin{bmatrix} \gamma_{0, i} \\ \gamma_{1, i}\end{bmatrix} + \epsilon_{ij} -->
    <!-- $$ -->
    
    <!-- Letting -->
    <!-- $$ -->
    <!-- y_i = \begin{bmatrix} y_{i1} \\ y_{i2} \\ $\vdots$ \\ y_{in_i} \end{bmatrix}, \ \ x_i = \begin{bmatrix} x_{i1}^T \\ x_{i2}^T \\ \vdots \\ x_{in_i}^T \end{bmatrix}, \ \ U_i = \begin{bmatrix} u_{i1}^T \\ u_{i2}^T \\ \vdots \\ u_{in_i}^T \end{bmatrix}, \ \ \epsilon_i = \begin{bmatrix} \epsilon_{i1} \\ \epsilon_{i2} \\ $\vdots$ \\ \epsilon_{in_i} \end{bmatrix} -->
    <!-- $$ -->
    
    
    