print("Hello world")

# LOAD packages
library(lme4)

library(optimization)

n_groups <- 7
n_rep <- 3

df.bond <- data.frame(ingot = rep(1:n_groups, each = n_rep),
                      metal = rep(c("n", "i", "c"), n_groups), 
                      pres = c(67,71.9,72.2,
                               67.5,68.8,66.4,
                               76,82.6,74.5,
                               72.7,78.1,67.3,
                               73.1,74.2,73.2,
                               65.8,70.8,68.7,
                               75.6,84.9,69))

n_total <- dim(df.bond)[1]
    
# print(df.bond$ingot)
# print(class(df.bond$ingot))
df.bond$ingot <- factor(df.bond$ingot)
# print(df.bond$ingot)
# print(class(df.bond$ingot))

lmm.bond <- lmer(pres ~ metal + (1 | ingot), REML = FALSE, data = df.bond)
print(summary(lmm.bond))
print(anova(lmm.bond))

matrix.x <- model.matrix(lmm.bond)
# print(matrix.x)

# linear mixed model by maximum likelihood estimation

# Now specify the design matrix

matrix.z <- matrix(0, n_total, n_groups)
for (i in 1:n_groups){
    matrix.z[c((i - 1) * 3 + 1):(i * 3), i] = 1
}
# print(df.z)
colnames(matrix.z) <- paste0("ingot:", 1:7)

loglikef <- function(x) {
    vector.Y <- as.vector(df.bond$pres)
    matrix.G <- x[1] * diag(1, nrow = n_groups)
    matrix.R <- x[2] * diag(1, nrow = n_total)
    matrix.V <- matrix.z %*% matrix.G %*% t(matrix.z) + matrix.R
    vector.Beta <- solve(t(matrix.x) %*% solve(matrix.V) %*% matrix.x) %*% t(matrix.x) %*% solve(matrix.V) %*% vector.Y
    
    loglike <- log(det(matrix.V)) + t(vector.Y - matrix.x %*% vector.Beta) %*% solve(matrix.V) %*% (vector.Y - matrix.x %*% vector.Beta)
    
    return(c(loglike))
}

hat.cov.para <- optim_nm(fun = loglikef, k = 2, start = c(1, 1), maximum = FALSE, tol = 0.00000000001)$par

# Then estimate fix effect
hat.beta <- function(x) {
    vector.Y <- as.vector(df.bond$pres)
    matrix.G <- x[1] * diag(1, nrow = n_groups)
    matrix.R <- x[2] * diag(1, nrow = n_total)
    matrix.V <- matrix.z %*% matrix.G %*% t(matrix.z) + matrix.R
    vector.Beta <- solve(t(matrix.x) %*% solve(matrix.V) %*% matrix.x) %*% t(matrix.x) %*% solve(matrix.V) %*% vector.Y
    return(vector.Beta)
}

print(round(hat.beta(x = hat.cov.para), 4))

## Now solve the probelm using REML
relmm.bond <- lmer(pres ~ metal + (1|ingot), REML = TRUE, data = df.bond)
print(summary(relmm.bond))


reloglikef <- function(x) {
    vector.Y <- as.vector(df.bond$pres)
    matrix.G <- x[1] * diag(1, nrow = n_groups)
    matrix.R <- x[2] * diag(1, nrow = n_total)
    matrix.V <- matrix.z %*% matrix.G %*% t(matrix.z) + matrix.R
    vector.Beta <- solve(t(matrix.x) %*% solve(matrix.V) %*% matrix.x) %*% t(matrix.x) %*% solve(matrix.V) %*% vector.Y
    loglike <- log(det(matrix.V)) + log(det(t(matrix.x) %*% solve(matrix.V) %*% matrix.x)) + t(vector.Y - matrix.x %*% vector.Beta) %*% solve(matrix.V) %*% (vector.Y - matrix.x %*% vector.Beta)
    
    return(c(loglike))
}

print(optim_nm(fun = reloglikef, k = 2, start = c(1, 1), maximum = FALSE, tol = 0.000000001)$par)
print(optim(par = c(1, 1), reloglikef)$par)




