
library(lme4)
attach(sleepstudy)

x <- Days
# print(x)
groups <- Subject
# print(groups)
    
U <- list()
# X <- list()


for (i in levels(groups)){
    # print(i)
    U[[i]] <- cbind(1, x[groups == i])
    # X[i] <- cbind(1, x[groups == i])
    # print(X[[i]])
}

U <- bdiag(U)
# X <- cbind(1, x)
print(summary(U))

detach(sleepstudy)


