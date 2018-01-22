library("ClustOfVar")

data <- read.csv("hurricane/Irma/MTurk_Irma_var_cluster.csv", header=TRUE)
X <- as.data.frame(data)

tree <- hclustvar(X)
plot(tree)



stab <- stability(tree)
stab$meanCR
plot(stab, main = "Stability of the partitions")

p8 <- cutreevar(tree, 8)
write.csv(p8$scores, 'hurricane/Irma/Irma_8clusters.csv')

p9 <- cutreevar(tree, 9)
write.csv(p9$scores, 'hurricane/Irma/Irma_9clusters.csv')

p11 <- cutreevar(tree, 11)
print(p11)
write.csv(p11$scores, 'hurricane/Irma/Irma_11clusters.csv')

#tree2 <- kmeansvar(X, init=11)
#plot(tree2)