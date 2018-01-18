library("ClustOfVar")

data <- read.csv("hurricane/Harvey/MTurk_Harvey_var_cluster_v2.csv", header=TRUE)
#data <- lapply(data, as.numeric)
X <- as.data.frame(data)
#X.quali <- X[,1]
#X.quanti <- X[,2:56]
#tree <- hclustvar(X.quanti, X.quali)

tree <- hclustvar(X)
plot(tree)

stab <- stability(tree)
stab$meanCR
plot(stab, main = "Stability of the partitions")

p6 <- cutreevar(tree, 6)
print(p6)
write.csv(p6$scores, 'hurricane/Harvey/harvey_6clusters.csv')
# cluster1: demo, house
# cluster2: children, elders, special_needs, 
# cluster3: evac_notice, friends, neighbors
# cluster4: tv
# cluster5: social media
# cluster6: risk

p9 <- cutreevar(tree, 9)
write.csv(p9$scores, 'hurricane/Harvey/harvey_9clusters.csv')
# cluster1: house, money
# cluster2: elders, special_needs
# cluster3: children
# cluster4: pets, race
# cluster5: house material
# cluster6: evac_notice, friends, neighbors
# cluster7: tv
# cluster8: social media
# cluster9: risk
