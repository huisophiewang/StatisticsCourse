library(bnlearn)
head(learning.test)
# iamb algorithm
plot(inter.iamb(learning.test))
# gs algorithm
plot(gs(learning.test))

# correct graph
data(learning.test)
res = empty.graph(names(learning.test))
modelstring(res) = "[A][C][F][B|A][D|A:C][E|B:F]"
plot(res)

# coronary dataset
head(coronary)
res <- gs(coronary)
plot(res)

# partial ordering 
data(learning.test)
res = gs(learning.test, optimized = TRUE)
plot(res)
ntests(res)
res = set.arc(res, "A", "B")
plot(res)
ord = node.ordering(res)
ord
typeof(ord)
order <- c("A", "C", "F", "B", "D", "E")
res <- gs(learning.test, blacklist = ordering2blacklist(order))
plot(res)

res <- gs(learning.test, blacklist = tiers2blacklist(list(c("A", "C", "F"), c("B", "D", "E"))))
plot(res)

# rowMax <- function(df) 
# {
#   apply(df, 1, max)
# }
# close <- rowMax(data[,c(16:17)])
# see <- rowMeans(data[,c(18:19)])
# peer <- data[,20]
# prev <- rowMeans(data[,c(23:24)])
# finance <- rowMeans(data[,c(25:28)])

#######################################################################################
library(bnlearn)
library(Rgraphviz)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels.csv", header=TRUE)
#data <- d1[,sample(ncol(d1))]
#cor(data)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_NotEvac.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_EarlyEvac.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_LateEvac.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_LowIncome.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_HighIncome.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_VeryLowIncome.csv", header=TRUE)
#data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_Young.csv", header=TRUE)
data <- read.csv("C:/Users/Sophie/workspace/LiliModel/Lili_BN_labels_Old.csv", header=TRUE)

# 1-4
demo <- c("Age", "Gender", "Race", "Marriage")
# 5-8
personal <- c("HouseholdSize", "NumChd", "Edu", "Income")
# 9-12
house <- c("Owner", "HouseStruct", "CloseCoast","CloseWater")
# 13-14
official <- c("OfficialHurricWatch", "OfficialEvac")
# 15-19
info <- c("SrcLocalAuth", "SrcLocalMedia", "SrcNationalMedia", "SrcInternet", "SrcPeers")
#local_info <- c("SrcLocalAuth", "SrcLocalMedia", "SrcPeers")
# 20-22
see <- c("SeeStormCond", "SeeShopClose", "SeePeerEvac")
# 23-24
prev <- c("PrevStormExp", "PrevFalseAlarm")
# 25-29
concern <- c("ProtectFromLooter", "ProtectFromStorm", "LostIncome", "EvacExpense", "Traffic")
# 30
evac <- c("Evac")

X <- data
#X <- data[, c( 20, 21, 22, 28, 30)]

#X <- data[, c(9, 10, 11, 12, 13, 14, 15, 16, 19, 20, 21, 22, 28, 30)]
#X <- data[, c( 11, 13, 14, 15, 19, 22, 30)]
  
#X <- cbind(close, see, peer, data["OfficialHurricWatch"], data["OfficialEvac"], prev, finance, data["Traffic"], data["Evac"])

#res <- gs(X, alpha=0.01)
#res <- gs(X, alpha=0.05, blacklist = tiers2blacklist(list(c("CloseCoast","CloseWater", sources, "concern"), evac)))
#res1 <- gs(X, alpha=0.01, blacklist = tiers2blacklist(list(c(demo, personal, house),  c(see, "EvacExpense", official),"Evac")))
#res1 <- gs(X, alpha=0.05, blacklist = tiers2blacklist(list(house, c(see, official, local_info, "EvacExpense"), evac)))
#res1 <- inter.iamb(X, alpha=0.01, blacklist = tiers2blacklist(list(c("CloseCoast", "OfficialHurricWatch", "OfficialEvac", "SrcLocalAuth", "SrcPeers", "SeePeerEvac"), evac)))
res1 <- iamb(X, alpha=0.01, blacklist = tiers2blacklist(list(c(demo, personal, house),c(prev, official, info, see, concern))))
#res2 <- hc(X, blacklist = tiers2blacklist(list(reasons, "Evac")))
#compare(res1, res2)
#res <- inter.iamb(X)
#plot(res)

#res1 <- gs(X, alpha=0.01, blacklist = tiers2blacklist(list(c(demo, personal, house),  c(official, see, info, prev, concern),"Evac")))
graphviz.plot(res1, shape="rectangle")






