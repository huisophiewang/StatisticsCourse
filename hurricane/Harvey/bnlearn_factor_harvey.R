library(bnlearn)
library(plyr)


data <- read.csv("hurricane/Harvey/harvey_6clusters.csv", header=TRUE)
# cluster1: demo, house
# cluster2: children, elders, special_needs, 
# cluster3: evac_notice, friends, neighbors
# cluster4: tv
# cluster5: social media
# cluster6: risk
X <- rename(data, c("cluster1"="demo_house", "cluster2"="children_elders",
               "cluster3"="evac_notice_friends_nb", "cluster4"="tv_radio",
               "cluster5"="social_media", "cluster6"="risk"))

X <- discretize(X[,1:6], method='interval', breaks=3, ordered=TRUE)
X$evac_decision = data$evac_decision
X$evac_decision <- as.ordered(X$evac_decision)
blklist <- list(c("demo_house","children_elders","evac_notice_friends_nb","tv_radio","social_media","risk"),c("evac_decision"))

prior <- empty.graph(nodes=names(X))
learned_net <- hc(X, score="bic", blacklist=tiers2blacklist(blklist), start=prior)
graphviz.plot(learned_net, shape="rectangle")

#res <- gs(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
res <- inter.iamb(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
#res <- mmpc(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
#res <- si.hiton.pc(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
graphviz.plot(res, shape="rectangle")

################################################################################
data <- read.csv("hurricane/Harvey/harvey_9clusters.csv", header=TRUE)
# cluster1: house, money
# cluster2: elders, special_needs
# cluster3: children
# cluster4: pets, race
# cluster5: house material
# cluster6: evac_notice, friends, neighbors
# cluster7: tv
# cluster8: social media
# cluster9: risk
X <- rename(data, c("cluster1"="house_money", "cluster2"="elders",
                    "cluster3"="children", "cluster4"="pets_race",
                    "cluster5"="house_material", "cluster6"="evac_notice_friends_nb",
                    "cluster7"="tv_radio","cluster8"="social_media", "cluster9"="risk"))

X <- discretize(X[,1:9], method='interval', breaks=3, ordered=TRUE)
X$evac_decision = data$evac_decision
X$evac_decision <- as.ordered(X$evac_decision)
blklist <- list(c("house_money","elders","children","pets_race","house_material",
                  "evac_notice_friends_nb","tv_radio","social_media"),c("risk"),c("evac_decision"))

#res <- gs(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
res <- inter.iamb(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
#res <- mmpc(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
#res <- si.hiton.pc(X, alpha=0.05, blacklist = tiers2blacklist(blklist))
graphviz.plot(res, shape="rectangle")


