library(bnlearn)

#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library(Rgraphviz)

data <- read.csv("hurricane/Harvey/MTurk_Harvey.csv", header=TRUE)
#data <- lapply(data, as.numeric)
data <- lapply(data, as.ordered)
X <- as.data.frame(data)

demo <- c("age", "is_male", "edu", "income", "househd_size", "is_white", "is_black", "is_asian", "is_hispanic",
          "has_children", "has_elders", "has_special_needs", "has_pets")

house <- c("house_single_fam", "house_condo", "house_mobile", "house_wood", "house_brick", "is_owner", "has_insurance", "coast_dist")

info_tv <- c("use_tv_radio", "tv_radio_storm_damage", "tv_radio_casualty","tv_radio_preparation", "tv_radio_road_traffic","tv_radio_people_stay", "tv_radio_people_leave")

info_social_media <- c("use_social_media", "social_media_storm_damage", "social_media_storm_strength", "social_media_road_traffic", "social_media_preparation","social_media_people_stay", "social_media_people_leave")

info_other <- c("friends_suggest_stay", "friends_suggest_evac","neighbors_stay", "neighbors_evac")

evac_notice <- c("received_evac_notice", "no_evac_notice", "received_mandatory", "received_voluntary", 
                 "received_stay_notice", "no_stay_notice", "evac_notice_before_landfall", "evac_notice_after_landfall")

risk <- c("risk_of_stay", "risk_of_evac", "wind_risk_to_safety", "wind_risk_to_property", "flood_risk_to_safety", "flood_risk_to_property")

evac_ability <- c("ability_children", "ability_elders", "ability_special_needs", "ability_pets", 
                  "ability_expense", "ability_no_transport", "ability_no_place", "ability_job")

evac_decision <- c("evac_decision")

blklist <- list(c(demo, house, info_tv, info_social_media, info_other, evac_notice), c(risk, evac_ability), evac_decision)
#net <- iamb(X, alpha=0.01, blacklist = tiers2blacklist(blklist))
#graphviz.plot(net, shape="rectangle")

# explore
# sapply(X, class)
# sapply(X, levels)
# levels(X$age)
# learned <- hc(X)
# graphviz.plot(learned, shape="rectangle")
# score(learned, data=X, type="bic")
# arc.strength(learned, data=X, criterion="bic")

# random start
# nodes <- names(X)
# start <- random.graph(node=nodes, method="ic-dag", num=1000, every=50)
# netlist <- lapply(start, function(net){hc(X, score="bic", blacklist=tiers2blacklist(blklist), start=net)})
# rnd <- custom.strength(netlist, nodes=nodes)
# rnd[(rnd$strength > 0.85) & (rnd$direction >= 0.5), ]
# avg.start <- averaged.network(rnd, threshold = 0.85)
# graphviz.plot(avg.start, shape="rectangle")



prior <- empty.graph(nodes=names(X))

# demo
prior <- set.arc(prior, from="age", to="ability_children")
prior <- set.arc(prior, from="househd_size", to="ability_children")
prior <- set.arc(prior, from="has_children", to="ability_children")
prior <- set.arc(prior, from="age", to="ability_elders")
prior <- set.arc(prior, from="househd_size", to="ability_elders")
prior <- set.arc(prior, from="has_elders", to="ability_elders")
prior <- set.arc(prior, from="age", to="ability_special_needs")
prior <- set.arc(prior, from="househd_size", to="ability_special_needs")
prior <- set.arc(prior, from="has_special_needs", to="ability_special_needs")
prior <- set.arc(prior, from="has_pets", to="ability_pets")
prior <- set.arc(prior, from="age", to="income")
prior <- set.arc(prior, from="edu", to="income")
prior <- set.arc(prior, from="is_white", to="income")
prior <- set.arc(prior, from="is_black", to="income")
prior <- set.arc(prior, from="is_asian", to="income")
prior <- set.arc(prior, from="is_hispanic", to="income")
prior <- set.arc(prior, from="income", to="ability_expense")
prior <- set.arc(prior, from="income", to="ability_no_place")
prior <- set.arc(prior, from="income", to="ability_no_transport")
prior <- set.arc(prior, from="income", to="ability_job")

# house
prior <- set.arc(prior, from="coast_dist", to="risk_of_stay")
prior <- set.arc(prior, from="coast_dist", to="risk_of_evac")
prior <- set.arc(prior, from="coast_dist", to="wind_risk_to_safety")
prior <- set.arc(prior, from="coast_dist", to="wind_risk_to_property")
prior <- set.arc(prior, from="coast_dist", to="flood_risk_to_safety")
prior <- set.arc(prior, from="coast_dist", to="flood_risk_to_property")
prior <- set.arc(prior, from="house_single_fam", to="risk_of_stay")
prior <- set.arc(prior, from="house_single_fam", to="wind_risk_to_safety")
prior <- set.arc(prior, from="house_single_fam", to="wind_risk_to_property")
prior <- set.arc(prior, from="house_single_fam", to="flood_risk_to_safety")
prior <- set.arc(prior, from="house_single_fam", to="flood_risk_to_property")
prior <- set.arc(prior, from="house_condo", to="risk_of_stay")
prior <- set.arc(prior, from="house_condo", to="wind_risk_to_safety")
prior <- set.arc(prior, from="house_condo", to="wind_risk_to_property")
prior <- set.arc(prior, from="house_condo", to="flood_risk_to_safety")
prior <- set.arc(prior, from="house_condo", to="flood_risk_to_property")
prior <- set.arc(prior, from="house_mobile", to="risk_of_stay")
prior <- set.arc(prior, from="house_mobile", to="wind_risk_to_safety")
prior <- set.arc(prior, from="house_mobile", to="wind_risk_to_property")
prior <- set.arc(prior, from="house_mobile", to="flood_risk_to_safety")
prior <- set.arc(prior, from="house_mobile", to="flood_risk_to_property")
prior <- set.arc(prior, from="house_wood", to="risk_of_stay")
prior <- set.arc(prior, from="house_wood", to="wind_risk_to_safety")
prior <- set.arc(prior, from="house_wood", to="wind_risk_to_property")
prior <- set.arc(prior, from="house_wood", to="flood_risk_to_safety")
prior <- set.arc(prior, from="house_wood", to="flood_risk_to_property")
prior <- set.arc(prior, from="house_brick", to="risk_of_stay")
prior <- set.arc(prior, from="house_brick", to="wind_risk_to_safety")
prior <- set.arc(prior, from="house_brick", to="wind_risk_to_property")
prior <- set.arc(prior, from="house_brick", to="flood_risk_to_safety")
prior <- set.arc(prior, from="house_brick", to="flood_risk_to_property")
prior <- set.arc(prior, from="is_owner", to="risk_of_stay")
prior <- set.arc(prior, from="is_owner", to="wind_risk_to_property")
prior <- set.arc(prior, from="is_owner", to="flood_risk_to_property")
prior <- set.arc(prior, from="has_insurance", to="risk_of_stay")
prior <- set.arc(prior, from="has_insurance", to="wind_risk_to_property")
prior <- set.arc(prior, from="has_insurance", to="flood_risk_to_property")

# media
prior <- set.arc(prior, from="tv_radio_storm_damage", to="risk_of_stay")
prior <- set.arc(prior, from="tv_radio_storm_damage", to="wind_risk_to_property")
prior <- set.arc(prior, from="tv_radio_storm_damage", to="flood_risk_to_property")
prior <- set.arc(prior, from="tv_radio_casualty", to="risk_of_stay")
prior <- set.arc(prior, from="tv_radio_casualty", to="wind_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_casualty", to="flood_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_road_traffic", to="risk_of_evac")
#prior <- set.arc(prior, from="tv_radio_road_traffic", to="risk_of_stay")
prior <- set.arc(prior, from="tv_radio_preparation", to="risk_of_stay")
#prior <- set.arc(prior, from="tv_radio_preparation", to="risk_of_evac")
prior <- set.arc(prior, from="tv_radio_people_leave", to="risk_of_stay")
prior <- set.arc(prior, from="tv_radio_people_leave", to="risk_of_evac")
prior <- set.arc(prior, from="tv_radio_people_leave", to="wind_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_people_leave", to="wind_risk_to_property")
prior <- set.arc(prior, from="tv_radio_people_leave", to="flood_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_people_leave", to="flood_risk_to_property")
prior <- set.arc(prior, from="tv_radio_people_stay", to="risk_of_stay")
prior <- set.arc(prior, from="tv_radio_people_stay", to="risk_of_evac")
prior <- set.arc(prior, from="tv_radio_people_stay", to="wind_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_people_stay", to="wind_risk_to_property")
prior <- set.arc(prior, from="tv_radio_people_stay", to="flood_risk_to_safety")
prior <- set.arc(prior, from="tv_radio_people_stay", to="flood_risk_to_property")
prior <- set.arc(prior, from="social_media_storm_damage", to="risk_of_stay")
prior <- set.arc(prior, from="social_media_storm_strength", to="risk_of_stay")
prior <- set.arc(prior, from="social_media_road_traffic", to="risk_of_stay")
prior <- set.arc(prior, from="social_media_preparation", to="risk_of_stay")
prior <- set.arc(prior, from="social_media_people_stay", to="risk_of_stay")
prior <- set.arc(prior, from="social_media_people_leave", to="risk_of_stay")
prior <- set.arc(prior, from="friends_suggest_stay", to="risk_of_stay")
prior <- set.arc(prior, from="friends_suggest_evac", to="risk_of_stay")
prior <- set.arc(prior, from="neighbors_stay", to="risk_of_stay")
prior <- set.arc(prior, from="neighbors_evac", to="risk_of_stay")

# evac notice
prior <- set.arc(prior, from="received_evac_notice", to="risk_of_stay")
prior <- set.arc(prior, from="no_evac_notice", to="risk_of_stay")
prior <- set.arc(prior, from="received_mandatory", to="risk_of_stay")
prior <- set.arc(prior, from="received_voluntary", to="risk_of_stay")
prior <- set.arc(prior, from="received_stay_notice", to="risk_of_stay")
prior <- set.arc(prior, from="no_stay_notice", to="risk_of_stay")


# 2nd layer
prior <- set.arc(prior, from="ability_children", to="evac_decision")
prior <- set.arc(prior, from="ability_elders", to="evac_decision")
prior <- set.arc(prior, from="ability_special_needs", to="evac_decision")
prior <- set.arc(prior, from="ability_pets", to="evac_decision")
prior <- set.arc(prior, from="ability_expense", to="evac_decision")
prior <- set.arc(prior, from="ability_no_place", to="evac_decision")
prior <- set.arc(prior, from="ability_no_transport", to="evac_decision")
prior <- set.arc(prior, from="ability_job", to="evac_decision")
prior <- set.arc(prior, from="risk_of_stay", to="evac_decision")
prior <- set.arc(prior, from="risk_of_evac", to="evac_decision")
# prior <- set.arc(prior, from="wind_risk_to_safety", to="evac_decision")
# prior <- set.arc(prior, from="wind_risk_to_property", to="evac_decision")
# prior <- set.arc(prior, from="flood_risk_to_safety", to="evac_decision")
# prior <- set.arc(prior, from="flood_risk_to_property", to="evac_decision")

### cause error - attempting to create a factor with more than INT_MAX levels.?????
### reason: risk_of_stay has too many parents (8+6+6+7=27)




# prior <- set.arc(prior, from="tv_radio_preparation", to="wind_risk_to_property")
# prior <- set.arc(prior, from="tv_radio_preparation", to="flood_risk_to_property")


#learned_net <- hc(X, score="bde", iss=15, blacklist=tiers2blacklist(blklist), start=prior)
learned_net <- hc(X, score="bic", blacklist=tiers2blacklist(blklist), start=prior)
graphviz.plot(learned_net, shape="rectangle")


learned_net <- gs(X, alpha=0.01, blacklist = tiers2blacklist(blklist), debug=TRUE)

# model averaging
boot <- boot.strength(X, R = 100, algorithm = "hc", algorithm.args = list(score="bic", blacklist=tiers2blacklist(blklist), start=prior))
#boot <- boot.strength(X, R = 1000, algorithm = "tabu", algorithm.args = list(score = "bic", blacklist=tiers2blacklist(blklist)))
#boot <- boot.strength(X, R = 100, algorithm = "gs", algorithm.args = list(alpha = 0.05, blacklist=tiers2blacklist(blklist)))

boot[(boot$strength > 0.7) & (boot$direction >= 0.01), ]
avg.boot <- averaged.network(boot, threshold=0.7)
graphviz.plot(avg.boot, shape="rectangle")

