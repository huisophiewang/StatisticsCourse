library(bnlearn)

source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)

data <- read.csv("hurricane/Harvey/MTurk_Harvey.csv", header=TRUE)
#data <- lapply(data, as.numeric)
data <- lapply(data, as.factor)
X <- as.data.frame(data)

demo <- c("age", "is_male", "edu", "income", "househd_size", "is_white", "is_black", "is_asian", "is_hispanic", "is_native", 
          "has_children", "has_elders", "has_special_needs", "has_pets")

house <- c("house_single_fam", "house_condo", "house_mobile", "house_wood", "house_brick", "is_owner", "has_insurance", "coast_dist")

info_tv <- c("use_tv_radio", "tv_radio_storm_damage", "tv_radio_casualty", "tv_radio_road_traffic", "tv_radio_preparation", "tv_radio_people_stay", "tv_radio_people_leave")

info_social_media <- c("use_social_media", "social_media_storm_damage", "social_media_storm_strength", "social_media_road_traffic", "social_media_preparation","social_media_people_stay", "social_media_people_leave")

info_other <- c("friends_suggest_stay", "friends_suggest_evac","neighbors_stay", "neighbors_evac")

evac_notice <- c("received_evac_notice", "no_evac_notice", "received_mandatory", "received_voluntary", 
                 "received_stay_notice", "no_stay_notice", "evac_notice_before_landfall", "evac_notice_after_landfall")

risk <- c("risk_of_stay", "risk_of_evac", "wind_risk_to_safety", "wind_risk_to_property", "flood_risk_to_safety", "flood_risk_to_property")

evac_ability <- c("ability_children", "ability_elders", "ability_special_needs", "ability_pets", 
                  "ability_expense", "ability_no_transport", "ability_no_place", "ability_job")

evac_decision <- c("evac_decision")

#blklist <- list(c(demo, house, info_tv, info_social_media, info_other, evac_notice), c(risk, evac_ability), evac_decision)
#net <- iamb(X, alpha=0.01, blacklist = tiers2blacklist(blklist))
#graphviz.plot(net, shape="rectangle")

#sapply(X, class)
#levels(X$age)
learned <- hc(X)
graphviz.plot(learned, shape="rectangle")
score(learned, data=X, type="bic")
arc.strength(learned, data=X, criterion="bic")

# model averaging
boot <- boot.strength(X, R = 1000, algorithm = "hc",
                      algorithm.args = list(score = "bic"))

boot[(boot$strength > 0.85) & (boot$direction >= 0.5), ]
avg.boot <- averaged.network(boot, threshold=0.85)
graphviz.plot(avg.boot, shape="rectangle")

