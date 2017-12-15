library(bnlearn)
library(Rgraphviz)

data <- read.csv("C:/Users/Sophie/workspace/Hurricane/MTurk/data/MTurk_Harvey_expand_evac_ability.csv", header=TRUE)
data <- lapply(data, as.numeric)
X <- as.data.frame(data)

demo <- c("age", "gender", "edu", "income", "househd_size", "r_white", "r_black", "r_asian", "r_hispanic", "r_native", 
          "has_children", "has_elders", "has_special_needs", "has_pets")

house <- c("hs_single_fam", "hs_condo", "hs_mobile", "hm_wood", "hm_brick", "owner", "insurance", "coast_dist")

info_tv <- c("info_tv", "info_tv_damage", "info_tv_casualty", "info_tv_road", "info_tv_prep", "info_tv_stay", "info_tv_leave")

info_social_media <- c("info_social", "info_social_damage", "info_social_storm", "info_social_road", "info_social_prep","info_social_leave", "info_social_stay")

info_other <- c("friends_suggest", "neighbors_doing")

evac_notice <- c("received_evac_notice", "no_evac_notice", "received_mandatory", "received_voluntary", 
                 "received_stay_notice", "no_stay_notice", "evac_notice_before_landfall", "evac_notice_after_landfall")

risk <- c("risk_stay", "risk_evac", "wind_safety", "wind_property", "flood_safety", "flood_property")

evac_ability <- c("ability_children", "ability_elders", "ability_special_needs", "ability_pets", 
                  "ability_expense", "ability_no_transport", "ability_no_place", "ability_job")

evac_decision <- c("evac_decision")

blklist <- list(c(demo, house, info_tv, info_social_media, info_other, evac_notice), c(risk, evac_ability), evac_decision)
net <- iamb(X, alpha=0.01, blacklist = tiers2blacklist(blklist))

graphviz.plot(net, shape="rectangle")

# cannot compute score, because graph is partially directed
score(net, X, type='bde')