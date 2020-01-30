#Problem 2
X <- c(16, 16, 16, 16, 
       24, 24, 24, 24, 
       32, 32, 32, 32, 
       40, 40, 40, 40)
Y <- c(199, 205, 196, 200, 
       218, 220, 215, 223, 
       237, 234, 235, 230, 
       250, 248, 253, 246)

t1 <- c(1,2)
t2 <- c(4,5)
sum(t1*t2) / sum(t1*t1)

plot(X, Y)
model <- lm(Y ~ X)
summary(model)
abline(model)


# Problem 3

library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
sestates <- read.csv("http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv", header = TRUE, stringsAsFactors = FALSE)
head(sestates)
estates <- tbl_df(sestates)


# replace "sale_date" with "day_week", "month", "day_month"
estates_a <- estates %>% 
  separate(sale_date, c("day_week", "month", "day_month")) 
glimpse(estates_a)

# the top 10 cities with the most transactions
estates %>%
  group_by(city) %>%
  summarise(count_by_city = n()) %>%
  arrange(desc(count_by_city)) %>%
  head(10)

# accumulated number of transactions from May 15 to May 21 in city ELK GROVE
estates_c <- estates_a %>%
  select(city, month, day_month) %>%
  filter(city=="ELK GROVE", month=="May", day_month>=15 & day_month<=21) %>%
  group_by(day_month) %>%
  summarise(count_by_day = n()) %>%
  arrange(day_month) %>%
  mutate(cum_count_by_day=cumsum(count_by_day)) 
estates_c

# For each type of house, the highest 3 transaction prices
estates %>%
  select(type, city, price) %>%
  group_by(type) %>%
  arrange(desc(price)) %>%
  top_n(3)

# For each type of house, the highest 3 transaction prices per sq_ft
summary(estates$sq__ft)
estates[estates == 0] <- NA
summary(estates$sq__ft)
estates %>%
  select(type, city, price, sq__ft) %>%
  mutate(price_sq = price / sq__ft) %>%
  group_by(type) %>%
  arrange(desc(price_sq)) %>%
  top_n(3) %>%
  select(type, city, price_sq)

# For each type of house in city SACRAMENTO, the number of transactions, average price, and average price per square foot?

estates[estates == 0] <- NA
estates %>%
  filter(city=="SACRAMENTO") %>%
  select(type, price, sq__ft) %>%
  mutate(price_sq = price / sq__ft) %>%
  group_by(type) %>%
  summarise(n_trans=n(), avg_price=mean(price), avg_price_sq=mean(price_sq, na.rm=TRUE))




# Problem 4

library(ggplot2, quietly = TRUE)
#data(diamonds)

p <- ggplot(diamonds, aes(x=carat, y=price)) 
p <- p + geom_point()
#p + geom_smooth(method='lm',formula=y~x)

p <- p + facet_grid(cut ~ .)
p + geom_smooth(method='lm',formula=y~x)