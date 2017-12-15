library(ggmap)

gps <- read.csv("C:/Users/Sophie/Documents/Statistics/project/gps_addr/wifigps_addr_01.csv")
map <- get_googlemap(center = c(lon = median(gps$longitude), lat = median(gps$latitude)),
                    zoom = 15,
                    maptype = c("roadmap"))

p <- ggmap(map, extent="device")
p + geom_point(aes(x = gps$longitude, y = gps$latitude),
               data = gps,
               colour = "red", 
               alpha=0.2,
               size = 3) + ggtitle("01")

ggsave("C:/Users/Sophie/Documents/Statistics/project/test.png")

library(ggmap)
ids <- c('01', '02', '03', '04', '05', 
         '07', '08', '09', '10', '14', 
         '15', '16', '17', '18', '19', 
         '20', '22', '23', '24', '27', 
         '30', '32', '33', '35', '43')

s <- "C:/Users/Sophie/Documents/Statistics/project/gps_addr/wifigps_addr_"

for (id in ids){
  fp <- paste0(s, id, ".csv")
  gps <- read.csv(fp)
  map <- get_googlemap(center = c(lon = median(gps$longitude), lat = median(gps$latitude)),
                       zoom = 15,
                       maptype = c("roadmap"))
  
  p <- ggmap(map, extent="device")
  p + geom_point(aes(x = gps$longitude, y = gps$latitude),
                 data = gps,
                 colour = "red", 
                 alpha=0.2,
                 size = 3) + ggtitle(id)
  output_plot <- paste0("C:/Users/Sophie/Documents/Statistics/project/plots/", id, ".png")
  ggsave(output_plot)
}