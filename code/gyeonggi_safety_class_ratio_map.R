install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("sp")
install.packages("plotly")
install.packages("sf")
install.packages("terra")
install.packages("broom")
install.packages("ggalt")
install.packages("ggthemes")
install.packages("viridis")
install.packages("dplyr")
install.packages('geojsonio')
install.packages('ggspatial')

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(sp)
library(plotly)
library(sf)
library(terra)
library(broom)
library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(ggthemes)
library(viridis)
library(dplyr)
library(geojsonio)
library(ggspatial)

P <- read.csv("data/g_r_mean4.csv", header = TRUE) # 시각화할 데이터셋
map <- geojson_read("data/TL_SCCO_SIG.geojson", what="sp") # 지리 정보 데이터셋
head(map)

# 좌표계 변환
from_crs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m")
to_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

map <- spTransform(map, to_crs)
summary(map)
head(map)

map <- gBuffer(map, byid=TRUE, width=0)
new_map <- fortify(map, region="SIG_CD")
str(new_map)
head(new_map)

# 경기만 추출
new_map$id <- as.numeric(new_map$id)
gyeonggi_map <- new_map[new_map$id <= 41900,]

# 확인
unique_ids <- unique(gyeonggi_map$id)
print(unique_ids)

# merge
join_map <- left_join(P,gyeonggi_map, by = 'id')
glimpse(join_map)

# 시군구별 경계
ggplot() + geom_polygon(data = join_map, aes(x=long, y=lat, group=group), fill = 'white', color='black')

# 이름 넣기
gyeonggi_district<- read.csv('data/gyeonggi_long_lat.csv', header = T)

# 옵션 변경
ggplot() + geom_polygon(data = join_map, aes(x=long, y=lat, group=group, fill = mean_safty))
unique(join_map$id)

# ggplot을 활용해 지도 변형
plot <- ggplot() + geom_polygon(data = join_map, aes(x=long, y=lat, group=group, fill = mean_safty))
plot + scale_fill_gradient(low = "#dce4f0", high = "#0067a3", space = "Lab", guide = "colourbar") + theme_bw() + labs(title = "경기도 평균 안전등급 분포") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))+geom_text(data = gyeonggi_district, aes(x = long, y = lat, label= paste(시군구명)))

# ggplot을 활용해 지도 변형
plot <- ggplot() + geom_polygon(data = join_map, aes(x=long, y=lat, group=group, fill = mean_after))
plot + scale_fill_gradient(low = "#dce4f0", high = "#0067a3", space = "Lab", guide = "colourbar") + theme_bw() + labs(title = "경기도 시군구별 준공 후 경과연수") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))+geom_text(data = gyeonggi_district, aes(x = long, y = lat, label= paste(시군구명)))

# C, D, E 등급 비율
P_cde <- read.csv("data/g_r_pct_cde2.csv", header = TRUE)

# merge
join_map_cde <- left_join(P_cde,gyeonggi_map, by = 'id')
glimpse(join_map_cde)

ggplot() + geom_polygon(data = join_map_cde, aes(x=long, y=lat, group=group), fill = 'white', color='black')

# ggplot을 활용해 지도 변형
plot <- ggplot() + geom_polygon(data = join_map_cde, aes(x=long, y=lat, group=group, fill = pct))
plot + scale_fill_gradient(low = "#dce4f0", high = "#0067a3", space = "Lab", guide = "colourbar") + theme_bw() + labs(title = "경기도 시군구별 cde교량 비율") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))+geom_text(data = gyeonggi_district, aes(x = long, y = lat, label= paste(시군구명)))