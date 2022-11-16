#Jaguar Datapaper
y1 <- read.csv("jaguar_movement_data.csv", header=T, sep=",")
View(y1)

library (readxl)
y2 <- read_excel("Jaguar_additional information.xlsx", na = "-")
View(y2)

library(dplyr)
y1 <- y1 %>%
  rename(ID = "individual.local.identifier..ID.",
         x = "location.long",
         y = "location.lat",
         tag = "tag.local.identifier") 

y2 <- y2 %>%
  rename(Age = "Estimated Age") 

y3 = merge(y1, y2, by="ID", all.x=F)
y3 <- y3 %>% rename(id = "ID") 
View(y3)

y4<- filter(y3, study.name=="Sao Bento")
View(y4)

#Plot locations of all individuals
plot(y4$x~y4$y, col = as.factor(y4$id), pch = 16, 
     xlab="Longitude", ylab="Latitude")

# Calculate the trajectory among locations for each individual
library(adehabitatLT)  
y4.ltraj <- as.ltraj(xy = y4[,c("x", "y")], id = y4$id, typeII=FALSE)
plot(y4.ltraj)
y4.ltraj 

t1<-y4.ltraj[[1]]# The first six locations of the first animal
t2<-y4.ltraj[[2]]
t3<-y4.ltraj[[3]]
t4<-y4.ltraj[[4]]
t5<-y4.ltraj[[5]]
t6<-y4.ltraj[[6]]
t7<-y4.ltraj[[7]]
t8<-y4.ltraj[[8]]
t9<-y4.ltraj[[9]]
t10<-y4.ltraj[[10]]
t11<-y4.ltraj[[11]]

y4.ltraj_all<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) #combine all trajectories
summary(y4.ltraj_all)

y4.traj<-merge(x=y4,y=y4.ltraj_all,by=c("x","y"),all.x=TRUE,all.y=TRUE)

# Create a dataframe to hold all of the contents of bltu.paths with a column for id. 
# Put first element into the dataframe
total.path.df <- data.frame(y4.ltraj[[1]], id = attr(y4.ltraj[[1]], "id"))
# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories.
for(i in 2:length(y4.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(y4.ltraj[[i]], id = attr(y4.ltraj[[i]], "id")))
}

# Calculate distance travelled per interval and add it to the dataframe
total.path.df$distperweek <- total.path.df$dist / (total.path.df$dt/60/60/24)

# Aggregate to show mean distance per interval for each turtle
path.summary <- aggregate(distperweek~id, data = total.path.df, FUN = mean)
path.summary$sd <- aggregate(distperweek~id, data = total.path.df, FUN = sd)$distperweek

# Look at summmary dataframe
path.summary
summary(path.summary)
# Make a graph to visualize data using ggplot
library(ggplot2)
# Create limits used for error bars in graph
limits <- aes(ymax = path.summary$distperweek + path.summary$sd, 
              ymin = path.summary$distperweek - path.summary$sd)

# Make plot. Choose the dataframe (data) and aesthetics (aes; for the x and y)
path.plot <- ggplot(data = path.summary, aes(x = id, y = distperweek, colour = id)) + 
  geom_point(size = 3) + # add points
  geom_errorbar(limits, width = 0.2) + # adds error bars
  labs(x = "Animal number", 
       y = "Mean distance travelled per week (m)" ) + # Axis labels
  theme_classic() + # Make plot black and white with no background grid
  theme(legend.position = "none")
path.plot # call plot

# Only include three columns (id, x, and y coordinates) for making MCP's
y4.sp <- y4[, c("id", "x", "y")] 

# Create a SpatialPointsDataFrame by defining the coordinates
library(sp)
coordinates(y4.sp) <- c("x", "y")
# Set the coordinate reference system (CRS)
proj4string(y4.sp) <- CRS( "+proj=longlat +ellps=GRS80 +datum=WGS84" )

library(adehabitatHR) # Load library
y4.mcp1 <- mcp(y4.sp, percent = 95)## estimates the MCP 95%
y4.mcp1
plot(y4.mcp1)## Plot the home ranges  MCP 95%
plot(y4.mcp1, col = alpha(1:11, 0.5), add = TRUE)
y4.mcp95<-as.data.frame(y4.mcp1)## Store the home-range size as dataframe
y4.mcp95<- y4.mcp95 %>%
  rename(MCP95 = "area") 

y4.mcp2 <- mcp(y4.sp, percent = 100)## estimates the MCP 100%
y4.mcp2
plot(y4.mcp2)## Plot the home ranges MCP 100%
plot(y4.mcp2, col = alpha(1:11, 0.5), add = TRUE)
y4.mcp100<-as.data.frame(y4.mcp2)## Store the home-range size as dataframe
y4.mcp100<- y4.mcp100 %>%
  rename(MCP100 = "area") 

y4.mcp<-merge(x=y4.mcp95,y=y4.mcp100,by=c("id"),all.x=TRUE,all.y=TRUE)
summary(y4.mcp)

y4.metrics<-merge(x=path.summary,y=y4.mcp,by=c("id"),all.x=TRUE,all.y=TRUE)

library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(y4.sp, col = as.factor(y4.sp@data$id), pch = 20)
plot(y4.mcp1, col = alpha(1:11, 0.5), add = TRUE)

library(leaflet)
library(leaflet.extras)
library(leafem)
library(htmlwidgets)

labs <- lapply(seq(nrow(y4.mcp1)), function(i) {
  paste0( '<p>', y4.mcp1[i, "id"], '<p></p>', 
          y4.mcp1[i, "area"], '</p>' ) 
})

labs <- lapply(seq(nrow(y4)), function(i) {
  paste0( '<p>', y4[i, "tag"], '<p></p>', 
          y4[i, "Sex"],'</p><p>',
          y4[i, "Age"],'</p><p>',
          y4[i, "Weight"],'</p><p>',
          y4[i, "timestamp"], '<p></p>', 
          y4[i, "lon"],'</p><p>',
          y4[i, "lat"], '<p></p>',
          y4[i, "study.name"],'</p><p>', 
          y4[i, "country"], '</p>' ) 
})

img <- "https://raw.githubusercontent.com/fblpalmeira/jaguar_interactivemap/main/data/onca_colar.png"

fct <- factor(y4.mcp1$id)
pal <- colorFactor(ggthemes::gdocs_pal()(5), y4.mcp1$id)
leaflet(y4.mcp1) %>% addTiles() %>% 
  addCircles(lng = ~y4.sp$x, lat = ~y4.sp$y, color = ~pal(y4.sp$id), 
             radius = 1, opacity = 0.2, fillOpacity = .1, group ="Circles",
             popup=paste("<b>Jaguar ID:</b>", y4$id, "<br>",
                         "<b>Gender:</b>", y4$Sex, "<br>", 
                         "<b>Age (years):</b>", y4$Age, "<br>",
                         "<b>Weight (kg):</b>", y4$Weight, "<br>",
                         "<b>Timestamp:</b>", y4$timestamp, "<br>", 
                         "<b>Longitude:</b>", y4$x, "<br>",
                         "<b>Latitude:</b>", y4$y, "<br>",
                         "<b>Study name:</b>", y4$study.name, "<br>",
                         "<b>Country:</b>", y4$country)) %>%
  addPolygons(weight = 1, opacity = 0.9, fillOpacity = .4, color = ~pal(id), group="Polygons",
              popup=paste("<b>Jaguar ID:</b>", y4.mcp1$id, "<br>",
                          "<b>MCP95% (ha):</b>", y4.mcp1$area)) %>%
  addLegend('bottomleft', pal = pal, values = ~id, title = 'Jaguar ID') %>%
  addLayersControl(overlayGroups = c("id")) %>%
  addEasyButton(easyButton(
  icon="fa-globe", title="Zoom",
  onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
  addResetMapButton() %>% 
  addMeasure(position="topleft", primaryLengthUnit = "meters", primaryAreaUnit = "hectares") %>%
  addLogo(img, position="topleft", width = 70, height = 35)%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>% 
  addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>% 
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  addLayersControl(
    baseGroups = c("ESRI World Imagery","Open Topo Map","Open Street Map"),
    overlayGroups = c("Polygons",  "Circles"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Circles") %>% 
  addMiniMap(toggleDisplay = TRUE) 

