#Make a map of helsinki
library(leaflet)
library(magrittr)
 

Helsinkimap <- leaflet() %>% 
    addTiles() %>% 
    setView(24.953390, 60.174259, zoom = 13) %>% 
    addMarkers(24.953390, 60.174259, popup = 'Siwa Liisankatu')

#png(file = "map1.png", width = 480, height = 480)
Helsinkimap
#dev.off()