library(rMaps)
library(rCharts)
L2 <- Leaflet$new()
L2$setView(c(29.7632836,  -95.3632715), 10)
L2$tileLayer(provider = "MapQuestOpen.OSM")
L2

data(crime, package = 'ggmap')
library(plyr)
crime_dat = ddply(crime, .(lat, lon), summarise, count = length(address))
crime_dat = toJSONArray2(na.omit(crime_dat), json = F, names = F)
cat(rjson::toJSON(crime_dat[1:2]))

# Add leaflet-heat plugin. Thanks to Vladimir Agafonkin
L2$addAssets(jshead = c(
  "http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js"
))

# Add javascript to modify underlying chart
L2$setTemplate(afterScript = sprintf("
                                     <script>
                                     var addressPoints = %s
                                     var heat = L.heatLayer(addressPoints).addTo(map)           
                                     </script>
                                     ", rjson::toJSON(crime_dat)
))

L2
