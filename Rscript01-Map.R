# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information.csv", header=T)
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West Cell"

# Ontario shape
ontario <- st_read("Data/Province.shp")

# make a site data frame
sites <- data.frame(
  Site = Site.info$Field.Number,
  Year = as.character(Site.info$Year),
  Cell = Site.info$Waterbody.Name,
  lon  = Site.info$Start.Longitude,
  lat  =  Site.info$Start.Latitude)

min(sites$lon); max(sites$lon)
min(sites$lat); max(sites$lat)

# make fake points to enable a better cropping of map tiles 
Fakesite <- data.frame(
  Site = c("F1",'F2','F3','F4'),
  Year = 2020,
  Cell = "Fake",
  lon  = c(-82.42, -82.386, -82.415, -82.415),
  lat  =  c(42.36, 42.36, 42.36, 42.38)
)
sites.edit <- rbind(sites, Fakesite)

# convert to spatial frame
sites.sf <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
sites.sf.fake <- st_as_sf(sites.edit, coords = c("lon", "lat"), crs = 4326)

sites.sf <- st_transform(sites.sf, 26917)
sites.sf.fake <- st_transform(sites.sf.fake, 26917)

# get background image for map
img <- get_tiles(sites.sf.fake, provider = "Esri.WorldImagery", zoom = 16, 
                 crop=TRUE)
plot(img)

# inset map of Ontario
locator <- ggplot() +
  geom_sf(data = ontario, fill = "grey90") +
  geom_point(aes(x = -82.35, y = 42.45), colour = "orange", 
             shape=15, size = 2)+
  coord_sf(xlim = c(-96, -74),ylim = c(41, 57)) +
  annotate("text", label="Ontario", x=-88,y=52)+
  theme_void() +
  theme(panel.background = element_blank())
locator

# main sampling map
main_map <- ggplot() +
  geom_spatraster_rgb(data = img) +
  geom_sf(data = sites.sf, aes(color=Year, shape=Cell), size = 2)+
  scale_color_manual(values=c("gold","red"))+
  theme(legend.title = element_blank()) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.001))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001))+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering,
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.8, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.25,
                   pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm")) +
  coord_sf() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'top',
        legend.margin = margin(0,0,0,0))

#export
#tiff("Results/Figures/Map.tiff", height=5, width=6, units='in', res=800)
ggdraw() +
  draw_plot(main_map) +
  draw_plot(locator, x = 0.12, y = 0.59, width = 0.28, height = 0.28)
#dev.off()