#Let's check out some rotifer data
library(tidyverse)
library(lubridate)
library(zooper)
library(DroughtData)
library(sf)
library(ggbeeswarm)
library(deltamapr)
library(ggmap)

rotifers = Zoopsynther(Data_type = "Community",
                       Sources = "EMP",
                       Size_class = "Micro")
rotifers = filter(rotifers, Phylum == "Rotifera")

unique(rotifers$Taxname)

#I'm going to squich Synchaeta bicornis with the rest of Syncheata
#plus organize date info a bit
rotifers = mutate(rotifers, WY = case_when(month(Date) %in% c(10,11,12) ~ Year +1,
                                           TRUE ~ Year),
                  Day = yday(Date),
                  Month = case_when(month(Date) %in% c(10,11,12) ~ month(Date-9),
                                    TRUE ~ month(Date)+3),
                  Taxname = case_when(Taxname == "Synchaeta bicornis" ~ "Synchaeta_UnID",
                                      TRUE ~ Taxname)) %>%
  group_by(WY, Taxname, SampleID, Station, SalSurf, Month, Date,
           Day, Latitude, Longitude) %>%
  summarize(CPUE = sum(CPUE, na.rm = T))

#quik plot
ggplot(rotifers, aes(x= WY, y = CPUE, fill = Taxname))+
  geom_col()

#Sampling effort changed over time, average woudl be better

rot = group_by(rotifers, WY, Taxname) %>%
  summarize(CPUE = mean(CPUE, na.rm = T), Salinity = mean(SalSurf, na.rm = T)) %>%
  mutate(Rotifer = factor(Taxname, levels = c("Asplanchna_UnID", "Keratella_UnID", 
                                              "Polyarthra_UnID", "Synchaeta_UnID",
                                              "Trichocerca_UnID", "Rotifera_UnID"),
                          labels = c("Asplanchna sp.", "Keratella sp.", "Polyarthra sp.",
                                     "Synchaeta sp.", "Trichocerca sp.", "Other Rotifer")))
ggplot(rot, aes(x= WY, y = CPUE, fill = Rotifer))+
  geom_col()+ theme_bw()+ scale_fill_brewer(palette = "Dark2")

##
#salinity time series
Salin =  select(ungroup(rotifers), WY, SalSurf, SampleID) %>%
  distinct() %>%
  group_by(WY) %>%
  summarise(Salinity = mean(SalSurf, na.rm = T))

ggplot(Salin, aes(x= WY, y = Salinity))+
  geom_col()+ theme_bw()

###
ggplot(rot, aes(x= Salinity, y = CPUE, color = Rotifer))+
  geom_point()+ theme_bw()+ scale_fill_brewer(palette = "Dark2")+
  facet_wrap(~Rotifer) + scale_y_log10()+ geom_smooth(method = "lm")


ggplot(rot, aes(x= WY, y = CPUE/1000, fill = Rotifer))+
  geom_col()+ theme_bw()+ 
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  facet_wrap(~Rotifer, nrow = 3)+ ylab("Catch per unit effort (number/1000m3)")+ xlab("Water Year")+
  theme(legend.position = "top")

ggsave("figures/RotiferTS.png", width = 5, height = 5, units = "in")

ggplot(rot, aes(x= WY, y = CPUE, fill = Taxname))+
  geom_col(position = "fill")

#What the heck was going on int he 70s? Plus it looks like there is a big bump in wet yers

yearassignments <- read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/DroughtSynthesis/data/yearassignments.csv")
yrs = rename(yearassignments, WY = Year)

rot2 = left_join(rot, yrs) %>%
  filter(WY > 1980)

ggplot(rot2, aes(x = Index, y = CPUE, color = Taxname))+
  geom_point()+ geom_smooth()


labels = data.frame(Yr_type = c("Critical", "Dry", "Below Normal", "Above Normal",
                                "Wet"), Lables = c("C", "D", "B", "A", "W"))

rot2 = left_join(rot2, labels)

#year type by species
ggplot(rot2, aes(x = WY, y = CPUE/1000, fill = Yr_type))+
  
  geom_col()+facet_wrap(~Rotifer, scales = "free_y", nrow = 6)+
  drt_color_pal_yrtype()+ theme_bw() +
  geom_text(aes(label = Lables), y = -.01, size = 3)+
  theme(legend.position = "top") + ylab("Catch per unit effort (number/1000m3)")+
  xlab("Water Year")

ggsave("figures/RotiferYrtype.png", width = 6, height = 10, units = "in")

#calculate total rotifers
tots = group_by(rotifers, WY, SampleID) %>%
  summarize(CPUE = sum(CPUE)) %>%
  left_join(yrs) %>%
  group_by(Index, WY, Yr_type) %>%
  summarize(sdCPUE = sd(CPUE), CPUE = mean(CPUE))

#graph total abundance
ggplot(tots, aes(x = WY, y = CPUE/1000))+ geom_area(fill = "darkgreen")+
  geom_area(aes(y= CPUE/1000+ sdCPUE/1000), alpha = 0.5, fill = "darkseagreen")+
  theme_bw() + ylab("Catch per 1000 m3") + xlab("Year")

ggsave("RotiferTimeSeries.png", device = "png",
       width = 6, height = 4, units = "in")

pal_yrtype2 <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal"="lightblue",  "Wet" = "#481F70FF") 
shapes = pal_yrtype2 <- c( "Critical" = 3, "Dry" = 16, "Below Normal" = 6,"Above Normal"=8,  "Wet" = 11) 


# 

#total abundance versus WY
ggplot(filter(tots, WY > 1980), aes(x = Index, y = log(CPUE)))+
  geom_point(aes(color = Yr_type, shape = Yr_type))+ geom_smooth(method = "lm")+ 
  scale_color_manual(values = pal_yrtype2, name = NULL)+
  scale_shape_manual(values = shapes, name = NULL)+
  theme_bw()+ ylab("Catch per unit effort (log-transformed)")+ xlab("Water Year Index")

ggsave("figures/RotiferOutflow.png", width = 6, height = 4, units = "in")

#Asplanchna is pretty rare, is it salinity linked?
asp = filter(rotifers, Taxname == "Asplanchna_UnID", WY > 1980)
ggplot(asp, aes(x = SalSurf, y = CPUE)) + geom_point()
ggplot(asp, aes(x = Day, y = CPUE, color = WY)) + geom_point()

#check out salinity for all species
rotifersP = filter(rotifers, CPUE != 0)
ggplot(rotifersP, aes(x = Taxname, y = SalSurf)) + geom_boxplot()
ggplot(rotifersP, aes(x = Taxname, y = SalSurf)) + geom_quasirandom()

#Maybe look regionally?
load("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/DroughtSynthesis/DroughtRegions.RData")
regions = st_transform(Regions, crs = 4326) %>%
  st_make_valid()
rotsf = st_as_sf(filter(rotifers, !is.na(Latitude)), 
                 coords = c("Longitude", "Latitude"),
                 crs = 4326)
rotsf = st_join(rotsf, regions) 

rot4 = group_by(rotsf, WY, Region, Taxname) %>%
  summarize(CPUE = mean(CPUE))

ggplot(rotsf, aes(x = Date, y = CPUE)) +
  facet_wrap(~Region)+ geom_point()

ggplot(rot4, aes(x = WY, y = CPUE, fill = Taxname)) +
  facet_wrap(~Region)+ geom_col()

ggplot(rotsf, aes(x = Day, y = CPUE, color = Taxname)) +
  facet_wrap(~Region)+ geom_point()

#ok, let's see if this works:
# Library
#devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)



# Stream graph with a legend
pp <- streamgraph(rot, key="Taxname", value="CPUE", date="WY", 
                  height="300px", width="1000px") %>%
  sg_legend(show=TRUE, label="names: ")

pp

streamgraph(rot, key="Taxname", value="CPUE", date="WY", 
            height="300px", width="1000px", interactive = F) 


ppt <- streamgraph(tots, key =   value="CPUE", date="WY", 
                   height="300px", width="1000px") 

ppt
# save the widget
# library(htmlwidgets)
# saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/streamgraphDropdown.html"))

#would it look better by month?

rot3 = rotifers %>%
  group_by(Month, WY, Taxname) %>%
  summarize(CPUE = mean(CPUE))%>%
  mutate(Ymonth = WY + (Month-1)/12)

pp2 <- streamgraph(rot3, key="Taxname", value="CPUE", date="Ymonth", 
                   height="300px", width="1000px", scale = "continuous") %>%
  sg_legend(show=TRUE, label="names: ")

pp2
#meh,doing it by year is better

#Can I put pie charts on a map?

ggplot() + geom_sf(data = WW_Delta)+
  geom_sf(data = filter(rotsf, WY == 2017), 
          aes(size = CPUE, color = Taxname))

make_pie <- function(dt){
  ggplot() +
    geom_bar(data = dt,
             aes(x = "", y = CPUE, fill = Taxname),
             stat = "identity", width = 1) +
    scale_fill_brewer(palette = "Dark2", guide = NULL)+
    coord_polar("y") +
    theme_void() 
}

rot2020 = filter(rotsf, WY == 2017) %>%
  filter(!Station %in% c("NZEZ2SJR", "NZEZ6SJR", "NZEZ2", "NZEZ6")) %>%
  group_by(Station, Taxname) %>%
  summarize(CPUE = mean(CPUE))

rot2020tot = group_by(rot2020, Station) %>%
  summarize(size = sum(CPUE)/5000)

pies = rot2020 %>%
  split(.$Station) %>%
  map(make_pie)

leg <- get_legend(make_pie(filter(rot2020, Station == "NZ002")))

#dataframe of locations
stations = group_by(rotifers, Station) %>%
  filter(WY == 2017, !Station %in% c("NZEZ2SJR", "NZEZ6SJR", "NZEZ2", "NZEZ6")) %>%
  summarize(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
  filter(!is.na(Latitude))


#UGH
library(leaflet)
library(leaflet.minicharts)

rot2020wide = pivot_wider(st_drop_geometry(rot2020), 
                          names_from = Taxname, values_from = CPUE) %>%
  ungroup() %>%
  mutate(Station = NULL)

leaflet(data = WW_Delta)%>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  #addLegend("topright",  val, opacity=1)%>%
  addMinicharts(lng = stations$Longitude, 
                lat = stations$Latitude,
                type = "pie",
                chartdata = rot2020wide, width = rot2020tot$size)
