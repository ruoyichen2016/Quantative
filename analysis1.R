rm(list=ls()) 
#insert wd
options(scipen=999)
options(max.print=100000)

install.packages("readr")
install.packages("ggplot2")
install.packages("translateR")
install.packages("stringr")
install.packages("splitstackshape")
install.packages("ggmap")
install.packages("sf")
install.packages("mapview")
install.packages("cowplot")

library(translateR);library(readr);library(ggplot2);library(stringr);library(plyr);library(cowplot);
library(splitstackshape);library(data.table);library(dplyr);library(sf);library(mapview);library(ggmap);library(ggpubr)


#insert your ggmap key
register_google(key = "")
#check if key is saved
has_google_key()

##AWPS locations
pslocations <- read_csv("data1_1.csv")

##Utilizing the longitude and latitudes for an interactive map
locations <- st_as_sf(pslocations, coords = c("locations.lon", "locations.lat"), crs=4326)
mapview(locations)#creates interactive map 

#Center 
bwmap <- get_googlemap(center = c(82.111552, 22.070422), zoom = 5,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:on")
##plot
awps_map <- ggmap(bwmap) +
  geom_point(data=pslocations, aes(x=locations.lon, y=locations.lat) , color="darkblue", size=1) + 
  theme_void()
awps_map




###Bihar
gender_crimes <- read_csv("data1_2.csv")
locations <- select(gender_crimes, locs, ps_loc_1, ps_loc_2)

##count of a variable to show in heat
gender_heat <- gender_crimes %>% 
  group_by(locs) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#merging the new count/heat dataframe with the longs/lats
gender_heat2 <- join(x=gender_heat,y=locations,by="locs",type='left')
gender_heat2 <- gender_heat2[!duplicated(gender_heat2$locs), ]
gender_heat2 <- gender_heat2[complete.cases(gender_heat2), ]


#subsetting AWPS from gender_heat2
awps_gender_heat <- gender_heat2[with(gender_heat2, grepl("Mahila", gender_heat2$locs)),]

#map frame 
bwmap <- get_googlemap(center = c(85.778542, 25.957424), zoom = 8,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:on")


##Ggplotting gender/awps heat map
map1 <- ggmap(bwmap) +
  geom_point(data = gender_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_gender_heat,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void() + ggtitle("SPS = 84928 / AWPS = 2402") + theme(plot.title = element_text(size = 20, hjust = 0.5))
map1



##Remove all crimes related to 498
nondowry_crimes <- gender_crimes [ ! with(gender_crimes, grepl(1, gender_crimes$dowry)),]
nondowry_awps_no <- nondowry_crimes[with(nondowry_crimes, grepl(1, nondowry_crimes$awps)),]


##Creating a "non-dowry gender" heat map
nondowry_heat <- nondowry_crimes %>% 
  group_by(locs) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#Merging the new count/heat dataframe with the longs/lats
nondowry_heat2 <- join(x=nondowry_heat,y=locations,by="locs",type='left')
nondowry_heat2 <- nondowry_heat2[!duplicated(nondowry_heat2$locs), ]
nondowry_heat2 <- nondowry_heat2[complete.cases(nondowry_heat2), ]

#subsetting the AWPS, so I can add a different color
nondowry_awps <- nondowry_heat2[with(nondowry_heat2, grepl("Mahila", nondowry_heat2$locs)),]

##Ggplotting nondowry crimes/awps non-dowry heat map
map2 <- ggmap(bwmap) +
  geom_point(data = nondowry_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = nondowry_awps,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 10) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void()+ ggtitle("SPS = 74912 / AWPS = 807 (*IPC Section 498-A)") + theme(plot.title = element_text(size = 20, hjust = 0.5))
map2 
map2 + labs(x=NULL, y=NULL)


#cowplotting the two maps
ggarrange(map1, map2, labels = c("A)","B)"))




###Uttar Pradesh
#Map frame
bwmap <- get_googlemap(center = c(80.299931, 26.456291), zoom = 7,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:on")



#load data
gender_crimes <- read_csv("data1_3.csv")
locations <- select(gender_crimes, locs, ps_loc_1, ps_loc_2)


#count of var
gender_heat <- gender_crimes %>% 
  group_by(locs) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#merging the new count/heat dataframe with the longs/lats
gender_heat2 <- join(x=gender_heat,y=locations,by="locs",type='left')
gender_heat2 <- gender_heat2[!duplicated(gender_heat2$locs), ]
gender_heat2 <- gender_heat2[complete.cases(gender_heat2), ]


#subsetting the AWPS from gender_heat2, so I can add a different color
awps_gender_heat <- gender_heat2[with(gender_heat2, grepl("महिला", gender_heat2$locs)),]

##Ggplotting gender/awps heat map
map1 <- ggmap(bwmap) +
  geom_point(data = gender_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_gender_heat,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void() + ggtitle("SPS = 2123 / AWPS = 253") + theme(plot.title = element_text(size = 20, hjust=0.5))
map1


#############

##Remove all crimes related to 498
nondowry_crimes <- gender_crimes [ ! with(gender_crimes, grepl(1, gender_crimes$dowry)),]
nondowry_awps_no <- nondowry_crimes[with(nondowry_crimes, grepl(1, nondowry_crimes$awps)),]
#there are 0 non-498A crimes in UP in AWPS, the remaining are 498 only.

##Creating a "non-dowry gender" heat map
nondowry_heat <- nondowry_crimes %>% 
  group_by(locs) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))



nondowry_heat2 <- join(x=nondowry_heat,y=locations,by="locs",type='left')
nondowry_heat2 <- nondowry_heat2[!duplicated(nondowry_heat2$locs), ]
nondowry_heat2 <- nondowry_heat2[complete.cases(nondowry_heat2), ]#removing NA's


##Ggplotting nondowry crimes/awps non-dowry heat map
map2 <- ggmap(bwmap) +
  geom_point(data = nondowry_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  scale_color_manual(values = c(SPS = "darkblue")) +
  scale_size_area(max_size = 10) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void() + ggtitle("SPS = 1625 / AWPS = 0 (*IPC Section 498-A)") + theme(plot.title = element_text(size = 20, hjust=0.5))
map2
map2 + labs(x=NULL, y=NULL)



#cowplotting the two maps
ggarrange(map1, map2, labels = c("A)","B)"))




##############
#Haryana

#load
data <- read_csv("data1_4.csv")
##Subset all data that includes AWPS
data <- data[with(data, grepl("WOMEN|Women", data$station)),]

##make a table for when awps opened (using aggregate function)
aggregate(system ~ station, data, function(x) min(x))
opening <- aggregate(system ~ station + district, data, function(x) min(x)) #aggregating (or arranging to get min date of each station)
opening <- arrange(opening, system)#arranging by date
opening

#now the actual reform (remove Sonipat)
#let's remove that extra week in dataset so monthly graph is neater
data <- subset(data, system < "2017-07-31")
reform <- data[ ! with(data, grepl("SONIPAT", data$district)),]
reform <- count(reform, Month_Yr, district)
reform <- reform %>% group_by(district) %>% mutate(avg = mean(n))

plot2 <- ggplot(data = reform, aes(Month_Yr, n)) +
  geom_line(color = "steelblue", group=1) + labs(title = "AWPS FIR Count (August 2015 Reform Stations)", subtitle = "", y = "Count by Month", x = "") + 
  facet_wrap(~ district) +
  theme(axis.text.x = element_text(colour="grey20", size=9, angle=45, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=16, family="Arial"))

plot2


###Making maps
gender_crimes <- read_csv("data1_4.csv")
pslocations2 <- select(gender_crimes, locs, ps_loc_1, ps_loc_2)


##Creating a "gender crimes" heat map
#First creating new column that gives the count of a variable I want to show in heat.
gender_heat <- gender_crimes %>% 
  dplyr::group_by(locs) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))


#Now merging the new count/heat dataframe with the longs/lats
gender_heat2 <- join(gender_heat, pslocations2, type='left')
gender_heat2 <- gender_heat2[!duplicated(gender_heat2$locs), ]
gender_heat2 <- gender_heat2[complete.cases(gender_heat2), ]




#subsetting the AWPS from gender_heat2, so I can add a different color
awps_gender_heat <- gender_heat2[with(gender_heat2, grepl("WOMEN|Women", gender_heat2$locs)),]

sum(awps_gender_heat$count, na.rm=T)

#I take the center as Jind. 
bwmap <- get_googlemap(center = c(76.343796, 29.326937), zoom = 8,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:on")


##Ggplotting gender/awps heat map
map0 <- ggmap(bwmap) +
  geom_point(data = gender_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_gender_heat,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(title = "",
       size = "Count",
       subtitle = "SPS = 19339 / AWPS = 5006",
       color = NULL) + ggtitle("Gendered FIRs at SPS & AWPS (Haryana): 2015-2017") +
  guides(color = guide_legend(override.aes = list(size = 3))) + theme_void()
map0 <- map0 + theme(plot.title = element_text(size = 20, hjust=0.5, face = "bold")) 
map0


###looking at week before august 28 and week after
aug_pre <- subset(gender_crimes, system <= "2015-08-27")
aug_pre <- subset(aug_pre, system >= "2015-08-20")
unique(aug_pre$system)

aug_post <- subset(gender_crimes, system >= "2015-08-28")
aug_post <- subset(aug_post, system <= "2015-09-04")
unique(aug_post$system)


##Creating a "gender crimes" heat map
#First creating new column that gives the count of a variable I want to show in heat.
gender_heat <- aug_pre %>% 
  dplyr::group_by(locs) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

#Now merging the new count/heat dataframe with the longs/lats
gender_heat2 <- join(gender_heat, pslocations2, type='left')
gender_heat2 <- gender_heat2[!duplicated(gender_heat2$locs), ]
gender_heat2 <- gender_heat2[complete.cases(gender_heat2), ]


#subsetting the AWPS from gender_heat2, so I can add a different color
awps_gender_heat <- gender_heat2[with(gender_heat2, grepl("WOMEN|Women", gender_heat2$locs)),]


##Ggplotting gender/awps heat map
map1 <- ggmap(bwmap) +
  geom_point(data = gender_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_gender_heat,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 5))) + theme_void() + scale_size(range=c(2,20),breaks=c(3,5,7,9,11),labels=c("3","5","7","9","11"),guide="legend") +
  ggtitle("SPS = 268 / AWPS = 3 (Week Before AWPS)") + theme(plot.title = element_text(size = 20, hjust=0.5))
map1



##now post
#First creating new column that gives the count of a variable I want to show in heat.
gender_heat <- aug_post %>% 
  dplyr::group_by(locs) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))
#Now merging the new count/heat dataframe with the longs/lats
gender_heat2 <- join(gender_heat, pslocations2, type='left')
gender_heat2 <- gender_heat2[!duplicated(gender_heat2$locs), ]
gender_heat2 <- gender_heat2[complete.cases(gender_heat2), ]



#subsetting the AWPS from gender_heat2, so I can add a different color
awps_gender_heat <- gender_heat2[with(gender_heat2, grepl("WOMEN|Women", gender_heat2$locs)),]
sum(awps_gender_heat$count, na.rm = T) 



##Ggplotting gender/awps heat map
map2 <- ggmap(bwmap) +
  geom_point(data = gender_heat2,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_gender_heat,
             aes(x = ps_loc_2, y = ps_loc_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(size = "Count",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 5))) + theme_void() + scale_size(range=c(2,20),breaks=c(3,5,7,9,11),labels=c("3","5","7","9","11"),guide="legend") +
  ggtitle("SPS = 180 / AWPS = 84 (Week After AWPS)") + theme(plot.title = element_text(size = 20, hjust=0.5))
map2


##nested plot_grid
ggarrange(map1, map2, labels = c("A)","B)"))












