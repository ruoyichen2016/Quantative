rm(list=ls()) 
#set wd
options(scipen=999) 
options(max.print=100000)

install.packages("readr")
install.packages("ggplot2")
install.packages("translateR")
install.packages("stringr")
install.packages("splitstackshape")
install.packages("cowplot")
install.packages("dplyr")
install.packages('Jmisc')
install.packages('psych')
install.packages('qwraps2')
install.packages("multiwayvcov")
install.packages("fastDummies")
install.packages("dplyr")

library(translateR);library(readr);library(ggplot2);library(stringr);library(plyr);
library(splitstackshape);library(data.table);library(dplyr);library(cowplot);library(Jmisc);library(psych);library(qwraps2);library(xtable);library(sandwich);library(stargazer);library(lmtest);library(ggpubr);library(multiwayvcov);library(reshape)

##############################################################
#Transfer data (at/before registration). Include Zero FIRs.
data <- read.csv("data3_1.csv")
#removing all empty rows
data <- data[!is.na(data$Sr.No),]

#no. of transfers to mahila thanas or all-women stations
summary(data$TRANSFERED_PS) #2236/8002
data <- cSplit(data, "FIR_No", sep="/")

#awps 
data$awps <-  ifelse(grepl("MAHILA", data$TRANSFERED_PS), "awps", 'other')

##mahila thanas subset
data2 <- data[with(data, grepl("MAHILA", data$TRANSFERED_PS)),]


####################
#If registered_district and Transfered_district are same (zonal)
#If registered_district and Transfered_district are different (district)
data$REGISTERED_DISTRICT <- as.character(data$REGISTERED_DISTRICT)
data$TRANFERED_DISTRICT <- as.character(data$TRANSFERED_DISTRICT)


data$issue <- ifelse(grepl("MAHILA", data$TRANSFERED_PS), "mahila_trans", 
                     ifelse(data$TRANSFERED_DISTRICT==data$REGISTERED_DISTRICT, "zonal_trans",
                            "district_trans"))

prop.table(table(data$issue))

#police stations TRANSFERRED TO
data$ps_transfered <- paste(data$TRANSFERED_PS, data$TRANSFERED_DISTRICT)

#police stations FORWARDED FROM
data$ps_forwarded <- paste(data$REGISTERED_PS, data$REGISTERED_DISTRICT)


#Top 10 ps transferred
table <- data %>%
  group_by(ps_transfered) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) 
ps_table <- arrange(table, desc(prop))#total of 1166 police stations
ps_table <- ps_table[1:20,]

ggplot(data = ps_table, aes(x = ps_transfered, y = count)) + 
  geom_bar(stat="identity") + coord_flip() + xlab("Police Station/District") + ggtitle("Top 20 of 1166 Stations That Crimes Transferred To (Uttar Pradesh)") + theme(plot.title = element_text(size = 20, hjust=0.5))


#Top 10 ps that did the forwarding
table <- data %>%
  group_by(ps_forwarded) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) 
ps_table <- arrange(table, desc(prop))#total of 1166 police stations
ps_table <- ps_table[1:20,]

ggplot(data = ps_table, aes(x = ps_forwarded, y = count)) + 
  geom_bar(stat="identity") + coord_flip() + theme_minimal() + xlab("Police Station/District") + ggtitle("Top 20 Stations That Forwarded")


########
##unique ps_transfered
unique(data$ps_transfered)#1166

##unique ps_forwarded?
unique(data$ps_forwarded)#897


##Combine all the police stations to make distinct list
names(data)
stations <- data[, 11:12]

#"unlisting" and appending each column 
stations <- data.frame(a=unlist(stations, use.names = FALSE))#

#distinct stations
list <- stations %>%
  group_by(a) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) 
list
list <- arrange(list, desc(prop))#sort by descending order
list

list <- list[,1]


##get longitudes/latitudes of each station
library(ggmap)

##add locations of police stations in UP 
pslocations_up <- read_csv("data3_2.csv")

####MERGE with ps_transfered and ps_forwarded 
#clean relevant variables in both datasets' columns to ensure no leading/lagging spaces
data$ps_forwarded <- str_squish(data$ps_forwarded)
data$ps_transfered <- str_squish(data$ps_transfered)
pslocations_up$a <- str_squish(pslocations_up$a)

##merge
data2 <- left_join(data,pslocations_up,by=c("ps_forwarded"="a"))
data2 <- left_join(data2,pslocations_up,by=c("ps_transfered"="a"))
names(data2)


##calculate distance between them
data2 <- cSplit(data2, "final.x", sep=",")
data2 <- cSplit(data2, "final.y", sep=",")

library(geosphere)
data2$distance<-distHaversine(data2[,14:13], data2[,16:15])
data2$distance <- data2$distance*0.001

##box-plots
data2$issue <- as.factor(data2$issue)

##within-district transfers
within <- data2 [ ! with(data2, grepl("district_trans", data2$issue)),]

plot1 <- ggplot(within, aes(x=issue, y=distance)) + 
  geom_boxplot(outlier.colour = NA, notch = TRUE) + stat_summary(fun.y=mean, geom="point", size=4) + theme(legend.position = "none") +
  scale_x_discrete(name = "Intra-District", labels=c("mahila_trans" = "AWPS (N=2237)", "zonal_trans" = "Non-AWPS (N=3260)")) + scale_y_continuous(breaks = seq(0, 65, 5), limits=c(0, 65)) + geom_point(aes(fill=issue), size=0.5, shape=21, colour="grey20", alpha=0.2,
                                                                                                                       position=position_jitter(width=0.3, height=0.2)) + ggtitle("Complainants Forwarded to Another UP Station (2015-2017) & Distance (Km)") + theme(plot.title = element_text(size = 20, hjust=0.5))
plot1   



#####Heat maps of registered and forwarded police stations
##Utilizing the longitude and latitudes for an interactive map
names(data2)
transfered <- select(data2, ps_transfered, final.y_1, final.y_2)
transfered <- na.omit(transfered)


##insert your ggmap key
register_google(key = "")

#check if key is saved
has_google_key()


bwmap <- get_googlemap(center = c(80.299931, 26.456291), zoom = 7,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:on")
ggmap(bwmap) +
  geom_point(data=transfered, aes(x=final.y_2, y=final.y_1) , color="darkblue", size=1) + theme_void()



bwmap2 <- get_googlemap(center = c(80.299931, 27.456291), zoom = 7,
                       color = "bw",
                       key = "",
                       style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:on")
ggmap(bwmap2) +
  geom_point(data=transfered, aes(x=final.y_2, y=final.y_1) , color="darkblue", size=1) + theme_void()



##Creating a heat map
#First creating new column that gives the count of a variable I want to show in heat.
heat <- transfered %>% 
  group_by(ps_transfered) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#Now merging the new count/heat dataframe with the longs/lats
heat2 <- left_join(heat, pslocations_up, by=c("ps_transfered"="a"))

#split "final"
heat2 <- cSplit(heat2, "final", sep=",")

#subsetting the AWPS from gender_heat2, so I can add a different color
awps_heat <- heat2[with(heat2, grepl("MAHILA", heat2$ps_transfered)),]

##ggplotting 
plot <- ggmap(bwmap) +
  geom_point(data = heat2,
             aes(x = final_2, y = final_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_heat,
             aes(x = final_2, y = final_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(title = "",
       size = "Count",
       subtitle = "",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void()
plot 


plot <- ggmap(bwmap2) +
  geom_point(data = heat2,
             aes(x = final_2, y = final_1, size = count, color = "SPS"), 
             alpha = 0.4) +
  geom_point(data = awps_heat,
             aes(x = final_2, y = final_1, size = count, color = "AWPS"),
             alpha = 0.5) +
  scale_color_manual(values = c(SPS = "darkblue", AWPS = "red")) +
  scale_size_area(max_size = 14) +
  labs(title = "",
       size = "Count",
       subtitle = "",
       color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 6))) + theme_void()
plot 



####flow map
plot


delete <- within [ ! with(within, grepl("zonal_trans", within$issue)),]

###
save <- plot + geom_segment(data= delete, aes(x=final.y_2,y=final.y_1, xend=final.x_2, yend=final.x_1, 
                                      group = issue), 
                    lineend = "round") + ggtitle("Stations Complainants Forwarded To (Uttar Pradesh)") + theme(plot.title = element_text(size = 15, face = "bold", hjust=0.5))
save




####
#Justifications for transfers (midway through investigation)

data <- read.csv("data3_3.csv")
#select crimes 395-402 to illustrate reasons for transfer