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

library(translateR);library(readr);library(ggplot2);library(stringr);library(plyr);
library(splitstackshape);library(data.table);library(dplyr);library(cowplot);library(Jmisc);library(psych);library(qwraps2);library(xtable);library(sandwich);library(stargazer);library(lmtest);library(ggpubr);library(multiwayvcov);library(reshape)
library(broom);library(purrr)
library(Hmisc)

#universe gendered crime Haryana
data <- read_csv("data2_1.csv")

#data2 will be everything aside from AWPS
##Rename Sonipat's AWPS to include.
data$station <- revalue(data$station, c("WOMEN POLICE STATION"="AWPS Sonipat", "WOMEN PS KHANPUR KALAN"="AWPS Sonipat2"))
data2 <- data[ ! with(data, grepl("WOMEN|Women", data$station)),]

##crimes per day
scatter <- as.data.frame(table(data$system))
scatter$Var1 <- as.numeric(scatter$Var1)
levels <- ggplot(scatter, aes(Var1,Freq)) + geom_point() + geom_smooth() + 
  xlab("Days Since January 1 2015") + ylab("Daily No. of FIRs") + ggtitle("No. of Gendered Crimes Per Day Haryana 2015-2017")
levels

##pre/post intervention 
scatter <- data %>% 
  group_by(system) %>% 
  summarise(count = n()) 
scatter$inter <- ifelse(scatter$system >= '2015-08-28', 1, 0)
scatter$group <- ifelse(scatter$inter == 0, "pre", "post")
scatter$days <- 1:nrow(scatter) #add a column for days

plot1 <- ggplot(scatter, aes(days,count)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("FIRs Per Day") +  ggtitle("FIR Count") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + labs(subtitle = "Total = 24,345 (Stations = 285)")
plot1

#without AWPS 
scatter2 <- data2 %>% 
  group_by(system) %>% 
  summarise(count = n()) 
scatter2$inter <- ifelse(scatter2$system >= '2015-08-28', 1, 0)
scatter2$group <- ifelse(scatter2$inter == 0, "pre", "post")
scatter2$days <- 1:nrow(scatter2) #add a column for days

plot2 <- ggplot(scatter2, aes(days,count)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("FIRs Per Day") +  ggtitle("FIR Count (SPS)") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + labs(subtitle = "Total = 19,630 (Stations = 265)")
plot2

#crime rate (based on 2011 census with no adjustment for yearly increase in population)
scatter$rate <- (scatter$count/25351462)*100000

plot3 <- ggplot(scatter, aes(days,rate)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("Gendered (/Total Pop)") +  ggtitle("Gendered Rate") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("")
plot3

#and without AWPS
scatter2$rate <- (scatter2$count/25351462)*100000

plot4 <- ggplot(scatter2, aes(days,rate)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("Gendered (/Total Pop)") +  ggtitle("Gendered Rate (SPS)") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("")
plot4

##Mean rate before and after
subset_pre <- subset(scatter2, scatter2$group=="pre")
subset_post <- subset(scatter2, scatter2$group=="post")
t.test(subset_pre$rate, subset_post$rate)

############
##total2 represents all non-gendered crimes in Haryana from 2015-2018
total_scatter <- read_csv("data2_2.csv")


### add it to scatter 
scatter <- data %>% 
  group_by(system) %>% 
  summarise(count = n()) 

#rename count so I can merge it with total_scatter
names(scatter) <- c("system", "gend_count")

#join
new_scatter <- join(scatter, total_scatter, type='left')

##total crimes per day
new_scatter$full <- new_scatter$gend_count + new_scatter$count

new_scatter$days <- 1:nrow(new_scatter) #add a column for days

all_plot <- ggplot(new_scatter, aes(days,full)) + geom_point() + geom_smooth() + 
  xlab("Days Since January 1 2015") + ylab("FIRs Per Day") + ggtitle("No. of Crimes Per Day Haryana 2015-2017")
all_plot


#dummy pre/post intervention 
new_scatter$inter <- ifelse(new_scatter$system >= '2015-08-28', 1, 0)
new_scatter$group <- ifelse(new_scatter$inter == 0, "pre", "post")

##now Rate2 (gendered crimes divided by full for pre-post)
new_scatter$rate2 <- new_scatter$gend_count/new_scatter$full

plot5 <- ggplot(new_scatter, aes(days,rate2)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("Gendered (/Total Crime)") +  ggtitle("Proportion") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("")
plot5


##standard police stations only
scatter2 <- data2 %>% group_by(system) %>% summarise(count = n()) 

##and from new_scatter, let's get the "full"
names(scatter2) <- c("system", "gend_count_non_awps")

#let's join again
new_scatter <- join(new_scatter, scatter2, type='left')

##rate2 without awps
new_scatter$rate2_non_awps <- new_scatter$gend_count_non_awps/new_scatter$full

plot6 <- ggplot(new_scatter, aes(days,rate2_non_awps)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("Gendered (/Total Crime)") +  ggtitle("Proportion (SPS)") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("")
plot6


graph1 <- ggarrange(plot1, plot3, plot5, plot2, plot4, plot6, labels = c("A)","B)","C)","D)","E)","F)"), common.legend = TRUE, legend="bottom")
graph1 <- annotate_figure(graph1, bottom = "Days: 2015/01/01 - 2017/08/08")
graph1


###########################
data2 <- data[ ! with(data, grepl("WOMEN|Women", data$station)),]

#individual crimes (total and SPS)
test3 <- data[with(data, grepl(1, data$rape)),]
test4 <- data[with(data, grepl(1, data$dowry)),]
test6 <- data[with(data, grepl(1, data$pocso)),]

test3 <- test3 %>% group_by(system) %>% summarise(count = n())
test4 <- test4 %>% group_by(system) %>% summarise(count = n())
test6 <- test6 %>% group_by(system) %>% summarise(count = n())

test03 <- data2[with(data2, grepl(1, data2$rape)),]
test04 <- data2[with(data2, grepl(1, data2$dowry)),]
test06 <- data2[with(data2, grepl(1, data2$pocso)),]

test03 <- test03 %>% group_by(system) %>% summarise(count = n())
test04 <- test04 %>% group_by(system) %>% summarise(count = n())
test06 <- test06 %>% group_by(system) %>% summarise(count = n())

##renaming each of the columns for merging
names(test3)[2]<-"rape"
names(test4)[2]<-"dowry"
names(test6)[2]<-"pocso"

names(test03)[2]<-"rape_sps"
names(test04)[2]<-"dowry_sps"
names(test06)[2]<-"pocso_sps"
 
##now let's join all together. First make a list.
df_list <- list(new_scatter, test3, test03, test4, test04, test6, test06)
new_scatter <- merge_recurse(df_list)

###now add "0" for all na's.
new_scatter[is.na(new_scatter)] <- 0

##adding rates
new_scatter$rat3 <- (new_scatter$rape/25351462)*100000
new_scatter$rat4 <- (new_scatter$dowry/25351462)*100000
new_scatter$rat6 <- (new_scatter$pocso/25351462)*100000

new_scatter$rat03 <- (new_scatter$rape_sps/25351462)*100000
new_scatter$rat04 <- (new_scatter$dowry_sps/25351462)*100000
new_scatter$rat06 <- (new_scatter$pocso_sps/25351462)*100000


rape <- ggplot(new_scatter, aes(days,rat3)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Rape") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 3,353")
rape


rape_sps <- ggplot(new_scatter, aes(days,rat03)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Rape (SPS)") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 2,171")
rape_sps


dowry <- ggplot(new_scatter, aes(days,rat4)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Dowry Harassment") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 9,147")
dowry

dowry_sps <- ggplot(new_scatter, aes(days,rat04)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Dowry Harassment (SPS)") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 7,447")
dowry_sps


pocso <- ggplot(new_scatter, aes(days,rat6)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Child Sexual Assault") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 2,868")
pocso

pocso_sps <- ggplot(new_scatter, aes(days,rat06)) + geom_point(size = 0.01) + 
  geom_smooth(aes(colour=group)) + ylab("") +  ggtitle("Child Sexual Assault (SPS)") + theme(plot.title = element_text(size = 15, hjust=0.5)) +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + scale_color_brewer(palette = "Dark2") + labs(subtitle = "N = 1,942")
pocso_sps


graph11 <- ggarrange(dowry, rape, pocso, dowry_sps, rape_sps, pocso_sps, labels = c("A)","B)","C)","D)","E)","F)"), common.legend = TRUE, legend="bottom")
graph11 <- annotate_figure(graph11, bottom = "Days: 2015/01/01 - 2017/08/08")
graph11




##CHAPTER TWO (FIR STATUS)
####Case Status Records Merged With Crime Reports
##final_newdat
new_dat <- na.omit(data) 

#pre/post
new_dat$group <- ifelse(new_dat$system >= '2015-08-28', "post", "pre")


means1 <- as.data.frame(prop.table(table(new_dat$group, new_dat$status),1))
means1


plot2 <- ggplot(means1,aes(x=Var2,y=Freq,fill=factor(Var1)))+
  geom_bar(stat="identity",position="dodge")+
  xlab("") + 
  ylab("Percentage") + scale_fill_grey(start = .1, end = .6) +  
  labs(fill = "", caption = "") + geom_text(aes(label=round(Freq,2)),position = position_dodge(width=1), vjust=0, hjust=.5, size = 4) +
  theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5, angle = 35),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=16, family="Arial")) + ggtitle("Police Status Pre/Post")
plot2 <- plot2 + scale_x_discrete(labels=c("Cancelled" = "cancelled", "ChargeSheet" = "chargesheet", "Under Investigation" = "under investigation", "Untraced"="untraced")) + theme(plot.title = element_text(size = 15, hjust=0.5))
plot2


##other plot
perception_a <- new_dat %>% group_by(group) %>% count(group, status) %>% mutate(prop = prop.table(n))

library(REdaS)


fig1 <- ggplot(data=perception_a, aes(x=status, y=prop, fill=group)) + geom_bar(stat="identity", position=position_dodge()) 
fig1 <- fig1 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("Post","Pre"))
fig1 <- fig1 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.5)
fig1 <- fig1 + theme(plot.title = element_text(size = 15, hjust=0.5)) + ggtitle("Gendered Case Statuses in Haryana Criminal Justice System") 
fig1 <- fig1 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=12)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 
fig1 <- fig1 + labs(caption = "")
fig1 <- fig1 + scale_y_continuous(breaks=seq(0,0.6,0.05))
fig1


##with 200 day bandwidth around intervention 
bandwidth <- subset(new_dat, system < "2015-12-06")
bandwidth <- subset(bandwidth, system > "2015-05-20")


perception_b <- bandwidth %>% group_by(group) %>% count(group, status) %>% mutate(prop = prop.table(n))
perception_b



fig2 <- ggplot(data=perception_b, aes(x=status, y=prop, fill=group)) + geom_bar(stat="identity", position=position_dodge()) 
fig2 <- fig2 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("Post","Pre"))
fig2 <- fig2 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-2.5)
fig2 <- fig2 + theme(plot.title = element_text(size = 15, hjust=0.5)) + ggtitle("(200-Day Bandwidth)") 
fig2 <- fig2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=12)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 
fig2 <- fig2 + labs(caption = "")
fig2 <- fig2 + scale_y_continuous(breaks=seq(0,0.6,0.05))
fig2



###smaller bandwidth of 100 days
bandwidth2 <- subset(new_dat, system < "2015-10-17")
bandwidth2 <- subset(bandwidth2, system > "2015-07-09")


perception_c <- bandwidth2 %>% group_by(group) %>% count(group, status) %>% mutate(prop = prop.table(n))
perception_c


fig3 <- ggplot(data=perception_c, aes(x=status, y=prop, fill=group)) + geom_bar(stat="identity", position=position_dodge()) 
fig3 <- fig3 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("Post","Pre"))
fig3 <- fig3 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-2.5)
fig3 <- fig3 + theme(plot.title = element_text(size = 15, hjust=0.5)) + ggtitle("(100-Day Bandwidth)") 
fig3 <- fig3 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=12)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 
fig3 <- fig3 + labs(caption = "")
fig3 <- fig3 + scale_y_continuous(breaks=seq(0,0.6,0.05))
fig3


graph <- ggarrange(fig2, fig3, common.legend = TRUE, legend="bottom")
graph <- annotate_figure(graph, bottom = "", top = text_grob("Status of Gendered Crimes in Haryana Criminal Justice System", size = 20, face = "bold"))
graph

##dummy cols
library(fastDummies)
dum <- dummy_cols(new_dat, select_columns = "status")
names(dum)

#rename column
colnames(dum)[colnames(dum)=="status_Cancelled"] <- "cancelled"
colnames(dum)[colnames(dum)=="status_ChargeSheet"] <- "chargesheet"
colnames(dum)[colnames(dum)=="status_Untraced"] <- "untraced"
colnames(dum)[colnames(dum)=="status_Under Investigation"] <- "under_investigation"

##200 day bandwidth around intervention 
bandwidth <- subset(dum, system < "2015-12-06")
bandwidth <- subset(bandwidth, system > "2015-05-20")

#pre/post
pre <- subset(bandwidth, group == "pre")
post <- subset(bandwidth, group == "post")

##tests 
a1 <- t.test(post$cancelled, pre$cancelled)
a2 <- t.test(post$chargesheet, pre$chargesheet)

tab <- map_df(list(a1, a2), tidy)
tab

setnames(tab, old=c("estimate1","estimate2", "estimate"), new=c("post", "pre", "diff"))
tab$eval <- c("cancelled","chargesheet")
tab$eval <- factor(tab$eval, levels = tab$eval)

##ggplot 
plot2 <- ggplot(tab,aes(x=reorder(eval, desc(eval)), y=diff,ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + ggtitle("Diff. in Status: 200 Day Bandwidth") + xlab("") + 
  ylab("") + geom_hline(yintercept=0, linetype="dashed") + ylim(-0.07, 0.07)
plot2 <- plot2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5, angle = 35),
                       axis.text.y = element_text(colour="grey20", size=12))
plot2

###100 days
bandwidth2 <- subset(dum, system < "2015-10-17")
bandwidth2 <- subset(bandwidth2, system > "2015-07-09")


#pre/post
pre <- subset(bandwidth2, group == "pre")
post <- subset(bandwidth2, group == "post")


## t tests 
r1 <- t.test(post$cancelled, pre$cancelled)
r2 <- t.test(post$chargesheet, pre$chargesheet)

tab <- map_df(list(r1, r2), tidy)
tab

setnames(tab, old=c("estimate1","estimate2", "estimate"), new=c("post", "pre", "diff"))
tab$eval <- c("cancelled","chargesheet")
tab$eval <- factor(tab$eval, levels = tab$eval)

##ggplot  
plot3 <- ggplot(tab,aes(x=reorder(eval, desc(eval)), y=diff,ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + ggtitle("Diff. in Status: 100 Day Bandwidth") + xlab("") + 
  ylab("") + geom_hline(yintercept=0, linetype="dashed") + ylim(-0.07, 0.07)
plot3 <- plot3 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5, angle = 35),
                       axis.text.y = element_text(colour="grey20", size=12))
plot3


graph <- ggarrange(plot2, plot3, nrow = 1, ncol =2, common.legend = TRUE, legend="bottom")
graph <- annotate_figure(graph, top = text_grob("Status of Gendered Crimes in Haryana Criminal Justice System", size = 20, face = "bold"))
graph


#####Descriptives
#Nongendered crimes per day
total_scatter <- read_csv("data2_2.csv")

#how many crimes each day
a <- aggregate(sr_no ~ system, data, function(x) length(unique(x)))

#how many rape cases per day
d <- data[with(data, grepl(1, data$rape)),]
d <- aggregate(sr_no ~ system, d, function(x) length(unique(x)))

#how many dowry harrassment cases per day
f <- data[with(data, grepl(1, data$dowry)),]
f <- aggregate(sr_no ~ system, f, function(x) length(unique(x)))

##how many POCSO cases per day
g <- data[with(data, grepl(1, data$pocso)),]
g <- aggregate(sr_no ~ system, g, function(x) length(unique(x)))

##how many sexual harassment  cases per day
h <- data[with(data, grepl(1, data$harass)),]
h <- aggregate(sr_no ~ system, h, function(x) length(unique(x)))


descriptives <- list(a, total_scatter, d, f, g, h) %>% reduce(left_join, by = "system")
descriptives[is.na(descriptives)] <- 0

##rename columns
names(descriptives) <- c("system", "gendered", "nongendered", "rape", "dowry", "pocso", "harassment")


###Descriptives table
library("papeR")
test <- xtable(summarise(descriptives))
test
test <- test[,c(1,2,4,5)]
print(xtable(test), include.rownames=FALSE)



####CHAPTER THREE: CASES FOR WOMEN

##cases assigned to women and female complainants (excluding sensitive cases)
data <- read_csv("data2_3.csv")

#total number of unique Female Investigating Officers PER month
female_invest <- data[with(data, grepl("f", data$investoff_gend)),]

female_invest_month <- aggregate(index_names ~ Month_Yr, female_invest, function(x) length(unique(x)))
total_invest_month <- aggregate(index_names ~ Month_Yr, data, function(x) length(unique(x)))

female_invest_month$ratio <- female_invest_month$index_names/total_invest_month$index_names

female_invest_month <- cbind(female_invest_month, NewColumn="investigators per month")#add a separate column to make it easy to ggplot2
female_invest_month

plot <- ggplot(data=female_invest_month, aes(Month_Yr,ratio)) + 
  geom_point(aes(shape = factor(NewColumn)), size = 3)+
  geom_line(aes(group=NewColumn))+
  theme_bw() + xlab("Date") +
  ylab("Monthly Ratio of Active Female Investigators") + ggtitle("") +
  theme(legend.key = element_blank()) + 
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
plot 



subset_investoff <- subset(data, investoff_gend %in% c('f'))
io <- data.frame(table(subset_investoff$Month_Yr))
io <- cbind(io, NewColumn="investigating officer")
io

io <- io [ ! with(io, grepl("2017-08", io$Var1)),]
io

plot1 <- ggplot(data=io, aes(Var1,Freq)) + geom_point(aes(shape = factor(NewColumn)), size = 3) +
  geom_line(aes(group=NewColumn)) + ylab("Total Cases") + ggtitle("Cases Investigated by Policewomen") +
  theme(legend.key = element_blank()) + xlab("Month") +
  theme(legend.title = element_blank()) + theme(legend.text = element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot1 <- plot1 + theme(legend.position = "none") + labs(subtitle = "N = 3,923") + theme(plot.title = element_text(size = 20, hjust=0.5))
plot1


##time series
#cases investigated by women each day
scatter <- subset_investoff %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

#cases investigated by everyone per day 
scatter2 <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

colnames(scatter2)[2] <- "total"

#join data
cases <- join(scatter, scatter2, by = "system", type='full')


#add 0s
cases[is.na(cases)] <- 0

##
cases <- cases[order(as.Date(cases$system, format="%Y/%m/%d")),]

#percentage/rate
cases$perc <- cases$count/cases$total


#dummy pre/post intervention 
cases$inter <- ifelse(cases$system >= '2015-08-28', 1, 0)
cases$group <- ifelse(cases$inter == 0, "pre", "post")
cases$index <- 1:nrow(cases) #add a column for days


plot3 <- ggplot(cases, aes(index,perc)) + geom_point(size = 0.01) + 
  geom_smooth(se=TRUE, aes(colour=group)) + ylab("") +  ggtitle("Proportion of Cases Investigated by Policewomen") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + labs(subtitle = "") + theme(plot.title = element_text(size = 20, hjust=0.5))
plot3


plot_grid(plot1, plot3, ncol=2, nrow=1)

##############
#cases where women are victims
table(data$complainant_gender)##women are 23865 complainants

subset_victim <- subset(data, complainant_gender %in% c('female'))

#cases brought forward by women each day
scatter <- subset_victim %>% 
  group_by(system) %>% 
  dplyr::summarise(count = n()) 

#
scatter2 <- data %>% 
  group_by(system) %>% 
  dplyr::summarise(count = n()) 

colnames(scatter2)[2] <- "total"

#join data
cases <- join(scatter, scatter2, by = "system", type='full')


#add 0s
cases[is.na(cases)] <- 0

##Now need to sort dataframe because there are 0's at the bottom
cases <- cases[order(as.Date(cases$system, format="%Y/%m/%d")),]


#percentage/rate
cases$perc <- cases$count/cases$total


#now create a dummy pre/post intervention 
cases$inter <- ifelse(cases$system >= '2015-08-28', 1, 0)
cases$group <- ifelse(cases$inter == 0, "pre", "post")
cases$index <- 1:nrow(cases) #add a column for days


plot4 <- ggplot(cases, aes(index,perc)) + geom_point(size = 0.01) + 
  geom_smooth(se=TRUE, aes(colour=group)) + ylab("") +  ggtitle("Proportion of Cases Brought by Women Complainants") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + labs(subtitle = "") + theme(plot.title = element_text(size = 20, hjust=0.5))
plot4


#cases brought forward by women each day vis a vis a female investigator
subset_victim2 <- subset(subset_investoff, complainant_gender %in% c('female'))


scatter <- subset_victim2 %>% 
  group_by(system) %>% 
  dplyr::summarise(count = n()) 

#cases investigated by everyone per day 
scatter2 <- data %>% 
  group_by(system) %>% 
  dplyr::summarise(count = n()) 

colnames(scatter2)[2] <- "total"

#join data
cases <- join(scatter, scatter2, by = "system", type='full')


#add 0s
cases[is.na(cases)] <- 0

##Now need to sort dataframe because there are 0's at the bottom
cases <- cases[order(as.Date(cases$system, format="%Y/%m/%d")),]


#percentage/rate
cases$perc <- cases$count/cases$total


#now create a dummy pre/post intervention 
cases$inter <- ifelse(cases$system >= '2015-08-28', 1, 0)
cases$group <- ifelse(cases$inter == 0, "pre", "post")
cases$index <- 1:nrow(cases) #add a column for days


plot4 <- ggplot(cases, aes(index,perc)) + geom_point(size = 0.01) + 
  geom_smooth(se=TRUE, aes(colour=group)) + ylab("") +  ggtitle("Proportion of Cases Investigated by Policewomen [Woman Complainant]") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("") + labs(subtitle = "") + theme(plot.title = element_text(size = 20, hjust=0.5))
plot4



####Additional Descriptives
names(data)
data$fem_suspect <- ifelse(data$primary_suspect=="Female",1,0)
data$complainant_gend2 <- ifelse(data$complainant_gender=="female",1,0)
data$io2 <- ifelse(data$investoff_gend=="f",1,0)
library(fastDummies)
data <- dummy_cols(data, select_columns = "io_rank")


data5 <- select(data, distance, no_of_sections, male_suspect_count, female_suspect_count, io2, fem_suspect, complainant_gend2,io_rank_SI, io_rank_ASI, io_rank_HC, io_rank_I)

###Descriptives table
summary(data5)



#########################
#Load data
data <- read_csv("data2_1.csv")
summary(data$system)

#variables for year, month, and day of the week
data$year <- as.factor(format(as.Date(data$system), "%Y"))
data$month <- as.factor(format(as.Date(data$system), "%m"))
data$day <- weekdays(as.Date(data$system))

#dummy pre/post intervention 
scatter2 <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

sum(scatter2$count)#total gendered crimes

#dummy pre/post intervention 
scatter2$inter <- ifelse(scatter2$system >= '2015-08-28', 1, 0)
scatter2$group <- ifelse(scatter2$inter == 0, "pre", "post")
scatter2$index <- 1:nrow(scatter2)

ggplot(scatter2, aes(index,count)) + geom_point() + 
  geom_smooth(aes(colour=group)) + geom_smooth(method="lm", se=FALSE, aes(colour=group)) + ylab("Total") +  ggtitle("Gendered FIRs Per Day Haryana 2015-2017") +
  geom_vline(aes(xintercept = 241), linetype="dotted") + xlab("Days Since January 1 2015")

#Rate of gendered crimes per day
data3  <- scatter2
data3$rate <- (data3$count/25351462)*100000

ggplot(data3, aes(index,rate)) + geom_point() + 
  geom_smooth(aes(colour=group)) + geom_smooth(method="lm", se=FALSE, aes(colour=group)) + ylab("Rate") +  ggtitle("Rate of Gendered FIRs Per Day Haryana 2015-2018") +
  geom_vline(aes(xintercept = 241), linetype="dotted") + xlab("Days Since January 1 2015")


##
##date controls
data3$year <- as.factor(format(as.Date(data3$system), "%Y"))
data3$month <- as.factor(format(as.Date(data3$system), "%m"))
data3$day <- weekdays(as.Date(data3$system))

#time 
data3$base <- as.Date('2015-08-28')
data3$days <- difftime(data3$system, data3$base, units = c("days"))
data3$days <- as.numeric(data3$days)

summary(lm(rate ~ days + inter + days*inter, data = data3))
summary(lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2), data = data3))
summary(lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3), data = data3))

##SET 1
reg1 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = data3)
reg2 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = data3)
reg3 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = data3)

reg1 <- coeftest(reg1, vcov=NeweyWest(reg1, prewhite=FALSE, verbose = TRUE)) 
reg1
reg2 <- coeftest(reg2, vcov=NeweyWest(reg2, prewhite=FALSE, verbose = TRUE)) 
reg2
reg3 <- coeftest(reg3, vcov=NeweyWest(reg3, prewhite=FALSE, verbose = TRUE)) 
reg3

###200 day bandwidth
bandwidth <- subset(data3, system < "2015-12-06")
bandwidth <- subset(bandwidth, system > "2015-05-20")
summary(bandwidth$system)

##SET 2 
reg4 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = bandwidth)
reg5 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2)  + as.factor(day), data = bandwidth)
reg6 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = bandwidth)

reg4 <- coeftest(reg4, vcov=NeweyWest(reg4, prewhite=FALSE, verbose = TRUE)) 
reg4
reg5 <- coeftest(reg5, vcov=NeweyWest(reg5, prewhite=FALSE, verbose = TRUE)) 
reg5
reg6 <- coeftest(reg6, vcov=NeweyWest(reg6, prewhite=FALSE, verbose = TRUE)) 
reg6

##100 day bandwidth
bandwidth2 <- subset(data3, system < "2015-10-17")
bandwidth2 <- subset(bandwidth2, system > "2015-07-09")
summary(bandwidth2$system)

##SET 3
reg7 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = bandwidth2)
reg8 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = bandwidth2)
reg9 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = bandwidth2)

reg7 <- coeftest(reg7, vcov=NeweyWest(reg7, prewhite=FALSE, verbose = TRUE)) 
reg7
reg8 <- coeftest(reg8, vcov=NeweyWest(reg8, prewhite=FALSE, verbose = TRUE)) 
reg8
reg9 <- coeftest(reg9, vcov=NeweyWest(reg9, prewhite=FALSE, verbose = TRUE)) 
reg9

##SPS  (Standard Police Stations)
data$station <- revalue(data$station, c("WOMEN POLICE STATION"="AWPS Sonipat", "WOMEN PS KHANPUR KALAN"="AWPS Sonipat2"))
data2 <- data[ ! with(data, grepl("WOMEN|Women", data$station)),]

gender_crimes <- data2
scatter2 <- gender_crimes %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 
sum(scatter2$count)

#dummy pre/post intervention 
scatter2$inter <- ifelse(scatter2$system >= '2015-08-28', 1, 0)
scatter2$group <- ifelse(scatter2$inter == 0, "pre", "post")
scatter2$index <- 1:nrow(scatter2) 

ggplot(scatter2, aes(index,count)) + geom_point() + 
  geom_smooth(aes(colour=group)) + geom_smooth(method="lm", se=FALSE, aes(colour=group)) + ylab("Total") +  ggtitle("Gendered FIRs Per Day Haryana 2015-2018") +
  geom_vline(aes(xintercept = 239), linetype="dotted") + xlab("Days Since January 1 2015")

data4 <- scatter2

##rate
data4$rate <- (data4$count/25351462)*100000

ggplot(data4, aes(index,rate)) + geom_point() + 
  geom_smooth(aes(colour=group)) + geom_smooth(method="lm", se=FALSE, aes(colour=group)) + ylab("Rate2") +  ggtitle("Rate of Gendered FIRs Per Day Haryana 2015-2018") +
  geom_vline(aes(xintercept = 241), linetype="dotted") + xlab("Days Since January 1 2015")


##date controls
data4$year <- as.factor(format(as.Date(data4$system), "%Y"))
data4$month <- as.factor(format(as.Date(data4$system), "%m"))
data4$day <- weekdays(as.Date(data4$system))#getting day of the week

#time 
data4$base <- as.Date('2015-08-28')#first create a base that all other dates will be substracted from/added to
data4$days <- difftime(data4$system, data4$base, units = c("days"))
data4$days <- as.numeric(data4$days)


##SET 4 
##lets add controls for year, month, day of the week because this is time series data 
reg11 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = data4)
reg12 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = data4)
reg13 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = data4)


reg11 <- coeftest(reg11, vcov=NeweyWest(reg11, prewhite=FALSE, verbose = TRUE)) 
reg11
reg12 <- coeftest(reg12, vcov=NeweyWest(reg12, prewhite=FALSE, verbose = TRUE)) 
reg12
reg13 <- coeftest(reg13, vcov=NeweyWest(reg13, prewhite=FALSE, verbose = TRUE)) 
reg13


##SET 5 
bandwidth3 <- subset(data4, system < "2015-12-06")
bandwidth3 <- subset(bandwidth3, system > "2015-05-20")
summary(bandwidth3$system)


##
reg14 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = bandwidth3)
reg15 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = bandwidth3)
reg16 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = bandwidth3)


reg14 <- coeftest(reg14, vcov=NeweyWest(reg14, prewhite=FALSE, verbose = TRUE)) 
reg14
reg15 <- coeftest(reg15, vcov=NeweyWest(reg15, prewhite=FALSE, verbose = TRUE)) 
reg15
reg16 <- coeftest(reg16, vcov=NeweyWest(reg16, prewhite=FALSE, verbose = TRUE)) 
reg16


##SET 6 
bandwidth4 <- subset(data4, system < "2015-10-17")
bandwidth4 <- subset(bandwidth4, system > "2015-07-09")

##
reg17 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = bandwidth4)
reg18 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = bandwidth4)
reg19 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = bandwidth4)

reg17 <- coeftest(reg17, vcov=NeweyWest(reg17, prewhite=FALSE, verbose = TRUE)) 
reg17
reg18 <- coeftest(reg18, vcov=NeweyWest(reg18, prewhite=FALSE, verbose = TRUE)) 
reg18
reg19 <- coeftest(reg19, vcov=NeweyWest(reg19, prewhite=FALSE, verbose = TRUE)) 
reg19


##stargazer columns of three
stargazer(reg1, reg2, reg3, reg11, reg12, reg13, keep = c("\\binter\\b"), column.labels = c("All", "SPS"), column.separate = c(3, 3),  add.lines = list(c("Observations", "949", "949","949","949","949","949")))
stargazer(reg4, reg5, reg6, reg14, reg15, reg16, keep = c("\\binter\\b"), column.labels = c("All", "SPS"), column.separate = c(3, 3),  add.lines = list(c("Observations", "199", "199","199","199","199","199")))
stargazer(reg7, reg8, reg9, reg17, reg18, reg19, keep = c("\\binter\\b"), column.labels = c("All", "SPS"), column.separate = c(3, 3),  add.lines = list(c("Observations", "99", "99","99","99","99","99")))



##Placebo Test. before/after August 2016 
placebo1 <- data4

placebo1$inter <- ifelse(placebo1$system >= '2016-08-28', 1, 0)
placebo1$group <- ifelse(placebo1$inter == 0, "pre", "post")

placebo1$base <- as.Date('2016-08-28')
placebo1$days <- difftime(placebo1$system, placebo1$base, units = c("days"))
placebo1$days <- as.numeric(placebo1$days)

#200 day bandwidth
placebo1 <- subset(placebo1, system < "2016-12-06")
placebo1 <- subset(placebo1, system > "2016-05-20")
summary(placebo1$system)

pla1 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = placebo1)
pla2 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = placebo1)
pla3 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = placebo1)

pla1 <- coeftest(pla1, vcov=NeweyWest(pla1, prewhite=FALSE, verbose = TRUE)) 
pla1
pla2 <- coeftest(pla2, vcov=NeweyWest(pla2, prewhite=FALSE, verbose = TRUE)) 
pla2
pla3 <- coeftest(pla3, vcov=NeweyWest(pla3, prewhite=FALSE, verbose = TRUE)) 
pla3


#100 day bandwidth
placebo2 <- data4
placebo2$inter <- ifelse(placebo2$system >= '2016-08-28', 1, 0)
placebo2$group <- ifelse(placebo2$inter == 0, "pre", "post")

placebo2$base <- as.Date('2016-08-28')#first create a base that all other dates will be substracted from/added to
placebo2$days <- difftime(placebo2$system, placebo2$base, units = c("days"))
placebo2$days <- as.numeric(placebo2$days)

#100 day bandwidth
placebo2 <- subset(placebo2, system < "2016-10-17")
placebo2 <- subset(placebo2, system > "2016-07-09")
summary(placebo2$system)

##Placebo 2
pla4 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = placebo2)
pla5 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = placebo2)
pla6 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = placebo2)

pla4 <- coeftest(pla4, vcov=NeweyWest(pla4, prewhite=FALSE, verbose = TRUE)) 
pla4
pla5 <- coeftest(pla5, vcov=NeweyWest(pla5, prewhite=FALSE, verbose = TRUE)) 
pla5
pla6 <- coeftest(pla6, vcov=NeweyWest(pla6, prewhite=FALSE, verbose = TRUE)) 
pla6

stargazer(pla1, pla2, pla3, pla4, pla5, pla6, keep = c("\\binter\\b"), column.labels = c("200-Day (SPS)", "100-Day (SPS)"), column.separate = c(3, 3),  add.lines = list(c("Observations", "199", "199","199","99","99","99")))


###
#individual crimes dowry, rape, POCSO
rape_full <- data[ with(data, grepl(1, data$rape)),]
dowry_full <- data[ with(data, grepl(1, data$dowry)),]
pocso_full <- data[ with(data, grepl(1, data$pocso)),]

rape_sps <- data2[ with(data2, grepl(1, data2$rape)),]
dowry_sps <- data2[ with(data2, grepl(1, data2$dowry)),]
pocso_sps <- data2[ with(data2, grepl(1, data2$pocso)),]


#merge with total
##rates
scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- rape_full %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

#merging
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##remove top row and add 0s for all others
merged[is.na(merged)] <- 0
merged = merged[-1,]

##sort
rape_full <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]
sum(rape_full$count)

#now create a dummy pre/post intervention 
rape_full$inter <- ifelse(rape_full$system >= '2015-08-28', 1, 0)
rape_full$group <- ifelse(rape_full$inter == 0, "pre", "post")
rape_full$index <- 1:nrow(rape_full) #add a column for days

rape_full$rate <- (rape_full$count/25351462)*100000

##date controls
rape_full$year <- as.factor(format(as.Date(rape_full$system), "%Y"))
rape_full$month <- as.factor(format(as.Date(rape_full$system), "%m"))
rape_full$day <- weekdays(as.Date(rape_full$system))#getting day of the week

#time
rape_full$base <- as.Date('2015-08-28')
rape_full$days <- difftime(rape_full$system, rape_full$base, units = c("days"))
rape_full$days <- as.numeric(rape_full$days)

##(Rape FULL)
reg10 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = rape_full)
reg11 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = rape_full)
reg12 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = rape_full)

reg10 <- coeftest(reg10, vcov=NeweyWest(reg10, prewhite=FALSE, verbose = TRUE)) 
reg10
reg11 <- coeftest(reg11, vcov=NeweyWest(reg11, prewhite=FALSE, verbose = TRUE)) 
reg11
reg12 <- coeftest(reg12, vcov=NeweyWest(reg12, prewhite=FALSE, verbose = TRUE)) 
reg12


##(200 day bandwidth)
band <- subset(rape_full, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)

reg13 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg14 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg15 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg13 <- coeftest(reg13, vcov=NeweyWest(reg13, prewhite=FALSE, verbose = TRUE)) 
reg13
reg14 <- coeftest(reg14, vcov=NeweyWest(reg14, prewhite=FALSE, verbose = TRUE)) 
reg14
reg15 <- coeftest(reg15, vcov=NeweyWest(reg15, prewhite=FALSE, verbose = TRUE)) 
reg15


###100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")

reg16 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg17 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg18 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg16 <- coeftest(reg16, vcov=NeweyWest(reg16, prewhite=FALSE, verbose = TRUE)) 
reg16
reg17 <- coeftest(reg17, vcov=NeweyWest(reg17, prewhite=FALSE, verbose = TRUE)) 
reg17
reg18 <- coeftest(reg18, vcov=meatHAC(reg18, prewhite=FALSE, verbose = TRUE)) 
reg18


#dowry
##merge
##Creating  rates
scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- dowry_full %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

# 
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##remove the top row and add 0s
merged[is.na(merged)] <- 0
merged = merged[-1,]

##sort
dowry_full <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]
sum(dowry_full$count)


#dummy pre/post intervention 
dowry_full$inter <- ifelse(dowry_full$system >= '2015-08-28', 1, 0)
dowry_full$group <- ifelse(dowry_full$inter == 0, "pre", "post")
dowry_full$index <- 1:nrow(dowry_full) 

dowry_full$rate <- (dowry_full$count/25351462)*100000

##date controls
dowry_full$year <- as.factor(format(as.Date(dowry_full$system), "%Y"))
dowry_full$month <- as.factor(format(as.Date(dowry_full$system), "%m"))
dowry_full$day <- weekdays(as.Date(dowry_full$system))#getting day of the week

#time
dowry_full$base <- as.Date('2015-08-28')
dowry_full$days <- difftime(dowry_full$system, dowry_full$base, units = c("days"))
dowry_full$days <- as.numeric(dowry_full$days)


##(dowry FULL)
reg19 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_full)
reg20 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_full)
reg21 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_full)

reg19 <- coeftest(reg19, vcov=NeweyWest(reg19, prewhite=FALSE, verbose = TRUE)) 
reg19
reg20 <- coeftest(reg20, vcov=NeweyWest(reg20, prewhite=FALSE, verbose = TRUE)) 
reg20
reg21 <- coeftest(reg21, vcov=NeweyWest(reg21, prewhite=FALSE, verbose = TRUE)) 
reg21


##200 day bandwidth
band <- subset(dowry_full, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)


reg22 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg23 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg24 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg22 <- coeftest(reg22, vcov=NeweyWest(reg22, prewhite=FALSE, verbose = TRUE)) 
reg22
reg23 <- coeftest(reg23, vcov=NeweyWest(reg23, prewhite=FALSE, verbose = TRUE)) 
reg23
reg24 <- coeftest(reg24, vcov=NeweyWest(reg24, prewhite=FALSE, verbose = TRUE)) 
reg24


###100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")

reg25 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg26 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg27 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg25 <- coeftest(reg25, vcov=NeweyWest(reg25, prewhite=FALSE, verbose = TRUE)) 
reg25
reg26 <- coeftest(reg26, vcov=NeweyWest(reg26, prewhite=FALSE, verbose = TRUE)) 
reg26
reg27 <- coeftest(reg27, vcov=meatHAC(reg27, prewhite=FALSE, verbose = TRUE)) 
reg27



#pocso
#merge
##Creating  rates
scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- pocso_full %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))


#merge
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##
merged[is.na(merged)] <- 0
merged = merged[-1,]

##sort
pocso_full <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]
sum(pocso_full$count)


#dummy pre/post intervention 
pocso_full$inter <- ifelse(pocso_full$system >= '2015-08-28', 1, 0)
pocso_full$group <- ifelse(pocso_full$inter == 0, "pre", "post")
pocso_full$index <- 1:nrow(pocso_full) #add a column for days

pocso_full$rate <- (pocso_full$count/25351462)*100000

##date controls
pocso_full$year <- as.factor(format(as.Date(pocso_full$system), "%Y"))
pocso_full$month <- as.factor(format(as.Date(pocso_full$system), "%m"))
pocso_full$day <- weekdays(as.Date(pocso_full$system))#getting day of the week

#time 
pocso_full$base <- as.Date('2015-08-28')
pocso_full$days <- difftime(pocso_full$system, pocso_full$base, units = c("days"))
pocso_full$days <- as.numeric(pocso_full$days)


##(POCSO FULL)
reg28 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_full)
reg29 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_full)
reg30 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_full)

reg28 <- coeftest(reg28, vcov=NeweyWest(reg28, prewhite=FALSE, verbose = TRUE)) 
reg28
reg29 <- coeftest(reg29, vcov=NeweyWest(reg29, prewhite=FALSE, verbose = TRUE)) 
reg29
reg30 <- coeftest(reg30, vcov=NeweyWest(reg30, prewhite=FALSE, verbose = TRUE)) 
reg30


##(200 day bandwidth)
band <- subset(pocso_full, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)


reg31 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg32 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg33 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg31 <- coeftest(reg31, vcov=NeweyWest(reg31, prewhite=FALSE, verbose = TRUE)) 
reg31
reg32 <- coeftest(reg32, vcov=NeweyWest(reg32, prewhite=FALSE, verbose = TRUE)) 
reg32
reg33 <- coeftest(reg33, vcov=NeweyWest(reg33, prewhite=FALSE, verbose = TRUE)) 
reg33



## 100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")


reg34 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg35 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg36 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg34 <- coeftest(reg34, vcov=NeweyWest(reg34, prewhite=FALSE, verbose = TRUE)) 
reg34
reg35 <- coeftest(reg35, vcov=NeweyWest(reg35, prewhite=FALSE, verbose = TRUE)) 
reg35
reg36 <- coeftest(reg36, vcov=meatHAC(reg36, prewhite=FALSE, verbose = TRUE)) 
reg36




####SPS
#dowry sps
##Creating  rates
data$station <- revalue(data$station, c("WOMEN POLICE STATION"="AWPS Sonipat", "WOMEN PS KHANPUR KALAN"="AWPS Sonipat2"))
data2 <- data[ ! with(data, grepl("WOMEN|Women", data$station)),]

scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- dowry_sps %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

#merge
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##
merged[is.na(merged)] <- 0
merged = merged[-1,]

##sort
dowry_sps <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]
sum(dowry_sps$count)

#dummy pre/post intervention 
dowry_sps$inter <- ifelse(dowry_sps$system >= '2015-08-28', 1, 0)
dowry_sps$group <- ifelse(dowry_sps$inter == 0, "pre", "post")
dowry_sps$index <- 1:nrow(dowry_sps) #add a column for days

dowry_sps$rate <- (dowry_sps$count/25351462)*100000

##date controls
dowry_sps$year <- as.factor(format(as.Date(dowry_sps$system), "%Y"))
dowry_sps$month <- as.factor(format(as.Date(dowry_sps$system), "%m"))
dowry_sps$day <- weekdays(as.Date(dowry_sps$system))#getting day of the week

#time 
dowry_sps$base <- as.Date('2015-08-28')
dowry_sps$days <- difftime(dowry_sps$system, dowry_sps$base, units = c("days"))
dowry_sps$days <- as.numeric(dowry_sps$days)


##dowry
reg46 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_sps)
reg47 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_sps)
reg48 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = dowry_sps)

reg46 <- coeftest(reg46, vcov=NeweyWest(reg46, prewhite=FALSE, verbose = TRUE)) 
reg46
reg47 <- coeftest(reg47, vcov=NeweyWest(reg47, prewhite=FALSE, verbose = TRUE)) 
reg47
reg48 <- coeftest(reg48, vcov=NeweyWest(reg48, prewhite=FALSE, verbose = TRUE)) 
reg48


##(200 day bandwidth)
band <- subset(dowry_sps, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)

reg49 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg50 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg51 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg49 <- coeftest(reg49, vcov=NeweyWest(reg49, prewhite=FALSE, verbose = TRUE)) 
reg49
reg50 <- coeftest(reg50, vcov=NeweyWest(reg50, prewhite=FALSE, verbose = TRUE)) 
reg50
reg51 <- coeftest(reg51, vcov=NeweyWest(reg51, prewhite=FALSE, verbose = TRUE)) 
reg51

###100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")


reg52 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg53 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg54 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg52 <- coeftest(reg52, vcov=NeweyWest(reg52, prewhite=FALSE, verbose = TRUE)) 
reg52
reg53 <- coeftest(reg53, vcov=NeweyWest(reg53, prewhite=FALSE, verbose = TRUE)) 
reg53
reg54 <- coeftest(reg54, vcov=NeweyWest(reg54, prewhite=FALSE, verbose = TRUE)) 
reg54


###

#rape sps
##Creating  rates
scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- rape_sps %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))


#merge 
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##
merged[is.na(merged)] <- 0
merged = merged[-1,]

##sort
rape_sps <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

#dummy pre/post intervention 
rape_sps$inter <- ifelse(rape_sps$system >= '2015-08-28', 1, 0)
rape_sps$group <- ifelse(rape_sps$inter == 0, "pre", "post")
rape_sps$index <- 1:nrow(rape_sps) 

rape_sps$rate <- (rape_sps$count/25351462)*100000

##date controls
rape_sps$year <- as.factor(format(as.Date(rape_sps$system), "%Y"))
rape_sps$month <- as.factor(format(as.Date(rape_sps$system), "%m"))
rape_sps$day <- weekdays(as.Date(rape_sps$system))#getting day of the week

#time 
rape_sps$base <- as.Date('2015-08-28')
rape_sps$days <- difftime(rape_sps$system, rape_sps$base, units = c("days"))
rape_sps$days <- as.numeric(rape_sps$days)

##rape SPS
reg55 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = rape_sps)
reg56 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = rape_sps)
reg57 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = rape_sps)

reg55 <- coeftest(reg55, vcov=NeweyWest(reg55, prewhite=FALSE, verbose = TRUE)) 
reg55
reg56 <- coeftest(reg56, vcov=NeweyWest(reg56, prewhite=FALSE, verbose = TRUE)) 
reg56
reg57 <- coeftest(reg57, vcov=NeweyWest(reg57, prewhite=FALSE, verbose = TRUE)) 
reg57



##(200 day bandwidth)
band <- subset(rape_sps, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)


reg58 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg59 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg60 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg58 <- coeftest(reg58, vcov=NeweyWest(reg58, prewhite=FALSE, verbose = TRUE)) 
reg58
reg59 <- coeftest(reg59, vcov=NeweyWest(reg59, prewhite=FALSE, verbose = TRUE)) 
reg59
reg60 <- coeftest(reg60, vcov=NeweyWest(reg60, prewhite=FALSE, verbose = TRUE)) 
reg60

## 100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")


reg61 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg62 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg63 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg61 <- coeftest(reg61, vcov=NeweyWest(reg61, prewhite=FALSE, verbose = TRUE)) 
reg61
reg62 <- coeftest(reg62, vcov=NeweyWest(reg62, prewhite=FALSE, verbose = TRUE)) 
reg62
reg63 <- coeftest(reg63, vcov=NeweyWest(reg63, prewhite=FALSE, verbose = TRUE)) 
reg63


##pocso
##Creating  rates
scatter <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

scatter2 <- pocso_sps %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(desc(count))

#merge
colnames(scatter)[2] <- "count2"

merged <- join(scatter, scatter2, type='full')
merged <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]

##
merged[is.na(merged)] <- 0
merged = merged[-1,]

##
pocso_sps <- merged[order(as.Date(merged$system, format="%Y/%m/%d")),]
sum(pocso_sps$count)


#dummy pre/post intervention 
pocso_sps$inter <- ifelse(pocso_sps$system >= '2015-08-28', 1, 0)
pocso_sps$group <- ifelse(pocso_sps$inter == 0, "pre", "post")
pocso_sps$index <- 1:nrow(pocso_sps)
pocso_sps$rate <- (pocso_sps$count/25351462)*100000

##date controls
pocso_sps$year <- as.factor(format(as.Date(pocso_sps$system), "%Y"))
pocso_sps$month <- as.factor(format(as.Date(pocso_sps$system), "%m"))
pocso_sps$day <- weekdays(as.Date(pocso_sps$system))

#time 
pocso_sps$base <- as.Date('2015-08-28')
pocso_sps$days <- difftime(pocso_sps$system, pocso_sps$base, units = c("days"))
pocso_sps$days <- as.numeric(pocso_sps$days)


##
reg64 <- lm(rate ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_sps)
reg65 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_sps)
reg66 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(year) + as.factor(month) + as.factor(day), data = pocso_sps)

reg64 <- coeftest(reg64, vcov=NeweyWest(reg64, prewhite=FALSE, verbose = TRUE)) 
reg64
reg65 <- coeftest(reg65, vcov=NeweyWest(reg65, prewhite=FALSE, verbose = TRUE)) 
reg65
reg66 <- coeftest(reg66, vcov=NeweyWest(reg66, prewhite=FALSE, verbose = TRUE)) 
reg66


##(200 day bandwidth)
band <- subset(pocso_sps, system < "2015-12-06")
band <- subset(band, system > "2015-05-20")
summary(band$system)


reg67 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band)
reg68 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band)
reg69 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band)

reg67 <- coeftest(reg67, vcov=NeweyWest(reg67, prewhite=FALSE, verbose = TRUE)) 
reg67
reg68 <- coeftest(reg68, vcov=NeweyWest(reg68, prewhite=FALSE, verbose = TRUE)) 
reg68
reg69 <- coeftest(reg69, vcov=NeweyWest(reg69, prewhite=FALSE, verbose = TRUE)) 
reg69

#####100 day bandwidth
band2 <- subset(band, system < "2015-10-17")
band2 <- subset(band2, system > "2015-07-09")


reg70 <- lm(rate ~ days + inter + days*inter + as.factor(day), data = band2)
reg71 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + as.factor(day), data = band2)
reg72 <- lm(rate ~ days + inter + days*inter + I(days^2) + inter*I(days^2) + I(days^3) + inter*I(days^3) + as.factor(day), data = band2)

reg70 <- coeftest(reg70, vcov=NeweyWest(reg70, prewhite=FALSE, verbose = TRUE)) 
reg70
reg71 <- coeftest(reg71, vcov=NeweyWest(reg71, prewhite=FALSE, verbose = TRUE)) 
reg71
reg72 <- coeftest(reg72, vcov=NeweyWest(reg72, prewhite=FALSE, verbose = TRUE)) 
reg72



#stargazing (sps + awps)
stargazer(reg19,reg20,reg21, reg10,reg11,reg12, reg28,reg29,reg30, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "949", "949","949","949","949","949","949","949","949")))
stargazer(reg22,reg23,reg24,reg13,reg14,reg15,reg31,reg32,reg33, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "199", "199","199","199","199","199","199","199","199")))
stargazer(reg25,reg26,reg27,reg16,reg17,reg18,reg34,reg35,reg36, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "99", "99","99","99","99","99","99","99","99")))


#stargazing (sps only)
stargazer(reg55,reg56,reg57,reg46,reg47,reg48,reg64,reg65,reg66, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "949", "949","949","949","949","949","949","949","949")))
stargazer(reg58,reg59,reg60,reg49,reg50,reg51,reg67,reg68,reg69, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "199", "199","199","199","199","199","199","199","199")))
stargazer(reg61,reg62,reg63,reg52,reg53,reg54,reg70,reg71,reg72, keep = c("\\binter\\b"), column.labels = c("Dowry","Rape","POCSO"), column.separate = c(3, 3, 3),  add.lines = list(c("Observations", "99", "99","99","99","99","99","99","99","99")))


#######
##proportion of crimes investigated by women and where women are victims, excluding sensitive
data <- read_csv("data2_3.csv")##Total of 269955 cases

##cases investigated by women
subset_investoff <- subset(data, investoff_gend %in% c('f'))

#cases investigated by women each day as proportion of total
scatter <- subset_investoff %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

#cases investigated by everyone per day 
scatter2 <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

colnames(scatter2)[2] <- "total"

#join data
cases <- join(scatter, scatter2, by = "system", type='full')

#add 0s
cases[is.na(cases)] <- 0

##sort
cases <- cases[order(as.Date(cases$system, format="%Y/%m/%d")),]

#percentage/rate
cases$perc <- cases$count/cases$total

#dummy pre/post intervention 
cases$inter <- ifelse(cases$system >= '2015-08-28', 1, 0)
cases$group <- ifelse(cases$inter == 0, "pre", "post")
cases$index <- 1:nrow(cases) 


##date controls
cases$year <- as.factor(format(as.Date(cases$system), "%Y"))
cases$month <- as.factor(format(as.Date(cases$system), "%m"))
cases$day <- weekdays(as.Date(cases$system))

#
cases$base <- as.Date('2015-08-28')
cases$days <- difftime(cases$system, cases$base, units = c("days"))
cases$days <- as.numeric(cases$days)

## 
regression1 <- lm(perc ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = cases)
regression1 <- coeftest(regression1, vcov=NeweyWest(regression1, prewhite=FALSE, verbose = TRUE)) 
regression1


##200 day bandwidth
bandwidth <- subset(cases, system < "2015-12-06")
bandwidth <- subset(bandwidth, system > "2015-05-20")
summary(bandwidth$system)


##SET 2
regression2 <- lm(perc ~ days + inter + days*inter + as.factor(day), data = bandwidth)
regression2 <- coeftest(regression2, vcov=NeweyWest(regression2, prewhite=FALSE, verbose = TRUE)) 
regression2


###100 day 
bandwidth2 <- subset(cases, system < "2015-10-17")
bandwidth2 <- subset(bandwidth2, system > "2015-07-09")
summary(bandwidth2$system)


regression3 <- lm(perc ~ days + inter + days*inter + as.factor(day), data = bandwidth2)
regression3 <- coeftest(regression3, vcov=NeweyWest(regression3, prewhite=FALSE, verbose = TRUE)) 
regression3


##Female victims
table(data$complainant_gender)##women are 23865 primary complainants
subset_victim <- subset(data, complainant_gender %in% c('female'))

#cases brought forward by women each day
scatter <- subset_victim %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

#cases by everyone per day 
scatter2 <- data %>% 
  dplyr::group_by(system) %>% 
  dplyr::summarise(count = n()) 

colnames(scatter2)[2] <- "total"

#join data
cases <- join(scatter, scatter2, by = "system", type='full')

#add 0s
cases[is.na(cases)] <- 0

##sort
cases <- cases[order(as.Date(cases$system, format="%Y/%m/%d")),]

#percentage/rate
cases$perc <- cases$count/cases$total

#dummy pre/post intervention 
cases$inter <- ifelse(cases$system >= '2015-08-28', 1, 0)
cases$group <- ifelse(cases$inter == 0, "pre", "post")
cases$index <- 1:nrow(cases) 


##date controls
cases$year <- as.factor(format(as.Date(cases$system), "%Y"))
cases$month <- as.factor(format(as.Date(cases$system), "%m"))
cases$day <- weekdays(as.Date(cases$system))#getting day of the week

#
cases$base <- as.Date('2015-08-28')
cases$days <- difftime(cases$system, cases$base, units = c("days"))
cases$days <- as.numeric(cases$days)

##
regression4 <- lm(perc ~ days + inter + days*inter + as.factor(year) + as.factor(month) + as.factor(day), data = cases)
regression4 <- coeftest(regression4, vcov=NeweyWest(regression4, prewhite=FALSE, verbose = TRUE)) 
regression4


###200 day bandwidth
bandwidth <- subset(cases, system < "2015-12-06")
bandwidth <- subset(bandwidth, system > "2015-05-20")
summary(bandwidth$system)


##SET 2 
regression5 <- lm(perc ~ days + inter + days*inter + as.factor(day), data = bandwidth)
regression5 <- coeftest(regression5, vcov=NeweyWest(regression5, prewhite=FALSE, verbose = TRUE)) 
regression5


###100 day bandwidth
bandwidth2 <- subset(cases, system < "2015-10-17")
bandwidth2 <- subset(bandwidth2, system > "2015-07-09")
summary(bandwidth2$system)


regression6 <- lm(perc ~ days + inter + days*inter + as.factor(day), data = bandwidth2)
regression6 <- coeftest(regression6, vcov=NeweyWest(regression6, prewhite=FALSE, verbose = TRUE)) 
regression6


#stargazing 
stargazer(regression1, regression2, regression3, keep = c("\\binter\\b"), column.separate = c(2, 2),  add.lines = list(c("Observations", "949", "199","99")))
stargazer(regression4, regression5, regression6, keep = c("\\binter\\b"), column.separate = c(2, 2),  add.lines = list(c("Observations", "949", "199","99")))


















