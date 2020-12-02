rm(list=ls()) 
#set wd

install.packages('ggplot2', repos = "http://cran.us.r-project.org")
install.packages('reshape2', repos = "http://cran.us.r-project.org")
install.packages('cowplot')

library(ggplot2);library(reshape2);library(dplyr); library(cowplot);library(gridExtra)

#loading the women_bprd dataset
women_bprd <- read.csv("data5_1.csv")

#total by rank for each year (Haryana)
cops <- read.csv("data5_2.csv")

#merge ranks in 4 groups: IPS, IPS/State, State, Constables
cops$IPS <- rowSums(cops[,c("DGP.Addl.DGP", "IGP", "DIG", "AIGP..SSP.SP")], na.rm=TRUE)
cops$state_IPS <- rowSums(cops[,c("Addl.SP.Dy.COMN", "ASP.Dy.SP")], na.rm=TRUE)
cops$state <- rowSums(cops[,c("Inspector", "Sub..Inspector", "A.S.I")], na.rm=TRUE)
cops$constables <- rowSums(cops[,c("H.Constable", "Constable")], na.rm=TRUE)

#now select relevant vars
new_cops <- data.frame(cops$YEAR, cops$IPS, cops$state_IPS, cops$state, cops$constables)
new_cops

#Same for women
total_women <- subset(women_bprd, women_bprd$STATE.UT == 'HARYANA')
total_women  

#merge ranks in 4 groups: IPS, IPS/State, State, Constables
total_women$IPS <- rowSums(total_women[,c("DGP.Spl.DG.ADGP", "Addl.DG", "IGP", "DIG", "AIGP.SSP.SP.COM")], na.rm=TRUE)
total_women$state_IPS <- rowSums(total_women[,c("ADL.SP.Dy.COM", "ASP.Dy.SP.Asst.COM")], na.rm=TRUE)
total_women$state <- rowSums(total_women[,c("INSP.", "S.I", "A.S.I")], na.rm=TRUE)
total_women$constables <- rowSums(total_women[,c("Head.Const.", "Const.")], na.rm=TRUE)

#now select 
new_female <- data.frame(total_women$YEAR, total_women$IPS, total_women$state_IPS, total_women$state, total_women$constables)
new_female


#merged dataset of women cops divided by total cops by rank
total <- data.frame(new_cops$cops.YEAR, new_female$total_women.IPS/new_cops$cops.IPS, new_female$total_women.state_IPS/new_cops$cops.state_IPS,
                    new_female$total_women.state/new_cops$cops.state, new_female$total_women.constables/new_cops$cops.constables)

names(total)[1] <- 'year'
names(total)[2] <- 'IPS'
names(total)[3] <- 'state_IPS'
names(total)[4] <- 'state'
names(total)[5] <- 'constables'


total <- total[4:10,]

#melting and ggplot
total2 <- melt(total, id="year")  # convert to long format
total2

#increase by rank
figure_3 <- ggplot(data=total2, aes(factor(year),value, shape = variable)) + 
  geom_point(aes(shape = factor(variable)), size = 3)+
  geom_line(aes(group=variable, linetype=variable))+ xlab("Year") +
  ylab("Percent") + ggtitle("Gender Representation in Law Enforcement By Rank 2011-2017 [Haryana]") +
  theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
        axis.text.y = element_text(colour="grey20", size=12),
        text=element_text(size=16, family="Arial")) + theme(plot.caption=element_text()) + theme(plot.title = element_text(size = 15, face = "bold", hjust=0.5))
figure_3 <- figure_3 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black")) 

figure_3 <- figure_3 + theme(plot.title = element_text(size = 20, hjust=0.5))
figure_3


#####
##2008 and 2017
jump_women <- data.frame(women_bprd$YEAR, women_bprd$STATE.UT, women_bprd$X..Women)
#subset the data to just 2008 and 2017 
perc_jump <- subset(jump_women, women_bprd$YEAR == 2008 | women_bprd$YEAR == 2017)
perc_jump

dat1 <- subset(perc_jump, perc_jump$women_bprd.YEAR == "2008")

tenyears1 <- subset(perc_jump, women_bprd.YEAR=="2008")
tenyears2 <- subset(perc_jump, women_bprd.YEAR=="2017")
names(tenyears1) <- c("year", "state", "percent1")
names(tenyears2) <- c("year2", "state", "percent2")

dat  <- merge(tenyears1, tenyears2, by = "state")
dat$diff <- dat$percent2 - dat$percent1

dat2  <- select(dat, state, year2, diff)

names(dat1) <- c("year2", "state", "diff")

final <- rbind(dat1, dat2)


#There's no increase in Kerala and Puducherry - so let's add 0's there
final$diff[final$diff <= -0.0000000000000000000001] <- 0

#convert to lowercase


library(tools)

final$state <- as.character(final$state)
final$state <- sapply(final$state, tolower)
final$state <- toTitleCase(final$state)

figure_2 <- ggplot(data=final, aes(x=state, y=diff, fill=factor(year2, levels = c("2017","2008")))) + geom_bar(stat="identity") 
figure_2 + coord_flip() + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5)) + 
  ggtitle("Gender Representation in the Indian Police in 2008 & 2017 (Bureau of Police Research & Development)") + geom_vline(aes(xintercept = 32), linetype="dashed") +
  xlab("") + ylab("Percent") + theme(legend.title = element_blank()) + 
  theme(axis.text.y = element_text(size = 15)) + scale_fill_manual(values=c("grey74", "grey26")) 



bprd2 <- subset(women_bprd, women_bprd$YEAR == '2017')

figure_2 <- ggplot(data=bprd2, aes(x=reorder(STATE.UT, desc(X..Women)), y=X..Women)) + geom_bar(stat="identity") 


figure_2 + coord_flip() + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5)) + 
  ggtitle("Gender Representation in the Indian Police 2017") + geom_vline(aes(xintercept = 19), linetype="dashed") + geom_vline(aes(xintercept = 10), linetype="dashed") +
  xlab("") + ylab("Percent") + theme(legend.title = element_blank()) + 
  theme(axis.text.y = element_text(size = 15)) + scale_fill_manual(values=c("grey74", "grey26")) 



#####For NFHS graphs/tables - download the NFHS-4 data:  https://www.dhsprogram.com/
#load NFHS data
#remove columns that have all NA's and create new subset
data <- Filter(function(x) !(all(x=="")), data)

data$state <- ifelse(data$v101 == "haryana", "Haryana", 
                     ifelse(data$v101 == "uttar pradesh", "UP", 
                            "Rest"))

##Table of descriptives 
haryana <- subset(data, data$v101=="haryana")
up <- subset(data, data$v101=="uttar pradesh")


haryana_sum <- Hmisc::describe(haryana)
haryana_sum[1:100,]
haryana_sum[100:164,]

##############
##Domestic Violence Module subset
data2 <- subset(data, data$v044=="woman selected and interviewed")
##Physical
data2$push <- ifelse(data2$d105a == "never", 0, 1)
data2$slap <- ifelse(data2$d105b == "never", 0, 1)
data2$punch <- ifelse(data2$d105c == "never", 0, 1)
data2$kick <- ifelse(data2$d105d == "never", 0, 1)
data2$strangle <- ifelse(data2$d105e == "never", 0, 1)
data2$knife <- ifelse(data2$d105f == "never", 0, 1)
data2$arm <- ifelse(data2$d105j == "never", 0, 1)

##Sexual
data2$forced_sex <- ifelse(data2$d105h == "never", 0, 1)
data2$forced2 <- ifelse(data2$d105i == "never", 0, 1)
data2$physical <- ifelse(data2$d105k == "never", 0, 1)


#Total
#Any form of physical - create dummy from never in d106 and d107
prop.table(table(data2$d106))#mild
prop.table(table(data2$d107))#severe
#Any form of sexual - d108
prop.table(table(data2$d108))
#Dummy for Afraid
data2$afraid <- ifelse(data2$d129 == "never afraid", 0, 1)
prop.table(table(data2$afraid))


##
haryana <- subset(data2, data2$v101=="haryana")

sources <- as.data.frame(cbind(prop.table(table(data2$push)), prop.table(table(data2$slap)), prop.table(table(data2$punch)),
                               prop.table(table(data2$kick)), prop.table(table(data2$strangle)), prop.table(table(data2$knife)),
                               prop.table(table(data2$arm)),prop.table(table(data2$forced_sex)),prop.table(table(data2$forced2)),
                               prop.table(table(data2$physical)),prop.table(table(data2$d106)),prop.table(table(data2$d107)),prop.table(table(data2$d108)),prop.table(table(data2$afraid))))
colnames(sources)<- c("Pushed","Slapped","Punched","Kicked",
                      "Strangled","Threatened With Weapon","Arm Twisted","Forced Into Sex","Forced Into Sexual Act","Physically Forced","Any Violence","Any Severe","Any Sex","Afraid")
sources
sources <- sources[-c(1), ]  #remove all the no's
sources
sources <- round(sources,3)*100
sources

dtestm1 <- melt(sources)
dtestm1


##haryana
sources <- as.data.frame(cbind(prop.table(table(haryana$push)), prop.table(table(haryana$slap)), prop.table(table(haryana$punch)),
                               prop.table(table(haryana$kick)), prop.table(table(haryana$strangle)), prop.table(table(haryana$knife)),
                               prop.table(table(haryana$arm)),prop.table(table(haryana$forced_sex)),prop.table(table(haryana$forced2)),
                               prop.table(table(haryana$physical)),prop.table(table(haryana$d106)),prop.table(table(haryana$d107)),prop.table(table(haryana$d108)),prop.table(table(haryana$afraid))))
colnames(sources)<- c("Pushed","Slapped","Punched","Kicked",
                      "Strangled","Threatened With Weapon","Arm Twisted","Forced Into Sex","Forced Into Sexual Act","Physically Forced","Any Violence","Any Severe","Any Sex","Afraid")
sources
sources <- sources[-c(1), ]  #remove all the no's
sources
sources <- round(sources,3)*100
sources

dtestm2 <- melt(sources)
dtestm2



test <- cbind(dtestm2, dtestm1)
print(xtable(test), include.rownames=FALSE)


##Turning to Others:
#Ever sought help from anyone?
prop.table(table(data$d119y))
prop.table(table(data$d119y))

#Whom did you seek help?
help <- subset(data, data$d119y=="sought help from someone")
prop.table(table(help$d119h))#own family
prop.table(table(help$d119i))#husband/partner family
prop.table(table(help$d119xd))#friend
prop.table(table(help$d119u))#neighbor
prop.table(table(help$d119xe))#police
prop.table(table(help$d119xf))#religious leader


#ggplotting 
sources <- as.data.frame(cbind(prop.table(table(help$d119h)), prop.table(table(help$d119i)), prop.table(table(help$d119xd)),
                               prop.table(table(help$d119u)), prop.table(table(help$d119xe)), prop.table(table(help$d119xf))))
colnames(sources)<- c("own family","husband family","friend","neighbor",
                      "police","religious leader")
sources
sources <- sources[-c(1), ]  #remove all the no's

dtestm1 <- melt(sources)
dtestm1

figure <- ggplot(data = dtestm1, aes(x = variable, y = value)) + 
  geom_bar(stat="identity") + coord_flip() + xlab("") + ylab("") + 
  theme(plot.title = element_text(size = 20, hjust=-0.5)) + theme(axis.text.x = element_text(colour="grey20", size=12),axis.text.y = element_text(colour="grey20", size=12),
                                                                  text=element_text(size=16, family="Arial"))
figure <- figure + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

figure








