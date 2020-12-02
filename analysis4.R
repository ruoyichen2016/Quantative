rm(list=ls()) 
#set wd


install.packages('ggplot2', repos = "http://cran.us.r-project.org")
install.packages("ggsignif")
install.packages('reshape2', repos = "http://cran.us.r-project.org")
install.packages("gridExtra")
install.packages("stargazer")
install.packages('sandwich', repos = "http://cran.us.r-project.org")
install.packages('lmtest', repos = "http://cran.us.r-project.org")
install.packages('data.table')
install.packages('pysch')
install.packages('ggpubr')
install.packages("papeR")


library(ggplot2);library(plyr);library(dplyr);library(ggsignif);library(reshape2);library(reshape2)
library(gridExtra);library(sandwich);library(lmtest);library(data.table);library(stargazer);library(psych);library(readxl)
library(broom);library(purrr);library(ggpubr);library(papeR);library(readr);library(REdaS)

#load CSDS data
#data <- read_excel("data.xlsx")
#data2 <- read_excel("data2.xlsx")
#data2 <- data2 %>% mutate_if(is.character,as.factor) #convert all characters to factors

#revalue
data2$strength <- revalue(data2$Q33a, c("1: Very justified"="justified", "2: Somewhat justified"="justified", "3: Somewhat unjustified"="unjustified","4: Very unjustified"="unjustified","8: Don't know"="DK"))
data2$home <- revalue(data2$Q33b, c("1: Very justified"="justified", "2: Somewhat justified"="justified", "3: Somewhat unjustified"="unjustified","4: Very unjustified"="unjustified","8: Don't know"="DK"))
data2$heinous <- revalue(data2$Q33c, c("1: Very justified"="justified", "2: Somewhat justified"="justified", "3: Somewhat unjustified"="unjustified","4: Very unjustified"="unjustified","8: Don't know"="DK"))
data2$inflexible <- revalue(data2$Q33d, c("1: Very justified"="justified", "2: Somewhat justified"="justified", "3: Somewhat unjustified"="unjustified","4: Very unjustified"="unjustified","8: Don't know"="DK"))

####
tab <- subset(data2, State_id == "07: Haryana")
awps_introduced <- tab[with(tab, grepl("1: Introduced", tab$Q34a)),]
not_introduced <- tab[with(tab, grepl("2: Not introduced", tab$Q34a)),]


perception_a <- tab %>% group_by(Q34a) %>% count(Q34a, strength) %>% mutate(prop = prop.table(n))
perception_a <- perception_a[1:6,]
perception_b <- tab %>% group_by(Q34a) %>% count(Q34a, home) %>% mutate(prop = prop.table(n))
perception_b <- perception_b[1:6,]
perception_c <- tab %>% group_by(Q34a) %>% count(Q34a, heinous) %>% mutate(prop = prop.table(n))
perception_c <- perception_c[1:6,]
perception_d <- tab %>% group_by(Q34a) %>% count(Q34a, inflexible) %>% mutate(prop = prop.table(n))
perception_d <- perception_d[1:6,]


fig1 <- ggplot(data=perception_a, aes(x=strength, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig1 <- fig1 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig1 <- fig1 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=0, hjust=-0.2)
fig1 <- fig1 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Being in the police requires physical strength and aggressive behavior which women lack.") 
fig1 <- fig1 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig1 <- fig1 + scale_y_continuous(breaks=seq(0,0.85,0.05))
fig1

##

fig2 <- ggplot(data=perception_b, aes(x=home, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig2 <- fig2 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig2 <- fig2 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=0, hjust=-0.2)
fig2 <- fig2 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("A woman should prioritize managing the home instead of joining police.") 
fig2 <- fig2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig2 <- fig2 + scale_y_continuous(breaks=seq(0,0.85,0.05))
fig2

#


fig3 <- ggplot(data=perception_c, aes(x=heinous, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig3 <- fig3 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig3 <- fig3 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=0, hjust=-0.2)
fig3 <- fig3 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Women police are incapable of handling high intensity crimes and cases.") 
fig3 <- fig3 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 


fig3 <- fig3 + scale_y_continuous(breaks=seq(0,0.85,0.05))
fig3

##



fig4 <- ggplot(data=perception_d, aes(x=inflexible, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig4 <- fig4 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig4 <- fig4 + geom_text(aes(label=round(prop,2)),position = position_dodge(width=1), vjust=0, hjust=-0.2)
fig4 <- fig4 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Because of inflexible working hours it is difficult for women to work in police.") 
fig4 <- fig4 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 


fig4 <- fig4 + scale_y_continuous(breaks=seq(0,0.85,0.05))
fig4

#grid extra
fig_combined1 <- ggarrange(fig1, fig2, fig3, fig4, labels = c("A)", "B)", "C)", "D)"), ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
fig_combined1 <- annotate_figure(fig_combined1, top = text_grob("Attitudes About Women in the Police (Haryana: CSDS-Common Cause Survey 2017)", face = "bold", size = 20))
fig_combined1




####Male and Female ALL INDIA 
tab <- subset(data2, Z3 == "1: Male")

awps_introduced <- tab[with(tab, grepl("1: Introduced", tab$Q34a)),]
not_introduced <- tab[with(tab, grepl("2: Not introduced", tab$Q34a)),]

perception_a <- tab %>% group_by(Q34a) %>% count(Q34a, strength) %>% mutate(prop = prop.table(n))
perception_a <- perception_a[1:6,]
perception_b <- tab %>% group_by(Q34a) %>% count(Q34a, home) %>% mutate(prop = prop.table(n))
perception_b <- perception_b[1:6,]
perception_c <- tab %>% group_by(Q34a) %>% count(Q34a, heinous) %>% mutate(prop = prop.table(n))
perception_c <- perception_c[1:6,]
perception_d <- tab %>% group_by(Q34a) %>% count(Q34a, inflexible) %>% mutate(prop = prop.table(n))
perception_d <- perception_d[1:6,]



fig1 <- ggplot(data=perception_a, aes(x=strength, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig1 <- fig1 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig1 <- fig1 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.3)
fig1 <- fig1 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Being in police requires physical strength and aggressive behavior which women lack.") 
fig1 <- fig1 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig1 <- fig1 + scale_y_continuous(breaks=seq(0,0.7,0.05))
fig1

##

fig2 <- ggplot(data=perception_b, aes(x=home, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig2 <- fig2 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig2 <- fig2 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.3)
fig2 <- fig2 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("A woman should prioritize managing the home instead of joining police.") 
fig2 <- fig2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig2 <- fig2 + scale_y_continuous(breaks=seq(0,0.65,0.05), limits = c(0.0,0.65))
fig2

#


fig3 <- ggplot(data=perception_c, aes(x=heinous, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig3 <- fig3 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig3 <- fig3 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1),vjust=-1.4)
fig3 <- fig3 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Women police are incapable of handling high intensity crimes and cases.") 
fig3 <- fig3 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 


fig3 <- fig3 + scale_y_continuous(breaks=seq(0,0.55,0.05), limits = c(0.0,0.55))
fig3

##

fig4 <- ggplot(data=perception_d, aes(x=inflexible, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig4 <- fig4 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig4 <- fig4 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.3)
fig4 <- fig4 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Because of inflexible working hours it is difficult for women to work in police.") 
fig4 <- fig4 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig4 <- fig4 + scale_y_continuous(breaks=seq(0,0.55,0.05))
fig4


#grid extra
fig_combined1 <- ggarrange(fig1, fig2, fig3, fig4, labels = c("A)", "B)", "C)", "D)"), ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
fig_combined1 <- annotate_figure(fig_combined1, top = text_grob("Attitudes About Women in the Police (Male Respondents: CSDS-Common Cause Survey 2017)", face = "bold", size = 20))
fig_combined1


#######FEMALE RESPONDENTS ALL INDIA
tab <- subset(data2, Z3 == "2: Female")

awps_introduced <- tab[with(tab, grepl("1: Introduced", tab$Q34a)),]
not_introduced <- tab[with(tab, grepl("2: Not introduced", tab$Q34a)),]

perception_a <- tab %>% group_by(Q34a) %>% count(Q34a, strength) %>% mutate(prop = prop.table(n))
perception_a <- perception_a[1:6,]
perception_b <- tab %>% group_by(Q34a) %>% count(Q34a, home) %>% mutate(prop = prop.table(n))
perception_b <- perception_b[1:6,]
perception_c <- tab %>% group_by(Q34a) %>% count(Q34a, heinous) %>% mutate(prop = prop.table(n))
perception_c <- perception_c[1:6,]
perception_d <- tab %>% group_by(Q34a) %>% count(Q34a, inflexible) %>% mutate(prop = prop.table(n))
perception_d <- perception_d[1:6,]



fig1 <- ggplot(data=perception_a, aes(x=strength, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig1 <- fig1 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig1 <- fig1 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1),vjust=-1.5)
fig1 <- fig1 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Being in police requires physical strength and aggressive behavior which women lack.") 
fig1 <- fig1 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig1 <- fig1 + scale_y_continuous(breaks=seq(0,0.65,0.05))
fig1

##

fig2 <- ggplot(data=perception_b, aes(x=home, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig2 <- fig2 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig2 <- fig2 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.5)
fig2 <- fig2 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("A woman should prioritize managing the home instead of joining police.") 
fig2 <- fig2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 

fig2 <- fig2 + scale_y_continuous(breaks=seq(0,0.65,0.05), limits = c(0.0,0.65)) 
fig2

#

fig3 <- ggplot(data=perception_c, aes(x=heinous, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig3 <- fig3 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig3 <- fig3 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1), vjust=-1.7)
fig3 <- fig3 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Women police are incapable of handling high intensity crimes and cases.") 
fig3 <- fig3 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 


fig3 <- fig3 + scale_y_continuous(breaks=seq(0,0.65,0.05), limits = c(0.0,0.55))
fig3

##

fig4 <- ggplot(data=perception_d, aes(x=inflexible, y=prop, fill=Q34a)) + geom_col(stat="identity", position=position_dodge()) 
fig4 <- fig4 + theme(axis.ticks=element_blank(), axis.text.y = element_blank(), axis.title.x=element_blank(), legend.title=element_blank(), axis.title.y=element_blank()) + theme(text = element_text(size = 20)) + scale_fill_manual(values = c("gray71", "grey40"), labels=c("AWPS Introduced","AWPS Not Introduced"))
fig4 <- fig4 + geom_text(size = 3, aes(label=round(prop,2)),position = position_dodge(width=1),vjust=-1.7)
fig4 <- fig4 + theme(plot.title = element_text(size = 11, hjust=0.5)) + ggtitle("Because of inflexible working hours it is difficult for women to work in police.") 
fig4 <- fig4 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                     axis.text.y = element_text(colour="grey20", size=10)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black")) 


fig4 <- fig4 + scale_y_continuous(breaks=seq(0,0.65,0.05))
fig4

#grid extra
fig_combined1 <- ggarrange(fig1, fig2, fig3, fig4, labels = c("A)", "B)", "C)", "D)"), ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
fig_combined1 <- annotate_figure(fig_combined1, top = text_grob("Attitudes About Women in the Police (Female Respondents: CSDS-Common Cause Survey 2017)", face = "bold", size = 20))
fig_combined1



##diff
tab <- subset(data2, Z3 == "2: Female")

library(fastDummies)
tab <- dummy_cols(tab, select_columns = "strength")
tab <- dummy_cols(tab, select_columns = "home")
tab <- dummy_cols(tab, select_columns = "heinous")
tab <- dummy_cols(tab, select_columns = "inflexible")

colnames(tab)[colnames(tab)=="strength_justified"] <- "statement_A_justified"
colnames(tab)[colnames(tab)=="home_justified"] <- "statement_B_justified"
colnames(tab)[colnames(tab)=="heinous_justified"] <- "statement_C_justified"
colnames(tab)[colnames(tab)=="inflexible_justified"] <- "statement_D_justified"


awps_introduced <- tab[with(tab, grepl("1: Introduced", tab$Q34a)),]
not_introduced <- tab[with(tab, grepl("2: Not introduced", tab$Q34a)),]



a1 <- t.test(awps_introduced$statement_A_justified, not_introduced$statement_A_justified)
a2 <- t.test(awps_introduced$statement_B_justified, not_introduced$statement_B_justified)
a3 <- t.test(awps_introduced$statement_C_justified, not_introduced$statement_C_justified)
a4 <- t.test(awps_introduced$statement_D_justified, not_introduced$statement_D_justified)


tab <- map_df(list(a1, a2, a3, a4), tidy)
tab


setnames(tab, old=c("estimate1","estimate2", "estimate"), new=c("awps", "non_awps", "diff"))
tab$eval <- c("A","B","C","D")
tab$eval <- factor(tab$eval, levels = tab$eval)
tab


##ggplot 
plot2 <- ggplot(tab,aes(x=eval, y=diff,ymin=conf.low, ymax=conf.high)) + 
  geom_pointrange() + ggtitle("Diff. in Justified Responses to Negative Stereotypes (AWPS Minus Non-AWPS)") + xlab("") + 
  ylab("") + geom_hline(yintercept=0, linetype="dashed") 
plot2 <- plot2 + theme(axis.text.x = element_text(colour="grey20", size=14, hjust=.5, vjust=.5),
                       axis.text.y = element_text(colour="grey20", size=12))
plot2



#34a all women police station measures in your locality have been introduced?
awps <- as.data.frame(prop.table(table(data2$Q34a, data2$State_id),2))
awps <- awps[with(data, grepl("1: Introduced", awps$Var1)),]
awps$Var2 <- gsub("*..:", "", awps$Var2)
awps[order(awps$Freq, decreasing = T),]

#order by frequency in the barplot, remove all non-letters from Var1 variable
awps_fig <- ggplot(data = awps, aes(x = reorder(Var2, Freq), y = Freq)) + geom_col(stat="identity") + coord_flip() + theme_minimal() + ggtitle("% Respondents by State Who Say AWPS Introduced") + theme(axis.title.y=element_blank()) + theme(text = element_text(size = 13))
awps_fig


