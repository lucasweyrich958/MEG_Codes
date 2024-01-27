#Subnuclei x Detriment * Age * Sex Interaction Plots

library(tidyverse)
library(ggplot2)
library(Cairo)
library(car)
library(ggeffects)

setwd('D:/DevMind_T2_Slab/DevMIND_3p0_Scans/Stats')
df = read.csv('Nonparametric.csv')
#Violin Plots
p1 = ggplot(df, aes(x=SMU_Acct_00Twitch, y=SMU_Age)) +
  geom_violin(fill='#E488E2',trim=F) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans")) +
  ylab('Age (years)') +
  xlab('Most-Used Social Media Application')+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.75, fill='gray26')
p1
ggsave("ViolinPlot.png", type = "cairo")






#----Demographic Plots----
lm1 = lm(R_Centromedial~SMU_Age+R_Whole_amygdala+SM_Detriment_Index, df)
summary(lm1)
res1 = lm1$residuals
df$res_Centro = res1
df$SM_distress = df$SM_Detriment_Index
df$R_Centromedial = df$centro
crPlots(lm1,col='deeppink2',pch=19,grid=F,lwd=3)
p1 = ggpredict(model = lm1, terms = 'SM_Detriment_Index')

p1 = ggplot(df, aes(x=SM_Detriment_Index, y=R_Centromedial)) + geom_point(size=3, color = "mediumpurple3") +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  #scale_color_manual(values=c("mediumpurple3"),name=' ') +
  geom_smooth(method=lm, se=T,linewidth=1.5, color = 'gray26') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans")) +
  ylab('Right Centromedial Residuals') +
  xlab('Social Media Distress Index')
p1
ggsave("L_Basolateral_Detriment.png", type = "cairo")




lm2 = lm(L_CA4~SMU_Age+L_Whole_hippocampus+SM_Detriment_Index, df)
res2 = lm2$residuals
res2.5 = residuals.lm(lm2,'partial')
res2.5 = as.data.frame(res2.5)
res$hippo_res=res2

write.csv(res,'res.csv')
write.csv(res1.5,'res1.5.csv')
write.csv(res2.5,'res2.5.csv')

#Detriment Index x Age
p1 = ggplot(df, aes(x=Age, y=SM_Detriment_Index), color = "mediumpurple3") + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  #scale_color_manual(values=c("mediumpurple3"),name=' ') +
  geom_smooth(method=lm,se=F,linewidth=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab('Social Media Detriment Index') +
  xlab('Age (years)')
p1
ggsave("L_Basolateral_Detriment.png", type = "cairo")

#Volumes x Age


#----Detriment Index----
#Left Basolateral Amygdala x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=L_Amy_Basolateral_Complex, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.25, 0.175)) +
  ylab(bquote('Left Basolateral Complex'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("L_Basolateral_Detriment.png", type = "cairo")

#Right Baseolateral x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=R_Amy_Basolateral_Complex, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right Basolateral Complex'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("R_Basolateral_Detriment.png", type = "cairo")


#Left CA1 x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=L_hippo_CA1, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Left CA1'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("L_CA1.png", type = "cairo")

#Left CA3 x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=L_hippo_CA3, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Left CA3'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("L_CA3.png", type = "cairo")

#Left CA4/DG x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=L_hippo_CA4.DG, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Left CA4/DG'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("L_CA4.png", type = "cairo")

#Right CA1 x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=R_Hippo_CA1, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA1'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("R_CA1.png", type = "cairo")

#Right CA3 x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=R_Hippo_CA3, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA3'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("R_CA3.png", type = "cairo")

#Right CA4/DG x Detriment Index
p1 = ggplot(df, aes(x=SM_Detriment_Index, y=R_Hippo_CA4.DG, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA4/DG'~(mm^3))) +
  xlab('Social Media Detriment Index')
p1
ggsave("R_CA4.png", type = "cairo")

#----Rest----
#Left Basolateral x Negative Affect
p1 = ggplot(df, aes(x=SM_NegativeAffect, y=L_Amy_Basolateral_Complex, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Left Basolateral Complex'~(mm^3))) +
  xlab('Negative Affect')
p1
ggsave("L_Baso_NegAff.png", type = "cairo")

#Right Centromedial x Passive Use
p1 = ggplot(df, aes(x=Passive_Z, y=R_Amy_Centromedial_Complex, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right Centromedial Complex'~(mm^3))) +
  xlab('Passive Use')
p1
ggsave("R_Centro_Passive.png", type = "cairo")

#Left CA1 x Negative Affect
p1 = ggplot(df, aes(x=SM_NegativeAffect, y=R_Hippo_CA1, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA1'~(mm^3))) +
  xlab('Negative Affect')
p1
ggsave("R_CA1_NegAff.png", type = "cairo")

#Right CA1 x Active Use
p1 = ggplot(df, aes(x=Active_Z, y=R_Hippo_CA1, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Rigth CA1'~(mm^3))) +
  xlab('Active Use')
p1
ggsave("R_CA1_Active.png", type = "cairo")

#Right CA3 x Active Use
p1 = ggplot(df, aes(x=Active_Z, y=R_Hippo_CA3, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA3'~(mm^3))) +
  xlab('Active Use')
p1
ggsave("R_CA3_Active.png", type = "cairo")

#Right CA4/DG x Active Use
p1 = ggplot(df, aes(x=Active_Z, y=R_Hippo_CA4.DG, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA4/DG'~(mm^3))) +
  xlab('Active Use')
p1
ggsave("R_CA4_Active.png", type = "cairo")

#Right CA4/DG x Negative Affect
p1 = ggplot(df, aes(x=SM_NegativeAffect, y=R_Hippo_CA4.DG, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Right CA4/DG'~(mm^3))) +
  xlab('Negative Affect')
p1
ggsave("R_CA4_NegAff.png", type = "cairo")

#Left CA4/DG  x Negative Affect
p1 = ggplot(df, aes(x=SM_NegativeAffect, y=L_hippo_CA4.DG, color=Sex)) + geom_point(size=3) +
  #scale_color_gradient(low="deepskyblue2", high="chartreuse") +
  scale_color_manual(values=c("darkorchid4", "indianred3", 'darkorchid1','indianred1'),name=' ') +
  geom_smooth(method=lm,se=F,size=1.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 15, color = 'gray28'),
        axis.title = element_text(size = 15, color = 'gray28'),
        axis.title.x  = element_text(family = "sans"),
        axis.title.y  = element_text(family = "sans"),
        legend.text = element_text(family = "sans",color = 'gray28'),
        legend.key=element_rect(fill="white"),
        legend.position = c(0.175, 0.175)) +
  ylab(bquote('Left CA4/DG'~(mm^3))) +
  xlab('Negative Affect')
p1
ggsave("L_CA4_NegAff.png", type = "cairo")