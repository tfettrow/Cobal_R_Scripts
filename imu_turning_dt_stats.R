library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
library(ggpubr)
library(magrittr)
library(dplyr) 
library(stringi)
library(lattice)
library(ggplot2)
library(ggsignif)
emm_options(lmerTest.limit = 1000000)
emm_options(pbkrtest.limit = 1000000)

# INSIDE WALKING ACC
setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
dat <- read.csv(file = 'avg_inside_walking_acc_table.txt')
lm_model <- lmer(acc ~ group +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group)
pval <- summary(lsmeans_results$contrasts)$p.value

file_name_tiff = paste0("Inside_Walking_Acc",".tiff")
file_path = file.path("figures",file_name_tiff)
ggplot(data=dat, aes(x = factor(group), y=acc)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
  ggtitle("Inside Walking Centrip ACC") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 11, color = "#1e1e1e"),
        axis.text.y=element_text(size = 11, color = "#1e1e1e"),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "#1e1e1e"),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
  geom_signif(y_position=c(0.025, 0.0225, 0.02, .0175, .015, .0125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
              annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                             round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
ggsave(file_path)


# OUTSIDE WALKING ACC
dat <- read.csv(file = 'avg_outside_walking_acc_table.txt')
lm_model <- lmer(acc ~ group +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group)
pval <- summary(lsmeans_results$contrasts)$p.value

file_name_tiff = paste0("Outside_Walking_Acc",".tiff")
file_path = file.path("figures",file_name_tiff)
ggplot(data=dat, aes(x = factor(group), y=acc)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
  ggtitle("Outside Walking Centrip ACC") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 11, color = "#1e1e1e"),
        axis.text.y=element_text(size = 11, color = "#1e1e1e"),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "#1e1e1e"),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
  geom_signif(y_position=c(0.025, 0.0225, 0.02, .0175, .015, .0125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
              annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                             round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
ggsave(file_path)


# INSIDE TURNING ACC
dat <- read.csv(file = 'avg_inside_turning_acc_table.txt')
lm_model <- lmer(acc ~ group +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group)
pval <- summary(lsmeans_results$contrasts)$p.value

file_name_tiff = paste0("Inside_Turning_Acc",".tiff")
file_path = file.path("figures",file_name_tiff)
ggplot(data=dat, aes(x = factor(group), y=acc)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
  ggtitle("Inside Turning Centrip ACC") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 11, color = "#1e1e1e"),
        axis.text.y=element_text(size = 11, color = "#1e1e1e"),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "bottom",
        legend.text = element_text(color = "#1e1e1e"),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
  geom_signif(y_position=c(0.25, 0.225, 0.2, .175, .15, .125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
              annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                             round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
ggsave(file_path)



# OUTSIDE TURNING ACC
dat <- read.csv(file = 'avg_outside_turning_acc_table.txt')
lm_model <- lmer(acc ~ group +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group)
pval <- summary(lsmeans_results$contrasts)$p.value

file_name_tiff = paste0("Outside_Turning_Acc",".tiff")
file_path = file.path("figures",file_name_tiff)
ggplot(data=dat, aes(x = factor(group), y=acc)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
  geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
  ggtitle("Outside Turning Centrip ACC") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
  axis.title = element_text(face="bold", color = "#1e1e1e"),
  axis.text.x=element_text(size = 11, color = "#1e1e1e"),
  axis.text.y=element_text(size = 11, color = "#1e1e1e"),
  axis.ticks = element_line(colour = '#1e1e1e', size = .5),
  legend.position = "bottom",
  legend.text = element_text(color = "#1e1e1e"),
  legend.background = element_rect(fill = "white"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white", color = "white"),
  axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
  geom_signif(y_position=c(0.25, 0.225, 0.2, .175, .15, .125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
              annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                           round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  
  # INSIDE SLOPE
  dat <- read.csv(file = 'slope_inside_table.txt')
  lm_model <- lmer(slope ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Inside_Slope",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=slope)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Inside Slope") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(0.0025, 0.00225, 0.002, .00175, .0015, .00125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)

  # OUTSIDE SLOPE
  dat <- read.csv(file = 'slope_outside_table.txt')
  lm_model <- lmer(slope ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Outside_Slope",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=slope)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Outside Slope") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(0.0025, 0.00225, 0.002, .00175, .0015, .00125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  
  # INSIDE Walk vs Turn Distance
  dat <- read.csv(file = 'avg_inside_WalkvTurn_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Inside_WalkvTurn_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Inside Walk vs Turn Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(175, 165, 155, 145, 135, 125), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  # OUTSIDE Walk vs Turn Distance
  dat <- read.csv(file = 'avg_outside_WalkvTurn_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Outside_WalkvTurn_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Outside Walk vs Turn Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(200, 190, 180, 170, 160, 150), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  
  # Inside within Walk Distance
  dat <- read.csv(file = 'avg_inside_wWalk_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Inside_wWalk_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Inside within Walk Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(9.5, 9, 8.5, 8, 7.5, 7), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  # Outside within Walk Distance
  dat <- read.csv(file = 'avg_outside_wWalk_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Outside_wWalk_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Outside within Walk Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(9.5, 9, 8.5, 8, 7.5, 7), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  
  # Inside within Turn Distance
  dat <- read.csv(file = 'avg_inside_wTurn_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Inside_wTurn_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Inside within Turn Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(75, 70, 65, 60, 55, 50), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)
  
  
  # Outside within Turn Distance
  dat <- read.csv(file = 'avg_outside_wTurn_Distance_table.txt')
  lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
  anova_lm_model <- anova(lm_model)
  rg_lm_model <- ref_grid(lm_model)
  confint_lm_model <- emmeans(rg_lm_model, "group")
  lsmeans_results <- lsmeans(lm_model,pairwise~group)
  pval <- summary(lsmeans_results$contrasts)$p.value
  
  file_name_tiff = paste0("Outside_wTurn_Distance",".tiff")
  file_path = file.path("figures",file_name_tiff)
  ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) + 
    geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
    ggtitle("Outside within Turn Distance") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
          axis.title = element_text(face="bold", color = "#1e1e1e"),
          axis.text.x=element_text(size = 11, color = "#1e1e1e"),
          axis.text.y=element_text(size = 11, color = "#1e1e1e"),
          axis.ticks = element_line(colour = '#1e1e1e', size = .5),
          legend.position = "bottom",
          legend.text = element_text(color = "#1e1e1e"),
          legend.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = "white"),
          axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) + 
    geom_signif(y_position=c(90, 85, 80, 75, 70, 65), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
                annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +  
                               round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")  
  ggsave(file_path)