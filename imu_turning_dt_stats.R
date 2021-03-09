library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
library(ggpubr)
library(magrittr)
library(plyr)
library(dplyr)
library(stringi)
library(lattice)
library(ggplot2)
library(ggsignif)
library(xtable)
library(tidyverse)
library(forcats)
library(stargazer)
library(huxtable)
library(readxl)

####################  2 Group IMU Analysis  ###########################

# INSIDE
# avg acc
setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
dat <- read.csv(file = '2grp_inside_table.txt')
lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
vc <- VarCorr(lm_model)
print(vc,comp=c("Variance","Std.Dev."),digits=2)
## variance only
print(vc,comp=c("Variance"))
as.data.frame(vc)
as.data.frame(vc,order="lower.tri")
# rg_lm_model <- ref_grid(lm_model)
# confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
pval <- summary(lsmeans_results$contrasts)$p.value

ant <- hux(anova_lm_model)
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)

ht <- hux(lsmeans_results$contrasts)
ht <- add_colnames(ht)
col_width(ht) <- .5
col_width(ht)
ht <- set_col_width(ht, 1, 1)
col_width(ht)
bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ht)

# change group names to be consistent across data (imu vs behav)=
revalue(dat$group, c("young" = "YA")) -> dat$group
revalue(dat$group, c("old" = "OA")) -> dat$group


file_name_pdf = paste0("2grp_Inside_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_Inside_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = group, y=acc, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  # geom_boxplot() +  #color="red", fill="orange", alpha=0.2
  # geom_jitter(position=position_jitter(0.25),color='#1e1e1e', alpha=.5)+
  ggtitle("Centripetal ACC - Inside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# INSIDE
# resid acc
file_name_pdf = paste0("2grp_Inside_Acc_resid",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_Inside_Acc_resid",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
ggplot(lm_model, aes(fill = reorder(condition,desc(condition)), x = group, y=.resid, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
ggtitle("Residuals Centripetal ACC - Inside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "residuals", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# OUTSIDE
# avg acc
setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
dat <- read.csv(file = '2grp_outside_table.txt')
lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
head(residuals(lm_model, level = 0:1))
# rg_lm_model <- ref_grid(lm_model)
# confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
pval <- summary(lsmeans_results$contrasts)$p.value

ant <- hux(anova_lm_model)
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)

ht <- hux(lsmeans_results$contrasts)
ht <- add_colnames(ht)
col_width(ht) <- .5
col_width(ht)
ht <- set_col_width(ht, 1, 1)
col_width(ht)
bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ht)


# change group names to be consistent across data (imu vs behav)=
revalue(dat$group, c("young" = "YA")) -> dat$group
revalue(dat$group, c("old" = "OA")) -> dat$group

file_name_pdf = paste0("2grp_Outside_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_Outside_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = group, y=acc, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  # geom_boxplot() +  #color="red", fill="orange", alpha=0.2
  # geom_jitter(position=position_jitter(0.25),color='#1e1e1e', alpha=.5)+
  ggtitle("Centripetal ACC - Outside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# OUTSIDE
# resid acc

file_name_pdf = paste0("2grp_Outside_Acc_resid",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_Outside_Acc_resid",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
# dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(lm_model, aes( fill = reorder(condition,desc(condition)), x = group, y=.resid, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Residuals Centripetal ACC - Outside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "residuals", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)

###### 2grp BEHAVIORAL DATA ###################

setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
#exclude the last subject (3001 vestibular ppt)
dt_data = read_excel(file.path("GABA_Aging_IMU_BehavioralData.xlsx"), cell_rows(1:69), sheet = 1, col_types = "text")

# adjusting for the subjects that were included in imu analysis
dt_data_subset = subset(dt_data, Participant_Num %in% c("1001","1002","1003","1004","1005","1006","1007","1008","1009","1010",
                                                        "1011","1012","1013","1014","1015","1016","1017","1018","1019","1020",
                                                        "1021","1022","1023","1024","1025","1026","1027","1028","1029","1030",
                                                        "1031","1032","1033","1034","1035","1036","1037","2001","2002","2003",
                                                        "2004","2005","2006","2007","2008","2010","2011","2012","2013","2014",
                                                        "2016","2017","2018","2020","2021","2023","2025","2026","2029","2030",
                                                        "2035","2036","2037","2038","2039"))

# change group names to be consistent across data (imu vs behav)=
revalue(dt_data_subset$Age_Group, c("Young" = "YA")) -> dt_data_subset$Age_Group
revalue(dt_data_subset$Age_Group, c("Old" = "OA")) -> dt_data_subset$Age_Group

TotAtt_DTCost_Avg <- (as.numeric(dt_data_subset$WWT_1_TotAtt_DTCost) + as.numeric(dt_data_subset$WWT_2_TotAtt_DTCost)) / 2
TotAcc_DTCost_Avg <- (as.numeric(dt_data_subset$WWT_1_Acc_DTCost) + as.numeric(dt_data_subset$WWT_2_Acc_DTCost)) / 2
dt_data_subset$TotAtt_DTCost_Avg <- TotAtt_DTCost_Avg
dt_data_subset$TotAcc_DTCost_Avg <- TotAcc_DTCost_Avg


file_name_pdf = paste0("2grp_DTC_Att",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_DTC_Att",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dt_data_subset$Age_Group <- factor(dt_data_subset$Age_Group)
dt_data_subset$Age_Group <- factor(dt_data_subset$Age_Group, levels = rev(levels(dt_data_subset$Age_Group)))
ggplot(dt_data_subset, aes(x = factor(Age_Group), y=TotAtt_DTCost_Avg, color=Age_Group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Dual-Task Cost - Attempts") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "DT Cost (# of Attempts)", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


file_name_pdf = paste0("2grp_DTC_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("2grp_DTC_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
ggplot(dt_data_subset, aes(x = factor(Age_Group), y=TotAcc_DTCost_Avg, color=Age_Group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Dual-Task Cost - Accuracy") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue")) +
  labs(x = " ", y = "DT Cost (% Accuracy)", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# check behavioral data for normality
ggdensity(dt_data_subset$TotAtt_DTCost_Avg,
          main = "Density plot of attempts",
          xlab = "Attempts")
ggqqplot(dt_data_subset$TotAtt_DTCost_Avg)
shapiro.test(dt_data_subset$TotAtt_DTCost_Avg)

ggdensity(dt_data_subset$TotAcc_DTCost_Avg,
          main = "Density plot of accuracy",
          xlab = "Accuracy")
ggqqplot(dt_data_subset$TotAcc_DTCost_Avg)
shapiro.test(dt_data_subset$TotAcc_DTCost_Avg)

# using non-parametric due to non-normality ^^
results_att <- wilcox.test(TotAtt_DTCost_Avg ~ Age_Group, data = dt_data_subset, exact = FALSE)
results_acc <- wilcox.test(TotAcc_DTCost_Avg ~ Age_Group, data = dt_data_subset, exact = FALSE)

anova_lm_model <- anova(lm_model)
head(residuals(lm_model, level = 0:1))
# rg_lm_model <- ref_grid(lm_model)
# confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
pval <- summary(lsmeans_results$contrasts)$p.value

ant <- hux(results_att)

ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


####################  3 Group IMU Analysis  ###########################

# INSIDE
# avg acc
setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
dat <- read.csv(file = '3grp_inside_table.txt')
lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
vc <- VarCorr(lm_model)
print(vc,comp=c("Variance","Std.Dev."),digits=2)
## variance only
print(vc,comp=c("Variance"))
as.data.frame(vc)
as.data.frame(vc,order="lower.tri")
# rg_lm_model <- ref_grid(lm_model)
# confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
pval <- summary(lsmeans_results$contrasts)$p.value

ant <- hux(anova_lm_model)
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)

ht <- hux(lsmeans_results$contrasts)
ht <- add_colnames(ht)
col_width(ht) <- .5
col_width(ht)
ht <- set_col_width(ht, 1, 1)
col_width(ht)
bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ht)

# change group names to be consistent across data (imu vs behav)=
revalue(dat$group, c("young" = "YA")) -> dat$group
revalue(dat$group, c("old" = "OA")) -> dat$group
revalue(dat$group, c("neuro" = "OA-neuro")) -> dat$group


file_name_pdf = paste0("3grp_Inside_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_Inside_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = group, y=acc, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  # geom_boxplot() +  #color="red", fill="orange", alpha=0.2
  # geom_jitter(position=position_jitter(0.25),color='#1e1e1e', alpha=.5)+
  ggtitle("Centripetal ACC - Inside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue","green")) +
  labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# INSIDE
# resid acc
file_name_pdf = paste0("3grp_Inside_Acc_resid",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_Inside_Acc_resid",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
ggplot(lm_model, aes(fill = reorder(condition,desc(condition)), x = group, y=.resid, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Residuals Centripetal ACC - Inside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue","green")) +
  labs(x = " ", y = "residuals", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# OUTSIDE
# avg acc
setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
dat <- read.csv(file = '3grp_outside_table.txt')
lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
head(residuals(lm_model, level = 0:1))
# rg_lm_model <- ref_grid(lm_model)
# confint_lm_model <- emmeans(rg_lm_model, "group")
lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
pval <- summary(lsmeans_results$contrasts)$p.value

ant <- hux(anova_lm_model)
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)

ht <- hux(lsmeans_results$contrasts)
ht <- add_colnames(ht)
col_width(ht) <- .5
col_width(ht)
ht <- set_col_width(ht, 1, 1)
col_width(ht)
bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ht)


# change group names to be consistent across data (imu vs behav)=
revalue(dat$group, c("young" = "YA")) -> dat$group
revalue(dat$group, c("old" = "OA")) -> dat$group
revalue(dat$group, c("neuro" = "OA-neuro")) -> dat$group

file_name_pdf = paste0("3grp_Outside_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_Outside_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = group, y=acc, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  # geom_boxplot() +  #color="red", fill="orange", alpha=0.2
  # geom_jitter(position=position_jitter(0.25),color='#1e1e1e', alpha=.5)+
  ggtitle("Centripetal ACC - Outside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue","green")) +
  labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


# OUTSIDE
# resid acc

file_name_pdf = paste0("3grp_Outside_Acc_resid",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_Outside_Acc_resid",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
# dat$group <- factor(dat$group, levels = rev(levels(dat$group)))
ggplot(lm_model, aes( fill = reorder(condition,desc(condition)), x = group, y=.resid, color=group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Residuals Centripetal ACC - Outside Foot") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_fill_manual(values=alpha(c("white","black"),.75)) +
  scale_color_manual(values=c("orange", "blue","green")) +
  labs(x = " ", y = "residuals", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)


###### 3grp BEHAVIORAL DATA ###################

setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
#exclude the last subject (3001 vestibular ppt)
dt_data = read_excel(file.path("GABA_Aging_IMU_BehavioralData.xlsx"), cell_rows(1:69), sheet = 1, col_types = "text")

# adjusting for the subjects that were included in imu analysis
dt_data_subset = subset(dt_data, Participant_Num %in% c("1001","1002","1003","1004","1005","1006","1007","1008","1009","1010",
                                                        "1011","1012","1013","1014","1015","1016","1017","1018","1019","1020",
                                                        "1021","1022","1023","1024","1025","1026","1027","1028","1029","1030",
                                                        "1031","1032","1033","1034","1035","1036","1037","2001","2002","2003",
                                                        "2004","2005","2006","2007","2008","2010","2011","2012","2013","2014",
                                                        "2016","2017","2018","2020","2021","2023","2025","2026","2029","2030",
                                                        "2035","2036","2037","2038","2039"))

# change group names to be consistent across data (imu vs behav)=
revalue(dt_data_subset$TwoPD_Neuropathy_Group, c("None_YA" = "YA")) -> dt_data_subset$TwoPD_Neuropathy_Group
revalue(dt_data_subset$TwoPD_Neuropathy_Group, c("None_OA" = "OA")) -> dt_data_subset$TwoPD_Neuropathy_Group
revalue(dt_data_subset$TwoPD_Neuropathy_Group, c("Neuro" = "OA-neuro")) -> dt_data_subset$TwoPD_Neuropathy_Group

TotAtt_DTCost_Avg <- (as.numeric(dt_data_subset$WWT_1_TotAtt_DTCost) + as.numeric(dt_data_subset$WWT_2_TotAtt_DTCost)) / 2
TotAcc_DTCost_Avg <- (as.numeric(dt_data_subset$WWT_1_Acc_DTCost) + as.numeric(dt_data_subset$WWT_2_Acc_DTCost)) / 2
dt_data_subset$TotAtt_DTCost_Avg <- TotAtt_DTCost_Avg
dt_data_subset$TotAcc_DTCost_Avg <- TotAcc_DTCost_Avg

file_name_pdf = paste0("3grp_DTC_Att",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_DTC_Att",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
dt_data_subset$TwoPD_Neuropathy_Group <- factor(dt_data_subset$TwoPD_Neuropathy_Group, levels = c("YA","OA","OA-neuro"))
#dt_data_subset$TwoPD_Neuropathy_Group <- factor(dt_data_subset$TwoPD_Neuropathy_Group, levels = rev(levels(dt_data_subset$TwoPD_Neuropathy_Group)))
ggplot(dt_data_subset, aes(x = factor(TwoPD_Neuropathy_Group), y=TotAtt_DTCost_Avg, color=TwoPD_Neuropathy_Group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Dual-Task Cost - Attempts") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_color_manual(values=c("orange", "blue", "green")) +
  labs(x = " ", y = "DT Cost (# of Attempts)", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)

file_name_pdf = paste0("3grp_DTC_Acc",".pdf")
file_path_pdf = file.path("figures",file_name_pdf)
file_name_tiff = paste0("3grp_DTC_Acc",".tiff")
file_path_tiff = file.path("figures",file_name_tiff)
ggplot(dt_data_subset, aes(x = factor(TwoPD_Neuropathy_Group), y=TotAcc_DTCost_Avg, color=TwoPD_Neuropathy_Group)) +
  stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1) +
  ggtitle("Dual-Task Cost - Accuracy") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
        axis.title = element_text(face="bold", color = "#1e1e1e"),
        axis.text.x=element_text(size = 14, color = "#1e1e1e"),
        axis.text.y=element_text(size = 14, color = "#1e1e1e"),
        axis.title.y = element_text(size = 14),
        axis.ticks = element_line(colour = '#1e1e1e', size = .5),
        legend.position = "top",
        legend.text = element_text(color = "#1e1e1e", size = 14),
        legend.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
  scale_color_manual(values=c("orange", "blue", "green")) +
  labs(x = " ", y = "DT Cost (% Accuracy)", fill = " ", color = " ", size = 14)
dev.off()
ggsave(file_path_pdf)
ggsave(file_path_tiff)

# using non-parametric due to non-normality ^^
results_att <- kruskal.test(TotAtt_DTCost_Avg ~ TwoPD_Neuropathy_Group, data = dt_data_subset)
results_acc <- kruskal.test(TotAcc_DTCost_Avg ~ TwoPD_Neuropathy_Group, data = dt_data_subset)

results_att <- wilcox.test(TotAtt_DTCost_Avg ~ TwoPD_Neuropathy_Group, data = dt_data_subset, exact = FALSE)
results_acc <- wilcox.test(TotAcc_DTCost_Avg ~ TwoPD_Neuropathy_Group, data = dt_data_subset, exact = FALSE)
###### LEGACY (since 20201215) #########

#
# # INSIDE WALKING ACC
# setwd("Z:/Shared drives/GABA_Aging_IMU/PROCESSED")
# dat <- read.csv(file = 'avg_inside_walking_acc_table.txt')
# lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
# anova_lm_model <- anova(lm_model)
# # rg_lm_model <- ref_grid(lm_model)
# # confint_lm_model <- emmeans(rg_lm_model, "group")
# lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
# pval <- summary(lsmeans_results$contrasts)$p.value
#
# ant <- hux(anova_lm_model)
# ant <- add_rownames(ant)
# col_width(ant) <- .5
# col_width(ant)
# ant <- set_col_width(ant, 1, 1)
# col_width(ant)
# bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
# print_latex(ant)
#
# ht <- hux(lsmeans_results$contrasts)
# ht <- add_colnames(ht)
# col_width(ht) <- .5
# col_width(ht)
# ht <- set_col_width(ht, 1, 1)
# col_width(ht)
# bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
# file_name_pdf = paste0("Inside_Walking_Acc",".pdf")
# file_path_pdf = file.path("figures",file_name_pdf)
# file_name_tiff = paste0("Inside_Walking_Acc",".tiff")
# file_path_tiff = file.path("figures",file_name_tiff)
# ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=acc, color=group)) +
#   stat_boxplot(geom ="errorbar", stat_params = list(width = 1), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
# # geom_boxplot() +  #color="red", fill="orange", alpha=0.2
#   # geom_jitter(position=position_jitter(0.25),color='#1e1e1e', alpha=.5)+
#   ggtitle("Walking Centripetal ACC - Inside Foot") +
#   theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#         axis.title = element_text(face="bold", color = "#1e1e1e"),
#         axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#         axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#         axis.title.y = element_text(size = 14),
#         axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#         legend.position = "top",
#         legend.text = element_text(color = "#1e1e1e", size = 14),
#         legend.background = element_rect(fill = "white"),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white", color = "white"),
#         axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#         scale_fill_manual(values=alpha(c("white","black"),.5)) +
#         labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
#        ggsave(file_path_pdf)
#         ggsave(file_path_tiff)
#
#         print_latex(ht)
#
#
# # OUTSIDE WALKING ACC
# dat <- read.csv(file = 'avg_outside_walking_acc_table.txt')
# lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
# anova_lm_model <- anova(lm_model)
# # rg_lm_model <- ref_grid(lm_model)
# # confint_lm_model <- emmeans(rg_lm_model, "group")
# lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
# pval <- summary(lsmeans_results$contrasts)$p.value
#
# ant <- hux(anova_lm_model)
# ant <- add_rownames(ant)
# col_width(ant) <- .5
# col_width(ant)
# ant <- set_col_width(ant, 1, 1)
# col_width(ant)
# bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
# print_latex(ant)
#
#
# ht <- hux(lsmeans_results$contrasts)
# ht <- add_colnames(ht)
# col_width(ht) <- .5
# col_width(ht)
# ht <- set_col_width(ht, 1, 1)
# col_width(ht)
# bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#
# file_name_pdf = paste0("Outside_Walking_Acc",".pdf")
# file_path_pdf = file.path("figures",file_name_pdf)
# file_name_tiff = paste0("Outside_Walking_Acc",".tiff")
# file_path_tiff = file.path("figures",file_name_tiff)
# ggplot(data=dat, aes(fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=acc, color=group)) +
#   stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#   # geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   # geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#   ggtitle("Walking Centripetal ACC - Outside Foot") +
#   theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#         axis.title = element_text(face="bold", color = "#1e1e1e"),
#         axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#         axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#         axis.title.y = element_text(size = 14),
#         axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#         legend.position = "top",
#         legend.text = element_text(color = "#1e1e1e", size = 14),
#         legend.background = element_rect(fill = "white"),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white", color = "white"),
#         axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid"))  +
#   scale_fill_manual(values=alpha(c("white","black"),.5)) +
#   labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
# ggsave(file_path_pdf)
# ggsave(file_path_tiff)
# print_latex(ht)
#
#
# # INSIDE TURNING ACC
# dat <- read.csv(file = 'avg_inside_turning_acc_table.txt')
# lm_model <- lmer(acc ~ group*condition + (1|subject), data=dat, REML = FALSE)
# anova_lm_model <- anova(lm_model)
# # rg_lm_model <- ref_grid(lm_model)
# # confint_lm_model <- emmeans(rg_lm_model, "group")
# lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
# pval <- summary(lsmeans_results$contrasts)$p.value
#
# ant <- hux(anova_lm_model)
# ant <- add_rownames(ant)
# col_width(ant) <- .5
# col_width(ant)
# ant <- set_col_width(ant, 1, 1)
# col_width(ant)
# bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
# print_latex(ant)
#
#
# ht <- hux(lsmeans_results$contrasts)
# ht <- add_colnames(ht)
# col_width(ht) <- .5
# col_width(ht)
# ht <- set_col_width(ht, 1, 1)
# col_width(ht)
# bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#
# file_name_pdf = paste0("Inside_Turning_Acc",".pdf")
# file_path_pdf = file.path("figures",file_name_pdf)
# file_name_tiff = paste0("Inside_Turning_Acc",".tiff")
# file_path_tiff = file.path("figures",file_name_tiff)
# ggplot(data=dat, aes(fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=acc, color=group)) +
#   stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
# # + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   # geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#   ggtitle("Turning Centripetal ACC - Inside Foot") +
#   theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#         axis.title = element_text(face="bold", color = "#1e1e1e"),
#         axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#         axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#         axis.title.y = element_text(size = 14),
#         axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#         legend.position = "top",
#         legend.text = element_text(color = "#1e1e1e", size = 14),
#         legend.background = element_rect(fill = "white"),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white", color = "white"),
#         axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#   scale_fill_manual(values=alpha(c("white","black"),.5)) +
#   labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
# ggsave(file_path_pdf)
# ggsave(file_path_tiff)
#
# print_latex(ht)
#
#
#
# # OUTSIDE TURNING ACC
# dat <- read.csv(file = 'avg_outside_turning_acc_table.txt')
# lm_model <- lmer(acc ~ group*condition +  (1|subject), data=dat, REML = FALSE)
# anova_lm_model <- anova(lm_model)
# # rg_lm_model <- ref_grid(lm_model)
# # confint_lm_model <- emmeans(rg_lm_model, "group")
# lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
# pval <- summary(lsmeans_results$contrasts)$p.value
#
# ant <- hux(anova_lm_model)
# ant <- add_rownames(ant)
# col_width(ant) <- .5
# col_width(ant)
# ant <- set_col_width(ant, 1, 1)
# col_width(ant)
# bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
# print_latex(ant)
#
#
# ht <- hux(lsmeans_results$contrasts)
# ht <- add_colnames(ht)
# col_width(ht) <- .5
# col_width(ht)
# ht <- set_col_width(ht, 1, 1)
# col_width(ht)
# bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
# file_name_pdf = paste0("Outside_Turning_Acc",".pdf")
# file_path_pdf = file.path("figures",file_name_pdf)
# file_name_tiff = paste0("Outside_Turning_Acc",".tiff")
# file_path_tiff = file.path("figures",file_name_tiff)
# # ggplot(data=dat, aes(x = factor(group), y=acc)) +
# ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=acc, color=group)) +
#   stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
# # + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   # geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#   ggtitle("Turning Centripetal ACC - Outside Foot") +
#   theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#   axis.title = element_text(face="bold", color = "#1e1e1e"),
#   axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#   axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#   axis.title.y = element_text(size = 14),
#   axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#   legend.position = "top",
#   legend.text = element_text(color = "#1e1e1e", size = 14),
#   legend.background = element_rect(fill = "white"),
#   panel.border = element_blank(),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.background = element_rect(fill = "white"),
#   plot.background = element_rect(fill = "white", color = "white"),
#   axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#   scale_fill_manual(values=alpha(c("white","black"),.5)) +
#   labs(x = " ", y = "centripetal acceleration", fill = " ", color = " ", size = 14)
#   ggsave(file_path_pdf)
#   ggsave(file_path_tiff)
#
#   print_latex(ht)
#
#
#
#   # INSIDE SLOPE
#   dat <- read.csv(file = 'slope_inside_table.txt')
#   lm_model <- lmer(slope ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#   file_name_pdf = paste0("Inside_Slope",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Inside_Slope",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=slope, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#   # ggplot(data=dat, aes(x = factor(group), y=slope)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#     ggtitle("Walk-Turn Slope - Inside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#     scale_fill_manual(values=alpha(c("white","black"),.5)) +
#     labs(x = " ", y = "slope", fill = " ", color = " ", size = 14)
#   ggsave(file_path_pdf)
#   ggsave(file_path_tiff)
#
#   print_latex(ht)
#
#
#   # OUTSIDE SLOPE
#   dat <- read.csv(file = 'slope_outside_table.txt')
#   lm_model <- lmer(slope ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#   file_name_pdf = paste0("Outside_Slope",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Outside_Slope",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=slope, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#   # ggplot(data=dat, aes(x = factor(group), y=slope)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#     ggtitle("Walk-Turn Slope - Outside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#     scale_fill_manual(values=alpha(c("white","black"),.5)) +
#     labs(x = " ", y = "slope", fill = " ", color = " ", size = 14)
#    ggsave(file_path_pdf)
#   ggsave(file_path_tiff)
#
#   print_latex(ht)
#
#
#
#   # INSIDE Walk vs Turn Distance
#   dat <- read.csv(file = 'avg_inside_WalkvTurn_Distance_table.txt')
#   lm_model <- lmer(distance ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#   file_name_pdf = paste0("Inside_WalkvTurn_Distance",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Inside_WalkvTurn_Distance",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=distance, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#   # ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#     ggtitle("Walk-Turn Distance - Inside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#     scale_fill_manual(values=alpha(c("white","black"),.5)) +
#     labs(x = " ", y = "distance", fill = " ", color = " ", size = 14)
#   ggsave(file_path_pdf)
#   ggsave(file_path_tiff)
#
#   print_latex(ht)
#
#
#   # OUTSIDE Walk vs Turn Distance
#   dat <- read.csv(file = 'avg_outside_WalkvTurn_Distance_table.txt')
#   lm_model <- lmer(distance ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#
#   file_name_pdf = paste0("Outside_WalkvTurn_Distance",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Outside_WalkvTurn_Distance",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=distance, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#   # ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#     ggtitle("Walk-Turn Distance - Outside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=1, linetype = "solid")) +
#     scale_fill_manual(values=alpha(c("white","black"),.5)) +
#     labs(x = " ", y = "distance", fill = " ", color = " ", size = 14)
#   ggsave(file_path_pdf)
#   ggsave(file_path_tiff)
#
#   print_latex(ht)
#
#
#   ### old stuff vvv x####
#   #
#   # # Inside within Walk Distance
#   # dat <- read.csv(file = 'avg_inside_wWalk_Distance_table.txt')
#   # lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
#   # anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   # lsmeans_results <- lsmeans(lm_model,pairwise~group)
#   # pval <- summary(lsmeans_results$contrasts)$p.value
#   #
#   #
#   # file_name_pdf = paste0("Inside_wWalk_Distance",".pdf")
#   # file_path_pdf = file.path("figures",file_name_pdf)
#   # file_name_tiff = paste0("Inside_wWalk_Distance",".tiff")
#   # file_path_tiff = file.path("figures",file_name_tiff)
#   # ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#   #   ggtitle("Inside within Walk Distance") +
#   #   theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#   #         axis.title = element_text(face="bold", color = "#1e1e1e"),
#   #         axis.text.x=element_text(size = 11, color = "#1e1e1e"),
#   #         axis.text.y=element_text(size = 11, color = "#1e1e1e"),
#   #         axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#   #         legend.position = "bottom",
#   #         legend.text = element_text(color = "#1e1e1e"),
#   #         legend.background = element_rect(fill = "white"),
#   #         panel.border = element_blank(),
#   #         panel.grid.major = element_blank(),
#   #         panel.grid.minor = element_blank(),
#   #         panel.background = element_rect(fill = "white"),
#   #         plot.background = element_rect(fill = "white", color = "white"),
#   #         axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) +
#   #   geom_signif(y_position=c(9.5, 9, 8.5, 8, 7.5, 7), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
#   #               annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +
#   #                              round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")
#   # ggsave(file_path_pdf)
#   # ggsave(file_path_tiff)
#   #
#   #
#   # # Outside within Walk Distance
#   # dat <- read.csv(file = 'avg_outside_wWalk_Distance_table.txt')
#   # lm_model <- lmer(distance ~ group +  (1|subject), data=dat, REML = FALSE)
#   # anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   # lsmeans_results <- lsmeans(lm_model,pairwise~group)
#   # pval <- summary(lsmeans_results$contrasts)$p.value
#   #
#   # file_name_pdf = paste0("Outside_wWalk_Distance",".pdf")
#   # file_path = file.path("figures",file_name_pdf)
#   # ggplot(data=dat, aes(x = factor(group), y=distance)) + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#   #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#   #   ggtitle("Outside within Walk Distance") +
#   #   theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#   #         axis.title = element_text(face="bold", color = "#1e1e1e"),
#   #         axis.text.x=element_text(size = 11, color = "#1e1e1e"),
#   #         axis.text.y=element_text(size = 11, color = "#1e1e1e"),
#   #         axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#   #         legend.position = "bottom",
#   #         legend.text = element_text(color = "#1e1e1e"),
#   #         legend.background = element_rect(fill = "white"),
#   #         panel.border = element_blank(),
#   #         panel.grid.major = element_blank(),
#   #         panel.grid.minor = element_blank(),
#   #         panel.background = element_rect(fill = "white"),
#   #         plot.background = element_rect(fill = "white", color = "white"),
#   #         axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) +
#   #   geom_signif(y_position=c(9.5, 9, 8.5, 8, 7.5, 7), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
#   #               annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +
#   #                              round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")
#   # ggsave(file_path)
#   #
#
#   # Inside within Turn Distance
#   dat <- read.csv(file = 'avg_inside_wTurn_Distance_table.txt')
#   lm_model <- lmer(distance ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ht)
#
#   file_name_pdf = paste0("Inside_wTurn_Distance",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Inside_wTurn_Distance",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=distance, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#     # + geom_boxplot(color="red", fill="orange", alpha=0.2) +
#     #   geom_jitter(position=position_jitter(0.2),color='#1e1e1e', alpha=.5)+
#     ggtitle("Turn Distance - Inside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) +
#           scale_fill_manual(values=alpha(c("white","black"),.5)) +
#           labs(x = " ", y = "distance", fill = " ", color = " ", size = 14)
#     ggsave(file_path_pdf)
#     ggsave(file_path_tiff)
#
#
#   # Outside within Turn Distance
#   dat <- read.csv(file = 'avg_outside_wTurn_Distance_table.txt')
#   lm_model <- lmer(distance ~ group*condition +  (1|subject), data=dat, REML = FALSE)
#   anova_lm_model <- anova(lm_model)
#   # rg_lm_model <- ref_grid(lm_model)
#   # confint_lm_model <- emmeans(rg_lm_model, "group")
#   lsmeans_results <- lsmeans(lm_model,pairwise~group*condition)
#   pval <- summary(lsmeans_results$contrasts)$p.value
#
#   ant <- hux(anova_lm_model)
#   ant <- add_rownames(ant)
#   col_width(ant) <- .5
#   col_width(ant)
#   ant <- set_col_width(ant, 1, 1)
#   col_width(ant)
#   bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ant)
#
#   ht <- hux(lsmeans_results$contrasts)
#   ht <- add_colnames(ht)
#   col_width(ht) <- .5
#   col_width(ht)
#   ht <- set_col_width(ht, 1, 1)
#   col_width(ht)
#   bottom_border(ht)[1, ] <- brdr(0.4, "solid", "blue")
#   print_latex(ht)
#
#   file_name_pdf = paste0("Outside_wTurn_Distance",".pdf")
#   file_path_pdf = file.path("figures",file_name_pdf)
#   file_name_tiff = paste0("Outside_wTurn_Distance",".tiff")
#   file_path_tiff = file.path("figures",file_name_tiff)
#   ggplot(data=dat, aes( fill = reorder(condition,desc(condition)), x = reorder(group, desc(group)), y=distance, color=group)) +
#     stat_boxplot(geom ="errorbar", stat_params = list(width = 0.25), geom_params = list(size = 2)) + geom_boxplot(lwd=1.5) +
#     ggtitle("Turn Distance - Outside Foot") +
#     theme(plot.title = element_text(hjust = 0.5, size = 18, family = "Tahoma", face = "bold", color = "#1e1e1e"),
#           axis.title = element_text(face="bold", color = "#1e1e1e"),
#           axis.text.x=element_text(size = 14, color = "#1e1e1e"),
#           axis.text.y=element_text(size = 14, color = "#1e1e1e"),
#           axis.title.y = element_text(size = 14),
#           axis.ticks = element_line(colour = '#1e1e1e', size = .5),
#           legend.position = "top",
#           legend.text = element_text(color = "#1e1e1e", size = 14),
#           legend.background = element_rect(fill = "white"),
#           panel.border = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(fill = "white"),
#           plot.background = element_rect(fill = "white", color = "white"),
#           axis.line = element_line(colour = "#1e1e1e",size=0.1, linetype = "dotted")) +
#           scale_fill_manual(values=alpha(c("white","black"),.5)) +
#           labs(x = " ", y = "distance", fill = " ", color = " ", size = 14)
#   ggsave(file_path_pdf)
#   ggsave(file_path_tiff)



  #   geom_signif(y_position=c(90, 85, 80, 75, 70, 65), xmin=c(1, 1, 1, 2, 2, 3), xmax=c(2, 3, 4, 3, 4, 4),
  #               annotation=c(round(pval[1]/.01)*.01, round(pval[2]/.01)*.01, round(pval[3]/.01)*.01, round(pval[4]/.01)*.01, +
  #                              round(pval[5]/.01)*.01, round(pval[6]/.01)*.01), tip_length=0.025, color="#1e1e1e")
  # ggsave(file_path)
