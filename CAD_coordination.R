vision_indices_away = which(vision$direction == 'STIM_AWAY')
vision_indices_towards = which(vision$direction == 'STIM_TOWARDS')
vision_indices_control = which(vision$direction == 'STIM_NONE')


#install.packages("lme4")
library(lme4)
#install.packages("sjPlot")
#library(sjPlot) # table functions
#library(sjmisc) # sample data
require(lattice) # xy plots
library(car)

# for writing to log
con <- file("univariate_mechs_control_analysis.log")
sink(con, append=TRUE)

indices_slow = which(cad$cadence == '80BPM' & cad$direction != 'STIM_NONE')
indices_fast = which(cad$cadence == '110BPM'  & cad$direction != 'STIM_NONE')
indices_slow_control = which(cad$cadence == '80BPM' & cad$direction == 'STIM_NONE')
indices_fast_control = which(cad$cadence == '110BPM' & cad$direction == 'STIM_NONE')

# load data
cad = read.csv("~/R_wd/results_cad.csv", header = TRUE)


cop_from_com_x_integrated_inverted_mm_all <- cad$cop_from_com_x_integrated_sym_ONE * 1000
step_placement_x_inverted_mm_all <- cad$step_placement_x_sym_ONE * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_all <- cad$trigger_leg_ankle_dorsiflexion_integrated_doublestance_TWO * -1
subject_all = cad$subject
cadence_all = cad$cadence
direction_all = cad$direction
library(lme4)
library(lmerTest)
library(emmeans)
require(lattice)
emm_options(pbkrtest.limit = 10000)
number_of_simulations = 100

# Comprehensive LMER (includes all directions)
# CoP-Step
cop_step_model <- lmer(step_placement_x_inverted_mm_all ~ cop_from_com_x_integrated_inverted_mm_all*cadence_all + cop_from_com_x_integrated_inverted_mm_all*direction_all + (1 + cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_step_model <- anova(cop_step_model)
confint_cop_step_model <- confint(cop_step_model)

# COP-Push
cop_push_model <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_all ~  cop_from_com_x_integrated_inverted_mm_all*cadence_all + cop_from_com_x_integrated_inverted_mm_all*direction_all  + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_push_model <- anova(cop_push_model)
confint_cop_push_model <- confint(cop_push_model)

# Step-Push
step_push_model <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_all ~ step_placement_x_inverted_mm_all*cadence_all + step_placement_x_inverted_mm_all*direction_all  + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_push_model <- anova(step_push_model)
confint_step_push_model <- confint(step_push_model)