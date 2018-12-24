# create indices
vision_indices_away = which(vision$direction == 'STIM_AWAY')
vision_indices_towards = which(vision$direction == 'STIM_TOWARDS')
vision_indices_control = which(vision$direction == 'STIM_NONE')


#install.packages("lme4")
library(lme4)
#install.packages("sjPlot")
#library(sjPlot) # table functions
#library(sjmisc) # sample data
require(lattice) # xy plots

data_root = getwd()

# for writing to log
con <- file("univariate_mechs_control_analysis.log")
sink(con, append=TRUE)

com_x_inverted_pushoff_end_mm_towards <- vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] * 1000
com_x_vel_inverted_pushoff_end_mm_towards <- vision$com_x_vel_inverted_pushoff_end_TWO[vision_indices_towards] * 1000
cop_from_com_x_integrated_inverted_mm_towards <- vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards]  * 1000
step_placement_x_inverted_mm_towards <- vision$step_placement_x_inverted_ONE[vision_indices_towards] * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_towards <- vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards] * -57.29
step_time_ONE_towards = vision$step_time_response_ONE[vision_indices_towards] * 1000
step_time_TWO_towards = vision$step_time_response_TWO[vision_indices_towards] * 1000
step_length_ONE_towards = vision$step_length_response_ONE[vision_indices_towards] * 1000
step_length_TWO_towards = vision$step_length_response_TWO[vision_indices_towards] * 1000
fy_towards = vision$fy_band_end_ONE[vision_indices_towards]
subject_towards = vision$subject[vision_indices_towards]


cop_from_com_x_integrated_inverted_mm_away <- vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] * 1000
step_placement_x_inverted_mm_away <- vision$step_placement_x_inverted_ONE[vision_indices_away] * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_away <- vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] * -57.29
step_time_ONE_away = vision$step_time_response_ONE[vision_indices_away]* 1000
step_time_TWO_away = vision$step_time_response_TWO[vision_indices_away]* 1000
step_length_ONE_away = vision$step_length_response_ONE[vision_indices_away]* 1000
step_length_TWO_away = vision$step_length_response_TWO[vision_indices_away]* 1000
fy_away = vision$fy_band_end_ONE[vision_indices_away]
subject_away = vision$subject[vision_indices_away]

cop_from_com_x_integrated_inverted_mm_control <- vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_control] * 1000
step_placement_x_inverted_mm_control <- vision$step_placement_x_inverted_ONE[vision_indices_control] * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_control <- vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_control] * -57.29
step_time_ONE_control = vision$step_time_response_ONE[vision_indices_control]* 1000
step_time_TWO_control = vision$step_time_response_TWO[vision_indices_control]* 1000
step_length_ONE_control = vision$step_length_response_ONE[vision_indices_control]* 1000
step_length_TWO_control = vision$step_length_response_TWO[vision_indices_control]* 1000
fy_control = vision$fy_band_end_ONE[vision_indices_control]
subject_control = vision$subject[vision_indices_control]

cop_from_com_x_integrated_inverted_mm_all <- vision$cop_from_com_x_integrated_inverted_ONE* 1000
step_placement_x_inverted_mm_all <- vision$step_placement_x_inverted_ONE * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_all <- vision$trigger_leg_ankle_dorsiflexion_integrated_TWO * -57.29
step_time_ONE_all = vision$step_time_response_ONE* 1000
step_time_TWO_all = vision$step_time_response_TWO* 1000
step_length_ONE_all = vision$step_length_response_ONE* 1000
step_length_TWO_all = vision$step_length_response_TWO* 1000
fy_all = vision$fy_band_end_ONE
subject_all = vision$subject
direction_all = vision$direction


# COP-STEP
# towards
cop_step_towards_model_exp = lmer(step_placement_x_inverted_mm_towards ~ cop_from_com_x_integrated_inverted_mm_towards + (1|subject_towards), REML=FALSE)

summary(cop_step_towards_model_exp)
confint(cop_step_towards_model_exp)


# iPO - STEP
# towards
iPO_step_towards_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_towards ~ step_placement_x_inverted_mm_towards + (1|subject_towards), REML=FALSE)

summary(iPO_step_towards_model_exp)
confint(iPO_step_towards_model_exp)

# iPO - Ankle
# towards

iPO_cop_towards_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_towards ~ cop_from_com_x_integrated_inverted_mm_towards + (1|subject_towards), REML=FALSE)

summary(iPO_cop_towards_model_exp)
confint(iPO_cop_towards_model_exp)


# iPO- step time 1
# towards

iPO_time1_towards_model_exp = lmer(step_time_ONE_towards ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)

summary(iPO_time1_towards_model_exp)
confint(iPO_time1_towards_model_exp)


# iPO - step time 2
# towards

iPO_time2_towards_model_exp = lmer(step_time_TWO_towards  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)

summary(iPO_time2_towards_model_exp)
confint(iPO_time2_towards_model_exp)


# iPO - step length 1
# towards

iPO_length1_towards_model_exp = lmer(step_length_ONE_towards  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)

summary(iPO_length1_towards_model_exp)
confint(iPO_length1_towards_model_exp)


# iPO - step length 2
# towards

iPO_length2_towards_model_exp = lmer( step_length_TWO_towards  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)

summary(iPO_length2_towards_model_exp)
confint(iPO_length2_towards_model_exp)

# iPO - fy
# towards

iPO_fy_towards_model_exp = lmer(fy_towards  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)

summary(iPO_fy_towards_model_exp)
confint(iPO_fy_towards_model_exp)

# Inidivudal Interdependence Plots
# towards
xyplot(vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] ~ vision$step_placement_x_inverted_ONE[vision_indices_towards] | vision$subject[vision_indices_towards], 
       main="lateral ankle vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. CoP-CoM (m s)")


xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards] ~ vision$step_placement_x_inverted_ONE[vision_indices_towards] | vision$subject[vision_indices_towards],
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] | vision$subject[vision_indices_towards],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards] ~ vision$step_time_response_TWO[vision_indices_towards],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")


xyplot(step_time_TWO_towards ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards,
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")

# Direction Away Models

# COP-STEP 
# away
cop_step_away_model_exp = lmer(step_placement_x_inverted_mm_away ~ cop_from_com_x_integrated_inverted_mm_away + (1|subject_away), REML=FALSE)

summary(cop_step_away_model_exp)
confint(cop_step_away_model_exp)

# iPO - STEP
# away
iPO_step_away_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_away ~ step_placement_x_inverted_mm_away + (1|subject_away), REML=FALSE)

summary(iPO_step_away_model_exp)
confint(iPO_step_away_model_exp)

# iPO - Ankle
# away

iPO_cop_away_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_away ~ cop_from_com_x_integrated_inverted_mm_away + (1|subject_away), REML=FALSE)

summary(iPO_cop_away_model_exp)
confint(iPO_cop_away_model_exp)


# iPO- step time 1
# away

iPO_time1_away_model_exp = lmer(step_time_ONE_away ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

summary(iPO_time1_away_model_exp)
confint(iPO_time1_away_model_exp)


# iPO - step time 2
# away

iPO_time2_away_model_exp = lmer(step_time_TWO_away  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

summary(iPO_time2_away_model_exp)
confint(iPO_time2_away_model_exp)


# iPO - step length 1
# away

iPO_length1_away_model_exp = lmer(step_length_ONE_away  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

summary(iPO_length1_away_model_exp)
confint(iPO_length1_away_model_exp)


# iPO - step length 2
# away
iPO_length2_away_model_exp = lmer( step_length_TWO_away  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

summary(iPO_length2_away_model_exp)
confint(iPO_length2_away_model_exp)

# iPO - fy
# away
iPO_fy_away_model_exp = lmer(fy_away  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

summary(iPO_fy_away_model_exp)
confint(iPO_fy_away_model_exp)


# Inidivudal Interdependence Plots
# away
xyplot(vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] ~ vision$step_placement_x_inverted_ONE[vision_indices_away] | vision$subject[vision_indices_away], 
       main="lateral ankle vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. CoP-CoM (m s)")


xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] ~ vision$step_placement_x_inverted_ONE[vision_indices_away] | vision$subject[vision_indices_away],
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] | vision$subject[vision_indices_away],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] ~ vision$step_time_response_TWO[vision_indices_away],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(step_time_TWO_away ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away | vision$subject[vision_indices_away],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")


# Direction Control Models

# COP-STEP 
# control
#cop_step_control_model_null = lmer(step_placement_x_inverted_mm_control  ~  (1+cop_from_com_x_integrated_inverted_mm_control|subject_control), REML=FALSE)
cop_step_control_model_exp = lmer(step_placement_x_inverted_mm_control ~ cop_from_com_x_integrated_inverted_mm_control + (1|subject_control), REML=FALSE)
summary(cop_step_control_model_exp)
confint(cop_step_control_model_exp)

# iPO - STEP
# control
iPO_step_control_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_control ~ step_placement_x_inverted_mm_control + (1|subject_control), REML=FALSE)
summary(iPO_step_control_model_exp)
confint(iPO_step_control_model_exp)

# iPO - Ankle
# control
iPO_cop_control_model_exp = lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_control ~ cop_from_com_x_integrated_inverted_mm_control + (1|subject_control), REML=FALSE)
summary(iPO_cop_control_model_exp)
confint(iPO_cop_control_model_exp)

# iPO- step time 1
# control
iPO_time1_control_model_exp = lmer(step_time_ONE_control ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)

summary(iPO_time1_control_model_exp)
confint(iPO_time1_control_model_exp)


# iPO - step time 2
# control
iPO_time2_control_model_exp = lmer(step_time_TWO_control  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)

summary(iPO_time2_control_model_exp)
confint(iPO_time2_control_model_exp)


# iPO - step length 1
# control
iPO_length1_control_model_exp = lmer(step_length_ONE_control  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)

summary(iPO_length1_control_model_exp)
confint(iPO_length1_control_model_exp)


# iPO - step length 2
# control
iPO_length2_control_model_exp = lmer( step_length_TWO_control  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)

summary(iPO_length2_control_model_exp)
confint(iPO_length2_control_model_exp)

# iPO - fy
# control
iPO_fy_control_model_exp = lmer(fy_control  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)

summary(iPO_fy_control_model_exp)
confint(iPO_fy_control_model_exp)


# Inidivudal Interdependence Plots
# control
xyplot(vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_control] ~ vision$step_placement_x_inverted_ONE[vision_indices_control] | vision$subject[vision_indices_control], 
       main="lateral ankle vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. CoP-CoM (m s)")


xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_control] ~ vision$step_placement_x_inverted_ONE[vision_indices_control] | vision$subject[vision_indices_control],
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_control] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_control] | vision$subject[vision_indices_control],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_control] ~ vision$step_time_response_TWO[vision_indices_control],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")


# # # # Test slopes are not different between conditions # # # #

cop_step_towards_model_exp = lmer(step_placement_x_inverted_mm_towards ~ cop_from_com_x_integrated_inverted_mm_towards + (1|subject_towards), REML=FALSE)
cop_step_away_model_exp = lmer(step_placement_x_inverted_mm_away ~ cop_from_com_x_integrated_inverted_mm_away + (1|subject_away), REML=FALSE)
cop_step_control_model_exp = lmer(step_placement_x_inverted_mm_control ~ cop_from_com_x_integrated_inverted_mm_control + (1|subject_control), REML=FALSE)

anova(cop_step_away_model_exp, cop_step_towards_model_exp)


iPO_step_towards_model_exp = lmer(step_placement_x_inverted_mm_towards ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)
iPO_step_away_model_exp = lmer(step_placement_x_inverted_mm_away ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)
iPO_step_control_model_exp = lmer(step_placement_x_inverted_mm_control ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)


iPO_cop_towards_model_exp = lmer(cop_from_com_x_integrated_inverted_mm_towards  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1|subject_towards), REML=FALSE)
iPO_cop_control_model_exp = lmer(cop_from_com_x_integrated_inverted_mm_control  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1|subject_control), REML=FALSE)
iPO_cop_away_model_exp = lmer(cop_from_com_x_integrated_inverted_mm_away  ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1|subject_away), REML=FALSE)

