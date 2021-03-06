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
library(car)

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
library(lme4)
library(lmerTest)
library(emmeans)
require(lattice)
emm_options(pbkrtest.limit = 10000)
number_of_simulations = 100;



# Comprehensive LMER (includes all directions)
# CoP-Step
cop_step_model <- lmer(step_placement_x_inverted_mm_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all  +  (1 + cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_step_model <- anova(cop_step_model)
confint_cop_step_model <- confint(cop_step_model)


cop_model = lmer(cop_from_com_x_integrated_inverted_mm_all ~ direction_all  +  (1 |subject_all), REML=TRUE)
confint_cop_model = confint(cop_model)

# cop_model_towards = lmer(cop_from_com_x_integrated_inverted_mm_towards  ~  (1 |subject_towards), REML=TRUE)
# confint_cop_model_towards = confint(cop_model_towards)
# cop_model_away = lmer(cop_from_com_x_integrated_inverted_mm_away ~  (1 |subject_away), REML=TRUE)
# confint_cop_model_away = confint(cop_model_away)
# cop_model_control = lmer(cop_from_com_x_integrated_inverted_mm_control ~  (1 |subject_control), REML=TRUE)
# confint_cop_model_control = confint(cop_model_control)

# step_model_towards = lmer(step_placement_x_inverted_mm_towards  ~  (1 |subject_towards), REML=TRUE)
# confint_step_model_towards = confint(step_model_towards)
# step_model_away = lmer(step_placement_x_inverted_mm_away ~  (1 |subject_away), REML=TRUE)
# confint_step_model_away = confint(step_model_away)
# step_model_control = lmer(step_placement_x_inverted_mm_control ~  (1 |subject_control), REML=TRUE)
# confint_step_model_control = confint(step_model_control)



step_model = lmer(step_placement_x_inverted_mm_all ~ direction_all  +  (1 |subject_all), REML=TRUE)
confint_step_model = confint(step_model)



dataEllipse(cop_from_com_x_integrated_inverted_mm_towards, step_placement_x_inverted_mm_towards, levels=c(0.95))

#confint_cop_step_model <- confint(cop_step_model)

# COP-Push
cop_push_model <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_all ~  cop_from_com_x_integrated_inverted_mm_all*direction_all  + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_push_model <- anova(cop_push_model)
#confint_cop_push_model <- confint(cop_push_model)

# Step-Push
step_push_model <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_all ~ step_placement_x_inverted_mm_all*direction_all  + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_push_model <- anova(step_push_model)
#confint_step_push_model <- confint(step_push_model)


# iPO - step length 1
push_length1_model <- lmer(step_length_ONE_all ~ trigger_leg_ankle_dorsiflexion_integrated_deg_all*direction_all + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_all|subject_all), REML=TRUE)
anova_push_length1_model <- anova(push_length1_model)
#confint_push_length1_model <- confint(push_length1_model)


# iPO - step length 2

push_length2_model <- lmer(step_length_TWO_all ~ trigger_leg_ankle_dorsiflexion_integrated_deg_all*direction_all + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_all|subject_all), REML=TRUE)
anova_push_length2_model <- anova(push_length2_model)
#confint_push_length2_model <- confint(push_length2_model)


# iPO- step time 1

push_time1_model <- lmer(step_time_ONE_all ~ trigger_leg_ankle_dorsiflexion_integrated_deg_all*direction_all + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_all|subject_all), REML=TRUE)
anova_push_time1_model <- anova(push_time1_model)
#confint_push_time1_model <- confint(push_time1_model)


# iPO - step time 2

push_time2_model <- lmer(step_time_TWO_all ~ trigger_leg_ankle_dorsiflexion_integrated_deg_all*direction_all + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_all|subject_all), REML=TRUE)
anova_push_time2_model <- anova(push_time2_model)
confint_push_time2_model <- confint(push_time2_model)

# iPO - fy

push_fy_model <- lmer(fy_all ~ trigger_leg_ankle_dorsiflexion_integrated_deg_all*direction_all + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_all|subject_all), REML=TRUE)
anova_push_fy_model <- anova(push_fy_model)
confint_push_fy_model <- confint(push_fy_model)


# # # Ankle Gait parameters # # #

# cop - step length 1
cop_length1_model <- lmer(step_length_ONE_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_length1_model <- anova(cop_length1_model)
#confint_push_length1_model <- confint(push_length1_model)


# cop - step length 2

cop_length2_model <- lmer(step_length_TWO_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_length2_model <- anova(cop_length2_model)
#confint_push_length2_model <- confint(push_length2_model)


# cop - step time 1

cop_time1_model <- lmer(step_time_ONE_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_time1_model <- anova(cop_time1_model)
#confint_push_time1_model <- confint(push_time1_model)


# cop - step time 2

cop_time2_model <- lmer(step_time_TWO_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_time2_model <- anova(cop_time2_model)
#confint_cop_time2_model <- confint(cop_time2_model)

# cop - fy

cop_fy_model <- lmer(fy_all ~ cop_from_com_x_integrated_inverted_mm_all*direction_all + (1+cop_from_com_x_integrated_inverted_mm_all|subject_all), REML=TRUE)
anova_cop_fy_model <- anova(cop_fy_model)
#confint_cop_fy_model <- confint(cop_fy_model)

# # # Step Gait parameters # # #

# step - step length 1
step_length1_model <- lmer(step_length_ONE_all ~ step_placement_x_inverted_mm_all*direction_all + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_length1_model <- anova(step_length1_model)
#confint_push_length1_model <- confint(push_length1_model)


# step - step length 2

step_length2_model <- lmer(step_length_TWO_all ~ step_placement_x_inverted_mm_all*direction_all + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_length2_model <- anova(step_length2_model)
#confint_push_length2_model <- confint(push_length2_model)


# step - step time 1

step_time1_model <- lmer(step_time_ONE_all ~ step_placement_x_inverted_mm_all*direction_all + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_time1_model <- anova(step_time1_model)
#confint_push_time1_model <- confint(push_time1_model)


# step - step time 2

step_time2_model <- lmer(step_time_TWO_all ~ step_placement_x_inverted_mm_all*direction_all + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_time2_model <- anova(step_time2_model)
#confint_step_time2_model <- confint(step_time2_model)

# Step - fy

step_fy_model <- lmer(fy_all ~ step_placement_x_inverted_mm_all*direction_all + (1+step_placement_x_inverted_mm_all|subject_all), REML=TRUE)
anova_step_fy_model <- anova(step_fy_model)
#confint_step_fy_model <- confint(step_fy_model)


# vvv LMER By Direction vvv
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




# # # # # # COM Analysis # # # # # 
# all mechs | com
# towards

com_null = lmer(vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] ~ (1|vision$subject[vision_indices_towards]), REML = FALSE)

com_1mech_towards = lmer(vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] + (1 + vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards]|vision$subject[vision_indices_towards]), REML = FALSE)

com_2mech_towards = lmer(vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] +  vision$step_placement_x_inverted_ONE[vision_indices_towards] + (1 + vision$step_placement_x_inverted_ONE[vision_indices_towards] + vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards]|vision$subject[vision_indices_towards]), REML = FALSE)

com_3mech_towards = lmer(vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] + vision$step_placement_x_inverted_ONE[vision_indices_towards] + vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards]
      + (1+vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards]+ vision$step_placement_x_inverted_ONE[vision_indices_towards] + vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards]|vision$subject[vision_indices_towards]), REML = FALSE)

com_3mech_towards = lmer(vision$com_x_inverted_pushoff_end_TWO[vision_indices_towards] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards] + vision$step_placement_x_inverted_ONE[vision_indices_towards] + vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_towards]
              + (1|vision$subject[vision_indices_towards]), REML = FALSE)

# towards

com_vel_cop_towards_null = lmer(com_x_vel_inverted_pushoff_end_mm_towards ~ (1+cop_from_com_x_integrated_inverted_mm_towards|subject_towards), REML=FALSE)
com_vel_cop_towards_exp = lmer(com_x_vel_inverted_pushoff_end_mm_towards ~ cop_from_com_x_integrated_inverted_mm_towards + (1+cop_from_com_x_integrated_inverted_mm_towards|subject_towards), REML=FALSE)
[summary(com_vel_cop_towards_exp)
  anova(com_vel_cop_towards_exp)
  confint(com_vel_cop_towards_exp)
  
  com_vel_step_towards_null = lmer(com_x_vel_inverted_pushoff_end_mm_towards ~ (1+step_placement_x_inverted_mm_towards|subject_towards), REML=FALSE)
  com_vel_step_towards_exp = lmer(com_x_vel_inverted_pushoff_end_mm_towards ~ step_placement_x_inverted_mm_towards + (1+step_placement_x_inverted_mm_towards|subject_towards), REML=FALSE)
  summary(com_vel_step_towards_exp)
  anova(com_vel_step_towards_exp)
  confint(com_vel_step_towards_exp)
  
  com_vel_ipo_towards_exp = lmer(com_x_vel_inverted_pushoff_end_mm_towards ~ trigger_leg_ankle_dorsiflexion_integrated_deg_towards + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_towards|subject_towards), REML=FALSE)
  summary(com_vel_ipo_towards_exp)
  anova(com_vel_ipo_towards_exp)
  confint(com_vel_ipo_towards_exp)
  
  # away
  
  com_vel_cop_away_null = lmer(com_x_vel_inverted_pushoff_end_mm_away ~ (1+cop_from_com_x_integrated_inverted_mm_away|subject_away), REML=FALSE)
  com_vel_cop_away_exp = lmer(com_x_vel_inverted_pushoff_end_mm_away ~ cop_from_com_x_integrated_inverted_mm_away + (1+cop_from_com_x_integrated_inverted_mm_away|subject_away), REML=FALSE)
  summary(com_vel_cop_away_exp)
  anova(com_vel_cop_away_exp)
  confint(com_vel_cop_away_exp)
  
  com_vel_step_away_null = lmer(com_x_vel_inverted_pushoff_end_mm_away ~ (1+step_placement_x_inverted_mm_away|subject_away), REML=FALSE)
  com_vel_step_away_exp = lmer(com_x_vel_inverted_pushoff_end_mm_away ~ step_placement_x_inverted_mm_away + (1+step_placement_x_inverted_mm_away|subject_away), REML=FALSE)
  summary(com_vel_step_away_exp)
  anova(com_vel_step_away_exp)
  confint(com_vel_step_away_exp)
  
  com_vel_ipo_away_exp = lmer(com_x_vel_inverted_pushoff_end_mm_away ~ trigger_leg_ankle_dorsiflexion_integrated_deg_away + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_away|subject_away), REML=FALSE)
  summary(com_vel_ipo_away_exp)
  anova(com_vel_ipo_away_exp)
  confint(com_vel_ipo_away_exp)
  
  # control
  com_vel_cop_control_null = lmer(com_x_vel_inverted_pushoff_end_mm_control ~ (1+cop_from_com_x_integrated_inverted_mm_control|subject_control), REML=FALSE)
  com_vel_cop_control_exp = lmer(com_x_vel_inverted_pushoff_end_mm_control ~ cop_from_com_x_integrated_inverted_mm_control + (1+cop_from_com_x_integrated_inverted_mm_control|subject_control), REML=FALSE)
  summary(com_vel_cop_control_exp)
  anova(com_vel_cop_control_exp)
  confint(com_vel_cop_control_exp)
  
  com_vel_step_control_null = lmer(com_x_vel_inverted_pushoff_end_mm_control ~ (1+step_placement_x_inverted_mm_control|subject_control), REML=FALSE)
  com_vel_step_control_exp = lmer(com_x_vel_inverted_pushoff_end_mm_control ~ step_placement_x_inverted_mm_control + (1+step_placement_x_inverted_mm_control|subject_control), REML=FALSE)
  summary(com_vel_step_control_exp)
  anova(com_vel_step_control_exp)
  confint(com_vel_step_control_exp)
  
  com_vel_ipo_control_exp = lmer(com_x_vel_inverted_pushoff_end_mm_control ~ trigger_leg_ankle_dorsiflexion_integrated_deg_control + (1+trigger_leg_ankle_dorsiflexion_integrated_deg_control|subject_control), REML=FALSE)
  summary(com_vel_ipo_control_exp)
  anova(com_vel_ipo_control_exp)
  confint(com_vel_ipo_control_exp)
  
  

#anova(com_null, com_1mech_towards, com_2mech_towards, com_3mech_towards)

com_mech_towards = lmer(com_x_inverted_pushoff_end_mm_towards ~ cop_from_com_x_integrated_inverted_mm_towards + step_placement_x_inverted_mm_towards + trigger_leg_ankle_dorsiflexion_integrated_deg_towards 
                        + (1|subject_towards), REML = FALSE)

summary(com_mech_towards)
anova(com_mech_towards)
confint(com_mech_towards)



