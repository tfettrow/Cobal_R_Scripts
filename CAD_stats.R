library(lme4)
library(lmerTest)
library(emmeans)
require(lattice) 
emm_options(pbkrtest.limit = 5000)
number_of_simulations = 100;

indices_slow = which(dat$cadence == '80BPM' & dat$direction != 'STIM_NONE')
indices_fast = which(dat$cadence == '110BPM'  & dat$direction != 'STIM_NONE')
indices_control = which(dat$direction == 'STIM_NONE')

# load data
dat = read.csv("G:\Team Drives\CoBaL\Cadence\results.csv", header = TRUE)

step_placement_x_inverted_mm_fast <- dat$step_placement_x_sym_ONE[indices_fast] * 1000;
stimulus_response_x_inverted_mm_fast <- dat$stimulus_response_x_sym_ONE[indices_fast] * 1000;
cop_from_com_x_integrated_inverted_mm_fast <- dat$cop_from_com_x_integrated_sym_ONE[indices_fast] * 1000;
trigger_leg_ankle_dorsiflexion_integrated_fast <- dat$trigger_leg_ankle_dorsiflexion_integrated_ONE[indices_fast] * -1;

step_placement_x_inverted_mm_slow <- dat$step_placement_x_sym_ONE[indices_slow] * 1000;
stimulus_response_x_inverted_mm_slow <- dat$stimulus_response_x_sym_ONE[indices_slow] * 1000;
cop_from_com_x_integrated_inverted_mm_slow <- dat$cop_from_com_x_integrated_sym_ONE[indices_slow] * 1000;
trigger_leg_ankle_dorsiflexion_integrated_slow <- dat$trigger_leg_ankle_dorsiflexion_integrated_ONE[indices_slow] * -1;

step_time <- dat$step_time_ONE
step_length <- dat$step_length_ONE
velocity <- dat$velocity_ONE

step_placement_x_inverted_mm <- dat$step_placement_x_sym_ONE * 1000;
stimulus_response_x_inverted_mm <- dat$stimulus_response_x_sym_ONE * 1000;
swing_leg_heel_x_mm <- dat$swing_leg_heel_x_ONE * 1000;

trigger_leg_knee_rotation_step_end_deg <- dat$trigger_leg_knee_rotation_step_end_ONE;
swing_leg_hip_adduction_step_end_deg <- dat$swing_leg_hip_adduction_step_end_ONE;
swing_leg_hip_rotation_step_end_deg <- dat$swing_leg_hip_rotation_step_end_ONE;

cop_from_com_x_integrated_singlestance_inverted_mm <- dat$cop_from_com_x_integrated_singlestance_sym_ONE * 1000;
trigger_leg_ankle_eversion_integrated_singlestance_deg <- dat$trigger_leg_ankle_eversion_integrated_singlestance_ONE;
trigger_leg_peroneus_integrated_singlestance_perc <- dat$trigger_leg_peroneus_integrated_singlestance_ONE;
trigger_leg_tibiant_integrated_singlestance_perc <- dat$trigger_leg_tibiant_integrated_singlestance_ONE;

cop_from_com_x_integrated_inverted_mm <- dat$cop_from_com_x_integrated_sym_ONE * 1000;
trigger_leg_ankle_eversion_integrated_deg <- dat$trigger_leg_ankle_eversion_integrated_ONE;
trigger_leg_peroneus_integrated_perc <- dat$trigger_leg_peroneus_integrated_ONE;
trigger_leg_tibiant_integrated_perc <- dat$trigger_leg_tibiant_integrated_ONE;


trigger_leg_ankle_dorsiflexion_integrated_deg <- dat$trigger_leg_ankle_dorsiflexion_integrated_doublestance_TWO * -1;
trigger_leg_gastroc_integrated_singlestance_perc <- dat$trigger_leg_gastroc_integrated_singlestance_ONE;

cadence <- dat$cadence;
trigger_foot <- dat$trigger_foot;
subject <- dat$subject;

com_x_long_max <- dat$com_x_max_long * 1000;

com_x_bandend <- dat$com_x_sym_band_end_TWO
com_x_vel_bandend <- dat$com_x_vel_sym_band_end_TWO
com_x_atPO <- dat$com_x_sym_pushoff_end_TWO
com_x_vel_atPO <- dat$com_x_vel_sym_pushoff_end_TWO
  

# # # Test Trigger Foot Significance # # # 

con <- file("cadence.log")
sink(con, append=TRUE)

# CoM x long Max
fm_com_x_long_max_inverted_mm <- lmer(com_x_long_max ~ trigger_foot+cadence + (1|subject))
anova_com_x_long_max_inverted_mm <- anova(fm_com_x_long_max_inverted_mm)
rg_com_x_long_max_inverted_mm <- ref_grid(fm_com_x_long_max_inverted_mm)
confint_com_x_long_max_inverted_mm <- emmeans(rg_com_x_long_max_inverted_mm, "cadence")

# STEP PLACEMENT
fm_step_placement_x_inverted_mm <- lmer(step_placement_x_inverted_mm ~ trigger_foot+cadence + (1|subject))
anova_step_placement_x_inverted_mm <- anova(fm_step_placement_x_inverted_mm)
rg_step_placement_x_inverted_mm <- ref_grid(fm_step_placement_x_inverted_mm)
confint_step_placement_x_inverted_mm <- emmeans(rg_step_placement_x_inverted_mm, "cadence")

fm_stimulus_response_x_inverted_mm <- lmer(stimulus_response_x_inverted_mm ~ trigger_foot+cadence + (1|subject))
anova_stimulus_response_x_inverted_mm <- anova(fm_stimulus_response_x_inverted_mm)
rg_stimulus_response_x_inverted_mm <- ref_grid(fm_stimulus_response_x_inverted_mm)
confint_stimulus_response_x_inverted_mm <- emmeans(rg_stimulus_response_x_inverted_mm, "cadence")

fm_swing_leg_heel_x_mm <- lmer(stimulus_response_x_inverted_mm ~ trigger_foot+cadence + (1|subject))
anova_swing_leg_heel_x_mm <- anova(fm_swing_leg_heel_x_mm)
rg_swing_leg_heel_x_mm <- ref_grid(fm_swing_leg_heel_x_mm)
confint_swing_leg_heel_x_mm <- emmeans(rg_swing_leg_heel_x_mm, "cadence")

fm_trigger_leg_knee_rotation_step_end_deg <- lmer(trigger_leg_knee_rotation_step_end_deg ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_knee_rotation_step_end_deg <- anova(fm_trigger_leg_knee_rotation_step_end_deg)
rg_trigger_leg_knee_rotation_step_end_deg <- ref_grid(fm_trigger_leg_knee_rotation_step_end_deg)
confint_trigger_leg_knee_rotation_step_end_deg <- emmeans(rg_trigger_leg_knee_rotation_step_end_deg, "cadence")

fm_swing_leg_hip_rotation_step_end_deg <- lmer(swing_leg_hip_rotation_step_end_deg ~ trigger_foot+cadence + (1|subject))
anova_swing_leg_hip_rotation_step_end_deg <- anova(fm_swing_leg_hip_rotation_step_end_deg)
rg_swing_leg_hip_rotation_step_end_deg <- ref_grid(fm_swing_leg_hip_rotation_step_end_deg)
confint_swing_leg_hip_rotation_step_end_deg <- emmeans(rg_swing_leg_hip_rotation_step_end_deg, "cadence")

fm_swing_leg_hip_adduction_step_end_deg <- lmer(swing_leg_hip_adduction_step_end_deg ~ trigger_foot+cadence + (1|subject))
anova_swing_leg_hip_adduction_step_end_deg <- anova(fm_swing_leg_hip_adduction_step_end_deg)
rg_swing_leg_hip_adduction_step_end_deg <- ref_grid(fm_swing_leg_hip_adduction_step_end_deg)
confint_swing_leg_hip_adduction_step_end_deg <- emmeans(rg_swing_leg_hip_adduction_step_end_deg, "cadence")

# LAT ANKLE
fm_cop_from_com_x_integrated_singlestance_inverted_mm <- lmer(cop_from_com_x_integrated_singlestance_inverted_mm  ~ trigger_foot*cadence + (1|subject))
anova_cop_from_com_x_integrated_singlestance_inverted_mm <- anova(fm_cop_from_com_x_integrated_singlestance_inverted_mm)
rg_cop_from_com_x_integrated_singlestance_inverted_mm <- ref_grid(fm_cop_from_com_x_integrated_singlestance_inverted_mm)
confint_cop_from_com_x_integrated_singlestance_inverted_mm <- emmeans(rg_cop_from_com_x_integrated_singlestance_inverted_mm, "cadence")

fm_trigger_leg_ankle_eversion_integrated_singlestance_deg <- lmer(trigger_leg_ankle_eversion_integrated_singlestance_deg ~ trigger_foot*cadence + (1|subject))
anova_trigger_leg_ankle_eversion_integrated_singlestance_deg <- anova(fm_trigger_leg_ankle_eversion_integrated_singlestance_deg)
rg_trigger_leg_ankle_eversion_integrated_singlestance_deg <- ref_grid(fm_trigger_leg_ankle_eversion_integrated_singlestance_deg)
confint_trigger_leg_ankle_eversion_integrated_singlestance_deg <- emmeans(rg_trigger_leg_ankle_eversion_integrated_singlestance_deg, "cadence")

fm_trigger_leg_peroneus_integrated_singlestance_perc <- lmer(trigger_leg_peroneus_integrated_singlestance_perc ~ trigger_foot*cadence + (1|subject))
anova_trigger_leg_peroneus_integrated_singlestance_perc <- anova(fm_trigger_leg_peroneus_integrated_singlestance_perc)
rg_trigger_leg_peroneus_integrated_singlestance_perc <- ref_grid(fm_trigger_leg_peroneus_integrated_singlestance_perc)
confint_trigger_leg_peroneus_integrated_singlestance_perc <- emmeans(rg_trigger_leg_peroneus_integrated_singlestance_perc, "cadence")

fm_trigger_leg_tibiant_integrated_singlestance_perc <- lmer(trigger_leg_tibiant_integrated_singlestance_perc ~ trigger_foot*cadence + (1|subject))
anova_trigger_leg_tibiant_integrated_singlestance_perc <- anova(fm_trigger_leg_tibiant_integrated_singlestance_perc)
rg_trigger_leg_tibiant_integrated_singlestance_perc <- ref_grid(fm_trigger_leg_tibiant_integrated_singlestance_perc)
confint_trigger_leg_tibiant_integrated_singlestance_perc <- emmeans(rg_trigger_leg_tibiant_integrated_singlestance_perc, "cadence")


fm_cop_from_com_x_integrated_inverted_mm <- lmer(cop_from_com_x_integrated_inverted_mm  ~ trigger_foot+cadence + (1|subject))
anova_cop_from_com_x_integrated_inverted_mm <- anova(fm_cop_from_com_x_integrated_inverted_mm)
rg_cop_from_com_x_integrated_inverted_mm <- ref_grid(fm_cop_from_com_x_integrated_inverted_mm)
confint_cop_from_com_x_integrated_inverted_mm <- emmeans(rg_cop_from_com_x_integrated_inverted_mm, "cadence")

fm_trigger_leg_ankle_eversion_integrated_deg <- lmer(trigger_leg_ankle_eversion_integrated_deg ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_ankle_eversion_integrated_deg <- anova(fm_trigger_leg_ankle_eversion_integrated_deg)
rg_trigger_leg_ankle_eversion_integrated_deg <- ref_grid(fm_trigger_leg_ankle_eversion_integrated_deg)
confint_trigger_leg_ankle_eversion_integrated_deg <- emmeans(rg_trigger_leg_ankle_eversion_integrated_deg, "cadence")

fm_trigger_leg_peroneus_integrated_perc <- lmer(trigger_leg_peroneus_integrated_perc ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_peroneus_integrated_perc <- anova(fm_trigger_leg_peroneus_integrated_perc)
rg_trigger_leg_peroneus_integrated_perc <- ref_grid(fm_trigger_leg_peroneus_integrated_perc)
confint_trigger_leg_peroneus_integrated_perc <- emmeans(rg_trigger_leg_peroneus_integrated_perc, "cadence")

fm_trigger_leg_tibiant_integrated_perc <- lmer(trigger_leg_tibiant_integrated_perc ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_tibiant_integrated_perc <- anova(fm_trigger_leg_tibiant_integrated_perc)
rg_trigger_leg_tibiant_integrated_perc <- ref_grid(fm_trigger_leg_tibiant_integrated_perc)
confint_trigger_leg_tibiant_integrated_perc <- emmeans(rg_trigger_leg_tibiant_integrated_perc, "cadence")

# PUSH OFF
fm_trigger_leg_ankle_dorsiflexion_integrated_deg <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_ankle_dorsiflexion_integrated_deg <- anova(fm_trigger_leg_ankle_dorsiflexion_integrated_deg)
rg_trigger_leg_ankle_dorsiflexion_integrated_deg <- ref_grid(fm_trigger_leg_ankle_dorsiflexion_integrated_deg)
confint_trigger_leg_ankle_dorsiflexion_integrated_deg <- emmeans(rg_trigger_leg_ankle_dorsiflexion_integrated_deg, "cadence")

fm_trigger_leg_gastroc_integrated_singlestance_perc <- lmer(trigger_leg_gastroc_integrated_singlestance_perc  ~ trigger_foot+cadence + (1|subject))
anova_trigger_leg_gastroc_integrated_singlestance_perc <- anova(fm_trigger_leg_gastroc_integrated_singlestance_perc)
rg_trigger_leg_gastroc_integrated_singlestance_perc <- ref_grid(fm_trigger_leg_gastroc_integrated_singlestance_perc)
confint_trigger_leg_gastroc_integrated_singlestance_perc <- emmeans(rg_trigger_leg_gastroc_integrated_singlestance_perc, "cadence")

# STEP TIME
fm_step_time <- lmer(step_time  ~ trigger_foot*cadence + (1|subject))
anova_step_time <- anova(fm_step_time)
rg_step_time <- ref_grid(fm_step_time)
confint_step_time <- emmeans(rg_step_time, "cadence")

# STEP LENGTH
fm_step_length <- lmer(step_length  ~ trigger_foot*cadence + (1|subject))
anova_step_length <- anova(fm_step_length)
rg_step_length <- ref_grid(fm_step_length)
confint_step_length <- emmeans(rg_step_length, "cadence")

# STEP LENGTH
fm_velocity <- lmer(velocity  ~ trigger_foot*cadence + (1|subject))
anova_velocity <- anova(fm_velocity)
rg_velocity <- ref_grid(fm_velocity)
confint_velocity <- emmeans(rg_velocity, "cadence")

# COM POS 2nd Step
# STEP LENGTH
fm_com_x_bandend <- lmer(com_x_bandend  ~ trigger_foot*cadence + (1|subject))
anova_com_x_bandend <- anova(fm_com_x_bandend)
rg_com_x_bandend <- ref_grid(fm_com_x_bandend)
confint_com_x_bandend <- emmeans(rg_com_x_bandend, "cadence")

# COM VEL 2nd Step
fm_com_x_vel_bandend <- lmer(com_x_vel_bandend  ~ trigger_foot*cadence + (1|subject))
anova_com_x_vel_bandend <- anova(fm_com_x_vel_bandend)
rg_com_x_vel_bandend <- ref_grid(fm_com_x_vel_bandend)
confint_com_x_vel_bandend <- emmeans(rg_com_x_vel_bandend, "cadence")

# COM POS at PUSHOFF
fm_com_x_atPO <- lmer(com_x_atPO  ~ trigger_foot*cadence + (1|subject))
anova_com_x_atPO <- anova(fm_com_x_atPO)
rg_com_x_atPO <- ref_grid(fm_com_x_atPO)
confint_com_x_atPO <- emmeans(rg_com_x_atPO, "cadence")

# COM VEL at PUSHOFF
fm_com_x_vel_atPO <- lmer(com_x_vel_atPO  ~ trigger_foot*cadence + (1|subject))
anova_com_x_vel_atPO <- anova(fm_com_x_vel_atPO)
rg_com_x_vel_atPO <- ref_grid(fm_com_x_vel_atPO)
confint_com_x_vel_atPO <- emmeans(rg_com_x_vel_atPO, "cadence")



xyplot(dat$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] ~ dat$step_placement_x_inverted_ONE[vision_indices_away] | dat$subject[vision_indices_away], 
       main="lateral ankle vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. CoP-CoM (m s)")

xyplot(dat$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] ~ dat$step_placement_x_inverted_ONE[vision_indices_away] | dat$subject[vision_indices_away],
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(vision$trigger_leg_ankle_dorsiflexion_integrated_TWO[vision_indices_away] ~ vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] | vision$subject[vision_indices_away],
       main="Pushoff vs. lateral ankle",
       xlab="int. CoP-CoM (m s)",
       ylab="int. Ankle Dorsiflexion (deg s)")