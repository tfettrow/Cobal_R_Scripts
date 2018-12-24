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
dat = read.csv("G:\Team Drives\CoBaL\Cadence\results_pilot.csv", header = TRUE)

step_placement_x_inverted_mm_fast <- dat$step_placement_x_sym_ONE[indices_fast] * 1000;
stimulus_response_x_inverted_mm_fast <- dat$stimulus_response_x_sym_ONE[indices_fast] * 1000;
cop_from_com_x_integrated_inverted_mm_fast <- dat$cop_from_com_x_integrated_sym_ONE[indices_fast] * 1000;
trigger_leg_ankle_dorsiflexion_integrated_fast <- dat$trigger_leg_ankle_dorsiflexion_integrated_ONE[indices_fast] * -57.29;

step_placement_x_inverted_mm_slow <- dat$step_placement_x_sym_ONE[indices_slow] * 1000;
stimulus_response_x_inverted_mm_slow <- dat$stimulus_response_x_sym_ONE[indices_slow] * 1000;
cop_from_com_x_integrated_inverted_mm_slow <- dat$cop_from_com_x_integrated_sym_ONE[indices_slow] * 1000;
trigger_leg_ankle_dorsiflexion_integrated_slow <- dat$trigger_leg_ankle_dorsiflexion_integrated_ONE[indices_slow] * -57.29;


step_time <- dat$step_time_ONE
step_length <- dat$step_length_ONE
velocity <- dat$velocity_ONE
step_placement_x_inverted_mm <- dat$step_placement_x_sym_ONE
stimulus_response_x_inverted_mm <- dat$stimulus_response_x_sym_ONE 
cop_from_com_x_integrated_inverted_mm <- dat$cop_from_com_x_integrated_sym_ONE
trigger_leg_ankle_dorsiflexion_integrated_deg <- dat$trigger_leg_ankle_dorsiflexion_integrated_TWO * -57.29;
cadence <- dat$cadence;
trigger_foot <- dat$trigger_foot;
subject <- dat$subject;

com_x_bandend <- dat$com_x_sym_band_end_TWO
com_x_vel_bandend <- dat$com_x_vel_sym_band_end_TWO
com_x_atPO <- dat$com_x_sym_pushoff_end_TWO
com_x_vel_atPO <- dat$com_x_vel_sym_pushoff_end_TWO
  

# # # Test Trigger Foot Significance # # # 

# STEP PLACEMENT
fm_step_placement_x_inverted_mm <- lmer(step_placement_x_inverted_mm ~ trigger_foot*cadence + (1|subject))
anova_step_placement_x_inverted_mm <- anova(fm_step_placement_x_inverted_mm)
rg_step_placement_x_inverted_mm <- ref_grid(fm_step_placement_x_inverted_mm)
confint_step_placement_x_inverted_mm <- emmeans(rg_step_placement_x_inverted_mm, "cadence")

# STIM RESPONSE
fm_stimulus_response_x_inverted_mm <- lmer(stimulus_response_x_inverted_mm ~ trigger_foot*cadence + (1|subject))
anova_stimulus_response_x_inverted_mm <- anova(fm_stimulus_response_x_inverted_mm)
rg_stimulus_response_x_inverted_mm <- ref_grid(fm_stimulus_response_x_inverted_mm)
confint_stimulus_response_x_inverted_mm <- emmeans(rg_stimulus_response_x_inverted_mm, "cadence")

# LAT ANKLE
fm_cop_from_com_x_integrated_inverted_mm <- lmer(cop_from_com_x_integrated_inverted_mm  ~ trigger_foot*cadence + (1|subject))
anova_cop_from_com_x_integrated_inverted_mm <- anova(fm_cop_from_com_x_integrated_inverted_mm)
rg_cop_from_com_x_integrated_inverted_mm <- ref_grid(fm_cop_from_com_x_integrated_inverted_mm)
confint_cop_from_com_x_integrated_inverted_mm <- emmeans(rg_cop_from_com_x_integrated_inverted_mm, "cadence")

# PUSH OFF
fm_trigger_leg_ankle_dorsiflexion_integrated_deg <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg  ~ trigger_foot*cadence + (1|subject))
anova_trigger_leg_ankle_dorsiflexion_integrated_deg <- anova(fm_trigger_leg_ankle_dorsiflexion_integrated_deg)
rg_trigger_leg_ankle_dorsiflexion_integrated_deg <- ref_grid(fm_trigger_leg_ankle_dorsiflexion_integrated_deg)
confint_trigger_leg_ankle_dorsiflexion_integrated_deg <- emmeans(rg_trigger_leg_ankle_dorsiflexion_integrated_deg, "cadence")

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