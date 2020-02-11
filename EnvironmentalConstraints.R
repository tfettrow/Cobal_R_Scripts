library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
number_of_simulations = 100;


# lateral ankle
cop_from_com_x_model <- lmer(cop_from_com_x_integrated_sym_ONE ~ trigger_foot + zone_direction +  (1|subject), data=results, REML = FALSE)
anova_cop_from_com_x_model <- anova(cop_from_com_x_model)
rg_cop_from_com_x_model <- ref_grid(cop_from_com_x_model)
confint_cop_from_com_x_model <- emmeans(rg_cop_from_com_x_model, "zone_direction")


# step placement
step_placement_model <- lmer(step_placement_x_sym_ONE ~ trigger_foot + zone_direction +  (1|subject), data=results, REML = FALSE)
anova_step_placement_model <- anova(step_placement_model)
rg_step_placement_model <- ref_grid(step_placement_model)
confint_step_placement_model <- emmeans(rg_step_placement_model, "zone_direction")

# stimulus response
stimulus_response_model <- lmer(stimulus_response_x_sym_ONE ~ trigger_foot + zone_direction +  (1|subject), data=results, REML = FALSE)
anova_stimulus_response_model <- anova(stimulus_response_model)
rg_stimulus_response_model <- ref_grid(stimulus_response_model)
confint_stimulus_response_model <- emmeans(rg_stimulus_response_model, "zone_direction")

# com max
com_x_inverted_model <- lmer(com_x_inverted_max ~ trigger_foot + zone_direction + (1|subject), data=results_long, REML = FALSE)
anova_com_x_inverted_model <- anova(com_x_inverted_model)
rg_com_x_inverted_model <- ref_grid(com_x_inverted_model)
confint_com_x_inverted_model <- emmeans(rg_com_x_inverted_model, "zone_direction")

# trigger leg ankle eversion
trigger_leg_ankle_eversion_model <- lmer(trigger_leg_ankle_eversion_integrated_singlestance_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_ankle_eversion_model <- anova(trigger_leg_ankle_eversion_model)
rg_trigger_leg_ankle_eversion_model <- ref_grid(trigger_leg_ankle_eversion_model)
confint_trigger_leg_ankle_eversion_model <- emmeans(rg_trigger_leg_ankle_eversion_model, "zone_direction")

# trigger leg peroneous longus emg
trigger_leg_peroneous_emg_model <- lmer(trigger_leg_peroneous_integrated_singlestance_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_peroneous_emg_model <- anova(trigger_leg_peroneous_emg_model)
rg_trigger_leg_peroneous_emg_model <- ref_grid(trigger_leg_peroneous_emg_model)
confint_trigger_leg_peroneous_emg_model <- emmeans(rg_trigger_leg_peroneous_emg_model, "zone_direction")

# trigger leg tibialis anterior emg
trigger_leg_tibialis_anterior_emg_model <- lmer(trigger_leg_tibiant_integrated_singlestance_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_tibialis_anterior_emg_model <- anova(trigger_leg_tibialis_anterior_emg_model)
rg_trigger_leg_tibialis_anterior_emg_model <- ref_grid(trigger_leg_tibialis_anterior_emg_model)
confint_trigger_leg_tibialis_anterior_emg_model <- emmeans(rg_trigger_leg_tibialis_anterior_emg_model, "zone_direction")

# trigger leg knee rotation  
trigger_leg_knee_rotation_model <- lmer(trigger_leg_knee_rotation_step_end_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_knee_rotation_model <- anova(trigger_leg_knee_rotation_model)
rg_trigger_leg_knee_rotation_model <- ref_grid(trigger_leg_knee_rotation_model)
confint_trigger_leg_knee_rotation_model <- emmeans(rg_trigger_leg_knee_rotation_model, "zone_direction")

# contra leg hip rotation  (**need to confirm we are looking at contra leg**)
contra_leg_hip_rotation_model <- lmer(contra_leg_hip_rotation_step_end_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_contra_leg_hip_rotation_model <- anova(contra_leg_hip_rotation_model)
rg_contra_leg_hip_rotation_model <- ref_grid(contra_leg_hip_rotation_model)
confint_contra_leg_hip_rotation_model <- emmeans(rg_contra_leg_hip_rotation_model, "zone_direction")


# contra leg hip adduction (**need to confirm we are looking at contra leg**)
contra_leg_hip_adduction_model <- lmer(contra_leg_hip_adduction_step_end_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_contra_leg_hip_adduction_model <- anova(contra_leg_hip_adduction_model)
rg_contra_leg_hip_adduction_model <- ref_grid(contra_leg_hip_adduction_model)
confint_contra_leg_hip_adduction_model <- emmeans(rg_contra_leg_hip_adduction_model, "zone_direction")


# trigger leg ankle dorsi flexion  (negative values of plantar flexion) 
trigger_leg_ankle_dorsiflexion_model <- lmer(trigger_leg_ankle_dorsiflexion_integrated_doublestance_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_ankle_dorsiflexion_model <- anova(trigger_leg_ankle_dorsiflexion_model)
rg_trigger_leg_ankle_dorsiflexion_model <- ref_grid(trigger_leg_ankle_dorsiflexion_model)
confint_trigger_leg_ankle_dorsiflexion_model <- emmeans(rg_trigger_leg_ankle_dorsiflexion_model, "zone_direction")

# trigger leg gastroc med emg
trigger_leg_gastroc_med_emg_model <- lmer(trigger_leg_gastroc_med_integrated_singlestance_ONE ~ trigger_foot + zone_direction + (1|subject), data=results, REML = FALSE)
anova_trigger_leg_gastroc_med_emg_model <- anova(trigger_leg_gastroc_med_emg_model)
rg_trigger_leg_gastroc_med_emg_model <- ref_grid(trigger_leg_gastroc_med_emg_model)
confint_trigger_leg_gastroc_med_emg_model <- emmeans(rg_trigger_leg_gastroc_med_emg_model, "zone_direction")









# # # Power Analysis # # # # 

# COM

# Step Placement

fm_step_placement_x_inverted_mm <- lmer(step_placement_x_directionSym_ONE ~ zone_direction_bin + direction_bin + zone_direction_bin*direction_bin + (1|subject), data=dat, REML = FALSE)

fixef(fm_step_placement_x_inverted_mm)["zone_direction_bin"] <- 0.005

print(powerSimResults_step <- powerSim(fm_step_placement_x_inverted_mm, nsim=number_of_simulations))

fm_step_extended <- extend(fm_step_placement_x_inverted_mm, along="subject", n=20)

powerSim(fm_step_extended)

power_curve_step <- powerCurve(fm_step_extended, along="subject")


# LAT ANKLE
fm_cop_from_com_x_integrated_inverted_mm <- lmer(cop_from_com_x_integrated_directionSym_ONE  ~ zone_direction_bin + direction_bin + zone_direction_bin*direction_bin  + (1|subject), data=dat, REML = FALSE)

fixef(fm_cop_from_com_x_integrated_inverted_mm)["zone_direction_bin"] <- 0.001

print(powerSimResults_step <- powerSim(fm_cop_from_com_x_integrated_inverted_mm, nsim=number_of_simulations))

fm_cop_from_com_x_extended <- extend(fm_cop_from_com_x_integrated_inverted_mm, along="subject", n=20)

powerSim(fm_cop_from_com_x_extended)

power_curve_cop <- powerCurve(fm_cop_from_com_x_extended, along="subject")

