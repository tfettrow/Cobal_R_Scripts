library(lme4)
library(lmerTest)
library(emmeans)
emm_options(pbkrtest.limit = 5000)

# load data
data_root = getwd()
dat = read.csv(paste(data_root, .Platform$file.sep, "results.csv", sep=""), header = TRUE)

step_placement_x_inverted_mm <- dat$step_placement_x_inverted * 1000;
stimulus_response_x_inverted_mm <- dat$stimulus_response_x_inverted * 1000;
contra_leg_hip_abduction_step_end_inverted <- dat$contra_leg_hip_abduction_step_end_inverted;
contra_leg_glut_med_integrated_inverted <- dat$contra_leg_glut_med_integrated_inverted;

cop_from_com_x_integrated_inverted_mm <- dat$cop_from_com_x_integrated_inverted * 1000;
trigger_leg_ankle_eversion_step_end_inverted <- dat$trigger_leg_ankle_eversion_step_end_inverted;
trigger_leg_pero_lng_integrated_inverted <- dat$trigger_leg_pero_lng_integrated_inverted;

direction <- dat$direction;
stance_foot <- dat$stance_foot;
subject <- dat$subject;

# fit reduced model, perform anova and calculate confidence intervals for all variables
fm_step_placement_x_inverted_mm <- lmer(step_placement_x_inverted_mm ~ stance_foot*direction + (1|subject))
anova_step_placement_x_inverted_mm <- anova(fm_step_placement_x_inverted_mm)
rg_step_placement_x_inverted_mm <- ref_grid(fm_step_placement_x_inverted_mm)
confint_step_placement_x_inverted_mm <- emmeans(rg_step_placement_x_inverted_mm, "direction")

fm_stimulus_response_x_inverted_mm <- lmer(stimulus_response_x_inverted_mm ~ stance_foot*direction + (1|subject))
anova_stimulus_response_x_inverted_mm <- anova(fm_stimulus_response_x_inverted_mm)
rg_stimulus_response_x_inverted_mm <- ref_grid(fm_stimulus_response_x_inverted_mm)
confint_stimulus_response_x_inverted_mm <- emmeans(rg_stimulus_response_x_inverted_mm, "direction")

fm_contra_leg_hip_abduction_step_end_inverted <- lmer(contra_leg_hip_abduction_step_end_inverted ~ stance_foot*direction + (1|subject))
anova_contra_leg_hip_abduction_step_end_inverted <- anova(fm_contra_leg_hip_abduction_step_end_inverted)
rg_contra_leg_hip_abduction_step_end_inverted <- ref_grid(fm_contra_leg_hip_abduction_step_end_inverted)
confint_contra_leg_hip_abduction_step_end_inverted <- emmeans(rg_contra_leg_hip_abduction_step_end_inverted, "direction")

fm_contra_leg_glut_med_integrated_inverted <- lmer(contra_leg_glut_med_integrated_inverted ~ stance_foot*direction + (1|subject))
anova_contra_leg_glut_med_integrated_inverted <- anova(fm_contra_leg_glut_med_integrated_inverted)
rg_contra_leg_glut_med_integrated_inverted <- ref_grid(fm_contra_leg_glut_med_integrated_inverted)
confint_contra_leg_glut_med_integrated_inverted <- emmeans(rg_contra_leg_glut_med_integrated_inverted, "direction")


fm_cop_from_com_x_integrated_inverted_mm <- lmer(cop_from_com_x_integrated_inverted_mm ~ stance_foot*direction + (1|subject))
anova_cop_from_com_x_integrated_inverted_mm <- anova(fm_cop_from_com_x_integrated_inverted_mm)
rg_cop_from_com_x_integrated_inverted_mm <- ref_grid(fm_cop_from_com_x_integrated_inverted_mm)
confint_cop_from_com_x_integrated_inverted_mm <- emmeans(rg_cop_from_com_x_integrated_inverted_mm, "direction")

fm_trigger_leg_ankle_eversion_step_end_inverted <- lmer(trigger_leg_ankle_eversion_step_end_inverted ~ stance_foot*direction + (1|subject))
anova_trigger_leg_ankle_eversion_step_end_inverted <- anova(fm_trigger_leg_ankle_eversion_step_end_inverted)
rg_trigger_leg_ankle_eversion_step_end_inverted <- ref_grid(fm_trigger_leg_ankle_eversion_step_end_inverted)
confint_trigger_leg_ankle_eversion_step_end_inverted <- emmeans(rg_trigger_leg_ankle_eversion_step_end_inverted, "direction")

fm_trigger_leg_pero_lng_integrated_inverted <- lmer(trigger_leg_pero_lng_integrated_inverted ~ stance_foot*direction + (1|subject))
anova_trigger_leg_pero_lng_integrated_inverted <- anova(fm_trigger_leg_pero_lng_integrated_inverted)
rg_trigger_leg_pero_lng_integrated_inverted <- ref_grid(fm_trigger_leg_pero_lng_integrated_inverted)
confint_trigger_leg_pero_lng_integrated_inverted <- emmeans(rg_trigger_leg_pero_lng_integrated_inverted, "direction")


# save results
deg2rad = 57.2958;
ratio2percent = 100;
confint_contra_leg_hip_abduction_step_end_inverted_table <- print(confint_contra_leg_hip_abduction_step_end_inverted)
confint_contra_leg_hip_abduction_step_end_inverted_table$emmean = confint_contra_leg_hip_abduction_step_end_inverted_table$emmean * deg2rad
confint_contra_leg_hip_abduction_step_end_inverted_table$SE = confint_contra_leg_hip_abduction_step_end_inverted_table$SE * deg2rad
confint_contra_leg_hip_abduction_step_end_inverted_table$lower.CL = confint_contra_leg_hip_abduction_step_end_inverted_table$lower.CL * deg2rad
confint_contra_leg_hip_abduction_step_end_inverted_table$upper.CL = confint_contra_leg_hip_abduction_step_end_inverted_table$upper.CL * deg2rad

confint_contra_leg_glut_med_integrated_inverted_table <- print(confint_contra_leg_glut_med_integrated_inverted)
confint_contra_leg_glut_med_integrated_inverted_table$emmean = confint_contra_leg_glut_med_integrated_inverted_table$emmean * ratio2percent
confint_contra_leg_glut_med_integrated_inverted_table$SE = confint_contra_leg_glut_med_integrated_inverted_table$SE * ratio2percent
confint_contra_leg_glut_med_integrated_inverted_table$lower.CL = confint_contra_leg_glut_med_integrated_inverted_table$lower.CL * ratio2percent
confint_contra_leg_glut_med_integrated_inverted_table$upper.CL = confint_contra_leg_glut_med_integrated_inverted_table$upper.CL * ratio2percent

confint_trigger_leg_ankle_eversion_step_end_inverted_table <- print(confint_trigger_leg_ankle_eversion_step_end_inverted)
confint_trigger_leg_ankle_eversion_step_end_inverted_table$emmean = confint_trigger_leg_ankle_eversion_step_end_inverted_table$emmean * deg2rad
confint_trigger_leg_ankle_eversion_step_end_inverted_table$SE = confint_trigger_leg_ankle_eversion_step_end_inverted_table$SE * deg2rad
confint_trigger_leg_ankle_eversion_step_end_inverted_table$lower.CL = confint_trigger_leg_ankle_eversion_step_end_inverted_table$lower.CL * deg2rad
confint_trigger_leg_ankle_eversion_step_end_inverted_table$upper.CL = confint_trigger_leg_ankle_eversion_step_end_inverted_table$upper.CL * deg2rad

confint_trigger_leg_pero_lng_integrated_inverted_table <- print(confint_trigger_leg_pero_lng_integrated_inverted)
confint_trigger_leg_pero_lng_integrated_inverted_table$emmean = confint_trigger_leg_pero_lng_integrated_inverted_table$emmean * ratio2percent
confint_trigger_leg_pero_lng_integrated_inverted_table$SE = confint_trigger_leg_pero_lng_integrated_inverted_table$SE * ratio2percent
confint_trigger_leg_pero_lng_integrated_inverted_table$lower.CL = confint_trigger_leg_pero_lng_integrated_inverted_table$lower.CL * ratio2percent
confint_trigger_leg_pero_lng_integrated_inverted_table$upper.CL = confint_trigger_leg_pero_lng_integrated_inverted_table$upper.CL * ratio2percent


write.csv(format(anova_step_placement_x_inverted_mm, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/step_placement_x_inverted_mm.csv", sep = ""))
write.csv(format(print(confint_step_placement_x_inverted_mm), digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/step_placement_x_inverted_mm.csv", sep = ""))

write.csv(format(anova_stimulus_response_x_inverted_mm, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/stimulus_response_x_inverted_mm.csv", sep = ""))
write.csv(format(print(confint_stimulus_response_x_inverted_mm), digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/stimulus_response_x_inverted_mm.csv", sep = ""))

write.csv(format(anova_contra_leg_hip_abduction_step_end_inverted, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/contra_leg_hip_abduction_step_end_inverted.csv", sep = ""))
write.csv(format(confint_contra_leg_hip_abduction_step_end_inverted_table, digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/contra_leg_hip_abduction_step_end_inverted.csv", sep = ""))

write.csv(format(anova_contra_leg_glut_med_integrated_inverted, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/contra_leg_glut_med_integrated_inverted.csv", sep = ""))
write.csv(format(confint_contra_leg_glut_med_integrated_inverted_table, digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/contra_leg_glut_med_integrated_inverted.csv", sep = ""))


write.csv(format(anova_cop_from_com_x_integrated_inverted_mm, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/cop_from_com_x_integrated_inverted_mm.csv", sep = ""))
write.csv(format(print(confint_cop_from_com_x_integrated_inverted_mm), digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/cop_from_com_x_integrated_inverted_mm.csv", sep = ""))

write.csv(format(anova_trigger_leg_ankle_eversion_step_end_inverted, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/trigger_leg_ankle_eversion_step_end_inverted.csv", sep = ""))
write.csv(format(confint_trigger_leg_ankle_eversion_step_end_inverted_table, digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/trigger_leg_ankle_eversion_step_end_inverted.csv", sep = ""))

write.csv(format(anova_trigger_leg_pero_lng_integrated_inverted, digits=4), file = paste(data_root, .Platform$file.sep, "anovaResults/trigger_leg_pero_lng_integrated_inverted.csv", sep = ""))
write.csv(format(confint_trigger_leg_pero_lng_integrated_inverted_table, digits=4), file = paste(data_root, .Platform$file.sep, "confintResults/trigger_leg_pero_lng_integrated_inverted.csv", sep = ""))


