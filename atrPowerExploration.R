library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
number_of_simulations = 10;

# read in data
dat = read.csv("~/R_wd/results_pilot.csv", header = TRUE)
atr = read.csv("~/Desktop/results.csv", header = TRUE)


# fit models
fm_atr_pushoff <- lmer(trigger_leg_ankle_dorsiflexion_integrated_TWO ~ affected_side + (1|subject), data=atr)

# define effect sizes
fixef(fm_atr_pushoff)["affected_sideTRIGGER_UNAFFECTED"] <- 0.02

# run power analysis
print(powerSimResults_pushoff <- powerSim(fm_atr_pushoff, nsim=number_of_simulations))

# extend
fm_atr_pushoff_extended <- extend(fm_atr_pushoff, along="subject", n=20)

# power sim for extended model
powerSim(fm_atr_pushoff_extended)

# power curve for extended model
power_curve_pushoff <- powerCurve(fm_atr_pushoff_extended, along="subject")


# fit models
fm_atr_step <- lmer(step_placement_x_inverted_ONE ~ affected_side + (1|subject), data=atr)

# define effect sizes
fixef(fm_atr_step)["affected_sideTRIGGER_UNAFFECTED"] <- 0.005

# run power analysis
print(powerSimResults_step <- powerSim(fm_atr_step, nsim=number_of_simulations))

# extend
fm_atr_step_extended <- extend(fm_atr_step, along="subject", n=20)

# power sim for extended model
powerSim(fm_atr_step_extended)

# power curve for extended model
power_curve_step <- powerCurve(fm_atr_step_extended, along="subject")



###### ######################################

number_of_simulations = 10;

# read in data
dat = read.csv("~/R_wd/results_pilot.csv", header = TRUE)
atr = read.csv("~/Desktop/results.csv", header = TRUE)


# fit models
fm_trigger_leg_pero_lng_integrated_inverted <- lmer(dat$trigger_leg_pero_lng_integrated_inverted ~ stance_foot + direction + (1|subject), data=dat)
lmer(atr$trigger_leg_ankle_dorsiflexion_integrated_ONE ~ affected_side + (1|subject), data=atr)
model1 <- glmer(z ~ x + (1|g), family="poisson", data=simdata)

# define effect sizes
fixef(fm_trigger_leg_pero_lng_integrated_inverted)["direction"] <- 0.02
fixef(fm_trigger_leg_ankle_dorsiflexion_integrated_ONE)["affected_sideTRIGGER_UNAFFECTED"] <- 0.02
fixef(model1)["x"] <- -0.05

# run power analysis
print(powerSimResults <- powerSim(fm_trigger_leg_pero_lng_integrated_inverted, test=fixed("direction"), nsim=number_of_simulations))
print(powerSimResults <- powerSim(fm_trigger_leg_ankle_dorsiflexion_integrated_ONE, test=fixed("affected_side"), nsim=number_of_simulations))
powerSim(model1, nsim=number_of_simulations)

# extend
fm_trigger_leg_pero_lng_integrated_inverted_extended <- extend(fm_trigger_leg_pero_lng_integrated_inverted, along=('subject'), n=20)
extend(fm_trigger_leg_ankle_dorsiflexion_integrated_ONE, along='subject', n=20, data=atr)
model2 <- extend(model1, along="x", n=20)

# power sim for extended model
power_sim_extended_1 <- powerSim(fm_trigger_leg_pero_lng_integrated_inverted_extended, test=fixed("direction"), nsim=10)
powerSim(fm_trigger_leg_ankle_dorsiflexion_integrated_ONE_extended, test=fixed("affected_side"), nsim=number_of_simulations)

# power curve for extended model
power_curve_1 <- powerCurve(fm_trigger_leg_pero_lng_integrated_inverted_extended, test=fixed("direction"), along="subject")
power_curve_2 <- powerCurve(fm_trigger_leg_ankle_dorsiflexion_integrated_ONE_extended, test=fixed("affected_side"), along="subject")

############################



# Power Analysis Example (file:///C:/Users/tfett/Downloads/GreenP.MacLeodCJ2016.pdf)

model1 <- glmer(z ~ x + (1|g), family="poisson", data=simdata)
fixef(model1)["x"] <- -0.05
powerSim(model1, nsim=number_of_simulations)
model2 <- extend(model1, along="x", n=20)
powerSim(model2)
pc2 <- powerCurve(model2)