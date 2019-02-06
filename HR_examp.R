library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
number_of_simulations = 1000;
# read in data
dat = read.csv("~/R_wd/results_2removed.csv", header = TRUE)


# fit models
fm_step <- lmer(step_placement_x_inverted_ONE ~ direction + (1|subject), data=dat)

# define effect sizes
fixef(fm_step)["directionSTIM_TOWARDS"] <- 0.005

# run power analysis
print(powerSimResults_step <- powerSim(fm_step, nsim=number_of_simulations))

# extend
fm_step_extended <- extend(fm_step, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended)

# power curve for extended model
power_curve_step <- powerCurve(fm_step_extended, along="subject")