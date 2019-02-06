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



# fit models
fm_step_noinvert <- lmer(step_placement_x_ONE ~ direction + (1|subject), data=dat)

# define effect sizes
fixef(fm_step_noinvert)["directionSTIM_TOWARDS"] <- 0.005

# run power analysis
print(powerSimResults_step_noinvert <- powerSim(fm_step_noinvert, nsim=number_of_simulations))

# extend
fm_step_extended_noinvert <- extend(fm_step_noinvert, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_noinvert)

# power curve for extended model
power_curve_step_noinvert <- powerCurve(fm_step_extended_noinvert, along="subject")




#######################################################################################################
## Replicate HR Power Analysis ##
threesubj = read.csv("~/R_wd/results_3subj.csv", header = TRUE)

# fit models
fm_step_3subj <- lmer(step_placement_x_inverted_ONE ~ direction + (1|subject), data=threesubj)

# define effect sizes
fixef(fm_step_3subj)["directionSTIM_TOWARDS"] <- 0.005

# run power analysis
print(powerSimResults_step_3subj <- powerSim(fm_step_3subj, nsim=number_of_simulations))

# extend
fm_step_extended_3subj <- extend(fm_step_3subj, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_3subj)

# power curve for extended model
power_curve_step_3subj <- powerCurve(fm_step_extended_3subj, along="subject")



#######################################################################################################
## Replicate HR Power Analysis ##
threesubj2stim = read.csv("~/R_wd/results_3subj2stim.csv", header = TRUE)

# fit models
fm_step_3subj2stim <- lmer(step_placement_x_inverted_ONE ~ direction + (1|subject), data=threesubj2stim)

# define effect sizes
fixef(fm_step_3subj2stim)["directionSTIM_TOWARDS"] <- 0.005

# run power analysis
print(powerSimResults_step_3subj2stim <- powerSim(fm_step_3subj2stim, nsim=number_of_simulations))

# extend
fm_step_extended_3subj2stim <- extend(fm_step_3subj2stim, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_3subj2stim)

# power curve for extended model
power_curve_step_3subj2stim <- powerCurve(fm_step_extended_3subj2stim, along="subject")

#######################################################################################################
## Replicate HR Power Analysis ##
threesubj2stim = read.csv("~/R_wd/results_3subj2stim.csv", header = TRUE)

# fit models
fm_step_3subj2stim_bin <- lmer(step_placement_x_inverted_ONE ~ direction_bin + (1|subject), data=threesubj2stim)

# define effect sizes
fixef(fm_step_3subj2stim)[-1] <- 0.005

# run power analysis
print(powerSimResults_step_3subj2stim_bin <- powerSim(fm_step_3subj2stim_bin, nsim=number_of_simulations))

# extend
fm_step_extended_3subj2stim_bin <- extend(fm_step_3subj2stim_bin, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_3subj2stim_bin)

# power curve for extended model
power_curve_step_3subj2stim_bin <- powerCurve(fm_step_extended_3subj2stim_bin, along="subject")

#######################################################################################################

## Replicate HR Power Analysis Invert STIM_AWAY ##

threesubj2stim = read.csv("~/R_wd/results_3subj2stim.csv", header = TRUE)
vision_indices_away = which(threesubj2stim$direction == 'STIM_AWAY')

for (i in vision_indices_away){
threesubj2stim$step_placement_x_inverted_ONE[i] <- threesubj2stim$step_placement_x_inverted_ONE[i] * -1
}

# fit models
fm_step_3subj2stim_bin <- lmer(step_placement_x_inverted_ONE ~ direction_bin + (1|subject), data=threesubj2stim)

# define effect sizes
fixef(fm_step_3subj2stim_bin)["direction_bin"] <- 0.005

# run power analysis
print(powerSimResults_step_3subj2stim_bin <- powerSim(fm_step_3subj2stim_bin, nsim=number_of_simulations))

# extend
fm_step_extended_3subj2stim_bin <- extend(fm_step_3subj2stim_bin, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_3subj2stim_bin)

# power curve for extended model
power_curve_step_3subj2stim_bin <- powerCurve(fm_step_extended_3subj2stim_bin, along="subject")

#######################################################################################################

# for writing to log
con <- file("results_pilot_power.log")
sink(con, append=TRUE)

## Replicate HR Power Analysis ##
vision = read.csv("~/R_wd/results_pilot.csv", header = TRUE)


# fit models
fm_step_vision <- lmer(step_placement_x_inverted ~ direction + (1|subject), data=vision)

# define effect sizes
# (-1 for direction)
fixef(fm_step_vision)[-1] <- 0.005

# run power analysis
print(powerSimResults_step_vision <- powerSim(fm_step_vision, nsim=number_of_simulations))

# extend
fm_step_extended_vision <- extend(fm_step_vision, along="subject", n=20)

# power sim for extended model
powerSim(fm_step_extended_vision)

# power curve for extended model
power_curve_step <- powerCurve(fm_step_extended_vision, along="subject")


#######################################################################################################

# create indices
vision_indices_away = which(threesubj$direction == 'STIM_AWAY')
vision_indices_towards = which(threesubj$direction == 'STIM_TOWARDS')
vision_indices_away_bin = which(vision$direction == -1)
vision_indices_towards_bin = which(vision$direction == 1)

cop_from_com_x_integrated_inverted_mm_away_threesubj <- threesubj$cop_from_com_x_integrated_inverted_ONE[vision_indices_away] * -1000
step_placement_x_inverted_mm_away_threesubj <- threesubj$step_placement_x_inverted_ONE[vision_indices_away] * -1000

cop_from_com_x_integrated_inverted_mm_towards_threesubj <- threesubj$cop_from_com_x_integrated_inverted_ONE[vision_indices_towards]  * 1000
step_placement_x_inverted_mm_towards_threesubj <- threesubj$step_placement_x_inverted_ONE[vision_indices_towards] * 1000


cop_from_com_x_integrated_inverted_mm_away_vision <- vision$cop_from_com_x_integrated_inverted[vision_indices_away_bin] * 1000
step_placement_x_inverted_mm_away_vision <- vision$step_placement_x_inverted[vision_indices_away_bin] * 1000

cop_from_com_x_integrated_inverted_mm_towards_vision <- vision$cop_from_com_x_integrated_inverted[vision_indices_towards_bin] * 1000
step_placement_x_inverted_mm_towards_vision <- vision$step_placement_x_inverted[vision_indices_towards_bin] * 1000


plot(step_placement_x_inverted_mm_away_threesubj)
plot(step_placement_x_inverted_mm_away_vision)


xyplot(vision$cop_from_com_x_integrated_inverted_ONE[vision_indices_control] ~ vision$step_placement_x_inverted_ONE[vision_indices_control] | vision$subject[vision_indices_control], 
       main="lateral ankle vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. CoP-CoM (m s)")
