# create indices
atr_indices_affected = which(atr$affected_side == 'TRIGGER_AFFECTED' )
atr_indices_unaffected = which(atr$affected_side == 'TRIGGER_UNAFFECTED') 

install.packages("lme4")
library(lme4)
install.packages("simr")
library(simr)
#install.packages("sjPlot")
#library(sjPlot) # table functions
#library(sjmisc) # sample data
require(lattice) # xy plots


number_of_simulations = 100;

step_placement_x_inverted_mm_allSTIMS <- atr$step_placement_x_inverted_ONE
stimulus_response_x_inverted_mm_allSTIMS <- atr$stimulus_response_x_inverted_ONE
trigger_leg_ankle_dorsiflexion_integrated_deg_allSTIMs <- atr$trigger_leg_ankle_dorsiflexion_integrated_TWO
atr_affected_side <- atr$affected_side
subject <- atr$subject

# PowerAnalysis for pushoff
fm_pushoff_integrated_mm <- lmer(trigger_leg_ankle_dorsiflexion_integrated_deg_allSTIMs ~ atr_affected_side + (1|subject), data = atr)
powerSim(fm_pushoff_integrated_mm, test=fixed("atr_affected_side"), nsim=number_of_simulations)
powerSim(fm_pushoff_integrated_mm)

fm_pushoff_integrated_mm_extended <- extend(fm_pushoff_integrated_mm, along="subject", n=20)
fixef(fm_pushoff_integrated_mm_extended)["atr_affected_side"] <- 5
powerSim(fm_pushoff_integrated_mm_extended, nsim=number_of_simulations)

# PowerAnalysis for step placement
fm_step_mm <- lmer(step_placement_x_inverted_mm_allSTIMS ~ atr_affected_side + (1|subject), data = atr)
powerSim(fm_step_mm, test=fixed("atr_affected_side"), nsim=number_of_simulations)
powerSim(fm_step_mm)

# PowerAnalysis for stim response
#fixef(fm_stim_mm)["atr_affected_sideTRIGGER_UNAFFECTED"]
fm_stim_mm <- lmer(stimulus_response_x_inverted_mm_allSTIMS ~ atr_affected_side + (1 |subject), data = atr)
powerSim(fm_stim_mm, test = fixed("atr_affected_sideTRIGGER_UNAFFECTED"), nsim=number_of_simulations)
powerSim(fm_stim_mm)

fm_stim_mm_extended <- extend(fm_stim_mm, along="subject", n=20)
powerSim(fm_stim_mm_extended, test = fixed("atr_affected_side"), nsim=10)
powerCurve(fm_stim_mm_extended, test=fixed("atr_affected_side"), along="subject", nsim=100)






# # # plots and other stuff # # #
xyplot(trigger_leg_ankle_dorsiflexion_integrated_deg_affected ~ step_placement_x_inverted_mm_affected | subject_affected,
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(trigger_leg_ankle_dorsiflexion_integrated_deg_affected ~ stimulus_response_x_inverted_mm_affected | subject_affected,
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(trigger_leg_ankle_dorsiflexion_integrated_deg_unaffected ~ step_placement_x_inverted_mm_unaffected | subject_unaffected,
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")

xyplot(trigger_leg_ankle_dorsiflexion_integrated_deg_unaffected ~ stimulus_response_x_inverted_mm_unaffected | subject_unaffected,
       main="Pushoff vs. step placement",
       xlab="Step Placment (m)",
       ylab="int. Ankle Dorsiflexion (deg s)")


step_placement_x_inverted_mm_affected <- atr$step_placement_x_inverted_ONE[atr_indices_affected] * 1000
stimulus_response_x_inverted_mm_affected <- atr$stimulus_response_x_inverted_ONE[atr_indices_affected] * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_affected <- atr$trigger_leg_ankle_dorsiflexion_integrated_TWO[atr_indices_affected]
subject_affected <- atr$subject[atr_indices_affected]

step_placement_x_inverted_mm_unaffected <- atr$step_placement_x_inverted_ONE[atr_indices_unaffected] * 1000
stimulus_response_x_inverted_mm_unaffected <- atr$stimulus_response_x_inverted_ONE[atr_indices_unaffected] * 1000
trigger_leg_ankle_dorsiflexion_integrated_deg_unaffected <- atr$trigger_leg_ankle_dorsiflexion_integrated_TWO[atr_indices_unaffected] 
subject_unaffected <- atr$subject[atr_indices_unaffected]


# testing linear regression analysis
iPO_step_affected_model_exp = lmer(step_placement_x_inverted_mm_affected ~ trigger_leg_ankle_dorsiflexion_integrated_deg_affected + (1|subject_affected), data=atr)

summary(iPO_step_affected_model_exp)

plot(fitted(iPO_step_towards_model_exp),residuals(iPO_step_towards_model_exp))
hist(residuals(iPO_step_towards_model_exp))
qqnorm(residuals(iPO_step_towards_model_exp))

anova(iPO_step_towards_model_null,iPO_step_towards_model_exp)

confint(iPO_step_affected_model_exp)
