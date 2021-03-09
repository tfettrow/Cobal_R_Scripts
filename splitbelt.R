library(lme4)
library(emmeans)
library(lmerTest)
library(simr)
library(ggpubr)
library(magrittr)
library(plyr)
library(dplyr)
library(stringi)
library(lattice)
library(ggplot2)
library(ggsignif)
library(xtable)
library(tidyverse)
library(forcats)
library(stargazer)
library(huxtable)
library(readxl)
library(broom)
library(purrr)
library(tidyverse)
library(tibble)

setwd("Z:/Shared drives/GABA_Aging_MoCap")
dat <- read.csv(file = 'results_pleateu_split.csv')

lm_model <- lmer(step_length_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
vc <- VarCorr(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


lm_model <- lmer(step_time_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



lm_model <- lmer(stance_time_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



lm_model <- lmer(cop_from_mpsis_x_integrated_singlestance_response_sym ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



lm_model <- lmer(step_width_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


lm_model <- lmer(stimulus_response_x_sym ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



setwd("Z:/Shared drives/GABA_Aging_MoCap")
dat <- read.csv(file = 'results_pleateu_after.csv')

lm_model <- lmer(step_length_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
vc <- VarCorr(lm_model)
rg_lm_model <- ref_grid(lm_model)
confint_lm_model <- emmeans(rg_lm_model, "group")
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


lm_model <- lmer(step_time_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


lm_model <- lmer(stance_time_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



lm_model <- lmer(cop_from_mpsis_x_integrated_singlestance_response_sym ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)



lm_model <- lmer(step_width_response ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)


lm_model <- lmer(stimulus_response_x_sym ~ group*startfoot +  (1|subject), data=dat, REML = FALSE)
anova_lm_model <- anova(lm_model)
lsmeans_results <- lsmeans(lm_model,pairwise~group*startfoot)
pval <- summary(lsmeans_results$contrasts)$p.value
fe <- hux(fixef(lm_model, add.dropped=TRUE))
ant <- hux(anova_lm_model)
ant$`Sum Sq`<- NULL
ant$NumDF<- NULL
ant$Estimate <- fe
ant$Estimate[1]<- "Estimate"
ant$Estimate[2]<- paste(ant$Estimate[2], "(YA)")
ant$Estimate[3]<- paste(ant$Estimate[3], "(fast)")
ant$Estimate[4]<- paste(ant$Estimate[4], "(YA:fast)")
ant <- ant[c("Estimate", "Mean Sq", "DenDF", "F value", "Pr(>F)")]
ant <- add_rownames(ant)
col_width(ant) <- .5
col_width(ant)
ant <- set_col_width(ant, 1, 1)
col_width(ant)
bottom_border(ant)[1, ] <- brdr(0.4, "solid", "blue")
print_latex(ant)




setwd("Z:/Shared drives/GABA_Aging_MoCap")
dat <- read.csv(file = 'results_mag_after_left.csv')


lm_model <- lmer(after~variable + (1|subject), data=dat)
anova_lm_model <- anova(lm_model)
lm_model <- lm(stp~variable, data=dat)
anova_lm_model <- anova(lm_model)
emm_options(pbkrtest.limit = 3150)
emm_options(lmerTest.limit = 3150)
lsmeans_results <- lsmeans(lm_model,pairwise~variable)

emtrends_results <- emtrends(lm_model, pairwise~variable, trend = "group")
emtrends_results <- emtrends(lm_model, "variable", var = "group")

pval <- summary(lsmeans_results$contrasts)$p.value

res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

man_res <- manova(cbind(step_length_response, step_time_response,stance_time_response)~group,data=dat)

t.test(dat$step_length_response~dat$group, mu = 0, alternative = "two.sided")
t.test(step_length_response~group, data=dat)


t.test(step_time_response ~ group, data=dat)
t.test(stance_time_response ~ group, data=dat)
t.test(cop_from_mpsis_x_integrated_singlestance_response_sym ~ group, data=dat)
t.test(step_width_response ~ group, data=dat)
t.test(stimulus_response_x_sym ~ group, data=dat)

tab <- map_df(list(t1, t2, t3, t4, t5, t6), tidy)

setwd("Z:/Shared drives/GABA_Aging_MoCap")
dat <- read.csv(file = 'results_mag_after_right.csv')

t.test(step_length_response ~ group, data=dat)
t.test(step_time_response ~ group, data=dat)
t.test(stance_time_response ~ group, data=dat)
t.test(cop_from_mpsis_x_integrated_singlestance_response_sym ~ group, data=dat)
t.test(step_width_response ~ group, data=dat)
t.test(stimulus_response_x_sym ~ group, data=dat)

