library("pwr")

cohen.ES(test = 'r', size = 'medium')

r_power <- pwr.r.test(r=.128, sig.level = .05, n=90)

anova_power <- pwr.anova.test(k = 4, f=.128, sig.level = .05, n=90)
plot(anova_power)
