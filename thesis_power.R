
# Power analyses ----------------------------------------------------------
library(pwr)

# For Hypothesis 1a
pwr.t.test(d = .14, n = 44, 
           sig.level = .05, 
           type = "two.sample", 
           alternative = "two.sided")

# For Hypothesis 1b
f_estimate_power_hyp1b <- sqrt((.9*3)/(88-4))
pwr.anova.test(f = f_estimate_power_hyp1b, 
               k = 4,
               n = 22,
               sig.level = .05,
               power = NULL)

# For Hypothesis 3
pwr.t.test(n = 44, 
           d = .35, 
           sig.level = 0.05, 
           power = NULL, 
           type = "two.sample", 
           alternative = "two.sided")

# For Hypothesis 4
power.prop.test(p1 = .5, 
                p2 = .32, 
                n = 8,
                sig.level = 0.05,
                alternative = "two.sided")

