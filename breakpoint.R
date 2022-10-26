# Breakpoint analysis
library(segmented)

BRlm <- lm(Sensor.Value ~ SEL, data = BR_June)
summary(BRlm)

# When not providing estimates for the breakpoints "psi = NA" can be used.
# The number of breakpoints that will show up is not defined
BR_BP <- segmented(BRlm, 
                   seg.Z = ~ SEL, psi = NA)
                   #psi = list(Date.time.UTC = c(4, 15)))
summary(BR_BP)