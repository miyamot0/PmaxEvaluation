# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

par(mfrow = c(1, 2))

# Figure 1-1
# Plot Approximate vs. Derivative
plot(compareFrame$HurshDerivative, 
     compareFrame$HurshPmax,
     main = "Derivative-based and Approximate Unit Elasticity",
     xlab = "Derivative-based",
     ylab = "Approximate",
     type = "p")
abline(lm(compareFrame$HurshPmax ~ compareFrame$HurshDerivative), col = "red")

# Figure 1-2
# Plot Derivative vs. Analytical
plot(compareFrame$HurshDerivative,
     compareFrame$AnalyticPmax,
     main = "Derivative-based and Analytical Unit Elasticity",
     xlab = "Derivative-based",
     ylab = "Analytical",
     type = "p")
abline(lm(compareFrame$AnalyticPmax ~ compareFrame$HurshDerivative), col = "green")