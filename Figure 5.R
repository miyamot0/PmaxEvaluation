# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

results$RAnalytic <- compareFrame$AnalyticPmax
results$RApproximate <- compareFrame$HurshPmax
results$RObserved <- compareFrame$ObservedPmax

par(mfrow = c(2, 3),     # 2x2 layout
    oma = c(2, 2, 1, 0), # two rows of text at the outer left and bottom margin
    mar = c(2, 2, 1, 0), # space for one row of text at ticks and to separate plots
    mgp = c(2.5, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)            # allow content to protrude into outer margin (and beyond)

results$Diff <- results$RAnalytic - results$RApproximate

plot(results$K, 
     results$Diff,
     axes = FALSE,
     main = "",
     ylab = expression('Approximated P'[MAX]*'  Difference'),
     xlim = c(1, 6),
     xlab = "")
axis(side = 2, labels = (i %% 2 == 0))
axis(side = 1, labels = NA)
box(which = "plot", bty = "l")

plot(results$Q0d, 
     results$Diff,
     axes = FALSE,
     main = "",
     ylab = "",
     #xlim = c(0.001, 0.009),
     xlab = "")
axis(side = 2, labels = NA)
axis(side = 1, labels = NA)
box(which = "plot", bty = "l")

plot(results$Alpha, 
     results$Diff,
     axes = FALSE,
     main = "",
     ylab = "",
     xlim = c(0.001, 0.009),
     xlab = "")
axis(side = 2, labels = NA)
axis(side = 1, labels = NA)
box(which = "plot", bty = "l")

results$Diff <- results$RAnalytic - results$RObserved

plot(results$K, 
     results$Diff,
     main = "",
     ylab = expression('Observed P'[MAX]*'  Difference'),
     xlim = c(1, 6),
     xlab = "K Value",
     axes = FALSE)
axis(side = 2, labels = (i %% 2 == 0))
axis(side = 1, labels = (i %% 2 == 0))
box(which = "plot", bty = "l")

plot(results$Q0d, 
     results$Diff,
     axes = FALSE,
     main = "",
     ylab = "",
     #xlim = c(0.001, 0.009),
     xlab = expression('Q'[0]*' Value'))
axis(side = 2, labels = NA)
axis(side = 1, labels = (i %% 2 == 0))
box(which = "plot", bty = "l")

plot(results$Alpha, 
     results$Diff,
     axes = FALSE,
     main = "",
     ylab = "",
     xlim = c(0.001, 0.009),
     xlab = "Alpha Value")
axis(side = 2, labels = NA)
axis(side = 1, labels = (i %% 2 == 0))
box(which = "plot", bty = "l")