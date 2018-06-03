# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

par(mfrow = c(1, 1))

# Prepare boxplots, for descriptive summary
suppressMessages(boxPlotData <- reshape2::melt(compareFrame))

boxplot(value~variable,
        data=boxPlotData, 
        main="Distribution of Unit Elasticity Calculations", 
        #ylim = c(0, 100),
        xlab="Unit Elasticity Method", 
        xaxt = "n",
        ylab="Calculation Result")

atx <- 1:4
labels <- c("Approximated", "Observed", "Derivative", "Analytic")
axis(1, at=atx, labels=labels)