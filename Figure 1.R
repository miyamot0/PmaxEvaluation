# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

par(mfrow = c(1, 2))

# These values are used in all demonstrations
Q0 <- 103.24
alpha <- 0.000283586
K <- 1.5

GetDemandConsumption <- function(P, Q0, A, K) {
  log(Q0)/log(10) + K * (exp(-alpha * Q0 * P) - 1)
}

prices <- seq(-1, 2, 0.01)
consumption <- log(Q0)/log(10) + K * (exp(-alpha * Q0 * 10^prices) - 1)

# Normal demand curve plot
plot(prices, 
     consumption,
     main = "Demand Curve in Logarithmic Units",
     yaxt = "n",
     xaxt = "n",
     ylab = "Consumption",
     xlab = "Unit Price",
     type = "l",
     ylim = c(-1, 2),
     xlim = c(-1, 2))

# Custom axis for clarity
atx <- c(-1, 0, 1, 2)
labels <- sapply(atx, function(i) as.expression(bquote(.(10^i))) )
axis(1, at=atx, labels=labels)

# Same as above
aty <- c(-1, 0, 1, 2, 3)
labels2 <- sapply(aty, function(i) as.expression(bquote(.(10^i))) )
axis(2, at=aty, labels=labels2)

abline(v = log10(0.1), col = "lightgray")
abline(h = log10(0.1), col = "lightgray")

for (i in seq(0, 100, by = 10)) {
  abline(v = log10(i), col = "lightgray")
  abline(h = log10(i), col = "lightgray")
}

pMax <- 15.62583
Cons <- GetDemandConsumption(pMax, Q0, alpha, K)

points(log10(pMax), Cons, col = "black")

lines(prices, consumption)

# TODO: clean up margins


# Side Fig

prices <- seq(0.5, 1.8, 0.01)
consumption <- log(Q0)/log(10) + K * (exp(-alpha * Q0 * 10^prices) - 1)

# Normal demand curve plot
plot(prices, 
     consumption,
     main = "Demand at Unit Elasticity",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "Unit Price",
     type = "l",
     lty = 3,
     ylim = c(log10(20), log10(40)),
     xlim = c(log10(10), log10(20)))

# Custom axis for clarity
atx <- c(1, log10(20), log10(30), log10(40), log10(50))
labels <- sapply(atx, function(i) as.expression(bquote(.(10^i))) )
axis(1, at=atx, labels=labels)

# Same as above
aty <- c(1, log10(20), log10(30), log10(40), log10(50))
labels2 <- sapply(aty, function(i) as.expression(bquote(.(10^i))) )
axis(2, at=aty, labels=labels2)

for (i in seq(0, 100, by = 10)) {
  abline(v = log10(i), col = "lightgray")
  abline(h = log10(i), col = "lightgray")
}

points(log10(pMax), Cons, col = "black")

prePmax <- log10(pMax - 0.5)
preCons <- GetDemandConsumption(10^prePmax, Q0, alpha, K)

postPmax <- log10(pMax + 0.5)
postCons <- GetDemandConsumption(10^postPmax, Q0, alpha, K)

yDiff <- preCons - postCons
xDiff <- prePmax - postPmax

points(c(prePmax, postPmax), c(preCons, postCons), col = "gray", pch = 21)

lines(c(prePmax, prePmax), c(preCons - 0.005, postCons))
lines(c(prePmax, postPmax - 0.005), c(postCons, postCons))

text(log10(15.5), log10(24.75), 
     labels = paste("Price Difference: \n", round(postPmax - prePmax, 4), sep = ""))
arrows(log10(15.5), 
       log10(26), 
       x1 = log10(15.5), 
       y1 = log10(28),
       code = 2)

text(log10(11.6), log10(28.55), 
     labels = paste("Consumption Difference: \n", round(postCons - preCons, 4), sep = ""))
arrows(log10(13.5), 
       log10(29), 
       x1 = log10(15), 
       y1 = log10(29),
       code = 2)

text(log10(15.5), log10(35.25), 
     labels = paste("PMAX = -0.0278 / 0.0278 = -1", sep = ""))
arrows(log10(pMax), 
       log10(34), 
       x1 = log10(pMax), 
       y1 = log10(30),
       code = 2)