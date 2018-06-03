# Shawn Gilroy
# GPL-V3
# Pmax Evaluation

par(mfrow = c(1, 2))

# Demand Curve Slope

Q0 <- 103.24
alpha <- 0.000283586
K <- 1.5

prices <- seq(0.1, 35, 0.01)
slope <- CalculateHurshDerivative(prices, Q0, alpha, K)

# Plot of derivative values
plot(prices, slope,
     main = "First Order Derivative",
     xaxt = "n",
     ylab = "Slope",
     xlab = "Unit Price",
     type = "l",
     log = "x")

# Custom axis for clarity
atx <- c(0.1, 1, 10, 100, 1000)
labels <- sapply(atx, function(i) as.expression(bquote(.(i))) )
axis(1, at=atx, labels=labels)

# Arrow to pmax
arrows(5, 
       -1, 
       x1 = 15, 
       y1 = -1,
       code = 2)

# Pmax text
text(2.5, -1, labels = c("Exact Pmax"))

text(0.2, -1.2, labels = c("Pmax: 15.6278"))

# Demand Modification

#prices <- seq(0.1, 100, 0.01)
#slope <- CalculateHurshDerivative(prices, Q0, alpha, K)

# Note: this is the operative change
slope <- abs(slope + 1)

# Plot of modified demand curve
plot(prices, slope,
     main = "Loss Function",
     xaxt = "n",
     ylab = "Loss Value",
     xlab = "Unit Price",
     type = "l",
     log = "x")

# Pretty axes
atx <- c(0.1, 1, 10, 100, 1000)
labels <- sapply(atx, function(i) as.expression(bquote(.(i))) )
axis(1, at=atx, labels=labels)

# Arrow to first zero value
arrows(15.62583,
       0.7,
       x1 = 15.62583,
       y1 = 0.1,
       code = 2)

# Label for derivative-based Pmax
text(15, 0.8, labels = c("Exact Pmax"))

text(0.2, 0.05, labels = c("Pmax: 15.6278"))