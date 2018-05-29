
# Perform Simulations?
if (FALSE) {
  # Set seed for replicability
  set.seed(65535)
  
  # SD for residuals
  sdMap <- 0.5
  
  # maximum n series to simulate
  nPoints <- 5000
  
  # Apt data prices, means, sd's
  pricePoints <- c(0,0.25,0.5,1,1.5,2,2.5,3,4,5,6,7,8,9,10,15,20)
  
  # Data means from APT
  consumptionMean <- c(5.856884058,5.425724638,5.257246377,5.049818841,4.714673913,
                       4.413043478,4.081521739,3.685688406,3.151268116,2.674818841,
                       2.16576087,1.799637353,1.424818841,1.113327289,0.923913043,
                       0.490942029,0.350543478)
  
  # Data sd's from APT
  consumptionSD <- c(4.705973278,4.301995389,4.194430187,3.938184029,3.70212462,
                     3.457774816,3.262748553,3.018651843,2.808459372,2.470158851,
                     2.252749078,2.349112958,1.78097839,1.733577022,1.699213558,
                     1.189442317,0.900553209)
  
  # pre-allocate a frame
  preallocatedFrame <- data.frame(matrix(vector(),
                                         nPoints,
                                         length(pricePoints),
                                         dimnames = list(c(),
                                                         c(pricePoints))),
                                  stringsAsFactors = FALSE)
  
  # Naming conventions (they are odd with numerics)
  pricePointsName <- names(preallocatedFrame)
  
  for (i in 1:length(pricePointsName)) {
    # Based on means/sds
    preallocatedFrame[,pricePointsName[i]] <- rnorm(nPoints, mean=consumptionMean[i], sd=sdMap*consumptionSD[i])
  }
  
  # Restore colnames, add row #'s and columns for passes
  colnames(preallocatedFrame) <- pricePoints
  preallocatedFrame$row <- seq(from = 1, to = nrow(preallocatedFrame), by = 1)
  preallocatedFrame$pass <- NA
  
  # Round negatives to flat zero
  tempMat <- as.matrix(preallocatedFrame)
  tempMat[tempMat < 0] <- 0
  preallocatedFrame <- as.data.frame(tempMat)
  
  # Loop through beez to get # passing
  for (i in 1:nrow(preallocatedFrame)) {
    test <- data.frame(id = rep(preallocatedFrame[i, "row"], length(pricePoints)),
                       x = pricePoints,
                       y = c(unname(unlist(preallocatedFrame[i, 1:17]))))
    
    preallocatedFrame[i, "pass"] <- beezdemand::CheckUnsystematic(test)$TotalPass
  }
  
  # Select series that hit all 3 passes
  passingSeriesFrame = preallocatedFrame[preallocatedFrame$pass == 3, 1:17]
  passingSeriesFrame$id <- 1:nrow(passingSeriesFrame)
}

# Trim to 1000?
if (TRUE) {
  if (nrow(passingSeriesFrame)) {
    # grab only first 1000
    passingSeriesFrame <- passingSeriesFrame[1:1000,]
    
  } else {
    message("Not enough series")
    break;
    
  }
}

# Helper fx's
if (TRUE) {
  # Ben Bolker's port from GSL
  # GPLv3
  # 
  # z = input
  # b = branch (principal, by default)
  # eps = machine error
  # min-imag = imaginary value
  lambertW = function(z,b=0,maxiter=10,eps=.Machine$double.eps,
                      min.imag=1e-9) {
    if (any(round(Re(b)) != b))
      stop("branch number for W must be an integer")
    if (!is.complex(z) && any(z<0)) z=as.complex(z)
    ## series expansion about -1/e
    ##
    ## p = (1 - 2*abs(b)).*sqrt(2*e*z + 2);
    ## w = (11/72)*p;
    ## w = (w - 1/3).*p;
    ## w = (w + 1).*p - 1
    ##
    ## first-order version suffices:
    ##
    w = (1 - 2*abs(b))*sqrt(2*exp(1)*z + 2) - 1
    ## asymptotic expansion at 0 and Inf
    ##
    v = log(z + as.numeric(z==0 & b==0)) + 2*pi*b*1i;
    v = v - log(v + as.numeric(v==0))
    ## choose strategy for initial guess
    ##
    c = abs(z + exp(-1));
    c = (c > 1.45 - 1.1*abs(b));
    c = c | (b*Im(z) > 0) | (!Im(z) & (b == 1))
    w = (1 - c)*w + c*v
    ## Halley iteration
    ##
    for (n in 1:maxiter) {
      p = exp(w)
      t = w*p - z
      f = (w != -1)
      t = f*t/(p*(w + f) - 0.5*(w + 2.0)*t/(w + f))
      w = w - t
      if (abs(Re(t)) < (2.48*eps)*(1.0 + abs(Re(w)))
          && abs(Im(t)) < (2.48*eps)*(1.0 + abs(Im(w))))
        break
    }
    if (n==maxiter) warning(paste("iteration limit (",maxiter,
                                  ") reached, result of W may be inaccurate",sep=""))
    if (all(Im(w)<min.imag)) w = as.numeric(w)
    return(w)
  }
  
  # Does what it says on the tin, calculates pmax
  CalculateHurshPmax <- function(Q0_, alpha_, K_) {
    (1/(Q0_ * alpha_ * K_^1.5)) * (0.083 * K_ + 0.65)
  }
  
  # Does what it says on the tin, calculates slope using Hursh's derivative equation
  CalculateHurshDerivative <- function(price_, Q0_, alpha_, K_) {
    (log((10^K_)) * (-alpha_ * Q0_ * price_ * exp(-alpha_ * Q0_ * price_)))
  }
  
  SlopeLossFunction <- function(par, data) {
    abs((log((10^data$K)) * (-data$A * data$Q0 * par[1] * exp(-data$A * data$Q0 * par[1]))) + 1)
  }
  
  SlopeDifferential <- function(Q0_, alpha_, K_) {
    # Note, prices are in log-units to preserve the log-log comparison
    prices <- seq(-2, 3, 0.001)
    
    # Consumption, with prices expressed with exponential changes
    consumption <- log(Q0_)/log(10) + K_ * (exp(-alpha_ * Q0_ * 10^prices) - 1)
    
    # Calculate all deltas for consumption and divide by deltas in prices
    slope <- diff(consumption)/diff(prices)
    
    # Modify slope
    slope <- abs(slope + 1)
    
    # Find the smallest slope value (i.e., 0) and return the corresponding unit price
    10^(prices[which.min(slope) + 1])  
  }
  
  GetSolution <- function(Q0_, K_, A_) {
    starts <- CalculateHurshPmax(Q0_, A_, K_)
    
    dat <- data.frame(Q0 = Q0_,
                      A = A_,
                      K = K_) 
    
    result <- optimx::optimx(par = c(starts), 
                             fn = SlopeLossFunction,
                             data = dat,
                             method = c("BFGS"),
                             control=list(maxit=2500))
    
    return(result$p1)
  }  
}

# Prep for analysis
data = reshape::melt(passingSeriesFrame, id.vars = c("id"))
  colnames(data) <- c("id", "x", "y")

  # Switch to long-form, for beez
  data$id <- as.numeric(data$id)
  data$x <- as.numeric(data$x)
  data$y <- as.numeric(data$y)
  
results <- beezdemand::FitCurves(data, equation = "hs", k = "ind", idcol = "id")
results <- results[, c("Q0d","K", "Alpha", "Pmaxd")]
  
# Pre-allocate for speed
compareFrame <- data.frame(id = 1:nrow(results))
compareFrame$HurshPmax <- NA
compareFrame$HurshDerivative <- NA
compareFrame$SlopeDifferential <- NA
compareFrame$AnalyticPmax <- NA

compareFrame <- compareFrame[, 2:5]

# Perform Measures
for (i in 1:nrow(compareFrame)) {
  message(paste("Scoring #", i, " of ", nrow(compareFrame), sep = ""))
  
  compareFrame[i, "HurshPmax"] <- CalculateHurshPmax(results[i,"Q0d"], results[i,"Alpha"], results[i, "K"])
  compareFrame[i, "HurshDerivative"] <- GetSolution(results[i,"Q0d"], results[i, "K"], results[i,"Alpha"])
  compareFrame[i, "SlopeDifferential"] <- SlopeDifferential(results[i,"Q0d"], results[i,"Alpha"], results[i, "K"])
  compareFrame[i, "AnalyticPmax"] <- -lambertW(z = -1/log((10^results[i, "K"]))) / (results[i,"Alpha"] * results[i,"Q0d"])
}

# Prepare boxplots, for descriptive summary
suppressMessages(boxPlotData <- reshape2::melt(compareFrame))

boxplot(value~variable,
        data=boxPlotData, 
        main="Pmax Calculations", 
        ylim = c(0, 100),
        xlab="Calculation Method", 
        ylab="Pmax Value")

# Correlations, to the sixth decimal
correlationTable<-round(cor(compareFrame), 6)

# Print out matrix
correlationTableClean<-correlationTable
correlationTableClean[lower.tri(correlationTable, diag=TRUE)]<-""
correlationTableClean<-as.data.frame(correlationTableClean)
correlationTableClean

# Figure 1
# Plot Approximate vs. Derivative
plot(compareFrame$HurshDerivative, 
     compareFrame$HurshPmax,
     main = "Approximate and Derivative-based PMAX",
     xlab = "Derivative-based PMAX",
     ylab = "Approximated PMAX",
     type = "p")
abline(lm(compareFrame$HurshPmax ~ compareFrame$HurshDerivative), col = "red")

# Figure 2
# Plot Derivative vs. Analytical
plot(compareFrame$HurshDerivative,
     compareFrame$AnalyticPmax,
     main = "Correlation of Approximated and Symbolic PMAX",
     xlab = "Derivative-based Solution",
     ylab = "Analytical Solution",
     type = "p")
abline(lm(compareFrame$AnalyticPmax ~ compareFrame$HurshDerivative), col = "green")