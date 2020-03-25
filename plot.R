# plot script
## Setup
yran <- range(X, # simulated path
              mu., VaR., # fitted conditional mean and VaR_alpha
              mu.predict, VaR.predict, VaR.CI) # predicted mean, VaR and CIs
myran <- max(abs(yran))
yran <- c(-myran, myran) # y-range for the plot
xran <- c(1, length(X) + m) # x-range for the plot

## Simulated (original) data (X_t), fitted conditional mean mu_t and VaR_alpha
plot(X, type = "l", xlim = xran, ylim = yran, xlab = "Time t", ylab = "",
     main = "Simulated ARMA-GARCH, fit, VaR, VaR predictions and CIs")
lines(as.numeric(mu.), col = adjustcolor("darkblue", alpha.f = 0.5)) # hat{\mu}_t
lines(VaR., col = "darkred") # estimated VaR_alpha
mtext(paste0("Expected exceed.: ",btest$expected.exceed,"   ",
             "Actual exceed.: ",btest$actual.exceed,"   ",
             "Test: ", btest$cc.Decision),
      side = 4, adj = 0, line = 0.5, cex = 0.9) # label

## Predictions
t. <- length(X) + seq_len(m) # future time points
lines(t., mu.predict, col = "blue") # predicted process X_t (or mu_t)
lines(t., VaR.predict, col = "red") # predicted VaR_alpha
lines(t., VaR.CI[1,], col = "orange") # lower 95%-CI for VaR_alpha
lines(t., VaR.CI[2,], col = "orange") # upper 95%-CI for VaR_alpha
legend("bottomright", bty = "n", lty = rep(1, 6), lwd = 1.6,
       col = c("black", adjustcolor("darkblue", alpha.f = 0.5), "blue",
               "darkred", "red", "orange"),
       legend = c(expression(X[t]), expression(hat(mu)[t]),
                  expression("Predicted"~mu[t]~"(or"~X[t]*")"),
                  substitute(widehat(VaR)[a], list(a = alpha)),
                  substitute("Predicted"~VaR[a], list(a = alpha)),
                  substitute("95%-CI for"~VaR[a], list(a = alpha))))