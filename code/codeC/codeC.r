data <- scan("out.dnc-corecipient", skip=1)
data <- matrix(data, nrow = length(data)/3, ncol = 3, byrow = TRUE)
data <- data[,-3]
data <- as.data.frame(data)
names(data)[1] <- "recip1ID"
names(data)[2] <- "recip2ID"
data <- subset(data, recip1ID < recip2ID)
se <- data$recip1ID
se <- table(se)
se_df <- as.data.frame(se)
degrees <- se_df$Freq
deTab <- table(degrees)
ddf <- as.data.frame(deTab)
degrees <- as.numeric(ddf$degrees)
index <- ddf$Freq
plot(log(degrees), log(index))
intercept <- coef(lm( log(index)~log(degrees) ))["(Intercept)"]
slope <- coef(lm( log(index)~log(degrees) ))["log(degrees)"]
abline(intercept, slope)

cat("Gamma: ", slope, "\nIntercept: ", intercept)