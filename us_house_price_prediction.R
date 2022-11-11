#### Load Data and Merge Datasets ####

# Case-Shiller U.S. National Home Price Index
df_hpi <- read.csv("sample_data/CSUSHPISA.csv", stringsAsFactors = FALSE, header = TRUE)
df_hpi$DATE = as.Date(df_hpi$DATE, "%Y-%m-%d")
df_hpi <- df_hpi[order(df_hpi$DATE, decreasing = TRUE), ]  
df_hpi$period <- format(df_hpi$DATE, "%Y-%m")

# 30-Year Fixed Rate Mortgage Average in the United States
df_mortgage <- read.csv("sample_data/MORTGAGE30US.csv", stringsAsFactors = FALSE, header = TRUE)
df_mortgage$DATE = as.Date(df_mortgage$DATE, "%Y-%m-%d")
df_mortgage$period <- format(df_mortgage$DATE, "%Y-%m")
df_mortgage <- df_mortgage[order(df_mortgage$DATE, decreasing = TRUE), ]  

df_mortgage <- df_mortgage %>% 
  group_by(period) %>%
  mutate(row_number = row_number())

df_mortgage <- filter(df_mortgage, row_number == 1)

# Consumer Price Index for All Urban Consumers: All Items in U.S. City Average 
df_cpi <- read.csv("sample_data/CPIAUCSL.csv", stringsAsFactors = FALSE, header = TRUE)
df_cpi$DATE = as.Date(df_cpi$DATE, "%Y-%m-%d")
df_cpi <- df_cpi[order(df_cpi$DATE, decreasing = TRUE), ]  
df_cpi$period <- format(df_cpi$DATE, "%Y-%m")
df_cpi$CPIAUCSL_YOY = df_cpi$CPIAUCSL/lead(df_cpi$CPIAUCSL, n=12) - 1

# S&P 500
df_spy <- read.csv("sample_data/S&P 500 Historical Data.csv", stringsAsFactors = FALSE, header = TRUE)
df_spy$DATE = paste(df_spy$Date,"01",sep=" ")
df_spy$DATE = as.Date(df_spy$DATE, "%b %y %d")
df_spy <- df_spy[order(df_spy$DATE, decreasing = TRUE), ]  
df_spy$period <- format(df_spy$DATE, "%Y-%m")
names(df_spy)[names(df_spy) == "Price"] <- "SPY"
df_spy$SPY = as.double(gsub(",", "", df_spy$SPY))

# Unemployment Rate
df_ur <- read.csv("sample_data/UNRATE.csv", stringsAsFactors = FALSE, header = TRUE)
df_ur$DATE = as.Date(df_ur$DATE, "%Y-%m-%d")
df_ur <- df_ur[order(df_ur$DATE, decreasing = TRUE), ]  
df_ur$period <- format(df_ur$DATE, "%Y-%m")

# Merge datasets (inner join based on HPI)
df <- df_hpi[c("period","CSUSHPISA")]
df_mortgage <- df_mortgage[c("period","MORTGAGE30US")]
df_cpi <- df_cpi[c("period","CPIAUCSL_YOY")]
df_spy <- df_spy[c("period","SPY")]
df_ur <- df_ur[c("period","UNRATE")]

df <- merge(x = df_hpi, y = df_mortgage, by = "period", all.x = TRUE)
df <- merge(x = df, y = df_cpi, by = "period", all.x = TRUE)
df <- merge(x = df, y = df_spy, by = "period", all.x = TRUE)
df <- merge(x = df, y = df_ur, by = "period", all.x = TRUE)
df <- na.omit(df)

df <- df[c('period', 'CSUSHPISA', 'MORTGAGE30US', 'CPIAUCSL_YOY', 'SPY', 'UNRATE')]
colnames(df) <- c('period', 'hpi', 'mortgage', 'cpi', 'spy', 'unrate')

summary(df)

df_ts <- ts(df[, 2:ncol(df)], start=c(1987,01), freq=12)

# Split the total datasets into training and test(only last six months data) set
l <- nrow(df_ts)
train <- ts(df_ts[1:(l-6),], start=c(1987,01), freq=12)
test <- ts(df_ts[(l-5):l,], start=c(2021,10), freq=12)

# ACF plots of each time series to check the order of difference
par(mfrow=c(2,3))
acf(df_ts[,1], main='ACF of HPI')
acf(df_ts[,2], main='ACF of Mortgage')
acf(df_ts[,3], main='ACF of CPI')
acf(df_ts[,4], main='ACF of S&P500')
acf(df_ts[,5], main='ACF of Unemployment Rate')

# ACF plots of each 1st-order differenced time series
par(mfrow=c(2,3))
acf(diff(df_ts[,1]), main='ACF of 1st-order Differenced HPI')
acf(diff(df_ts[,2]), main='ACF of 1st-order DifferencedMortgage')
acf(diff(df_ts[,3]), main='ACF of 1st-order DifferencedCPI')
acf(diff(df_ts[,4]), main='ACF of 1st-order DifferencedS&P500')
acf(diff(df_ts[,5]), main='ACF of 1st-order DifferencedUnemployment Rate')

# 1st order difference training data  
diff.train <- diff(train)

# select VAR model order using Akaike Information Criterion
var.train.select <- VARselect(diff.train, lag.max=24)
var.train.select$selection

var.train.mod <- VAR(diff.train, p=16)

# Extract coefficients and variance covariance of each time series and its lagged time series
coef.hpi <- coefficients(var.train.mod)$hpi[-(16*5+1),1]
var.hpi <- vcov(var.train.mod)[2:81, 2:81]

coef.mortgage <- coefficients(var.train.mod)$mortgage[-(16*5+1),1]
var.mortgage <- vcov(var.train.mod)[83:162, 83:162]

coef.cpi <- coefficients(var.train.mod)$cpi[-(16*5+1),1]
var.cpi <- vcov(var.train.mod)[164:243, 164:243]

coef.spy <- coefficients(var.train.mod)$spy[-(16*5+1),1]
var.spy <- vcov(var.train.mod)[245:324, 245:324]

coef.unrate <- coefficients(var.train.mod)$unrate[-(16*5+1),1]
var.unrate <- vcov(var.train.mod)[326:405, 326:405]

hpi.true <- ts(df_ts[1:nrow(df_ts),1], start=c(1987,01), freq=12)
l <- length(hpi.true)

ymin <- min(hpi.true[(l-40):l])
ymax <- max(hpi.true[(l-40):l])

plot(hpi.true[(l-40):l], type='l', ylim=c(ymin, ymax), ylab='Home Price Index', xlab='Time', main='HPI Predictions for Test Data')

points(ts(final.train.pred, start=36), lwd=2, col='red')

legend('topleft', legend=c('Actual HPI', 'Predicted HPI (Last Six Months)'),
       lwd=2, col=c('black', 'red'))

# 1st order difference full data 
diff.ts <- diff(df_ts)
var.mod <- VAR(diff.ts, p=16) # Same VAR order we figured out with training data
var.fcst <- predict(var.mod, n.ahead=6)
var.pred <- var.fcst$fcst$hpi[,1]

last <- tail(df_ts, 1)[1]

final.pred <- rep(0,6)
for (i in 1:6) {
  final.pred[i] <- last + var.pred[i]
  if (i >= 2){
    final.pred[i] <- final.pred[i-1] + var.pred[i]
  }
}

cat("Forecase for the next six months' HPI: ",
    final.pred)

hpi.true <- ts(df_ts[1:nrow(df_ts),1], start=c(1987,01), freq=12)
hpi.plus.pred <- c(hpi.true, final.pred)
l <- length(hpi.plus.pred)

ymin <- min(hpi.plus.pred)
ymax <- max(hpi.plus.pred)

plot(hpi.plus.pred[(l-40):l], type='l', ylim=c(ymin, ymax), ylab='Home Price Index', xlab='Time', main='HPI Predictions for Next Six Months')

points(ts(final.pred, start=36), type='l', lwd=4, col='red')

legend('topleft', legend=c('Actual HPI', 'Predicted HPI (Upcoming Six Months)'),
       lwd=2, col=c('black', 'red'))

