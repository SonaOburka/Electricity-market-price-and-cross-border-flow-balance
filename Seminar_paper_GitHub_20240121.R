#install.packages("gridExtra")
library("tidyverse")
library("dplyr")
library("rvest")
library("stringr")
library("glue") 
library("base")
library("ggplot2")
library("readxl")
library("gridExtra")
library("cowplot")
library("stats")
library("mvnormtest")
library("MVN")
library("corrplot")
library("robustHD")
library("xts")
library("tseries")
library("PerformanceAnalytics")
library("scales")
library("forecast")
library("epitools")
library("MASS")
library("lmtest")
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

# ---------------
# 1. INTRODUCTION
# ---------------
# This code is a part of the seminar work from MENE on applying statistical methods 
# studied in the MENE practical seminar on the Czech electricity market data. 
# The dataset was downloaded from OTE and contains:
#   daily data by hours for January 20023 for:
#   1. electricity market prices results (Price_EUR)
#   2. cross-border electricity flow balances between:
#         Czechia and Austria (CZAT)
#         Czechia and Germany (CZDE)
#         Czechia and Poland (CZPL)
#         Czechia and Slovakia (CZSK)

# -------------------------------------
# 2. DATA AND DATA FILE CHARACTERISTICS
# -------------------------------------
# Specifies the dataset

# DEFINITION OF THE VARIABLE "THEME" THAT IS GOING TO BE USED 
# FOR SELECTED GGPLOT2 GRAPH OUTPUTS
theme <- theme(
  plot.title = element_text(size = 17),
  axis.text.x = element_text(size = 17),
  axis.text.y = element_text(size = 17),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20)
)
# ----------------------------------------------
# 3 VISUALIZATION OF DATA AND HYPOTHESES TESTING
# ----------------------------------------------
# 3.1 Descriptive analysis
# ----------------------------------------------
setwd("C://Users/GitHub")
data_pcbf <- read_excel("Seminar_paper_GitHub_20240121.xls")
data_pcbf <- as.data.frame(data_pcbf) 
# GLANCE AT DATA
head(data_pcbf,10)
str(data_pcbf)

# DATAFRAME SUMMARY, CHECKING PRESENCE OF "NA" VALUEs
# Look at descriptive statistics of the raw data as well as number of NA values and empty strings-----
summary(data_pcbf[3:7])
colSums(is.na(data_pcbf))
colSums(data_pcbf[3:7] == " ", na.rm = TRUE)

# VISUALIZATION USING LINE GRAPHS
data_pcbf$combined_datetime <- make_datetime(year(data_pcbf$Day), month(data_pcbf$Day), day(data_pcbf$Day), data_pcbf$Hour - 1)
# Electricity result price (1/2023)
ggplot(data = data_pcbf, aes(x = combined_datetime, y = Price_EUR, color = "Line Color")) +
  geom_line(linewidth = 0.6, show.legend = FALSE) +
  scale_color_manual(values = "blue") +
  labs(x = "Date", y = "EUR") +
  scale_y_continuous(name = "EUR", limits = c(-30000, 1400000), breaks = seq(-30000, 1400000, by = 150000), labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )
# Cross-border electricity flows balance (1/2023)
plt_CZAT <- ggplot(data = data_pcbf, aes(x = combined_datetime, y = CZAT, color = "Line Color")) +
  geom_line(linewidth = 0.4, show.legend = FALSE) +
  scale_color_manual(values = "blue") +
  labs(title = "CZDE (MWh, 1/2023)", x = "Date", y = "MWh") +
  theme(plot.title = element_text(size = 18)) + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
plt_CZDE <- ggplot(data = data_pcbf, aes(x = combined_datetime, y = CZDE, color = "Line Color")) +
  geom_line(linewidth = 0.4, show.legend = FALSE) +
  scale_color_manual(values = "blue") +
  labs(title = "CZDE (MWh, 1/2023)", x = "Date", y = "MWh") +
  theme(plot.title = element_text(size = 18)) + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
plt_CZPL <- ggplot(data = data_pcbf, aes(x = combined_datetime, y = CZPL, color = "Line Color")) +
  geom_line(linewidth = 0.4, show.legend = FALSE) +
  scale_color_manual(values = "blue") +
  labs(title = "CZPL (MWh, 1/2023)", x = "Date", y = "MWh") +
  theme(plot.title = element_text(size = 18)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
plt_CZSK <- ggplot(data = data_pcbf, aes(x = combined_datetime, y = CZSK, color = "Line Color")) +
  geom_line(linewidth = 0.4, show.legend = FALSE) +
  scale_color_manual(values = "blue") +
  labs(title = "CZSK (MWh, 1/2023)", x = "Date", y = "MWh") +
  theme(plot.title = element_text(size = 18)) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
combined_plot <- plot_grid(plt_CZAT, plt_CZDE, plt_CZPL, plt_CZSK, ncol = 2)
combined_plot

sd(data_pcbf$Price_EUR)
sd(data_pcbf$CZAT)
sd(data_pcbf$CZDE)
sd(data_pcbf$CZPL)
sd(data_pcbf$CZSK)

# ----------------------------------------------
# 3.1 Exploratory analysis
# ----------------------------------------------
# IDENTIFICATION AND REMOVAL OF OUTLIERS
# Boxplots by variables to see, if there are any outliers
CZAT <- data.frame(
  Columns = "CZAT",
  Values = data_pcbf$CZAT
)
CZDE <- data.frame(
  Columns = "CZDE",
  Values = data_pcbf$CZDE
)
CZPL <- data.frame(
  Columns = "CZPL",
  Values = data_pcbf$CZPL
)
CZSK <- data.frame(
  Columns = "CZSK",
  Values = data_pcbf$CZSK
)
gathered_data <- rbind(CZAT, CZDE, CZPL, CZSK)

plot_cbflow <- ggplot(gathered_data, aes(x = Columns, y = Values, fill = Columns)) +
  geom_boxplot() +
  labs(y = "MWh", x = "Cross-border area", title = "Cross-border electricity flow balances (in MWh)") +
  theme_minimal() +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.position = "none") +
  scale_y_continuous(name = "MWh", limits = c(-2500, 3000), breaks = seq(-2500, 3000, by = 500), labels = comma) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )

plot_price <- ggplot(data_pcbf, aes(y = Price_EUR)) +
  geom_boxplot(fill = "skyblue") +
  labs(y = "Price per MWh (EUR)", x = "Price", title = "Price") +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_y_continuous(limits = c(-30000, 1400000), breaks = seq(-30000, 1400000, by = 150000), labels = comma) +
  scale_x_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )
combined_boxplot <- plot_grid(plot_price, plot_cbflow, ncol = 2, rel_widths = c(1, 2))
combined_boxplot

# outliers removal by applying Mahalanobis distance including removal of outliers from the dataset
del_cumsum = 0
outliars = data.frame()
data_pcbf_original <- data_pcbf[,c("Price_EUR","CZAT", "CZDE", "CZPL", "CZSK")] 
data_cleaning <- data_pcbf[,c("Price_EUR","CZAT", "CZDE", "CZPL", "CZSK")] 
for (i in 1:10) {
  mahal <- mahalanobis(data_cleaning, colMeans(data_cleaning), cov(data_cleaning), inverted = FALSE)
  mahal_del <- length(mahal[mahal > 13])
  del_cumsum <- mahal_del + del_cumsum
  print(paste("Iteration:", i, "; Outlying records removed:",mahal_del, "; Cummulative sum of removed records:", del_cumsum))
  if (length(mahal[mahal > 13]) > 0){
    data_pcbf <- data_pcbf[-c(which(mahal > 13)),] # need to 
    outliars <- rbind(outliars,data_pcbf[c(which(mahal > 13)),])
    data_cleaning <- data_cleaning[-c(which(mahal > 13)),]  
  } else break
}

data_pcbf_clean <- data_pcbf

# BOXPLOTS OF VARIABLES BEFORE AND AFTER OUTLIERS REMOVAL
# Data preparation
Price <- list(Original = data_pcbf_original$Price_EUR,
             Cleaned = data_pcbf_clean$Price_EUR)
CZAT <- list(Original = data_pcbf_original$CZAT,
             Cleaned = data_pcbf_clean$CZAT)
CZDE <- list(Original = data_pcbf_original$CZDE,
             Cleaned = data_pcbf_clean$CZDE)
CZPL <- list(Original = data_pcbf_original$CZPL,
             Cleaned = data_pcbf_clean$CZPL)
CZSK <- list(Original = data_pcbf_original$CZSK,
             Cleaned = data_pcbf_clean$CZSK)

# Electricity result price per MWh: original vs. cleaned data
par(mfrow = c(1,1))
boxplot(Price, ylab = "EUR")
dev.off
# Boxplots of variables before and after outliers removal
par(mfrow = c(2,2))
boxplot(CZAT, ylab = "MWh", main="CZAT: original vs. cleaned data")
par(cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.4)

boxplot(CZDE, ylab = "MWh", main="CZDE: original vs. cleaned data")
par(cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.4)

boxplot(CZPL, ylab = "MWh", main="CZPL: original vs. cleaned data")
par(cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.4)

boxplot(CZSK, ylab = "MWh", main="CZAT: original vs. cleaned data")
par(cex.lab = 1.6, cex.main = 1.6, cex.axis = 1.4)
dev.off

# SUMMARY BY ALL VARIABLES BEFORE AND AFTER OUTLIERS WERE REMOVED
summary(data_pcbf_original)
summary(data_pcbf_clean[3:7])

# STANDARD DEVIATIONS BEFORE AND AFTER OUTLIERS REMOVAL
sd(data_pcbf$Price_EUR)
sd(data_pcbf$CZAT)
sd(data_pcbf$CZDE)
sd(data_pcbf$CZPL)
sd(data_pcbf$CZSK)

sd(data_pcbf_clean$Price_EUR)
sd(data_pcbf_clean$CZAT)
sd(data_pcbf_clean$CZDE)
sd(data_pcbf_clean$CZPL)
sd(data_pcbf_clean$CZSK)

sd(data_pcbf_original$Price_EUR) - sd(data_pcbf_clean$Price_EUR)
sd(data_pcbf_original$CZAT) - sd(data_pcbf_clean$CZAT)
sd(data_pcbf_original$CZDE) - sd(data_pcbf_clean$CZDE)
sd(data_pcbf_original$CZPL) - sd(data_pcbf_clean$CZPL)
sd(data_pcbf_original$CZSK) - sd(data_pcbf_clean$CZSK)

colSums(is.na(data_pcbf_clean))
colSums(data_pcbf_clean[3:7] == "", na.rm = TRUE)

# HYPOTHESES TESTING ON NORMAL DISTRIBUTION OF VARIABLES
# Price_EUR variable
# ------------------
par(mfrow = c(1,2))
# Q-Q graph for normality 
qqnorm(data_pcbf_clean$Price, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: Price_EUR", xlab = "Theoretical Quantiles", ylab = "Price Quantiles")
qqline(data_pcbf_clean$Price, col = "red", lwd = 3)
# Histogram
hist(data_pcbf_clean$Price,probability=TRUE, breaks = 20, xlab = "Price per MWh (EUR)", main = "Histogram: Price")
lines(density(data_pcbf_clean$Price),col="red",lwd = 3)
dev.off
# Kolmogorov-Smirnov test on normal distribution
# H0: the variable Price_EUR has normal distribution 
ks.test(data_pcbf_clean$Price, "pnorm", mean=mean(data_pcbf_clean$Price), sd=sd(data_pcbf_clean$Price))

# CZAT variable
# ------------------
par(mfrow = c(1,2))
# Q-Q graph for normality 
qqnorm(data_pcbf_clean$CZAT, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: CZAT", xlab = "Theoretical Quantiles", ylab = "CZAT Quantiles")
qqline(data_pcbf_clean$CZAT, col = "red", lwd = 3)
# Histogram
hist(data_pcbf_clean$CZAT,probability=TRUE, breaks = 20, xlab = "MWh", main = "Histogram: CZAT")
lines(density(data_pcbf_clean$CZAT),col="red",lwd = 3)
dev.off
# Kolmogorov-Smirnov test on normal distribution
ks.test(data_pcbf_clean$CZAT, "pnorm", mean=mean(data_pcbf_clean$CZAT), sd=sd(data_pcbf_clean$CZAT))

# CZDE variable
# ------------------
par(mfrow = c(1,2))
# Q-Q graph for normality 
qqnorm(data_pcbf_clean$CZDE, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: CZDE", xlab = "Theoretical Quantiles", ylab = "CZDE Quantiles")
qqline(data_pcbf_clean$CZDE, col = "red", lwd = 3)
# Histogram
hist(data_pcbf_clean$CZDE,probability=TRUE, breaks = 20, xlab = "MWh", main = "Histogram: CZDE")
lines(density(data_pcbf_clean$CZDE),col="red",lwd = 3)
dev.off
# Kolmogorov-Smirnov test on normal distribution
ks.test(data_pcbf_clean$CZDE, "pnorm", mean=mean(data_pcbf_clean$CZDE), sd=sd(data_pcbf_clean$CZDE))

# CZPL variable
# ------------------
par(mfrow = c(1,2))
# Q-Q graph for normality 
qqnorm(data_pcbf_clean$CZPL, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: CZPL", xlab = "Theoretical Quantiles", ylab = "CZPL Quantiles")
qqline(data_pcbf_clean$CZPL, col = "red", lwd = 3)
# Histogram
hist(data_pcbf_clean$CZPL,probability=TRUE,  breaks = 15, xlab = "MWh", main = "Histogram: CZPL")
lines(density(data_pcbf_clean$CZPL),col="red",lwd = 3)
dev.off
# Kolmogorov-Smirnov test on normal distribution
ks.test(data_pcbf_clean$CZPL, "pnorm", mean=mean(data_pcbf_clean$CZPL), sd=sd(data_pcbf_clean$CZPL))

# CZSK variable
# ------------------
par(mfrow = c(1,2))
# Q-Q graph for normality 
qqnorm(data_pcbf_clean$CZSK, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: CZSK", xlab = "Theoretical Quantiles", ylab = "CZSK Quantiles")
qqline(data_pcbf_clean$CZSK, col = "red", lwd = 3)
# Histogram
hist(data_pcbf_clean$CZSK,probability=TRUE, breaks = 25, xlab = "MWh", main = "Histogram: CZSK")
lines(density(data_pcbf_clean$CZSK),col="red",lwd = 3)
dev.off
# Kolmogorov-Smirnov test on normal distribution
ks.test(data_pcbf_clean$CZSK, "pnorm", mean=mean(data_pcbf_clean$CZSK), sd=sd(data_pcbf_clean$CZSK))

# ANALYSIS OF RELATIONSHIPS BETWEEN VARIABLES
data_pcbf_rel <- data_pcbf_clean[,3:7]
chart.Correlation(data_pcbf_rel, histogram=FALSE, pch=19)

# GRAPHS OF RELATIONSHIPS BETWEEN PRICE AS DEPENDENT VARIABLE AND CZAT, CZDE, CZPL AND CZSK AS REGRESSORS
theme(
  plot.title = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

graph1 <- ggplot(mapping = aes(x = data_pcbf_clean$CZAT, y = data_pcbf_clean$Price)) + 
  geom_point() +
  labs(x = "CZAT (MWh)", y = "Price per MWh (EUR)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add trend line (linear model)
  geom_smooth(color = "blue") + # Add trend line 
  theme 
graph2 <- ggplot(mapping = aes(x = data_pcbf_clean$CZDE, y = data_pcbf_clean$Price)) + 
  geom_point() +
  labs(x = "CZDE (MWh)", y = "Price per MWh (EUR)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add trend line (linear model)
  geom_smooth(color = "blue") + # Add trend line 
  theme 
graph3 <- ggplot(mapping = aes(x = data_pcbf_clean$CZPL, y = data_pcbf_clean$Price)) + 
  geom_point() +
  labs(x = "CZPL (MWh)", y = "Price per MWh (EUR)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add trend line (linear model)
  geom_smooth(color = "blue") + # Add trend line 
  theme 
graph4 <- ggplot(mapping = aes(x = data_pcbf_clean$CZSK, y = data_pcbf_clean$Price)) + 
  geom_point() +
  labs(x = "CZSK (MWh)", y = "Price per MWh (EUR)") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add trend line (linear model)
  geom_smooth(color = "blue") + # Add trend line 
  theme 
grid.arrange(graph1, graph2, graph3, graph4, ncol = 2)

#-----------------------
# 4. REGRESSION ANALYSIS
#-----------------------
# MODEL 1 
# -------
data_pcbf_clean_lm_1 <- lm(Price_EUR ~ CZAT + CZDE + CZPL + CZSK, data_pcbf_clean)
summary(data_pcbf_clean_lm_1)

# MODEL 2 - RESIDUAL ANALYSIS
# ---------------------------
model_2 <- lm(Price_EUR ~ CZAT + CZDE + CZSK, data_pcbf_clean)
summary(model_2)
summary(model_2$residuals) 

options(scipen = 999)

# NORMALDISTRIBUTION OF RESIDUALS
par(mfrow = c(1,2))
# Q-Q graf pro normalitu 
qqnorm(model_2$residuals, pch = 1, frame = TRUE, main = "Normal Q-Q Plot: Residuals", xlab = "Theoretical Quantiles", ylab = "Residual Quantiles")
qqline(model_2$residuals, col = "red", lwd = 3)
# Histogram
hist(model_2$residuals, probability=TRUE, breaks = 40, xlab = "Residuals", ylab = "Frequency", main = "Histogram of residuals")  
lines(density(model_2$residuals),col="red", lwd = 3)
dev.off
shapiro.test(model_2$residuals)  

# HETEROOSCEDASTICITY
par(mfrow = c(1,1))
# Scatter plot
plot(model_2$residuals, ylab = "Residuals", ylim = c(-400000, 400000))  # graf rezidui
abline(h = 5, col = "red", lwd = 2)
# Breusch-Pagan test
bptest(model_2)

# AUTOCORRELATION
dwtest(model_2)

#----------------
# TIME SERIES
#----------------
# READING DATA FROM EXCEL, SELECTING RELATIVE COLUMNS AND RENAMING THEM, PRINTING VARIABLES STRUCTURE
setwd("C://Users/GitHub")
data_price <- read_excel("Seminar_paper_GitHub_20240121.xls")
data_price <- as.data.frame(data_price[1:3]) 

# RENAMING COLUMNS
names(data_price)[names(data_price) == "Day"] <- "date"
names(data_price)[names(data_price) == "Hour"] <- "hour"
names(data_price)[names(data_price) == "Price_EUR"] <- "price"
data_price
str(data_price)

# CREATING TRAIN AND TEST DATA FRAMES
train_df <- data.frame(date = as.POSIXct(paste(data_price$date[1:720], data_price$hour[1:720] - 1, sep = " "), format = "%Y-%m-%d %H"),
                       price = data_price$price[1:720],
                       type = "Train"
)
test_df <- data.frame(date = as.POSIXct(paste(data_price$date[721:744], data_price$hour[721:744] - 1, sep = " "), format = "%Y-%m-%d %H"),
                      price = data_price$price[721:744],
                      type = "Test"
)

# CREATING DATETIME VECTOR FOR TIME SERIES
# Set the start and end datetime values
start_datetime <- as.POSIXct("2023-01-01 00:00:00", tz = "CET")
end_datetime <- as.POSIXct("2023-01-31 23:00:00", tz = "CET")  # Adjust the end datetime as needed
# Create a sequence of datetime values at hourly intervals
hourly_sequence <- seq(from = start_datetime, to = end_datetime, by = "hour")
#as.POSIXct(1672527630, origin = "1970-01-01")

# CREATING TRAIN TIME SERIE
train_data_ts <- ts(data_price$price[1:720], start = hourly_sequence[1], frequency = 24)
str(train_data_ts)

# LINEAR GRAPH AND BOXPLOTS OF "PRICE" TIME-SERIE
lineplot_price <- ggplot(data = train_df, aes(x = date, y = price)) +
  geom_line(size = 0.6) +
  labs(title = "Price per MWh(1/2023)", x = "Date", y = "Price per MWh (EUR)") +
  scale_y_continuous(limits = c(-30000, 1400000), breaks = seq(-30000, 1400000, by = 150000), labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

boxplot_price <- ggplot(train_df, aes(y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Price per MWh (1/2023)", x = "") +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_y_continuous(name = "Price per MWh (EUR)", limits = c(-30000, 1400000), breaks = seq(-30000, 1400000, by = 150000), labels = comma) +
  scale_x_continuous(breaks = NULL) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )
combo_plot <- plot_grid(lineplot_price, boxplot_price, ncol = 2,  rel_widths = c(2, 1))
combo_plot

# DECOMPOSITION (ADDITIVE AND MULTIPLICATIVE METHODS) INCLUDING RESPECTIVE PLOTS
options(scipen = 999)
# Decomposition of additive time series
dec_a <- decompose(train_data_ts) # SEZONNI SLOZKA JE VIDET, ROSTOUCI TREND A NAHODNA SLOZKA
plot(dec_a, xaxt = "n", xlab = "January 2023")
axis(1, at = c(1, 3, 5), labels = c("2023-01-01", "2023-01-15", "2023-01-31"))  

# TIME SERIES STATIONARY TEST: AUGMENTED DICKEY-FULLER TEST
# No differenciation
adf.test(train_data_ts, k=0)

# USE MODEL DETAILS FROM AUTO.ARIMA() FUNCTION TO FIT ARIMA MODEL ON TIME SERIES DATA
model_aa_train_output1 <- auto.arima(train_data_ts)
model_aa_train_output1

# MODEL 1 -> DIAGNOSTICS
checkresiduals(model_aa_train_output1, plot =  TRUE, xlab = "January 2023")
# Jarque-Bera test of normal distribution
jarque.bera.test(model_aa_train_output1$residuals)
# Ljung-Box test for autocorrelation
Box.test(model_aa_train_output1$residuals, type = "Ljung")

# FORECAST of the 31.1.2023
forecast <- forecast(model_aa_train_output1, 24)
accuracy(forecast)

# PREPARATION OF DATASETS FOR RESULTS VISUAZLIZATION IN GRAPHS
forecast_df <- data.frame(forecast)
colnames(forecast_df)
forecast_df$date <- test_df$date
type <- "Forecast"
colnames(forecast_df)[1] <- "price"

# Electricity prices results and their forecast (EUR, 1/2023)
ggplot() +
  geom_line(data = train_df, aes(x = date, y = price, color = "Train"), size = 0.6) +
  geom_line(data = test_df, aes(x = date, y = price, color = "Test"), size = 0.6) +
  geom_line(data = forecast_df, aes(x = date, y = price, color = "Forecast"), size = 1) +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = Lo.80, ymax = Hi.80), alpha = 0.3, fill = "dimgray") +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = Lo.95, ymax = Hi.95), alpha = 0.3, fill = "grey") +
  scale_color_manual(values = c("Train" = "black", "Test" = "blue", "Forecast" = "red")) +
  scale_y_continuous(name = "Price per MWh (EUR)", limits = c(-30000, 1350000), breaks = seq(-30000, 1350000, by = 150000), labels = scales::comma) +
  labs(x = "Date", color = "Legend") +
  theme_minimal() +
  theme(legend.position = c(0.90, 0.85),
        legend.key.size = unit(1, "lines")) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17)
  )
# Detailed graph of electricity prices results forecast (EUR, 31/1/2023)
ggplot() +
  geom_line(data = test_df, aes(x = date, y = price, color = "Test"), size = 1.1) +
  geom_line(data = forecast_df, aes(x = date, y = price, color = "Forecast"), size = 1.2) +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = Lo.80, ymax = Hi.80), alpha = 0.3, fill = "dimgray") +
  geom_ribbon(data = forecast_df, aes(x = date, ymin = Lo.95, ymax = Hi.95), alpha = 0.3, fill = "grey") +
  scale_color_manual(values = c("Test" = "blue", "Forecast" = "red")) +
  scale_y_continuous(name = "Price per MWh (EUR)", limits = c(-120000, 900000), breaks = seq(-120000, 900000, by = 150000), labels = scales::comma) +
  labs(x = "Date", color = "Legend") +
  theme_minimal() +
  theme(legend.position = c(0.90, 0.85),
        legend.key.size = unit(1, "lines")) +
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17)
  )
