# Seminar Paper
## Unicorn University - Energy Industry Seminar
Winter semester 2023/2024   

**Attached**:  
* Semina paper: **pdf**  
* Code: **R**   
* Source data: **xls**   

In this seminar paper, we practiced various data **visualization techniques** in the R programming language, as well as **regression** and **time series analysis**.

The **dataset** utilized in this seminar work includes a time series variable reflecting daily electricity market
price results, along with four variables showcasing daily cross-border electricity flow balances ('CZAT',
'CZDE', 'CZPL', 'CZSK'). These variables, also time series in nature, provide a geographical context that is
integral to the focus of this seminar work.

**Visualization of data** was performed throughout the work using various graph types such as linear graphs,
scatter plots, boxplots, Q-Q plots, histograms, and correlograms. Hypothesis testing was conducted using
several tests, including the Kolmogorov-Smirnov test for normal distribution, Shapiro-Wilk test for normality,
Breusch-Pagan test for heteroscedasticity, Jarque-Bera test for normal distribution of residuals, Durbin-
Watson test for autocorrelation, and Dickey-Fuller test for stationarity.

The second version of the **regression model**, utilizing the dependent variable of daily electricity market price
results and independent variables of cross-border electricity flow balances 'CZAT', 'CZDE', 'CZSK', was
discussed, and residual testing was conducted. The analysis revealed that the model has its limitations, and
further refinement in model setup is warranted.

**Time series analysis** was performed on the daily electricity market price results time series variable that had
been cleaned in the previous section. An ARIMA model was obtained using the auto.arima function.
Subsequent residual testing indicated that not all assumptions were met. Nevertheless, the forecast function
was applied, and the results were visualized in graphs

