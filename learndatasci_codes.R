# Code to replicate python analysis from learndatasci.com
#
# Date: March 24, 2017
#
# Author: Naumaan Nayyar
# Source: http://www.learndatasci.com/predicting-housing-prices-linear-regression-using-python-pandas-statsmodels/

#### ENVIRONMENT ####
# Clear workspace
rm(list=ls())
cat("\014")
graphics.off()

# Import required libraries
library(data.table)

# LOAD FILES
data_gdp <- fread('datasets/learndatasci/gdp.csv')
data_unemployment <- fread('datasets/learndatasci/unemployment-macro.csv')
data_federal_funds <- fread('datasets/learndatasci/fed_funds.csv')
data_shiller <- fread('datasets/learndatasci/shiller.csv')
data_hpi <- fread('datasets/learndatasci/monthly-hpi.csv')

# Merge datasets
mydata <- Reduce(function(...) merge(..., by='date'), list(data_hpi,
                data_federal_funds, data_gdp, data_shiller, data_unemployment))

# Exploratory analysis
cor.data <- cor(mydata[,!"date"])
cor.data.m <- melt(cor.data)
plot.cor <- ggplot(data = cor.data.m, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(low="white", high="red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(plot.cor)

# Generate models
mymodel <- lm(housing_price_index ~ total_unemployed, data = mydata)
print(summary(mymodel))
plot.model <- ggplot(data = mydata,
                     aes(x = housing_price_index, y = total_unemployed)) +
              geom_point(color="blue") +
              geom_smooth(color="green", method="lm")
print(plot.model)
