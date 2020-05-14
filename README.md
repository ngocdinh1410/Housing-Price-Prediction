# Housing-Price-Prediction-Using-R
I am trying to predict housing price based on historical information on the house

<p>The housing market has so much information. For the average buyer/seller, it can be hard to quantify those housing data into “How much is this house worth?”</p>
<p>With data on historical sale of houses, we hope to build a model that can predict the Sale Price of a house based on the house’s quality and characteristics.</p>

<h2>Feature Development:</h2>
<p>
<li>Overall quality variable: changed the data type to numeric</li>
<li>Calculated the years since the original construction date and since the remodel date for each house by using the YearBuilt and YearRemodAdd variables</li>
<li>Dropped outliers below 1st and above 99th percentile for variables SalePrice, GrLivArea, Lot Area</li>
</p>

[**R Code File**](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/GROUP%20PROJECT_v4.R)


[**Data File**](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/housing_data.csv)


[**Report File**](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/R%20Final%20Report.docx)


[**Powerpoint**](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/Housing%20Project.pptx)


<p>Import the or install the following packages:</p>

```
library(doMC)
library(dplyr)
library(purrr)
library(caret)
library(corrplot)
library(knitr)
library(ggplot2)
library(tidyverse)
library(car)
library(MASS)
library(pROC)
library(broom)
library(glmnet)
library(yardstick)
```

<h2>Initial Plots:</h2>

![alt text](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/GrLivArea.png "grlivarea")


![alt text](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/saleprice.png "Sale Price")


<h2>Sample model plots:</h2>

![alt text](https://github.com/ngocdinh1410/Housing-Price-Prediction/blob/master/model1pointplot.png "model 1")


<h2>Hypothesis:</h2>
<ul>Not all variables will be important or significant in predicting sale price (needs further analysis through p-value test…)</ul>
<ul>Variables that correlate well with sale price in the corrplot could be the important variables to predicting the sale price</ul>
<ul>Some of our variables will have multicollinearity issues (quality-related variables)</ul>

<h2>Conclusion:</h2>
<li>Linear regression model without outliers is our robust model.</li>
<li>Kitchen quality, height of the basement, and quality of material on the exterior are prominent variables that contribute to the difference of sale price.</li>
<h2>Limitation:</h2>
<li>Limited to the fields of linear regression models</li>
<li>Limited by external validations</li>
