
library("tidyverse") # to perform tasks such as subsetting and transforming data.
library("readr") # to import files into the environment.
library("dplyr") # to perform data manipulation.
library("tidyr") # to facilitate the preprocessing tasks.
library("ggplot2") # to perform data visualization functions.
library("ggthemes") # to apply a themes for visualizations.
library("GGally") # ggplot2 extension to conduct correlation visualizations.
library("pROC") # to perform the AUC-ROC test.
library("finalfit") # to plot the odds ratio of the logistic regression model.


# import the data into the environment.
financial_data <- read_csv("C:/Users/Osama/Desktop/2014_Financial_Data.csv") 


# to explore the main dataset.
View(financial_data)


# to find out the columns in the dataset. 
colnames(financial_data) 



# Data Pre-processing:



# to add a column name for stocks.
names(financial_data)[1]<- "stock"


# to compute new financial ratios.
financial_data$'net_income_to_total_assets' <- 
  financial_data$`Net Income` / financial_data$`Total assets`

financial_data$'retained_earnings_to_total_assets' <- 
  financial_data$`Retained earnings (deficit)` / financial_data$`Total assets`

financial_data$'earnings_before_income_tax_to_total_assets' <- 
  financial_data$EBIT / financial_data$`Total assets`

financial_data$'working_capital_to_total_assets' <- 
  financial_data$`Working Capital` / financial_data$`Total assets`

financial_data$'total_current_assets_to_total_assets' <- 
  financial_data$`Total current assets` / financial_data$`Total assets`

financial_data$'total_debt_to_total_assets' <- 
  financial_data$`Total debt` / financial_data$`Total assets`


# to select only required columns to use in the study.
financial_ratios <- financial_data %>%  select(
  'stock',
  'Sector',
  'Class',
  'net_income_to_total_assets',
  'retained_earnings_to_total_assets',
  'earnings_before_income_tax_to_total_assets',
  'currentRatio',
  'working_capital_to_total_assets',
  'cashFlowToDebtRatio',
  'total_current_assets_to_total_assets',
  'cashRatio',
  'total_debt_to_total_assets',
  'Receivables Turnover'
)


View(financial_ratios)


nrow(financial_ratios)  
# the number of observations is 3808 before removing rows that contain NA values.


# to remove rows with NA values.
financial_ratios_clean <- na.omit(financial_ratios) 


View(financial_ratios_clean) 


nrow(financial_ratios_clean)  
# the number of observations is 2054 after removing rows containing NA values.


# to rename columns for consistency.
names(financial_ratios_clean)[2] <- "sector"
names(financial_ratios_clean)[3] <- "profitable_next_year"
names(financial_ratios_clean)[7] <- "current_ratio"
names(financial_ratios_clean)[9] <- "cash_flow_to_debt_ratio"
names(financial_ratios_clean)[11] <- "cash_ratio"
names(financial_ratios_clean)[13] <- "receivables_turnover"


# to spot outliers and have a general view of the descriptive stats.
summary(financial_ratios_clean) 


# for the purposes of this study, ratios with the value of 0 will be removed 
# alongside a few extreme values that are suspiciously typos.
financial_ratios_clean_final <- subset(
  financial_ratios_clean,
  financial_ratios_clean$`retained_earnings_to_total_assets` >- 100 &
  financial_ratios_clean$'earnings_before_income_tax_to_total_assets' !=0 &
  financial_ratios_clean$`current_ratio` <30 &
  financial_ratios_clean$`current_ratio` !=0 &
  financial_ratios_clean$`working_capital_to_total_assets` >- 30 &
  financial_ratios_clean$`working_capital_to_total_assets` < 30 &
  financial_ratios_clean$`cash_flow_to_debt_ratio` > -350 &
  financial_ratios_clean$`cash_flow_to_debt_ratio` < 350 &
  financial_ratios_clean$`cash_ratio` < 30 &
  financial_ratios_clean$`cash_ratio` != 0 &
  financial_ratios_clean$`total_debt_to_total_assets` <3 &
  financial_ratios_clean$`total_debt_to_total_assets` !=0 &
  financial_ratios_clean$`receivables_turnover` <1000 &
  financial_ratios_clean$`receivables_turnover` != 0
)


View(financial_ratios_clean_final)


nrow(financial_ratios_clean_final)
# the number of observations is 1931 after completing the data preprocessing phase.


# to add the profitability variable as a factor to be able to visualize it.
financial_ratios_clean_final$profitable_next_year_factor <- 
  as.factor(financial_ratios_clean_final$profitable_next_year)


# to group by sector for visualization sorting.
financial_ratios_clean_final <- financial_ratios_clean_final %>%
  group_by(sector) %>%
  mutate(n = n())


# the count  of stocks per sector sub-categorized by profitability in 2015 will be
# visualized into a bar chart using the following code:
ggplot(financial_ratios_clean_final) +
aes(x = fct_reorder(sector, n), fill = profitable_next_year_factor) +
geom_bar(position = "dodge") +
scale_fill_manual(values = c(`0` = "darkred", `1` = "darkgray")) +
labs(
  x = "Sector",
  y = "Count",
  title = "Count of Sectors' Stocks By Profitability",
  caption = "stocks count per sector sub-categorized by profitability next year",
  fill = "Profitability in 2015"
) +
coord_flip() +
theme_economist_white() +
theme(
  plot.title = element_text(face = "italic"),
  axis.title.x = element_text(size = 10L, face = "bold.italic")
)


# to obtain descriptive statistics about the table used.
summary(financial_ratios_clean_final) 


# to export the final dataset as a CSV.
write.csv(
  financial_ratios_clean_final,
  "C:/Users/Osama/Desktop/Financial_ratios_clean_final.csv",
  row.names = FALSE
) 



# Correlation Analysis:



# the independent variables, which are all continuous, will
# be extracted from the data frame. 
financial_ratios_correlation <- financial_ratios_clean_final[, 4:12] 


# the following code will visualize the correlation matrix without
# duplicate values and customized aesthetics.
financial_ratios_correlation  %>% 
  ggcorr(
    label = TRUE, label_round = 2,
    hjust = 0.9, layout.exp = 3,
    low = "darkred", high = "green4"
  ) +
  labs(title = "Independent Variables' Correlation Matrix") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold"))



# Binary Logistic Regression (Initial):



# to remove the identifier column
financial_ratios_model <- financial_ratios_clean_final[, 2:13] 


# to shuffle the data and split the data with a ratio of 70/30 for train and test.
financial_ratios_logistic <- 
  financial_ratios_model[sample(1 : nrow(financial_ratios_model)), ]
split_point <- floor(nrow(financial_ratios_logistic) * 0.7)
financial_ratios_train <- financial_ratios_logistic[1 : split_point, ]
financial_ratios_test <- 
  financial_ratios_logistic[(split_point + 1) : nrow(financial_ratios_logistic), ]


# to fit the initial logistic regression model using the train data.
model_initial <- glm(
  profitable_next_year ~  
  net_income_to_total_assets +
  retained_earnings_to_total_assets +
  earnings_before_income_tax_to_total_assets +
  current_ratio +
  working_capital_to_total_assets +
  cash_flow_to_debt_ratio +
  total_current_assets_to_total_assets +
  cash_ratio +
  total_debt_to_total_assets +
  receivables_turnover,
  data = financial_ratios_train,
  family = binomial()
)


# to make predictions on the test data using the fitted model.
predictions_initial <- 
  predict(model_initial, newdata = financial_ratios_test, type = "response")


# to convert the predictions to a binary outcome (0 or 1).
predictions_binary_initial <- ifelse(predictions_initial > 0.5, 1, 0)


# to calculate the accuracy of the initial model.
accuracy_initial <- 
  mean(predictions_binary_initial == financial_ratios_test$profitable_next_year)


# to create a confusion matrix.
confusion_matrix_initial <- 
  table(financial_ratios_test$profitable_next_year, predictions_binary_initial)


# to measure sensitivity/recall.
recall_initial <- 
  confusion_matrix_initial[2, 2] /
  (confusion_matrix_initial[2, 2] + confusion_matrix_initial[2, 1])


# to measure specificity.
specificity_initial <- 
  confusion_matrix_initial[1, 1] /
  (confusion_matrix_initial[1, 1] + confusion_matrix_initial[1, 2])


# to measure precision.
precision_initial <- 
  confusion_matrix_initial[2, 2] / 
  (confusion_matrix_initial[2, 2] + confusion_matrix_initial[1, 2])


# to measure F1-Score.
f1_score_initial <- 
  2 * (precision_initial * recall_initial) / (precision_initial + recall_initial)


# to measure AUC-ROC.
roc_obj_initial <- 
  roc(financial_ratios_test$profitable_next_year, predictions_initial)
auc_roc_initial <- 
  auc(roc_obj_initial)


# to print the metrics of the initial model.
print(paste("Initial Model Accuracy: ", accuracy_initial))
print(paste("Initial Model Sensitivity (Recall): ", recall_initial))
print(paste("Initial Model Specificity: ", specificity_initial))
print(paste("Initial Model Precision: ", precision_initial))
print(paste("Initial Model F1-Score: ", f1_score_initial))
print(paste("Initial Model AUC-ROC: ", auc_roc_initial))


# to summarize the model and make interpretations.
summary(model_initial)



## Forward Stepwise Regression:



# the following code will run the stepwise regression:
financial_ratios_stepwise <- financial_ratios_model[, 2:11]
intercept_only <- lm(profitable_next_year ~ 1, data = financial_ratios_stepwise)
all <- 
  lm(profitable_next_year ~ ., data = financial_ratios_stepwise)
forward <- 
  step(intercept_only, direction = 'forward', scope = formula(all), trace = 0)
forward$anova
forward$coefficients 
# the code shows that the stepwise regression has identified 4 out of the 10 
# independent variables as the best fit for the logistic regression model.



# Logistic Regression (Final):



# to fit the final logistic regression model using the train data.
model_final <- glm(
  profitable_next_year ~ 
  earnings_before_income_tax_to_total_assets +
  current_ratio +
  cash_flow_to_debt_ratio +
  total_current_assets_to_total_assets,
  data = financial_ratios_train,
  family = binomial()
)


# to make predictions on the test data using the fitted model.
predictions_final <- 
  predict(model_final, newdata = financial_ratios_test, type = "response")


# to convert the predictions to a binary outcome (0 or 1).
predictions_binary_final <- ifelse(predictions_final > 0.5, 1, 0)


# to calculate the accuracy of the final model.
accuracy_final <- 
  mean(predictions_binary_final == financial_ratios_test$profitable_next_year)


# to create a confusion matrix.
confusion_matrix_final <- 
  table(financial_ratios_test$profitable_next_year, predictions_binary_final)


# to measure sensitivity/Recall .
recall_final <- 
  confusion_matrix_final[2, 2] / 
  (confusion_matrix_final[2, 2] + confusion_matrix_final[2, 1])


# to measure specificity.
specificity_final <- 
  confusion_matrix_final[1, 1] / 
  (confusion_matrix_final[1, 1] + confusion_matrix_final[1, 2])


# to measure precision. 
precision_final <- 
  confusion_matrix_final[2, 2] / 
  (confusion_matrix_final[2, 2] + confusion_matrix_final[1, 2])


# to measure F1-Score.
f1_score_final <- 
  2 * (precision_final * recall_final)/(precision_final + recall_final)


# to measure AUC-ROC.
roc_obj_final <- 
  roc(financial_ratios_test$profitable_next_year, predictions_final)
auc_roc_final <- 
  auc(roc_obj_final)


# to print the metrics of the final model.
print(paste("Final Model Accuracy: ", accuracy_final))
print(paste("Final Model Sensitivity (Recall): ", recall_final))
print(paste("Final Model Specificity: ", specificity_final))
print(paste("Final Model Precision: ", precision_final))
print(paste("Final Model F1-Score: ", f1_score_final))
print(paste("Final Model AUC-ROC: ", auc_roc_final))



# to visualize odds ratios of the final model's independent variables.
financial_ratios_clean_final %>% 
  or_plot(
  'profitable_next_year',
  c(
  'earnings_before_income_tax_to_total_assets ',
  'current_ratio ',
  'cash_flow_to_debt_ratio ',
  'total_current_assets_to_total_assets'
  ),
  table_text_size = 3.5
)
