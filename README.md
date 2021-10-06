# Credit-Card-Defaulters (In R)

## Introduction
Banks usually offer credit cards depending on a person's credit history and score. Because any client can default at any time, there is no defined mechanism for identifying defaulters. Although banks profit by selling defaulters' assets in extreme cases, this usually comes at a great cost in terms of litigation and takes a long period. As a result, responsible lending by banks is crucial to reducing defaults.

## Purpose
Using several Machine Learning Models in R language, I attempted to forecast credit card defaults in this research. This can be used by banks to determine the likelihood of a client failing on their payments, allowing them to lend responsibly.
 
## About the Dataset
The data includes information about credit card holders, their numerous personal characteristics, bill amounts, and whether or not the customer has defaulted. The dataset has a total of 30k records and 24 data fields.

## Dataset details
Below are the features in this dataset:
LIMIT_BAL: Limit of the credit card
SEX: Gender- Male or Female
EDUCATION: Level of Education
MARRIAGE: Marriage Status
AGE: Age of a customer
PAY_0 to PAY_6: Salary of a customer
BILL_AMT1 to BILL_AMT6: Bill amount of a customer
PAY_AMT1 to PAY_AMT6: Amount needs to be paid by the customer
default payment next month: Defaulters who didn't pay credit card bills on time.

## Models Implemented:
1) Logistic Regression
2) Classification using KNN
3) Neural Networks
4) Naïve Bayes
5) Ensemble Learning

## Conclusion
The Neural Networks technique produced the best results, followed by the KNN technique. When compared to other models, Naive Bayes gives the worst outcome; perhaps this model isn't appropriate for this scenario. According to the demographics, customers who are female, better educated, single, and between the ages of 30 and 40 are more likely to pay on time.
