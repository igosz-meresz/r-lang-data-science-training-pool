setwd("C:\\Users\\igorr\\Documents\\LEARNING\\R - The Complete R Pogramming For Data Science\\my_scripts")
getwd()

# --------------------- WHAT IS LOGISTIC REGRESSION? ---------------------------

# Practical definition:
# Logistic regression is a machine learning algorithm that can distinguish
# between two classes at a time. Though it contains the term 'regression'
# it is not exactly an algorithm that predicts quantities.

# It is very much relevant today. In use across business domains, such as,
# banking, e-commerce, retail, supply chain, etc.


# --------------------- WHEN TU USE IT? ----------------------------------------

# Whenever you want to classify a Y that can take only two possible values:

# For example;
# Medical: Given cancerous tissue is malignant or not
# Advertising: Whether a given person will click on an ad or not

# Important thing is, it uses supporting labeled data with classified cases.
# It produces a probability score rather than just binary class.
# So, it can be used to classify more than 2 classes, using One-vs-Rest Strategy.


# --------------------- ONE-VS-REST STRATEGY -----------------------------------
# https://www.kdnuggets.com/2020/08/one-vs-rest-one-multi-class-classification.html


# --------------------- POPULAR USE CASES --------------------------------------
# DOMAIN:             | PREDICT:                       |  INFLUENCED BY:
# Marketing & Sales   | Buyer Propensity: Whether a    |  Subscriber
#                     | given person will purchase     |  Previous Purchase
#                     | a product.                     |  Last known visit
# 
# Marketing & Sales   | Attrition Modeling: Whether a  |  Demographic Info (Age, Sex, Location)
#                     | given customer will stop using |  Product usage stats
#                     | the brand/product.             |  Customer service history
# 
# Banking and Fin.    | Fraud Detection: Whether a     |  Location, Frequency
#                     | given credit card transaction  |  Historic pattern
#                     | is fraudulent or not.          |  Device and Mode
# 
# Supply Chain        | Late Shipments: Whether a given|  Transportation mode
#                     | shipment will arrive late or   |  Carrier, Supplier past
#                     | not (esp. long lead times)     |  performance, weather
# 
# Banking and Fin.    | Credit risk modeling: Whether  |  History of late payments
#                     | a given network connection     |  Credit history, demographics
#                     | is legit.                      |  Collateral
# 
# Information Security| Malicious Connection: Whether  |  User Agent
#                     | a given network connection is  |  Failed logins
#                     | legit.                         |  Network parameters
# 
# Marketing           | Ad Click Through: Whether an   |  Browsing history
#                     | ad will be clicked by a        |  past searches
#                     | customer.                      |  device information, location


# --------------------- ADVANTAGES ---------------------------------------------

# A key advantage of logistic regression is it provides a nicely calibrated
# prediction score that represents the probability of occurrence of the event.
# Good baseline model for classification.
# Statistically significant and explainable.

# --------------------- THE MATH BEHIND IT -------------------------------------
# https://www.analyticsvidhya.com/blog/2021/08/conceptual-understanding-of-logistic-regression-for-data-science-beginners/

