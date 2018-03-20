# Kaggle-Santandar Product Recommendation Competition 
The Santandar Product Recommendation Competition is hosted by Kaggle.  The goal of the challenge is to predict which of the 24 products Santandar’s existing customers will likely to add in the next month based on their past behavior and that of similar customers.  The training data consists of approximately 13.6 millions of rows of data, with a timeframe spanning across from January 2015 to May 2016.  The user data consists of 24 predictors including demographics data such as age, province of residence, and sex.  The product data consists of flags to indicate ownership for the 24 products for the respective months.  The objective is to recommend the top 7 products that the customer is likely to add to each of the nearly 1 million customers in the test set.  The scoring matrices is based on mean average precision at 7 (MAP@7 criterion).  Higher score is rewarded if actual outcome is predicted earlier in the list of recommendations. 

Link to [data](https://www.kaggle.com/c/santander-product-recommendation/data).

Link to [write-up](https://github.com/kikimeow/Kaggle-Santandar/blob/master/Santandar%20Product%20Recommendation%20Competition%20Summary.pdf).  


To run the files, first obtain the file from the Kaggle link.  The three codes to run to submit an output are as follows: 
  1. Santadar_preprocess.R
  2. Santadar_model.R
  3. Santandar_submission.R

Before running the file, please modify the path.  
