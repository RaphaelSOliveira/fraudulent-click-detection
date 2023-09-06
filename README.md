# Fraudulent Click Detection
## Kaggle and Data Source

The data worked on in this project are public. They were taken from the [Kaggle](https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data) website. 

## Summary:
### The business problem:

China's largest independent big data service platform, covers more than 70% of active mobile devices across the country. They process 3 billion clicks a day, which 90% are potentially fraudulent. Their current approach to preventing click fraud for app developers is to measure a user's click journey and flag IP addresses that produce a lot of clicks but never end up installing apps.

### Purpose of Work:

From historical data, create a classification model that measures whether a user will download a mobile app after clicking on an ad.

### Language Used:

This work was carried out with the R language.

## Conclusion:

The model achieved an average accuracy of 95% allowing the completion of the project. The steps of selecting variables and balancing the dataset were extremely important for the predictive model to have a good success rate. In addition, the RandomForest algorithm stood out in relation to the SVM.