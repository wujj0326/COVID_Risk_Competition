# COVID_Risk_Score
The competition of getting risk scores for each city in LA county

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

```
File Name: COVID_LSTM_Prediction.ipynb, Get_risk_score.ipynb
```
### Data

The Raw Data are all stored in their corresponding URL.

Moreover, the LSTM model would generate the predicted values that would be used to get the final risk score

### Prerequisites

```
Python 3
```

### Installing Packages for Python

```
Pandas
Numpy
Matplotlib.pyplot
Sklearn  
Tensorflow
Math
```

## Running the codes

```
Run all the cell in COVID_Risk_Score.ipynb and 
Each section in the python notebook is labelled, and there are comments explaining the code.
```

### Break down into end to end tests



* Two models one for death, one for cases

* Predicted numbers as hazard

  * Hazard * vulnerability

  * Our innovation: we defined two types of risk, one for death and one for infection, then multiplied by the hyperparameter alpha to merge as one score.

* The data is from here:

  https://lahub.maps.arcgis.com/home/item.html?id=8659eeee6bf94eabb93398773aa25416&view=list#overview

```step1. classify features into groups```

* vulnerable factors related to death cases:

  * elderly

  * asthma

  * cardiovascular

* vulnearble factors related to infected cases:

  * poverty: the higher the value the poorer the area

  * traffic

  * population


```step2. get raw scores```

* those features are measured at different unit and maginitude

* normalize them before summarize

* each entry bocomes 0 to 1

* take a sum to get a raw score for death vulnerablity and case vulnerabilty

* put the two raw score column into a sigmoid function -->two sigmoid_raw_score, one for death and one for cases

```step3. calcualte the risk score```

* apply a hyperparameter (alpha) to both hazard. The formula is: alpha * infected hazard + (1-alpha) * death hazard

* multiply each scores to the city

* calculate the mean of risk score to get a weekly risk score

```step4. generate the risk level```

* generate the risk level by calculating the quatile of the weekly risk score


## Authors

* **Eric Wu** 
* **Rongxing Chen**
* **Kayla Tanli**
* **Zuo Zuo**

* **Mentor: Richard Zhen Tang**

## License

This project is licensed under the RMDS


