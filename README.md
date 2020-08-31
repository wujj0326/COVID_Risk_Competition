# COVID_Risk_Score
The competition of getting risk scores for each city in LA county

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

```
File Name: COVID_Risk_Score_Ver2.ipynb
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
Run all the cell in COVID_Risk_Score_Ver2.ipynb and 
Each section in the python notebook is labelled, and there are comments explaining the code.
```

### Break down into end to end tests



* LSTM Model would predict the increase cases for every city in Los Angeles

* Predicted numbers as hazard

  * Hazard * vulnerability


* The data is from here:

  https://lahub.maps.arcgis.com/home/item.html?id=8659eeee6bf94eabb93398773aa25416&view=list#overview

```step1. classify features into groups```

* vulnerable factors related to confirmed cases:

  * elderly

  * asthma

  * cardiovascular

  * poverty: the higher the value the poorer the area

  * traffic

  * population


```step2. get local risk scores```

* those features are measured at different unit and maginitude

* normalize them before summarize

* each entry bocomes 0 to 1

* take a sum to get a raw score for vulnerabilty

* put the raw score column into a sigmoid function


```step2. get google mobility scores```

* get the google mobility data 

* use LSTM to predict the mobility score for the next 7 days

* sum the percentage change and normalize it


```step3. calcualte the risk score```

* multiply each scores to the city



## Authors

* **Eric Wu** 
* **Rongxing Chen**
* **Kayla Tanli**
* **Zuo Zuo**

* **Mentor: Richard Zhen Tang**

## License

This project is licensed under the RMDS


