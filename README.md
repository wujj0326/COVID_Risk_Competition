# COVID_Risk_Competition
The competition of getting risk scores for each city in LA county

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

```
File Name: COVID_Challenge.ipynb
```
### Data

The Data are all stored in their corresponding URL, so there is no need to download the data  

### Prerequisites

```
Python 3
```

### Installing Packages

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
Run each section in the file named COVID_Challenge.ipynb in the folder. 
Each section in the notebook is labelled, and there are comments explaining the code.
```

### Break down into end to end tests

1.       Two models one for death, one for cases

2.       Predicted numbers a hazard

Hazard * vulnerability

Our innovation: we defined two types of risk: one for death and one for infection

•       data is from here:

•       https://lahub.maps.arcgis.com/home/item.html?id=8659eeee6bf94eabb93398773aa25416&view=list#overview

step1. classify features into groups

•       vulnerable factors related to death cases

•       elderly

•       asthma

•       cardiovascular

•       vulnearble factors related to infected cases

•       poverty: the higher the value the poorer the area

•       traffic

•       population


step2. get raw scores

•       those features are measured at different unit and maginitude

•       normalize them before summarize

•       (obs - min(obs))/(max(obs) - min(obs))

•       then each entry bocomes 0 to 1

•       and take a sum to get a raw score for death vulnerablity and case vulnerabilty

•       and put the two raw score column into a sigmoid function -->two sigmoid_raw_score, one for death and one for cases

step3. calcualte the risk score

•       multiply each scores to the city


## Authors

* **Eric Wu** 
* **Rongxing Chen**
* **Kayla Tanli**
* **Zuo Zuo**

Mentor: Richard Zhen Tang

## License

This project is licensed under the RMDS


