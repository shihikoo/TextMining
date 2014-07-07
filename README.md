Text miming for publications
========

This projects include several text-mining R functions and a script for neuroscience papers. 

The data are xlsx files from previous work of CAMARADES group. 

Before running the scipt, it is required that the xlsx files have to be opened in Excel or similar program and convert as a txt file (which should automatically change the file to a tab seprated file).

In order to run the application, set up the parameter settings in pain.r and then source it. 

Current changable parameters are machine learning model (NB, kNN, SVM), related parameters for each model, features sparse level and ratio of data set to use.

There is also an example in the script generate all sorts of curves (learning curve, feature curve, cost curve, gamma curve, ) for analyzing model and tuning the parameters.