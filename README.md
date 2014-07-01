Text miming for publications
========

This projects include several text-mining R functions and a script for neuroscience papers. 

The data are a xlsx file. 

Before running the scipt, it is required that the xlsx file has to be opened in Excel or similar program, keep only the needed columns (ID, Abstract, s1.included, s1.excluded, s2.included, s2.excluded, and convert/save the file as a txt file ( which should automatically change the file to a tab seprated file).

In order to run the application, set up the parameter settings in pain.r and then source it. Current changable parameters are machine learning model (NB, kNN, SVM), related parameters for the model, features sparse level and ratio of data set to use.