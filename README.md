cpg-statrisk-2014
=================

R scripts for generating statistical forecasts of onsets of state-led mass killing episodes described in this post on my blog, Dart-Throwing Chimp:

http://wp.me/p1domH-1kD

The script that does the estimating and forecasting is "validation.and.prediction.R". All of the other scripts are used to generate the data used in that process. All of the data sets used in this analysis are in the public domain and available for free, but not all of them are posted online, and many of the ones that are online aren't posted in a format that's easy to ingest and merge. So, to facilitate replication of the modeling piece, the .csv created at the end of the first section of the validation-and-prediction script can be found on my Google Drive here:

https://drive.google.com/file/d/0B5wyt4eDq98GYm1hUUxuR09pejQ/edit?usp=sharing

A data dictionary for that file cab be found here as varlist.txt.

Note that results of any re-estimation will not exactly match results described in the blog post because Random Forests entails some randomized elements (duh!) that will produce a slightly different forest, and thus forecasts, each time the process is rerun. The results for all of the logistic regression models should match exactly, however, and the unweighted average of the estimates from the three models of mass-killing onset should be very close to the ones presented in the post.

I did this work under contract to the U.S. Holocaust Memorial Museum's Center for the Prevention of Genocide. All errors are my own.
