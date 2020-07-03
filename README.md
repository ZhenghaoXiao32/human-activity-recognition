# Human Activity Recognition: predicting weight-lifting activities


## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.

The goal of this project is to recognize human weight lifting activity using data from accelertometers on the belt, forearm, arm, and dumbell of 6 participants. More information about the human activity recognition is available [here](http://groupware.les.inf.puc-rio.br/har).

## Required Packages
`caret`

`gbm`

`lattice`

## Summary

In this project, I built a generalized boosted tree model, tuned the parameters with k-fold cross-validation, and reached 0.9994 accuracy on the testing data.
