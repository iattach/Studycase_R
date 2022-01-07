# Project structure

- **code**: contains the R runbook and the dataset (that you can also download from [Kaggle](https://www.kaggle.com/agirlcoding/all-space-missions-from-1957))
- **exports**: contains a HTML version of the runbook and a png version of the last diagram which is not easy to read on easy screen

# Requirements to run the code

- Replace work directory with your own workspace:

```
setwd("~/Desktop/Mengji/R")
```

- Change dataset path with your own dataset file:

```
space_missions <- read.csv('./space_missions.csv')
```
