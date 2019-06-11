# Calculations

The scripts in this folder conatain all calculations. 

## Docker usage
To reproduce the results, we recommend to use this docker container, where all package versions are fixed: 

```
docker pull sherresthal/classifer:0.0.1
docker run -v /e/Stefanie/Classifier/Scripts_github:/dats/scripts -it sherresthal/classifer:0.0.1 bash
```
Within the container, you can start an R-session by typing `R` and then run the scripts interactively. 

## main_predictions.R:

This script calculates the main prediction results for figures 2, 4 and S2-S7 and S10-12. 
The output is written in the results folder with the following subfolders: 

- Randomsampling
- Crosstudy
- Crossplatform



