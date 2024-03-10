# Modeling COVID Time-Series Data - Country Level Predictions at a Weekly Scale

##### Data Wizards: Donald Stricklin, Alex Olmeta, Alex Paine

## Data

The initial data was gathered from [*Our World in Data*](https://github.com/owid/covid-19-data/tree/master/public/data), a subsidiary of the non-profit Global Change Data Lab. This was then pared down to the `covid_cleaner.csv` file which contains outcome variable `new_cases` as well as 40 predictor variables. The majority of our models were built on individual country datasets, subsets of the `covid_cleaner.csv` file. We focused on 10 countries:

-   Argentina, Australia, India, Italy, Malaysia, Mexico, Morocco, Peru, Sweden, United Kingdom

## Models

We trained 3 different families of models:

-   ARIMA (ARIMA, SARIMA, Auto-ARIMA)

-   Prophet (univariate and multivariate)

-   XGBoost

## General Notes

This project was a highly collaborative effort. Our approach was to each build a baseline model of every type and then come together, discuss findings, and improve models based on team insights. Our best models can be found on the main branch, while individual attempts are housed in team members' personal branches.
