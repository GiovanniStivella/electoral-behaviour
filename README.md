# Modelling electoral behaviour in Pennsylvania

In this repository I collect R scripts that I have used to estimate ideological positions of Pennsylvania voters and politicians based on a spatial model of voting.

I firstly took electoral data at precinct-level from ALARM dataset, which also includes ethnic composition of precincts (`Preprocessing_ALARM.R`).
Then, I took estimates of candidates' ideological positions as estimated by DIME (`Preprocessing_DIME.R`)
In order to account also for other demographic features of the precinct, I collected data from census at a precinct-level (`tidycensus.R`).

Finally, I merged all this data in order to create a dataset that collects electoral results, demographic composition and educational attainment in each precinct in Pennsylvania for each statewide election from 2016 to 2020 (`Merging.R`).

Starting from this dataset, I was able to perform statistical analysis.

The first stage was to perform linear regression and producing estimates of the ideological positions of precincts (`lmer_maps.R`)

Then, Bayesian estimation of ideological positions of both precincts and candidates are tried through STAN software.
