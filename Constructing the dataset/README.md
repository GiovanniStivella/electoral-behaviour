I firstly took electoral data at precinct-level from ALARM dataset, which also includes ethnic composition of precincts (`Preprocessing_ALARM.R`).
Then, I took estimates of candidates' ideological positions as estimated by DIME (`Preprocessing_DIME.R`)
In order to account also for other demographic features of the precinct, I collected data from census at a precinct-level (`tidycensus.R`).

Finally, I merged all this data in order to create a dataset that collects electoral results, demographic composition and educational attainment in each precinct in Pennsylvania for each statewide election from 2016 to 2020 (`Merging.R`).
