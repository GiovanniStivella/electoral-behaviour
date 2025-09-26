# Modelling electoral behaviour in Pennsylvania

In this repository I collect R scripts that I have used to estimate ideological positions of Pennsylvania voters and politicians based on a spatial model of voting.

In the folder `Constructing the dataset` I have collected the scripts used to build a dataset that collects electoral results at a precinct level in statewide elections in Pennsylvania from 2016 to 2020, combined with demographic characteristics of precincts and estimates of ideological positions of candidates according to DIME dataset.

I performed linear regression to produce estimates of the ideological positions of precincts and plot them into a geographical map (`lmer_maps.R`)

In the folder `Bayesian estimation` I collected the scripts of R and Stan used to perform Bayesian estimate of ideological scores of both the precincts and the candidates.

Finally, I compared results obtained through linear regression and resulst obtained through Bayesian estimation (`Comparisons without demographics.R` and `Fixed candidate ideology.stan`).
In particular, in this comparison I took a linear regression that does not account for demographic characteristics and I compared it with a Bayesian estimation of ideology of precincts taking ideology of candidates fixed according to DIME scores. As the ideological positions of precincts estimated by linear regression follow a distribution that resembles a Normal distribution, I expect nearly identical results when I estimate through Bayesian estimation with a Normal prior.
