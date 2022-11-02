# swimctr
Oregon SWIM v3.0pre Commercial Transport (CT) Model

This repository contains the code necessary to build and run the commercial transport (CT) module of Oregon's statewide integrated model (SWIM) system. The code is written in R, and configured as a formal R package. It uses the doParallel library to reduce the model run time for selected functions. Running them in parallel is not required, but results in substantial runtime reductions. Our initial tests reveal that the simulation scales well up to about 16 cores, but afterwards runtime starts increasing due to the additional time required to set up and populate so many cores. Users will need to test different hardware configurations in order to determine optimal number of cores for their hardware.

The CT system is set up as a R package, which can be installed from this repository:
```r
install.packages("devtools")  # If not already installed
devtools::install_github("tlumip/swimctr")
```

In theory one could mix and match the included functions in any order to run the model, but the run_model template assumes that the model is implemented as described in the SWIM2 Model Development Guide and project wiki:

+ Run setup (e.g., setting paths, reading runtime properties, importing input-output make and use coefficients)
+ Generating synthetic firms
+ Running the local truck tour model, which includes the typical sequence of generation, destination choice, and temporal allocation of trucks from the synthetic firms
+ Running the inter-regional truck model, which transforms annual commodity flow estimates into daily truck trips between external markets and the modeled area
+ Combining the local and inter-regional truck trips, including departure times, into a single trip list for later processing (e.g., traffic assignment, data mining)

A simplified and fast process for generating pseudo-firms is currently implemented, which takes employment levels by sector from the PECAS model and creates a pseudo-firm for each sector within each alpha zone. This allows the synthetic firms to be quickly built in each simulation year, gets around the limitation that PECAS generates flows by sector (not firms), and obviates need to add forecasts of how firm size will change in the future. The simplified firm synthesis code is included within this repo, rather than using heavyweight firm synthesizer. The synthetic firms are generated for all of the alpha zones in the model, to include those in the halo (portions of states adjacent to Oregon). 

The inter-regional commodity flows are obtained from the FHWA [Freight Analysis Framework (FAF)](https://faf.ornl.gov/fafweb/). Flows from Version 5.4 were used during the update to CT v3.0 models. These data must be pre-processed for use in CT using the `prebuild_faf_multiyear.rmd` script in the `data/raw` folder. Also included in the package are the files used to translate tons to truckload equivalents using payload conversion factors borrowed from the 2012-16 Canadian Commodity Flow Survey.

The calibrated model parameter data for use in Oregon are included in the `data-raw` directory. When used with the [SWIM2 system](https://www.oregon.gov/ODOT/Planning/Pages/Technical-Tools.aspx) they should be placed in the `t0` folder when building SWIM runs, with their location made explicit in the `swim.properties` file. Even when these data are successfully bundled into the package the user will be able to overwrite them by simply specifying different data as part of the model setup.
