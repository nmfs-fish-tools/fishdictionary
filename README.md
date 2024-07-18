# fishdictionary
The development and use of this tool was presented in [Wetzel et al. 2024](https://www.sciencedirect.com/science/article/pii/S0165783624001681?via%3Dihub) in a special edition of Fisheries Research on Stock Assessment Good Practices in fisheries. Quantitative methods for assessing the status of marine fish populations began in the 19th century with simple catch-curve analysis and have evolved through the decades to use more advanced integrated statistical modeling. The corresponding assessment documents that communicate model estimates to other scientists and fisheries managers, generally, have grown in length and complexity, often describing hundreds of parameter estimates and population dynamics. These documents use a wide range of synonyms or similar terms that can vary either within a single assessment document or among other documents within or across fisheries management regions. The lack of standardization of terms used in assessment documents can lead to challenges in scientific reviews, communicating scientific results to fisheries managers, and for understanding assessments from other management regions.  

# Shiny based webpage
The information within {fishdictionary} is presented as an interactive shiny based webpage (https://connect.fisheries.noaa.gov/fishdictionary/) where users can look up specific terms, definitions, and the preferred terminology.

# Installation
The package can be installed from github via either {pak} or {remotes}:

```
# Option 1: install via {remotes}
install.packages("remotes")
remotes::install_github("nmfs-fish-tools/fishdictionary")

# Option 2: install via {pak}
install.packages("pak")
pak::pkg_install("nmfs-fish-tools/fishdictionary")
```
Once you have installed {fishdictionary}, it can be loaded using:

```
library(fishdictionary)
````

# How to contribute
This package is designed to grow with input from the fisheries community and is an open source `R` package that can be used to denote common fisheries terminology and standardize use. The dictionary is written in a JSON format and can be added to or modified using the R functions by:

1. Create a fork of the github repository.
2. Install {roxygen2} and {rsconnect}.
3. Add or modify the .R file with a roxygen2 block above a single line of code setting the value of the term to NULL.
4. After the .R file is populated and saved, run `roxygen2::roxygenize()` to create a corresponding .Rd file to display the documentation entry with proper formatting.
5. Check that the .Rd file appears to be formatted properly.
6. Run the command rsconnect::writeManifest("./inst/Shiny") which will create a new manifest.json file that will update the Shiny app on Rstudio connect.
7. Add the new .R, .Rd, and manifest.json file to a github commitand open a pull request from the fork.

# Citation

Wetzel, C.R.,  Stawitz, C.C., Li, B., Johnson, K.F., and Schmitz, G.M. 2024. Ordering the alphabet soup: Strategies to improve consistency and develop a framework of tools for fisheries science. Fisheries Research, 278: 107104.
