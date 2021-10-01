*The modelling work is still at an evaluation stage.*

# expanse_multiyear

Version 0.1.0

This is a place to explore spatio-temporal modelling for multiple years at an annual scale


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```

## Code

In the folder src, you can find R code for implementing the multi-year modelling for annual average exposures for the entire Europe. 

#### SLR, RF
``00_ model_structure_clean.R`` gives the modelling process for supervised linear regression and random forests.

#### GTWR
``00_gtwr.R`` implements the geographically and temporally weighted regression.
These two R scripts implements 5-fold cross-validation 


``00_optimize_gtwr.R`` tunes the parameters for GTWR (lamda, ksi, and the equivalent temporal distance).


### Output random forests predictions at random points
``01_combineRandomPointsPredictors.R``
``02_rf_randomAndEscape.R``
#### Then visualize the results (combined with the predictions obtained from GEE for SLR, GWR, GTWR)
``03_vis_escape2.R``
``03_vis_randomPoints2.R``



## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
