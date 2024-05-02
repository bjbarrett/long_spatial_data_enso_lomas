# long_spatial_data_enso_lomas
Repository for:

Jacobson, Barrett, Perry, Finerty, Tiedeman and Crofoot. 2024. A new approach to geostatistical synthesis of historical records reveals capuchin spatial responses to climate and demographic change. *Ecology Letters*.

This is code for running the analyses from the ctmm outputs.
The code for validation is hosted one level up on Edmond.
This paper created outputs of figures, tables, and model outputs in R and converted them to LaTeX commands.
These were called from Overleaf so the results, tables, and figures yield a dynamic fully reproducible document linking `R` to LaTeX using `.tex` files and specific LaTeX commands.
This is an implementation of [Bret Beheim's workflow here](https://babeheim.com/blog/2022-11-02-dynamic-latex/).

The source data for the historical reconstruction is hosted on a separate Dryad repository [here](https://datadryad.org/stash/share/k1W69-3V5K-Hd9NiW6xwe53QWouJYX2ZEprmkT75EU4).
Exact movement locations and GPS data of the study species are kept anonymous for conservation reasons.
They are [archived on Movebank](https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study3388993859) and can be [requested from Susan Perry](sperry@anthro.ucla.edu)

## \\manuscript
See `\manuscript` folder for Overleaf project

## \\code
`01_data_fit_processing.R` containts code to process the CTMM outputs from Odd's scripts

`02_models_enso.R` is R script to fit the STAN models. If you wish to propagate the uncertainty in home range estimates from `ctmm` AKDEs like we do here this, combined with the stan code will show you how

`03_ensoplots.R` is code to generate many of the plots related to ENSO and group size

`04_enso_tables.R` is `R `code to generate LaTeX files that are the in the `\manuscript` folder

`05_dynamic_calcs.R` `R `code to generate `.tex` files with dynamic computations

## \\data 
`df_annual_riparian.csv` is the proportion of riparian area data that is in the supplemental material

`df_slpHRarea_group_size.csv` is the data used in the main group size and ENSO components
It contains the group home range areas and SD per year alongside group sizes. 

`era5_environmental_df.csv` is data used in Figure S1 from era5 database. See [here](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview) for more detail.

## \\stan_code 
Code for `.stan` programs to fit hierarchcail models. 
Models used for testing and validating code, as well as those included in the final paper are included.

## \\tex_files_from_R
`.tex` files of tables and dynamic computations generated using code
