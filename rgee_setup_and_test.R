remotes::install_github("r-spatial/rgee")
library(rgee)

## It is necessary just once
ee_install()

# Initialize Earth Engine!
ee_Initialize()

# full check of status of rgee dependencies
ee_check() # Check non-R dependencies
ee_clean_credentials() # Remove credentials of a specific user
ee_clean_pyenv() # Remove reticulate system variables


