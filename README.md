# RangeR : Easy calculation of acoustic receiver detection range from static and mobile range tests <img src="vignettes/RangeR_hex.png" align="right" width="120" />

RangeR package for calculating the effective range of Innovasea acoustic receivers, along with other handy functions for easy manipulation of acoustic telemetry data. Useful function include distance between receiver calculations, find closest neighbouring receiver, geodetic to utm conversions and more.

## Overview

### Mobile (Drift) Range Tests

Mobile range tests, often referred to a drift tests involve deployment of a stationary receiver and moving a transmitter within the detection field. 

Mobile range tests are often short term and do not cover a wide range of environmental conditions which can impact detection range. While not incorporating environmental variation, mobile range tests can capture a greater range of distance measures between the receiver and transmission source.  

### Static Range Tests

Static range tests typically span longer time scales than mobile range tests but are typically limited to sampling a very small number of distances between receivers and the transmission source. The benefit of static tests is their ability to capture a greater range of enviornmental variability.  


Step to calculate Dift Range Test

1.  parse logfiles from stationary receiver using FathomR::parse_fathom_files()
2.  select devices used in the test using select_devices()
3.  get the start and end time of the deployment using get_deployment_time()
4.  create a station vector listing Long,Lat
5.  select the range test Full_ID using select_range_transmitter(det)
6.  extract transmission from the VR100 log using extract_VR100_transmission
7.  extract the detection data from the stationery receiver using drift_detections()
8.  compile drift analysis using compile_drifts()


## Main Functions

### Select Devices
`select_devices()`

Selection of the stationary receiver which defines the detection data and the device (VR100) used to record the transmission of acoustic signals

### Get Deployment Time
`get_deployment_time()`

A function designed to automatically calculate the start and the end of the stationary receiver deployment using the in-built tilt sensor. When performing range tests using VR2W receivers, users define start and end times manually. 

### Select Range Transmitter
`select_range_transmitter`

The function scans the transmission data from the VR100 identified by select_devices() and allows the user to define the full ID code of the transmitter being used in the range test. 

### Extract (VR100) Transmissions
`extract_VR100_transmission`

Creates a dataframe from the VR100 log filtering be the full ID code defined by the `select_range_transmitter` function.  


# Installation

- **R** `RangeR` requires`R` version ≥ 4.1 which can be installed from **R-project [`R`](https://www.r-project.org)**. If you are unsure of your R version, try `R.version.string` from the `R` console. To update R from within RStudio, install and load the `installr` package and run `updateR()`

- **Install remotes**
``` 
# Install the remotes package
install.packages("remotes")

# Use remotes to install the FathomR package from github repo
remotes::install_github("hughpederson/RangeR", 
                        build_vignettes = TRUE,
                        dependencies = TRUE)
```
