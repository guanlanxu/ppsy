# ppsy


`ppsy` provides a solution for operational psychometric workflow. 


## Installation

You can install `ppsy` from GitHub. 

devtools::install_github("guanlanxu/ppsy")

## Usage

`ppsy` provides tools for operational psychometric workflow. Below are some highlighted features:

* `newadmin_setup` function creates directories for specified administration. This function takes care of both operational and field-testing scenarios.  

* `get_path` function generates all paths that would be needed in future work, including path to calibration data, to test map, to bank files, to input and output files, etc.


* IRT calibration function `cal_fun` calls IRTPRO or WINSTEPS. This function does require IRTPRO and/or WINSTEPS to be installed on local machine in advance. Currently, this package only supports `Rasch`, `2PL`, `3PL`, and `GPC` models. Model fit statistics can be computed using `modfit_fun` function. 

* Equating, drift analysis, and fit plots. (see `STURIT_4drift()`,`get_d2()`,`anchor_d2check()`). 

* Raw-to-scale score conversion table can be generated using `get_rsss` function. 

We recommend to run `newadmin_setup` and `get_path` before any calibration and/or equating work. 


## Contributing

If you would like to contribute, please contact the author of this package.

## License
GPL v3.0

=======

## Other Info
https://guanlanxu.github.io/posts/2023/04/ppsy/



