# Material from 2018 UseR Conference: Statistical Models for Sport in R

The repo includes R markdown files and html slides for the tutorial.

For attendees of the workshop, install all required packages with the following

```r
devtools::install_github('skoval/UseR2018')
```

Or alternatively manually install the following packages from CRAN:

```r
install.packages(c(
  'rvest', 'jsonlite', 'dplyr', 'tidyr', 'stringr', 'ggplot2', 'ggthemes',
  'scales', 'lubridate', 'BradleyTerry2', 'pitchRx', 'mgcv', 'rjags'))
```

And the following packages from GitHub

```r
devtools::install_github(c(
  'skoval/deuce',
  'johndharrison/wdman',
  'johndharrison/binman',
  'ropensci/Rselenium'
  ))
```

Also install the following additional software:

- Docker @ https://docs.docker.com/install/
