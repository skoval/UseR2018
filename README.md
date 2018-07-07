# Material from 2018 UseR Conference: Statistical Models for Sport in R

The repo includes R markdown files and html slides for the tutorial.

For attendees of the workshop, completing all of the examples will require that you have installed all of the following packages from CRAN:

- rvest
- jsonlite
- dplyr
- tidyr
- stringr
- ggplot2
- ggthemes
- scales
- lubridate
- BradleyTerry2
- pitchRx
- mgcv
- rjags

Install the following package from github:

- deuce with `install_github('skoval/deuce')`

- RSelenium with `install_github('ropensci/Rselenium')`

Install the following additional software:

- Docker @ https://docs.docker.com/install/

# Troubleshooting

## RSelenium

Recent versions of R may throw an error about the RSelenium dependencies on wdman and binman. It is possible to install RSelenium without these dependencies. You can see instructions for that here:

[RSelenium without wdman and binman](https://github.com/ropensci/RSelenium/pull/177)


## Docker and Windows

If you are using a Windows system, you may encounter difficulties installing docker and the firefox Selenium server. Below are instructions suggested by [Ian Maurer](www.ianmaurer.net) for installation on Windows.

To run an instance of Firefox browser on a Linux VM using Docker on Windows 10

Note that:

1. You must have Windows Pro or Enterprise installed.

2. Containers and Hyper-V must be enabled. Right-click Start button, search for

   Turn Windows Features On or Off, select this then click the boxes for Containers

   and Hyper-V. Make sure that all Hyper-V features are ticked. These settings will
    require restarts when you click OK.

3. Docker must be installed and running. Hover over its icon in the system tray to

check. It might be necessary to right-click on icon, go to Settings and on the

Reset tab select Restart Docker. If that does not work try Reset to Factory Defaults.

Note that a factory default reset will erase all existing containers.

3. Because the images we use are Linux-based, we must run a Linux VM in our Docker

 container. The VM is usually named MobyLinuxVM and can be found by right-clicking

Start and searching for Hyper-V. Click to open the Hyper-V manager. MobyLinuxVM

 should be listed. Its state needs to be set to running. If it is not then right-

click on the state and start it.

4. You should then be able to execute the following command from an R script or the RStudio console without errors

shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
