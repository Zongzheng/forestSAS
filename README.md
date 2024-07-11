# forestSAS

An R package for forest spatial structure analysis systems.

In recent years, there has been considerable interest in a group of neighborhood-based structural parameters that properly express the spatial structure characteristics of tree populations and forest communities and have strong operability for guiding forestry practices.forestSAS package provide more important information and allow us to better understand and analyze the fine-scale spatial structure of tree populations and stand structure.

## Installation

The development version from github:

```R
##install.packages("devtools")
library(devtools)
remotes::install_github("Zongzheng/forestSAS")
##install.packages("shiny","webshot","ggplot2","ggimage","spatstat")
library(shiny)
library(webshot)
library(ggplot2)
library(ggimage)
library(spatstat)
library(forestSAS)
####App for forestSAS package is used for simulated data
forestSAS_simapp()
####App for forestSAS package is used for real data
forestSAS_realapp()
```

##Citation

Chai ZZ.2016.forestSAS：An R package for forest spatial structure analysis systems.URL:https://github.com/Zongzheng/forestSAS.
