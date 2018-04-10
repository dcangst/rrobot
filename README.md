# rrobot - Useful R functions for Tecan Freedom Evo Liquid Handling Platforms

functions for generating worklists and reading data from reader. It is geared toward the setup we have 
in the (Theoretical Biology Group)[www.tb.ethz.ch] at ETH Zürich but might be useful, or at least a
starting point for others, so here goes.

Obviously, use at your own risk. Make sure you know what the commmands do (they usually correspond to a single 
Basic or advanced worklist command that you can find in the EvoWare Documentation.)

## Installation

To install the package directly from git you will need the following things:

* on Mac OS
    * Xcode (to build the packages; from the App Store (for free)).
    * [git](http://git-scm.com)
    * devtools (from CRAN or and/or github, see below)

* On Windows (i think, not tested)
    * [Rtools](http://cran.r-project.org/bin/windows/Rtools/) to build packages
    * [git](http://git-scm.com)
    * devtools (from CRAN and/or github, see below)

to install directly from git:
```R
    
    ### run once ###
    # macOS / probably other unix systems (not tested):
    # install.packages(c("devtools"))
    # for dev-version run: devtools::install_github("r-lib/devtools")
    
    # Windows:
    # install.packages(c("devtools","stringr"))
    # library(devtools)
    # build_github_devtools()

    # !! Restart R before continuing !!#
    # install.packages("devtools.zip", repos = NULL)
    # Remove the package after installation
    # unlink("devtools.zip")
    ### end run once ###

    devtools::install_github("dcangst/rrobot")

```

## Usage

First, load package and initialize defaults and worklist:

```R
    library(rrobot)
    init()
    gwl #displays the initialized worklist & worktable
```

`init` has some reasonable (?) defaults, see `?init` for more info.

After that you can use the commands provided. Every command adds a row to `gwl$worklist`. See `?rrobot`, or use `?` in general, for details about the commands available, or check the source and implement more! Obviously all commands can be looped etc.

all commands have a some helpful information, use e.g. `?adv_aspirate`. `?rrobot` lists the most useful commands.

When finished with assembling your worklist use `write.gwl(gwl)` to write the worklist to the current working directory using the filename specified in `init`.


## Usage in a EVOware script

One can run R code from the command line on Windows:
`RScript.exe --vanilla --slave Rfile.R --args arg1 arg2 arg3...`
arguments can then be accessed within the `R` script like this:
`args <- commandArgs(TRUE)` where `args[1] = "arg1"` (a character vector).

one can also write a batch script:
```bat
    set R_Script="C:\Program Files\R\R-3.1.2\bin\RScript.exe"
    %R_Script% --vanilla --slave Rfile.R --args arg1 arg2 arg3...`
```

more info:
http://cran.r-project.org/doc/manuals/R-intro.pdf , appendix B
or `?Rscript`

This can then be called from within EvoWare

## Develop

I started to develop the package using [devtools](https://github.com/hadley/devtools) which uses [roxygen2](https://github.com/klutometis/roxygen) to document packages.

here is how I build the documentation and package:

```R
library(devtools)

dev_mode()

setwd("path to local git")
document()
build()
load_all()
```

after that you're ready to use the current build and commit! `dev_mode()` changes into dev_mode: this means that you do not overwrite your (potentially more stable) packages, but work in a safe development environment.
