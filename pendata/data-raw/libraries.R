

library(rlang)
library(tidyverse)
tprint <- 75  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint)
library(quarto)

# load packages needed for development process ---- library(devtools)
# library(usethis) library(pkgload) library(lintr) library(styler)

# tools ----
library(fs)
library(vroom)
library(readxl)
library(openxlsx)  # for writing xlsx files
library(lubridate)
# library(RcppRoll) library(fredr) library(tidycensus)
library(zoo)

# boyd libraries - install from github/donboyd5
library(btools)
# library(bdata) library(bggtools)

# graphics
library(RColorBrewer)
library(scales)
# library(ggbeeswarm)
library(patchwork)
library(gridExtra)
# library(ggrepel) library(ggbreak)

# tables
library(knitr)
library(kableExtra)
library(DT)
library(gt)
library(gtExtras)
library(janitor)
library(skimr)
