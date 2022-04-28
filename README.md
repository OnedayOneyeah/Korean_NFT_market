# The characteristics of the Korean NFT market and strategies for target expansion
## Overview

This repository is for the research on the specialty of the Korean NFT market in comparison to the global and American NFT market, and suggests the solution for expanding the targets. The Input folder includes the datasets manipulated in the paper and a bibTex file used for a bibliography in RMarkdown file. The output folder consists of RMarkdown file with the exported pdf file, and the log file. Finally, RScript cleaning the input datasets is included in Script folder.

## Comupational Requirements

To run the codes in RMarkdown and RScript file, R[1] is required to be installed. Related R packages should be installed before running the codes as well, and it is acheivable by running codes in 'computational_requirements.R' file in this repository. We used tidyverse[2], tidytext[3], Hmisc[4] to manipulate the datasets, knitr[5] and kableExtra[6] to generate tables with the cleaned datasets and fix its placement in the paper. Finally, bookdown[7] was used as a powerful cross referene tool for generating final pdf document.

## Data Availability and Provenance Statements

### Statement about Rights

I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.

### Summary of availability

All data used in this paper is available here.

## References

[1] R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

[2] Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686

[3] Silge J, Robinson D (2016). “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” JOSS, 1(3). doi: 10.21105/joss.00037 (URL: https://doi.org/10.21105/joss.00037), <URL: http://dx.doi.org/10.21105/joss.00037>

[4] Frank E Harrell Jr (2021). Hmisc: Harrell Miscellaneous. R package version 4.6-0. https://CRAN.R-project.org/package=Hmisc

[5] Yihui Xie (2021). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package version 1.37.

[6] Hao Zhu (2021). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. R package version 1.3.4. https://CRAN.R-project.org/package=kableExtra

[7] Yihui Xie (2021). bookdown: Authoring Books and Technical Documents with R Markdown. R package version 0.24.
