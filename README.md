
## cartools: Tools for understanding highway performance.

### Installation

You can install the released version of **cartools** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cartools")
```

### Recommendation

To obtain the latest updates, use RStudio and download **cartools** from
GitHub. Run the following:

1.  install.packages(“devtools”).

2.  devtools::install\_github(“pjossenbuggen/Basic”).

3.  library(cartools)

4.  *zipper.simulate*(5,5,53.1,5,-700,0.125,0,40,-500,14,1,1.333). To
    see the results, click on Merge.pdf above. A time-location
    trajectory \(t-x\) is shown.

### State of development

At this point in time, **cartools** focuses on freeway bottleneck
merging, the lose of a traffic lane from two lanes to one, and the
effects of speed volatility on performance. Two merging protocols are
analyzed: 1. *Zipper* 2. *Side-by-side*.

For the **cartools** vignette, click on:
<https://pjossenbruggen.github.io/cartools/index.html>.
