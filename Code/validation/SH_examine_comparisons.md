Compare findings
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(ggplot2)
```

``` r
le.table <- read.csv("~/BlackWhiteMortalityGap/Code/3_validation/le_table.csv", header = T)
SH.dat <- read.csv("~/SH_HA_supp_app_data.csv", header = T)
```

``` r
SH.dat <- SH.dat %>% rename(state = State, 
                            le.black.original = le.black,
                            le.white.original = le.white,
                            le.gap.original = le.gap, 
                            le.gap.ci.original = le.gap.ci)

compare.le <- merge(le.table, SH.dat, by = c("state", "sex", "year"))

test <- strsplit(stringr::str_sub(compare.le$le.gap.ci.original, 2, -2), ",")
test2 <- matrix(unlist(test), ncol=2, byrow=TRUE)

compare.le$le.gap.lcl.original <- as.numeric(test2[ , 1])
compare.le$le.gap.ucl.original <- as.numeric(test2[ , 2])
rm(test, test2)

compare.le <- compare.le %>% mutate(le.gap.width.original = le.gap.ucl.original - le.gap.lcl.original,
                                    le.gap.width = le.gap.ucl - le.gap.lcl,
                                    small.black.state = state %in% c("Hawaii", "Alaska", "Maine", 
                                                                     "New Hampshire","Utah", "New Mexico")
                                    )

write.csv(compare.le, "~/BlackWhiteMortalityGap/Code/3_validation/compare_LE.csv")
```

**Figure 1: Comparison of life expectancy between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.white.original, y = le.white)) +
  geom_point(aes(x = le.black.original, y = le.black, col = "blacks")) + 
  geom_point(aes(col = "whites")) +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy (years)") +
  xlab("Original life expectancy (years)") +
  geom_text(data = subset(compare.le, small.black.state == T & year != "difference"),
            aes(x = le.black.original, y = le.black, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-1-1.png)

**Figure 2: Comparison of life expectancy change over time (1990 vs. 2009) between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.white.original, y = le.white)) +
  geom_point(aes(x = le.black.original, y = le.black, col = "blacks")) + 
  geom_point(aes(col = "whites")) +
  facet_grid(~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy change (years)") +
  xlab("Original life expectancy change (years)") +
  geom_text(data = subset(compare.le, small.black.state == T & year == "difference"),
            aes(x = le.black.original, y = le.black, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-2-1.png)

**Figure 3: Comparison of life expectancy gap between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.gap.original, y = le.gap)) +
  geom_point() +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy gap (years)") +
  xlab("Original life expectancy gap (years)") +
  geom_text(data = subset(compare.le, small.black.state == T & year != "difference"),
            aes(x = le.gap.original, y = le.gap, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-3-1.png)

**Figure 4: Comparison of change over time (1990 vs. 2009) in the life expectancy gap between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.gap.original, y = le.gap)) +
  geom_point() +
  facet_grid(.~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Change in life expectancy gap (years)") +
  xlab("Original change in life expectancy gap (years)") +
  geom_text(data = subset(compare.le, small.black.state == T & year == "difference"),
            aes(x = le.gap.original, y = le.gap, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-4-1.png)

**Figure 5: Comparison of CI width for LE gap between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.gap.width.original, y = le.gap.width)) +
  geom_point() +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Width of CI for LE gap") +
  xlab("Original width of CI for LE gap") +
  geom_text(data = subset(compare.le, small.black.state == T & year != "difference"),
            aes(x = le.gap.width.original, y = le.gap.width, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-5-1.png)

**Figure 6: Comparison of the change over time (1990 vs. 2009) in the CI width for LE gap between the original model and our model**

``` r
ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.gap.width.original, y = le.gap.width)) +
  geom_point() +
  facet_grid(.~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Width of CI for LE gap") +
  xlab("Original width of CI for LE gap") +
  geom_text(data = subset(compare.le, small.black.state == T & year == "difference"),
            aes(x = le.gap.width.original, y = le.gap.width, label = state), size = 2.5)
```

![](SH_examine_comparisons_files/figure-markdown_github/unnamed-chunk-6-1.png)