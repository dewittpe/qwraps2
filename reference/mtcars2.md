# mtcars2

An extended version of
[`mtcars`](https://rdrr.io/r/datasets/mtcars.html) data set.

## Usage

``` r
mtcars2
```

## Format

a data.frame with 32 rows and 19 columns

|          |               |                                          |                                    |
|----------|---------------|------------------------------------------|------------------------------------|
| \[, 1\]  | make          | Manufacturer name                        | parted out from `rownames(mtcars)` |
| \[, 2\]  | model         |                                          | parted out from `rownames(mtcars)` |
| \[, 3\]  | mpg           | miles per (US) gallon                    | identical to mtcars\$mpg           |
| \[, 4\]  | disp          | Displacement (cu.in.)                    | identical to mtcars\$disp          |
| \[, 5\]  | hp            | Gross horsepower                         | identical to mtcars\$hp            |
| \[, 6\]  | drat          | Rear axle ratio                          | identical to mtcars\$drat          |
| \[, 7\]  | wt            | weight (1000 lbs)                        | identical to mtcars\$wt            |
| \[, 8\]  | qsec          | 1/4 mile time                            | identical to mtcars\$qsec          |
| \[, 9\]  | cyl           | number of cylinders                      | identical to mtcars\$cyl           |
| \[, 10\] | cyl_character |                                          |                                    |
| \[, 11\] | cyl_factor    |                                          |                                    |
| \[, 12\] | vs            | Engine (0 = V-shaped, 1 = straight)      | identical to mtcars\$vs            |
| \[, 13\] | engine        |                                          |                                    |
| \[, 14\] | am            | Transmission (0 = automatic, 1 = manual) | identical to mtcars\$am            |
| \[, 15\] | transmission  |                                          |                                    |
| \[, 16\] | gear          | Number of forward gears                  | identical to mtcars\$gear          |
| \[, 17\] | gear_factor   |                                          |                                    |
| \[, 18\] | carb          | Number of carburetors                    | identical to mtcars\$carb          |
| \[, 19\] | test_date     | fictitious testing date                  |                                    |

## See also

[`vignette("qwraps2-data-sets", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/articles/qwraps2-data-sets.md)
for details on the construction of the data set.
