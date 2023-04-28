
## Arranging and grouping multiple times to sort data ...

## is there an easy way to do this in dplyr?

## The order of the groups should be determined by the vals
## Within in group, arranged by the year
names <- rep(c("A", "B", "C"), times = 3)
year <- rep(2019:2021, each = 3)
vals <- c(3, 7, 5, 4, 2, 9, 8, 6, 1)

df <- data.frame( names, year, vals)

## For this example, the output should be:
# _|_names_|_year_|_vals_|_
# _|_  C  _|_2019_|_ 5  _|_
# _|_  C  _|_2020_|_ 9  _|_
# _|_  C  _|_2021_|_ 1  _|_
# _|_  B  _|_2019_|_ 7  _|_
# _|_  B  _|_2020_|_ 2  _|_
# _|_  B  _|_2021_|_ 6  _|_
# _|_  A  _|_2019_|_ 3  _|_
# _|_  A  _|_2020_|_ 4  _|_
# _|_  A  _|_2021_|_ 8  _|_

