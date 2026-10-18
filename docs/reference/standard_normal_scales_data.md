# Score scales of the standard normal chart, as a data frame

Returns the z, T, percentile, sten, stanine, grade and IQ scales of
[`standard_normal_scales`](https://sedzinfo.github.io/snp/reference/standard_normal_scales.md)
in the format
[`standard_normal_custom`](https://sedzinfo.github.io/snp/reference/standard_normal_custom.md)
takes, so you can start from them and change, remove or add scales.

## Usage

``` r
standard_normal_scales_data()
```

## Value

A data frame with the columns scale, z, z_end, label and colour, and one
row per tick mark (z, T, percentile and IQ, with `z_end` and `colour`
set to `NA`) or per band (sten, stanine and grade).

## See also

[`standard_normal_custom`](https://sedzinfo.github.io/snp/reference/standard_normal_custom.md)

## Examples

``` r
scales<-standard_normal_scales_data()
head(scales)
#>   scale  z z_end label colour
#> 1     z -4    NA    -4   <NA>
#> 2     z -3    NA    -3   <NA>
#> 3     z -2    NA    -2   <NA>
#> 4     z -1    NA    -1   <NA>
#> 5     z  0    NA     0   <NA>
#> 6     z  1    NA     1   <NA>
unique(scales$scale)
#> [1] "z"          "T"          "PERCENTILE" "STEN"       "STANINE"    "GRADE"      "IQ"        
```
