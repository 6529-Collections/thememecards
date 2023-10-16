<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:1714        Length:1714        Min.   : 1.000   Length:1714       
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.313                     
                                           3rd Qu.: 1.000                     
                                           Max.   :76.000                     
         name          
     Length:1714       
     Class :character  
     Mode  :character  
