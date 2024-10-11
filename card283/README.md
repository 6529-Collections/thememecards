
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:25          Length:25          Min.   :1   Length:25         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:25         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20896969 # https://etherscan.io/block/20896969
block_hash <- "0x1187fa5bf48e6a58ee1ae174cdd5b6837a846f9df2d93f64e8b1d31d8f40302e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4767 

## Code

``` r
subtract_multi <- function(x, y) {
  for (i in y) {
    where <- match(i, x)
    if (!is.na(where)) {
      x <- x[-where]
    }
  }
  return(x)
}

pick <- function(df,
                 contracts=c(),
                 address_remove=NULL,
                 address_subtract=NULL,
                 address_max=Inf,
                 address_pick=NA,
                 address_replace=FALSE) {

  df <- df %>%
    dplyr::filter(name %in% contracts) %>%
    dplyr::filter(!(address %in% address_remove))
  
  df_by_address <- df %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      balance = ifelse(sum(balance) <= address_max, sum(balance), address_max)
    )
  
  pool <- df_by_address %>%
    dplyr::arrange(address) %>%
    dplyr::select(address, balance) %>%
    purrr::pmap(function(address, balance) {
      base::rep(address, balance)
    }) %>%
    unlist() %>%
    subtract_multi(address_subtract)
  
  if (is.na(address_pick)) {
    return(pool)
  } else {
    return(base::sample(pool, size=address_pick, replace=address_replace))
  }
}

tally <- function(x) {
  if (length(x) > 0) {
    unlist(x) %>%
    data.frame(address = .) %>%
    dplyr::group_by(address) %>%
    dplyr::summarise(
      amount = n()
    ) %>%
    dplyr::arrange(desc(amount), address)
  }
}
```

``` r
base::set.seed(seed)

address_remove <- c(
  "0x3a3548e060be10c2614d0a4cb0c03cc9093fd799",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","ZIGOR"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("NGEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","ZIGOR","NGEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 19 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1dffd9127c32d30035477ad65fb1fabf81b11f04      1
     2 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     3 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     4 0x446a8a5def6ff0cf68046b6d22fd3f9f868fa616      1
     5 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     6 0x5c0a60854dd913c73ffe538c71d810f9b1b24c84      1
     7 0x6f4ae4a5874367a7f0728499928b0ec027fbe07f      1
     8 0x712e63c56304e9932d3aa339ef7244fbccce4508      1
     9 0x84ecd8bae0fff81b722a24d4e00b010bd02691d4      1
    10 0x889c17e967f6af03ab6f7183dab25ee4767b1811      1
    11 0x90e5aa59a9df2add394df81521dbbed5f3c4a1a3      1
    12 0x972e633f5f6f62d1f2a13977f113941fcb3b606b      1
    13 0xa342d9e35afe0b9b580ac441c6e03c0a1558f118      1
    14 0xab6ca2017548a170699890214bfd66583a0c1754      1
    15 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    16 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    17 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    18 0xddded508d470892a60d0a76df30918a7a89e37a6      1
    19 0xe468ce99444174bd3bbbed09209577d25d1ad673      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 2 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
    2 0xf0e36d240365cf1d18794b1221e2b867d866ca94      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 21 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1dffd9127c32d30035477ad65fb1fabf81b11f04      1
     2 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     3 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     4 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
     5 0x446a8a5def6ff0cf68046b6d22fd3f9f868fa616      1
     6 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     7 0x5c0a60854dd913c73ffe538c71d810f9b1b24c84      1
     8 0x6f4ae4a5874367a7f0728499928b0ec027fbe07f      1
     9 0x712e63c56304e9932d3aa339ef7244fbccce4508      1
    10 0x84ecd8bae0fff81b722a24d4e00b010bd02691d4      1
    11 0x889c17e967f6af03ab6f7183dab25ee4767b1811      1
    12 0x90e5aa59a9df2add394df81521dbbed5f3c4a1a3      1
    13 0x972e633f5f6f62d1f2a13977f113941fcb3b606b      1
    14 0xa342d9e35afe0b9b580ac441c6e03c0a1558f118      1
    15 0xab6ca2017548a170699890214bfd66583a0c1754      1
    16 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    17 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    18 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    19 0xddded508d470892a60d0a76df30918a7a89e37a6      1
    20 0xe468ce99444174bd3bbbed09209577d25d1ad673      1
    21 0xf0e36d240365cf1d18794b1221e2b867d866ca94      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.2 (2022-10-31)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
