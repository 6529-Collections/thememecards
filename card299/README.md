
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:57          Length:57          Min.   :1   Length:57         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:57         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21196969 # https://etherscan.io/block/21196969
block_hash <- "0xf8fdd04e5de61f4179528ff8c1cd866d5f80196ac24d14f5415343eb4b325705"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4571 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Babylon","Babylon2","Jesperish11","Foundation","Foundation2","MovsumxJesperish","ThreeRooms"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("JesperishEditions","JesperishEditions2"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Babylon","Babylon2","Jesperish11","Foundation","Foundation2","MovsumxJesperish","ThreeRooms","JesperishEditions","JesperishEditions2"), address_remove=address_remove,address_max=1)
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
     1 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     2 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     3 0x229c098df6ad81894378456efaa1ee20652a77ef      1
     4 0x3b2887ae95e50f22e9a1b225fff308503aed48e7      1
     5 0x3deed956b999b83361b85bff31d388c35125411d      1
     6 0x3f291388477ab6be9968b2db0aee48fb0e2154e1      1
     7 0x478087e12db15302a364c64cdb79f14ae6c5c9b7      1
     8 0x478bb542f7658d635abba67edb987806dff5b83d      1
     9 0x49ffe4429610efa450b0654ea9a20e0eb4960920      1
    10 0x55c0c8186541868c03f434e9606e112ee0153965      1
    11 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
    12 0x5e6efe126f017901a2ed55954c5b54dd7d4e6bdb      1
    13 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    14 0x80ce7206da1ed2804d645a91705212ed742f285d      1
    15 0x8b51c1ba09ee33e7649cac62ccb6d0f410f5647a      1
    16 0x9f08b507410725ab74ed3d61ed471235f1ee7d16      1
    17 0xb4ef3e6c45e39ab6b8d39bb935e57c68f4d19a0b      1
    18 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    19 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x073d67c28d909a18555a4a860df3f309ba5472c7      1
     2 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     3 0x1cbb87bb9a4dd0316189eede2277a58590dc124a      1
     4 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     5 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
     6 0x4da792b5058f59162e1b619749a0ce4e984d4841      1
     7 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     8 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
     9 0x6db40b1656a020e6dfbe1ff36a16d8742954acd8      1
    10 0x72915ad3110eb31768a562f540ac1ebcd51d3dc8      1
    11 0x77039399801f462b4ed13444a266b16355c471bf      1
    12 0x782adafbf47a604f146af4a059908e946eae539f      1
    13 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    14 0x85a19dd2ad0d1d2b25bb164810fad08cdc0b33d7      1
    15 0x95ecac09c1cd59e8a6d492bd06774b7b98f17be7      1
    16 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    17 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    18 0xaa5429e85fb9c8d39e835a533091642676be83d1      1
    19 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
    20 0xd3fac37e04a928f581459ea81d7b1e2dc64c2fbb      1
    21 0xe21de01cdcf9186a4d223818ebec987335a3b49e      1
    22 0xf824ef230b0f7fc9038f9fdbc249717419219e77      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 41 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x073d67c28d909a18555a4a860df3f309ba5472c7      1
     2 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     3 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     4 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     5 0x1cbb87bb9a4dd0316189eede2277a58590dc124a      1
     6 0x229c098df6ad81894378456efaa1ee20652a77ef      1
     7 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     8 0x3b2887ae95e50f22e9a1b225fff308503aed48e7      1
     9 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
    10 0x3deed956b999b83361b85bff31d388c35125411d      1
    11 0x3f291388477ab6be9968b2db0aee48fb0e2154e1      1
    12 0x478087e12db15302a364c64cdb79f14ae6c5c9b7      1
    13 0x478bb542f7658d635abba67edb987806dff5b83d      1
    14 0x49ffe4429610efa450b0654ea9a20e0eb4960920      1
    15 0x4da792b5058f59162e1b619749a0ce4e984d4841      1
    16 0x55c0c8186541868c03f434e9606e112ee0153965      1
    17 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
    18 0x5ce9ad759e41bf1b3dfc1a41db940a90d7a43460      1
    19 0x5e6efe126f017901a2ed55954c5b54dd7d4e6bdb      1
    20 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    21 0x6db40b1656a020e6dfbe1ff36a16d8742954acd8      1
    22 0x72915ad3110eb31768a562f540ac1ebcd51d3dc8      1
    23 0x77039399801f462b4ed13444a266b16355c471bf      1
    24 0x782adafbf47a604f146af4a059908e946eae539f      1
    25 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    26 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    27 0x80ce7206da1ed2804d645a91705212ed742f285d      1
    28 0x85a19dd2ad0d1d2b25bb164810fad08cdc0b33d7      1
    29 0x8b51c1ba09ee33e7649cac62ccb6d0f410f5647a      1
    30 0x95ecac09c1cd59e8a6d492bd06774b7b98f17be7      1
    31 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    32 0x9f08b507410725ab74ed3d61ed471235f1ee7d16      1
    33 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    34 0xaa5429e85fb9c8d39e835a533091642676be83d1      1
    35 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
    36 0xb4ef3e6c45e39ab6b8d39bb935e57c68f4d19a0b      1
    37 0xd3fac37e04a928f581459ea81d7b1e2dc64c2fbb      1
    38 0xe21de01cdcf9186a4d223818ebec987335a3b49e      1
    39 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    40 0xf824ef230b0f7fc9038f9fdbc249717419219e77      1
    41 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

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
