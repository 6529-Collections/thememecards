
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:68          Length:68          Min.   :1   Length:68         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:68         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19644269 # https://etherscan.io/block/19644269
block_hash <- "0x7194da7d15e3118147a79f90bf63edca564124f9275eb8468e5c41820f71704d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4428 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","ScarletSerenity","AbstractAerials","BONES","WorldOfSaltLakes","Foundation","Exclusives","AMomentForeverEditions","AMomentForever"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 43 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x000ab07b26c48ef3caf9ec23520d86794c9fd74a      1
     2 0x007f44362400de9f364efa919ff84c6ea9e210d5      1
     3 0x01e40c0e12f9bb7457eba8279197f14fa796fcf5      1
     4 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
     5 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     6 0x23324ed44904260fe555b18e5ba95c6030b9227d      1
     7 0x2985b2c952da8840cfb0b40fa44203b9f5c17cf1      1
     8 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
     9 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
    10 0x39f52180e71dbbae7e1b03cbb557f2ac3e7ac8df      1
    11 0x3bc1b52d90fcd503031ca39f718474f3b42b2200      1
    12 0x3c8cd5597ac98cb0871db580d3c9a86a384b9106      1
    13 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    14 0x5c5b6cade3f45fca78aac5a9877f1b73c51300e3      1
    15 0x5da31e46961ce6eb4c95c4deabbcf717086db167      1
    16 0x64f7e4e59a5614411a1a1675de29874a923bb3ee      1
    17 0x67a84046112f4bda0bf5c787671141650f10304e      1
    18 0x6a7b7a3b2993242fd0f2a66caac3666525acf1ba      1
    19 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    20 0x6b93809e145d2301fb21d955e4241444f784dcd9      1
    21 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    22 0x7126a97fe29f2aa6154fb3f462e42633488ecb35      1
    23 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    24 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    25 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
    26 0x96951beae8a3c191800877cdde67dc12a5881584      1
    27 0x970324b1ea88c20a2f7e6c557409cd91222409b6      1
    28 0x99a97f93e9d4960f8c846e0c66b57517fc35f950      1
    29 0x9f5cf0b16d38f1aba27e68d3c0ce34c65c2e3663      1
    30 0xbe2aaec782c989aa11f87a66c9fabb913efb24e0      1
    31 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    32 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    33 0xc6b947791f61316ac434cde35b43b9dad306a39d      1
    34 0xcbb0403736b26c1180808ade89d9ac4e3589ac7d      1
    35 0xcf4d76f0d3b32f6126ae60ab0c0fd2a02779c213      1
    36 0xdc220de7655c7c49f5b486884ca11076bf82b10f      1
    37 0xde67c92c7281de52097880412ea2dc2f85e578a6      1
    38 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    39 0xe220c41e5f8d54e1b85188912c099080db86ef38      1
    40 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    41 0xe99b70874dd7785e7d88b0a629afe80ac1e19634      1
    42 0xf17e8d8ec75b02033ea7c686ed13b9df2a2d70a5      1
    43 0xfbda58bcaafe5242f3f4aedde42dc2966d4d5b48      1

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
