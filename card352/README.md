
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:1230        Length:1230        Min.   : 1.000   Length:1230       
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.235                     
                                           3rd Qu.: 1.000                     
                                           Max.   :20.000                     
         name          
     Length:1230       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22296969 # https://etherscan.io/block/22296969
block_hash <- "0x6a55a0caddb181e1dd8511370409f3b1e6e30afa8b1d2ace40efba73a53784cc"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4853 

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


allow_artist_all <- pick(snapshot, contracts=c("SuperRare"), address_remove=address_remove,address_max=1)
```

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 37 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x000f59bfbcdfff89ae5bb910e984ffaebcf652cc      1
     2 0x09c080cc0015223023f6a7ffb4d22680573934d9      1
     3 0x0ae509708e0847da0133bc779896b8e1414ad121      1
     4 0x107752288b215467de25c16119787d715ec8e26e      1
     5 0x111d5d8bf4e11dfb1667878d2f857946deef9808      1
     6 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     7 0x22cf7cb48c74b07e1c0dcab5c047c5ed73292805      1
     8 0x23570363ff61804f43dc902ac49d8e05ce7566bc      1
     9 0x2408cb9d83d3cc41b24b2e5295268d235010b713      1
    10 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
    11 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    12 0x43e9783152424bb3b8f832509b657f9e7542e111      1
    13 0x4949b46633b810bdd745b028062b30f6b647ec60      1
    14 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
    15 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    16 0x6a78534269e5c01f28afe401fe26a022b42e0d43      1
    17 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
    18 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    19 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    20 0x7d88f221e9ecef9eef118e0d0e8a47f81b37e0b6      1
    21 0x7dc1edfbb5ee7d7c8f1a53b59dcb6e6901928401      1
    22 0x8a09ed6a990324d26fd4070d6c81b2bbd3288cfd      1
    23 0x9237c2ec55dd857536a1e5bd57ed020afa71f814      1
    24 0x9f128b52128027dd6ea9892f741519979d36fa34      1
    25 0xa218e658f1f863147caa781610170f6b5f48de21      1
    26 0xa3c1cd90099dc4a1607263836483655aba62b2b3      1
    27 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
    28 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    29 0xbd72d021d3cb334deb3151db905ee073b8eee518      1
    30 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    31 0xc77c58dff4820b93c69b8c7342a3e3965c80dc95      1
    32 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    33 0xf8a884cd9bd31094a49a13659b5d8e945242d633      1
    34 0xf95752fd023fd8802abdd9cbe8e9965f623f8a84      1
    35 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    36 0xfbc1c78ea33496aa0d4e33f430e3def42014195f      1
    37 0xfdcd5aa8db191e1ed969369650cf481b1fc0bf53      1

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
