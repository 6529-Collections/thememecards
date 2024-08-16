
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:118         Length:118         Min.   :1.000   Length:118        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.059                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:118        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20495569 # https://etherscan.io/block/20495569
block_hash <- "0x61ae406d56b85b0fc03017833c0979e04df2445a6a251fefbd6347aaabe583b5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4635 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","dasOriginals","FKWR","TheLastShift"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("skyRunnerreTurnEditions","skyRunnerEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","dasOriginals","FKWR","TheLastShift","skyRunnerreTurnEditions","skyRunnerEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 11 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02081f465b8bd66058d50ef1a985d0c7af695363      1
     2 0x1022245eb550f9778209ca4514dcb693b45498be      1
     3 0x317f13cd5d21455dbc938648496a4d90049d01ca      1
     4 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     5 0x57c571d126bb2cc883691efb8e091c1827a11b2f      1
     6 0x91cb7aae3905c8de09c88592702c8c313d4e2109      1
     7 0xa4b515705e77c4569954233ee22e2204bff445f8      1
     8 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     9 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    10 0xc580950681be90e085734509b8cdcb6b16ad41b3      1
    11 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 28 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0e7a408a775af29310970de51b59501e21eee87a      1
     2 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     3 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     4 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     5 0x32f9c5f723b28c4fb37503cb0935bf5651986e69      1
     6 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     7 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
     8 0x47fe90158eb5b3a40369be6cc2e911a81cd25626      1
     9 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
    10 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
    11 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    12 0x74c7e104d699193d7cbeb8ae4b5e1174468e3aed      1
    13 0x7ad84e097ac8261fba7b4b1afe23ed84553a618d      1
    14 0x7cfbfda5525508d462eb8523dd1f5d4491e048d3      1
    15 0x8d2a43ff7e015c55c2be316a52bec176a4328a9b      1
    16 0x93736e331b1ab6490fb8b77af6363bbd12bf65b9      1
    17 0x9fde21b020cd066a24c8fb919fde2fb8c33c1969      1
    18 0xae92ac902fb7937f652dcec459424b122747a86c      1
    19 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
    20 0xb7574cac36d39121c0f8ebd5770223c5292d274a      1
    21 0xbdc46fd302b0372e118b0f41e6a66411148473ee      1
    22 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    23 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    24 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    25 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    26 0xd219122e3a0a327fd028a6fd9389315d1b2ed99f      1
    27 0xd6f38b314c0e4baef7b40c328420b0005f50d992      1
    28 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 39 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02081f465b8bd66058d50ef1a985d0c7af695363      1
     2 0x0e7a408a775af29310970de51b59501e21eee87a      1
     3 0x1022245eb550f9778209ca4514dcb693b45498be      1
     4 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     5 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     6 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     7 0x317f13cd5d21455dbc938648496a4d90049d01ca      1
     8 0x32f9c5f723b28c4fb37503cb0935bf5651986e69      1
     9 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
    10 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
    11 0x47fe90158eb5b3a40369be6cc2e911a81cd25626      1
    12 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
    13 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
    14 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
    15 0x57c571d126bb2cc883691efb8e091c1827a11b2f      1
    16 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    17 0x74c7e104d699193d7cbeb8ae4b5e1174468e3aed      1
    18 0x7ad84e097ac8261fba7b4b1afe23ed84553a618d      1
    19 0x7cfbfda5525508d462eb8523dd1f5d4491e048d3      1
    20 0x8d2a43ff7e015c55c2be316a52bec176a4328a9b      1
    21 0x91cb7aae3905c8de09c88592702c8c313d4e2109      1
    22 0x93736e331b1ab6490fb8b77af6363bbd12bf65b9      1
    23 0x9fde21b020cd066a24c8fb919fde2fb8c33c1969      1
    24 0xa4b515705e77c4569954233ee22e2204bff445f8      1
    25 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    26 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    27 0xae92ac902fb7937f652dcec459424b122747a86c      1
    28 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
    29 0xb7574cac36d39121c0f8ebd5770223c5292d274a      1
    30 0xbdc46fd302b0372e118b0f41e6a66411148473ee      1
    31 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    32 0xc580950681be90e085734509b8cdcb6b16ad41b3      1
    33 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    34 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    35 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    36 0xd219122e3a0a327fd028a6fd9389315d1b2ed99f      1
    37 0xd6f38b314c0e4baef7b40c328420b0005f50d992      1
    38 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1
    39 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1

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
