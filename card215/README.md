
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:65          Length:65          Min.   :1.000   Length:65         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.015                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:65         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19545569 # https://etherscan.io/block/19545569
block_hash <- "0x5ef163f5c5b126d39c5ac77b9005424c6ead7802f3dcae6918e64d3a009bc943"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4651 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","RNDMFRKS","SURREALWORLDS","NIGHTATMOSPHERES"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LEVITATEEditions","LEVITATEMotionEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","RNDMFRKS","SURREALWORLDS","NIGHTATMOSPHERES","LEVITATEEditions","LEVITATEMotionEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 26 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
     3 0x17bf4672860289adbc797130a4bfc71a5a92072b      1
     4 0x1d28a703ab06bcef8666e4692fe63188b6fcbca2      1
     5 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     6 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
     7 0x37416906c8011358dab16f0d73beebf580d4afa8      1
     8 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     9 0x5d47cad09c1131893652a6165288942d6a840772      1
    10 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    11 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    12 0x64a8dda827deb895a36667d01063f2ab840e46b2      1
    13 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
    14 0x91cb7aae3905c8de09c88592702c8c313d4e2109      1
    15 0xab7473da0af4658ae2879cb641d82e7652721486      1
    16 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    17 0xc449f005667bef849261b35accf931a4bace48fb      1
    18 0xc876b346d50c4199458ee0e3754883f62fe3a5b0      1
    19 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    20 0xcf54b70bf3cec789dea3b7b2a01d3147cd6cd0ef      1
    21 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    22 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    23 0xee01560234f8fa4fdc909e247393bf2d502cdc22      1
    24 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    25 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    26 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 24 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x000a3a10859ed467e7ad40963cbc12a0450bb761      1
     2 0x03ee832367e29a5cd001f65093283eabb5382b62      1
     3 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     4 0x0dfe9ec2f57c3b95653f47da96b49861e4010b36      1
     5 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     6 0x1fa4e96db8ec57e826f5e8251c6a3fb327868226      1
     7 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     8 0x28efaf6cab2b5b848778647c95bf256ce3ba3b8d      1
     9 0x2d6b14f0aafc341d282a594599798e5c3df4f9ec      1
    10 0x3ce6c9160d1333936a94c2fe17806fa0f865575f      1
    11 0x3f85c730aa5d1fb515d1ff5b8f88e16d5574d822      1
    12 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
    13 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    14 0x8684e8c7d4ae0256372e5fd727ca2bd3f4ac95b2      1
    15 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    16 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    17 0xa6c579879252d8abb6e9150e4aa0196ba81c7b27      1
    18 0xa8c9d8691f97af5d22cf986d36faea60a50aaf38      1
    19 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    20 0xb65d28d8d260fa51ae6eeb97543ff77271ba25ee      1
    21 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    22 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
    23 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    24 0xef136cd8d9bbd4746ed6172e93c7be36b53b6741      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x000a3a10859ed467e7ad40963cbc12a0450bb761      1
     2 0x03ee832367e29a5cd001f65093283eabb5382b62      1
     3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     4 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     5 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
     6 0x0dfe9ec2f57c3b95653f47da96b49861e4010b36      1
     7 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     8 0x17bf4672860289adbc797130a4bfc71a5a92072b      1
     9 0x1d28a703ab06bcef8666e4692fe63188b6fcbca2      1
    10 0x1fa4e96db8ec57e826f5e8251c6a3fb327868226      1
    11 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
    12 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
    13 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
    14 0x28efaf6cab2b5b848778647c95bf256ce3ba3b8d      1
    15 0x2d6b14f0aafc341d282a594599798e5c3df4f9ec      1
    16 0x37416906c8011358dab16f0d73beebf580d4afa8      1
    17 0x3ce6c9160d1333936a94c2fe17806fa0f865575f      1
    18 0x3f85c730aa5d1fb515d1ff5b8f88e16d5574d822      1
    19 0x4187ddd9340689c8e4a124b7532fca0a13eb9511      1
    20 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
    21 0x5d47cad09c1131893652a6165288942d6a840772      1
    22 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    23 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    24 0x64a8dda827deb895a36667d01063f2ab840e46b2      1
    25 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    26 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
    27 0x8684e8c7d4ae0256372e5fd727ca2bd3f4ac95b2      1
    28 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    29 0x91cb7aae3905c8de09c88592702c8c313d4e2109      1
    30 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    31 0xa6c579879252d8abb6e9150e4aa0196ba81c7b27      1
    32 0xa8c9d8691f97af5d22cf986d36faea60a50aaf38      1
    33 0xab7473da0af4658ae2879cb641d82e7652721486      1
    34 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    35 0xb65d28d8d260fa51ae6eeb97543ff77271ba25ee      1
    36 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    37 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    38 0xc449f005667bef849261b35accf931a4bace48fb      1
    39 0xc578958dd1880cf00bffbb7feb9c28cbbbcad3bf      1
    40 0xc876b346d50c4199458ee0e3754883f62fe3a5b0      1
    41 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    42 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    43 0xcf54b70bf3cec789dea3b7b2a01d3147cd6cd0ef      1
    44 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    45 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    46 0xee01560234f8fa4fdc909e247393bf2d502cdc22      1
    47 0xef136cd8d9bbd4746ed6172e93c7be36b53b6741      1
    48 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    49 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    50 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1

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
