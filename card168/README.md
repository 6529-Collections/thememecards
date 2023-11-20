
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:70          Length:70          Min.   :1.000   Length:70         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.029                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:70         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18596969 # https://etherscan.io/block/18596969
block_hash <- "0x5f8b821141cf306d58df4da16bea0e79a029d651a87e5772c4f3bcff705f6cfe"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4805 

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
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","LiBoar","Foundation","LiBoarV2","SaturatedExplorations","Airdrop","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LiEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","LiBoar","Foundation","LiBoarV2","SaturatedExplorations","Airdrop","MakersPlace","LiEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     3 0x0da6df9f5710cb24de6cf018e4294d8b82dbaa02      1
     4 0x19c2f1d50fcff3ea6b29f991c9098a336dd49a87      1
     5 0x3deed956b999b83361b85bff31d388c35125411d      1
     6 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     7 0x4f37c66fa4cb8fc9b7348b4b9b40f2d086492c8f      1
     8 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
     9 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    10 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
    11 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    12 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    13 0x7ec8fab1d7d5c8dfdf7a63babc6e538a09cefe86      1
    14 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    15 0x8888888888e9997e64793849389a8faf5e8e547c      1
    16 0xbb9968926fd27ce2c4f4c57e4e4a5d7eb3f5c477      1
    17 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    18 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    19 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    20 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    21 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    22 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 32 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0b6eef6f251263ff0fc6f4a2f4aff72de8c9c278      1
     2 0x0dccafbf01442ac219ba721e02a1b01352873b52      1
     3 0x0dec2c1f5825755d90748792392101b26cc6046a      1
     4 0x1184fa9a6f73a5789c0f0d0618c31e0fe7ef459b      1
     5 0x1643635228eda7c2c82f05b520fe755eabaed547      1
     6 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     7 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
     8 0x279a3fdbd6d6252afc5c422439c7d7859a51a05e      1
     9 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    10 0x3d7e3fca7123e687601da8631fe0922a1999a3a7      1
    11 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    12 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
    13 0x4661e47f96883c3e3bddd8fdd8dd56a14648738f      1
    14 0x5a884c92f55d74f9c7eab17613a92ab4dd8ff421      1
    15 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    16 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
    17 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    18 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    19 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    20 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    21 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
    22 0x8fe6d26da24815749aab914d37e5a2dc756111de      1
    23 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    24 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    25 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    26 0xafbd69a1e9c961e1bdfcb132d6045d671e08c800      1
    27 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    28 0xc1ce02bb775da439d108afde6956d3989eda3d80      1
    29 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    30 0xdb2b36c24064a0115fa0474fde4d9ba1f1099122      1
    31 0xe63fa6524fa2d252cc3b46fdb4839900bfbfbb49      1
    32 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 54 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     3 0x0b6eef6f251263ff0fc6f4a2f4aff72de8c9c278      1
     4 0x0da6df9f5710cb24de6cf018e4294d8b82dbaa02      1
     5 0x0dccafbf01442ac219ba721e02a1b01352873b52      1
     6 0x0dec2c1f5825755d90748792392101b26cc6046a      1
     7 0x1184fa9a6f73a5789c0f0d0618c31e0fe7ef459b      1
     8 0x1643635228eda7c2c82f05b520fe755eabaed547      1
     9 0x19c2f1d50fcff3ea6b29f991c9098a336dd49a87      1
    10 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
    11 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
    12 0x279a3fdbd6d6252afc5c422439c7d7859a51a05e      1
    13 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    14 0x3d7e3fca7123e687601da8631fe0922a1999a3a7      1
    15 0x3deed956b999b83361b85bff31d388c35125411d      1
    16 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    17 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
    18 0x4661e47f96883c3e3bddd8fdd8dd56a14648738f      1
    19 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
    20 0x4f37c66fa4cb8fc9b7348b4b9b40f2d086492c8f      1
    21 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
    22 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    23 0x5a884c92f55d74f9c7eab17613a92ab4dd8ff421      1
    24 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    25 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
    26 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    27 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
    28 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    29 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    30 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    31 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    32 0x7ec8fab1d7d5c8dfdf7a63babc6e538a09cefe86      1
    33 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    34 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    35 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
    36 0x8888888888e9997e64793849389a8faf5e8e547c      1
    37 0x8fe6d26da24815749aab914d37e5a2dc756111de      1
    38 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    39 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    40 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    41 0xafbd69a1e9c961e1bdfcb132d6045d671e08c800      1
    42 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    43 0xbb9968926fd27ce2c4f4c57e4e4a5d7eb3f5c477      1
    44 0xc1ce02bb775da439d108afde6956d3989eda3d80      1
    45 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    46 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    47 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    48 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    49 0xdb2b36c24064a0115fa0474fde4d9ba1f1099122      1
    50 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    51 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    52 0xe63fa6524fa2d252cc3b46fdb4839900bfbfbb49      1
    53 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    54 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1

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
