
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:167         Length:167         Min.   : 1.000   Length:167        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.419                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:167        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20394269 # https://etherscan.io/block/20394269
block_hash <- "0xbc21836273c388066b3789e4cb91d9790a0d11ae3e01c6924a66fadcc317ddd4"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4600 

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

allow_artist1    <- pick(snapshot, contracts=c("Doodverse","Doodverse2","Doodverse3","Foundation"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ADragonsTaleEditions","AnxietyEditions","TreasuresEditions","DoodverseEverydayEditions","TheHiddenWallsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Doodverse","Doodverse2","Doodverse3","Foundation","ADragonsTaleEditions","AnxietyEditions","TreasuresEditions","DoodverseEverydayEditions","TheHiddenWallsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x013f415e9456d90b9d4d931d1dcdc4576b392f24      1
     2 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     3 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
     4 0x7e2f447bb8bd3a96a247711e2e0d09777c876e47      1
     5 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     6 0x8888888888e9997e64793849389a8faf5e8e547c      1
     7 0x94e8b6295be681a2c4ddad11f501e0fe9abb758a      1
     8 0x951038bb372d16180c0afb0f46ab283059154004      1
     9 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    10 0xfb05a32f8fd69c4bba55c6e29c2c69b29cd2164d      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 83 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01a18b4adbc7a0e19fcfbdf63e85ec6f3c9ce9e7      1
     2 0x0235fb40f10b5ea49a9fb7660a8af76787c0d394      1
     3 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
     4 0x0c33a078c3fd0880bc945a5b7f92344cf9208eee      1
     5 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     6 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     7 0x11fbddc4e34c6bcf0df96d6aecd01d52e252098c      1
     8 0x12c599851cc522f466beb4ffb578025663cd2e8b      1
     9 0x16454b24c8fc7f19300e29d4ba2af8d6336ced1f      1
    10 0x16a8a5908402402da6f2a43ee878074ac2e84886      1
    11 0x223bb5ee1c1142d8a288e10d80302a127243450e      1
    12 0x22cf7cb48c74b07e1c0dcab5c047c5ed73292805      1
    13 0x283dfaa16e979c0fa4944c7ed145204a3cf4d74b      1
    14 0x2a109a1bcc0da13ed8fbf3288793b415320faa8b      1
    15 0x2a66c5a57c3d8b6ad6546312a7e7f0160cfbd427      1
    16 0x3055909429f637041b3e23d3fa357f98bd7ab418      1
    17 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
    18 0x3ac9e19507b2bebd1ea1d29f51c5ea36d221e780      1
    19 0x3ace4279442c0aa7aa692e58d6e7d1f72f99f5d2      1
    20 0x3bbbcbc1c0b584f61ed746096e7b67c6bc2dac93      1
    21 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    22 0x465e2a5228ddd5c1ca8e4edfc8ab676b07e0ed92      1
    23 0x4b9acf924eafd2e6d87c20626964df5212b9b10c      1
    24 0x4d0838582cab6aa41cc2fb5e65f87f2958dae620      1
    25 0x4d2fb20b25e39cc6db1e673acbb8c6c467b7c594      1
    26 0x51a7e0801baa24428f05722c43bd1bd9fe55f25c      1
    27 0x54259b3ce08a581f7023eeda4096ee8162f11871      1
    28 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
    29 0x55f572eb1e468609d28ae45105a8ec6dbc297766      1
    30 0x56bbf8bfe4a7b9337206271b099033de47ec5629      1
    31 0x5adb20ffa14bd285bd278fbf85ac8f2f3e90330b      1
    32 0x5c1500662d910ff8533c877ae6b90b012ca06d14      1
    33 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
    34 0x5f3badfd6e15322d9d86af95a62e21d769fe1a3f      1
    35 0x6008d8a4f8f381d77c6bf3d78896c32a9b5416c0      1
    36 0x61e9948a87c865f135a9c70ac6a8e5ec5c501040      1
    37 0x6d662328199040d2c1fa8e80f52b658df126e79e      1
    38 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
    39 0x6e8b01d40f5dbeb4e230f08d31427c5b5d59e315      1
    40 0x706f3d4d685560c931be933bcab6a7ffe94ff395      1
    41 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
    42 0x761ef130fbae97ac7a2cf252a41b3015347a55d3      1
    43 0x789ccf95ad3eef429d9a68809a7cc40893e5757b      1
    44 0x7e79701e4aef422fae81f24347ccec6865340316      1
    45 0x8218a2445679e38f358e42f88fe2125c98440d59      1
    46 0x829a43ec6f297b67d86385b983dae104e500f20e      1
    47 0x878578b7b9d96b83aafb1a142ac0d2ea05231003      1
    48 0x8889ebb11295f456541901f50bcb5f382047caac      1
    49 0x8a74bd0aba7e8791336e4427cb9a937177ef8bed      1
    50 0x8c475c40ff0cca6cb6c9329115024524b763709a      1
    51 0x8cd30f89910f39f373f4b337d1a3a44692c2fc9b      1
    52 0x8d54fbd16397e2b6ad91ea4df76d51427a01a3d2      1
    53 0x8f4f597e38e93084ecdc60878fdae3767836a91e      1
    54 0x91f95b32d4d07c80ea796a184745f63d92f743ef      1
    55 0x920b95f5ac9ca368ea2d2011e5fd31661fe9bb46      1
    56 0x9254fad610d5af7c2af34532f41dd8c0f9c6871a      1
    57 0x97bb06cc59ab514ada616c47d3fa25b98a5e17d6      1
    58 0x9899bf2c97ac8600bf692bfb61c49283150b49b0      1
    59 0x99c38041469db097b63335d214dafa360b1b5b2a      1
    60 0x9be3ebd8246dcf239184b6594a244a5522fcbd92      1
    61 0x9d0ba0398d5fe82fdf6675eee6eaa57439c94ce5      1
    62 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    63 0xb12dc33b17a83e73e6c350fb1f74de917ce02cc7      1
    64 0xb567e389bcf9d6f8a03d00404c7326e18400da33      1
    65 0xbd8fd0fd2ab4e06f148e58bdfa4e0ca7d1b28893      1
    66 0xc1fd5d565b22a8795d2873f1cbe181c09d5e6c9e      1
    67 0xc6aef9394574777c237fc10bb122589d36d13dc7      1
    68 0xcab4161ae91900b9cbd1a3c643a84dcb66f241bd      1
    69 0xcf08d44e41e9419f2b1b8012001102645601127a      1
    70 0xd085a1ca1b59ecabbe0c30d8cf5cbaf4f72b9c66      1
    71 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    72 0xdaa1131400bb96fad912bdf5d329d43f55c029bb      1
    73 0xdb760099f0f3359131e08716848f23c014d35b9e      1
    74 0xdbbce16edee36909115d374a886ae0cd6be56eb6      1
    75 0xdcfd871740f830bca00846e02e708e0d63bfd46a      1
    76 0xe0e8c1d735698060477e79a8e4c20276fc2ec7a7      1
    77 0xe5204d838473e1cff76d543d57591135bbb3a711      1
    78 0xe78635dcfb0172f987d2236e0dce74cc36970f4e      1
    79 0xeda4aeabae2d559e7c5aed03cc288bbf44a03134      1
    80 0xefaf1fa1362a23729cb879ab475e923c651bd532      1
    81 0xf1d8f13fd137669ac768edf467f358200fafba72      1
    82 0xf2021c8db8d41b0daf73189c477387fd7bcc1340      1
    83 0xff839b277ac4a40e458caff575dc24e129298cd1      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 93 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x013f415e9456d90b9d4d931d1dcdc4576b392f24      1
     2 0x01a18b4adbc7a0e19fcfbdf63e85ec6f3c9ce9e7      1
     3 0x0235fb40f10b5ea49a9fb7660a8af76787c0d394      1
     4 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
     5 0x0c33a078c3fd0880bc945a5b7f92344cf9208eee      1
     6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     7 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     8 0x11fbddc4e34c6bcf0df96d6aecd01d52e252098c      1
     9 0x12c599851cc522f466beb4ffb578025663cd2e8b      1
    10 0x16454b24c8fc7f19300e29d4ba2af8d6336ced1f      1
    11 0x16a8a5908402402da6f2a43ee878074ac2e84886      1
    12 0x223bb5ee1c1142d8a288e10d80302a127243450e      1
    13 0x22cf7cb48c74b07e1c0dcab5c047c5ed73292805      1
    14 0x283dfaa16e979c0fa4944c7ed145204a3cf4d74b      1
    15 0x2a109a1bcc0da13ed8fbf3288793b415320faa8b      1
    16 0x2a66c5a57c3d8b6ad6546312a7e7f0160cfbd427      1
    17 0x3055909429f637041b3e23d3fa357f98bd7ab418      1
    18 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    19 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
    20 0x3ac9e19507b2bebd1ea1d29f51c5ea36d221e780      1
    21 0x3ace4279442c0aa7aa692e58d6e7d1f72f99f5d2      1
    22 0x3bbbcbc1c0b584f61ed746096e7b67c6bc2dac93      1
    23 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    24 0x465e2a5228ddd5c1ca8e4edfc8ab676b07e0ed92      1
    25 0x4b9acf924eafd2e6d87c20626964df5212b9b10c      1
    26 0x4d0838582cab6aa41cc2fb5e65f87f2958dae620      1
    27 0x4d2fb20b25e39cc6db1e673acbb8c6c467b7c594      1
    28 0x51a7e0801baa24428f05722c43bd1bd9fe55f25c      1
    29 0x54259b3ce08a581f7023eeda4096ee8162f11871      1
    30 0x54966b7b70eba0a6bd5f5ab5e292f266e2f89c89      1
    31 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
    32 0x55f572eb1e468609d28ae45105a8ec6dbc297766      1
    33 0x56bbf8bfe4a7b9337206271b099033de47ec5629      1
    34 0x5adb20ffa14bd285bd278fbf85ac8f2f3e90330b      1
    35 0x5c1500662d910ff8533c877ae6b90b012ca06d14      1
    36 0x5e451b2d167c2a8faf0a8b23cff27996a28f28bc      1
    37 0x5f3badfd6e15322d9d86af95a62e21d769fe1a3f      1
    38 0x6008d8a4f8f381d77c6bf3d78896c32a9b5416c0      1
    39 0x61e9948a87c865f135a9c70ac6a8e5ec5c501040      1
    40 0x6d662328199040d2c1fa8e80f52b658df126e79e      1
    41 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
    42 0x6e8b01d40f5dbeb4e230f08d31427c5b5d59e315      1
    43 0x706f3d4d685560c931be933bcab6a7ffe94ff395      1
    44 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
    45 0x761ef130fbae97ac7a2cf252a41b3015347a55d3      1
    46 0x789ccf95ad3eef429d9a68809a7cc40893e5757b      1
    47 0x7e2f447bb8bd3a96a247711e2e0d09777c876e47      1
    48 0x7e79701e4aef422fae81f24347ccec6865340316      1
    49 0x8218a2445679e38f358e42f88fe2125c98440d59      1
    50 0x829a43ec6f297b67d86385b983dae104e500f20e      1
    51 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    52 0x878578b7b9d96b83aafb1a142ac0d2ea05231003      1
    53 0x8888888888e9997e64793849389a8faf5e8e547c      1
    54 0x8889ebb11295f456541901f50bcb5f382047caac      1
    55 0x8a74bd0aba7e8791336e4427cb9a937177ef8bed      1
    56 0x8c475c40ff0cca6cb6c9329115024524b763709a      1
    57 0x8cd30f89910f39f373f4b337d1a3a44692c2fc9b      1
    58 0x8d54fbd16397e2b6ad91ea4df76d51427a01a3d2      1
    59 0x8f4f597e38e93084ecdc60878fdae3767836a91e      1
    60 0x91f95b32d4d07c80ea796a184745f63d92f743ef      1
    61 0x920b95f5ac9ca368ea2d2011e5fd31661fe9bb46      1
    62 0x9254fad610d5af7c2af34532f41dd8c0f9c6871a      1
    63 0x94e8b6295be681a2c4ddad11f501e0fe9abb758a      1
    64 0x951038bb372d16180c0afb0f46ab283059154004      1
    65 0x97bb06cc59ab514ada616c47d3fa25b98a5e17d6      1
    66 0x9899bf2c97ac8600bf692bfb61c49283150b49b0      1
    67 0x99c38041469db097b63335d214dafa360b1b5b2a      1
    68 0x9be3ebd8246dcf239184b6594a244a5522fcbd92      1
    69 0x9d0ba0398d5fe82fdf6675eee6eaa57439c94ce5      1
    70 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    71 0xb12dc33b17a83e73e6c350fb1f74de917ce02cc7      1
    72 0xb567e389bcf9d6f8a03d00404c7326e18400da33      1
    73 0xbd8fd0fd2ab4e06f148e58bdfa4e0ca7d1b28893      1
    74 0xc1fd5d565b22a8795d2873f1cbe181c09d5e6c9e      1
    75 0xc6aef9394574777c237fc10bb122589d36d13dc7      1
    76 0xcab4161ae91900b9cbd1a3c643a84dcb66f241bd      1
    77 0xcf08d44e41e9419f2b1b8012001102645601127a      1
    78 0xd085a1ca1b59ecabbe0c30d8cf5cbaf4f72b9c66      1
    79 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    80 0xdaa1131400bb96fad912bdf5d329d43f55c029bb      1
    81 0xdb760099f0f3359131e08716848f23c014d35b9e      1
    82 0xdbbce16edee36909115d374a886ae0cd6be56eb6      1
    83 0xdcfd871740f830bca00846e02e708e0d63bfd46a      1
    84 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    85 0xe0e8c1d735698060477e79a8e4c20276fc2ec7a7      1
    86 0xe5204d838473e1cff76d543d57591135bbb3a711      1
    87 0xe78635dcfb0172f987d2236e0dce74cc36970f4e      1
    88 0xeda4aeabae2d559e7c5aed03cc288bbf44a03134      1
    89 0xefaf1fa1362a23729cb879ab475e923c651bd532      1
    90 0xf1d8f13fd137669ac768edf467f358200fafba72      1
    91 0xf2021c8db8d41b0daf73189c477387fd7bcc1340      1
    92 0xfb05a32f8fd69c4bba55c6e29c2c69b29cd2164d      1
    93 0xff839b277ac4a40e458caff575dc24e129298cd1      1

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
