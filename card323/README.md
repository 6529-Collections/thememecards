
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:95          Length:95          Min.   :1.000   Length:95         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.063                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:95         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21700669 # https://etherscan.io/block/21700669
block_hash <- "0xc05ce77ad4d40cc9832368d87ed7caaee8f34e052de4cdcc32fc7715b39fad83"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4951 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","RachaelPeaseArt","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("EntwinedEditions","OriginEditions","MemeFriendsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","RachaelPeaseArt","MakersPlace","EntwinedEditions","OriginEditions","MemeFriendsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 3 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    2 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    3 0x9a3776d6c23321c5ac93963a0e92baafb4a1afdd      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 84 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0055cd5f017027d10adf4f13332181e6d8d886bb      1
     2 0x00bc5b61042fa8375dba651a2309bc5595302ec1      1
     3 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     4 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
     5 0x0871deb34bfd2052b1c10dc4f6c0912a2a47e927      1
     6 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     7 0x109632e956ca6baf528fe5a9724c43b0c4a63fbe      1
     8 0x1136bb7b66b2c7ebb8762d49f2b7000e971fb667      1
     9 0x1352b3af249ecbb4e6f319c2d3da7d76e5ca271d      1
    10 0x14a44d92c24bce9c05a7650ec5df15e61f01121b      1
    11 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
    12 0x1fcd249d9bbd4406801f4d9151e42d22dace85ae      1
    13 0x2b564248d8f4fd425f3d01a1c36817deaac159be      1
    14 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    15 0x326ef9fa575a92090d8dea0b1f053afca64fb19b      1
    16 0x33a8003b240416a02e51a2fd908c18929b207cf0      1
    17 0x33be64b5921955387ddb08a31c3193461612b565      1
    18 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    19 0x33ed9bbfb2d2f033d250b4acb8edeb25dc108e2f      1
    20 0x3837bed4055a7e0d9b1f180f038786e2e6055d8b      1
    21 0x383f7adecd735684563af9c2a8e2f5c79808fc83      1
    22 0x3bd5c757dc99692fea4b909b773e708df8947938      1
    23 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
    24 0x3d742f9d9592d38b23c2388dc4ddbbaea5f97c36      1
    25 0x3f5f847ad8ae39559ca41e4d6aa1524f198d9f2c      1
    26 0x43e82c94d0a445c1d90506ce5cba247abdcebdd8      1
    27 0x4461e1ab21e92161994cc37b984276c2ae1e8b2e      1
    28 0x480432248e76a7ef06ae12101aa40c218be95509      1
    29 0x49909c68aecdfe6512271ecddb1adb6190c8eb2d      1
    30 0x4b3e77089c5eb6558b5355eab3bf2ee4156bf2a9      1
    31 0x53b77b9a06dd7e11aee247957784d76484fff540      1
    32 0x54f61eb7659141e7edcc2ca6337ba1bee200ef0d      1
    33 0x5ca12f79e4d33b0bd153b40df59f6db9ee03482e      1
    34 0x5cde0ccb7e21ac91dcd4eb4b73fc0ac32612b453      1
    35 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
    36 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
    37 0x628a4ff5077f8b083d56509f761c67bc7c85b754      1
    38 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
    39 0x6763a93ec3ca8b1cb33ce9d34e124cba4126bea2      1
    40 0x6f10073d9154967b209f5c1cf578e42cd7a51b70      1
    41 0x72915ad3110eb31768a562f540ac1ebcd51d3dc8      1
    42 0x72df6d34240a5284945eb407bf150705f92e15df      1
    43 0x73be99a9feb67c2789657f324bf807e9d37a39f8      1
    44 0x75718ad4bb01606c57cf51e9d123dae5aecc8de4      1
    45 0x7be2efaa87769f6a07c94cb3999662f83150ed9c      1
    46 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    47 0x82463aa8cdfe9b8acae145f8127d1b70fc9ecd39      1
    48 0x861f27bed864358520c74678561a5d30da986c26      1
    49 0x8a2395d6f36d67db54700611741cf14d31fe42ba      1
    50 0x8ee3fc743653bc3854a71fb06505ba4c988a3148      1
    51 0x8efd9addd8de6a4e64664d1893dec51f8c3339e9      1
    52 0x8f53745aa8e6d80fb7a9732af888db883bbd6552      1
    53 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    54 0x951038bb372d16180c0afb0f46ab283059154004      1
    55 0x96488f9fcd0c6997c771a26d85d1f61cacfa502c      1
    56 0x987f05b5eeb8b30b02329d3888bebc3a7424e999      1
    57 0x99fcfa34ccce9c6d61783509e808a715ae67ac34      1
    58 0x9a72efab5df2ed99ce47f69e5e30a47c5fae21b5      1
    59 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    60 0xa84bd0e9646170b91c75a999332954a15e4969b3      1
    61 0xb22696918228573dd48638a5e7c8f87ab721dddd      1
    62 0xb69b1e83f7153790776f38f35f83831c9c1be01f      1
    63 0xb6c53af164112462c6ec46a626fd5083de62d250      1
    64 0xbeffddcf2e84106f77c2b60445dc257d65e19a26      1
    65 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    66 0xc13ce81feab5fc3d0fc636e1d6c0405010381e5f      1
    67 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    68 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    69 0xc6aa79a22b08c794b25dc58cb36f2250779cdf4f      1
    70 0xc8e3d3c9a6215d0f0f940fe8e50ee4f865c864c4      1
    71 0xcbb0fe555f61d23427740984325b4583a4a34c82      1
    72 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    73 0xd85936f9566ec0b410926fa5710cba5677601a18      1
    74 0xde882cac0d663f2b319c58115b38321f43b31931      1
    75 0xe254cf0d13c31725005414218923a0c2f018b0e6      1
    76 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    77 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    78 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    79 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    80 0xf8a3db410668e79e1179fd54dfec9e78269694c0      1
    81 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    82 0xf9add84e8476754273d8850f047497538ae07941      1
    83 0xf9b7cf4be6f4cde37dd1a5b75187d431d94a4fcc      1
    84 0xfaff9a8e78ba6ceaee89269b6d7a00e97d097ed0      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 87 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0055cd5f017027d10adf4f13332181e6d8d886bb      1
     2 0x00bc5b61042fa8375dba651a2309bc5595302ec1      1
     3 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     4 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
     5 0x0871deb34bfd2052b1c10dc4f6c0912a2a47e927      1
     6 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     7 0x109632e956ca6baf528fe5a9724c43b0c4a63fbe      1
     8 0x1136bb7b66b2c7ebb8762d49f2b7000e971fb667      1
     9 0x1352b3af249ecbb4e6f319c2d3da7d76e5ca271d      1
    10 0x14a44d92c24bce9c05a7650ec5df15e61f01121b      1
    11 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
    12 0x1fcd249d9bbd4406801f4d9151e42d22dace85ae      1
    13 0x2b564248d8f4fd425f3d01a1c36817deaac159be      1
    14 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    15 0x326ef9fa575a92090d8dea0b1f053afca64fb19b      1
    16 0x33a8003b240416a02e51a2fd908c18929b207cf0      1
    17 0x33be64b5921955387ddb08a31c3193461612b565      1
    18 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    19 0x33ed9bbfb2d2f033d250b4acb8edeb25dc108e2f      1
    20 0x3837bed4055a7e0d9b1f180f038786e2e6055d8b      1
    21 0x383f7adecd735684563af9c2a8e2f5c79808fc83      1
    22 0x3bd5c757dc99692fea4b909b773e708df8947938      1
    23 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
    24 0x3d742f9d9592d38b23c2388dc4ddbbaea5f97c36      1
    25 0x3f5f847ad8ae39559ca41e4d6aa1524f198d9f2c      1
    26 0x43e82c94d0a445c1d90506ce5cba247abdcebdd8      1
    27 0x4461e1ab21e92161994cc37b984276c2ae1e8b2e      1
    28 0x480432248e76a7ef06ae12101aa40c218be95509      1
    29 0x49909c68aecdfe6512271ecddb1adb6190c8eb2d      1
    30 0x4b3e77089c5eb6558b5355eab3bf2ee4156bf2a9      1
    31 0x53b77b9a06dd7e11aee247957784d76484fff540      1
    32 0x54f61eb7659141e7edcc2ca6337ba1bee200ef0d      1
    33 0x5ca12f79e4d33b0bd153b40df59f6db9ee03482e      1
    34 0x5cde0ccb7e21ac91dcd4eb4b73fc0ac32612b453      1
    35 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
    36 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
    37 0x628a4ff5077f8b083d56509f761c67bc7c85b754      1
    38 0x63923511f7b628483f9e160cc173e68a5ded11b6      1
    39 0x6763a93ec3ca8b1cb33ce9d34e124cba4126bea2      1
    40 0x6f10073d9154967b209f5c1cf578e42cd7a51b70      1
    41 0x72915ad3110eb31768a562f540ac1ebcd51d3dc8      1
    42 0x72df6d34240a5284945eb407bf150705f92e15df      1
    43 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    44 0x73be99a9feb67c2789657f324bf807e9d37a39f8      1
    45 0x74401b66f746630ccee98689b1a20e0149fbccab      1
    46 0x75718ad4bb01606c57cf51e9d123dae5aecc8de4      1
    47 0x7be2efaa87769f6a07c94cb3999662f83150ed9c      1
    48 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    49 0x82463aa8cdfe9b8acae145f8127d1b70fc9ecd39      1
    50 0x861f27bed864358520c74678561a5d30da986c26      1
    51 0x8a2395d6f36d67db54700611741cf14d31fe42ba      1
    52 0x8ee3fc743653bc3854a71fb06505ba4c988a3148      1
    53 0x8efd9addd8de6a4e64664d1893dec51f8c3339e9      1
    54 0x8f53745aa8e6d80fb7a9732af888db883bbd6552      1
    55 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    56 0x951038bb372d16180c0afb0f46ab283059154004      1
    57 0x96488f9fcd0c6997c771a26d85d1f61cacfa502c      1
    58 0x987f05b5eeb8b30b02329d3888bebc3a7424e999      1
    59 0x99fcfa34ccce9c6d61783509e808a715ae67ac34      1
    60 0x9a3776d6c23321c5ac93963a0e92baafb4a1afdd      1
    61 0x9a72efab5df2ed99ce47f69e5e30a47c5fae21b5      1
    62 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    63 0xa84bd0e9646170b91c75a999332954a15e4969b3      1
    64 0xb22696918228573dd48638a5e7c8f87ab721dddd      1
    65 0xb69b1e83f7153790776f38f35f83831c9c1be01f      1
    66 0xb6c53af164112462c6ec46a626fd5083de62d250      1
    67 0xbeffddcf2e84106f77c2b60445dc257d65e19a26      1
    68 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    69 0xc13ce81feab5fc3d0fc636e1d6c0405010381e5f      1
    70 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    71 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    72 0xc6aa79a22b08c794b25dc58cb36f2250779cdf4f      1
    73 0xc8e3d3c9a6215d0f0f940fe8e50ee4f865c864c4      1
    74 0xcbb0fe555f61d23427740984325b4583a4a34c82      1
    75 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    76 0xd85936f9566ec0b410926fa5710cba5677601a18      1
    77 0xde882cac0d663f2b319c58115b38321f43b31931      1
    78 0xe254cf0d13c31725005414218923a0c2f018b0e6      1
    79 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    80 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    81 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    82 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    83 0xf8a3db410668e79e1179fd54dfec9e78269694c0      1
    84 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    85 0xf9add84e8476754273d8850f047497538ae07941      1
    86 0xf9b7cf4be6f4cde37dd1a5b75187d431d94a4fcc      1
    87 0xfaff9a8e78ba6ceaee89269b6d7a00e97d097ed0      1

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
