
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:158         Length:158         Min.   :1.000   Length:158        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.032                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:158        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19994869 # https://etherscan.io/block/19994869
block_hash <- "0x2d2d2ea9736d19b224505261b1bb28e6310e2d49e63c77742d104b8264b28e3e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4396 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","CollectorsGifts","ParadigmStoriesPuzzle","TheRabbitHole","MakersPlace","Treasuresofatumultuossea"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ParadigmStoriesEditions","MOCABiddersClubEditions","ParafractalverseEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","CollectorsGifts","ParadigmStoriesPuzzle","TheRabbitHole","MakersPlace","Treasuresofatumultuossea","ParadigmStoriesEditions","MOCABiddersClubEditions","ParafractalverseEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 8 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x45e2c1b07cce6c0c361bb420c13fa0c6154981e9      1
    2 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
    3 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    4 0x8d7c3921ecf289a2c272529d0bd57f9c2e3f997a      1
    5 0xab7473da0af4658ae2879cb641d82e7652721486      1
    6 0xba02a5a08328fc0818ea35bef6604446cbac4c29      1
    7 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    8 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 121 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x002a3be63b3b312da5d58eb02d2b48869b46ec82      1
      2 0x011384e15a858a29f2b954f4a67ad3052ddc6d95      1
      3 0x0f13316b80872eda120d7844ed1c1e90fc001dc1      1
      4 0x107752288b215467de25c16119787d715ec8e26e      1
      5 0x11302831dac338809ae423c53e597ef0df3ea48f      1
      6 0x11ae370040f234d35fd205360a56e9af516178ce      1
      7 0x1222c55609df8b1697e235fd33edec065ffb4714      1
      8 0x13ac2c9314c262f1b79d5a9b331b625b15ef029f      1
      9 0x15c697b9c2a029ec88e7b13112bbcaf9fcbe4417      1
     10 0x18c6a47aca1c6a237e53ed2fc3a8fb392c97169b      1
     11 0x1cc1983396e55e925b777a0d70dfe8b56edee942      1
     12 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     13 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     14 0x1fd9277f03bd18f740bffbf9f160f10bd8fa9f11      1
     15 0x20744fbf4085fda538bbf0d30a5ae9c9bbf45d9c      1
     16 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     17 0x2a3f7e5170ea8ca967f85f091ef84591f639e031      1
     18 0x2be4aaa52893219ec124c8afc8168b7a6103811a      1
     19 0x2d9016f7c05c598180b411e69e1dc43a25074e32      1
     20 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     21 0x2e307ceab1b4c5f4cac508e3b13c3dbfe86a3c81      1
     22 0x2ec7641df0eff8659644c0f44199b0522e4232fb      1
     23 0x31e7e4bb3aa3a5dd3ac5e240223eb4416cffa5c3      1
     24 0x328544e8b958fcd5e0effba95dbeedee270c0209      1
     25 0x392bd6936f720056cfa331ed11f650c81447dc46      1
     26 0x3c59646d1385067f72af6b85ce5dec1d8a0540f5      1
     27 0x3c9e21167338f994a169e94764b7bb3b810c73e0      1
     28 0x3f1e2953ec44a6d73ab54e5c6a07b099e7dac7d2      1
     29 0x3fe859a56ac422ba7ef4ac9d6335681610b51a3d      1
     30 0x4124cf34f56fa151e05c91ace550ada0dd5aabd7      1
     31 0x42f3f76ba5202d7a4c48fd428b5613c657689bcc      1
     32 0x431c665d335cd1b0a68244e5e135ab758d429b0a      1
     33 0x4410bb5accef3848bc7b64131ceb3d54740682e8      1
     34 0x44e9d011df8741be2493e99297e9ad67bb1aa85b      1
     35 0x45c456e6abfa5d5492b7c6bdc4ef94e34c1c11f6      1
     36 0x460ec5732db536fdbe70fefa98d997e660779665      1
     37 0x4812fc190e06b7f81665805564e2450b4e2fd490      1
     38 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     39 0x4f85b7d80a2b6a4990687908ed3f08a6b4ca4fdc      1
     40 0x5219b866323ea879a3e34b965b776c5964433a8a      1
     41 0x53ac1d24c9b6533ab7bdeff8c11f9b4953c789b0      1
     42 0x55372173689c288552885d897d32f5f706f79aa6      1
     43 0x554150ab1eeb5ca5bb6a3a6aaa48050d3060b4f7      1
     44 0x56f8719319749f911b7c607e54804f2d94f9b93d      1
     45 0x59d2bcc2761029b058c812a32b7bf05f26078ee4      1
     46 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     47 0x5ac6b1ae54725d64f86da06c4a57ba583c68b3ac      1
     48 0x5fc64499864ab082756cc0fdfb0267d74b0dffb8      1
     49 0x60d38778adbbeeac88f741b833cbb9877228eea0      1
     50 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     51 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
     52 0x633c08816fccf8e9ee6932a56f90264109d2ebb1      1
     53 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
     54 0x69aad835cb3f62e435fc693443ce49bfe74b6dbe      1
     55 0x69cc8b11d3baf57cc5691450de6eba3e483807d2      1
     56 0x69f38c51ea3e2c48ceaca97b83276b12f369ae2a      1
     57 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
     58 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     59 0x6d42a8c31e1058e163429dba36ac4aa0d21ba0c5      1
     60 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     61 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
     62 0x6fd155b9d52f80e8a73a8a2537268602978486e2      1
     63 0x72774bc572ef9a2dff47c3f8cc200dc2fe3830c0      1
     64 0x734b5ce55f0e08ba02d1956eef689d20fbdcd4df      1
     65 0x7485ac6d8534691993348d51ab0f131a19fff763      1
     66 0x764abe778aa96cd04972444a8e1db83df13f7e66      1
     67 0x77350e1152efd5f2d807a6124015c629a907155e      1
     68 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     69 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
     70 0x7baf191cfaa4a0b08991fb728179baaf3917836a      1
     71 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     72 0x7ce438bf068c8f47f0f46cb7891fc7fd0956f117      1
     73 0x7d08c340fcfe051c462dcfda8b9ed47642b96b10      1
     74 0x7d2550161e8a31d0b9585bb9c88e63e9644af740      1
     75 0x7d85806c589d9e1301898932a32108f15c6daf14      1
     76 0x7dc4e09a175a687cbe4c2b282a356f5f63a17848      1
     77 0x81c77f8ac639955a7bfd5f25f9ab533b0d388f34      1
     78 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     79 0x8665f7ceadfbff09e2cc572f7a6e43198d87ff88      1
     80 0x8919014b0f6746407ce40670737bf8aab96f8124      1
     81 0x89cbb15c7ab08a39328b0e3ddf8cffa8d7c50899      1
     82 0x8af30b3ff1c29119ed336d45f77a6d59c3273b92      1
     83 0x8c42313ff71b7c816365ea1b546e450838000578      1
     84 0x8dc287825d71f758bab052608ba8a4f156f84176      1
     85 0x8ef9c3eb8d98c00cf1c94bdf9e85417dd536ba26      1
     86 0x922ec1109aa5b49822b72437d2d25f6ba749e585      1
     87 0x96a7e4d9796ec600e8b42cd1b0adb71fcf91390b      1
     88 0x9af481276b075e036bc23e887a8bd275e69ef74c      1
     89 0xa1acaddd259649d470b42c95738e5e89c8d8a233      1
     90 0xa238f1face573a7215443aa681e591896c0dd660      1
     91 0xa58c937e002030677bf6066b4efeeb9e76163e31      1
     92 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
     93 0xa8c045e857c1c4550119b612f22c3b27ece10340      1
     94 0xa934f1210de462c7ee2b5b78fd97b7cb7c38e95d      1
     95 0xb1c91bf26ad7d580d0ceb93f3f7659c347871555      1
     96 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
     97 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
     98 0xb73ad2223a9ad7d1e9614eb7f00dc0cb8bac4de2      1
     99 0xb76e4a9932538bbad705d2936d0db755389cacff      1
    100 0xb7f73311a823fa70059cf6e22a842c3bd64c53c3      1
    101 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    102 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    103 0xc38fe30a72813293bbb1575a654fa937596b9854      1
    104 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    105 0xcade1e68a994c5b1459ccd19150128ffef09ea3c      1
    106 0xcc87ab42fc4ace2a68bfc1e28d7ddd578273035a      1
    107 0xced2662fe30d876bef52f219eeac67e2b328effc      1
    108 0xd3c197dc53f55e0293964e4dde21a2e051b57a1b      1
    109 0xd79b812c8570508c04faba959e5fbb19230a6de2      1
    110 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    111 0xdd6cb36a0c22fdb2abc5e2ec9d96db68c29f69de      1
    112 0xe04885c3f1419c6e8495c33bdcf5f8387cd88846      1
    113 0xe2e8d1bc8cc2afce8332f5cff5a36fb966805856      1
    114 0xe59d87e5298ac2fb24c2fc55d256cb8fcb696238      1
    115 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    116 0xeda7ff0ab09da29a7eaa725e56be4898fdbcd288      1
    117 0xf1db8a2623193757317639d0532daa5e3c8ea20c      1
    118 0xf68c8baec45b250869576edb5e16bae95e8e9e16      1
    119 0xf6f48064ecce7f331494a1eab38eb4eb91202bbc      1
    120 0xf91ba1faf962b672a7ae12815afd2f432bc74186      1
    121 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 129 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x002a3be63b3b312da5d58eb02d2b48869b46ec82      1
      2 0x011384e15a858a29f2b954f4a67ad3052ddc6d95      1
      3 0x0f13316b80872eda120d7844ed1c1e90fc001dc1      1
      4 0x107752288b215467de25c16119787d715ec8e26e      1
      5 0x11302831dac338809ae423c53e597ef0df3ea48f      1
      6 0x11ae370040f234d35fd205360a56e9af516178ce      1
      7 0x1222c55609df8b1697e235fd33edec065ffb4714      1
      8 0x13ac2c9314c262f1b79d5a9b331b625b15ef029f      1
      9 0x15c697b9c2a029ec88e7b13112bbcaf9fcbe4417      1
     10 0x18c6a47aca1c6a237e53ed2fc3a8fb392c97169b      1
     11 0x1cc1983396e55e925b777a0d70dfe8b56edee942      1
     12 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     13 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     14 0x1fd9277f03bd18f740bffbf9f160f10bd8fa9f11      1
     15 0x20744fbf4085fda538bbf0d30a5ae9c9bbf45d9c      1
     16 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     17 0x2a3f7e5170ea8ca967f85f091ef84591f639e031      1
     18 0x2be4aaa52893219ec124c8afc8168b7a6103811a      1
     19 0x2d9016f7c05c598180b411e69e1dc43a25074e32      1
     20 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     21 0x2e307ceab1b4c5f4cac508e3b13c3dbfe86a3c81      1
     22 0x2ec7641df0eff8659644c0f44199b0522e4232fb      1
     23 0x31e7e4bb3aa3a5dd3ac5e240223eb4416cffa5c3      1
     24 0x328544e8b958fcd5e0effba95dbeedee270c0209      1
     25 0x392bd6936f720056cfa331ed11f650c81447dc46      1
     26 0x3c59646d1385067f72af6b85ce5dec1d8a0540f5      1
     27 0x3c9e21167338f994a169e94764b7bb3b810c73e0      1
     28 0x3f1e2953ec44a6d73ab54e5c6a07b099e7dac7d2      1
     29 0x3fe859a56ac422ba7ef4ac9d6335681610b51a3d      1
     30 0x4124cf34f56fa151e05c91ace550ada0dd5aabd7      1
     31 0x42f3f76ba5202d7a4c48fd428b5613c657689bcc      1
     32 0x431c665d335cd1b0a68244e5e135ab758d429b0a      1
     33 0x4410bb5accef3848bc7b64131ceb3d54740682e8      1
     34 0x44e9d011df8741be2493e99297e9ad67bb1aa85b      1
     35 0x45c456e6abfa5d5492b7c6bdc4ef94e34c1c11f6      1
     36 0x45e2c1b07cce6c0c361bb420c13fa0c6154981e9      1
     37 0x460ec5732db536fdbe70fefa98d997e660779665      1
     38 0x4812fc190e06b7f81665805564e2450b4e2fd490      1
     39 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     40 0x4f85b7d80a2b6a4990687908ed3f08a6b4ca4fdc      1
     41 0x5219b866323ea879a3e34b965b776c5964433a8a      1
     42 0x53ac1d24c9b6533ab7bdeff8c11f9b4953c789b0      1
     43 0x55372173689c288552885d897d32f5f706f79aa6      1
     44 0x554150ab1eeb5ca5bb6a3a6aaa48050d3060b4f7      1
     45 0x56f8719319749f911b7c607e54804f2d94f9b93d      1
     46 0x59d2bcc2761029b058c812a32b7bf05f26078ee4      1
     47 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
     48 0x5ac6b1ae54725d64f86da06c4a57ba583c68b3ac      1
     49 0x5fc64499864ab082756cc0fdfb0267d74b0dffb8      1
     50 0x60d38778adbbeeac88f741b833cbb9877228eea0      1
     51 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     52 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
     53 0x633c08816fccf8e9ee6932a56f90264109d2ebb1      1
     54 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
     55 0x69aad835cb3f62e435fc693443ce49bfe74b6dbe      1
     56 0x69cc8b11d3baf57cc5691450de6eba3e483807d2      1
     57 0x69f38c51ea3e2c48ceaca97b83276b12f369ae2a      1
     58 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
     59 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     60 0x6d42a8c31e1058e163429dba36ac4aa0d21ba0c5      1
     61 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     62 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     63 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
     64 0x6fd155b9d52f80e8a73a8a2537268602978486e2      1
     65 0x72774bc572ef9a2dff47c3f8cc200dc2fe3830c0      1
     66 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     67 0x734b5ce55f0e08ba02d1956eef689d20fbdcd4df      1
     68 0x7485ac6d8534691993348d51ab0f131a19fff763      1
     69 0x764abe778aa96cd04972444a8e1db83df13f7e66      1
     70 0x77350e1152efd5f2d807a6124015c629a907155e      1
     71 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     72 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
     73 0x7baf191cfaa4a0b08991fb728179baaf3917836a      1
     74 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     75 0x7ce438bf068c8f47f0f46cb7891fc7fd0956f117      1
     76 0x7d08c340fcfe051c462dcfda8b9ed47642b96b10      1
     77 0x7d2550161e8a31d0b9585bb9c88e63e9644af740      1
     78 0x7d85806c589d9e1301898932a32108f15c6daf14      1
     79 0x7dc4e09a175a687cbe4c2b282a356f5f63a17848      1
     80 0x81c77f8ac639955a7bfd5f25f9ab533b0d388f34      1
     81 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     82 0x8665f7ceadfbff09e2cc572f7a6e43198d87ff88      1
     83 0x8919014b0f6746407ce40670737bf8aab96f8124      1
     84 0x89cbb15c7ab08a39328b0e3ddf8cffa8d7c50899      1
     85 0x8af30b3ff1c29119ed336d45f77a6d59c3273b92      1
     86 0x8c42313ff71b7c816365ea1b546e450838000578      1
     87 0x8d7c3921ecf289a2c272529d0bd57f9c2e3f997a      1
     88 0x8dc287825d71f758bab052608ba8a4f156f84176      1
     89 0x8ef9c3eb8d98c00cf1c94bdf9e85417dd536ba26      1
     90 0x922ec1109aa5b49822b72437d2d25f6ba749e585      1
     91 0x96a7e4d9796ec600e8b42cd1b0adb71fcf91390b      1
     92 0x9af481276b075e036bc23e887a8bd275e69ef74c      1
     93 0xa1acaddd259649d470b42c95738e5e89c8d8a233      1
     94 0xa238f1face573a7215443aa681e591896c0dd660      1
     95 0xa58c937e002030677bf6066b4efeeb9e76163e31      1
     96 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
     97 0xa8c045e857c1c4550119b612f22c3b27ece10340      1
     98 0xa934f1210de462c7ee2b5b78fd97b7cb7c38e95d      1
     99 0xab7473da0af4658ae2879cb641d82e7652721486      1
    100 0xb1c91bf26ad7d580d0ceb93f3f7659c347871555      1
    101 0xb3007ff3c3f40bdf0126fec7c8e43c3fc50ea813      1
    102 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    103 0xb73ad2223a9ad7d1e9614eb7f00dc0cb8bac4de2      1
    104 0xb76e4a9932538bbad705d2936d0db755389cacff      1
    105 0xb7f73311a823fa70059cf6e22a842c3bd64c53c3      1
    106 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    107 0xba02a5a08328fc0818ea35bef6604446cbac4c29      1
    108 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    109 0xc38fe30a72813293bbb1575a654fa937596b9854      1
    110 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    111 0xcade1e68a994c5b1459ccd19150128ffef09ea3c      1
    112 0xcc87ab42fc4ace2a68bfc1e28d7ddd578273035a      1
    113 0xced2662fe30d876bef52f219eeac67e2b328effc      1
    114 0xd3c197dc53f55e0293964e4dde21a2e051b57a1b      1
    115 0xd79b812c8570508c04faba959e5fbb19230a6de2      1
    116 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    117 0xdd6cb36a0c22fdb2abc5e2ec9d96db68c29f69de      1
    118 0xe04885c3f1419c6e8495c33bdcf5f8387cd88846      1
    119 0xe2e8d1bc8cc2afce8332f5cff5a36fb966805856      1
    120 0xe59d87e5298ac2fb24c2fc55d256cb8fcb696238      1
    121 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    122 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    123 0xeda7ff0ab09da29a7eaa725e56be4898fdbcd288      1
    124 0xf1db8a2623193757317639d0532daa5e3c8ea20c      1
    125 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    126 0xf68c8baec45b250869576edb5e16bae95e8e9e16      1
    127 0xf6f48064ecce7f331494a1eab38eb4eb91202bbc      1
    128 0xf91ba1faf962b672a7ae12815afd2f432bc74186      1
    129 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1

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
