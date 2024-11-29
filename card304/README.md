
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:220         Length:220         Min.   :1.000   Length:220        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.077                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:220        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21246969 # https://etherscan.io/block/21246969
block_hash <- "0xabdd89d2b8753b380a23b2661d8f44e893d16559d12a3be8af53dcf80b2c46d0"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4699 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","PSYCHOKIDS","theartofBadoats2","theartofbadoats"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("CHACHAEditions","TOXICEditions","THEPSYCHOKIDSEDITIONS","VAINEditions","BADOATSEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","PSYCHOKIDS","theartofBadoats2","theartofbadoats","CHACHAEditions","TOXICEditions","THEPSYCHOKIDSEDITIONS","VAINEditions","BADOATSEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 16 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     2 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     3 0x1f389025797a047f25d6cea5dcd34b521ca458e7      1
     4 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     5 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
     6 0x49317b33a82fb576fba1462364d02569e766e33a      1
     7 0x4e323ee346748d7a11c020a38e4feec377779533      1
     8 0x672044e2abebaae1359737b7cc5a79071bbb1c0d      1
     9 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
    10 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    11 0xbdeb06519ceb89202e5246c748b5d8021f2a8fe8      1
    12 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    13 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    14 0xd44f069b1ecb1d70b8350807f466c43407eb887c      1
    15 0xdc6d8803798ee21d0ef101129fe34e09197ef9fa      1
    16 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 109 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02726a1b475d730135f74ef2a6b841937ef81e73      1
      2 0x03cf4fef434d94c2f88b22fc1ae7e4fa7cf96feb      1
      3 0x05acb16a4aa65ea299d8b2dad0a5f37cd4b09a73      1
      4 0x069a763f2f523ca45bc63175b57c0955f3cd53af      1
      5 0x06d74d7eda3521263e3fbd104bb106d32733713c      1
      6 0x0962835897d330a2e7a194e2da816dd776980cd2      1
      7 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
      8 0x0f4f82f5b58948e924e87edc31cdf8e4e304bb27      1
      9 0x1123e0ea9738187897ae67966f353ca4516db6ed      1
     10 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     11 0x124236d7e03761498813b00ea4e94f49804f8f87      1
     12 0x17ba032e497f6b779f46dd32e95d8d998dc2ab0d      1
     13 0x18119bf8fb02203052cc530240ef9ff377f9c823      1
     14 0x182fdb27d6aae88388ad38a2d7f0d1ac87a981dd      1
     15 0x190fb3b4118e5633820184fe9217049a88f07fcf      1
     16 0x191dbf79739f63cd998c2e4decb04f8259a2479e      1
     17 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     18 0x2662f48d4191b672cad523f54f761dfc13cbd2f1      1
     19 0x2877d3244f0f6c8e2a51e4f01719d70cc320af30      1
     20 0x291092ae084c08530a035cc844bf8ab0ebeb9e7d      1
     21 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     22 0x2c11cf38a6a5ac74ea438d0037f7f880021fc66f      1
     23 0x2c9134e49865a7a7a2f60ccb9cb31f46885a8796      1
     24 0x2d76564c2f8368cf7f5db9fd59906f8886b8e1ad      1
     25 0x2f1a2b474215a941e75db302b0c75044998d2878      1
     26 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     27 0x37d80cf9d319b2c6e06fe38464488214b962f059      1
     28 0x39bf9f9587c08f7fb80999008a13f3df13b61f08      1
     29 0x3a2ebfcd091ebedbbfac6706dd57776e1f985722      1
     30 0x42bd129e447643368ccaf951c9f6ada96bf84f1b      1
     31 0x44d5a0b1e165a17c16d940ca774a28e62d4b44b0      1
     32 0x46c91e9691ff9707f093267164e2910cbdc9af8c      1
     33 0x47a9dcf163132c8c1c271fc5d8a90a801c8c85ac      1
     34 0x4c8b46acb8ea62a154a9cfd3c446f6f4a678ebb0      1
     35 0x51930478ac95de7ea09c594684b54c0df568e912      1
     36 0x519fe04f730ba0b3f1b5fefeba5e0a2772292889      1
     37 0x54c8f64e743d00d159388197b956b787836f1fa3      1
     38 0x584456aab0110a0f1b177d8a1cde7aff6d09a292      1
     39 0x5855857fd0ee7f6c80442eb244e25cc9b9635c33      1
     40 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
     41 0x616cdc9d8852ac1d1f10e9e746e7c0c881a03957      1
     42 0x6543ff6da083cd8889e4b695d8c575a7a9c88500      1
     43 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     44 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     45 0x67580561ba59d3d59dfc854c9399d63e93c08d25      1
     46 0x6782b4af64b64c44f2cfdb9b123c4170e27d613e      1
     47 0x69091ff2fc3322fe88d7501ab0922c105ef9ac89      1
     48 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     49 0x6cb37d1f14843809e0e4c8e4ef69ea8214c8a11f      1
     50 0x6eab3ca6127d7001f0f266833c63cdb089d9885f      1
     51 0x70e5fdfdc8cf8ce717cd54e882f36804fba78921      1
     52 0x75ae3facc19d5726bbcc85f17eb93110c9f9ab08      1
     53 0x75c3db1bc80b6d566b124ee915b6fc76cbb5303c      1
     54 0x7652d37a175b96aafacf3fed7c735bead21ee594      1
     55 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
     56 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
     57 0x79dd8fab0661da2cd4131bb454bab060576ce2ee      1
     58 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
     59 0x7b70e3993d8ca1804f51b91a2a4b044d10ec8ec8      1
     60 0x7f99acc18a490163a6184d89fe2a2fc80b8fe2ba      1
     61 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     62 0x8160f95edd83e0e420b81e32937b3424fee2b926      1
     63 0x86b880d14f997b839c369ad4218fcedc60ca3091      1
     64 0x8820768caedc113ba97dd749d948ad0fd91c52b6      1
     65 0x896cd4101253ff9d2cab2d256371e5b1e41882f6      1
     66 0x8ce9f7b042fce1971b73240d16849a684a2657d5      1
     67 0x8cf5b8390a8ce9e3f5279e4a3f69f75c781c79db      1
     68 0x8d3af5908dbd28fb0c33f32b51f67c03ed2c00c8      1
     69 0x8d4b7642bd37d670b9a84212cb3cbede9bd5d875      1
     70 0x8dd698e3b14173143591a7c7a425d9bf0fa8ec2f      1
     71 0x9029dfdcb49aefc36e5112aaa0c8567d28ea5d74      1
     72 0x91a05aa62aee372200ad4ab39ee6c3f2aa96ecb0      1
     73 0x959d459fae3e8f26770afd2838b969c389915437      1
     74 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
     75 0x973ef7e4e91cc9098709d540a8ddfb708b331c87      1
     76 0x9832a8dd9a38b27c12a564e2362c581cb5f5bd2b      1
     77 0x991bcd28c326a8fa1754bd6100dfe938dad56894      1
     78 0x9939a8c8293f0a720c4843d1c6c20594ee6a9218      1
     79 0x9a14d1e7077ea070a7f86e0e0fa85899d502d32d      1
     80 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
     81 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     82 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
     83 0xa32c14849106522cd182599bc861bb90f18e6003      1
     84 0xa48b4b40fc346ff27783220b61f411cea89fa4f2      1
     85 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
     86 0xafe3bf864c426f11115b07d5899a4419504eeded      1
     87 0xb214106a52728d1e55c9327a5b8e7e8de104d7bd      1
     88 0xb310fdfd02d0a1518a8e2111cc3785eb9a09c136      1
     89 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
     90 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     91 0xbf00caee3f4d0e654b5e1a557914d257f126d055      1
     92 0xc3ae80c65a25d3e11bec83d7307273364cf68b1e      1
     93 0xc5bc9715874c91ded9313a06baefa1307bd72299      1
     94 0xc646e02557877d559e55a3984ec12654ddd84af3      1
     95 0xc746fe78b267b5cca0d1b6d3049cbcb9e45cccd6      1
     96 0xc8c0843f1fa7bdae072352f59852aa70f054067d      1
     97 0xd1fc6c2c64268a854a766196cdab99bc04bf4e44      1
     98 0xd545b77a99a62253b533934dc7ebe311915897f0      1
     99 0xe0154466c299d00925b7908138a38d5724848ec1      1
    100 0xe0ddd9b653e182d4761ec665cea7fd5243f32ce2      1
    101 0xe13ec43b554b53555aeff74c367c59636cab88e7      1
    102 0xe54c9ffd043af0add860045b367b8f02fc4c1b88      1
    103 0xeb4b8ecce912e40fa4877f1d5380a810c7b5036c      1
    104 0xeb889d3ffd7170cd1e25a3b2cb0d522b8eaa5cb7      1
    105 0xeda6bb211372aaadd2db0dabaf3c0d4ba90446aa      1
    106 0xf2384657d1cc8bdcdfed6d080f5f14a45f9c6f37      1
    107 0xf70caec95d5a8268da32ce569a15a60651e4a550      1
    108 0xfa031ecd543aae38a107fa8cb1b51e80c1e17876      1
    109 0xfe934349b512b386d8311920c3d7773e2a7485e8      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 125 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02726a1b475d730135f74ef2a6b841937ef81e73      1
      2 0x03cf4fef434d94c2f88b22fc1ae7e4fa7cf96feb      1
      3 0x05acb16a4aa65ea299d8b2dad0a5f37cd4b09a73      1
      4 0x069a763f2f523ca45bc63175b57c0955f3cd53af      1
      5 0x06d74d7eda3521263e3fbd104bb106d32733713c      1
      6 0x0962835897d330a2e7a194e2da816dd776980cd2      1
      7 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
      8 0x0f4f82f5b58948e924e87edc31cdf8e4e304bb27      1
      9 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     10 0x1123e0ea9738187897ae67966f353ca4516db6ed      1
     11 0x11f46fff2624737aa0d38507e761b6ef770eb94d      1
     12 0x124236d7e03761498813b00ea4e94f49804f8f87      1
     13 0x17ba032e497f6b779f46dd32e95d8d998dc2ab0d      1
     14 0x18119bf8fb02203052cc530240ef9ff377f9c823      1
     15 0x182fdb27d6aae88388ad38a2d7f0d1ac87a981dd      1
     16 0x190fb3b4118e5633820184fe9217049a88f07fcf      1
     17 0x191dbf79739f63cd998c2e4decb04f8259a2479e      1
     18 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     19 0x1f389025797a047f25d6cea5dcd34b521ca458e7      1
     20 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     21 0x2662f48d4191b672cad523f54f761dfc13cbd2f1      1
     22 0x2877d3244f0f6c8e2a51e4f01719d70cc320af30      1
     23 0x291092ae084c08530a035cc844bf8ab0ebeb9e7d      1
     24 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     25 0x2c11cf38a6a5ac74ea438d0037f7f880021fc66f      1
     26 0x2c9134e49865a7a7a2f60ccb9cb31f46885a8796      1
     27 0x2d76564c2f8368cf7f5db9fd59906f8886b8e1ad      1
     28 0x2f1a2b474215a941e75db302b0c75044998d2878      1
     29 0x33e8d10459643faab56f1974291cb603b88a09f8      1
     30 0x37d80cf9d319b2c6e06fe38464488214b962f059      1
     31 0x39bf9f9587c08f7fb80999008a13f3df13b61f08      1
     32 0x3a2ebfcd091ebedbbfac6706dd57776e1f985722      1
     33 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     34 0x42bd129e447643368ccaf951c9f6ada96bf84f1b      1
     35 0x44d5a0b1e165a17c16d940ca774a28e62d4b44b0      1
     36 0x46c91e9691ff9707f093267164e2910cbdc9af8c      1
     37 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
     38 0x47a9dcf163132c8c1c271fc5d8a90a801c8c85ac      1
     39 0x49317b33a82fb576fba1462364d02569e766e33a      1
     40 0x4c8b46acb8ea62a154a9cfd3c446f6f4a678ebb0      1
     41 0x4e323ee346748d7a11c020a38e4feec377779533      1
     42 0x51930478ac95de7ea09c594684b54c0df568e912      1
     43 0x519fe04f730ba0b3f1b5fefeba5e0a2772292889      1
     44 0x54c8f64e743d00d159388197b956b787836f1fa3      1
     45 0x584456aab0110a0f1b177d8a1cde7aff6d09a292      1
     46 0x5855857fd0ee7f6c80442eb244e25cc9b9635c33      1
     47 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
     48 0x616cdc9d8852ac1d1f10e9e746e7c0c881a03957      1
     49 0x6543ff6da083cd8889e4b695d8c575a7a9c88500      1
     50 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     51 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     52 0x672044e2abebaae1359737b7cc5a79071bbb1c0d      1
     53 0x67580561ba59d3d59dfc854c9399d63e93c08d25      1
     54 0x6782b4af64b64c44f2cfdb9b123c4170e27d613e      1
     55 0x69091ff2fc3322fe88d7501ab0922c105ef9ac89      1
     56 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     57 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
     58 0x6cb37d1f14843809e0e4c8e4ef69ea8214c8a11f      1
     59 0x6eab3ca6127d7001f0f266833c63cdb089d9885f      1
     60 0x70e5fdfdc8cf8ce717cd54e882f36804fba78921      1
     61 0x75ae3facc19d5726bbcc85f17eb93110c9f9ab08      1
     62 0x75c3db1bc80b6d566b124ee915b6fc76cbb5303c      1
     63 0x7652d37a175b96aafacf3fed7c735bead21ee594      1
     64 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
     65 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
     66 0x79dd8fab0661da2cd4131bb454bab060576ce2ee      1
     67 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
     68 0x7b70e3993d8ca1804f51b91a2a4b044d10ec8ec8      1
     69 0x7f99acc18a490163a6184d89fe2a2fc80b8fe2ba      1
     70 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     71 0x8160f95edd83e0e420b81e32937b3424fee2b926      1
     72 0x86b880d14f997b839c369ad4218fcedc60ca3091      1
     73 0x8820768caedc113ba97dd749d948ad0fd91c52b6      1
     74 0x896cd4101253ff9d2cab2d256371e5b1e41882f6      1
     75 0x8ce9f7b042fce1971b73240d16849a684a2657d5      1
     76 0x8cf5b8390a8ce9e3f5279e4a3f69f75c781c79db      1
     77 0x8d3af5908dbd28fb0c33f32b51f67c03ed2c00c8      1
     78 0x8d4b7642bd37d670b9a84212cb3cbede9bd5d875      1
     79 0x8dd698e3b14173143591a7c7a425d9bf0fa8ec2f      1
     80 0x9029dfdcb49aefc36e5112aaa0c8567d28ea5d74      1
     81 0x91a05aa62aee372200ad4ab39ee6c3f2aa96ecb0      1
     82 0x959d459fae3e8f26770afd2838b969c389915437      1
     83 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
     84 0x973ef7e4e91cc9098709d540a8ddfb708b331c87      1
     85 0x9832a8dd9a38b27c12a564e2362c581cb5f5bd2b      1
     86 0x991bcd28c326a8fa1754bd6100dfe938dad56894      1
     87 0x9939a8c8293f0a720c4843d1c6c20594ee6a9218      1
     88 0x9a14d1e7077ea070a7f86e0e0fa85899d502d32d      1
     89 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
     90 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     91 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
     92 0xa32c14849106522cd182599bc861bb90f18e6003      1
     93 0xa48b4b40fc346ff27783220b61f411cea89fa4f2      1
     94 0xa96d4549729c2a826237d91b3a3700cad7dfec4a      1
     95 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
     96 0xafe3bf864c426f11115b07d5899a4419504eeded      1
     97 0xb214106a52728d1e55c9327a5b8e7e8de104d7bd      1
     98 0xb310fdfd02d0a1518a8e2111cc3785eb9a09c136      1
     99 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    100 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    101 0xbdeb06519ceb89202e5246c748b5d8021f2a8fe8      1
    102 0xbf00caee3f4d0e654b5e1a557914d257f126d055      1
    103 0xc3ae80c65a25d3e11bec83d7307273364cf68b1e      1
    104 0xc5bc9715874c91ded9313a06baefa1307bd72299      1
    105 0xc646e02557877d559e55a3984ec12654ddd84af3      1
    106 0xc746fe78b267b5cca0d1b6d3049cbcb9e45cccd6      1
    107 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    108 0xc8c0843f1fa7bdae072352f59852aa70f054067d      1
    109 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    110 0xd1fc6c2c64268a854a766196cdab99bc04bf4e44      1
    111 0xd44f069b1ecb1d70b8350807f466c43407eb887c      1
    112 0xd545b77a99a62253b533934dc7ebe311915897f0      1
    113 0xdc6d8803798ee21d0ef101129fe34e09197ef9fa      1
    114 0xe0154466c299d00925b7908138a38d5724848ec1      1
    115 0xe0ddd9b653e182d4761ec665cea7fd5243f32ce2      1
    116 0xe13ec43b554b53555aeff74c367c59636cab88e7      1
    117 0xe54c9ffd043af0add860045b367b8f02fc4c1b88      1
    118 0xeb4b8ecce912e40fa4877f1d5380a810c7b5036c      1
    119 0xeb889d3ffd7170cd1e25a3b2cb0d522b8eaa5cb7      1
    120 0xeda6bb211372aaadd2db0dabaf3c0d4ba90446aa      1
    121 0xf2384657d1cc8bdcdfed6d080f5f14a45f9c6f37      1
    122 0xf70caec95d5a8268da32ce569a15a60651e4a550      1
    123 0xfa031ecd543aae38a107fa8cb1b51e80c1e17876      1
    124 0xfe934349b512b386d8311920c3d7773e2a7485e8      1
    125 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

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
