
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:193         Length:193         Min.   :1   Length:193        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:193        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19945069 # https://etherscan.io/block/19945069
block_hash <- "0x6265cb9e4036c21ae89e2f60d8b2947c700dc4c2ee9fb33c0ae21d4f73b703c9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4694 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","SuperTinyWorldII","Alliteration","STWDerivative","FantasyFrens","SuperTinyWorld"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("SuperTinyWorldIIEditions","MyLastTurnEditions","ShadowKnightEditions","HYPNOTISMEditions","GODSPELLSEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","SuperTinyWorldII","Alliteration","STWDerivative","FantasyFrens","SuperTinyWorld","SuperTinyWorldIIEditions","MyLastTurnEditions","ShadowKnightEditions","HYPNOTISMEditions","GODSPELLSEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 77 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00409fc839a2ec2e6d12305423d37cd011279c09      1
     2 0x00aa01d075218250b4cb59e24c553966b67ec5de      1
     3 0x099ef206007e2176a4b221a6a4425bedfc9fa145      1
     4 0x09ce5cb3313038043af9bbdf4c6b069aa4d72274      1
     5 0x0a24f4f999ca47b5d4b81fa7e6d8dfef370293ba      1
     6 0x0cfe3e0c1c24fdc272000683b7d4478af6b25d28      1
     7 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     8 0x0ef4db30f76bcbd1ee7ddbb056e699b69dfb8eae      1
     9 0x178442bb4d0d479b63136b27ef92495381244a70      1
    10 0x18285e915b1dfe8ba9fd735e9dbff9947bf960e2      1
    11 0x1b65dbceb04d9a2cdf42b8945c1e21179271ef51      1
    12 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
    13 0x1e7e5bdcef55ec658f2a29a9bcbc69cabaf1606a      1
    14 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
    15 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
    16 0x2755e1bde189518ed9c819a78066bfcaef876bfc      1
    17 0x2acdf23ce23c0b01d4c54e5cae1dbb420747c30b      1
    18 0x2d219a3337d34f84ceb977f898a6d7afb0b24124      1
    19 0x2d2dda1c5bec22cb49c32e0cf359ea150ffc1f05      1
    20 0x2e3d1c30606bacb3e75f4039b9b28ac9c6fdab84      1
    21 0x2fb03d2ed19aef9dab78f15fd630035be3543fec      1
    22 0x315f78aa18342155b65d87976c5ac1cefaf5d1de      1
    23 0x33ef80b9de51dc7e3f77b43385e211b70a94faa1      1
    24 0x343e73d33639d9c354fa215ca23ffcd0f5604ac1      1
    25 0x367497f910999a1d747c52228e700964946f8b25      1
    26 0x3a1d27a0fd490ed5484e601d68149ab8d89a9fb7      1
    27 0x3b8d8f9194a9d46249bbd62e9a0931957a1d2a25      1
    28 0x3e1a663540397105f200c8ae402c3f401e4d41d6      1
    29 0x3e5118bee774b06ce00115f269f800154de37ca0      1
    30 0x3fa3d8571b234705effa97d0795cf51176bb7afe      1
    31 0x40a22f1fdbdb868b491cfe089b2c7a8e481e5430      1
    32 0x411925f9d955fbac508f65bbfc60a750ce5b9259      1
    33 0x4276d883b78d0914c8006aa83eaab56e9f0204ee      1
    34 0x43f1e5e161511ed35002b8331a36275755c1991a      1
    35 0x53651a3a673f2974bc14334c953f14609e46057a      1
    36 0x5590779fcc2f66f94270d839e965863864043798      1
    37 0x5ea63a56b89745e9383920b23f2264ec64109ab9      1
    38 0x6540d468bd75ae655ac1939a0398ef2c58828fff      1
    39 0x6a2ec22820fd37c4cef5c4fa07763f760947e818      1
    40 0x6ff85e298b2ea083a53cd393c2f9d772ecf76824      1
    41 0x7290e55d298a5a9dc24a1dac4eaf5c2e9b23057b      1
    42 0x7885022b09be4e89abf7a1aa9ee2085640f6329b      1
    43 0x7e1fb8c2a6b74e6d3d2ba81246b110a11945f14b      1
    44 0x80f6d882465af03df880ed86f829129150b413be      1
    45 0x8b431c08f2da10cde2aed73e3a6602595c7615f2      1
    46 0x8c74c3153e829a9c7d60bd057b27d2eb3222dddf      1
    47 0x8fde9f4b8dd2781c85c31fa8deaab60bcd2c8060      1
    48 0x92454b0b5e4e03ee1e2cd271ed11adc02950ef4c      1
    49 0x9264a45901ffe8adf89b18daf8a68f0f416b1dc3      1
    50 0x933e054ca6638b4e7a6b0895fd67bff01ec5cc36      1
    51 0x963b95fa539d21e511403ca9778372eb55184040      1
    52 0x9bf8d39ea7deb0ffb56adf6975cf5f415a21868f      1
    53 0x9d3f31110d4e440460b8e605c55a54a261ab8224      1
    54 0xa92fa1d60a3c1ca5aa77f8420f20d5007ba6c186      1
    55 0xaa78d5313ddd1196e03f7489fa09c588ee3a428b      1
    56 0xab923f97f8f9041dbb2eae386ce740107e3b44dd      1
    57 0xaba902d57d9d5636edd1eeec983f4bbf64eb4ec9      1
    58 0xb87991fec50aa91f5dee55efa70817e750029789      1
    59 0xba5f9e98a76175215b0eb549b450edb75d23a2b2      1
    60 0xbe07d7b158e21640a292a618e22f91de3d8c3102      1
    61 0xc0eed8a8243f3aa3b886c198ea88547ccd0e6950      1
    62 0xc9f8fc772c7b5651bffe1991ffb8e0ee3db71afa      1
    63 0xca6ff14e20276eaf2cffe2d01ec6d1cf16bc036e      1
    64 0xd3c739664385528c12a27fde771dfa00075781f1      1
    65 0xd44220ad06207f21e6e7c2ae2af4671b71744fba      1
    66 0xd5d509c26bd0b247666ac90edcab0828e17cda28      1
    67 0xd84cd640cf6ee54834789bd3384e14ac080e0b8c      1
    68 0xdb4d32ca46310b4079fef1d135c88c1d1def32d7      1
    69 0xdc1ec74e2db84020e9e12ebf34543a7706e629b7      1
    70 0xe2fc3af0faeee9c340f0bcf06cb7d5bba34122e4      1
    71 0xe683a0ff00fae3e3d6413a204d4d7bf9e6a0429a      1
    72 0xe6dc0034edd9126da7e0c5a398d8e7dc71171fea      1
    73 0xed1d5d6d08fb2fd1d89fc7c2e114db5cf3354067      1
    74 0xedd818daaf15551443163f7ae5421dda18e5a82b      1
    75 0xee49f82e58a1c2b306720d0c68047cbf70c11fb5      1
    76 0xf51d05c6477f23647c0d4c9832e6a800e77621ed      1
    77 0xf80af50746b44396818022a6b84b065e2cf11d8a      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 38 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00a8ce8fd79d8a6a6b2d260b08a3b60956f0fcb1      1
     2 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     3 0x120a3c83bcd56318165cd5c8c6e8002aa93922cb      1
     4 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     5 0x27b8a663301ff184c77b5ca9559e16fec976a0d5      1
     6 0x2b8e79604e6eb7094d87c3379d6f5f4f6f17551d      1
     7 0x2fa2eb271bb8e87c6f783204a56fd07092611aaa      1
     8 0x3be3dcb90ab610dd3325336f01251c239bce83ef      1
     9 0x4e58101511abcecb8b10c75fb94871f0d2e0e9fc      1
    10 0x5574d78fc93e6d8952e69303993d24409bed7702      1
    11 0x5dc14baec2399f753762dcfec65e20d922094937      1
    12 0x60434547f5970e6cb8b64501595a98141c17e4c3      1
    13 0x617885c90888a82bd57b037a23144b1ce88ec0ba      1
    14 0x6269cd92e1e2e3b47dca97c72818ef86cbc06c4e      1
    15 0x62a36a68d4f3c1f17c7e7a823dc217b4477d2c48      1
    16 0x63f0a3660170a5c9cd4ca7b28b82f0011ffb37c4      1
    17 0x6ba9ec3f32b1c2f4523f37e1f513fef479a01874      1
    18 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
    19 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    20 0x8ee26622442c73f1384b15895c12c2ef423728fe      1
    21 0x9079a0a7e0ebee7650c8c9da2b6946e5a5b07c19      1
    22 0x9cac33308aa9c647973e96abf09f118c8c937bd1      1
    23 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
    24 0x9e575c86934b02fdd30a537e2d6ff0761b81711e      1
    25 0xa6e1dedd18352cbedeb6eace09d9231e67d46a69      1
    26 0xac2e80f93d507ab5ef326b38b8c7363e02bd2cfd      1
    27 0xae20f0454cc764619489087fb7ef4c3e890062a0      1
    28 0xb0703d86ffb8586751fd46a82b9050b48bbed23e      1
    29 0xb188ee1daca51ce6b58ccf8a81ce4025a714bc73      1
    30 0xc8b810fe39952aa65ca5a9387a3546b9b6bf5780      1
    31 0xcf005cb0f1f89d771dc37ab3e1389eb923803fd2      1
    32 0xd98009d2d013c74d3ffdbdcab3494d0e8f8bbaae      1
    33 0xe49034fe84ef98b15e4a4b968d462ac280845fde      1
    34 0xe68c7be272d3d4105e06a73a7b48845519abab64      1
    35 0xeaba908066aa44fd86730af8873272a543cfdafa      1
    36 0xee947e8d14cbd951105751bbe7d0a4480b6791d3      1
    37 0xf3cacb34460b9d781e2fa1eddefc5d49861e8439      1
    38 0xffd60a30bc05760360052a8519dac1618263fe66      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 115 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00409fc839a2ec2e6d12305423d37cd011279c09      1
      2 0x00a8ce8fd79d8a6a6b2d260b08a3b60956f0fcb1      1
      3 0x00aa01d075218250b4cb59e24c553966b67ec5de      1
      4 0x099ef206007e2176a4b221a6a4425bedfc9fa145      1
      5 0x09ce5cb3313038043af9bbdf4c6b069aa4d72274      1
      6 0x0a24f4f999ca47b5d4b81fa7e6d8dfef370293ba      1
      7 0x0cfe3e0c1c24fdc272000683b7d4478af6b25d28      1
      8 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
      9 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     10 0x0ef4db30f76bcbd1ee7ddbb056e699b69dfb8eae      1
     11 0x120a3c83bcd56318165cd5c8c6e8002aa93922cb      1
     12 0x178442bb4d0d479b63136b27ef92495381244a70      1
     13 0x18285e915b1dfe8ba9fd735e9dbff9947bf960e2      1
     14 0x1b65dbceb04d9a2cdf42b8945c1e21179271ef51      1
     15 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     16 0x1e7e5bdcef55ec658f2a29a9bcbc69cabaf1606a      1
     17 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     18 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     19 0x2755e1bde189518ed9c819a78066bfcaef876bfc      1
     20 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     21 0x27b8a663301ff184c77b5ca9559e16fec976a0d5      1
     22 0x2acdf23ce23c0b01d4c54e5cae1dbb420747c30b      1
     23 0x2b8e79604e6eb7094d87c3379d6f5f4f6f17551d      1
     24 0x2d219a3337d34f84ceb977f898a6d7afb0b24124      1
     25 0x2d2dda1c5bec22cb49c32e0cf359ea150ffc1f05      1
     26 0x2e3d1c30606bacb3e75f4039b9b28ac9c6fdab84      1
     27 0x2fa2eb271bb8e87c6f783204a56fd07092611aaa      1
     28 0x2fb03d2ed19aef9dab78f15fd630035be3543fec      1
     29 0x315f78aa18342155b65d87976c5ac1cefaf5d1de      1
     30 0x33ef80b9de51dc7e3f77b43385e211b70a94faa1      1
     31 0x343e73d33639d9c354fa215ca23ffcd0f5604ac1      1
     32 0x367497f910999a1d747c52228e700964946f8b25      1
     33 0x3a1d27a0fd490ed5484e601d68149ab8d89a9fb7      1
     34 0x3b8d8f9194a9d46249bbd62e9a0931957a1d2a25      1
     35 0x3be3dcb90ab610dd3325336f01251c239bce83ef      1
     36 0x3e1a663540397105f200c8ae402c3f401e4d41d6      1
     37 0x3e5118bee774b06ce00115f269f800154de37ca0      1
     38 0x3fa3d8571b234705effa97d0795cf51176bb7afe      1
     39 0x40a22f1fdbdb868b491cfe089b2c7a8e481e5430      1
     40 0x411925f9d955fbac508f65bbfc60a750ce5b9259      1
     41 0x4276d883b78d0914c8006aa83eaab56e9f0204ee      1
     42 0x43f1e5e161511ed35002b8331a36275755c1991a      1
     43 0x4e58101511abcecb8b10c75fb94871f0d2e0e9fc      1
     44 0x53651a3a673f2974bc14334c953f14609e46057a      1
     45 0x5574d78fc93e6d8952e69303993d24409bed7702      1
     46 0x5590779fcc2f66f94270d839e965863864043798      1
     47 0x5dc14baec2399f753762dcfec65e20d922094937      1
     48 0x5ea63a56b89745e9383920b23f2264ec64109ab9      1
     49 0x60434547f5970e6cb8b64501595a98141c17e4c3      1
     50 0x617885c90888a82bd57b037a23144b1ce88ec0ba      1
     51 0x6269cd92e1e2e3b47dca97c72818ef86cbc06c4e      1
     52 0x62a36a68d4f3c1f17c7e7a823dc217b4477d2c48      1
     53 0x63f0a3660170a5c9cd4ca7b28b82f0011ffb37c4      1
     54 0x6540d468bd75ae655ac1939a0398ef2c58828fff      1
     55 0x6a2ec22820fd37c4cef5c4fa07763f760947e818      1
     56 0x6ba9ec3f32b1c2f4523f37e1f513fef479a01874      1
     57 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
     58 0x6ff85e298b2ea083a53cd393c2f9d772ecf76824      1
     59 0x7290e55d298a5a9dc24a1dac4eaf5c2e9b23057b      1
     60 0x7885022b09be4e89abf7a1aa9ee2085640f6329b      1
     61 0x7e1fb8c2a6b74e6d3d2ba81246b110a11945f14b      1
     62 0x80f6d882465af03df880ed86f829129150b413be      1
     63 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
     64 0x8b431c08f2da10cde2aed73e3a6602595c7615f2      1
     65 0x8c74c3153e829a9c7d60bd057b27d2eb3222dddf      1
     66 0x8ee26622442c73f1384b15895c12c2ef423728fe      1
     67 0x8fde9f4b8dd2781c85c31fa8deaab60bcd2c8060      1
     68 0x9079a0a7e0ebee7650c8c9da2b6946e5a5b07c19      1
     69 0x92454b0b5e4e03ee1e2cd271ed11adc02950ef4c      1
     70 0x9264a45901ffe8adf89b18daf8a68f0f416b1dc3      1
     71 0x933e054ca6638b4e7a6b0895fd67bff01ec5cc36      1
     72 0x963b95fa539d21e511403ca9778372eb55184040      1
     73 0x9bf8d39ea7deb0ffb56adf6975cf5f415a21868f      1
     74 0x9cac33308aa9c647973e96abf09f118c8c937bd1      1
     75 0x9d3f31110d4e440460b8e605c55a54a261ab8224      1
     76 0x9dbef4969a9b01994a05867b2c25c169c60c7048      1
     77 0x9e575c86934b02fdd30a537e2d6ff0761b81711e      1
     78 0xa6e1dedd18352cbedeb6eace09d9231e67d46a69      1
     79 0xa92fa1d60a3c1ca5aa77f8420f20d5007ba6c186      1
     80 0xaa78d5313ddd1196e03f7489fa09c588ee3a428b      1
     81 0xab923f97f8f9041dbb2eae386ce740107e3b44dd      1
     82 0xaba902d57d9d5636edd1eeec983f4bbf64eb4ec9      1
     83 0xac2e80f93d507ab5ef326b38b8c7363e02bd2cfd      1
     84 0xae20f0454cc764619489087fb7ef4c3e890062a0      1
     85 0xb0703d86ffb8586751fd46a82b9050b48bbed23e      1
     86 0xb188ee1daca51ce6b58ccf8a81ce4025a714bc73      1
     87 0xb87991fec50aa91f5dee55efa70817e750029789      1
     88 0xba5f9e98a76175215b0eb549b450edb75d23a2b2      1
     89 0xbe07d7b158e21640a292a618e22f91de3d8c3102      1
     90 0xc0eed8a8243f3aa3b886c198ea88547ccd0e6950      1
     91 0xc8b810fe39952aa65ca5a9387a3546b9b6bf5780      1
     92 0xc9f8fc772c7b5651bffe1991ffb8e0ee3db71afa      1
     93 0xca6ff14e20276eaf2cffe2d01ec6d1cf16bc036e      1
     94 0xcf005cb0f1f89d771dc37ab3e1389eb923803fd2      1
     95 0xd3c739664385528c12a27fde771dfa00075781f1      1
     96 0xd44220ad06207f21e6e7c2ae2af4671b71744fba      1
     97 0xd5d509c26bd0b247666ac90edcab0828e17cda28      1
     98 0xd84cd640cf6ee54834789bd3384e14ac080e0b8c      1
     99 0xd98009d2d013c74d3ffdbdcab3494d0e8f8bbaae      1
    100 0xdb4d32ca46310b4079fef1d135c88c1d1def32d7      1
    101 0xdc1ec74e2db84020e9e12ebf34543a7706e629b7      1
    102 0xe2fc3af0faeee9c340f0bcf06cb7d5bba34122e4      1
    103 0xe49034fe84ef98b15e4a4b968d462ac280845fde      1
    104 0xe683a0ff00fae3e3d6413a204d4d7bf9e6a0429a      1
    105 0xe68c7be272d3d4105e06a73a7b48845519abab64      1
    106 0xe6dc0034edd9126da7e0c5a398d8e7dc71171fea      1
    107 0xeaba908066aa44fd86730af8873272a543cfdafa      1
    108 0xed1d5d6d08fb2fd1d89fc7c2e114db5cf3354067      1
    109 0xedd818daaf15551443163f7ae5421dda18e5a82b      1
    110 0xee49f82e58a1c2b306720d0c68047cbf70c11fb5      1
    111 0xee947e8d14cbd951105751bbe7d0a4480b6791d3      1
    112 0xf3cacb34460b9d781e2fa1eddefc5d49861e8439      1
    113 0xf51d05c6477f23647c0d4c9832e6a800e77621ed      1
    114 0xf80af50746b44396818022a6b84b065e2cf11d8a      1
    115 0xffd60a30bc05760360052a8519dac1618263fe66      1

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
