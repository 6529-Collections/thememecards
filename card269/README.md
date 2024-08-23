
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:143         Length:143         Min.   :1   Length:143        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:143        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20546969 # https://etherscan.io/block/20546969
block_hash <- "0x870945634be441ce9f43755abe61cb20906a2960d6c26e49130fea9e8480ced6"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4519 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","ANARCHY","Penitence","MOSHPITT","FASHIONKILLA","Shadows","SuprSketch","X","SKETCHES","RAM"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("RAMARCHIVESEdition","RAMAirdropEdition"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","ANARCHY","Penitence","MOSHPITT","FASHIONKILLA","Shadows","SuprSketch","X","SKETCHES","RAM","RAMARCHIVESEdition","RAMAirdropEdition"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 45 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     3 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
     4 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     5 0x1f6db2c51185429a1b166321d8ae164a4c189b49      1
     6 0x257984b6b5bcfd665532649450242d4df1eb304f      1
     7 0x2d8f8dd6b16a1b5989ff9b82584cb612737de1a9      1
     8 0x300772d881e58a4eb2bb0844d5fc9b2e5476b31b      1
     9 0x3090fb16a6fcf9f85a8d55b710958227154f5083      1
    10 0x31947faed86374c5f8add1bf31781367ab2abf8e      1
    11 0x3685ea73cd33d0d9815780b8fa8caada4aabfc51      1
    12 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
    13 0x4c97542874cdad022e79485678e5746f96d38a75      1
    14 0x4f68c715035649694b677d8ff587dfd2a7f2941c      1
    15 0x60b69d9a8c8e21c19a56a848ee5fd2e63b0741fd      1
    16 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    17 0x762c8b9ffe6b8b34c8667717ec9ba07e53af1f7b      1
    18 0x782adafbf47a604f146af4a059908e946eae539f      1
    19 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    20 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
    21 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    22 0x94d7fa78a5477f91c8bb1f321846a2b7902c7c64      1
    23 0x9b65bcad994551a7ee35be0750f45e444e7f5cc2      1
    24 0x9df495176076301dd318633fe3f67b6ef783a165      1
    25 0x9e279e147afde24e2bc1630953d16cdeae46fb0a      1
    26 0x9e9b2b95c7322f1c799abaa85cc6a9244297c9bc      1
    27 0xa0f085121482cc4278853fb6ebbfa3967885cc63      1
    28 0xa34223a79b330865a0141dbc08b190a9439d418d      1
    29 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    30 0xad5cb5d9d57fbede4db43799e1cbc25c0f601634      1
    31 0xae49c1ad3cf1654c1b22a6ee38dd5bc4ae08fef7      1
    32 0xb35ad86cd35b44dd827f05a0a760283aa58b4c74      1
    33 0xb3711303da2f2af927c4ab32ed4c0693572f35c7      1
    34 0xb855a2ae0f15ed98a5099b51f3acbcd065bd5c78      1
    35 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    36 0xc7282b3b928be27bf4f238bfaa473c0e1f7e9380      1
    37 0xd125565d88cbddcab8e36084ab68ae200b567b4a      1
    38 0xda5dabc83df157bd5afb825f7d7348104da90244      1
    39 0xdc3a18b34630d85e6e3075c8f37461e615e00f67      1
    40 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
    41 0xe0154466c299d00925b7908138a38d5724848ec1      1
    42 0xee52b116157a7b38fa97ef9cd6f912fcb3dbbf14      1
    43 0xf562053531bfefe69bb5b6d169b03135ea6b598c      1
    44 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1
    45 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 46 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0753c365552b30b7232e7892218e26b207f00751      1
     2 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     3 0x1ba2a537a771aa4eac2f18427716557e4e744864      1
     4 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     5 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
     6 0x29d7796f27795c6e02a30f47df883362e69b9654      1
     7 0x2eeb236c606b51d259643ec547a7f41392a7f071      1
     8 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     9 0x346bb57fb7874bba1c06917da8e9901b1aa4bfea      1
    10 0x369615bc57975b06c418f42d3fda3754a385b97b      1
    11 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
    12 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    13 0x46c91e9691ff9707f093267164e2910cbdc9af8c      1
    14 0x4bc7dcc4badb5b7551e148e4c1f24f41676a194b      1
    15 0x582969a9b2016c8f151362edd78119c034498129      1
    16 0x5f1ecc61b0cbae02c6e9e3a90b970a2b6f6e3cca      1
    17 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    18 0x70cc2cd17112b1107917131c2e6d939d0b6df194      1
    19 0x713e87680bd4f44207a6bc03906964477302ec82      1
    20 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    21 0x73d97c30603b73cf4ccde4934c6027a9599d861d      1
    22 0x75ae3facc19d5726bbcc85f17eb93110c9f9ab08      1
    23 0x76c3da8d7b6aad6b3cc12714237b42cd4535b860      1
    24 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    25 0x836ca162d001ca23f40eca50dcdd21c66e350f64      1
    26 0x8fb3ed352df6e8ea152dab12f2d060e4e52d9e6e      1
    27 0x9e52bf0f8973324721f4d4e71e913f172074b6d9      1
    28 0x9ed2bb769dd2dc726ec178d3c49cd265b7cfa151      1
    29 0xa1484b4e545789e3348621a3e910779f0c171bab      1
    30 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    31 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
    32 0xc1cbfd0f49450878c074e3935554002201db3235      1
    33 0xc5b009225d42513343c99cf2e4ce60f11ddcf6c1      1
    34 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    35 0xcb216d5f968647c64bbff5b6830587115278eb87      1
    36 0xcc46f3c48c1f66f6b2d0710385618da93e4cc6c7      1
    37 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    38 0xda21ea810aebe6b567450ff213b9d1e697bc442c      1
    39 0xe0ddd9b653e182d4761ec665cea7fd5243f32ce2      1
    40 0xe4450f045798bda9e1742610cc0ac2a8fbc56b9c      1
    41 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    42 0xf2fc60b88137a9f22bbb5ed0b9310401ade39479      1
    43 0xf31781b4811b737e66662997679fb07dacf63355      1
    44 0xf76a07f67e1f6f9db8ebba3ee9acb6b8933f89f0      1
    45 0xfcb2f5af69a28400c2a03da735a17e3cba1919e2      1
    46 0xfe5d96f817e684ba20473e7c54f1ba3f0ad5207e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 91 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0753c365552b30b7232e7892218e26b207f00751      1
     3 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     4 0x194cbec1b7f8ef5582c8b7f5f9b05fd2334c4c48      1
     5 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     6 0x1ba2a537a771aa4eac2f18427716557e4e744864      1
     7 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     8 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     9 0x1f6db2c51185429a1b166321d8ae164a4c189b49      1
    10 0x223e42994eb3c7b4953c86973892ccf0bab4374a      1
    11 0x257984b6b5bcfd665532649450242d4df1eb304f      1
    12 0x29d7796f27795c6e02a30f47df883362e69b9654      1
    13 0x2d8f8dd6b16a1b5989ff9b82584cb612737de1a9      1
    14 0x2eeb236c606b51d259643ec547a7f41392a7f071      1
    15 0x300772d881e58a4eb2bb0844d5fc9b2e5476b31b      1
    16 0x3090fb16a6fcf9f85a8d55b710958227154f5083      1
    17 0x31947faed86374c5f8add1bf31781367ab2abf8e      1
    18 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    19 0x346bb57fb7874bba1c06917da8e9901b1aa4bfea      1
    20 0x3685ea73cd33d0d9815780b8fa8caada4aabfc51      1
    21 0x369615bc57975b06c418f42d3fda3754a385b97b      1
    22 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
    23 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
    24 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    25 0x46c91e9691ff9707f093267164e2910cbdc9af8c      1
    26 0x4bc7dcc4badb5b7551e148e4c1f24f41676a194b      1
    27 0x4c97542874cdad022e79485678e5746f96d38a75      1
    28 0x4f68c715035649694b677d8ff587dfd2a7f2941c      1
    29 0x582969a9b2016c8f151362edd78119c034498129      1
    30 0x5f1ecc61b0cbae02c6e9e3a90b970a2b6f6e3cca      1
    31 0x60b69d9a8c8e21c19a56a848ee5fd2e63b0741fd      1
    32 0x6ce9f49239e8adaf5bda1154262a3a054fad9db3      1
    33 0x70cc2cd17112b1107917131c2e6d939d0b6df194      1
    34 0x713e87680bd4f44207a6bc03906964477302ec82      1
    35 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    36 0x73d97c30603b73cf4ccde4934c6027a9599d861d      1
    37 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    38 0x75ae3facc19d5726bbcc85f17eb93110c9f9ab08      1
    39 0x762c8b9ffe6b8b34c8667717ec9ba07e53af1f7b      1
    40 0x76c3da8d7b6aad6b3cc12714237b42cd4535b860      1
    41 0x782adafbf47a604f146af4a059908e946eae539f      1
    42 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    43 0x836ca162d001ca23f40eca50dcdd21c66e350f64      1
    44 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    45 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
    46 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    47 0x8fb3ed352df6e8ea152dab12f2d060e4e52d9e6e      1
    48 0x94d7fa78a5477f91c8bb1f321846a2b7902c7c64      1
    49 0x9b65bcad994551a7ee35be0750f45e444e7f5cc2      1
    50 0x9df495176076301dd318633fe3f67b6ef783a165      1
    51 0x9e279e147afde24e2bc1630953d16cdeae46fb0a      1
    52 0x9e52bf0f8973324721f4d4e71e913f172074b6d9      1
    53 0x9e9b2b95c7322f1c799abaa85cc6a9244297c9bc      1
    54 0x9ed2bb769dd2dc726ec178d3c49cd265b7cfa151      1
    55 0xa0f085121482cc4278853fb6ebbfa3967885cc63      1
    56 0xa1484b4e545789e3348621a3e910779f0c171bab      1
    57 0xa34223a79b330865a0141dbc08b190a9439d418d      1
    58 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    59 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    60 0xad5cb5d9d57fbede4db43799e1cbc25c0f601634      1
    61 0xae49c1ad3cf1654c1b22a6ee38dd5bc4ae08fef7      1
    62 0xb35ad86cd35b44dd827f05a0a760283aa58b4c74      1
    63 0xb3711303da2f2af927c4ab32ed4c0693572f35c7      1
    64 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
    65 0xb855a2ae0f15ed98a5099b51f3acbcd065bd5c78      1
    66 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    67 0xc1cbfd0f49450878c074e3935554002201db3235      1
    68 0xc5b009225d42513343c99cf2e4ce60f11ddcf6c1      1
    69 0xc7282b3b928be27bf4f238bfaa473c0e1f7e9380      1
    70 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    71 0xcb216d5f968647c64bbff5b6830587115278eb87      1
    72 0xcc46f3c48c1f66f6b2d0710385618da93e4cc6c7      1
    73 0xd125565d88cbddcab8e36084ab68ae200b567b4a      1
    74 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    75 0xda21ea810aebe6b567450ff213b9d1e697bc442c      1
    76 0xda5dabc83df157bd5afb825f7d7348104da90244      1
    77 0xdc3a18b34630d85e6e3075c8f37461e615e00f67      1
    78 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
    79 0xe0154466c299d00925b7908138a38d5724848ec1      1
    80 0xe0ddd9b653e182d4761ec665cea7fd5243f32ce2      1
    81 0xe4450f045798bda9e1742610cc0ac2a8fbc56b9c      1
    82 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    83 0xee52b116157a7b38fa97ef9cd6f912fcb3dbbf14      1
    84 0xf2fc60b88137a9f22bbb5ed0b9310401ade39479      1
    85 0xf31781b4811b737e66662997679fb07dacf63355      1
    86 0xf562053531bfefe69bb5b6d169b03135ea6b598c      1
    87 0xf76a07f67e1f6f9db8ebba3ee9acb6b8933f89f0      1
    88 0xfcb2f5af69a28400c2a03da735a17e3cba1919e2      1
    89 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1
    90 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1
    91 0xfe5d96f817e684ba20473e7c54f1ba3f0ad5207e      1

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
