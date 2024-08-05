
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:124         Length:124         Min.   :1.000   Length:124        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.113                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:124        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20446069 # https://etherscan.io/block/20446069
block_hash <- "0x05b7c76555f100cc8b568edf509caed82ade6b72095227215b57890b50bc82c1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4552 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","MakersPlace2","KnownOrigin","Laprisamata"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LAPRISAMATAEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","MakersPlace2","KnownOrigin","Laprisamata","LAPRISAMATAEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0f79fc26408ef7fc1d193187685eac9ea921858e      1
     2 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     3 0x22925707d59f89c2edf103b79436fce932d559eb      1
     4 0x24422361687270c1ac2dd3f336e1bc130849617b      1
     5 0x2525f1a08a2d5f037dda969f2fa1b56e4b4b47f3      1
     6 0x25b12eea057708adcb521ef7da0d4112523372fa      1
     7 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
     8 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     9 0x2e21f5d32841cf8c7da805185a041400bf15f21a      1
    10 0x336bd505b648d378a251f06ec3f6693620913f92      1
    11 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
    12 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    13 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    14 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
    15 0x50a545a4303733bc332918c6dd55d7a8f6dbb234      1
    16 0x50cde770461ef53b62e083313d64b5b274b4bb78      1
    17 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    18 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    19 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    20 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    21 0x898c4607809945b49d65ea51580101798931b241      1
    22 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    23 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    24 0xb76a622c7fadbe11c415d141fd7b9eb4b1f414b9      1
    25 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    26 0xc023b4c45cbe38f9f2b90a3f2bb94330c0909d7b      1
    27 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    28 0xc55b7614fdc6f11d649d03420262f91db1181be5      1
    29 0xd0c877b474cd51959931a7f70d7a6c60f50cdae7      1
    30 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    31 0xfd357a6bff9a8f91f457162d894aabd076f8e725      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
     2 0x0513e58916432fdef1cc424f0f2b4cdfe438bc9a      1
     3 0x052d5c3a06ad80940447ca2692220ceff5df9952      1
     4 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     5 0x0b39425288c3eb3626607111da04b268938d8a14      1
     6 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     7 0x2468bca46b3913ed19e73fcc355e448d46f73894      1
     8 0x269fd4e0c6fbad7c682d8924570dbddda3b092dc      1
     9 0x272b5fb6abb5988a38b250ff013bc1ed961caaf1      1
    10 0x2e47f5d3961d35d12e4feb5e79e02384bd0d56b6      1
    11 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    12 0x367859c494b1fb465899f398ba03cf09608bb781      1
    13 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
    14 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    15 0x3981b7b4af5f5a522ebbbbc9e641f55e8fff247c      1
    16 0x39bb2cde4cf7912a3e7a8585f1c49a15b1ae1fd2      1
    17 0x4134e323bc2b45ce5f1926ba621049c53405f604      1
    18 0x435d821ee5b346850545cb18443ca9808a9d47d0      1
    19 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
    20 0x4a8b9e2c2940fdd39aceb384654dc59acb58c337      1
    21 0x4adf0dbc8bb29a61be114c22879202a76790c9d9      1
    22 0x51fd17457ed27a7a38bd2bcf7f9c2de644fa33d4      1
    23 0x55716a1890b1f2087116e5978fab01150c4fe997      1
    24 0x59240e33416fdb8a09702b754712384d2a911412      1
    25 0x5a64b7f45c4c48fa861422e035068aa9c7d663c1      1
    26 0x5c5bb26835287b1d7b0fdb5e0c7ea890c509f851      1
    27 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
    28 0x5fa00c434254b4ee4e305d3694786374914ecfa8      1
    29 0x62d03153358ff577ee0dcf1db555a687f59a8875      1
    30 0x644b31d8f3a0a2849e7626e7088442d8dd88f6e7      1
    31 0x66d8edd7bfd00981d82ecc6d219597170952c71d      1
    32 0x6af6d6b3fba247129c32544b53106ab3bb772902      1
    33 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
    34 0x76735fad3c9459628e6e0f013402353582447aff      1
    35 0x772e3295e83fa106f327c21de8cd032dcd563327      1
    36 0x87896e79102ecdffc942dbd7c1ae242ca3e2a726      1
    37 0x8a212825bbc75154931d0e1e098883cfbe2e51cf      1
    38 0x9758a81c01d9f67fc321811a4d3f6fbc8bc3366a      1
    39 0x9b5c8aabf848a89b1378a930d42fb094a7d94c74      1
    40 0x9dd8cbd80eff662ececeffa043a29d18b2383f1d      1
    41 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    42 0xa224a1da3a11ccf910ed21dc0492c25898a2d92e      1
    43 0xa2b68750c6eaed70380b90b1bae1e3470fcf06be      1
    44 0xaa03fa34f254b8c498744188b8e69536a4fc9cbb      1
    45 0xad4cd1366f78e48531cefe9e0c718bcfe4cc2da3      1
    46 0xafb4bbc61f5314efd15843465db6939e1b9c6838      1
    47 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    48 0xbacb0f8f7b2caa041aef5b99732c2deadf292479      1
    49 0xbb9beeaa6202c7c29cbf40fe2a28133211b3a63b      1
    50 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    51 0xc2a4d3ffc69391f67cd06c62813b9d8117019797      1
    52 0xc91b306c7d0860d1708e63fe7715ba9c11403b4e      1
    53 0xc9be4f9e4251e489035e20104842bf087fbe5bf0      1
    54 0xcac01281d6eabed54575030fcec1f65db4d5fd1f      1
    55 0xcbe8340bbe38de99cc86508d75ca611eff7a3426      1
    56 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    57 0xd16899c24b42b529aa788dc6882dba4c932b5906      1
    58 0xd53d57ad9fcfd13ee5286e6cca809dc8346767fd      1
    59 0xe2d95138b1190bf24823cb0c2febf728026bd097      1
    60 0xe3c2c90bfc938091000d87fa6d36af17c017ee4f      1
    61 0xe634e71c66ee5e4d09e5b240859d7b8d6a930b97      1
    62 0xea63ddc77c3317300964e7971185b2140bb31a14      1
    63 0xee5df0a018dc11cc9d143406926781c7aff1ff79      1
    64 0xf21041305240ea7c312c6691a73fb34213232123      1
    65 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    66 0xf7b6409b12a540947688f054dbf5b629fb3fc7d8      1
    67 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    68 0xffa53d4a990d98daeb71e12036528e93bbb926f6      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 99 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
     2 0x0513e58916432fdef1cc424f0f2b4cdfe438bc9a      1
     3 0x052d5c3a06ad80940447ca2692220ceff5df9952      1
     4 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
     5 0x0b39425288c3eb3626607111da04b268938d8a14      1
     6 0x0f79fc26408ef7fc1d193187685eac9ea921858e      1
     7 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     8 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     9 0x22925707d59f89c2edf103b79436fce932d559eb      1
    10 0x24422361687270c1ac2dd3f336e1bc130849617b      1
    11 0x2468bca46b3913ed19e73fcc355e448d46f73894      1
    12 0x2525f1a08a2d5f037dda969f2fa1b56e4b4b47f3      1
    13 0x25b12eea057708adcb521ef7da0d4112523372fa      1
    14 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
    15 0x269fd4e0c6fbad7c682d8924570dbddda3b092dc      1
    16 0x272b5fb6abb5988a38b250ff013bc1ed961caaf1      1
    17 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    18 0x2e21f5d32841cf8c7da805185a041400bf15f21a      1
    19 0x2e47f5d3961d35d12e4feb5e79e02384bd0d56b6      1
    20 0x336bd505b648d378a251f06ec3f6693620913f92      1
    21 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
    22 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
    23 0x367859c494b1fb465899f398ba03cf09608bb781      1
    24 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
    25 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    26 0x3981b7b4af5f5a522ebbbbc9e641f55e8fff247c      1
    27 0x39bb2cde4cf7912a3e7a8585f1c49a15b1ae1fd2      1
    28 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    29 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    30 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
    31 0x4134e323bc2b45ce5f1926ba621049c53405f604      1
    32 0x435d821ee5b346850545cb18443ca9808a9d47d0      1
    33 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
    34 0x4a8b9e2c2940fdd39aceb384654dc59acb58c337      1
    35 0x4adf0dbc8bb29a61be114c22879202a76790c9d9      1
    36 0x50a545a4303733bc332918c6dd55d7a8f6dbb234      1
    37 0x50cde770461ef53b62e083313d64b5b274b4bb78      1
    38 0x51fd17457ed27a7a38bd2bcf7f9c2de644fa33d4      1
    39 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    40 0x55716a1890b1f2087116e5978fab01150c4fe997      1
    41 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    42 0x59240e33416fdb8a09702b754712384d2a911412      1
    43 0x5a64b7f45c4c48fa861422e035068aa9c7d663c1      1
    44 0x5c5bb26835287b1d7b0fdb5e0c7ea890c509f851      1
    45 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
    46 0x5fa00c434254b4ee4e305d3694786374914ecfa8      1
    47 0x62d03153358ff577ee0dcf1db555a687f59a8875      1
    48 0x644b31d8f3a0a2849e7626e7088442d8dd88f6e7      1
    49 0x66d8edd7bfd00981d82ecc6d219597170952c71d      1
    50 0x6af6d6b3fba247129c32544b53106ab3bb772902      1
    51 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
    52 0x76735fad3c9459628e6e0f013402353582447aff      1
    53 0x772e3295e83fa106f327c21de8cd032dcd563327      1
    54 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    55 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    56 0x87896e79102ecdffc942dbd7c1ae242ca3e2a726      1
    57 0x898c4607809945b49d65ea51580101798931b241      1
    58 0x8a212825bbc75154931d0e1e098883cfbe2e51cf      1
    59 0x9758a81c01d9f67fc321811a4d3f6fbc8bc3366a      1
    60 0x9b5c8aabf848a89b1378a930d42fb094a7d94c74      1
    61 0x9dd8cbd80eff662ececeffa043a29d18b2383f1d      1
    62 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    63 0xa224a1da3a11ccf910ed21dc0492c25898a2d92e      1
    64 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    65 0xa2b68750c6eaed70380b90b1bae1e3470fcf06be      1
    66 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    67 0xaa03fa34f254b8c498744188b8e69536a4fc9cbb      1
    68 0xad4cd1366f78e48531cefe9e0c718bcfe4cc2da3      1
    69 0xafb4bbc61f5314efd15843465db6939e1b9c6838      1
    70 0xb76a622c7fadbe11c415d141fd7b9eb4b1f414b9      1
    71 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    72 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    73 0xbacb0f8f7b2caa041aef5b99732c2deadf292479      1
    74 0xbb9beeaa6202c7c29cbf40fe2a28133211b3a63b      1
    75 0xc023b4c45cbe38f9f2b90a3f2bb94330c0909d7b      1
    76 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    77 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    78 0xc2a4d3ffc69391f67cd06c62813b9d8117019797      1
    79 0xc55b7614fdc6f11d649d03420262f91db1181be5      1
    80 0xc91b306c7d0860d1708e63fe7715ba9c11403b4e      1
    81 0xc9be4f9e4251e489035e20104842bf087fbe5bf0      1
    82 0xcac01281d6eabed54575030fcec1f65db4d5fd1f      1
    83 0xcbe8340bbe38de99cc86508d75ca611eff7a3426      1
    84 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    85 0xd0c877b474cd51959931a7f70d7a6c60f50cdae7      1
    86 0xd16899c24b42b529aa788dc6882dba4c932b5906      1
    87 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    88 0xd53d57ad9fcfd13ee5286e6cca809dc8346767fd      1
    89 0xe2d95138b1190bf24823cb0c2febf728026bd097      1
    90 0xe3c2c90bfc938091000d87fa6d36af17c017ee4f      1
    91 0xe634e71c66ee5e4d09e5b240859d7b8d6a930b97      1
    92 0xea63ddc77c3317300964e7971185b2140bb31a14      1
    93 0xee5df0a018dc11cc9d143406926781c7aff1ff79      1
    94 0xf21041305240ea7c312c6691a73fb34213232123      1
    95 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    96 0xf7b6409b12a540947688f054dbf5b629fb3fc7d8      1
    97 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    98 0xfd357a6bff9a8f91f457162d894aabd076f8e725      1
    99 0xffa53d4a990d98daeb71e12036528e93bbb926f6      1

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
