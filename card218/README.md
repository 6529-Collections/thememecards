
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:110         Length:110         Min.   :1.000   Length:110        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.055                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:110        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19596969 # https://etherscan.io/block/19596969
block_hash <- "0x7423d6846be52a53112d2322084bda797037c193405202ef7b8f0a785d629491"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4215 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","Birth","TheNereids","ZhukliveCollection"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("THETHREEGRACESOPENEDITIONBYZHUK","THETHREEGRACESBYZHUK","SEVENVIRTUESBYZHUK","CURATEDDROPDAY","Noble"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","Birth","TheNereids","ZhukliveCollection","THETHREEGRACESOPENEDITIONBYZHUK","THETHREEGRACESBYZHUK","SEVENVIRTUESBYZHUK","CURATEDDROPDAY","Noble"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     2 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     3 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     4 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
     5 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
     6 0xbb68e3df7e327a99d47285096ae28fb13c8a15e4      1
     7 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
     8 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
     9 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    10 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    11 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    12 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    13 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 78 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     5 0x180669310d089fafcbee40479ee719752d680d4e      1
     6 0x1898834593bc24b3afcd810e3c8835c247dfdec1      1
     7 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     8 0x23602ca06e977c86339ffddad74966e824ab691e      1
     9 0x26ec99704a2c8617c28a9ece2081d6124f0fd8f5      1
    10 0x2f3810b59cc0b1d30325c2845fe58d5a665010e6      1
    11 0x30b57589ce24faeef437dc9d15690e1c16869098      1
    12 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
    13 0x34cf7ac942a815ddeddf1319ce91dea69af46dcb      1
    14 0x363e89408093719f67b7a674b74006989442116a      1
    15 0x370c4fafd8b9a621d01df979866c5259a2f74211      1
    16 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    17 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
    18 0x46131b1b4f4470ca4ee8d8b59a29123ea808fd92      1
    19 0x46bab2aae1fa10a6f19aa522558e03080311e01c      1
    20 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
    21 0x4bdfae7d2a6fc206c5a80f4d41e683a5d5b30d5b      1
    22 0x4c44c067b0b72a4c4d1e405e7543455666c364b0      1
    23 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    24 0x5168895228e49c785d3d77a4621f076403da79a0      1
    25 0x5223e83500c6a30f8cb765af2865738b6bd5c438      1
    26 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
    27 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    28 0x642c449cd9afb9db4dc284e1bb89ff67037d14b9      1
    29 0x6582a0fae4931833aa1f663785da13cbfcb90ab6      1
    30 0x6b0327bbd4ae7aac5af9e1d4c8271c3e6cdd8eda      1
    31 0x7490c6b2207bbaad46f692928194242a6f296630      1
    32 0x74920900637c563b0014701b63f29bc9e79afedc      1
    33 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    34 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    35 0x79813a66d14df527853bc1f81a993688aa680169      1
    36 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    37 0x8105e062d6b94c951e93739aee33018bd6a6d1c7      1
    38 0x84052a229edc2c88de6cde9212259045d2b2c529      1
    39 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    40 0x8f052c26f086a90059b53d269ea44b79c6be255b      1
    41 0x9380da47618d355a9eec7a2ec37e50c2df9bca6b      1
    42 0x953a8dc0e0e2e8fdf8607e5c3c55dd53b2f4fba6      1
    43 0x974b225ccd02d16f5f8209f368eee2a097fc1028      1
    44 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    45 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    46 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    47 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    48 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    49 0xa83fa9c834bb5abe25f9c56eb5845b3203df94f2      1
    50 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    51 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
    52 0xb006e407c055350055c197cc00aa38301d916d0c      1
    53 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    54 0xbad470e65c8986c9bb3e5d75d8e3685b1679c393      1
    55 0xbbc2042d88b41dbf441cc9817a25995777e0defe      1
    56 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    57 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    58 0xc29dad4ba326211f35bd777f82b31a01af059f6a      1
    59 0xcda2efb1de50aa4476b4a20c36bfffdf97b5ae80      1
    60 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    61 0xcf3ffc5be760a33c14dfcfcb97dd3dbc0708f36c      1
    62 0xd2d667b236cf6dc95db080f9a8e6b1ce13971dd5      1
    63 0xd7997b633327b15681bf19d6fd39986d9be2bf3f      1
    64 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    65 0xd8eaf27844490bad1d120b56a05634f6d5e2f589      1
    66 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    67 0xda4bcdd57b4cf52a391e2aa351297a53b552cd17      1
    68 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    69 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    70 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    71 0xe2cedc47153286f185939d7bdd2f5ec09f8c1f2a      1
    72 0xe30f3e4868f56289cada843f581099e5644d37be      1
    73 0xf4583e42598d6cb30bc1eb541d09bb3c3b1a64e8      1
    74 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    75 0xf5671d951c0aa6e4bd69a854fc2d15fe707ddd0e      1
    76 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    77 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    78 0xf9b7d79932b16c6bf8d08dbce15cd5e6942dd18f      1

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
     1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
     2 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     3 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     6 0x180669310d089fafcbee40479ee719752d680d4e      1
     7 0x1898834593bc24b3afcd810e3c8835c247dfdec1      1
     8 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     9 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
    10 0x23602ca06e977c86339ffddad74966e824ab691e      1
    11 0x26ec99704a2c8617c28a9ece2081d6124f0fd8f5      1
    12 0x2f3810b59cc0b1d30325c2845fe58d5a665010e6      1
    13 0x30b57589ce24faeef437dc9d15690e1c16869098      1
    14 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
    15 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    16 0x34cf7ac942a815ddeddf1319ce91dea69af46dcb      1
    17 0x363e89408093719f67b7a674b74006989442116a      1
    18 0x370c4fafd8b9a621d01df979866c5259a2f74211      1
    19 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    20 0x43d3026ab05f1fddfb5621b4f5b35908f9b43890      1
    21 0x46131b1b4f4470ca4ee8d8b59a29123ea808fd92      1
    22 0x46bab2aae1fa10a6f19aa522558e03080311e01c      1
    23 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
    24 0x4bdfae7d2a6fc206c5a80f4d41e683a5d5b30d5b      1
    25 0x4c44c067b0b72a4c4d1e405e7543455666c364b0      1
    26 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    27 0x5168895228e49c785d3d77a4621f076403da79a0      1
    28 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
    29 0x5223e83500c6a30f8cb765af2865738b6bd5c438      1
    30 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
    31 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    32 0x642c449cd9afb9db4dc284e1bb89ff67037d14b9      1
    33 0x6582a0fae4931833aa1f663785da13cbfcb90ab6      1
    34 0x6b0327bbd4ae7aac5af9e1d4c8271c3e6cdd8eda      1
    35 0x7490c6b2207bbaad46f692928194242a6f296630      1
    36 0x74920900637c563b0014701b63f29bc9e79afedc      1
    37 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    38 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    39 0x79813a66d14df527853bc1f81a993688aa680169      1
    40 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    41 0x8105e062d6b94c951e93739aee33018bd6a6d1c7      1
    42 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
    43 0x84052a229edc2c88de6cde9212259045d2b2c529      1
    44 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    45 0x8f052c26f086a90059b53d269ea44b79c6be255b      1
    46 0x9380da47618d355a9eec7a2ec37e50c2df9bca6b      1
    47 0x953a8dc0e0e2e8fdf8607e5c3c55dd53b2f4fba6      1
    48 0x974b225ccd02d16f5f8209f368eee2a097fc1028      1
    49 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    50 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    51 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    52 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    53 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    54 0xa83fa9c834bb5abe25f9c56eb5845b3203df94f2      1
    55 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    56 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
    57 0xb006e407c055350055c197cc00aa38301d916d0c      1
    58 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    59 0xbad470e65c8986c9bb3e5d75d8e3685b1679c393      1
    60 0xbb68e3df7e327a99d47285096ae28fb13c8a15e4      1
    61 0xbbc2042d88b41dbf441cc9817a25995777e0defe      1
    62 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    63 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    64 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    65 0xc29dad4ba326211f35bd777f82b31a01af059f6a      1
    66 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
    67 0xcda2efb1de50aa4476b4a20c36bfffdf97b5ae80      1
    68 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    69 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    70 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    71 0xcf3ffc5be760a33c14dfcfcb97dd3dbc0708f36c      1
    72 0xd2d667b236cf6dc95db080f9a8e6b1ce13971dd5      1
    73 0xd7997b633327b15681bf19d6fd39986d9be2bf3f      1
    74 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    75 0xd8eaf27844490bad1d120b56a05634f6d5e2f589      1
    76 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    77 0xda4bcdd57b4cf52a391e2aa351297a53b552cd17      1
    78 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    79 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    80 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    81 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    82 0xe2cedc47153286f185939d7bdd2f5ec09f8c1f2a      1
    83 0xe30f3e4868f56289cada843f581099e5644d37be      1
    84 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    85 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    86 0xf4583e42598d6cb30bc1eb541d09bb3c3b1a64e8      1
    87 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    88 0xf5671d951c0aa6e4bd69a854fc2d15fe707ddd0e      1
    89 0xf8e90d2d2f6f67a13d0a04374e22624a80a1e918      1
    90 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    91 0xf9b7d79932b16c6bf8d08dbce15cd5e6942dd18f      1

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
