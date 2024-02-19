
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:204         Length:204         Min.   :1   Length:204        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:204        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19246969 # https://etherscan.io/block/19246969
block_hash <- "0xf884b6d1a17d5a2717b549fc9988c2785c6afb753e9831d9c552ce4d9ea8e438"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4653 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","ExoArts","ExoArtsII","ExoVisions"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ExolorianOpenEditions","COMATOSEEditions","SLEEPINGBEAUTYEditions","KnownOriginEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","ExoArts","ExoArtsII","ExoVisions","ExolorianOpenEditions","COMATOSEEditions","SLEEPINGBEAUTYEditions","KnownOriginEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 32 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x08269c0fa570c4bc91cf054bfedece2a3cc9442b      1
     2 0x08e83e006dc0aa633ab43e01f7927e2029c7795d      1
     3 0x0ce390f18af702cca546297845a4a51d102123cf      1
     4 0x18c74520a37af6cbb102a93cf884fa195a24b834      1
     5 0x195fce941ae0c972b8dc814d2059537b67f14038      1
     6 0x1b24c8745f96eb0ffeb93067af5d253979c73e7a      1
     7 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     8 0x22ceba9af24d7a10aec6c200557e368efa3a6e39      1
     9 0x288af93bb8b388563b411f071bc136e9e4a56e51      1
    10 0x28e3e03240c4b7101c474bdbcab13c6bc42cc7eb      1
    11 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    12 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    13 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    14 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
    15 0x41ada8b7793a00c8300476a891cc264391763ffe      1
    16 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
    17 0x5eca61f46bb8da416ec8662e534bd68b03912c1e      1
    18 0x67744f4b07a3708da6a2362739cc0872e81a6555      1
    19 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    20 0x771e6a61c802a82dfb45f48ed74996b5c1966ed3      1
    21 0x7953e300281858db45cadee1d38bdb8e427576e8      1
    22 0x8c10f5cf9a916158da842bc55f7bfe925018822c      1
    23 0x94ebd7829b77ac5fb44c9cd8a09df0cce70ac835      1
    24 0xa7071299be9de85a7ac0c170e7f129856b7adc9f      1
    25 0xb3a714746a82de11424acc9eb459c54388a25af4      1
    26 0xbda4ee9033c6751e6c14bb59a373a9d4e28480c6      1
    27 0xca3be4b10c7dc7eecb1ea687623c6a75642b033e      1
    28 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    29 0xe9f3ad11d9901d82f4d05ec70ca59a0e21f56d37      1
    30 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    31 0xf2fbe258ce7c249d1563f6151201a5bda1e43cb3      1
    32 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 98 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
     3 0x03b8d4bbdbb2b1e469ba0d453703d57435b21b30      1
     4 0x041fffe0204090c755c92d703381ca21e10b837a      1
     5 0x05a8aa0ed1e1bc598c23b415f67cd774b530546c      1
     6 0x079073fb29f456d74ab5f49d7106e378205a346b      1
     7 0x0e7a408a775af29310970de51b59501e21eee87a      1
     8 0x0f44ba79089d8e6a089575187d7e1d9a81e71a05      1
     9 0x1920dbcfdf0fd291a71bec0b66e20ff8674b01a7      1
    10 0x19c2248e2d09aa01aaa5e8a6561ac5f518af0bd7      1
    11 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
    12 0x23c0954c4b997c58a1544717de90c8e174ea194c      1
    13 0x23f51d4d7ef1f96f7aa45e951d64a057f27fa8dc      1
    14 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
    15 0x2d5c46a34bf42b2296df0886812ebca22a0d49a6      1
    16 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
    17 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
    18 0x34433cb54d406d231cd511567f33ce1914fc888e      1
    19 0x369712ace28dd9d7ac6276cd8bf208a7c51e2937      1
    20 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
    21 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
    22 0x40414f138eb2ef938e6c3629897ef99d4464d4e8      1
    23 0x4267a379f9fca9c09136003d76e106401a332cf9      1
    24 0x4502073918cdda06f73924b958f685fa0ab07624      1
    25 0x45ce826ee6c5a131e14ef2399f419c8e9a537deb      1
    26 0x478e59a221ae43571b706789603cce0a87e769a1      1
    27 0x48a7f9b2d2efbc639fad50425d8a08995ba4a8c7      1
    28 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
    29 0x4c2ce80fd3dd0813d35c673fdf17859b57a705bf      1
    30 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
    31 0x4f58ef7e4ca06a2c19d21d9342ed9ecd1e4b1309      1
    32 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
    33 0x50429dab6e84ff77483296e432055e90cc2ba3f9      1
    34 0x511fc774b16484ba2891078ae74ced1750b5fb52      1
    35 0x52a48782f1ecc648b235d48d3d61d55b3afd892e      1
    36 0x5382a979ed42b239f6d12beca224c810f4144bd0      1
    37 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
    38 0x53d80cbdcd09cad1eae5251cf1517ee57f670996      1
    39 0x554de79a001002a2d71ee48aa7c17faf298f2ddb      1
    40 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
    41 0x57c571d126bb2cc883691efb8e091c1827a11b2f      1
    42 0x59175d35624b3e433f0b7b103a28ba52bd6d70f8      1
    43 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
    44 0x59d04f01411b879f9416073edb3382854edd4e08      1
    45 0x5c87758be1ebe8e18b82a535290b0e8525258a43      1
    46 0x60d8dee41ae9de6806350c8a055a4b93eceb36cc      1
    47 0x62050220633afe524d98fe3fccadbeacc2b96110      1
    48 0x765201a675dd4edab4e7d59bd06bba65d3ff06d2      1
    49 0x768e8456901ebf960dfc6b39fc4d442a2e7cf462      1
    50 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
    51 0x78d9998f31a94e125756b106cca79e1dae6a6014      1
    52 0x7b2359a9d8b50705d23a666423c842e53b789eca      1
    53 0x7c048c55d185fa6a0b1eefe67f3051ceda57deb3      1
    54 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    55 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    56 0x872930e5dc25f210afe6dc5420616df9d570cb92      1
    57 0x8a9a5e78fef6cd90c403c46f1d0e63cfaa582dad      1
    58 0x9050c8c69e6f1fe5cef50b28b3ebcbbe0d7e45f8      1
    59 0x906b8dd75e84a64ca6460c522424dfe685be9450      1
    60 0x90c5c5affe4a64611ac677fe4981481d16d2ee21      1
    61 0x942dfb0c7e87fb5f07e25ec7ff805e7f973cf929      1
    62 0x94bc8265b29ae4881121188394a5c54288fbc14f      1
    63 0x99ddb57f6cde05fc7666920d36e9d51eafbc79f4      1
    64 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
    65 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    66 0xa0632f2989368b504815e02e43b52d2f701626b9      1
    67 0xa264f355089bd93777979e9e40b278400b8778e9      1
    68 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
    69 0xa949eba085bd6b07e8ce5073accfd5732d576122      1
    70 0xab53e599e7fe1e78ccdd7118a44b74a338e45536      1
    71 0xabf4022d9ef8dce73debb0622868a628aedfc874      1
    72 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    73 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
    74 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
    75 0xb6682691708da7b85da007ea51e9f9d85bad5b15      1
    76 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    77 0xb89d68928ecae680c0a5b0444e361a5b62f69554      1
    78 0xbdbbbc0b1df75bdf767df6037e4ff7bec052b51d      1
    79 0xbf707d776e8a6e46df879e4b13a709b3f395935c      1
    80 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    81 0xc249a03bd6ba908d4f4eb66f23433122aedcbee0      1
    82 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    83 0xcad8d27877a1e9d53f60bb73245a3778e3634f91      1
    84 0xcc21abf00a0c589c9818e6ac438d8759ca5e611d      1
    85 0xd1be2b50186ef64fd387baf7cc24573bfb752da6      1
    86 0xd764f692dae9747e67f463636120f743a564e346      1
    87 0xda2a640c0412f9e36af68aa57118eb001cc6ccc9      1
    88 0xdb0dcc28deb985f4a1c872f332b4c97c2821c3ea      1
    89 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
    90 0xdca649392b3a6de6a0d733fe5a5071ae12560f39      1
    91 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    92 0xe40a2a4c6dfc11f40f2bee39a3785f2c33c1d33b      1
    93 0xf03ef3e139a037363a9a8e623999a69276449039      1
    94 0xf5e4111b970972cc7c74a4e9e168309a3a320640      1
    95 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    96 0xfcab2f0f8e64bbda95e1c65d368e6cb22bc5a808      1
    97 0xfdbbbb09fc6118afb1cbb9f7d90143284bfc9a43      1
    98 0xfdcd7184142909f30713693fe6f7e014b91f7815      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 130 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x0264a803979bd0b485a6701f1ab17aab3bcd8ff7      1
      3 0x03b8d4bbdbb2b1e469ba0d453703d57435b21b30      1
      4 0x041fffe0204090c755c92d703381ca21e10b837a      1
      5 0x05a8aa0ed1e1bc598c23b415f67cd774b530546c      1
      6 0x079073fb29f456d74ab5f49d7106e378205a346b      1
      7 0x08269c0fa570c4bc91cf054bfedece2a3cc9442b      1
      8 0x08e83e006dc0aa633ab43e01f7927e2029c7795d      1
      9 0x0ce390f18af702cca546297845a4a51d102123cf      1
     10 0x0e7a408a775af29310970de51b59501e21eee87a      1
     11 0x0f44ba79089d8e6a089575187d7e1d9a81e71a05      1
     12 0x18c74520a37af6cbb102a93cf884fa195a24b834      1
     13 0x1920dbcfdf0fd291a71bec0b66e20ff8674b01a7      1
     14 0x195fce941ae0c972b8dc814d2059537b67f14038      1
     15 0x19c2248e2d09aa01aaa5e8a6561ac5f518af0bd7      1
     16 0x1b24c8745f96eb0ffeb93067af5d253979c73e7a      1
     17 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     18 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     19 0x22ceba9af24d7a10aec6c200557e368efa3a6e39      1
     20 0x23c0954c4b997c58a1544717de90c8e174ea194c      1
     21 0x23f51d4d7ef1f96f7aa45e951d64a057f27fa8dc      1
     22 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     23 0x288af93bb8b388563b411f071bc136e9e4a56e51      1
     24 0x28e3e03240c4b7101c474bdbcab13c6bc42cc7eb      1
     25 0x2d5c46a34bf42b2296df0886812ebca22a0d49a6      1
     26 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     27 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     28 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
     29 0x34433cb54d406d231cd511567f33ce1914fc888e      1
     30 0x369712ace28dd9d7ac6276cd8bf208a7c51e2937      1
     31 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     32 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     33 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     34 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     35 0x40414f138eb2ef938e6c3629897ef99d4464d4e8      1
     36 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     37 0x41ada8b7793a00c8300476a891cc264391763ffe      1
     38 0x4267a379f9fca9c09136003d76e106401a332cf9      1
     39 0x4502073918cdda06f73924b958f685fa0ab07624      1
     40 0x45ce826ee6c5a131e14ef2399f419c8e9a537deb      1
     41 0x478e59a221ae43571b706789603cce0a87e769a1      1
     42 0x48a7f9b2d2efbc639fad50425d8a08995ba4a8c7      1
     43 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
     44 0x4c2ce80fd3dd0813d35c673fdf17859b57a705bf      1
     45 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
     46 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
     47 0x4f58ef7e4ca06a2c19d21d9342ed9ecd1e4b1309      1
     48 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
     49 0x50429dab6e84ff77483296e432055e90cc2ba3f9      1
     50 0x511fc774b16484ba2891078ae74ced1750b5fb52      1
     51 0x52a48782f1ecc648b235d48d3d61d55b3afd892e      1
     52 0x5382a979ed42b239f6d12beca224c810f4144bd0      1
     53 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     54 0x53d80cbdcd09cad1eae5251cf1517ee57f670996      1
     55 0x554de79a001002a2d71ee48aa7c17faf298f2ddb      1
     56 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     57 0x57c571d126bb2cc883691efb8e091c1827a11b2f      1
     58 0x59175d35624b3e433f0b7b103a28ba52bd6d70f8      1
     59 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     60 0x59d04f01411b879f9416073edb3382854edd4e08      1
     61 0x5c87758be1ebe8e18b82a535290b0e8525258a43      1
     62 0x5eca61f46bb8da416ec8662e534bd68b03912c1e      1
     63 0x60d8dee41ae9de6806350c8a055a4b93eceb36cc      1
     64 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     65 0x67744f4b07a3708da6a2362739cc0872e81a6555      1
     66 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     67 0x765201a675dd4edab4e7d59bd06bba65d3ff06d2      1
     68 0x768e8456901ebf960dfc6b39fc4d442a2e7cf462      1
     69 0x771e6a61c802a82dfb45f48ed74996b5c1966ed3      1
     70 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
     71 0x78d9998f31a94e125756b106cca79e1dae6a6014      1
     72 0x7953e300281858db45cadee1d38bdb8e427576e8      1
     73 0x7b2359a9d8b50705d23a666423c842e53b789eca      1
     74 0x7c048c55d185fa6a0b1eefe67f3051ceda57deb3      1
     75 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     76 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     77 0x872930e5dc25f210afe6dc5420616df9d570cb92      1
     78 0x8a9a5e78fef6cd90c403c46f1d0e63cfaa582dad      1
     79 0x8c10f5cf9a916158da842bc55f7bfe925018822c      1
     80 0x9050c8c69e6f1fe5cef50b28b3ebcbbe0d7e45f8      1
     81 0x906b8dd75e84a64ca6460c522424dfe685be9450      1
     82 0x90c5c5affe4a64611ac677fe4981481d16d2ee21      1
     83 0x942dfb0c7e87fb5f07e25ec7ff805e7f973cf929      1
     84 0x94bc8265b29ae4881121188394a5c54288fbc14f      1
     85 0x94ebd7829b77ac5fb44c9cd8a09df0cce70ac835      1
     86 0x99ddb57f6cde05fc7666920d36e9d51eafbc79f4      1
     87 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     88 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
     89 0xa0632f2989368b504815e02e43b52d2f701626b9      1
     90 0xa264f355089bd93777979e9e40b278400b8778e9      1
     91 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
     92 0xa7071299be9de85a7ac0c170e7f129856b7adc9f      1
     93 0xa949eba085bd6b07e8ce5073accfd5732d576122      1
     94 0xab53e599e7fe1e78ccdd7118a44b74a338e45536      1
     95 0xabf4022d9ef8dce73debb0622868a628aedfc874      1
     96 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
     97 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
     98 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
     99 0xb3a714746a82de11424acc9eb459c54388a25af4      1
    100 0xb6682691708da7b85da007ea51e9f9d85bad5b15      1
    101 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    102 0xb89d68928ecae680c0a5b0444e361a5b62f69554      1
    103 0xbda4ee9033c6751e6c14bb59a373a9d4e28480c6      1
    104 0xbdbbbc0b1df75bdf767df6037e4ff7bec052b51d      1
    105 0xbf707d776e8a6e46df879e4b13a709b3f395935c      1
    106 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    107 0xc249a03bd6ba908d4f4eb66f23433122aedcbee0      1
    108 0xca3be4b10c7dc7eecb1ea687623c6a75642b033e      1
    109 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    110 0xcad8d27877a1e9d53f60bb73245a3778e3634f91      1
    111 0xcc21abf00a0c589c9818e6ac438d8759ca5e611d      1
    112 0xd1be2b50186ef64fd387baf7cc24573bfb752da6      1
    113 0xd764f692dae9747e67f463636120f743a564e346      1
    114 0xda2a640c0412f9e36af68aa57118eb001cc6ccc9      1
    115 0xdb0dcc28deb985f4a1c872f332b4c97c2821c3ea      1
    116 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
    117 0xdca649392b3a6de6a0d733fe5a5071ae12560f39      1
    118 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    119 0xe40a2a4c6dfc11f40f2bee39a3785f2c33c1d33b      1
    120 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    121 0xe9f3ad11d9901d82f4d05ec70ca59a0e21f56d37      1
    122 0xf03ef3e139a037363a9a8e623999a69276449039      1
    123 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    124 0xf2fbe258ce7c249d1563f6151201a5bda1e43cb3      1
    125 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    126 0xf5e4111b970972cc7c74a4e9e168309a3a320640      1
    127 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    128 0xfcab2f0f8e64bbda95e1c65d368e6cb22bc5a808      1
    129 0xfdbbbb09fc6118afb1cbb9f7d90143284bfc9a43      1
    130 0xfdcd7184142909f30713693fe6f7e014b91f7815      1

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
