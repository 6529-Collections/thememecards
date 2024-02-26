
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:307         Length:307         Min.   :1.000   Length:307        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.117                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:307        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19296069 # https://etherscan.io/block/19296069
block_hash <- "0x914cdbc3d69e5e6c6b294016df5a23787416e7850fdeb98ab03a035ec682333e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4612 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Worldofmono","Foundation","Anotherworlds","Digitalstampzuniverse","Mainchapter","Unknownfairytales"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("69yearsEditions","TechnologiesEditions","Seasons1Editions","SubwwaysworldEditions","SubwwaysworldEditions2","DigitalstampsuniverseEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Worldofmono","Foundation","Anotherworlds","Digitalstampzuniverse","Mainchapter","Unknownfairytales","69yearsEditions","TechnologiesEditions","Seasons_1Editions","SubwwaysworldEditions","SubwwaysworldEditions2","DigitalstampsuniverseEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 53 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
     2 0x0aed1878803e2f86b6ddb83f5523f0a793e973fc      1
     3 0x0ce390f18af702cca546297845a4a51d102123cf      1
     4 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     5 0x138ef119319c5c0a82c4da25d7ccb17ff26e5562      1
     6 0x15fe6532890e9e9585462a935e335cf3ee38fc72      1
     7 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     8 0x2821d94783e85a13ebc31110f833d629b9ca6d9c      1
     9 0x38284a0f7eb7933fd8a398f14d1c8e75da15f27e      1
    10 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
    11 0x44e278ed9927bb30eefd0f38d0fd1ba02334e740      1
    12 0x49ff66d90c67ca99feaa5e8e473396d5c25aa71a      1
    13 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
    14 0x54fa5a1b8bf0692358625a8a2dafa506fec2d41f      1
    15 0x57e263154307b1de961eaf703f88f813157c4139      1
    16 0x5d436fc21b5492733b13ee9bf12cb7624c4e70d5      1
    17 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    18 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
    19 0x67e1e0ca2fb369f26889e56c38d490b63d75656e      1
    20 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    21 0x76b2cfee86f8073091cf575ac5aafc748e5e981e      1
    22 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    23 0x7f484af42b87536adbeff4a12c36053b40b2a7a1      1
    24 0x838ff9d763f37c11543d0553ebafafb7bcc42ed5      1
    25 0x88bba92eb5ee38cefe7889f9e20a6d919f6f4703      1
    26 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    27 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    28 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    29 0x991eab746c648909ce2ecf4a8e944c63fd1ee493      1
    30 0x9993da67f310669d33ab5fe159792582c3bdbf36      1
    31 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    32 0x9b6c26e95f10e19cd2fa5488d994632cf7eb5daf      1
    33 0xa3bb15582fe17cfce5c0410862823b0a11789d11      1
    34 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    35 0xa864454c1a35434a1a711e8948f28b59fddec84f      1
    36 0xa8679b6fad644ab1a818e533f9c87302a46df37f      1
    37 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    38 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    39 0xaeace7f1deba36e5fa1ef9d8eed3e666183c1180      1
    40 0xb3173f5abf0b9dd52e489c75e1ae795f78145a24      1
    41 0xb3af48d951f16425d2fa5a8147ae255ed426f54d      1
    42 0xbd7ea7c975cc23049ac31968c764af012ce45f58      1
    43 0xc3d3f90e2210bbe23690e32b80a81745eb4db807      1
    44 0xcae79b098c9bd9eb1811709bbf57bbc124261345      1
    45 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    46 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    47 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    48 0xdf396b6447b284c6771e9bd1302c0c59bed7ab80      1
    49 0xdfe279c336e767beb9fb1c19c1695e2000a2c720      1
    50 0xe7c161519b315ae58f42f3b1709f42ae9a34a9e0      1
    51 0xec385c6a991cad9fe23e5b855526e1e44a1d5b9f      1
    52 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    53 0xf02de4505027133432df6eb111e4e60588b690e5      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 143 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007b9c4d506c6be7b4a3d454d078db5899172da      1
      2 0x00a58651879fd753ab58d639b633368859cda6e1      1
      3 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      5 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      6 0x0773fd9894ebc0aac1196c340d8a41a7ab14a7ae      1
      7 0x0bae2fd474615c52233c001a072f3e10553e26f7      1
      8 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
      9 0x0ddd9dd9b77d3ba97b2496e3590787def887a95b      1
     10 0x0eddbe13c343a993f59f22a9f77b225bb7308060      1
     11 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     12 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     13 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     14 0x1341df844780b66af4ccc98ae0f34be87eabe1d5      1
     15 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     16 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     17 0x1a3017c1789a19f78d28eb72bf65fe02810a2fe9      1
     18 0x1c8480a2928002549bcec6a04f71aa07b6130a66      1
     19 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     20 0x1fa83251e3773011b4ede627b1f16824c8fb8f2e      1
     21 0x21a1be842a708d89564fd7d4105ba0bf3b26c153      1
     22 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     23 0x23145f55648b1399356b519e96fc369f85a1f82d      1
     24 0x25a82f100d42c88b4e3ee8bf206b4d06d7a79997      1
     25 0x26a186f21b291421aa8bbb42068062f04231a763      1
     26 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     27 0x28723e6d0887795d064c0143fa79fcd1a8a9d8a5      1
     28 0x29bd2d1bc9382ab20ee799b6a8beaf9de1a8e929      1
     29 0x2a3d58e5b58e90896db207fce80e7440184782a6      1
     30 0x2a3f7e5170ea8ca967f85f091ef84591f639e031      1
     31 0x2a87046ba5fc27b8630fd6e46b953c8523e81345      1
     32 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     33 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     34 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     35 0x35e3885ed5f7ff3937de0172bb6a6093de19113e      1
     36 0x36f2ecd08d8369007cfe3cd323e89ab4a78e8b69      1
     37 0x394cdf3c66414c32e1761b83a5c34fb28ede9e4a      1
     38 0x3a88dcb6ed37a4b2d6d7218fdca073271095e2d3      1
     39 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     40 0x3de94c2ed08ab3e0e8d3abb4e1638c3134693d42      1
     41 0x446d8fed6c6124f8df17ed19de46977a29c30fd8      1
     42 0x4688b6f3c384c3cffe95c92f599213de4a9fa9ca      1
     43 0x478bb542f7658d635abba67edb987806dff5b83d      1
     44 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     45 0x4a84a25944c4c7dddfe418509bec937521210821      1
     46 0x4c2d965fabf61006cba206e4bb2148b664a66adb      1
     47 0x4f85185e32148a42a8a01d76739204508f66e844      1
     48 0x500bcbbe0ff8ee8fe7c50f7606650e39a50cfd4e      1
     49 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     50 0x5382a979ed42b239f6d12beca224c810f4144bd0      1
     51 0x538c4f2afa012911851dadf0655cb61377e7a8eb      1
     52 0x541a51cccc9fc7991ed0b1c3236e156e533b11a4      1
     53 0x597c8667bb7b9e053e550560d1853e5a30e0eaa6      1
     54 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     55 0x64ed28868990b8440bf2de0c62747a7a13393550      1
     56 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     57 0x686129b664a59b35ed1f070257f9ea8894196b69      1
     58 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
     59 0x7073d3e547921bb3a726f33066582e59a4b95b5f      1
     60 0x751b480dbc61c41cbf3506b1386cdfc446a20900      1
     61 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     62 0x76cc07437385885a5dcaaefd3b94f8fb0bc08930      1
     63 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
     64 0x8318b9c0e640114f7ed71de8b65142654573a152      1
     65 0x833b6cac387e694dadb02b26d0c55e346b4c9c34      1
     66 0x8710ae43cfcdeccf9263fd32fb231d43b9cc7ea4      1
     67 0x87fb030dc5e01b02dd4201a137833f8877aa06c0      1
     68 0x8888888888e9997e64793849389a8faf5e8e547c      1
     69 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
     70 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
     71 0x8f9136e0b058b1696b4d9185d8f479656d92547d      1
     72 0x904a819e486d4af2653eec8e424599335e2f190e      1
     73 0x90a87b61961927b0f3087ef369f7ab074ab55b36      1
     74 0x928ba3d85d23425cd58f3518cd98ff2df8b0f9a1      1
     75 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
     76 0x9405a00bef8aaaea0c844f03b7ac64ebfe4d2913      1
     77 0x95e37edb087b00f04db2efcc8a78974575e6f35a      1
     78 0x97575aac6912233403e9b8935e980dec40c55548      1
     79 0x9a44c4ef3c1589e3646a293ffb4e81d8a4fbdcd0      1
     80 0x9c422874f26ca563ea8ded6427575c237703050c      1
     81 0x9d7f9c2403b53d8f99c66a71fa1b6b67e848a0ef      1
     82 0x9eb6ac015d61d8789e83b73e492d6145abb9e86c      1
     83 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     84 0x9fe366d6efe4ae241656915915f2f050c5ae33be      1
     85 0xa193449b80551a12a38fcc8ec6ac9d842b1459e9      1
     86 0xa248ede5b10afb297bd693abd07143c4d9f6e8be      1
     87 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
     88 0xa7695409c5fef39a8367759a279386302a683b9a      1
     89 0xa939eb7ee2d1f2bc78c7d3e72a7822645888ea27      1
     90 0xadf3656c4a0b04eb20a56bfe8b6a776fa731fcc9      1
     91 0xae6ed8dc4818ab14cc80597c7a553ec9d55f4145      1
     92 0xae9e33d5aae9a20ec475fc57c7f19f62cde7ad61      1
     93 0xb48e45c76e7442d9944790085399e26b7d89b1ed      1
     94 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
     95 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     96 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
     97 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
     98 0xbe01db9ddce0c74d154e24bb8b294ecb182adb01      1
     99 0xbf5365c49a7821ea3dfd0e997aa547142706b159      1
    100 0xc088e2558d72726c0e60a61bdd55afa1792107fb      1
    101 0xc2977c2d19d95df83c91e1721b1a698d8470e34b      1
    102 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    103 0xc3ee684734b9afe6f2aaf4f8e0ea1e5ffda281d9      1
    104 0xc4464dc30eb53437a1e84f380f813f61ae7e174d      1
    105 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
    106 0xc491cc1c68df1b3f3d1eb3af1604e2787891fef9      1
    107 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    108 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    109 0xca074d33dd4b2192291117dba6fadb4949621869      1
    110 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    111 0xca60adfa4789c9ef603cdc15ef56f2f607c4bc75      1
    112 0xcb686c59098a384b52b805233ee82db40f3a31d0      1
    113 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    114 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    115 0xcf57a3b1c076838116731fde404492d9d168747a      1
    116 0xd178221f778a3f06a8fa98c9804ef68548639514      1
    117 0xd1b3976cd24333c68dc6746f891fc698da1c0a4a      1
    118 0xd787001237818c10614bc6cef5d39ccb0348a9da      1
    119 0xd7e683c992855b5e8edca21807fb03d4321d4f12      1
    120 0xd8e6d6de3c077b7a07d50b3b1aee8553378c9f61      1
    121 0xda07cbe67aca847143e56294f4148fdb75550c64      1
    122 0xdab68971008b8565e363cee4dfd217420910a899      1
    123 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    124 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
    125 0xe3a24ae91bcc72c6161b8f0ddf6e81694a2eec0b      1
    126 0xe66e1e9e37e4e148b21eb22001431818e980d060      1
    127 0xe68705ee4f07365c31fb7927f7da89dfe710ec5b      1
    128 0xe823c48f73f6e50ba3a123161c5920d17f66ad89      1
    129 0xe97c4939f1215a95eaf37516f2407494ee843359      1
    130 0xea13fc9b05806a76e478b82f6527d4f7eb21321d      1
    131 0xea462dbb88f5a740652dc27246b6242ebe7a2624      1
    132 0xed53036f802e2b6adbb62f0824bcc33ccc6017af      1
    133 0xeda556c5ee1c7aa66b08bc09d10da8a1c7d3d7d5      1
    134 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    135 0xef20763bbba9f73325c232e584c8141cdac44486      1
    136 0xf0f1a04a214efe01ac5e189e6d9f38cf5d98d84e      1
    137 0xf1a26d0d9355aea843544d941586b6bd8ded7732      1
    138 0xf1e9259db3999a1b28fca4c81f2b1cac5e5e3836      1
    139 0xf65a82a76874659444b704e75875921636d493c6      1
    140 0xf739b48e89ed4b7c2398cc4e2dcd54479b7bea50      1
    141 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    142 0xfbf71b5ff41496f4a4d5f56e52d73d4bb8b131a4      1
    143 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 170 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007b9c4d506c6be7b4a3d454d078db5899172da      1
      2 0x00a58651879fd753ab58d639b633368859cda6e1      1
      3 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      5 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      6 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
      7 0x0773fd9894ebc0aac1196c340d8a41a7ab14a7ae      1
      8 0x0aed1878803e2f86b6ddb83f5523f0a793e973fc      1
      9 0x0bae2fd474615c52233c001a072f3e10553e26f7      1
     10 0x0ce390f18af702cca546297845a4a51d102123cf      1
     11 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
     12 0x0ddd9dd9b77d3ba97b2496e3590787def887a95b      1
     13 0x0eddbe13c343a993f59f22a9f77b225bb7308060      1
     14 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     15 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     16 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     17 0x1341df844780b66af4ccc98ae0f34be87eabe1d5      1
     18 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     19 0x138ef119319c5c0a82c4da25d7ccb17ff26e5562      1
     20 0x15fe6532890e9e9585462a935e335cf3ee38fc72      1
     21 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     22 0x1c8480a2928002549bcec6a04f71aa07b6130a66      1
     23 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     24 0x21a1be842a708d89564fd7d4105ba0bf3b26c153      1
     25 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     26 0x23145f55648b1399356b519e96fc369f85a1f82d      1
     27 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     28 0x26a186f21b291421aa8bbb42068062f04231a763      1
     29 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     30 0x2821d94783e85a13ebc31110f833d629b9ca6d9c      1
     31 0x28723e6d0887795d064c0143fa79fcd1a8a9d8a5      1
     32 0x29bd2d1bc9382ab20ee799b6a8beaf9de1a8e929      1
     33 0x2a3d58e5b58e90896db207fce80e7440184782a6      1
     34 0x2a3f7e5170ea8ca967f85f091ef84591f639e031      1
     35 0x2a87046ba5fc27b8630fd6e46b953c8523e81345      1
     36 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     37 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     38 0x35e3885ed5f7ff3937de0172bb6a6093de19113e      1
     39 0x36f2ecd08d8369007cfe3cd323e89ab4a78e8b69      1
     40 0x38284a0f7eb7933fd8a398f14d1c8e75da15f27e      1
     41 0x394cdf3c66414c32e1761b83a5c34fb28ede9e4a      1
     42 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     43 0x3de94c2ed08ab3e0e8d3abb4e1638c3134693d42      1
     44 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
     45 0x44e278ed9927bb30eefd0f38d0fd1ba02334e740      1
     46 0x478bb542f7658d635abba67edb987806dff5b83d      1
     47 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     48 0x49ff66d90c67ca99feaa5e8e473396d5c25aa71a      1
     49 0x4a84a25944c4c7dddfe418509bec937521210821      1
     50 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
     51 0x4f85185e32148a42a8a01d76739204508f66e844      1
     52 0x500bcbbe0ff8ee8fe7c50f7606650e39a50cfd4e      1
     53 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     54 0x5382a979ed42b239f6d12beca224c810f4144bd0      1
     55 0x538c4f2afa012911851dadf0655cb61377e7a8eb      1
     56 0x541a51cccc9fc7991ed0b1c3236e156e533b11a4      1
     57 0x54fa5a1b8bf0692358625a8a2dafa506fec2d41f      1
     58 0x57e263154307b1de961eaf703f88f813157c4139      1
     59 0x5d436fc21b5492733b13ee9bf12cb7624c4e70d5      1
     60 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     61 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     62 0x64ed28868990b8440bf2de0c62747a7a13393550      1
     63 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
     64 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     65 0x67e1e0ca2fb369f26889e56c38d490b63d75656e      1
     66 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     67 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
     68 0x7073d3e547921bb3a726f33066582e59a4b95b5f      1
     69 0x751b480dbc61c41cbf3506b1386cdfc446a20900      1
     70 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     71 0x76b2cfee86f8073091cf575ac5aafc748e5e981e      1
     72 0x76cc07437385885a5dcaaefd3b94f8fb0bc08930      1
     73 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     74 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
     75 0x7f484af42b87536adbeff4a12c36053b40b2a7a1      1
     76 0x833b6cac387e694dadb02b26d0c55e346b4c9c34      1
     77 0x838ff9d763f37c11543d0553ebafafb7bcc42ed5      1
     78 0x8710ae43cfcdeccf9263fd32fb231d43b9cc7ea4      1
     79 0x8888888888e9997e64793849389a8faf5e8e547c      1
     80 0x88bba92eb5ee38cefe7889f9e20a6d919f6f4703      1
     81 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
     82 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
     83 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     84 0x8f9136e0b058b1696b4d9185d8f479656d92547d      1
     85 0x904a819e486d4af2653eec8e424599335e2f190e      1
     86 0x90a87b61961927b0f3087ef369f7ab074ab55b36      1
     87 0x928ba3d85d23425cd58f3518cd98ff2df8b0f9a1      1
     88 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
     89 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
     90 0x95e37edb087b00f04db2efcc8a78974575e6f35a      1
     91 0x97575aac6912233403e9b8935e980dec40c55548      1
     92 0x991eab746c648909ce2ecf4a8e944c63fd1ee493      1
     93 0x9993da67f310669d33ab5fe159792582c3bdbf36      1
     94 0x9a44c4ef3c1589e3646a293ffb4e81d8a4fbdcd0      1
     95 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
     96 0x9b6c26e95f10e19cd2fa5488d994632cf7eb5daf      1
     97 0x9c422874f26ca563ea8ded6427575c237703050c      1
     98 0x9eb6ac015d61d8789e83b73e492d6145abb9e86c      1
     99 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    100 0xa193449b80551a12a38fcc8ec6ac9d842b1459e9      1
    101 0xa248ede5b10afb297bd693abd07143c4d9f6e8be      1
    102 0xa3bb15582fe17cfce5c0410862823b0a11789d11      1
    103 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
    104 0xa7695409c5fef39a8367759a279386302a683b9a      1
    105 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    106 0xa864454c1a35434a1a711e8948f28b59fddec84f      1
    107 0xa8679b6fad644ab1a818e533f9c87302a46df37f      1
    108 0xa939eb7ee2d1f2bc78c7d3e72a7822645888ea27      1
    109 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    110 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    111 0xadf3656c4a0b04eb20a56bfe8b6a776fa731fcc9      1
    112 0xae6ed8dc4818ab14cc80597c7a553ec9d55f4145      1
    113 0xae9e33d5aae9a20ec475fc57c7f19f62cde7ad61      1
    114 0xaeace7f1deba36e5fa1ef9d8eed3e666183c1180      1
    115 0xb3173f5abf0b9dd52e489c75e1ae795f78145a24      1
    116 0xb3af48d951f16425d2fa5a8147ae255ed426f54d      1
    117 0xb48e45c76e7442d9944790085399e26b7d89b1ed      1
    118 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    119 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
    120 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
    121 0xbd7ea7c975cc23049ac31968c764af012ce45f58      1
    122 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
    123 0xbe01db9ddce0c74d154e24bb8b294ecb182adb01      1
    124 0xbf5365c49a7821ea3dfd0e997aa547142706b159      1
    125 0xc088e2558d72726c0e60a61bdd55afa1792107fb      1
    126 0xc2977c2d19d95df83c91e1721b1a698d8470e34b      1
    127 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    128 0xc3d3f90e2210bbe23690e32b80a81745eb4db807      1
    129 0xc4464dc30eb53437a1e84f380f813f61ae7e174d      1
    130 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
    131 0xc491cc1c68df1b3f3d1eb3af1604e2787891fef9      1
    132 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    133 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    134 0xca60adfa4789c9ef603cdc15ef56f2f607c4bc75      1
    135 0xcae79b098c9bd9eb1811709bbf57bbc124261345      1
    136 0xcb686c59098a384b52b805233ee82db40f3a31d0      1
    137 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    138 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    139 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    140 0xcf57a3b1c076838116731fde404492d9d168747a      1
    141 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    142 0xd787001237818c10614bc6cef5d39ccb0348a9da      1
    143 0xd7e683c992855b5e8edca21807fb03d4321d4f12      1
    144 0xd8e6d6de3c077b7a07d50b3b1aee8553378c9f61      1
    145 0xda07cbe67aca847143e56294f4148fdb75550c64      1
    146 0xdab68971008b8565e363cee4dfd217420910a899      1
    147 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    148 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    149 0xdf396b6447b284c6771e9bd1302c0c59bed7ab80      1
    150 0xdfe279c336e767beb9fb1c19c1695e2000a2c720      1
    151 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
    152 0xe3a24ae91bcc72c6161b8f0ddf6e81694a2eec0b      1
    153 0xe68705ee4f07365c31fb7927f7da89dfe710ec5b      1
    154 0xe7c161519b315ae58f42f3b1709f42ae9a34a9e0      1
    155 0xe823c48f73f6e50ba3a123161c5920d17f66ad89      1
    156 0xe97c4939f1215a95eaf37516f2407494ee843359      1
    157 0xea13fc9b05806a76e478b82f6527d4f7eb21321d      1
    158 0xea462dbb88f5a740652dc27246b6242ebe7a2624      1
    159 0xec385c6a991cad9fe23e5b855526e1e44a1d5b9f      1
    160 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    161 0xef20763bbba9f73325c232e584c8141cdac44486      1
    162 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    163 0xf02de4505027133432df6eb111e4e60588b690e5      1
    164 0xf0f1a04a214efe01ac5e189e6d9f38cf5d98d84e      1
    165 0xf1a26d0d9355aea843544d941586b6bd8ded7732      1
    166 0xf1e9259db3999a1b28fca4c81f2b1cac5e5e3836      1
    167 0xf65a82a76874659444b704e75875921636d493c6      1
    168 0xf83d854970c6d7fe7a6f6d26872cc121b996887a      1
    169 0xfbf71b5ff41496f4a4d5f56e52d73d4bb8b131a4      1
    170 0xffb5592fb5a51f85668ea6634c88ac1949cc063c      1

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
