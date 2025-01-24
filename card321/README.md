
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:377         Length:377         Min.   : 1.000   Length:377        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.122                     
                                           3rd Qu.: 1.000                     
                                           Max.   :18.000                     
         name          
     Length:377        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21650069 # https://etherscan.io/block/21650069
block_hash <- "0x697046dd5fd64e8582e93053c8c15b896dfcab40e39116811ec64ef704e3766d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4537 

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

allow_artist1    <- pick(snapshot, contracts=c("Believe","LeaveSomethingBehind","Marea","OdysseyoftheHeart","OdysseysCities","OdysseysSages","DeparturesOdyssey","EgyptianFantasy"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("RawHeartEditions","OdysseyoftheHeartEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Believe","LeaveSomethingBehind","Marea","OdysseyoftheHeart","OdysseysCities","OdysseysSages","DeparturesOdyssey","EgyptianFantasy","RawHeartEditions","OdysseyoftheHeartEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 72 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0602d65ceee1bfe665aac655ea3b86d824d8ca99      1
     2 0x09cf58a73fe686727f93251ec135edd570839e38      1
     3 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
     4 0x0ab6aff8c8b617e1636fcf96d2e0f9480bdff90d      1
     5 0x0b2910cc5eede0d727edca73a5e60e921554456b      1
     6 0x13950e9041a3ff9b0f5440dcce2689528fd231a6      1
     7 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     8 0x15002f16b66f3dc95e3f7193e30764a9efc2fb16      1
     9 0x172737bdb7b80a3fc43ac28164db0bbc47a2bc56      1
    10 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
    11 0x1a3b9b20b312ab98587e4b184459880f73c3031f      1
    12 0x1a7e6c5a59f070681640d59222757781fe090b89      1
    13 0x272ad1b527ccf355a231842a0e4978003bd7673b      1
    14 0x2d887be27dfba166ee4bdecc1d7788d590d90477      1
    15 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    16 0x30ecb9ad88f90a1f52fddefe9e106d53809056af      1
    17 0x31a6cbafe1240b4904e9e452f365060e50e90a18      1
    18 0x3cc4d5e7d3f4ae8cdb34d1a011b07b00309b556f      1
    19 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
    20 0x3ecc3b8ef2b8427cd84720455068a502ba2fb750      1
    21 0x46f9ad43e6415eed9dd5a694e1ebc40a75cd600c      1
    22 0x4e37253f71778c9bf97b54a1af394ef2d6b3e91e      1
    23 0x4f88c947e1c254cf0f56da819e024779fa202787      1
    24 0x533fa4a37aaffdb02456d374237091520790383e      1
    25 0x5433983c2f5a4e81295e0ef9c8bee981aca7692a      1
    26 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
    27 0x575011835453868d388a8aeac7817f5ab2419337      1
    28 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
    29 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    30 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
    31 0x658884c6be7a46cc14229c7646a57c9e049f4a6a      1
    32 0x6948108b463d5ee9c1cf6626502accea8fd47b7d      1
    33 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    34 0x6ebc0e0d832b9faba41d8b8ba0c2a5c65df1eb40      1
    35 0x82af7581909a9a9dd85c41180de73925c39fbf38      1
    36 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
    37 0x8fad217f8a5aa2264723ee2ea93bc88155a85f3a      1
    38 0x904a819e486d4af2653eec8e424599335e2f190e      1
    39 0x93425b5a58d1dd17c6bf5dbe4944039cd2656241      1
    40 0x99581599e8823ee9130380281478e729e926353f      1
    41 0x9c45f3cf0958c5a8b5b1f4477441b77c0fa68151      1
    42 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    43 0x9f04e6c3bee2def65557847e9a7ed2ca902e45f7      1
    44 0xa355440ff4995bf5d2b4453e72514465d22ec81a      1
    45 0xa4b8decbe1a2d8600dc5346c7e3be5163a7d984a      1
    46 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    47 0xa7695409c5fef39a8367759a279386302a683b9a      1
    48 0xae4d837caa0c53579f8a156633355df5058b02f3      1
    49 0xafc5197e8c63f2990da135c9d9639c2725c77c01      1
    50 0xb39cbb572b3ea04c663cf8c98b434e47b13fd04d      1
    51 0xb722e190319010d6e412509b582a77dfd75a7858      1
    52 0xb9f7765eb58e4053e0f69953f52a267071f668f4      1
    53 0xc4167dd0e9a234c76a1cfb1121b2543907a42f0b      1
    54 0xc95b9a13e364ace74cc41b88057e67f93d49e871      1
    55 0xd185a341e2d30df901d4792f52af0265b45d7d9c      1
    56 0xd526ebc929877963ee14e984fdb1d63b0fc2a096      1
    57 0xd80a70370695c68503aaf1552926d6477e585947      1
    58 0xd8f469c76bd1d6ff4208a9cbff9749cc2eb1c9f3      1
    59 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    60 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
    61 0xe0f66ac65020ee81eb612ac865e9a35fb80ede8e      1
    62 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    63 0xe16dce8e44b310953255ffeaa9086f1e3600e999      1
    64 0xe95e4c20b70aeeff43469933d46acc2aa3e6acf4      1
    65 0xea2a9ca3d62bef63cf562b59c5709b32ed4c0eca      1
    66 0xefb4bb33169fb4a3c76d311c3f168e744a5de7bf      1
    67 0xf1efdae403754486ad8613faa5b42b8bcb0bdf26      1
    68 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    69 0xf34149c2d2fdd3955593f749755ea2a4f8e9ea41      1
    70 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1
    71 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1
    72 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 124 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      2 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      3 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      4 0x0e7a408a775af29310970de51b59501e21eee87a      1
      5 0x11cd213b073f2b64aa11e55f5eb8c2f6597da5cc      1
      6 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
      7 0x1641964aed9635ab1f8ece9294142d6ab4ff81a5      1
      8 0x19fedef56411bb49afaf65f7e4be50629be17632      1
      9 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     10 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     11 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     12 0x269c6fb228eb602bd01d14dbc402555c665e11be      1
     13 0x270e8014a75dfc51c54349e7ebed8363a5898425      1
     14 0x285304c4cc2a512407cda997a805e551a5d5ad89      1
     15 0x2a007a4678e4e70903ac39d068ea777e4fe6c8b4      1
     16 0x2fb150e4da26c7b34c50660207d09367480a77a1      1
     17 0x32a09e738a126d4392a0aaa498942b9d629e9dee      1
     18 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     19 0x33fa380088a9bcafd0b2dbfc40cdefd60515329f      1
     20 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     21 0x422699b0f5891c8ddd306c08d9856032264c5e8e      1
     22 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     23 0x43e82c94d0a445c1d90506ce5cba247abdcebdd8      1
     24 0x45742d640011084bccd9cbc55dfe15671f694aca      1
     25 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     26 0x478bb542f7658d635abba67edb987806dff5b83d      1
     27 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     28 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
     29 0x4f51836e363342c52e9fcf2ea892e9ecf702812f      1
     30 0x53ed40c04d7bf1a908e0c57e0a889b85e534ea4e      1
     31 0x5644ea2b2e7784fc47599a983bf90fc380f6b717      1
     32 0x5972a0b205881659aa8cbf287fca3f3bdfd33e36      1
     33 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     34 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     35 0x5b962f3782702530eaab7e52e110a77d470f28e7      1
     36 0x5d65854dabbccd50eb8b698eb919695bbafa1636      1
     37 0x5e3c1c9694346a27a7a1e33b904798d5393fad82      1
     38 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     39 0x608273b078b23ec3c7396628b9b4ef81d8923409      1
     40 0x6772e726dff4ad2541c859b0799e8c18c6e2c489      1
     41 0x685cd3126cda027eb8b4136db2b088324282d28a      1
     42 0x6e388502b891ca05eb52525338172f261c31b7d3      1
     43 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     44 0x6ecae358e99dfdd1abe900bebe5f775431c12324      1
     45 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     46 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
     47 0x72da293d71f1dc131de6a3bec8fa23b619fe909d      1
     48 0x7a175df137a809ffd34cae46cf9a650d4e28ba46      1
     49 0x7cb8c06443aded7a0e51ff66efb2421d3b025465      1
     50 0x7e33f022f4cfab98dd93acc69442f0fd720cced0      1
     51 0x80d1a063ac15ec75938fe81ff673316797490a34      1
     52 0x8442853e514d18bb76356ea17d13448a2381e08f      1
     53 0x852c03de41993cf180bb8bba83e232a99e2fe635      1
     54 0x8889ebb11295f456541901f50bcb5f382047caac      1
     55 0x89018aa86f8f535951802126f30496eeecd5b358      1
     56 0x8aa986eb2f0d3b5001c9c2093698a4e13d646d5b      1
     57 0x8b0e5d5df0dbb29b903954a12d6bb752d8af44b4      1
     58 0x8b5a1a96fa5a485a202b0797c26635b5ad687c60      1
     59 0x8bcb31e0d773c8f376f64e79e3f094ddfa677821      1
     60 0x8d1f185fbf60bcacaf783d8f6436e117d1493658      1
     61 0x8e51fcdbc2bce3e6d33b036fce7da998469e0b66      1
     62 0x8e64fe4f19f2d1768a7820b80e9268eb52752926      1
     63 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
     64 0x901ad0ad484db9a59d939eef28093e39e5dca983      1
     65 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
     66 0x94d903150e921033ee0f7e759718cd45b48122e2      1
     67 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
     68 0x95f5272a4302e58a13422deaeffff646d2b940ab      1
     69 0x983573fe557103b4d9543369902e74a2b38af147      1
     70 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
     71 0x9b25f796dcf939b73d30ab1006d62ddde1a0b1d8      1
     72 0x9b49607dac38750107ab7d8e625ff61fb0a84ca3      1
     73 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
     74 0x9d21f5f14605e228d41fb8d559dd1af39c630224      1
     75 0xa132fc954338ec0d92bb5a7805e56908fb8dae8a      1
     76 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
     77 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     78 0xa7eccf5c6a2b1be67a1d9f117927113cbe7106fa      1
     79 0xae6c0c0b2cb013acb47f97d328a3af52f9ff9e1b      1
     80 0xae9309f7f54b02419df2123dd25b3454a359d937      1
     81 0xb6329fb6e540ad48fa53de021383f35230787e0e      1
     82 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
     83 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
     84 0xba3284b2218af3693ef80e6df41a97a35d1ea705      1
     85 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
     86 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
     87 0xc006aa7a4c23d0d36040964edf9c07bc868db8bb      1
     88 0xc044de7f0f2001ac5ba0d9cb2d10201c05f88402      1
     89 0xc1cf83f7c006cc8f50e62eec470c704f05cd7e29      1
     90 0xc2bb0927d2a90187c02f3274513c275ea9631aeb      1
     91 0xc6f8bfef9cdb009fa1577b9d80f79661d674fd25      1
     92 0xc74a7eee8a15383939d0546cae0812dda23e19c9      1
     93 0xc7b57729663ddd90a05af66b42e9d4f71448f099      1
     94 0xc839de0fed241607d50aa4107ae582443b906e4c      1
     95 0xcaba1b837293c3bdd5b4bd54eb04b896f7e600fc      1
     96 0xcc9b3f50d1befda707ff0b1a808e4e1f1aa15eb4      1
     97 0xcd746c0accb25b0bcc6ae74994ab2db7d0cf3efa      1
     98 0xceaaf79ecd1f11db5fb9259d763c6d0288ce6531      1
     99 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    100 0xd458f62925b3e135680626a0f520098972f93fe9      1
    101 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    102 0xd9ec55c34d9e7ad7131c4b1b84f10427bd107df8      1
    103 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    104 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    105 0xdb3652f22b5faf7798cdfdee2850babd841d4b46      1
    106 0xdc37e6f67bfb7361e25731fc477903e172d211f1      1
    107 0xe1d5d7293fef725e5be2bba3e29749ace065b9bc      1
    108 0xe3a6f6025d361b75ac0f95b5e3820f12b357deee      1
    109 0xe3cbab0b6e9567c4685b26e3996b2a485aa30695      1
    110 0xe588f6581329ce32e40641e20c61a96f296a763a      1
    111 0xe73739c857524d09708152489370dfbbebd6585a      1
    112 0xe9c1870e41754733f96a22e5abae490fb20f8e53      1
    113 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    114 0xed1cfdda9ab85738d984266f13c831f4cdefb91c      1
    115 0xf0556eae3130c5d31cb4cd342f5b112c7bfc7416      1
    116 0xf31781b4811b737e66662997679fb07dacf63355      1
    117 0xf35215fa8d0eb3ecd56d5cfd6420e960b4db5636      1
    118 0xf3b492ff2437f197fabf3a91f17530bc33221ccf      1
    119 0xf48419d4df982ba0e1362bc9163d4361a6265606      1
    120 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    121 0xf53af966dab1afaaac1171dd941148384d38417d      1
    122 0xf7497a5a08cee09cc9e89fe38669ee3810947917      1
    123 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    124 0xfee91bca027e2d74b656e2ae825619f61cb26736      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 196 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      2 0x0602d65ceee1bfe665aac655ea3b86d824d8ca99      1
      3 0x065094af6f8e06c0f0530bcf7d589e4d17608dfd      1
      4 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      5 0x09cf58a73fe686727f93251ec135edd570839e38      1
      6 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
      7 0x0ab6aff8c8b617e1636fcf96d2e0f9480bdff90d      1
      8 0x0b2910cc5eede0d727edca73a5e60e921554456b      1
      9 0x0e7a408a775af29310970de51b59501e21eee87a      1
     10 0x11cd213b073f2b64aa11e55f5eb8c2f6597da5cc      1
     11 0x13950e9041a3ff9b0f5440dcce2689528fd231a6      1
     12 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     13 0x15002f16b66f3dc95e3f7193e30764a9efc2fb16      1
     14 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     15 0x1641964aed9635ab1f8ece9294142d6ab4ff81a5      1
     16 0x172737bdb7b80a3fc43ac28164db0bbc47a2bc56      1
     17 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     18 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     19 0x1a3b9b20b312ab98587e4b184459880f73c3031f      1
     20 0x1a7e6c5a59f070681640d59222757781fe090b89      1
     21 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     22 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     23 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     24 0x269c6fb228eb602bd01d14dbc402555c665e11be      1
     25 0x270e8014a75dfc51c54349e7ebed8363a5898425      1
     26 0x272ad1b527ccf355a231842a0e4978003bd7673b      1
     27 0x285304c4cc2a512407cda997a805e551a5d5ad89      1
     28 0x2a007a4678e4e70903ac39d068ea777e4fe6c8b4      1
     29 0x2d887be27dfba166ee4bdecc1d7788d590d90477      1
     30 0x2fb150e4da26c7b34c50660207d09367480a77a1      1
     31 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     32 0x30ecb9ad88f90a1f52fddefe9e106d53809056af      1
     33 0x31a6cbafe1240b4904e9e452f365060e50e90a18      1
     34 0x32a09e738a126d4392a0aaa498942b9d629e9dee      1
     35 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     36 0x33fa380088a9bcafd0b2dbfc40cdefd60515329f      1
     37 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     38 0x3cc4d5e7d3f4ae8cdb34d1a011b07b00309b556f      1
     39 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     40 0x3ecc3b8ef2b8427cd84720455068a502ba2fb750      1
     41 0x422699b0f5891c8ddd306c08d9856032264c5e8e      1
     42 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     43 0x43e82c94d0a445c1d90506ce5cba247abdcebdd8      1
     44 0x45742d640011084bccd9cbc55dfe15671f694aca      1
     45 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     46 0x46f9ad43e6415eed9dd5a694e1ebc40a75cd600c      1
     47 0x478bb542f7658d635abba67edb987806dff5b83d      1
     48 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     49 0x4e37253f71778c9bf97b54a1af394ef2d6b3e91e      1
     50 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
     51 0x4f51836e363342c52e9fcf2ea892e9ecf702812f      1
     52 0x4f88c947e1c254cf0f56da819e024779fa202787      1
     53 0x533fa4a37aaffdb02456d374237091520790383e      1
     54 0x53ed40c04d7bf1a908e0c57e0a889b85e534ea4e      1
     55 0x5433983c2f5a4e81295e0ef9c8bee981aca7692a      1
     56 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     57 0x5644ea2b2e7784fc47599a983bf90fc380f6b717      1
     58 0x575011835453868d388a8aeac7817f5ab2419337      1
     59 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
     60 0x5972a0b205881659aa8cbf287fca3f3bdfd33e36      1
     61 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     62 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     63 0x5b962f3782702530eaab7e52e110a77d470f28e7      1
     64 0x5d65854dabbccd50eb8b698eb919695bbafa1636      1
     65 0x5e3c1c9694346a27a7a1e33b904798d5393fad82      1
     66 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     67 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     68 0x608273b078b23ec3c7396628b9b4ef81d8923409      1
     69 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     70 0x658884c6be7a46cc14229c7646a57c9e049f4a6a      1
     71 0x6772e726dff4ad2541c859b0799e8c18c6e2c489      1
     72 0x685cd3126cda027eb8b4136db2b088324282d28a      1
     73 0x6948108b463d5ee9c1cf6626502accea8fd47b7d      1
     74 0x6e388502b891ca05eb52525338172f261c31b7d3      1
     75 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
     76 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     77 0x6ebc0e0d832b9faba41d8b8ba0c2a5c65df1eb40      1
     78 0x6ecae358e99dfdd1abe900bebe5f775431c12324      1
     79 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     80 0x725f7066017093b83a8942d52e0750ba9b20fdcc      1
     81 0x72da293d71f1dc131de6a3bec8fa23b619fe909d      1
     82 0x7a175df137a809ffd34cae46cf9a650d4e28ba46      1
     83 0x7cb8c06443aded7a0e51ff66efb2421d3b025465      1
     84 0x7e33f022f4cfab98dd93acc69442f0fd720cced0      1
     85 0x80d1a063ac15ec75938fe81ff673316797490a34      1
     86 0x82af7581909a9a9dd85c41180de73925c39fbf38      1
     87 0x8442853e514d18bb76356ea17d13448a2381e08f      1
     88 0x852c03de41993cf180bb8bba83e232a99e2fe635      1
     89 0x8889ebb11295f456541901f50bcb5f382047caac      1
     90 0x89018aa86f8f535951802126f30496eeecd5b358      1
     91 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
     92 0x8aa986eb2f0d3b5001c9c2093698a4e13d646d5b      1
     93 0x8b0e5d5df0dbb29b903954a12d6bb752d8af44b4      1
     94 0x8b5a1a96fa5a485a202b0797c26635b5ad687c60      1
     95 0x8bcb31e0d773c8f376f64e79e3f094ddfa677821      1
     96 0x8d1f185fbf60bcacaf783d8f6436e117d1493658      1
     97 0x8e51fcdbc2bce3e6d33b036fce7da998469e0b66      1
     98 0x8e64fe4f19f2d1768a7820b80e9268eb52752926      1
     99 0x8fad217f8a5aa2264723ee2ea93bc88155a85f3a      1
    100 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    101 0x901ad0ad484db9a59d939eef28093e39e5dca983      1
    102 0x904a819e486d4af2653eec8e424599335e2f190e      1
    103 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    104 0x93425b5a58d1dd17c6bf5dbe4944039cd2656241      1
    105 0x94d903150e921033ee0f7e759718cd45b48122e2      1
    106 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    107 0x95f5272a4302e58a13422deaeffff646d2b940ab      1
    108 0x983573fe557103b4d9543369902e74a2b38af147      1
    109 0x99581599e8823ee9130380281478e729e926353f      1
    110 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
    111 0x9b25f796dcf939b73d30ab1006d62ddde1a0b1d8      1
    112 0x9b49607dac38750107ab7d8e625ff61fb0a84ca3      1
    113 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
    114 0x9c45f3cf0958c5a8b5b1f4477441b77c0fa68151      1
    115 0x9d21f5f14605e228d41fb8d559dd1af39c630224      1
    116 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    117 0x9f04e6c3bee2def65557847e9a7ed2ca902e45f7      1
    118 0xa132fc954338ec0d92bb5a7805e56908fb8dae8a      1
    119 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    120 0xa355440ff4995bf5d2b4453e72514465d22ec81a      1
    121 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
    122 0xa4b8decbe1a2d8600dc5346c7e3be5163a7d984a      1
    123 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    124 0xa7695409c5fef39a8367759a279386302a683b9a      1
    125 0xa7eccf5c6a2b1be67a1d9f117927113cbe7106fa      1
    126 0xae4d837caa0c53579f8a156633355df5058b02f3      1
    127 0xae6c0c0b2cb013acb47f97d328a3af52f9ff9e1b      1
    128 0xae9309f7f54b02419df2123dd25b3454a359d937      1
    129 0xafc5197e8c63f2990da135c9d9639c2725c77c01      1
    130 0xb39cbb572b3ea04c663cf8c98b434e47b13fd04d      1
    131 0xb6329fb6e540ad48fa53de021383f35230787e0e      1
    132 0xb721640f923f9eac62032097cbd916501a3d3e0f      1
    133 0xb722e190319010d6e412509b582a77dfd75a7858      1
    134 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
    135 0xb9f7765eb58e4053e0f69953f52a267071f668f4      1
    136 0xba3284b2218af3693ef80e6df41a97a35d1ea705      1
    137 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
    138 0xbf89b34ede7a7dea10bcb8be91e345344ac3039e      1
    139 0xc006aa7a4c23d0d36040964edf9c07bc868db8bb      1
    140 0xc044de7f0f2001ac5ba0d9cb2d10201c05f88402      1
    141 0xc1cf83f7c006cc8f50e62eec470c704f05cd7e29      1
    142 0xc2bb0927d2a90187c02f3274513c275ea9631aeb      1
    143 0xc4167dd0e9a234c76a1cfb1121b2543907a42f0b      1
    144 0xc6f8bfef9cdb009fa1577b9d80f79661d674fd25      1
    145 0xc74a7eee8a15383939d0546cae0812dda23e19c9      1
    146 0xc7b57729663ddd90a05af66b42e9d4f71448f099      1
    147 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    148 0xc95b9a13e364ace74cc41b88057e67f93d49e871      1
    149 0xcaba1b837293c3bdd5b4bd54eb04b896f7e600fc      1
    150 0xcc9b3f50d1befda707ff0b1a808e4e1f1aa15eb4      1
    151 0xcd746c0accb25b0bcc6ae74994ab2db7d0cf3efa      1
    152 0xceaaf79ecd1f11db5fb9259d763c6d0288ce6531      1
    153 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    154 0xd185a341e2d30df901d4792f52af0265b45d7d9c      1
    155 0xd458f62925b3e135680626a0f520098972f93fe9      1
    156 0xd526ebc929877963ee14e984fdb1d63b0fc2a096      1
    157 0xd80a70370695c68503aaf1552926d6477e585947      1
    158 0xd8f469c76bd1d6ff4208a9cbff9749cc2eb1c9f3      1
    159 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    160 0xd9ec55c34d9e7ad7131c4b1b84f10427bd107df8      1
    161 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    162 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    163 0xdb3652f22b5faf7798cdfdee2850babd841d4b46      1
    164 0xdc37e6f67bfb7361e25731fc477903e172d211f1      1
    165 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    166 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
    167 0xe0f66ac65020ee81eb612ac865e9a35fb80ede8e      1
    168 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    169 0xe16dce8e44b310953255ffeaa9086f1e3600e999      1
    170 0xe1d5d7293fef725e5be2bba3e29749ace065b9bc      1
    171 0xe3a6f6025d361b75ac0f95b5e3820f12b357deee      1
    172 0xe3cbab0b6e9567c4685b26e3996b2a485aa30695      1
    173 0xe588f6581329ce32e40641e20c61a96f296a763a      1
    174 0xe73739c857524d09708152489370dfbbebd6585a      1
    175 0xe95e4c20b70aeeff43469933d46acc2aa3e6acf4      1
    176 0xe9c1870e41754733f96a22e5abae490fb20f8e53      1
    177 0xea2a9ca3d62bef63cf562b59c5709b32ed4c0eca      1
    178 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    179 0xed1cfdda9ab85738d984266f13c831f4cdefb91c      1
    180 0xefb4bb33169fb4a3c76d311c3f168e744a5de7bf      1
    181 0xf0556eae3130c5d31cb4cd342f5b112c7bfc7416      1
    182 0xf1efdae403754486ad8613faa5b42b8bcb0bdf26      1
    183 0xf31781b4811b737e66662997679fb07dacf63355      1
    184 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    185 0xf34149c2d2fdd3955593f749755ea2a4f8e9ea41      1
    186 0xf35215fa8d0eb3ecd56d5cfd6420e960b4db5636      1
    187 0xf3b492ff2437f197fabf3a91f17530bc33221ccf      1
    188 0xf48419d4df982ba0e1362bc9163d4361a6265606      1
    189 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    190 0xf53af966dab1afaaac1171dd941148384d38417d      1
    191 0xf7497a5a08cee09cc9e89fe38669ee3810947917      1
    192 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    193 0xfd2021d69164ddcd27d2cd1391d5ddc5be1ca9d2      1
    194 0xfee91bca027e2d74b656e2ae825619f61cb26736      1
    195 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1
    196 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

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
