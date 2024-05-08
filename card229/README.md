
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:186         Length:186         Min.   :1.000   Length:186        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.048                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:186        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19795269 # https://etherscan.io/block/19795269
block_hash <- "0x41cd67497e0c82a0a0f24cce7056bc0490c1c73d378ef542996b56276dd97d62"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4521 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","BoundlessImagination","SquareView","CrossingLand","LoulanWorld","LoulanDerivative"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LoulanweuEditions","CHCCEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","BoundlessImagination","SquareView","CrossingLand","LoulanWorld","LoulanDerivative","LoulanweuEditions","CHCCEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 23 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00669015dac281554813e0e6fc5b32f4c3824544      1
     2 0x00ff192363430a35abbf968c535b64147e88abdb      1
     3 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
     4 0x0d03912943905671271f02d7a1e626b01c4e1440      1
     5 0x0d2f9fbc29b788a79fd53d93f9b8d495235cec3e      1
     6 0x11a51b3af70afae1b52cf51cb38cade20c1203dc      1
     7 0x1e983ce401336a8a74bc2983ae681c901117ae10      1
     8 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     9 0x3d6130c64eb3977d48092dbc28d1d084c6f7aed2      1
    10 0x5bc926e531431b5a1a0f26e2dd4a7aa8f322b1ce      1
    11 0x6cad38cec0128055bd5768f729c9d5c85ca29470      1
    12 0x7f32973cfdef197e56cb5ddd2c9412505a629c92      1
    13 0x9429b7d2a312b140e18981757b2b44b33881e76b      1
    14 0x94e8b6295be681a2c4ddad11f501e0fe9abb758a      1
    15 0xa249a826ff225e7fffd5a6fc118bef4feea2633f      1
    16 0xbaca8a4528ce21edd8d3ff46220151427868653a      1
    17 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    18 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    19 0xdf43c410cf1bfb8bf4876e5f366bde6576babb38      1
    20 0xe3cbab0b6e9567c4685b26e3996b2a485aa30695      1
    21 0xe8222179ebf7c93940b63928fcfc31b1bd5c0bf5      1
    22 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    23 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 123 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02479bbb9413ac734ad94460e597f98e60d78536      1
      2 0x075f344771dbbba0260db5640f6150657b2b3c46      1
      3 0x083ead940335d6908cdb078df005fb4c5f83a9b0      1
      4 0x0b89c54c8bfbccb8a4330afd450d54564d7251c1      1
      5 0x0b8ac8d2dca5179a8d05432b154be8124d739270      1
      6 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
      7 0x103bfb8b7058cdb8fb2d4cc6e4a8afa0fcc9cf77      1
      8 0x12dc5b1e1e124b2da9fa339e2f0323be1f180f04      1
      9 0x152ec9c0e3ff2cf02fcd7b22cc163c849e8c0102      1
     10 0x1570264a32ab9bc7ac931e28650db8174c0c0873      1
     11 0x1b3ca08ba8d6a60bd7250254f873d2d874c8fe89      1
     12 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     13 0x1f9074496e899c339e67be757b9d757cda1a07e7      1
     14 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     15 0x21ca8b1b771fb330349f7bb386481b9c3369085b      1
     16 0x265773b798f5a83876aa0a463526a5d86dee5e00      1
     17 0x269cd2ad02c7d38e17ca7aa533b1d79f7cc8d02c      1
     18 0x27bd2b199eaec39bce5604e030259c0bc1bdaaa0      1
     19 0x2b65ee5b1d99ec9cb2a8217bac34f0bdd1a19c4c      1
     20 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     21 0x2cd22313443210fd7924953c84a9ad23b1e23414      1
     22 0x2d3ffd5f6624e1bc063c1fb1722011cda927f5de      1
     23 0x2f5926896bc7f9432392099bbc66526cea300077      1
     24 0x2f71f0b0e5b7fc1b4f0ebd478d6b006a41a599b2      1
     25 0x2fa08fcc7e6faf795417c3a73189939c7a818817      1
     26 0x2fb5958975f91e39693fb0c5dfd9ebc8900e48e1      1
     27 0x2ffbfe97126261b600ae6771f13556cc6104c1dd      1
     28 0x312a3bfc8f32d64656fa5d54bb7e7e154c5cd3ca      1
     29 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
     30 0x3280c639fd18b32392b77f35748fa65e311799fa      1
     31 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     32 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     33 0x36d4706d9d8a106d2320fe14c9da4a4d13cbf498      1
     34 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     35 0x3bc3a258fc6dd9f6d3021a47b44706c656faa6da      1
     36 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     37 0x3e6b87ff2168d15794a865d09a6716415e7dbecf      1
     38 0x3edca1c467a44d10f6e63748c52552a04dcedc67      1
     39 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     40 0x42ddbe0c37da93e1743d2976a3d142b8bc4886b7      1
     41 0x48f50cfac09e7e0ae2911a36e4959a665720e016      1
     42 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     43 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     44 0x4dd7f16dea28f8e68201549dbf0df2e504a36494      1
     45 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
     46 0x5046e163e2f49b54f97119ef8c44b54d401af6cc      1
     47 0x511fc774b16484ba2891078ae74ced1750b5fb52      1
     48 0x52439c4de8c10ef8904e96ad00171c0e86169dde      1
     49 0x52fc228959a904efbe127052face70373f8a8894      1
     50 0x59068075a799594db03c0255eed68e8e121155c8      1
     51 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     52 0x5d4322057537eac2f0838063697724fbd4847fac      1
     53 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     54 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     55 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     56 0x6073ae7059582fb08ae030663c33d026b3ad0766      1
     57 0x6079696f3d9fc53ba8350d8db44e048e6a3814b1      1
     58 0x6286ce57c5a2ec8eafc09a1d74034770fd92d5ef      1
     59 0x63ed62de8944057087a19ea80eeed11a675cdfdc      1
     60 0x6422d607ca13457589a1f2dbf0ec63d5adf87bfb      1
     61 0x66c4f8f604f902939c6de0038d6ad6a561d4363d      1
     62 0x66ea2d335291a5afa8d3522eef6adfc3f8746e16      1
     63 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     64 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
     65 0x6e5033b09cb7de7dbfb6e7547cf274e09cff1ca9      1
     66 0x6f79a3d649db7cf1f6842f42b33374d8fbd13721      1
     67 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
     68 0x75874f95852b742ce3d571b5cc5e2fc4355cbd9a      1
     69 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     70 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     71 0x79e561168835c783240a0637320d308897bd0922      1
     72 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     73 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
     74 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     75 0x81a80a99c8ab3c57823d6235134979e8c13b2239      1
     76 0x84573d169d84f3bab429736ade19691e70359102      1
     77 0x877f3bcb3c0ad49b64c404970d5671e00231b59c      1
     78 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
     79 0x8ac236f975c5b44948c45c662834e28f017c7097      1
     80 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
     81 0x95e26ef85c6af021d3106d1e6ac345faa7c6f663      1
     82 0x96deaeb8fd37527b0a97d56718ee614c8f73187d      1
     83 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
     84 0x9d5393c69a8caed1af722aba7ce1e42b5672160b      1
     85 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
     86 0xa2dd9400107aeb8ed8a9fa638b46cc4d11c44053      1
     87 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
     88 0xa6134eb7381978804646088df4bb42c5232d82da      1
     89 0xab42c346917f95431799271394c3e061c005886d      1
     90 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
     91 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
     92 0xbeb08de7778139fa4e0f1f2b8b0da88d997a1636      1
     93 0xc267da714ddb007bc198cb9bdb722c66e652c687      1
     94 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
     95 0xc8e2cc6dd09d2f6201b0aabc295ffda3c4b99d7a      1
     96 0xc9f84ca819fac0bcf4cc58948ddd477385ce69a7      1
     97 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
     98 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
     99 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
    100 0xd211e839f02a450a05bedc52aafe520eb45efe59      1
    101 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    102 0xd63c2582b071f41090e89d975f738e87e7b8da50      1
    103 0xd6cd34f0d27d3942fb7aab193566bd76f694bf59      1
    104 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    105 0xe0bac6d36e57000f1ad2eb8ca0e9cc16fdd5df81      1
    106 0xe0e8c1d735698060477e79a8e4c20276fc2ec7a7      1
    107 0xe5451a8651ff622fb3a111633e0c709379aaf4a1      1
    108 0xe629117078d0849dce5e2c23b0ef40ab62a672a3      1
    109 0xec18673439ca47cb2788f1a8a82ceab0dd59d2b7      1
    110 0xec234324276680b8de93c048dd69d68181c00356      1
    111 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    112 0xee74258438bcd2882fe907c91a5371dc5dd5b0ee      1
    113 0xefaf1fa1362a23729cb879ab475e923c651bd532      1
    114 0xf06f0c1d0610e2c7427a43b8873c808087497513      1
    115 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    116 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    117 0xf8bd02c5bdeb28bbb240355a19a9831c4d0db413      1
    118 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    119 0xf8f9347ad38f607e2c510a211a563cf5c5f52e89      1
    120 0xfba206e73709f614e5a85aff27a98692d4f3c579      1
    121 0xfe5e101381727ba5a0e2cee531016192cd7d63cc      1
    122 0xfe6bae8676fdedb9a05735f4ab1adacd83501c30      1
    123 0xffe500927dbb171abddab384e1d1754338cca121      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 146 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00669015dac281554813e0e6fc5b32f4c3824544      1
      2 0x00ff192363430a35abbf968c535b64147e88abdb      1
      3 0x02479bbb9413ac734ad94460e597f98e60d78536      1
      4 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
      5 0x075f344771dbbba0260db5640f6150657b2b3c46      1
      6 0x083ead940335d6908cdb078df005fb4c5f83a9b0      1
      7 0x0b89c54c8bfbccb8a4330afd450d54564d7251c1      1
      8 0x0b8ac8d2dca5179a8d05432b154be8124d739270      1
      9 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     10 0x0d03912943905671271f02d7a1e626b01c4e1440      1
     11 0x0d2f9fbc29b788a79fd53d93f9b8d495235cec3e      1
     12 0x103bfb8b7058cdb8fb2d4cc6e4a8afa0fcc9cf77      1
     13 0x11a51b3af70afae1b52cf51cb38cade20c1203dc      1
     14 0x12dc5b1e1e124b2da9fa339e2f0323be1f180f04      1
     15 0x152ec9c0e3ff2cf02fcd7b22cc163c849e8c0102      1
     16 0x1570264a32ab9bc7ac931e28650db8174c0c0873      1
     17 0x1b3ca08ba8d6a60bd7250254f873d2d874c8fe89      1
     18 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     19 0x1e983ce401336a8a74bc2983ae681c901117ae10      1
     20 0x1f9074496e899c339e67be757b9d757cda1a07e7      1
     21 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     22 0x21ca8b1b771fb330349f7bb386481b9c3369085b      1
     23 0x265773b798f5a83876aa0a463526a5d86dee5e00      1
     24 0x269cd2ad02c7d38e17ca7aa533b1d79f7cc8d02c      1
     25 0x27bd2b199eaec39bce5604e030259c0bc1bdaaa0      1
     26 0x2b65ee5b1d99ec9cb2a8217bac34f0bdd1a19c4c      1
     27 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     28 0x2cd22313443210fd7924953c84a9ad23b1e23414      1
     29 0x2d3ffd5f6624e1bc063c1fb1722011cda927f5de      1
     30 0x2f5926896bc7f9432392099bbc66526cea300077      1
     31 0x2f71f0b0e5b7fc1b4f0ebd478d6b006a41a599b2      1
     32 0x2fa08fcc7e6faf795417c3a73189939c7a818817      1
     33 0x2fb5958975f91e39693fb0c5dfd9ebc8900e48e1      1
     34 0x2ffbfe97126261b600ae6771f13556cc6104c1dd      1
     35 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     36 0x312a3bfc8f32d64656fa5d54bb7e7e154c5cd3ca      1
     37 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
     38 0x3280c639fd18b32392b77f35748fa65e311799fa      1
     39 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     40 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     41 0x36d4706d9d8a106d2320fe14c9da4a4d13cbf498      1
     42 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     43 0x3bc3a258fc6dd9f6d3021a47b44706c656faa6da      1
     44 0x3d6130c64eb3977d48092dbc28d1d084c6f7aed2      1
     45 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     46 0x3e6b87ff2168d15794a865d09a6716415e7dbecf      1
     47 0x3edca1c467a44d10f6e63748c52552a04dcedc67      1
     48 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     49 0x42ddbe0c37da93e1743d2976a3d142b8bc4886b7      1
     50 0x48f50cfac09e7e0ae2911a36e4959a665720e016      1
     51 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     52 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     53 0x4dd7f16dea28f8e68201549dbf0df2e504a36494      1
     54 0x4f764a08c66251e13bdd85b4bb0652b739736328      1
     55 0x5046e163e2f49b54f97119ef8c44b54d401af6cc      1
     56 0x511fc774b16484ba2891078ae74ced1750b5fb52      1
     57 0x52439c4de8c10ef8904e96ad00171c0e86169dde      1
     58 0x52fc228959a904efbe127052face70373f8a8894      1
     59 0x59068075a799594db03c0255eed68e8e121155c8      1
     60 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     61 0x5bc926e531431b5a1a0f26e2dd4a7aa8f322b1ce      1
     62 0x5d4322057537eac2f0838063697724fbd4847fac      1
     63 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     64 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     65 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     66 0x6073ae7059582fb08ae030663c33d026b3ad0766      1
     67 0x6079696f3d9fc53ba8350d8db44e048e6a3814b1      1
     68 0x6286ce57c5a2ec8eafc09a1d74034770fd92d5ef      1
     69 0x63ed62de8944057087a19ea80eeed11a675cdfdc      1
     70 0x6422d607ca13457589a1f2dbf0ec63d5adf87bfb      1
     71 0x66c4f8f604f902939c6de0038d6ad6a561d4363d      1
     72 0x66ea2d335291a5afa8d3522eef6adfc3f8746e16      1
     73 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     74 0x6cad38cec0128055bd5768f729c9d5c85ca29470      1
     75 0x6d76cbd9762fc415f9d679bc557d6ffb9f46583b      1
     76 0x6e5033b09cb7de7dbfb6e7547cf274e09cff1ca9      1
     77 0x6f79a3d649db7cf1f6842f42b33374d8fbd13721      1
     78 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
     79 0x75874f95852b742ce3d571b5cc5e2fc4355cbd9a      1
     80 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     81 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     82 0x79e561168835c783240a0637320d308897bd0922      1
     83 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     84 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
     85 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     86 0x7f32973cfdef197e56cb5ddd2c9412505a629c92      1
     87 0x81a80a99c8ab3c57823d6235134979e8c13b2239      1
     88 0x84573d169d84f3bab429736ade19691e70359102      1
     89 0x877f3bcb3c0ad49b64c404970d5671e00231b59c      1
     90 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
     91 0x8ac236f975c5b44948c45c662834e28f017c7097      1
     92 0x9429b7d2a312b140e18981757b2b44b33881e76b      1
     93 0x94e8b6295be681a2c4ddad11f501e0fe9abb758a      1
     94 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
     95 0x95e26ef85c6af021d3106d1e6ac345faa7c6f663      1
     96 0x96deaeb8fd37527b0a97d56718ee614c8f73187d      1
     97 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
     98 0x9d5393c69a8caed1af722aba7ce1e42b5672160b      1
     99 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    100 0xa249a826ff225e7fffd5a6fc118bef4feea2633f      1
    101 0xa2dd9400107aeb8ed8a9fa638b46cc4d11c44053      1
    102 0xa52126809c92a62ba2f2966b5d8ce695a802da5e      1
    103 0xa6134eb7381978804646088df4bb42c5232d82da      1
    104 0xab42c346917f95431799271394c3e061c005886d      1
    105 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    106 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    107 0xbaca8a4528ce21edd8d3ff46220151427868653a      1
    108 0xbeb08de7778139fa4e0f1f2b8b0da88d997a1636      1
    109 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    110 0xc267da714ddb007bc198cb9bdb722c66e652c687      1
    111 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    112 0xc8e2cc6dd09d2f6201b0aabc295ffda3c4b99d7a      1
    113 0xc9f84ca819fac0bcf4cc58948ddd477385ce69a7      1
    114 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    115 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    116 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
    117 0xd211e839f02a450a05bedc52aafe520eb45efe59      1
    118 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    119 0xd63c2582b071f41090e89d975f738e87e7b8da50      1
    120 0xd6cd34f0d27d3942fb7aab193566bd76f694bf59      1
    121 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    122 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    123 0xdf43c410cf1bfb8bf4876e5f366bde6576babb38      1
    124 0xe0bac6d36e57000f1ad2eb8ca0e9cc16fdd5df81      1
    125 0xe0e8c1d735698060477e79a8e4c20276fc2ec7a7      1
    126 0xe3cbab0b6e9567c4685b26e3996b2a485aa30695      1
    127 0xe5451a8651ff622fb3a111633e0c709379aaf4a1      1
    128 0xe629117078d0849dce5e2c23b0ef40ab62a672a3      1
    129 0xe8222179ebf7c93940b63928fcfc31b1bd5c0bf5      1
    130 0xec18673439ca47cb2788f1a8a82ceab0dd59d2b7      1
    131 0xec234324276680b8de93c048dd69d68181c00356      1
    132 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    133 0xee74258438bcd2882fe907c91a5371dc5dd5b0ee      1
    134 0xefaf1fa1362a23729cb879ab475e923c651bd532      1
    135 0xf06f0c1d0610e2c7427a43b8873c808087497513      1
    136 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    137 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    138 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    139 0xf8bd02c5bdeb28bbb240355a19a9831c4d0db413      1
    140 0xf8be957f65e67fb0342992a51c30290d5758f880      1
    141 0xf8f9347ad38f607e2c510a211a563cf5c5f52e89      1
    142 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    143 0xfba206e73709f614e5a85aff27a98692d4f3c579      1
    144 0xfe5e101381727ba5a0e2cee531016192cd7d63cc      1
    145 0xfe6bae8676fdedb9a05735f4ab1adacd83501c30      1
    146 0xffe500927dbb171abddab384e1d1754338cca121      1

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
