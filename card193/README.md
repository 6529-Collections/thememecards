
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:217         Length:217         Min.   :1   Length:217        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:217        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19096269 # https://etherscan.io/block/19096269
block_hash <- "0x7852999869b47b41f455f98a0749adfea09a27ebedfa29be9bd08b11b744e65c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4736 

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
  "0xfC126dB15dcb6ed254a4090D896EbD6B04b21F7c"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","NightMood","TheCreationofShadowandLight","TravelDiary","LetsPlayaGame","FourSeasons","Foundation2","ForgottenClassic"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("Rarible","KnownOrigin","KnownOrigin2"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","NightMood","TheCreationofShadowandLight","TravelDiary","LetsPlayaGame","FourSeasons","Foundation2","ForgottenClassic","Rarible","KnownOrigin","KnownOrigin2"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 43 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     2 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
     3 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     4 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     5 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     6 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     7 0x3e099af007cab8233d44782d8e6fe80fecdc321e      1
     8 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     9 0x3e2f087699c17528eb09ab91f3bf268a0df20cd9      1
    10 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    11 0x44613705c2f17da81a9c3e01d703cf5c07110c42      1
    12 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
    13 0x4a9ac6903a33258475be9fcfdf9d7814053a8aff      1
    14 0x4cd6b514ad7d34e2876dd5122d8ebcdb272ce6e4      1
    15 0x4fd8c6a71a924fcc890402cecc8bc0cad9c49158      1
    16 0x533fa4a37aaffdb02456d374237091520790383e      1
    17 0x5c112c552c19db682e7d30ad06169138b5c90317      1
    18 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    19 0x60c4e51a1c797f562e628cddb4aeb84ac9f27d41      1
    20 0x62607d87b5793859fb80ac96c743e8e1df4e4905      1
    21 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    22 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    23 0x7a9fe22691c811ea339d9b73150e6911a5343dca      1
    24 0x84f18c8cea5cb28e9b7063cfc49eb914e80c0f81      1
    25 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    26 0x8c7bdcd5e325215b2e3954ad2e9a75be3b4733a9      1
    27 0x95b043e70b6fece9b4ed4a28c5e4ac9bffa28da8      1
    28 0x98cb6107a67da3375f16c304aeffe1fcc0b1239f      1
    29 0xa2c62a66f6660166838b95db60f234dfb59e765e      1
    30 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    31 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    32 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    33 0xb826f949a9a0360f1649bf3d8b72ecc3f780c81a      1
    34 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    35 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    36 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    37 0xdbbb17f800e93302100faa6f62fa358c1ae186ca      1
    38 0xe1ccd64c452096538b07fc68e89196fb6309e01c      1
    39 0xe3f663418251186888935dc1c4979fa3a3da1bac      1
    40 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    41 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    42 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    43 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x05e57519ad55beebeb200871cefcdd84b78b9cbd      1
     2 0x07b7379f9ba4c139e51354c785e47a64a6770acc      1
     3 0x0a87b1042a88c41e306953562b1e76ac13c6281e      1
     4 0x10491f92a279883a028e49281265b61040b6b433      1
     5 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
     6 0x14287e62b859a3a5e19b3c2d59ed1f12ac94ba4c      1
     7 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     8 0x158f25e42485952f063d4f5724339d4ced376432      1
     9 0x15fded1602c9cce36a464c9e75521dfe2d72161e      1
    10 0x1b65d7940eefd7ee53ea3fba8fd54bae255f8fdc      1
    11 0x1c5104877f2d7f71a5a007e66e852886e44687ae      1
    12 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
    13 0x1e6a0856fd670d91223864d692db85fc0b5ac2dc      1
    14 0x22e2ef078ef1ccf0a75d113a8b8389cccf746ff9      1
    15 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    16 0x393e5fc4ccbc1788e364f3496d4d6cdf13b6b077      1
    17 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
    18 0x3c8221321441b08c580506e21899e3fa88943672      1
    19 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
    20 0x4a4f1c998b89c0dc000983ffd997420dad282ca1      1
    21 0x4fcff4e12bd67f6c1fcf4be6499535c56d196ea3      1
    22 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
    23 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
    24 0x583aef35c4d5246ceb27705e3feda06bd30a955c      1
    25 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
    26 0x5cf613453aadf6b42dd41c5083bb4a1fa5f6336d      1
    27 0x632463e42b3bd9be625f6008934fc2770fcde2c3      1
    28 0x648a6e076f8231649f23f07e41bff5464afdd31d      1
    29 0x668248df4595e09aa253b31478312748078f7a20      1
    30 0x688ec9b0b8f8998f6423eb8a2b002364ddcf6cde      1
    31 0x6aa0b2891c3baa6c134efc7aa5b18747bc9cecf2      1
    32 0x6b18c84077901521027362c4b0b5fa0f78886263      1
    33 0x6c3675c755c06b236cb10b5acfa0445fd8aad455      1
    34 0x6ceca7911c1a4dd84451716b698995324609ad48      1
    35 0x6db5e720a947c7cc2f3fcad4cf5058402fc456c6      1
    36 0x6dd8dbddf1086b83a2d45497408188afcdda7ecc      1
    37 0x6faf9b4bd89642410ac0acbb591ac891ff1eff40      1
    38 0x6fe138faec9448c9219b67cb309e42c83c9cdf37      1
    39 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
    40 0x7075c58e3aa7c1a2de6a47820aa5d8581c40cd0f      1
    41 0x73347bdf5ab7a4a1376add59967874730d0dc647      1
    42 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    43 0x7e745c861b2b23f5e69f109d0d2f858e9131ecfa      1
    44 0x80b389d7f56915f496a1b88e38f19a65516cf49d      1
    45 0x8e04f3f772b4250c060e1b87fd28e420999699e7      1
    46 0x94a4e675141752ec84749448417a285319605867      1
    47 0xa1447daa4d91e1c75eff3ee80b33e12935fbbec0      1
    48 0xa25252fe35e98cff49a162a43efe2f9873b325f8      1
    49 0xa73b66af2ee0c0e2c7c6392ed6352ad0d3b05e33      1
    50 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
    51 0xab98594140d7dc21070fbe4106da840bdbe7494e      1
    52 0xaba5fd212294e95201adac9d2c9d5fe7f539dd02      1
    53 0xacbacca81254c36445488a0e4ff0bf5a516147ae      1
    54 0xb5288c69311dda764652c0596c3b2cf5582fb195      1
    55 0xb5d74cc9099cd572683f60ac1e78c7dca03570e5      1
    56 0xb719e142b085b47ca19905f0c0c325c5f937acfb      1
    57 0xb9e8e3135b87a55e25febc411d2450ea63476bc6      1
    58 0xbbba14a63a97d30d4b1e2fd221ee97bb068fad40      1
    59 0xbde7f803064da6179162a624570a398d53e6e5ea      1
    60 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    61 0xc32dd5e3395b12b4c200e110898585abcc60d95a      1
    62 0xc3feb2e38750b847fc5f73c40f02b389183ce905      1
    63 0xc750a405983a615c09e52541e3fef805a91f8d15      1
    64 0xcbe63ab8a9043059a124b38641623019b770016d      1
    65 0xd0f61674d6102ca71b5a41ccf774d42cfafe04de      1
    66 0xd2927a91570146218ed700566df516d67c5ecfab      1
    67 0xd7d743254bf6bae1a509f96d0369eb7f45a6f190      1
    68 0xd80775766186ef44c73422fdf97d92701c27f70e      1
    69 0xd95476c57b41cbdf1b19a49d4520c19ab3fab707      1
    70 0xda508d7aa1fc9ad252bed8bef26aac448b1c8e14      1
    71 0xe0b6310bb930a428eee16d5e6623e6f9182a5b6c      1
    72 0xe3699f1cb3c3a6dc95e5a1ec962f0118d8dabae9      1
    73 0xe3e529a4d91b6ffb851230ee96f087a7e00e2629      1
    74 0xe616e04c1237eff59f168626e11e6b8ca082f73d      1
    75 0xe9c69bd31dc6bb0ae6ae0dd059c9350a7eab3915      1
    76 0xeb393fdfacd3fa6970bf634966468cde5c9c30a9      1
    77 0xf55cbf08ae9424c572598b29064b2fc3f67add04      1
    78 0xf63f8baef30c1ef2f83d3142e8f952b5afbf62d5      1
    79 0xfc126db15dcb6ed254a4090d896ebd6b04b21f7c      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 122 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      2 0x05e57519ad55beebeb200871cefcdd84b78b9cbd      1
      3 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
      4 0x07b7379f9ba4c139e51354c785e47a64a6770acc      1
      5 0x0a87b1042a88c41e306953562b1e76ac13c6281e      1
      6 0x10491f92a279883a028e49281265b61040b6b433      1
      7 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
      8 0x14287e62b859a3a5e19b3c2d59ed1f12ac94ba4c      1
      9 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     10 0x158f25e42485952f063d4f5724339d4ced376432      1
     11 0x15fded1602c9cce36a464c9e75521dfe2d72161e      1
     12 0x1b65d7940eefd7ee53ea3fba8fd54bae255f8fdc      1
     13 0x1c5104877f2d7f71a5a007e66e852886e44687ae      1
     14 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     15 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     16 0x1e6a0856fd670d91223864d692db85fc0b5ac2dc      1
     17 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     18 0x22e2ef078ef1ccf0a75d113a8b8389cccf746ff9      1
     19 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     20 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     21 0x393e5fc4ccbc1788e364f3496d4d6cdf13b6b077      1
     22 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     23 0x3c8221321441b08c580506e21899e3fa88943672      1
     24 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     25 0x3e099af007cab8233d44782d8e6fe80fecdc321e      1
     26 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     27 0x3e2f087699c17528eb09ab91f3bf268a0df20cd9      1
     28 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     29 0x44613705c2f17da81a9c3e01d703cf5c07110c42      1
     30 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
     31 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     32 0x4a4f1c998b89c0dc000983ffd997420dad282ca1      1
     33 0x4a9ac6903a33258475be9fcfdf9d7814053a8aff      1
     34 0x4cd6b514ad7d34e2876dd5122d8ebcdb272ce6e4      1
     35 0x4fcff4e12bd67f6c1fcf4be6499535c56d196ea3      1
     36 0x4fd8c6a71a924fcc890402cecc8bc0cad9c49158      1
     37 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
     38 0x533fa4a37aaffdb02456d374237091520790383e      1
     39 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
     40 0x583aef35c4d5246ceb27705e3feda06bd30a955c      1
     41 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
     42 0x5c112c552c19db682e7d30ad06169138b5c90317      1
     43 0x5cf613453aadf6b42dd41c5083bb4a1fa5f6336d      1
     44 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
     45 0x60c4e51a1c797f562e628cddb4aeb84ac9f27d41      1
     46 0x62607d87b5793859fb80ac96c743e8e1df4e4905      1
     47 0x632463e42b3bd9be625f6008934fc2770fcde2c3      1
     48 0x648a6e076f8231649f23f07e41bff5464afdd31d      1
     49 0x668248df4595e09aa253b31478312748078f7a20      1
     50 0x688ec9b0b8f8998f6423eb8a2b002364ddcf6cde      1
     51 0x6aa0b2891c3baa6c134efc7aa5b18747bc9cecf2      1
     52 0x6b18c84077901521027362c4b0b5fa0f78886263      1
     53 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     54 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     55 0x6c3675c755c06b236cb10b5acfa0445fd8aad455      1
     56 0x6ceca7911c1a4dd84451716b698995324609ad48      1
     57 0x6db5e720a947c7cc2f3fcad4cf5058402fc456c6      1
     58 0x6dd8dbddf1086b83a2d45497408188afcdda7ecc      1
     59 0x6faf9b4bd89642410ac0acbb591ac891ff1eff40      1
     60 0x6fe138faec9448c9219b67cb309e42c83c9cdf37      1
     61 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
     62 0x7075c58e3aa7c1a2de6a47820aa5d8581c40cd0f      1
     63 0x73347bdf5ab7a4a1376add59967874730d0dc647      1
     64 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
     65 0x7a9fe22691c811ea339d9b73150e6911a5343dca      1
     66 0x7e745c861b2b23f5e69f109d0d2f858e9131ecfa      1
     67 0x80b389d7f56915f496a1b88e38f19a65516cf49d      1
     68 0x84f18c8cea5cb28e9b7063cfc49eb914e80c0f81      1
     69 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
     70 0x8c7bdcd5e325215b2e3954ad2e9a75be3b4733a9      1
     71 0x8e04f3f772b4250c060e1b87fd28e420999699e7      1
     72 0x94a4e675141752ec84749448417a285319605867      1
     73 0x95b043e70b6fece9b4ed4a28c5e4ac9bffa28da8      1
     74 0x98cb6107a67da3375f16c304aeffe1fcc0b1239f      1
     75 0xa1447daa4d91e1c75eff3ee80b33e12935fbbec0      1
     76 0xa25252fe35e98cff49a162a43efe2f9873b325f8      1
     77 0xa2c62a66f6660166838b95db60f234dfb59e765e      1
     78 0xa73b66af2ee0c0e2c7c6392ed6352ad0d3b05e33      1
     79 0xa98d2867de6b1dff44fe4b355dea098e81d06aeb      1
     80 0xab98594140d7dc21070fbe4106da840bdbe7494e      1
     81 0xaba5fd212294e95201adac9d2c9d5fe7f539dd02      1
     82 0xacbacca81254c36445488a0e4ff0bf5a516147ae      1
     83 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     84 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
     85 0xb5288c69311dda764652c0596c3b2cf5582fb195      1
     86 0xb5d74cc9099cd572683f60ac1e78c7dca03570e5      1
     87 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
     88 0xb719e142b085b47ca19905f0c0c325c5f937acfb      1
     89 0xb826f949a9a0360f1649bf3d8b72ecc3f780c81a      1
     90 0xb9e8e3135b87a55e25febc411d2450ea63476bc6      1
     91 0xbbba14a63a97d30d4b1e2fd221ee97bb068fad40      1
     92 0xbde7f803064da6179162a624570a398d53e6e5ea      1
     93 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     94 0xc32dd5e3395b12b4c200e110898585abcc60d95a      1
     95 0xc3feb2e38750b847fc5f73c40f02b389183ce905      1
     96 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
     97 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
     98 0xc750a405983a615c09e52541e3fef805a91f8d15      1
     99 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    100 0xcbe63ab8a9043059a124b38641623019b770016d      1
    101 0xd0f61674d6102ca71b5a41ccf774d42cfafe04de      1
    102 0xd2927a91570146218ed700566df516d67c5ecfab      1
    103 0xd7d743254bf6bae1a509f96d0369eb7f45a6f190      1
    104 0xd80775766186ef44c73422fdf97d92701c27f70e      1
    105 0xd95476c57b41cbdf1b19a49d4520c19ab3fab707      1
    106 0xda508d7aa1fc9ad252bed8bef26aac448b1c8e14      1
    107 0xdbbb17f800e93302100faa6f62fa358c1ae186ca      1
    108 0xe0b6310bb930a428eee16d5e6623e6f9182a5b6c      1
    109 0xe1ccd64c452096538b07fc68e89196fb6309e01c      1
    110 0xe3699f1cb3c3a6dc95e5a1ec962f0118d8dabae9      1
    111 0xe3e529a4d91b6ffb851230ee96f087a7e00e2629      1
    112 0xe3f663418251186888935dc1c4979fa3a3da1bac      1
    113 0xe616e04c1237eff59f168626e11e6b8ca082f73d      1
    114 0xe9c69bd31dc6bb0ae6ae0dd059c9350a7eab3915      1
    115 0xeb393fdfacd3fa6970bf634966468cde5c9c30a9      1
    116 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    117 0xed83f0bceef992fa58c8bc8a27269eedb3574922      1
    118 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    119 0xf55cbf08ae9424c572598b29064b2fc3f67add04      1
    120 0xf63f8baef30c1ef2f83d3142e8f952b5afbf62d5      1
    121 0xfc126db15dcb6ed254a4090d896ebd6b04b21f7c      1
    122 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1

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
