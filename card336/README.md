
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:229         Length:229         Min.   :1.000   Length:229        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.048                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:229        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21900669 # https://etherscan.io/block/21900669
block_hash <- "0x2e8a411b79224f9b3959e62b2046ddae87a1093c0c5f0ce91b9db8b14c8d3bbe"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4743 

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

allow_artist1    <- pick(snapshot, contracts=c("DianeLindoFoundation","DianeLindoPart2","minimotions","DianeLindoManifold","NeonHorror","GalaxySeries111","DianeLindoSR"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KnownOriginEditions","Editions","minimotionsEditions","EditionsSeries"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("DianeLindoFoundation","DianeLindoPart2","minimotions","DianeLindoManifold","NeonHorror","GalaxySeries111","DianeLindoSR","KnownOriginEditions","Editions","minimotionsEditions","EditionsSeries"), address_remove=address_remove,address_max=1)
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
     1 0x06395478361f2e7c5aa26a380d37876eb5cc1854      1
     2 0x11e01777ebc4ad185ea3de4a9efcb50bcd92248d      1
     3 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
     4 0x16bd65bebf6c2a0fef8c76e9ebe665afbb0ae7a8      1
     5 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     6 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     7 0x2ab3d3b420ee2cb0fc20208c0dfd0ef2d39c1d75      1
     8 0x3016d104c55f1756020cb6c11a090648a34a7930      1
     9 0x389ccea690c98a9c2077442a90d796b9e5c067de      1
    10 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
    11 0x440a2cafc33bf77807746ed844d495c4e8f538d0      1
    12 0x501c8e70f1af355681aca5572d7ded26c188341c      1
    13 0x51e5152bcd6ebea8aa64578c8ba51e9c6d0bf435      1
    14 0x5fc64499864ab082756cc0fdfb0267d74b0dffb8      1
    15 0x655a7ba052e5acde220a482777981c050fa087fe      1
    16 0x7853010ab93f48ce78137703f7f0d21ace194d11      1
    17 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    18 0x7cad02c1e53decf5d1102a4b9bee5de79c6d2e18      1
    19 0x8e731a0ba3ad95d90ce743b898ae2eb8dc551196      1
    20 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    21 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    22 0xc120cd7cf154b135fa8d4391cc3df66865ed22f5      1
    23 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
    24 0xca14f692d729f2cae3f22576a7e67b1d08175b5d      1
    25 0xcfaa6bd6f4de08c289e24f0baa55e707cd14ddd0      1
    26 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    27 0xe69171feff9b15daa83c0f41edcfba8994ffcd57      1
    28 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
    29 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    30 0xf84fb8403f2a321f89d8c18d3a9629320263514c      1
    31 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 117 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      2 0x09464ad754f39578bcaeedd64bc61f911cec01bb      1
      3 0x09d36e24cab699ad357b63bf59da51ef9aee2c3e      1
      4 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
      5 0x0dccafbf01442ac219ba721e02a1b01352873b52      1
      6 0x136bbfe37988f82f8585ed155615b75371489d45      1
      7 0x13a71d05a46e943b8837a66aa6cdff1c59ea3565      1
      8 0x14ee4a8d3e9d7b980743730f6828d15fd3c04ac9      1
      9 0x16373e2bee34b8a238ee62d4e2bced31e4f78e6c      1
     10 0x179318748b758d6c99a851f10f6667c998da9358      1
     11 0x19a68e5a24ead474cdbd1e74864317cc7ac3afa1      1
     12 0x1b49e7c19632d27b3362151ef1f1818e5eab69f7      1
     13 0x1d99a93b99dbd1321a4022f6bbababd07ac485d9      1
     14 0x1dd01b89666b63e246e75ea9dd2659daa9d1092c      1
     15 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     16 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     17 0x209dac717d8b54e45bf8cd20a3a17a801fa621c6      1
     18 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     19 0x26a1e46832bd5ffd7d21481d185fc258a7080316      1
     20 0x2b909662427fbf80b5253c0ab9f810fd0a024e79      1
     21 0x2cb7e8edcb8994e7727d343560e914c05cac453f      1
     22 0x30559da92dcaf2786afcc71541b46dd292d274b5      1
     23 0x30d4e9569d3cae79c124ebc75f7e5c43e3c8367e      1
     24 0x32ab4262df2c10ccc199f5f8e7332ed507ef3ea5      1
     25 0x33be64b5921955387ddb08a31c3193461612b565      1
     26 0x3775669465352353f599f73586494747463f851b      1
     27 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     28 0x404d48d313c3e1f06fe1eff103bdb64459345354      1
     29 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     30 0x46136c057a523d9294f108616636d5a53367eaa7      1
     31 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     32 0x47676c02d1c8e42a4b86d4fe66164b94f60b1da2      1
     33 0x48aae1669d4cf7988a227b3d68d9d6e9f02840ee      1
     34 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
     35 0x4f41c1e394062ce9979de26d5ac0a5e72484bce5      1
     36 0x53bebd20781aaa3a831f45b3c6889010a706ff9f      1
     37 0x557c60995797fa7b47be105227a2e46148d85750      1
     38 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     39 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     40 0x5e7d0d9cca8018c4a71d7a01c303a30f36a116a7      1
     41 0x61b262695b5637a5367a153ed4168d2689f41bbe      1
     42 0x649ff45a74d3ffb86a31fcd1e9c88f46e8bb38a0      1
     43 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     44 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     45 0x6c1dbbb980f2a645850131f8144606bda727c339      1
     46 0x72a1033cfafbb17ce3c64e97af1eaf53cf7718f1      1
     47 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     48 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     49 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
     50 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     51 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     52 0x7c632d97febebafa33f12d750bfc378906341263      1
     53 0x7d461bb03e204ca5dd3eefeae6cae9febd823054      1
     54 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     55 0x80b389d7f56915f496a1b88e38f19a65516cf49d      1
     56 0x8186505fabdbe4765b14ba463aaaf6e46ea163ae      1
     57 0x839126d8b0ff719d5394fd288a55ab24b2a7baac      1
     58 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
     59 0x8648880327b5afb6aec1bc6c4d17ffb3323a50ae      1
     60 0x86d89966f507bbffddf9a5dff22ef5609b0c80dc      1
     61 0x870494cb199cc021f2b0a30325020574f7a4ffcc      1
     62 0x8751ac343552314ee263ef16d66351c04c380891      1
     63 0x87bf447ac29ba9498e5c0859513c39a0931f303a      1
     64 0x898952b4d071ead9d3bdcc5e430b17efc4996737      1
     65 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
     66 0x91d5482f54f2dfddf21caf8f5528f689397ae223      1
     67 0x92be9a3f9f5b3ba7d8d18430723d3c4f82539e74      1
     68 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
     69 0x9412813932e88103181561f122daa581dece8f2e      1
     70 0x98b5d46557e3e6c9b7367047433a035f91836668      1
     71 0x98ccf605c43a0bf9d6795c3cf3b5fed836330511      1
     72 0x99d8d9a3c90b94b8a09c9292615e086b22ef4fd5      1
     73 0x99f76a2380f9868586d748da81bced8fd28628f0      1
     74 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
     75 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     76 0x9f128b52128027dd6ea9892f741519979d36fa34      1
     77 0xa2f8d521c8cf328132e187378d545b2b37be2f31      1
     78 0xa4d8907bf757d268c9d054d8cc274e2f54e7c760      1
     79 0xa724b42c73986c5c80530dc613a9b4975e2cbac2      1
     80 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
     81 0xa8bb2ba63c0c0c02ee3926dd6061a9840dd4bcfc      1
     82 0xab2a94fe62a0604b4fd10ce2396b40fb0dc9d5ac      1
     83 0xab7473da0af4658ae2879cb641d82e7652721486      1
     84 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     85 0xaca333db2c8f8ef2142cf01c112cf2444b01004e      1
     86 0xad6bacd1590f5417c000e0e953395a067e76db54      1
     87 0xaed89baed67e83e14eaaa2ba9bb054d4e58b1642      1
     88 0xafbd69a1e9c961e1bdfcb132d6045d671e08c800      1
     89 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
     90 0xb3e12a0ac4fb13fe091b448f45b81b1da0246994      1
     91 0xb867ba86746b4b3268508b014a3fefd2a938cb27      1
     92 0xbcc3c9b8b3166fe39a305318cb98f01835777141      1
     93 0xbf00caee3f4d0e654b5e1a557914d257f126d055      1
     94 0xc1202b2da243467882439944885339f9fd71279c      1
     95 0xc1a3cc1dbb944139ae91b349fa75e14101b10eef      1
     96 0xc2e40203fca8e5d1f845ab6d9395c2915cf4a393      1
     97 0xc329fdc037fe51cf176b360a46533dde9cd5e423      1
     98 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
     99 0xc560ba6c949713be4360193f77ec4855fa7cfea2      1
    100 0xc70b5a19b1e3af6019d612a7d152e5b7f1f83268      1
    101 0xc77b9a1efda8f3d87022cc8d323225a5a67fa902      1
    102 0xca76f775c7f1fc5d3309234cae90980ebf554150      1
    103 0xcb633142fceee99d560aa8849db67eff2ef5871a      1
    104 0xcd148897d736801f1db7109821b9e4366ac266d9      1
    105 0xd2b481b6e48f9ef4204eb6061ba7cf33dadcc286      1
    106 0xd2d15a46a6a864e6070cbe05fdb7e32636ba0048      1
    107 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    108 0xd5991743b0a3896a2afd5aae6cadbfb0df173939      1
    109 0xd6f38b314c0e4baef7b40c328420b0005f50d992      1
    110 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    111 0xdc613e6ecba50027235ac03ab1a844b441e57b43      1
    112 0xde1da096a49e503906bd83ab8d10b2c5026df1fa      1
    113 0xe4bac4bc44f1a97c781bf9de9c06124bcc05ee39      1
    114 0xf2c0149f0cff4c19b9819d1084f465df0e1b3795      1
    115 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    116 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    117 0xffba9936f514654b467b5530d444088c46672fc7      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 148 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x06395478361f2e7c5aa26a380d37876eb5cc1854      1
      2 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      3 0x09464ad754f39578bcaeedd64bc61f911cec01bb      1
      4 0x09d36e24cab699ad357b63bf59da51ef9aee2c3e      1
      5 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
      6 0x0dccafbf01442ac219ba721e02a1b01352873b52      1
      7 0x11e01777ebc4ad185ea3de4a9efcb50bcd92248d      1
      8 0x136bbfe37988f82f8585ed155615b75371489d45      1
      9 0x13a71d05a46e943b8837a66aa6cdff1c59ea3565      1
     10 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
     11 0x14ee4a8d3e9d7b980743730f6828d15fd3c04ac9      1
     12 0x16373e2bee34b8a238ee62d4e2bced31e4f78e6c      1
     13 0x16bd65bebf6c2a0fef8c76e9ebe665afbb0ae7a8      1
     14 0x179318748b758d6c99a851f10f6667c998da9358      1
     15 0x19a68e5a24ead474cdbd1e74864317cc7ac3afa1      1
     16 0x1b49e7c19632d27b3362151ef1f1818e5eab69f7      1
     17 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     18 0x1d99a93b99dbd1321a4022f6bbababd07ac485d9      1
     19 0x1dd01b89666b63e246e75ea9dd2659daa9d1092c      1
     20 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     21 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     22 0x209dac717d8b54e45bf8cd20a3a17a801fa621c6      1
     23 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     24 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     25 0x26a1e46832bd5ffd7d21481d185fc258a7080316      1
     26 0x2ab3d3b420ee2cb0fc20208c0dfd0ef2d39c1d75      1
     27 0x2b909662427fbf80b5253c0ab9f810fd0a024e79      1
     28 0x2cb7e8edcb8994e7727d343560e914c05cac453f      1
     29 0x3016d104c55f1756020cb6c11a090648a34a7930      1
     30 0x30559da92dcaf2786afcc71541b46dd292d274b5      1
     31 0x30d4e9569d3cae79c124ebc75f7e5c43e3c8367e      1
     32 0x32ab4262df2c10ccc199f5f8e7332ed507ef3ea5      1
     33 0x33be64b5921955387ddb08a31c3193461612b565      1
     34 0x3775669465352353f599f73586494747463f851b      1
     35 0x389ccea690c98a9c2077442a90d796b9e5c067de      1
     36 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     37 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
     38 0x404d48d313c3e1f06fe1eff103bdb64459345354      1
     39 0x440a2cafc33bf77807746ed844d495c4e8f538d0      1
     40 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     41 0x46136c057a523d9294f108616636d5a53367eaa7      1
     42 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     43 0x47676c02d1c8e42a4b86d4fe66164b94f60b1da2      1
     44 0x48aae1669d4cf7988a227b3d68d9d6e9f02840ee      1
     45 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
     46 0x4f41c1e394062ce9979de26d5ac0a5e72484bce5      1
     47 0x501c8e70f1af355681aca5572d7ded26c188341c      1
     48 0x51e5152bcd6ebea8aa64578c8ba51e9c6d0bf435      1
     49 0x53bebd20781aaa3a831f45b3c6889010a706ff9f      1
     50 0x557c60995797fa7b47be105227a2e46148d85750      1
     51 0x569ff93aec513dac5b818714268c4b1e49abcc40      1
     52 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     53 0x5e7d0d9cca8018c4a71d7a01c303a30f36a116a7      1
     54 0x5fc64499864ab082756cc0fdfb0267d74b0dffb8      1
     55 0x61b262695b5637a5367a153ed4168d2689f41bbe      1
     56 0x649ff45a74d3ffb86a31fcd1e9c88f46e8bb38a0      1
     57 0x655a7ba052e5acde220a482777981c050fa087fe      1
     58 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     59 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     60 0x6c1dbbb980f2a645850131f8144606bda727c339      1
     61 0x72a1033cfafbb17ce3c64e97af1eaf53cf7718f1      1
     62 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     63 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     64 0x77be5849ae7b1dd09cc1715b670a39ea18a92999      1
     65 0x7853010ab93f48ce78137703f7f0d21ace194d11      1
     66 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     67 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
     68 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     69 0x7c632d97febebafa33f12d750bfc378906341263      1
     70 0x7cad02c1e53decf5d1102a4b9bee5de79c6d2e18      1
     71 0x7d461bb03e204ca5dd3eefeae6cae9febd823054      1
     72 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     73 0x80b389d7f56915f496a1b88e38f19a65516cf49d      1
     74 0x8186505fabdbe4765b14ba463aaaf6e46ea163ae      1
     75 0x839126d8b0ff719d5394fd288a55ab24b2a7baac      1
     76 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
     77 0x8648880327b5afb6aec1bc6c4d17ffb3323a50ae      1
     78 0x86d89966f507bbffddf9a5dff22ef5609b0c80dc      1
     79 0x870494cb199cc021f2b0a30325020574f7a4ffcc      1
     80 0x8751ac343552314ee263ef16d66351c04c380891      1
     81 0x87bf447ac29ba9498e5c0859513c39a0931f303a      1
     82 0x898952b4d071ead9d3bdcc5e430b17efc4996737      1
     83 0x8e731a0ba3ad95d90ce743b898ae2eb8dc551196      1
     84 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
     85 0x91d5482f54f2dfddf21caf8f5528f689397ae223      1
     86 0x92be9a3f9f5b3ba7d8d18430723d3c4f82539e74      1
     87 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
     88 0x9412813932e88103181561f122daa581dece8f2e      1
     89 0x98b5d46557e3e6c9b7367047433a035f91836668      1
     90 0x98ccf605c43a0bf9d6795c3cf3b5fed836330511      1
     91 0x99d8d9a3c90b94b8a09c9292615e086b22ef4fd5      1
     92 0x99f76a2380f9868586d748da81bced8fd28628f0      1
     93 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
     94 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
     95 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     96 0x9f128b52128027dd6ea9892f741519979d36fa34      1
     97 0xa2f8d521c8cf328132e187378d545b2b37be2f31      1
     98 0xa4d8907bf757d268c9d054d8cc274e2f54e7c760      1
     99 0xa724b42c73986c5c80530dc613a9b4975e2cbac2      1
    100 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    101 0xa8bb2ba63c0c0c02ee3926dd6061a9840dd4bcfc      1
    102 0xab2a94fe62a0604b4fd10ce2396b40fb0dc9d5ac      1
    103 0xab7473da0af4658ae2879cb641d82e7652721486      1
    104 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    105 0xaca333db2c8f8ef2142cf01c112cf2444b01004e      1
    106 0xad6bacd1590f5417c000e0e953395a067e76db54      1
    107 0xaed89baed67e83e14eaaa2ba9bb054d4e58b1642      1
    108 0xafbd69a1e9c961e1bdfcb132d6045d671e08c800      1
    109 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
    110 0xb3e12a0ac4fb13fe091b448f45b81b1da0246994      1
    111 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    112 0xb867ba86746b4b3268508b014a3fefd2a938cb27      1
    113 0xbcc3c9b8b3166fe39a305318cb98f01835777141      1
    114 0xbf00caee3f4d0e654b5e1a557914d257f126d055      1
    115 0xc1202b2da243467882439944885339f9fd71279c      1
    116 0xc120cd7cf154b135fa8d4391cc3df66865ed22f5      1
    117 0xc1a3cc1dbb944139ae91b349fa75e14101b10eef      1
    118 0xc2e40203fca8e5d1f845ab6d9395c2915cf4a393      1
    119 0xc329fdc037fe51cf176b360a46533dde9cd5e423      1
    120 0xc4e60829d772ca5f566e45211becbf5f4cc763b5      1
    121 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
    122 0xc560ba6c949713be4360193f77ec4855fa7cfea2      1
    123 0xc70b5a19b1e3af6019d612a7d152e5b7f1f83268      1
    124 0xc77b9a1efda8f3d87022cc8d323225a5a67fa902      1
    125 0xca14f692d729f2cae3f22576a7e67b1d08175b5d      1
    126 0xca76f775c7f1fc5d3309234cae90980ebf554150      1
    127 0xcb633142fceee99d560aa8849db67eff2ef5871a      1
    128 0xcd148897d736801f1db7109821b9e4366ac266d9      1
    129 0xcfaa6bd6f4de08c289e24f0baa55e707cd14ddd0      1
    130 0xd2b481b6e48f9ef4204eb6061ba7cf33dadcc286      1
    131 0xd2d15a46a6a864e6070cbe05fdb7e32636ba0048      1
    132 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    133 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    134 0xd5991743b0a3896a2afd5aae6cadbfb0df173939      1
    135 0xd6f38b314c0e4baef7b40c328420b0005f50d992      1
    136 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    137 0xdc613e6ecba50027235ac03ab1a844b441e57b43      1
    138 0xde1da096a49e503906bd83ab8d10b2c5026df1fa      1
    139 0xe4bac4bc44f1a97c781bf9de9c06124bcc05ee39      1
    140 0xe69171feff9b15daa83c0f41edcfba8994ffcd57      1
    141 0xe6d6758d2fc0a7a0c476794855d035f3bcba245f      1
    142 0xf2c0149f0cff4c19b9819d1084f465df0e1b3795      1
    143 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    144 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    145 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    146 0xf84fb8403f2a321f89d8c18d3a9629320263514c      1
    147 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1
    148 0xffba9936f514654b467b5530d444088c46672fc7      1

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
