
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance       contract        
     Length:195         Length:195         Min.   :1.00   Length:195        
     Class :character   Class :character   1st Qu.:1.00   Class :character  
     Mode  :character   Mode  :character   Median :1.00   Mode  :character  
                                           Mean   :1.01                     
                                           3rd Qu.:1.00                     
                                           Max.   :3.00                     
         name          
     Length:195        
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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation","PointsinTime","cloudburst","FutureRelics"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("Anthem2Editions","Anthem1Editions","ProofofSteakEditions","CarnablesEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","PointsinTime","cloudburst","FutureRelics","Anthem2Editions","Anthem1Editions","ProofofSteakEditions","CarnablesEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 66 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
     2 0x0ae44fc0e1c6e86e5de1ace32057a44bfe85c9ea      1
     3 0x0dda698d2fe2fc1fb8f5b54ee9cd77fbd5a1d08b      1
     4 0x113d754ff2e6ca9fd6ab51932493e4f9dabdf596      1
     5 0x163c58774b6cf4df98b66ee3835f41ba7175f13c      1
     6 0x1b4b3220292a30089061ce4d774d3a622dbae7d4      1
     7 0x1d4e759ee4d927e06ada91edb98002a5f8fdd3c4      1
     8 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     9 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
    10 0x3827014f2236519f1101ae2e136985e0e603be79      1
    11 0x38488dca8dccfeebfd3cc298436134e3b3af6b8b      1
    12 0x39d5dcbca5b24c98e295c4979a5f84a9ee39061a      1
    13 0x3a6372b2013f9876a84761187d933dee0653e377      1
    14 0x3a80d39ea6d316edef9ba0c847305b76ce2b5e58      1
    15 0x3c4e0d0f2f47b27a3e60022ee1aee97b59c05a07      1
    16 0x3d9d40c87c1c98385b0909e97fc4b82e10522741      1
    17 0x469bd81594503d01ea9b1922ccd56c302fce17c7      1
    18 0x478087e12db15302a364c64cdb79f14ae6c5c9b7      1
    19 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
    20 0x4bf3805b23c99f8e0a5797e86fd0232a04a2a629      1
    21 0x511ef2d9d1b08b0bda6770448aca66df803998d4      1
    22 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    23 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    24 0x6278e4fe0e4670eac88014d6326f079b4d02d73c      1
    25 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
    26 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
    27 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
    28 0x73d4e8632ba37cc9bf9de5045e3e8834f78efa85      1
    29 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    30 0x780737124207f40dbb2bcad58594fead6ae12db9      1
    31 0x7b27c0cfabd0941577624edddc8ad0fd12fd7480      1
    32 0x7d05f69b2a2feb99c110dda9029a65b2f96d9910      1
    33 0x80a32851a6caa6a6126e2f4e5d50aaf3d4bcf40b      1
    34 0x81b98a0e207726584ce1ac687fcae6059b35ebcf      1
    35 0x8490b3dfba40b784e3a16974377c70a139306cfa      1
    36 0x887b86b6b6957f7bbea88b8cefd392f39236a88c      1
    37 0x916b46c8360eff50e5be4e108c33c17831f700c3      1
    38 0x9cee1b1d787bc70d8e4c00d14e25088ef66deb97      1
    39 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    40 0xa3ded378678d735eb14ad78db64887386932c62b      1
    41 0xabb89dd8d2ecb0b872811a80b9ea8fb9b45562b3      1
    42 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
    43 0xb2b557cce11a9d05d4df1dcb89fec2a6412b4b53      1
    44 0xb4d502361a3c6f823eeb9a99af09e110382206ee      1
    45 0xba8e220834c32fac376bbfb33820c001f022d72e      1
    46 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    47 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    48 0xc3edcbe0f93a6258c3933e86ffaa3bcf12f8d695      1
    49 0xc665a60f22dda926b920deb8ffac0ef9d8a17460      1
    50 0xd20bf69b592b9176e46ec229f82d7f1bf65fc9b2      1
    51 0xd573becb6a6b0a0d43065d468d07787ca65daf8a      1
    52 0xd62c014b45a35f78bcf465d75ada029c94fdabf9      1
    53 0xd62e3a24c71e124d126193e1db64c3f97abcf39b      1
    54 0xd743b27d75a6dcdb34bafc6de0acb8cecb3ac43f      1
    55 0xd773e2f5b5ffe1a8296984e53015d803ef989bb0      1
    56 0xda3845b44736b57e05ee80fc011a52a9c777423a      1
    57 0xe0de9065b602ffc0d30c812c4d77e56394320797      1
    58 0xe71d8ded9dee1187e25a2d0fca8c8b050559d7d3      1
    59 0xea310c966d3ff5e09c65487f1763b21361eb71ef      1
    60 0xecc44bb21c355a23c4a923354b67c6d97d3576b9      1
    61 0xee99987a3ca0cdc7b620f9c14f6c18f3cbb9e055      1
    62 0xf36480d3471dc81b0609c950e6cfde4bb014b576      1
    63 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    64 0xfa398d672936dcf428116f687244034961545d91      1
    65 0xfb7c8a716d8c9577ea293a28ff592bb967ea84c2      1
    66 0xfff5086e00bc92ee04826b0f5398ebbdb8ea4000      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 73 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x007f6d2808d556c665864f32df11a044e6b27c21      1
     2 0x0360b9acda197703cb3040715c75659c38f13cf7      1
     3 0x05f13a948a4c345e4921adb4ec12f9c76ee3d331      1
     4 0x0808d22c75783042babfa002b5127dd42e9e37c5      1
     5 0x1022245eb550f9778209ca4514dcb693b45498be      1
     6 0x12efdcbbcdd6695109ada38e33c56173c2ffe2c3      1
     7 0x1832bce19ba35763fbd3ab1d5238de07372a7825      1
     8 0x1ae50668be1f32179bce00eb203121f00907d808      1
     9 0x1b967d5a3a14f661cc3bb0632cda3e152765ea32      1
    10 0x2487140216d7a54c27d2d75dd672a72059679df8      1
    11 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
    12 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
    13 0x3020ae2d2bc994a1ae488a5bd11e3899a4e6f584      1
    14 0x32585799d538ae8f8aa49a233f69cf168bd20447      1
    15 0x3a8a9caf13b05159569f829377ce151a2edc8fb8      1
    16 0x3e2dbda5f310aade1503557b2e7f356eaf161641      1
    17 0x41edfbdde3b9b22678937a956690362668dc7765      1
    18 0x479b537c591877b7b6f7a084e9b07d90c4bff688      1
    19 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    20 0x4a22910f4db3e7bf000eacfa06126d5db7f0efd5      1
    21 0x4fc21517878b88dbee76f93cff2a3eab0d6304a3      1
    22 0x504eec66fa2ba8d44163ad484119db60fd59c70c      1
    23 0x516656beacd4c41f4e1a2273263ff4995d0fd1d3      1
    24 0x5223e83500c6a30f8cb765af2865738b6bd5c438      1
    25 0x53aee4229e3f77383a1436c64138ca68f31e2995      1
    26 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    27 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    28 0x5fd403fc29ddb91842d3c3f42e4dd1be2a1f5e37      1
    29 0x63784c971e102bce755b140680de86404843aedb      1
    30 0x6527939c107c507e22cec1f6740719a26a642ec1      1
    31 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    32 0x6892274bb66893113dd440fd6dff3d4ff98b9d30      1
    33 0x6aa9f1775f7e0f8b8663e0729b7d64e4fb61c9cc      1
    34 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
    35 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
    36 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    37 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    38 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    39 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    40 0x8261b63b07d1ad92aeb7cdd916bb9a25236ceb65      1
    41 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
    42 0x854a8a5fc02cf0472061e5f9bf909fde28fc5f0e      1
    43 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    44 0x89bedcde29266fe29e7efd2ff943992042162d50      1
    45 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    46 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    47 0x97ddb656b2b18c9aa46f998da5b22963c8df9fac      1
    48 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
    49 0x9f3051ccb829f3452170d42a683c4d765dacfe32      1
    50 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    51 0xa7b32f0c55bfe631a9470a6b3312a6685ffd9f45      1
    52 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    53 0xaa13d55dcebfb2ba20113be03cd9d0c06bf8c1d5      1
    54 0xabb94dbdd360cc0ee92b59420a49019e760e32b3      1
    55 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
    56 0xb2b40044be8d564911176c7c31fee1d083d6d7a7      1
    57 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    58 0xbc10cd06b201ddbe419119b4bfb5a7dac24c2753      1
    59 0xbdbbbc0b1df75bdf767df6037e4ff7bec052b51d      1
    60 0xc1003fd0f7126333ace9772f6dee41f0e9d21205      1
    61 0xc62817d4d11a848d33d3fe3e96be76bffd73dea3      1
    62 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    63 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    64 0xd69a4f613c03e991678d990a06904fab28662f08      1
    65 0xd99b5a7c7d57c3d93533dcb5034cb64dcecf995d      1
    66 0xe09e1349e8342c77961dc2855c1e1f7236eab6f7      1
    67 0xe1e0da672b12f7d0d025f4b52512ab1678b2c7fd      1
    68 0xe876275e5e1a77e0de4006e5d9b85b33621d1442      1
    69 0xefb409f69f833536362d6077f14b0a51efd9d468      1
    70 0xf47ff49820805577bc6df1c636382564dfa0a791      1
    71 0xf95ea2e425f3346204280476864e6b1d9baa4b2b      1
    72 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1
    73 0xfbda58bcaafe5242f3f4aedde42dc2966d4d5b48      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 139 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007f6d2808d556c665864f32df11a044e6b27c21      1
      2 0x0360b9acda197703cb3040715c75659c38f13cf7      1
      3 0x05f13a948a4c345e4921adb4ec12f9c76ee3d331      1
      4 0x0808d22c75783042babfa002b5127dd42e9e37c5      1
      5 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
      6 0x0ae44fc0e1c6e86e5de1ace32057a44bfe85c9ea      1
      7 0x0dda698d2fe2fc1fb8f5b54ee9cd77fbd5a1d08b      1
      8 0x1022245eb550f9778209ca4514dcb693b45498be      1
      9 0x113d754ff2e6ca9fd6ab51932493e4f9dabdf596      1
     10 0x12efdcbbcdd6695109ada38e33c56173c2ffe2c3      1
     11 0x163c58774b6cf4df98b66ee3835f41ba7175f13c      1
     12 0x1832bce19ba35763fbd3ab1d5238de07372a7825      1
     13 0x1ae50668be1f32179bce00eb203121f00907d808      1
     14 0x1b4b3220292a30089061ce4d774d3a622dbae7d4      1
     15 0x1b967d5a3a14f661cc3bb0632cda3e152765ea32      1
     16 0x1d4e759ee4d927e06ada91edb98002a5f8fdd3c4      1
     17 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     18 0x2487140216d7a54c27d2d75dd672a72059679df8      1
     19 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
     20 0x27b455c481d62f91a7b55d09cf2e3cd70858ef85      1
     21 0x3020ae2d2bc994a1ae488a5bd11e3899a4e6f584      1
     22 0x32585799d538ae8f8aa49a233f69cf168bd20447      1
     23 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     24 0x3827014f2236519f1101ae2e136985e0e603be79      1
     25 0x38488dca8dccfeebfd3cc298436134e3b3af6b8b      1
     26 0x39d5dcbca5b24c98e295c4979a5f84a9ee39061a      1
     27 0x3a6372b2013f9876a84761187d933dee0653e377      1
     28 0x3a80d39ea6d316edef9ba0c847305b76ce2b5e58      1
     29 0x3a8a9caf13b05159569f829377ce151a2edc8fb8      1
     30 0x3c4e0d0f2f47b27a3e60022ee1aee97b59c05a07      1
     31 0x3d9d40c87c1c98385b0909e97fc4b82e10522741      1
     32 0x3e2dbda5f310aade1503557b2e7f356eaf161641      1
     33 0x41edfbdde3b9b22678937a956690362668dc7765      1
     34 0x469bd81594503d01ea9b1922ccd56c302fce17c7      1
     35 0x478087e12db15302a364c64cdb79f14ae6c5c9b7      1
     36 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     37 0x479b537c591877b7b6f7a084e9b07d90c4bff688      1
     38 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     39 0x4a22910f4db3e7bf000eacfa06126d5db7f0efd5      1
     40 0x4bf3805b23c99f8e0a5797e86fd0232a04a2a629      1
     41 0x4fc21517878b88dbee76f93cff2a3eab0d6304a3      1
     42 0x504eec66fa2ba8d44163ad484119db60fd59c70c      1
     43 0x511ef2d9d1b08b0bda6770448aca66df803998d4      1
     44 0x516656beacd4c41f4e1a2273263ff4995d0fd1d3      1
     45 0x5223e83500c6a30f8cb765af2865738b6bd5c438      1
     46 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
     47 0x53aee4229e3f77383a1436c64138ca68f31e2995      1
     48 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     49 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
     50 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
     51 0x5fd403fc29ddb91842d3c3f42e4dd1be2a1f5e37      1
     52 0x6278e4fe0e4670eac88014d6326f079b4d02d73c      1
     53 0x63784c971e102bce755b140680de86404843aedb      1
     54 0x6527939c107c507e22cec1f6740719a26a642ec1      1
     55 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     56 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
     57 0x6892274bb66893113dd440fd6dff3d4ff98b9d30      1
     58 0x6aa9f1775f7e0f8b8663e0729b7d64e4fb61c9cc      1
     59 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     60 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
     61 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     62 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
     63 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     64 0x73d4e8632ba37cc9bf9de5045e3e8834f78efa85      1
     65 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     66 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     67 0x780737124207f40dbb2bcad58594fead6ae12db9      1
     68 0x7b27c0cfabd0941577624edddc8ad0fd12fd7480      1
     69 0x7d05f69b2a2feb99c110dda9029a65b2f96d9910      1
     70 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
     71 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     72 0x80a32851a6caa6a6126e2f4e5d50aaf3d4bcf40b      1
     73 0x81b98a0e207726584ce1ac687fcae6059b35ebcf      1
     74 0x8261b63b07d1ad92aeb7cdd916bb9a25236ceb65      1
     75 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
     76 0x8490b3dfba40b784e3a16974377c70a139306cfa      1
     77 0x854a8a5fc02cf0472061e5f9bf909fde28fc5f0e      1
     78 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
     79 0x887b86b6b6957f7bbea88b8cefd392f39236a88c      1
     80 0x89bedcde29266fe29e7efd2ff943992042162d50      1
     81 0x916b46c8360eff50e5be4e108c33c17831f700c3      1
     82 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
     83 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
     84 0x97ddb656b2b18c9aa46f998da5b22963c8df9fac      1
     85 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
     86 0x9cee1b1d787bc70d8e4c00d14e25088ef66deb97      1
     87 0x9f3051ccb829f3452170d42a683c4d765dacfe32      1
     88 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     89 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
     90 0xa3ded378678d735eb14ad78db64887386932c62b      1
     91 0xa7b32f0c55bfe631a9470a6b3312a6685ffd9f45      1
     92 0xa8879c580a54f190ed53b43d30de269097ad7543      1
     93 0xaa13d55dcebfb2ba20113be03cd9d0c06bf8c1d5      1
     94 0xabb89dd8d2ecb0b872811a80b9ea8fb9b45562b3      1
     95 0xabb94dbdd360cc0ee92b59420a49019e760e32b3      1
     96 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
     97 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
     98 0xb2b40044be8d564911176c7c31fee1d083d6d7a7      1
     99 0xb2b557cce11a9d05d4df1dcb89fec2a6412b4b53      1
    100 0xb4d502361a3c6f823eeb9a99af09e110382206ee      1
    101 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    102 0xba8e220834c32fac376bbfb33820c001f022d72e      1
    103 0xbc10cd06b201ddbe419119b4bfb5a7dac24c2753      1
    104 0xbdbbbc0b1df75bdf767df6037e4ff7bec052b51d      1
    105 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    106 0xc1003fd0f7126333ace9772f6dee41f0e9d21205      1
    107 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    108 0xc3edcbe0f93a6258c3933e86ffaa3bcf12f8d695      1
    109 0xc62817d4d11a848d33d3fe3e96be76bffd73dea3      1
    110 0xc665a60f22dda926b920deb8ffac0ef9d8a17460      1
    111 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    112 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    113 0xd20bf69b592b9176e46ec229f82d7f1bf65fc9b2      1
    114 0xd573becb6a6b0a0d43065d468d07787ca65daf8a      1
    115 0xd62c014b45a35f78bcf465d75ada029c94fdabf9      1
    116 0xd62e3a24c71e124d126193e1db64c3f97abcf39b      1
    117 0xd69a4f613c03e991678d990a06904fab28662f08      1
    118 0xd743b27d75a6dcdb34bafc6de0acb8cecb3ac43f      1
    119 0xd773e2f5b5ffe1a8296984e53015d803ef989bb0      1
    120 0xd99b5a7c7d57c3d93533dcb5034cb64dcecf995d      1
    121 0xda3845b44736b57e05ee80fc011a52a9c777423a      1
    122 0xe09e1349e8342c77961dc2855c1e1f7236eab6f7      1
    123 0xe0de9065b602ffc0d30c812c4d77e56394320797      1
    124 0xe1e0da672b12f7d0d025f4b52512ab1678b2c7fd      1
    125 0xe71d8ded9dee1187e25a2d0fca8c8b050559d7d3      1
    126 0xe876275e5e1a77e0de4006e5d9b85b33621d1442      1
    127 0xea310c966d3ff5e09c65487f1763b21361eb71ef      1
    128 0xecc44bb21c355a23c4a923354b67c6d97d3576b9      1
    129 0xee99987a3ca0cdc7b620f9c14f6c18f3cbb9e055      1
    130 0xefb409f69f833536362d6077f14b0a51efd9d468      1
    131 0xf36480d3471dc81b0609c950e6cfde4bb014b576      1
    132 0xf47ff49820805577bc6df1c636382564dfa0a791      1
    133 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    134 0xf95ea2e425f3346204280476864e6b1d9baa4b2b      1
    135 0xfa398d672936dcf428116f687244034961545d91      1
    136 0xfaa8a175b8b0fd2ddfb31347282afcb53870ef1a      1
    137 0xfb7c8a716d8c9577ea293a28ff592bb967ea84c2      1
    138 0xfbda58bcaafe5242f3f4aedde42dc2966d4d5b48      1
    139 0xfff5086e00bc92ee04826b0f5398ebbdb8ea4000      1

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
