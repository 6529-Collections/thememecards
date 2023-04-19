
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:28279       Length:28279       Min.   :1   Length:28279      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :3                     
         name          
     Length:28279      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17073269 # https://etherscan.io/block/17073269
block_hash <- "0x09e073ea607b71d7bf6c0c4e8bca630575065ada891c36a5ce36fc16a860b937"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4647 

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
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

airdrop_ryan    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","CryptoCubes","Singles"), address_remove=address_remove,address_max=1)


allow_ryan_phase1      <- pick(snapshot, contracts=c("Systems","Noble","Underdogs"),address_remove=address_remove,address_subtract=airdrop_ryan,address_max=1)
allow_memes150_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=150,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_ryan) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01db485f57dc000e761b85641f78c9d212a2eeab      1
     2 0x0847f2f58f9515e799662f7165704c1b103b8c54      1
     3 0x0d648f5ef02403282329baedacb670e13ffdb41f      1
     4 0x21301d901db04724597d1b6012ac49878157580d      1
     5 0x3244aacd5f2b441ab0fbfefee431413e50332957      1
     6 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     7 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
     8 0x533b9cdce87facf35c4066f48537660bf2bf16ad      1
     9 0x56577f982e2066fec566108f0a63f3d09555c6ca      1
    10 0x63141638f37c77e4ad0189b76d02607a0c80a1e7      1
    11 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    12 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    13 0x9769334fc882775f4951865aa473481880669d47      1
    14 0xbbd0d639bc42651de27d9beb5999033b4d59e67a      1
    15 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    16 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    17 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    18 0xead90127d60f78f3e97781938418004be1794eae      1
    19 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    20 0xfa8a62a68316475762966e3418c7343e2516adfe      1

## Allow Artist Phase 1

``` r
c(allow_ryan_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 60 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
     2 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     3 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
     4 0x11e975993095fb5587b5cab2abc4148fdf9435aa      1
     5 0x19c79a0c102a74d7bd317b4e51c229ea81769075      1
     6 0x1fd8df37b360d7366db649703014e637ee214f4a      1
     7 0x23602ca06e977c86339ffddad74966e824ab691e      1
     8 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     9 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
    10 0x32ed7ce154b816d2d71ccac6b384a5261aeacf60      1
    11 0x339dbf91cea8bb8a838865b047a0384246421e78      1
    12 0x370c4fafd8b9a621d01df979866c5259a2f74211      1
    13 0x3a3da350fd33a1854beaeab086261c848526811b      1
    14 0x3d1af3a5f9539157fd24a354fd19170ad45693df      1
    15 0x41ef2bf26cf9e2654bf3c8dd89fa32211fc64990      1
    16 0x44736f68f0a7a3b2244750997ac073a6a036e3e8      1
    17 0x4907911cf8911510b89bc7147a25700b2d260f36      1
    18 0x54be3a794282c030b15e43ae2bb182e14c409c5e      1
    19 0x590f4ca9b70860d1b89be8a7e69f22e59f6dcf6f      1
    20 0x63e0e8a3a9c59141843cc26e5433507b8a835999      1
    21 0x64bf7a7b25a5b15c1572aaf464204a8a528123f4      1
    22 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
    23 0x686bd755b9396e93eb924da11f78f3c92076494e      1
    24 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    25 0x6a3de9a0cb759f634a6a6f66c926d71b055482c4      1
    26 0x6abb3e6639b1ce3c7ce63f5a7046444407d154fd      1
    27 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    28 0x6cc91dcbc8e4e20a5744d9a0737034cf04629747      1
    29 0x6dd20703b80b0b97f87d44f377ea76b692504a13      1
    30 0x6fac5ca3a8a7f53385e87a1e7e683dd7486afc3b      1
    31 0x79788917f5963880f79e138a3f01ac049df40812      1
    32 0x7b640f5e754b6642cc0dfe63d407e790c204de06      1
    33 0x846d1163b9a0c9b3ca955d256e6c1ca70403291f      1
    34 0x8d309de16a745c15ffa00a46da4eb3e044c54d63      1
    35 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    36 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    37 0xa2259965ecee1144789999694a4656b3ab01b6f6      1
    38 0xa5e32f31948b1af2ffe2e1770ca1d4b6176e9738      1
    39 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    40 0xa8fd69a697ca45dd8f869dbd1dde73572dafbd86      1
    41 0xaabd17538ccf45a8b758f0551ad954614df34626      1
    42 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    43 0xb46babc5b1a3d79ade4597cb0090efb9f80ebda4      1
    44 0xb7bb7840932e997e5369277aee009cb1df06c803      1
    45 0xc0d206d816159a84ada6b045df79e3a8de957348      1
    46 0xc5633bc0bcc3897117841f434a8f5f95a724a7b8      1
    47 0xc982301321b1e958f2b1783d46fb919956507b64      1
    48 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    49 0xd03504e910b56dd690fc5f57f9b758011d001c94      1
    50 0xd76ed14e9dbb0983489f4a1bbb45ec18ac78b7df      1
    51 0xda65aefe911b9e018668f13e4adbfc9b221116c1      1
    52 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    53 0xe00712086490734ef8b4d72839a7237e505767f5      1
    54 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    55 0xed6ff3a2c8a13f2223f6a7b3985e54e1f8dc064b      1
    56 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    57 0xef16a708b9feeaa44aea2eae66b0a6df4951f5b6      1
    58 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    59 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    60 0xf8d0a4542b1277d8a330e69cc56bdc109029c623      1

## Allow Memes Phase 1

``` r
c(allow_memes150_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00a7f53d5a574a71ad23fb85f18f3b4c5a21ef54      1
      2 0x01b71498cde91a613eacca67fa46463b0dad724f      1
      3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      4 0x08d4c631940a224181e70b6c81a1fee6dbf3251e      1
      5 0x0aa78b747dfc137ef96b428951ebe32f9ac4efa3      1
      6 0x0b7d2c4f89d1cbb290af0733e1241ff1d6fc72a8      1
      7 0x0bc7815f29ede74742d7377821506f705cf0d809      1
      8 0x0e7a408a775af29310970de51b59501e21eee87a      1
      9 0x108289ecb4830e7de08dc8f9e58526ddeccd2d32      1
     10 0x10efe7195c7ca47b9b1e2da423fecae8527f5dba      1
     11 0x1622325033583cb93e3ce3019fa00d1038bdfc18      1
     12 0x16dac3c8f74191c405b73807652ef5dbdec7aa23      1
     13 0x17e7202da7a6411c27db0b8ec0d0839bec645202      1
     14 0x1989e01962646dff41d4994a411de9621bba5580      1
     15 0x1a051968e421023d61a0afac659b6916784f251f      1
     16 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     17 0x1be8214a3e0528623e0e10af349936b642bd3767      1
     18 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     19 0x1e6d8d1a6076916d38b120740308d07c01cdb2c7      1
     20 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     21 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
     22 0x2249f5016b1b7a306c5e98765bf94fd0a70d8b45      1
     23 0x2336ee0447b55103e8c01cfca82067445d8fa571      1
     24 0x25a82f100d42c88b4e3ee8bf206b4d06d7a79997      1
     25 0x25d50ce5c3c2962b1eb22963070432c8ace816b0      1
     26 0x26088bd0575c0ac94946fb8dfa0b240c42283e42      1
     27 0x2c1725bff8496ab8098406639ee34e5b015f74a2      1
     28 0x2cc21b6ce4f88835e87d624e8d7f61cd6392cbdf      1
     29 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
     30 0x2f1f1f7008b57c7421f39726a0d223c5b17e35dc      1
     31 0x309ba35771337651a771c502f7b9cf8aac866cca      1
     32 0x324e9e6e602fdf1f7f50decea6cb83fff575020f      1
     33 0x32cef996a8901743d26a900da9f042689334413f      1
     34 0x373db7e01ebfb92d9095ae6b8f6e3d24ce6f4d4d      1
     35 0x38167714e4bd03afc946e1fa8667269e48420a09      1
     36 0x38b2d736e41a273d607b24e888a09473226c46b8      1
     37 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     38 0x3a2c6b9d1548d752b85a17bb8cde226ee70980e2      1
     39 0x3a4c619284748869eb3ab16494b461256b72f875      1
     40 0x3bfcdda69bfef9ab5da3c3e75c0ff0e0a3074a5d      1
     41 0x3f7dd0b106b10d06bb4813715b4e24d9e626dd64      1
     42 0x41e12756498322b479f889af2b2f4b29a85d5605      1
     43 0x43176751abfbd7a39549748aab57641c26df7129      1
     44 0x48ae825591a926da5f49aca43608f28fdf37210b      1
     45 0x48c8bd589e8b81cfe13403ec4884f71e676961db      1
     46 0x494d36925644a66737c3cfe8f6d1ffb8768c9941      1
     47 0x494e6fd90217d3b19be362a4a698c760cb3caaba      1
     48 0x4aadfd4c8cdbe3a18ce88e36407517159c675279      1
     49 0x4b46f1e241838a910945a3fac242ddf6f5d7c041      1
     50 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
     51 0x4e3a10f142075389848ba955ab6282d9338839de      1
     52 0x4f36fc5b7d34fe98fc81e5a8e2c76312a4d53865      1
     53 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     54 0x5304c4954a557be473d058bee7f62ff3220da5bd      1
     55 0x54ffa6d7d9315081ab40806fe2b2aed7823c7987      1
     56 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
     57 0x5ab5844dd55ab73212d1527e4cf72fea884e39dd      1
     58 0x5eff35d620168bd496b39243fee0afdc593e972c      1
     59 0x60e5299f49cdbc13d152323105af462071b22c87      1
     60 0x6194bda4590bbdc6a128851aaaf34802dfa8e4a1      1
     61 0x621fa2a2dff1012ce2989e583c407d060ef59754      1
     62 0x6368ce2750325f5002587c70c8bc298b4df61eba      1
     63 0x6523ae151fb606439bb75855c6ae966b9251981e      1
     64 0x66d3c53e61be58c558ddb2f273271f99cb7673fa      1
     65 0x681a64617e1083c36cbebbd8ac9e64938d3c2591      1
     66 0x69419b29e25dfcb0f1f27906cadfa332fc145d7f      1
     67 0x6bdead6b984d74baee2da16bef7f58319e659398      1
     68 0x6c447537ed9c6928492e0f2882278ac530892ff2      1
     69 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
     70 0x6e1510ed41ba743344e9ba80646194351cca0c7f      1
     71 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
     72 0x7165a04c41c9e5e67d4850eab1dc6ede84d117f0      1
     73 0x733983a4c7e778ca9f57cac6ee361c4a9b24e4b1      1
     74 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
     75 0x75750d0bba74ecb961fa588873a0ef69c54361c1      1
     76 0x797a47774c061c45ee94232217bfceae6ac937c8      1
     77 0x79fdbef5ce6d2302db090cb75de318d2c3edccee      1
     78 0x7a926e039b30b9ca5a0499e6a8b1f7fe2c30aef8      1
     79 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
     80 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
     81 0x7ce2c24fa9447da94f15d9255c5f44bfdaf8dc07      1
     82 0x7d60a758f47822ec591b24df5a9d58fac84cb07e      1
     83 0x7f261cf6d968de4341183aeca33521e2c3a19bd9      1
     84 0x808421753a181e96812796b7ab43d3f356cc5a77      1
     85 0x81d4f31946c4ea0cbfde9664170a100a866c316f      1
     86 0x834c771e04770b1f54fe13d258e5a640d78fba29      1
     87 0x843f2e3d685caa87ae089c1b95cec9f43ee65afc      1
     88 0x849a27048de274c084f42551f395823f8fa18f2e      1
     89 0x86cc7ad354441bae1304f55c1f6df8ba98d44af2      1
     90 0x88d8fabbb70d3854b7be95a0237d957cbdea24c5      1
     91 0x89cc08700dcba9d4bad5295dee2a111b90b39917      1
     92 0x8e0f943577527f5e21be9ac1ba99c027360a300d      1
     93 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
     94 0x928ee99d063ba6839c30e663822c8cf6c6dcda02      1
     95 0x931f974d02d25beb1f2f510fa051f81fcb8837e3      1
     96 0x96545a59ce81bb6acaf95957ea09cf577889112f      1
     97 0x975fc3062c790429e40b1085728b3938d5bcef72      1
     98 0x989a5d65720e62d80299747e5a95024ff07732ff      1
     99 0x9d95d36f15c3cd3028d53a58ab6c60a1a7815179      1
    100 0x9f83a2a2b6e0b11a24dfa39817dd248475174a83      1
    101 0xa081ae42802e0be86f5a6dd9274adf50c9986a1d      1
    102 0xa14213bb6f4e324d5cea5606f1b5a4af12d9009d      1
    103 0xa22414e4af5767448624017c7e0151d22490412b      1
    104 0xa4b20acc360e5bd3cc073f5292b4612055b30dfb      1
    105 0xa4df50f02a778bf281ea0db761900d354449eb17      1
    106 0xa723bcc05f4e0f739d572e7fdff0b23af977a8f7      1
    107 0xa8207370cd7c6b87aa427459ec439486f3dfc8f0      1
    108 0xa976fb8ed77bc1016a06074ff5e3e4a5fa6161e0      1
    109 0xac707ee01a605bfc712536d703e5e790357c48e5      1
    110 0xadffc7ff3e937e3d7a34e989fac05fe06cd0fc99      1
    111 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
    112 0xb0e297eb644eabb16eac74220ba406b11edb2cd7      1
    113 0xb0f96a681e2e1bcbf40f99f0e48b8f3108089cb2      1
    114 0xb70ed8c7effec5173f7377178bb75251be182f21      1
    115 0xb8bb2fbad09c68aa90c8ad6cbcf33987bb7add38      1
    116 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    117 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    118 0xbd79b7e0cae4500eb02e6f212ddf2681ac8fb6d4      1
    119 0xc6d4b36751233d1c509bc94bdaaa8133b3a281f3      1
    120 0xcba2e4e7e929579101c428bc075ef864a8085b36      1
    121 0xd0afdb750dd8c5c6a82a17e90333944d1ecdfcea      1
    122 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    123 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    124 0xd5a0ba47d5217418992d6887b3bf044961cdec35      1
    125 0xd63929e5f4c6e82f683d3a5f260a393d47e1cef3      1
    126 0xd66a2ac715fd7628bbcbf49ac3dd4a0f3a5b847f      1
    127 0xd6701a92ee4e8df99a7ff1a453abba8da84e0c98      1
    128 0xd8c247ee44dfc6898fe4e224979f64e367d2c9ea      1
    129 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    130 0xdaac4e26f11223cc043dfd8e3efaf72333067339      1
    131 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
    132 0xddfcb75f00f2cea10097a0ecc44a32a9c4b62cc9      1
    133 0xe02a199b22fa76c823b066370f715b8c9db430de      1
    134 0xe4000d4f3f0e59ca00b803f54784fe0654a192f4      1
    135 0xe7380b7be1ed475de78b07033a8ef4dfb10205a5      1
    136 0xe7410f6f1d5201cb9ef70cd49c6436fce000b492      1
    137 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    138 0xeaf8fb108b81d6d32f3a6e2643957f705cec2f0b      1
    139 0xeb5d5bd750ba52b8ea39d4dbb746fa5cf981aa17      1
    140 0xeddfd770e5aa54d25d7ecc0670bbd9de2d9ae1b3      1
    141 0xef6914f95fc782a5397cff1686873da77fdeca8f      1
    142 0xefea7c883b74ef2865d953d8fa46d6e654b8ffdf      1
    143 0xf020a364d81900e8fe8f0e53b0267eb88d0c05e7      1
    144 0xf06fc22f0f9d15a271e18aa216fe183e20a7a092      1
    145 0xf5851672ab7fc8729de7472dd273066b5e3b4de5      1
    146 0xf916b594fa568fb4cf5ca7f0ebeb11ffd9e296c5      1
    147 0xfab0b832caebe5b628bc7ef381ec855f6aa4cf71      1
    148 0xfecf9b009af216056d27759c1489f00fc62428e2      1
    149 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1
    150 0xffb8a15dc8796af3e7bec74e528f1bd810b854ed      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    11 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    12 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    13 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    14 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    15 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    16 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    17 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    18 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    19 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    29 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    30 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    31 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    32 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    33 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    34 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    35 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    36 0x69e68074f1aada957edd39c5eae0069973343f30      1
    37 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    38 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    39 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    40 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    41 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    42 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    43 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    44 0x82139687faae8a29851902783e02e699de0e0846      1
    45 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    46 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    47 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    50 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    51 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    52 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    53 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    54 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    55 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    56 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    57 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    58 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    59 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    60 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    61 0xbf814810b44978de273191fd612aa47f7b69d564      1
    62 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    63 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    64 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    65 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    66 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    67 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    68 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    69 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    70 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    71 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    72 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    73 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    74 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    75 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    76 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    77 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    78 0xfd22004806a6846ea67ad883356be810f0428793      1
    79 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

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
