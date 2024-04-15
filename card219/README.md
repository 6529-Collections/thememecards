
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:227         Length:227         Min.   : 1.000   Length:227        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.471                     
                                           3rd Qu.: 1.000                     
                                           Max.   :35.000                     
         name          
     Length:227        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19644269 # https://etherscan.io/block/19644269
block_hash <- "0x7194da7d15e3118147a79f90bf63edca564124f9275eb8468e5c41820f71704d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4428 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","ImaginationofLavaa","PEPETALE","HUNDREDcollection","THERINGS","Foundation","CryptoGunnox"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("FREEDOMOFMICKEYEditions","HEARTBONEEditions","PepeandFriendsEditions","LavaasFriendsEditions","LavaaCardEditions","NotablePepesEditions","NobleEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","ImaginationofLavaa","PEPETALE","HUNDREDcollection","THERINGS","Foundation","CryptoGunnox","FREEDOMOFMICKEYEditions","HEARTBONEEditions","PepeandFriendsEditions","LavaasFriendsEditions","LavaaCardEditions","NotablePepesEditions","NobleEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 61 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x036cdb30b2d159c1a6b3219f306e8b23a93c94d2      1
     2 0x092314c3c7aab1f890facd3cec28cb3a4295d67d      1
     3 0x092f398f943cc1ff517f215215517d7f273f3ed9      1
     4 0x0ce390f18af702cca546297845a4a51d102123cf      1
     5 0x0e2afb6b65a167f8e1acea2655760dc93282c6bc      1
     6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     7 0x16c07fa0c3b7ba4b51b5f3e6a7f7c90a5b1d835a      1
     8 0x1726510192461c6d9df244fa3420fd66ab5aec95      1
     9 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
    10 0x1f6411a05076cb13830ae9cb8531598c33bdb215      1
    11 0x21c2972059c5f79ded07af9872375d61a72f7322      1
    12 0x254260ee635266c41d6a4b020ed74ca0028726e5      1
    13 0x2cebe4eb588de2d6028eb193a3cef97597ae142c      1
    14 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
    15 0x3c6deaf5c95717ed5442c9836dfc62c1e5e83164      1
    16 0x49d790cb37ff21a26a88a6e1932a07128805d906      1
    17 0x4bb6b621d91a5816da8ad6cb5405d36ae4068502      1
    18 0x5221d7d15d2bc5cb7cd80858cfaea4849850818a      1
    19 0x54ecad032486a010e5378bfb0aa2e6a752f335c4      1
    20 0x5c736d849527bb15fbc0111f4ca13b4cfc1a5c8d      1
    21 0x602d2a713ece658a76989f4ced1bd6179544e7aa      1
    22 0x6083d5a8bfaea5d199a5936a8a65a1d4a0e843ad      1
    23 0x62707f17000355c1115dcb52309686acb4bfa52d      1
    24 0x6301add4fb128de9778b8651a2a9278b86761423      1
    25 0x65a831d9fb2cc87a7956eb8e4720956f6bfc6eea      1
    26 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    27 0x68c62d8db8db114dd39a1bfac9a43d146b86fc06      1
    28 0x728a8a6353ee822481a83e9cc7b408cc8ac24f8e      1
    29 0x751c77aa53d29ece63e15bcbf2872a3469367885      1
    30 0x787ccb43496076b9e726c7ebc07aeea679c78d0d      1
    31 0x7e52db9f10de919937bc79be03b70acf88160b85      1
    32 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
    33 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    34 0x888776f59f9de11da8fe089a672bab75043ca7f8      1
    35 0x8888888888e9997e64793849389a8faf5e8e547c      1
    36 0x8889ebb11295f456541901f50bcb5f382047caac      1
    37 0x89bcb2d5a18796621e66c6797cec0203da44bff3      1
    38 0x89cc5ee6545349f829a4bb5132f7d7f078199236      1
    39 0x8a0454b05a952c572619935ed46b6d8d0aa97a76      1
    40 0x9fe366d6efe4ae241656915915f2f050c5ae33be      1
    41 0xa41c65ef6bafb38580e06c369453a416f2749bba      1
    42 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    43 0xb3792f424f8a8c65b8ff8430ec6859fb1187c0c1      1
    44 0xb63a5c5710f06e111fa14ac82da2183c5102b504      1
    45 0xb67d58cffae63a95fb380b952e65b1a8eac44d0b      1
    46 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    47 0xc48d912c6596a0138e058323fd9929209a66cfd8      1
    48 0xc540e9a093b73380b99a5a9213c3a70845b863dd      1
    49 0xc89ce9f096ddb405359b22a4863a08e8828e88d7      1
    50 0xca29f1ce29f3bc553c6c064aa2effc627c05ac0c      1
    51 0xcab1dbe6ec504703d16cd2754c82a2e74b9fd416      1
    52 0xcea4e31e1ca4b6d2391c41a7cb9afc72c81bda3f      1
    53 0xcf1e9e81f9b1e503e3897d5f9a3acf559b941f1a      1
    54 0xdd5e9609feff8ef9d6932a3e218821d27bb763d6      1
    55 0xdfeddc98ae6edcb5f63178a8589f41b6477f6c42      1
    56 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    57 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    58 0xf01976d37b02d388d5a54670439ba12df682825b      1
    59 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    60 0xf62c9ebf99607be71567c35a3e066e5b90baa7eb      1
    61 0xfc574113c3db18f57c6975e432b8eb8455cfd806      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0057ff99a06f82cd876c4f7f1718bd9a4f2e74b6      1
      2 0x00a3ba587f688f882621b70ee38cb16b7a057e53      1
      3 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      4 0x053e6294400a9268e35df445624f58087c7f388f      1
      5 0x1b10bd073ccaa7e88b4805e1484734fb1166c528      1
      6 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
      7 0x1fba0893b478fff6730f52a7ee96a7d180a8e5cf      1
      8 0x1fd8df37b360d7366db649703014e637ee214f4a      1
      9 0x203fbbea68bfef4984014a301a2a4c6787e49a60      1
     10 0x20b51851cd0aead3ea39a33d1677a46b3b8c069f      1
     11 0x23602ca06e977c86339ffddad74966e824ab691e      1
     12 0x24cef6ea31dd26f18ec1455c804f02d8d53b552f      1
     13 0x24d6ff72eccab41724d488a03e8ea721ec3177a3      1
     14 0x2550d50d0fa9477e4bcdc21626f2edfb94f3cbf7      1
     15 0x273008b0140e19d999cc8ddbae5c266a776fb23d      1
     16 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     17 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     18 0x2b26da567c2a8c220daf91be8e37a429d33aef0b      1
     19 0x2c31159f1497aed2cf9c028e55416f3050db1dff      1
     20 0x2d52ac4c7cd1cef8b62e9d31c36b291642970c3d      1
     21 0x304a97c9a85c92c93ca24e0a85b69f892b67355e      1
     22 0x307fb0634215379f9078404afe70034ef8142e2f      1
     23 0x30a898d50f18f317ecf6bf50240a34a416a7b1b4      1
     24 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     25 0x33bd61af24582c8742264d6a06f876fba211ef60      1
     26 0x340dc2e57d64e4f8a7010eb347fd1ca1b18bf935      1
     27 0x355de1773d5c46f0e7b4ea7cae9a553bd3f9c150      1
     28 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     29 0x3b19fb4f043e40d28bd357266f7dbb8a1a9bc644      1
     30 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     31 0x3e89a0f26657013bf251ced30d5d198c3b0d2466      1
     32 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
     33 0x428364c1919fd29b620f7f1dae3cad18376c74fa      1
     34 0x43176751abfbd7a39549748aab57641c26df7129      1
     35 0x447c3e43d795845add123dadb8a9918e045f0ca7      1
     36 0x44c2a80f8b924858db469a5b35128908e55c531f      1
     37 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
     38 0x498939d80526fe3a9133c4fecd51e193c1549624      1
     39 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     40 0x4f5aa4c9b972db8397ff4c1231fe4bffbdf7a1c8      1
     41 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     42 0x53bebd20781aaa3a831f45b3c6889010a706ff9f      1
     43 0x5843236ce0d3e08ecc8898a995212ab4c4a11107      1
     44 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     45 0x5bb039c7dc5fd8e94c23878dc2b8ed33681b2c47      1
     46 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
     47 0x5fdcef2b7e1585e6a888393d1330c5e5e88c27ec      1
     48 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     49 0x61fd0d043d519f5a2bd05785000f30db96809429      1
     50 0x65187716879a3d053437304535c7a66cb7903eb8      1
     51 0x66f377542600304ea500228481bc1ae740ca943f      1
     52 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     53 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     54 0x6a4406f5be648b406c0c20334e8a5de6b4999516      1
     55 0x6eae05e156738c20d8599efda1a6d87256544afd      1
     56 0x70f729e89eabacacf11293e40a700524112514d3      1
     57 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     58 0x7fed396e60cb8672b523e0d5c179136998a810b2      1
     59 0x81078a13acece5089e8834d97a15762b85cc3303      1
     60 0x83bff380d2c59f88f3132542fb23b40afcf361d7      1
     61 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
     62 0x8d54fbd16397e2b6ad91ea4df76d51427a01a3d2      1
     63 0x90e56349131d187e3349b8b37030adcad980ce89      1
     64 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
     65 0x93433264cccb31c80a62b0eee78e37ebbb0377e3      1
     66 0x9424116b9d61d04b678c5e5eddf8499f88ed9ade      1
     67 0x980072215e8c0ee6871c352a20056467f295306d      1
     68 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
     69 0x9a68254a63b08710901e721352342d3b2d30901c      1
     70 0x9c3b4b2d4101f2280ef9b1e810d618648b410340      1
     71 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
     72 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
     73 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
     74 0xb1364ad50f4792cbaf7f646ac6e0b556576ad817      1
     75 0xb28a151403e4c1b17642ec4488d0058b8498d77b      1
     76 0xb4627672ee52660a9e453ec541834e04583f3602      1
     77 0xb54640d76aab622214607833ccf3118a4810e151      1
     78 0xb6cf777e3696a502107417265c92d1b075636a10      1
     79 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
     80 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
     81 0xc1eca986c20103f3a180d797f5be64702bae7b22      1
     82 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
     83 0xd0437b797224cbef35bbe010a907e1c729968daf      1
     84 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
     85 0xd72508badc98629e324496180322e70ed2e28ee1      1
     86 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
     87 0xde0f33e45252fb0cd44368928f5b0820ccfbb46d      1
     88 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
     89 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
     90 0xe385dedaa0c833a99ad533918fc5abf9efe3ba4d      1
     91 0xe757970b801e5b99ab24c5cd9b5152234cff3001      1
     92 0xe7b15909675cd20a528cdc271e425b9923434f61      1
     93 0xe93e7fe38e743b72c37d9b63b03a9d25eae071ff      1
     94 0xe96ead0ad625122677684448155ba4ae2700814d      1
     95 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
     96 0xea9b13b35239482bf2bc05d7cbadf5ec47a0085f      1
     97 0xed8982578836f7d498856b3f543f128bb8621f2a      1
     98 0xeda388f94aea86a66d9e2ccab3c511b217714b01      1
     99 0xf033618c0eeeb9342641a227c17d76c799866f52      1
    100 0xf1ccae1131d406c65ac8034d4189aa51d3f6224b      1
    101 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    102 0xf27e69c6ef6dfc96f62f0b56dbd27ffedcaf72ba      1
    103 0xf39e356b26630115dd171f1d41640c45cec9ab21      1
    104 0xf3f3bed9e5c80a23255404c8841dee0db27f35cf      1
    105 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    106 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    107 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    108 0xf7e2f5919081fb9ff8d13fd340351f450cf33095      1
    109 0xf9270609374b754ca26cf00248c0aed197280909      1
    110 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    111 0xf9b7d79932b16c6bf8d08dbce15cd5e6942dd18f      1
    112 0xfd35356dcd225bbc7e8f1fde622bfbf5af105fe6      1
    113 0xff8991b1fa6b84e141bde964d442ceae0348040e      1
    114 0xffcad6bba49e6a63489c078c3a4a9298ee21cfab      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 175 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0057ff99a06f82cd876c4f7f1718bd9a4f2e74b6      1
      2 0x00a3ba587f688f882621b70ee38cb16b7a057e53      1
      3 0x036cdb30b2d159c1a6b3219f306e8b23a93c94d2      1
      4 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      5 0x053e6294400a9268e35df445624f58087c7f388f      1
      6 0x092314c3c7aab1f890facd3cec28cb3a4295d67d      1
      7 0x092f398f943cc1ff517f215215517d7f273f3ed9      1
      8 0x0ce390f18af702cca546297845a4a51d102123cf      1
      9 0x0e2afb6b65a167f8e1acea2655760dc93282c6bc      1
     10 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     11 0x16c07fa0c3b7ba4b51b5f3e6a7f7c90a5b1d835a      1
     12 0x1726510192461c6d9df244fa3420fd66ab5aec95      1
     13 0x1b10bd073ccaa7e88b4805e1484734fb1166c528      1
     14 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
     15 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
     16 0x1f6411a05076cb13830ae9cb8531598c33bdb215      1
     17 0x1fba0893b478fff6730f52a7ee96a7d180a8e5cf      1
     18 0x1fd8df37b360d7366db649703014e637ee214f4a      1
     19 0x203fbbea68bfef4984014a301a2a4c6787e49a60      1
     20 0x20b51851cd0aead3ea39a33d1677a46b3b8c069f      1
     21 0x21c2972059c5f79ded07af9872375d61a72f7322      1
     22 0x23602ca06e977c86339ffddad74966e824ab691e      1
     23 0x24cef6ea31dd26f18ec1455c804f02d8d53b552f      1
     24 0x24d6ff72eccab41724d488a03e8ea721ec3177a3      1
     25 0x254260ee635266c41d6a4b020ed74ca0028726e5      1
     26 0x2550d50d0fa9477e4bcdc21626f2edfb94f3cbf7      1
     27 0x273008b0140e19d999cc8ddbae5c266a776fb23d      1
     28 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     29 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     30 0x2b26da567c2a8c220daf91be8e37a429d33aef0b      1
     31 0x2c31159f1497aed2cf9c028e55416f3050db1dff      1
     32 0x2cebe4eb588de2d6028eb193a3cef97597ae142c      1
     33 0x2d52ac4c7cd1cef8b62e9d31c36b291642970c3d      1
     34 0x304a97c9a85c92c93ca24e0a85b69f892b67355e      1
     35 0x307fb0634215379f9078404afe70034ef8142e2f      1
     36 0x30a898d50f18f317ecf6bf50240a34a416a7b1b4      1
     37 0x3113ab5c691ecdac25c07bbf2bf80eb005e4088c      1
     38 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     39 0x33bd61af24582c8742264d6a06f876fba211ef60      1
     40 0x340dc2e57d64e4f8a7010eb347fd1ca1b18bf935      1
     41 0x355de1773d5c46f0e7b4ea7cae9a553bd3f9c150      1
     42 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     43 0x3b19fb4f043e40d28bd357266f7dbb8a1a9bc644      1
     44 0x3c6deaf5c95717ed5442c9836dfc62c1e5e83164      1
     45 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     46 0x3e89a0f26657013bf251ced30d5d198c3b0d2466      1
     47 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
     48 0x428364c1919fd29b620f7f1dae3cad18376c74fa      1
     49 0x43176751abfbd7a39549748aab57641c26df7129      1
     50 0x447c3e43d795845add123dadb8a9918e045f0ca7      1
     51 0x44c2a80f8b924858db469a5b35128908e55c531f      1
     52 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
     53 0x498939d80526fe3a9133c4fecd51e193c1549624      1
     54 0x49d790cb37ff21a26a88a6e1932a07128805d906      1
     55 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     56 0x4bb6b621d91a5816da8ad6cb5405d36ae4068502      1
     57 0x4f5aa4c9b972db8397ff4c1231fe4bffbdf7a1c8      1
     58 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
     59 0x5221d7d15d2bc5cb7cd80858cfaea4849850818a      1
     60 0x53bebd20781aaa3a831f45b3c6889010a706ff9f      1
     61 0x54ecad032486a010e5378bfb0aa2e6a752f335c4      1
     62 0x5843236ce0d3e08ecc8898a995212ab4c4a11107      1
     63 0x5b513d7bb62f7f28bc3aba2d52fe6167a9b3a827      1
     64 0x5bb039c7dc5fd8e94c23878dc2b8ed33681b2c47      1
     65 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
     66 0x5c736d849527bb15fbc0111f4ca13b4cfc1a5c8d      1
     67 0x5fdcef2b7e1585e6a888393d1330c5e5e88c27ec      1
     68 0x602d2a713ece658a76989f4ced1bd6179544e7aa      1
     69 0x6083d5a8bfaea5d199a5936a8a65a1d4a0e843ad      1
     70 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     71 0x61fd0d043d519f5a2bd05785000f30db96809429      1
     72 0x62707f17000355c1115dcb52309686acb4bfa52d      1
     73 0x6301add4fb128de9778b8651a2a9278b86761423      1
     74 0x65187716879a3d053437304535c7a66cb7903eb8      1
     75 0x65a831d9fb2cc87a7956eb8e4720956f6bfc6eea      1
     76 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
     77 0x66f377542600304ea500228481bc1ae740ca943f      1
     78 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     79 0x68c62d8db8db114dd39a1bfac9a43d146b86fc06      1
     80 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     81 0x6a4406f5be648b406c0c20334e8a5de6b4999516      1
     82 0x6eae05e156738c20d8599efda1a6d87256544afd      1
     83 0x70f729e89eabacacf11293e40a700524112514d3      1
     84 0x728a8a6353ee822481a83e9cc7b408cc8ac24f8e      1
     85 0x751c77aa53d29ece63e15bcbf2872a3469367885      1
     86 0x787ccb43496076b9e726c7ebc07aeea679c78d0d      1
     87 0x7e52db9f10de919937bc79be03b70acf88160b85      1
     88 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     89 0x7fed396e60cb8672b523e0d5c179136998a810b2      1
     90 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
     91 0x81078a13acece5089e8834d97a15762b85cc3303      1
     92 0x83bff380d2c59f88f3132542fb23b40afcf361d7      1
     93 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     94 0x888776f59f9de11da8fe089a672bab75043ca7f8      1
     95 0x8888888888e9997e64793849389a8faf5e8e547c      1
     96 0x8889ebb11295f456541901f50bcb5f382047caac      1
     97 0x89bcb2d5a18796621e66c6797cec0203da44bff3      1
     98 0x89cc5ee6545349f829a4bb5132f7d7f078199236      1
     99 0x8a0454b05a952c572619935ed46b6d8d0aa97a76      1
    100 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    101 0x8d54fbd16397e2b6ad91ea4df76d51427a01a3d2      1
    102 0x90e56349131d187e3349b8b37030adcad980ce89      1
    103 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    104 0x93433264cccb31c80a62b0eee78e37ebbb0377e3      1
    105 0x9424116b9d61d04b678c5e5eddf8499f88ed9ade      1
    106 0x980072215e8c0ee6871c352a20056467f295306d      1
    107 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    108 0x9a68254a63b08710901e721352342d3b2d30901c      1
    109 0x9c3b4b2d4101f2280ef9b1e810d618648b410340      1
    110 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
    111 0x9fe366d6efe4ae241656915915f2f050c5ae33be      1
    112 0xa41c65ef6bafb38580e06c369453a416f2749bba      1
    113 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    114 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
    115 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    116 0xb1364ad50f4792cbaf7f646ac6e0b556576ad817      1
    117 0xb28a151403e4c1b17642ec4488d0058b8498d77b      1
    118 0xb3792f424f8a8c65b8ff8430ec6859fb1187c0c1      1
    119 0xb4627672ee52660a9e453ec541834e04583f3602      1
    120 0xb54640d76aab622214607833ccf3118a4810e151      1
    121 0xb63a5c5710f06e111fa14ac82da2183c5102b504      1
    122 0xb67d58cffae63a95fb380b952e65b1a8eac44d0b      1
    123 0xb6cf777e3696a502107417265c92d1b075636a10      1
    124 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    125 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    126 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    127 0xc1eca986c20103f3a180d797f5be64702bae7b22      1
    128 0xc48d912c6596a0138e058323fd9929209a66cfd8      1
    129 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    130 0xc540e9a093b73380b99a5a9213c3a70845b863dd      1
    131 0xc89ce9f096ddb405359b22a4863a08e8828e88d7      1
    132 0xca29f1ce29f3bc553c6c064aa2effc627c05ac0c      1
    133 0xcab1dbe6ec504703d16cd2754c82a2e74b9fd416      1
    134 0xcea4e31e1ca4b6d2391c41a7cb9afc72c81bda3f      1
    135 0xcf1e9e81f9b1e503e3897d5f9a3acf559b941f1a      1
    136 0xd0437b797224cbef35bbe010a907e1c729968daf      1
    137 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    138 0xd72508badc98629e324496180322e70ed2e28ee1      1
    139 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    140 0xdd5e9609feff8ef9d6932a3e218821d27bb763d6      1
    141 0xde0f33e45252fb0cd44368928f5b0820ccfbb46d      1
    142 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    143 0xdfeddc98ae6edcb5f63178a8589f41b6477f6c42      1
    144 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    145 0xe385dedaa0c833a99ad533918fc5abf9efe3ba4d      1
    146 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    147 0xe757970b801e5b99ab24c5cd9b5152234cff3001      1
    148 0xe7b15909675cd20a528cdc271e425b9923434f61      1
    149 0xe93e7fe38e743b72c37d9b63b03a9d25eae071ff      1
    150 0xe96ead0ad625122677684448155ba4ae2700814d      1
    151 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    152 0xea9b13b35239482bf2bc05d7cbadf5ec47a0085f      1
    153 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    154 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    155 0xeda388f94aea86a66d9e2ccab3c511b217714b01      1
    156 0xf01976d37b02d388d5a54670439ba12df682825b      1
    157 0xf033618c0eeeb9342641a227c17d76c799866f52      1
    158 0xf1ccae1131d406c65ac8034d4189aa51d3f6224b      1
    159 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    160 0xf27e69c6ef6dfc96f62f0b56dbd27ffedcaf72ba      1
    161 0xf39e356b26630115dd171f1d41640c45cec9ab21      1
    162 0xf3f3bed9e5c80a23255404c8841dee0db27f35cf      1
    163 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    164 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    165 0xf62c9ebf99607be71567c35a3e066e5b90baa7eb      1
    166 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    167 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    168 0xf7e2f5919081fb9ff8d13fd340351f450cf33095      1
    169 0xf9270609374b754ca26cf00248c0aed197280909      1
    170 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    171 0xf9b7d79932b16c6bf8d08dbce15cd5e6942dd18f      1
    172 0xfc574113c3db18f57c6975e432b8eb8455cfd806      1
    173 0xfd35356dcd225bbc7e8f1fde622bfbf5af105fe6      1
    174 0xff8991b1fa6b84e141bde964d442ceae0348040e      1
    175 0xffcad6bba49e6a63489c078c3a4a9298ee21cfab      1

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
