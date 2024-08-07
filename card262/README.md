
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:173         Length:173         Min.   :1.000   Length:173        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.017                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:173        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20446069 # https://etherscan.io/block/20446069
block_hash <- "0x05b7c76555f100cc8b568edf509caed82ade6b72095227215b57890b50bc82c1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4552 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","esraeslenIII1o1","Foundation","Streets","Persona","Inside"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PlayWithMeEditions","MelovemeEditions","Demian","StreetsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","esraeslenIII1o1","Foundation","Streets","Persona","Inside","PlayWithMeEditions","MelovemeEditions","Demian","StreetsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     2 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     3 0x2a4d27abf7ebbce29131c9e8fefd3ab335aa4cce      1
     4 0x2ea2e43e69eadbc017d5036bb061a214c7e736de      1
     5 0x367497f910999a1d747c52228e700964946f8b25      1
     6 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     7 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     8 0x56e81bc43a5fc9a01ff000270bc55a02df268147      1
     9 0x59068075a799594db03c0255eed68e8e121155c8      1
    10 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
    11 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
    12 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    13 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
    14 0x810e096dda9ae3ae2b55a9c45068f9fe8eeea6db      1
    15 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    16 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    17 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    18 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    19 0xbc3523ec30599d38d49bb4d51016ac58c4b6b27a      1
    20 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    21 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
    22 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 92 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
     2 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     3 0x071642731357694b43567e961c6b99647962a8c7      1
     4 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
     5 0x0903b02db2adac11df0247c1c38bf6c3ce782db9      1
     6 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     7 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     8 0x107d8dbbd3ba1cc0f7d16561357b861e1fdaba7d      1
     9 0x13257e056112d578414e5d291926c9d684d5d9ef      1
    10 0x1e96b3d06c7dccc94f122f435d989a3efae84ff4      1
    11 0x236ce4f22bd029d90ae8601f85541528b1e9d465      1
    12 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
    13 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
    14 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    15 0x333d9d6f96bf1e20644e4ea5848bf6e90c13e95d      1
    16 0x350a081520192adce41cfd039b237ad2b5b88124      1
    17 0x35c17dd9df9156ee74726e29f598dca529efac8d      1
    18 0x3ac9e19507b2bebd1ea1d29f51c5ea36d221e780      1
    19 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    20 0x3b81babf4faec56bc60423e20f60a5da5617fcd1      1
    21 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
    22 0x419cb1ce6b42e786556e88db73d449f9b073dd44      1
    23 0x44a539792dc01121dc0d932dbe6c5f9af19ed57d      1
    24 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
    25 0x47865cbb78ac8b25b2619efe17b822a357945ddf      1
    26 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
    27 0x5090752b49b42eff5a3744c2914222fb02b5a925      1
    28 0x526525fe1222613defca291bcf176d9849d91c6d      1
    29 0x536b8612e71183e04b36ae8b399a511f1682762b      1
    30 0x59ca7f7a899648843e04d42316b940de0d0e1abd      1
    31 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
    32 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
    33 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    34 0x649ff45a74d3ffb86a31fcd1e9c88f46e8bb38a0      1
    35 0x657c1f8421ae93e88fb3171f019779e87b0263ca      1
    36 0x6cabe2e60154c9b7e6212378dc41818850c2eef8      1
    37 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    38 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
    39 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
    40 0x70cfb1fdd807dee18764c805da7517ad30f47009      1
    41 0x71eda26592f153519ca106d934b4eaee0cc469c3      1
    42 0x776bec5a4dfaf80d021ce80705ae363be0a78899      1
    43 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    44 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    45 0x7a785a2bb327a78e294583ffec3729901864cbec      1
    46 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
    47 0x7c17ebdce31d5f1584e3279862f27f36d8de0d8e      1
    48 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
    49 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
    50 0x81b1a210ec35c352a4c4e63d5e9e1d905bef738d      1
    51 0x8888888888e9997e64793849389a8faf5e8e547c      1
    52 0x8a8035f056af830b7205c58c1dc037f826fc2b92      1
    53 0x8b0fa4148021fa96d2fa5ee760068eb6dcfb2634      1
    54 0x8ba4c65d4864074b7df30ccac98b766e6aa49e67      1
    55 0x91bb0008b406ddfd9c5f66655d2af77fbe7c99b7      1
    56 0x93d42f5dc45847e65eda9a44dbdb0adf653a099d      1
    57 0x96615cb54c3d6634ac2bc0c5225d88e63a7b08a0      1
    58 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
    59 0xa249a826ff225e7fffd5a6fc118bef4feea2633f      1
    60 0xa549487362b9bc4a66804fbf4774b2cc26ffd4bb      1
    61 0xa8ff5ba66bf96bac81d81058d610b9b8c4cacff0      1
    62 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    63 0xaf2e919b59734b8d99f6bc1f62dc63d6519d14bc      1
    64 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
    65 0xb07952a55bf9c45c268f37c3631823df50ac721a      1
    66 0xb6079e8c2795152e0e52774782406fe4185bb24a      1
    67 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
    68 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    69 0xc3273e027c6fe74dbf941a2b4e91f5e5d8103762      1
    70 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    71 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    72 0xca177d4b3e6d4c2778912b47e6aae49b7c429819      1
    73 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    74 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    75 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    76 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
    77 0xd502b0383fbf710da7c85614810f6ea109bb7ac2      1
    78 0xdb4a4055913cdeeef3b6560f9a147c1bfeb77025      1
    79 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    80 0xdca95d7f7533548a9124edc6959fcdb2e56f0b07      1
    81 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1
    82 0xe91ef2fe89a7a439e1e664ea7ac862849723c71e      1
    83 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    84 0xee119e2fea524c93aea9962de5308070682801b1      1
    85 0xeebb6ab3d5f0b402efad1eca6f72f880b66066c9      1
    86 0xf05ed0587b069975ed20308096699f02dd36fc30      1
    87 0xf1a67d67cc97e9755e2c8b13a6ee3e538bf830d9      1
    88 0xf1dafb1ca027e69b67952e8a385e7f43791f2e70      1
    89 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    90 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    91 0xf889b1d99b440aaa48839d7032088d96f7b3bceb      1
    92 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
      2 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
      3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      4 0x071642731357694b43567e961c6b99647962a8c7      1
      5 0x0830337ed8d752b520d5547aadeae4b8d4154199      1
      6 0x0903b02db2adac11df0247c1c38bf6c3ce782db9      1
      7 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
      8 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
      9 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     10 0x107d8dbbd3ba1cc0f7d16561357b861e1fdaba7d      1
     11 0x13257e056112d578414e5d291926c9d684d5d9ef      1
     12 0x1e96b3d06c7dccc94f122f435d989a3efae84ff4      1
     13 0x236ce4f22bd029d90ae8601f85541528b1e9d465      1
     14 0x2a4d27abf7ebbce29131c9e8fefd3ab335aa4cce      1
     15 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
     16 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     17 0x2ea2e43e69eadbc017d5036bb061a214c7e736de      1
     18 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     19 0x333d9d6f96bf1e20644e4ea5848bf6e90c13e95d      1
     20 0x350a081520192adce41cfd039b237ad2b5b88124      1
     21 0x35c17dd9df9156ee74726e29f598dca529efac8d      1
     22 0x367497f910999a1d747c52228e700964946f8b25      1
     23 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     24 0x3ac9e19507b2bebd1ea1d29f51c5ea36d221e780      1
     25 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
     26 0x3b81babf4faec56bc60423e20f60a5da5617fcd1      1
     27 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     28 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     29 0x419cb1ce6b42e786556e88db73d449f9b073dd44      1
     30 0x44a539792dc01121dc0d932dbe6c5f9af19ed57d      1
     31 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
     32 0x47865cbb78ac8b25b2619efe17b822a357945ddf      1
     33 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
     34 0x5090752b49b42eff5a3744c2914222fb02b5a925      1
     35 0x526525fe1222613defca291bcf176d9849d91c6d      1
     36 0x536b8612e71183e04b36ae8b399a511f1682762b      1
     37 0x56e81bc43a5fc9a01ff000270bc55a02df268147      1
     38 0x59068075a799594db03c0255eed68e8e121155c8      1
     39 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     40 0x59ca7f7a899648843e04d42316b940de0d0e1abd      1
     41 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
     42 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
     43 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     44 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
     45 0x649ff45a74d3ffb86a31fcd1e9c88f46e8bb38a0      1
     46 0x657c1f8421ae93e88fb3171f019779e87b0263ca      1
     47 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     48 0x6cabe2e60154c9b7e6212378dc41818850c2eef8      1
     49 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     50 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
     51 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
     52 0x70cfb1fdd807dee18764c805da7517ad30f47009      1
     53 0x71eda26592f153519ca106d934b4eaee0cc469c3      1
     54 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     55 0x776bec5a4dfaf80d021ce80705ae363be0a78899      1
     56 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     57 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     58 0x7a785a2bb327a78e294583ffec3729901864cbec      1
     59 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     60 0x7c17ebdce31d5f1584e3279862f27f36d8de0d8e      1
     61 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     62 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
     63 0x810e096dda9ae3ae2b55a9c45068f9fe8eeea6db      1
     64 0x81b1a210ec35c352a4c4e63d5e9e1d905bef738d      1
     65 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     66 0x8888888888e9997e64793849389a8faf5e8e547c      1
     67 0x8a8035f056af830b7205c58c1dc037f826fc2b92      1
     68 0x8b0fa4148021fa96d2fa5ee760068eb6dcfb2634      1
     69 0x8ba4c65d4864074b7df30ccac98b766e6aa49e67      1
     70 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     71 0x91bb0008b406ddfd9c5f66655d2af77fbe7c99b7      1
     72 0x93d42f5dc45847e65eda9a44dbdb0adf653a099d      1
     73 0x96615cb54c3d6634ac2bc0c5225d88e63a7b08a0      1
     74 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
     75 0xa249a826ff225e7fffd5a6fc118bef4feea2633f      1
     76 0xa549487362b9bc4a66804fbf4774b2cc26ffd4bb      1
     77 0xa8ff5ba66bf96bac81d81058d610b9b8c4cacff0      1
     78 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     79 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
     80 0xaf2e919b59734b8d99f6bc1f62dc63d6519d14bc      1
     81 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
     82 0xb07952a55bf9c45c268f37c3631823df50ac721a      1
     83 0xb6079e8c2795152e0e52774782406fe4185bb24a      1
     84 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     85 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
     86 0xbc3523ec30599d38d49bb4d51016ac58c4b6b27a      1
     87 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
     88 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
     89 0xc3273e027c6fe74dbf941a2b4e91f5e5d8103762      1
     90 0xc5aca861be2856d25821f3ef0317950c369044fa      1
     91 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
     92 0xca177d4b3e6d4c2778912b47e6aae49b7c429819      1
     93 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
     94 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
     95 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
     96 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
     97 0xd502b0383fbf710da7c85614810f6ea109bb7ac2      1
     98 0xd92df6b7fe238fe2c14fdb5816eebcc131b53360      1
     99 0xdb4a4055913cdeeef3b6560f9a147c1bfeb77025      1
    100 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    101 0xdca95d7f7533548a9124edc6959fcdb2e56f0b07      1
    102 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1
    103 0xe91ef2fe89a7a439e1e664ea7ac862849723c71e      1
    104 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    105 0xee119e2fea524c93aea9962de5308070682801b1      1
    106 0xeebb6ab3d5f0b402efad1eca6f72f880b66066c9      1
    107 0xf05ed0587b069975ed20308096699f02dd36fc30      1
    108 0xf1a67d67cc97e9755e2c8b13a6ee3e538bf830d9      1
    109 0xf1dafb1ca027e69b67952e8a385e7f43791f2e70      1
    110 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    111 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    112 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    113 0xf889b1d99b440aaa48839d7032088d96f7b3bceb      1
    114 0xff559fc20d9b6d78e6e570d234b69c18142bb65e      1

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
