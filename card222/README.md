
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:197         Length:197         Min.   :1   Length:197        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:197        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19694269 # https://etherscan.io/block/19694269
block_hash <- "0x412e0d999c386b929d00bb111776be3c4435a4c25a1ba294ae8cece3622183b6"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4539 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","F331","DREAMS","UNEASE","TheArtist","Dancewiththetreeoflife","Foundation","RITUALS","Meds","MemoryGlitches","SCMMRS","cemhah1of1","ALIENATED"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("TheGrailEditions","DazeEditions","XEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","F331","DREAMS","UNEASE","TheArtist","Dancewiththetreeoflife","Foundation","RITUALS","Meds","MemoryGlitches","SCMMRS","cemhah1of1","ALIENATED","TheGrailEditions","DazeEditions","XEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 36 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
     2 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     3 0x17b2d1911262b9cda80aa5354a1c13ee9027ace1      1
     4 0x18b8192ea6c5288b2537503addfc3aaebcd6599d      1
     5 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     6 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     7 0x3233b8409c420bfb6488c39e79ab7a36532aaada      1
     8 0x33ba48ec6863152420e0ba6f0badb286f03e9db5      1
     9 0x34605931106d6d42a78579e22cd3feb0b468cfa3      1
    10 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    11 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
    12 0x41079d359874ddcc0dda6ff1b17e7047467f45fa      1
    13 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
    14 0x533b9cdce87facf35c4066f48537660bf2bf16ad      1
    15 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    16 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
    17 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    18 0x708e5b9e572f169e0e1aba4f7ff0088c5e1a746a      1
    19 0x7290bf7488af285e5ae095dbc5234dd2eac9b61b      1
    20 0x83b97b9454d90791480cfda42c01d461d4d4eaa3      1
    21 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    22 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    23 0xa750329b5821d02e0a3977d36ffc5f9de51f0fb6      1
    24 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    25 0xadd2c95b591c28e48d8b6b94043ee6da40fffb5b      1
    26 0xbec69dfce4c1fa8b7843fee1ca85788d84a86b06      1
    27 0xc0760166512c505547e0bd2196a42b40984a7670      1
    28 0xc2de42cf18e7b5d9774050eba2d5e2a33bf3bf63      1
    29 0xc449f005667bef849261b35accf931a4bace48fb      1
    30 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    31 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    32 0xe71775afbe475bd88d44e27f4bf1f94118e33e6d      1
    33 0xe97c4939f1215a95eaf37516f2407494ee843359      1
    34 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    35 0xf4530e09f865c6dac296ef50e61cd1556f44f093      1
    36 0xf9b55e4046c36a182457f87c95385d2437335bc1      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
      2 0x02d78c75532a490dae2596e6b07a75857815cd70      1
      3 0x09b50000e8366309283f2733e6fb6ab59370c9ae      1
      4 0x0a3084260397df8f488ed9b60be7bca91dfed25c      1
      5 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
      6 0x0be9a6d00d7768dfc751a0e8936ee7ca8418b678      1
      7 0x0c50d718a7bed724ba11a1157a45059122ff5b73      1
      8 0x0d427a8f3e9b96a0d7770e634778bba0990754a7      1
      9 0x0f5c4502bfa1e366b47880b3181c051380f89c0b      1
     10 0x0fb39dfd1b192bbb04e0e95c844329041b48f11d      1
     11 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     12 0x11ce4e1f33145da3903691742af1379703e1604c      1
     13 0x1341df844780b66af4ccc98ae0f34be87eabe1d5      1
     14 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     15 0x1534fdcb6052d8d2a989de505fa02e97d0a84a01      1
     16 0x156efcf76828e62ffa40a406677f8f2a46e14924      1
     17 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     18 0x1ad3dde7cf886536b4a8d7f5a2998577c4bfa48f      1
     19 0x1cbb87bb9a4dd0316189eede2277a58590dc124a      1
     20 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     21 0x2117bf88b4cb0186eaa87500a045fc998290e42a      1
     22 0x21b5a2f2d0b87f01ea030086b586bc4d63d516c3      1
     23 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     24 0x24f668020aa4c0de03e9efd376eccc4db9a386ae      1
     25 0x29b54625d1ac801d995aa328881592548395518b      1
     26 0x2ec6bb48dacb6ba145b3c877c0bb803da82d7cf7      1
     27 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
     28 0x3a43f6e92708cb75425b0b1063526efbe783864d      1
     29 0x3bb2272ddc716eafb7ef8cf8060267f23973d0eb      1
     30 0x429c8a65f4858ab2e6dc7112e88f0c0796a42692      1
     31 0x46b82704baf2e08ec2ffe72f32b6f92772bea15d      1
     32 0x4723028be4f45490ab0877a81fa1f14b4b8b6c7a      1
     33 0x47baba9b83c7cd484297e771d99076e904647511      1
     34 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
     35 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     36 0x4f74381834616f17b86caea2de94e18e3996e5a3      1
     37 0x4fc3b50ab3c30d294ab2ab8b06bd178c54157a6e      1
     38 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     39 0x5529e0608cfc0cab5bb86b59cba7e88f0f66dfef      1
     40 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
     41 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     42 0x5959002cb524181d5526714c2804c3775212d823      1
     43 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
     44 0x5cf6abe59292d3cbc8fedf9e610d34162606e9a0      1
     45 0x5f77911c11b5c131cb81fa546b89d5d83ecc6b00      1
     46 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     47 0x64ad18fd2cde41578d231955c98a714f8cbac239      1
     48 0x68d39be8c66851afbb61abb3b2e0a8ef59b58e80      1
     49 0x6abc68a20d82fd3793e2146249d72f241e57eb0e      1
     50 0x6dff1d922a97fda351aad8c5f7e8f69be187e0a6      1
     51 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
     52 0x71042c2a74bbc93ce9b8d4eb6b42fc6f07ef31d5      1
     53 0x719622742f0339d930663b3df9e4d21180a75f8d      1
     54 0x71a0496f59c0e2bb91e48bedd97dc233fe76319f      1
     55 0x77039399801f462b4ed13444a266b16355c471bf      1
     56 0x7752b30ebbc753cab112298db890c83dcdc500bf      1
     57 0x78f5761b5a541c2e0f7d1921eaa14a0546f41396      1
     58 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     59 0x7ebc6689843ca9212fb945830fdc248c436944e7      1
     60 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     61 0x7f2cf626a94c1a15cf1f208a66f08d8aa5660fcf      1
     62 0x8822f60f877cd912bcf3124532418913712e1516      1
     63 0x8c01bb23a1e7c0ce2b9d63f92e3b95955cf8ace4      1
     64 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     65 0x9699ca29b2a70c5f7c2e0cd3f173cd7a4c75202e      1
     66 0x97145d1d5e6fd6cc9285f4b2584f6674bb17e183      1
     67 0x98b5532ff6201d9bb679eee90cc34c10137998f4      1
     68 0x9ea7fa95ae8b4bf42d389c0225e0faedb2d3ad58      1
     69 0xa04d7b651b72a6b9d14f53a92220ea06bfff2dc4      1
     70 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
     71 0xa6d1ad3211ac69965c7221ae987f6190687d4c75      1
     72 0xacde31cb0eba050dbbe499051a2f5bd9feaba60f      1
     73 0xae3f1213e822aedcefad4c0404e75389504d0049      1
     74 0xb011ab6f339acf72996828e5ceffa4a9556fc78d      1
     75 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
     76 0xb0e5ca05aa921da8b194766341ee6630f4288e98      1
     77 0xb98c19ee72cee0614f70a1cb164faf9fdcb15c65      1
     78 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     79 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
     80 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
     81 0xbfec9ba7567320f2548736cd88b3132dbfb373fd      1
     82 0xc2a4d3ffc69391f67cd06c62813b9d8117019797      1
     83 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
     84 0xc5aca861be2856d25821f3ef0317950c369044fa      1
     85 0xcb85bf5787f7616315ced491d1c6a0589bc09178      1
     86 0xcdc021f199895816d3f3c0fdc3e843590b5cf00b      1
     87 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
     88 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
     89 0xd3300c1463586a6369616a567eb9b45485ed2e2a      1
     90 0xd5f9cb4c7c6d4fae16975c9a3baf19c838d95366      1
     91 0xdac8fc039f633969116d412522b3338e3f1eba44      1
     92 0xe0580e24364dd221045c68a674f12666bd3e4253      1
     93 0xe74f61730001bcce09b2006b176ab1babde27ea7      1
     94 0xe99c714d1d649f3dada0202d4728e1c424730b74      1
     95 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
     96 0xeb2be116a84c9603806e986314c7270d17f0021d      1
     97 0xf1ce03f0f25304a1fb911010f6e3232390ab121d      1
     98 0xf204b02abac29ddb66683abd7d0652905cd71645      1
     99 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    100 0xf8f327e4c1cbcb0ef014031b84069b9d3579f42d      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 136 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
      2 0x02d78c75532a490dae2596e6b07a75857815cd70      1
      3 0x09b50000e8366309283f2733e6fb6ab59370c9ae      1
      4 0x0a3084260397df8f488ed9b60be7bca91dfed25c      1
      5 0x0a7d89518246e1bb97d0e26140af4e65909a1edb      1
      6 0x0be9a6d00d7768dfc751a0e8936ee7ca8418b678      1
      7 0x0c50d718a7bed724ba11a1157a45059122ff5b73      1
      8 0x0d427a8f3e9b96a0d7770e634778bba0990754a7      1
      9 0x0f5c4502bfa1e366b47880b3181c051380f89c0b      1
     10 0x0fb39dfd1b192bbb04e0e95c844329041b48f11d      1
     11 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     12 0x11ce4e1f33145da3903691742af1379703e1604c      1
     13 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
     14 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     15 0x1341df844780b66af4ccc98ae0f34be87eabe1d5      1
     16 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     17 0x1534fdcb6052d8d2a989de505fa02e97d0a84a01      1
     18 0x156efcf76828e62ffa40a406677f8f2a46e14924      1
     19 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     20 0x17b2d1911262b9cda80aa5354a1c13ee9027ace1      1
     21 0x18b8192ea6c5288b2537503addfc3aaebcd6599d      1
     22 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     23 0x1ad3dde7cf886536b4a8d7f5a2998577c4bfa48f      1
     24 0x1cbb87bb9a4dd0316189eede2277a58590dc124a      1
     25 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     26 0x2117bf88b4cb0186eaa87500a045fc998290e42a      1
     27 0x21b5a2f2d0b87f01ea030086b586bc4d63d516c3      1
     28 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     29 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     30 0x24f668020aa4c0de03e9efd376eccc4db9a386ae      1
     31 0x29b54625d1ac801d995aa328881592548395518b      1
     32 0x2ec6bb48dacb6ba145b3c877c0bb803da82d7cf7      1
     33 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
     34 0x3233b8409c420bfb6488c39e79ab7a36532aaada      1
     35 0x33ba48ec6863152420e0ba6f0badb286f03e9db5      1
     36 0x34605931106d6d42a78579e22cd3feb0b468cfa3      1
     37 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     38 0x3a43f6e92708cb75425b0b1063526efbe783864d      1
     39 0x3bb2272ddc716eafb7ef8cf8060267f23973d0eb      1
     40 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
     41 0x41079d359874ddcc0dda6ff1b17e7047467f45fa      1
     42 0x429c8a65f4858ab2e6dc7112e88f0c0796a42692      1
     43 0x46b82704baf2e08ec2ffe72f32b6f92772bea15d      1
     44 0x4723028be4f45490ab0877a81fa1f14b4b8b6c7a      1
     45 0x47baba9b83c7cd484297e771d99076e904647511      1
     46 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
     47 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     48 0x4f7106c4c161e138b1a39f47c91c4f1043437fb2      1
     49 0x4f74381834616f17b86caea2de94e18e3996e5a3      1
     50 0x4fc3b50ab3c30d294ab2ab8b06bd178c54157a6e      1
     51 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     52 0x533b9cdce87facf35c4066f48537660bf2bf16ad      1
     53 0x5529e0608cfc0cab5bb86b59cba7e88f0f66dfef      1
     54 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     55 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
     56 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     57 0x5959002cb524181d5526714c2804c3775212d823      1
     58 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
     59 0x5cf6abe59292d3cbc8fedf9e610d34162606e9a0      1
     60 0x5f77911c11b5c131cb81fa546b89d5d83ecc6b00      1
     61 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     62 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     63 0x64ad18fd2cde41578d231955c98a714f8cbac239      1
     64 0x68d39be8c66851afbb61abb3b2e0a8ef59b58e80      1
     65 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     66 0x6abc68a20d82fd3793e2146249d72f241e57eb0e      1
     67 0x6dff1d922a97fda351aad8c5f7e8f69be187e0a6      1
     68 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
     69 0x708e5b9e572f169e0e1aba4f7ff0088c5e1a746a      1
     70 0x71042c2a74bbc93ce9b8d4eb6b42fc6f07ef31d5      1
     71 0x719622742f0339d930663b3df9e4d21180a75f8d      1
     72 0x71a0496f59c0e2bb91e48bedd97dc233fe76319f      1
     73 0x7290bf7488af285e5ae095dbc5234dd2eac9b61b      1
     74 0x77039399801f462b4ed13444a266b16355c471bf      1
     75 0x7752b30ebbc753cab112298db890c83dcdc500bf      1
     76 0x78f5761b5a541c2e0f7d1921eaa14a0546f41396      1
     77 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
     78 0x7ebc6689843ca9212fb945830fdc248c436944e7      1
     79 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     80 0x7f2cf626a94c1a15cf1f208a66f08d8aa5660fcf      1
     81 0x83b97b9454d90791480cfda42c01d461d4d4eaa3      1
     82 0x8822f60f877cd912bcf3124532418913712e1516      1
     83 0x8c01bb23a1e7c0ce2b9d63f92e3b95955cf8ace4      1
     84 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     85 0x9699ca29b2a70c5f7c2e0cd3f173cd7a4c75202e      1
     86 0x97145d1d5e6fd6cc9285f4b2584f6674bb17e183      1
     87 0x98b5532ff6201d9bb679eee90cc34c10137998f4      1
     88 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
     89 0x9ea7fa95ae8b4bf42d389c0225e0faedb2d3ad58      1
     90 0xa04d7b651b72a6b9d14f53a92220ea06bfff2dc4      1
     91 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
     92 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
     93 0xa6d1ad3211ac69965c7221ae987f6190687d4c75      1
     94 0xa750329b5821d02e0a3977d36ffc5f9de51f0fb6      1
     95 0xacde31cb0eba050dbbe499051a2f5bd9feaba60f      1
     96 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
     97 0xadd2c95b591c28e48d8b6b94043ee6da40fffb5b      1
     98 0xae3f1213e822aedcefad4c0404e75389504d0049      1
     99 0xb011ab6f339acf72996828e5ceffa4a9556fc78d      1
    100 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    101 0xb0e5ca05aa921da8b194766341ee6630f4288e98      1
    102 0xb98c19ee72cee0614f70a1cb164faf9fdcb15c65      1
    103 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    104 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
    105 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    106 0xbec69dfce4c1fa8b7843fee1ca85788d84a86b06      1
    107 0xbfec9ba7567320f2548736cd88b3132dbfb373fd      1
    108 0xc0760166512c505547e0bd2196a42b40984a7670      1
    109 0xc2a4d3ffc69391f67cd06c62813b9d8117019797      1
    110 0xc2de42cf18e7b5d9774050eba2d5e2a33bf3bf63      1
    111 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    112 0xc449f005667bef849261b35accf931a4bace48fb      1
    113 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    114 0xcb85bf5787f7616315ced491d1c6a0589bc09178      1
    115 0xcdc021f199895816d3f3c0fdc3e843590b5cf00b      1
    116 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    117 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    118 0xd3300c1463586a6369616a567eb9b45485ed2e2a      1
    119 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    120 0xd5f9cb4c7c6d4fae16975c9a3baf19c838d95366      1
    121 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    122 0xdac8fc039f633969116d412522b3338e3f1eba44      1
    123 0xe0580e24364dd221045c68a674f12666bd3e4253      1
    124 0xe71775afbe475bd88d44e27f4bf1f94118e33e6d      1
    125 0xe74f61730001bcce09b2006b176ab1babde27ea7      1
    126 0xe97c4939f1215a95eaf37516f2407494ee843359      1
    127 0xe99c714d1d649f3dada0202d4728e1c424730b74      1
    128 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    129 0xeb2be116a84c9603806e986314c7270d17f0021d      1
    130 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    131 0xf1ce03f0f25304a1fb911010f6e3232390ab121d      1
    132 0xf204b02abac29ddb66683abd7d0652905cd71645      1
    133 0xf4530e09f865c6dac296ef50e61cd1556f44f093      1
    134 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    135 0xf8f327e4c1cbcb0ef014031b84069b9d3579f42d      1
    136 0xf9b55e4046c36a182457f87c95385d2437335bc1      1

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