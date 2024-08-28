
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:463         Length:463         Min.   :1   Length:463        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:463        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20596969 # https://etherscan.io/block/20596969
block_hash <- "0xb2edc2bd0ec0763d6d17b033cfb88887fa56f78e515cb3544e0b6cd345505944"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4655 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","KnownOrigin2","Dilemmas","Mirage","MakersPlace","MakersPlace2","ReflectionBeyondTime","WhisperWithintheBlack","CatastrophicInterference","ChromasQuietDialogue","DontKiss","Foundation","foxbreathe","HAIROFMEDUSA","MirrortouchSynesthesia","ReflectionsfromWonderland","Surreal","SynestheticSymphony"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ADispassionateMadnessEditions","CosmicConvergenceEditions","CosmicPhantomEditions","HAIROFMEDUSAEditions","HAIROFMEDUSA2Editions","MindsPendulumEditions","SoaringDreamsEditions","VanishingEchoesEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","KnownOrigin2","Dilemmas","Mirage","MakersPlace","MakersPlace2","ReflectionBeyondTime","WhisperWithintheBlack","CatastrophicInterference","ChromasQuietDialogue","DontKiss","Foundation","foxbreathe","HAIROFMEDUSA","MirrortouchSynesthesia","ReflectionsfromWonderland","Surreal","SynestheticSymphony","ADispassionateMadnessEditions","CosmicConvergenceEditions","CosmicPhantomEditions","HAIROFMEDUSAEditions","HAIROFMEDUSA2Editions","MindsPendulumEditions","SoaringDreamsEditions","VanishingEchoesEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00668bd79ede077b99bbe1c4db59418bc333d4cf      1
      2 0x02ae9223c40c3ba7178f585541d6b7abfdc52ecb      1
      3 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      4 0x046ed5bba6d28333bbbdc907fbf8a136f2711f17      1
      5 0x05e5c46f7dbf28b05773a5dae86d6a019f05d301      1
      6 0x0a2492d1b9a80fe094a011d645666653f3d3fc09      1
      7 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
      8 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
      9 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
     10 0x15790bcdb4de080854510c6474834850843b41df      1
     11 0x172737bdb7b80a3fc43ac28164db0bbc47a2bc56      1
     12 0x1ad60130a2528c6f73a8c6e50758532949627dfd      1
     13 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     14 0x1c90d76468176a0caa4baf141677f253f73c83c2      1
     15 0x1cc1983396e55e925b777a0d70dfe8b56edee942      1
     16 0x1ce9f4a1dc6b9b64692d50441edd00beb703cb47      1
     17 0x1d8a9e8d00f6b7f65dac2078475119bc02da0682      1
     18 0x1dbfad0b15462a057f69c2c296e5dacc706edf2e      1
     19 0x1fb741e055f49f38f8032b1cc2be3646b68ce891      1
     20 0x208b82b04449cd51803fae4b1561450ba13d9510      1
     21 0x22a1789dd5ee19151356a05cd715d73081d833f8      1
     22 0x22fb4210001e372d093a18b85a3d663b276a0c65      1
     23 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     24 0x234e228a33b516500388ffae36034fa5bd2f668b      1
     25 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     26 0x2776dca31dfc7cce9c30a94917d0d72605b52ada      1
     27 0x2c6eadd3a66d3aa0cb000e5bfbc714f0c477a17e      1
     28 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     29 0x2e307ceab1b4c5f4cac508e3b13c3dbfe86a3c81      1
     30 0x2e8140ce18c020ea09e5eb3fff1b9ce3b0adb7b9      1
     31 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     32 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     33 0x3257aa1aaf7c0d3784e2a243d29102736d8d12e8      1
     34 0x3282f7a8d396fd37927a63897fa9ca98178ec480      1
     35 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     36 0x3433c2753da24df566ea14c40584179e97396cf3      1
     37 0x352db9eb25d487b4b1933388cd44bb2ad75e6e84      1
     38 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     39 0x3678bd5bd67b2f7363fdee06dfb390ba0bc64206      1
     40 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
     41 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     42 0x3805e9b956aca92899ef0c6faa7dfad20b5c5dac      1
     43 0x39722bc419824a3786572e0607ec1ef3d1e36bec      1
     44 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
     45 0x3e64014d2d3f0c3a3b0404110cea9060b4565e61      1
     46 0x40f0b846da70b06a3e509ba1bfb044541afcbfb1      1
     47 0x423a0a0f3479ab24b2b04c8dba8bbe0e2bbb5cc6      1
     48 0x42b23bb838525d458f2b8109219ef45d229067b7      1
     49 0x42f7c5275ac4372156027d939843c9c42523df2e      1
     50 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     51 0x463d5e23b9d49462d56994d411e5a9c2b7c4f1ad      1
     52 0x475226b2a2b68401ed0472761ce8d4cdc09d2999      1
     53 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
     54 0x4842fa65f19b4e6709c5ff3a5b2637d4614599e5      1
     55 0x49930606eee3f2d0e20898b8f0b0e5f517d194f9      1
     56 0x49fca680d7e1a3e3a647806c4f1cab93775ad1c5      1
     57 0x4f86fa2eb7b81e98954554f533589114ef2e171b      1
     58 0x5145fdb71cc57af2f971fbfd5ba9ac2c3326f0dd      1
     59 0x5219b866323ea879a3e34b965b776c5964433a8a      1
     60 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     61 0x53d7ef1523faa9b1cc2883df3a3f92a4d6273704      1
     62 0x569af884b14715ffecfc7d76e864caed2140cfa6      1
     63 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     64 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
     65 0x673e98e21404b1892e843163d2c018074640cbdc      1
     66 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
     67 0x69f7808a895543238a01bf22de15516f55a1bc23      1
     68 0x6ba926e2873ae7bd618688cb9d55f912b1a43b0d      1
     69 0x6c3675c755c06b236cb10b5acfa0445fd8aad455      1
     70 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     71 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
     72 0x6e85e758aed0e2a452607c7ba1895a03c8500750      1
     73 0x6f5f19c009fba91650a71e46cad764bc0c281a09      1
     74 0x7186d0ebafb8103f87cc500649389abdd4e756e7      1
     75 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     76 0x73b4649dc734576ea79854e9fd04fb6b548037e0      1
     77 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
     78 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
     79 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     80 0x762da606029d3120735aa1eec15464e265db7a3c      1
     81 0x76b588e62f9ce0496861711832567f129959eb19      1
     82 0x76ef45f256587238ae279e957cc5e84bb3b3aaf1      1
     83 0x78120f886ea64b48be46898a97051dadb9fb7bad      1
     84 0x7843862d114e579d29d60124690ab6e11b22e4a6      1
     85 0x78667266ef6aa9d647827cc19a9d789416ba36d2      1
     86 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     87 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     88 0x7da2bb329125b3b72c7ce90f4263e9d904033588      1
     89 0x7ee977186f9a26e7b83b6611ff47b1a1a3265eff      1
     90 0x7f88af45bf2ddda7adff78035e2d5dc674541acc      1
     91 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     92 0x82f23de5a474fc904041c357576afe53c30dd250      1
     93 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
     94 0x85b7f6346a85ad39a83e0b37c99d7edc00fcf9c0      1
     95 0x89cc08700dcba9d4bad5295dee2a111b90b39917      1
     96 0x8a9d49a6e9d037843560091fc280b9ff9819e462      1
     97 0x8b70750731b6dde6c3b91fcd84b54e71744684f6      1
     98 0x8c01de512c9fb73c0e9fccdc1ae4d2644db65030      1
     99 0x8c28488b493e4a7cff37717456036937db013f9f      1
    100 0x8c47286ffca3d75cc3b15ffe11093abe01913a3a      1
    101 0x8cb377959625e693986c6adef82fff01d4d91af8      1
    102 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    103 0x8f77d28c23479f2abeb72a31c577ac0ae5de1ebf      1
    104 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    105 0x9737f0f663096c91f7aff60fca3fbfe9a36a9a7d      1
    106 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
    107 0xa03194aaba024af9b092b602a856f76b7d94c323      1
    108 0xa16ce88f9f702d390244ce016f9416b23c9c3027      1
    109 0xa8db86817c83512de64ff054d4d9d3852b2f19dd      1
    110 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    111 0xaeb8aabe5fef661d8091c27b4f18771dcea61a8c      1
    112 0xafe6e36ab8c048a24fe58078468c5b82989425f4      1
    113 0xb23be5baef9983a1c72d186774b537d95c5d218f      1
    114 0xb2cae29f79fa0c313cb020197fb8d3db8d9d4628      1
    115 0xb5fc94e9d6c0ebb059c271f3f3e3a56206bf3c6a      1
    116 0xb80f7a714e5cddd7867276bda3409230d233bb3e      1
    117 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    118 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
    119 0xbdb6fdd2bf2aa01051540a0630ae568282a4bbef      1
    120 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    121 0xc4c7e7fae445149f7b27023beb06d3cffd5a0c8e      1
    122 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    123 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    124 0xcf9741bbce8ba8ec2b0dc8f23399a0bcf5c019d5      1
    125 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    126 0xd348f49907f0b8c99e428e57f6aaea5ccba332a8      1
    127 0xd515e7cb30befa0e93713ebe8cb31c4445927699      1
    128 0xd6de4506eb4e02eda3f2ac2978110f351f42a7ce      1
    129 0xd7f865907b928d70dd2c35c74054fbf6fed89a10      1
    130 0xd9cfab54f1234aeea22b2818ab919866a2809c1c      1
    131 0xd9ff1fbd68e910392dc404f8df0fcd23a64921c3      1
    132 0xdcd1ad8211354ecd58d4a8159baedd138642339c      1
    133 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    134 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    135 0xe9ceaf49e5afd867935bf9d3ad285f68fdeca4fb      1
    136 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    137 0xec973075e76fd3fb76b8b52222e59b99c11beef2      1
    138 0xecab3ed0d13c9172f54d433106ece2a8aa0e674a      1
    139 0xf0c7401fb586b44d450ccf3279668173470c9c1e      1
    140 0xf31781b4811b737e66662997679fb07dacf63355      1
    141 0xf32d7651f345305af13eddb101d9933150bb5859      1
    142 0xf3442dd014446b60d19cdcce0313160c3e515d41      1
    143 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    144 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    145 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    146 0xf7356eac86640c188eb5116b593e089f3e1f2131      1
    147 0xfa893e0326bc79aa30d72d64359e784770376d90      1
    148 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    149 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    150 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 53 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x01c463dfcbd285cf4733f6ec1e4bf58e8182b088      1
     3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     4 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     5 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     7 0x2548c3a70fa292630854e1b0b767df163eb3b2f0      1
     8 0x26439aeb008d63b6686798b5f08a84c7abefbd80      1
     9 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
    10 0x34b045e74b751dd5014653c73f910aea2402005a      1
    11 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    12 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
    13 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
    14 0x493e7c2e9cba0b80a9ce4b84e567ee6ae8501471      1
    15 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
    16 0x4c9b3b18a54e59b23a75250425670e6a7467d225      1
    17 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
    18 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
    19 0x58b258796254e49447fd2e5755089f117fe062fd      1
    20 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    21 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
    22 0x5f2d8fb9e5153bd38f2c2e9b01ed6f7270e3b79e      1
    23 0x65ae73a4cd82f2b4874f557d4d5763d227b26b8b      1
    24 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    25 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    26 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    27 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    28 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    29 0x78cdb41120f53f6361ca94a807283a929350beef      1
    30 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
    31 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    32 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    33 0x8db76be350a1e612281e8885ad87db092ee5c4f8      1
    34 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    35 0x8f4a07fe6c87e349388236b8d1928e14f3c7fc7e      1
    36 0x904a819e486d4af2653eec8e424599335e2f190e      1
    37 0x93dddb9ff5fe1d8d2d3ea6d74b191d3dfb4c5843      1
    38 0x97963380a1ed099905a7f3d8207e650caf2fdc05      1
    39 0x9ace182c0d1f30e6a8b90650659de6f29df6af23      1
    40 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    41 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    42 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    43 0xb977eeaf681d0034653e8529f6c270fd059d9623      1
    44 0xbb86c57ebafbb3426b1d01b870ec45996c20e2cf      1
    45 0xd237d3b488dcddbd85408d2e879988f97ac81280      1
    46 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    47 0xd4d2b38106e40f2693b617314ba5f406985864eb      1
    48 0xdad29981b5feefeeaf3ef92e678e53c5620a1fd8      1
    49 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1
    50 0xe92689183961135f51bd5edc601a0aa123c069f3      1
    51 0xee4243eea50d8840f459da4fada53679aec1c702      1
    52 0xf53af966dab1afaaac1171dd941148384d38417d      1
    53 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 203 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00668bd79ede077b99bbe1c4db59418bc333d4cf      1
      2 0x00ff192363430a35abbf968c535b64147e88abdb      1
      3 0x01c463dfcbd285cf4733f6ec1e4bf58e8182b088      1
      4 0x02ae9223c40c3ba7178f585541d6b7abfdc52ecb      1
      5 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      6 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      7 0x046ed5bba6d28333bbbdc907fbf8a136f2711f17      1
      8 0x05e5c46f7dbf28b05773a5dae86d6a019f05d301      1
      9 0x0a2492d1b9a80fe094a011d645666653f3d3fc09      1
     10 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
     11 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     12 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     13 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
     14 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     15 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
     16 0x15790bcdb4de080854510c6474834850843b41df      1
     17 0x172737bdb7b80a3fc43ac28164db0bbc47a2bc56      1
     18 0x1ad60130a2528c6f73a8c6e50758532949627dfd      1
     19 0x1b8d42f0451bc9077e58b1681abd874f174f376c      1
     20 0x1c90d76468176a0caa4baf141677f253f73c83c2      1
     21 0x1cc1983396e55e925b777a0d70dfe8b56edee942      1
     22 0x1ce9f4a1dc6b9b64692d50441edd00beb703cb47      1
     23 0x1d8a9e8d00f6b7f65dac2078475119bc02da0682      1
     24 0x1dbfad0b15462a057f69c2c296e5dacc706edf2e      1
     25 0x1fb741e055f49f38f8032b1cc2be3646b68ce891      1
     26 0x208b82b04449cd51803fae4b1561450ba13d9510      1
     27 0x22a1789dd5ee19151356a05cd715d73081d833f8      1
     28 0x22fb4210001e372d093a18b85a3d663b276a0c65      1
     29 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     30 0x234e228a33b516500388ffae36034fa5bd2f668b      1
     31 0x2548c3a70fa292630854e1b0b767df163eb3b2f0      1
     32 0x26439aeb008d63b6686798b5f08a84c7abefbd80      1
     33 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     34 0x2776dca31dfc7cce9c30a94917d0d72605b52ada      1
     35 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     36 0x2c6eadd3a66d3aa0cb000e5bfbc714f0c477a17e      1
     37 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     38 0x2e307ceab1b4c5f4cac508e3b13c3dbfe86a3c81      1
     39 0x2e8140ce18c020ea09e5eb3fff1b9ce3b0adb7b9      1
     40 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     41 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     42 0x3257aa1aaf7c0d3784e2a243d29102736d8d12e8      1
     43 0x3282f7a8d396fd37927a63897fa9ca98178ec480      1
     44 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     45 0x3433c2753da24df566ea14c40584179e97396cf3      1
     46 0x34b045e74b751dd5014653c73f910aea2402005a      1
     47 0x352db9eb25d487b4b1933388cd44bb2ad75e6e84      1
     48 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     49 0x3678bd5bd67b2f7363fdee06dfb390ba0bc64206      1
     50 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     51 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
     52 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     53 0x3805e9b956aca92899ef0c6faa7dfad20b5c5dac      1
     54 0x39722bc419824a3786572e0607ec1ef3d1e36bec      1
     55 0x3d4714e10f17a17bc4d2de4c5ee0cbdf07de351d      1
     56 0x3e64014d2d3f0c3a3b0404110cea9060b4565e61      1
     57 0x40f0b846da70b06a3e509ba1bfb044541afcbfb1      1
     58 0x423a0a0f3479ab24b2b04c8dba8bbe0e2bbb5cc6      1
     59 0x42b23bb838525d458f2b8109219ef45d229067b7      1
     60 0x42f7c5275ac4372156027d939843c9c42523df2e      1
     61 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     62 0x463d5e23b9d49462d56994d411e5a9c2b7c4f1ad      1
     63 0x4660e9cbe586a7ec727e076b5593eab9c51b5e4c      1
     64 0x475226b2a2b68401ed0472761ce8d4cdc09d2999      1
     65 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
     66 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     67 0x4842fa65f19b4e6709c5ff3a5b2637d4614599e5      1
     68 0x493e7c2e9cba0b80a9ce4b84e567ee6ae8501471      1
     69 0x49930606eee3f2d0e20898b8f0b0e5f517d194f9      1
     70 0x49fca680d7e1a3e3a647806c4f1cab93775ad1c5      1
     71 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
     72 0x4c9b3b18a54e59b23a75250425670e6a7467d225      1
     73 0x4f86fa2eb7b81e98954554f533589114ef2e171b      1
     74 0x4f8cdb39327588a2b11edcebc50cc1302aa87cfa      1
     75 0x5145fdb71cc57af2f971fbfd5ba9ac2c3326f0dd      1
     76 0x5219b866323ea879a3e34b965b776c5964433a8a      1
     77 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     78 0x53d7ef1523faa9b1cc2883df3a3f92a4d6273704      1
     79 0x569af884b14715ffecfc7d76e864caed2140cfa6      1
     80 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     81 0x5779ed044776ae2d0fb4c3ec9be5029b203df7ce      1
     82 0x58b258796254e49447fd2e5755089f117fe062fd      1
     83 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     84 0x59d14847c0d1584a867fe98acabb1dc95ab1496f      1
     85 0x5f2d8fb9e5153bd38f2c2e9b01ed6f7270e3b79e      1
     86 0x65ae73a4cd82f2b4874f557d4d5763d227b26b8b      1
     87 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
     88 0x673e98e21404b1892e843163d2c018074640cbdc      1
     89 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
     90 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     91 0x69f7808a895543238a01bf22de15516f55a1bc23      1
     92 0x6ba926e2873ae7bd618688cb9d55f912b1a43b0d      1
     93 0x6c3675c755c06b236cb10b5acfa0445fd8aad455      1
     94 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     95 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
     96 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     97 0x6e85e758aed0e2a452607c7ba1895a03c8500750      1
     98 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     99 0x6f5f19c009fba91650a71e46cad764bc0c281a09      1
    100 0x7186d0ebafb8103f87cc500649389abdd4e756e7      1
    101 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
    102 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    103 0x73b4649dc734576ea79854e9fd04fb6b548037e0      1
    104 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
    105 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
    106 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    107 0x762da606029d3120735aa1eec15464e265db7a3c      1
    108 0x76b588e62f9ce0496861711832567f129959eb19      1
    109 0x76ef45f256587238ae279e957cc5e84bb3b3aaf1      1
    110 0x78120f886ea64b48be46898a97051dadb9fb7bad      1
    111 0x7843862d114e579d29d60124690ab6e11b22e4a6      1
    112 0x78667266ef6aa9d647827cc19a9d789416ba36d2      1
    113 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    114 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    115 0x78cdb41120f53f6361ca94a807283a929350beef      1
    116 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    117 0x7da2bb329125b3b72c7ce90f4263e9d904033588      1
    118 0x7ee977186f9a26e7b83b6611ff47b1a1a3265eff      1
    119 0x7f88af45bf2ddda7adff78035e2d5dc674541acc      1
    120 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
    121 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    122 0x82f23de5a474fc904041c357576afe53c30dd250      1
    123 0x83a2fc8c7958422faa8a003e955751d696f546e1      1
    124 0x85b7f6346a85ad39a83e0b37c99d7edc00fcf9c0      1
    125 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    126 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    127 0x89cc08700dcba9d4bad5295dee2a111b90b39917      1
    128 0x8a9d49a6e9d037843560091fc280b9ff9819e462      1
    129 0x8b70750731b6dde6c3b91fcd84b54e71744684f6      1
    130 0x8c01de512c9fb73c0e9fccdc1ae4d2644db65030      1
    131 0x8c28488b493e4a7cff37717456036937db013f9f      1
    132 0x8c47286ffca3d75cc3b15ffe11093abe01913a3a      1
    133 0x8cb377959625e693986c6adef82fff01d4d91af8      1
    134 0x8db76be350a1e612281e8885ad87db092ee5c4f8      1
    135 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    136 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    137 0x8f4a07fe6c87e349388236b8d1928e14f3c7fc7e      1
    138 0x8f77d28c23479f2abeb72a31c577ac0ae5de1ebf      1
    139 0x904a819e486d4af2653eec8e424599335e2f190e      1
    140 0x93dddb9ff5fe1d8d2d3ea6d74b191d3dfb4c5843      1
    141 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    142 0x9737f0f663096c91f7aff60fca3fbfe9a36a9a7d      1
    143 0x97963380a1ed099905a7f3d8207e650caf2fdc05      1
    144 0x9ace182c0d1f30e6a8b90650659de6f29df6af23      1
    145 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
    146 0xa03194aaba024af9b092b602a856f76b7d94c323      1
    147 0xa16ce88f9f702d390244ce016f9416b23c9c3027      1
    148 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    149 0xa8db86817c83512de64ff054d4d9d3852b2f19dd      1
    150 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    151 0xaeb8aabe5fef661d8091c27b4f18771dcea61a8c      1
    152 0xafe6e36ab8c048a24fe58078468c5b82989425f4      1
    153 0xb23be5baef9983a1c72d186774b537d95c5d218f      1
    154 0xb2cae29f79fa0c313cb020197fb8d3db8d9d4628      1
    155 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    156 0xb5fc94e9d6c0ebb059c271f3f3e3a56206bf3c6a      1
    157 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    158 0xb80f7a714e5cddd7867276bda3409230d233bb3e      1
    159 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    160 0xb977eeaf681d0034653e8529f6c270fd059d9623      1
    161 0xbb86c57ebafbb3426b1d01b870ec45996c20e2cf      1
    162 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
    163 0xbdb6fdd2bf2aa01051540a0630ae568282a4bbef      1
    164 0xc00ae171acd39f84e66e19c7bad7bb676c1fe10c      1
    165 0xc4c7e7fae445149f7b27023beb06d3cffd5a0c8e      1
    166 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    167 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    168 0xcf9741bbce8ba8ec2b0dc8f23399a0bcf5c019d5      1
    169 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    170 0xd237d3b488dcddbd85408d2e879988f97ac81280      1
    171 0xd348f49907f0b8c99e428e57f6aaea5ccba332a8      1
    172 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    173 0xd4d2b38106e40f2693b617314ba5f406985864eb      1
    174 0xd515e7cb30befa0e93713ebe8cb31c4445927699      1
    175 0xd6de4506eb4e02eda3f2ac2978110f351f42a7ce      1
    176 0xd7f865907b928d70dd2c35c74054fbf6fed89a10      1
    177 0xd9cfab54f1234aeea22b2818ab919866a2809c1c      1
    178 0xd9ff1fbd68e910392dc404f8df0fcd23a64921c3      1
    179 0xdad29981b5feefeeaf3ef92e678e53c5620a1fd8      1
    180 0xdcd1ad8211354ecd58d4a8159baedd138642339c      1
    181 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    182 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    183 0xe7c803b8282b3175f19341419eee28e6a4d04a5a      1
    184 0xe92689183961135f51bd5edc601a0aa123c069f3      1
    185 0xe9ceaf49e5afd867935bf9d3ad285f68fdeca4fb      1
    186 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    187 0xec973075e76fd3fb76b8b52222e59b99c11beef2      1
    188 0xecab3ed0d13c9172f54d433106ece2a8aa0e674a      1
    189 0xee4243eea50d8840f459da4fada53679aec1c702      1
    190 0xf0c7401fb586b44d450ccf3279668173470c9c1e      1
    191 0xf31781b4811b737e66662997679fb07dacf63355      1
    192 0xf32d7651f345305af13eddb101d9933150bb5859      1
    193 0xf3442dd014446b60d19cdcce0313160c3e515d41      1
    194 0xf3fda50f3963e704e8ae85a982631c96afb999b2      1
    195 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    196 0xf53af966dab1afaaac1171dd941148384d38417d      1
    197 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    198 0xf7356eac86640c188eb5116b593e089f3e1f2131      1
    199 0xfa893e0326bc79aa30d72d64359e784770376d90      1
    200 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    201 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    202 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    203 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1

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
