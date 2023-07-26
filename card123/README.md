
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:40280       Length:40280       Min.   :1   Length:40280      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:40280      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17768569 # https://etherscan.io/block/17768569
block_hash <- "0x7915f959516b3525716ee5481e6562cfe6dcbd0799cc989289e1391534af586b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4418 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SuperRare","SamJ","Foundation","KnownOrigin","Rarible"), address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=100,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_pick=100,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_pick=100,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_pick=100,address_max=1)


allow_raw             <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles         <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     2 0x0ff891eeb165360c138e39c1056c58d402383fa1      1
     3 0x16d90c5fad600e56c3f4b1522a6075a00c08807d      1
     4 0x35860583266f6c6cad540ef07b4d36ec0d925916      1
     5 0x42ec9ab7db1ad0b3a0c77bbe77c436c58db8eb7e      1
     6 0x4812fc190e06b7f81665805564e2450b4e2fd490      1
     7 0x49c044afa2c8b8543798005611d53cdb35431c48      1
     8 0x520d9567cd51a1ff7babb9139ecc63d8923c3252      1
     9 0x57074bd8cd63bca19c1d2f121f98e2f214adf45c      1
    10 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
    11 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    12 0x882ff1134f17017fe2c1f4b464eee7d4f0b0d476      1
    13 0x9aaddc5040d2093dc0dafc0803cbfca822341bca      1
    14 0xa7b4c5f8d8e7bde30f1eeecde193c255dc92c9c7      1
    15 0xd9ff1fbd68e910392dc404f8df0fcd23a64921c3      1
    16 0xe40a2a4c6dfc11f40f2bee39a3785f2c33c1d33b      1
    17 0xe50f359b5bc6dac1d5eb19e1b2fde2cf322d6c02      1
    18 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    19 0xeafe529121b4caa91791c277b89140ed7a9ef1c0      1
    20 0xeda7ff0ab09da29a7eaa725e56be4898fdbcd288      1
    21 0xf0d6999725115e3ead3d927eb3329d63afaec09b      1
    22 0xf2186dc29f85b496a2ce2e4347c69332b29cec50      1

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
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     7 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    15 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    19 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    29 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    33 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    34 0x69e68074f1aada957edd39c5eae0069973343f30      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    40 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    44 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    45 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    53 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    54 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    55 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    56 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    57 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    58 0xbf814810b44978de273191fd612aa47f7b69d564      1
    59 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    60 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    61 0xc4e8b752c53df925013e03fe4d2273a8ccb6c545      1
    62 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    63 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    64 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    65 0xcfbf3aa72bcc8af0ba064d9e7b83888495a280de      1
    66 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    67 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
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

## Allow Random Memes 1 Phase 1

``` r
c(allow_memesRandom1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_randomMemes1_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
      2 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
      3 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
      4 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
      5 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
      6 0x111818a51c4177e8980566beea68fe334be7b76a      1
      7 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
      8 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
      9 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     10 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
     11 0x289256fa6de33947fd292a9e94a738c3d986f8e5      1
     12 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     13 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     14 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     15 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     16 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     17 0x388160a99390392278afdba240046b8b5e73f77b      1
     18 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
     19 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     20 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     21 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     22 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
     23 0x45360f55024132b3110166e1b327170daa2cc299      1
     24 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     25 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     26 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     27 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     28 0x52690f90740621f89f58521433e9b0921d626708      1
     29 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     30 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     31 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
     32 0x6140f00e4ff3936702e68744f2b5978885464cbb      1
     33 0x614b89f072ea263a9387460963142e73548fbaf1      1
     34 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
     35 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
     36 0x69e68074f1aada957edd39c5eae0069973343f30      1
     37 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
     38 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     39 0x714b194ba18daf0288848054bec788de08c00cea      1
     40 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
     41 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
     42 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
     43 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     44 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
     45 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
     46 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
     47 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
     48 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
     49 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
     50 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     51 0x95999c47c3e32a337ef108d657675c2757a606ed      1
     52 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
     53 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
     54 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
     55 0x9e640badecb7c628c6188b74488823e879f42a1a      1
     56 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
     57 0xa04f4a4b7306cb72f30828834db01699362a4989      1
     58 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
     59 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
     60 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
     61 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
     62 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
     63 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
     64 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
     65 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     66 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
     67 0xadd72e24a9e9117aa16d253cb421cb93b00240e3      1
     68 0xadebdeab678647a457743ea3af98f8b804e45c24      1
     69 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
     70 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
     71 0xb4627672ee52660a9e453ec541834e04583f3602      1
     72 0xb56ae8a727cf38f1f4716aeda6749d2af340d8f4      1
     73 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
     74 0xbc9ca5bd0f07700929f8d538233b0a9e60f4ddc5      1
     75 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
     76 0xc04208f289d3842ac168f2c373b3164e1d872650      1
     77 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
     78 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
     79 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
     80 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
     81 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
     82 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
     83 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
     84 0xd2f2a56affb67a9ae2499ab9df1689ee78fad89a      1
     85 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
     86 0xd5ec003289265705727b622f1700fe814e54ca67      1
     87 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
     88 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
     89 0xda34ee56031bacb68cc3ce57339c2a11c28d8eb3      1
     90 0xdc78107155918e230246439e4159fea4c477eae9      1
     91 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
     92 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
     93 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
     94 0xe22059d454c705f70ffe2a5706844a3a27a2bec8      1
     95 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
     96 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
     97 0xeecc9f94c674161e84ab0c60c020432ea8f88bc0      1
     98 0xf054274dd74987395d28136e53f39ef4f7b19994      1
     99 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    100 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_randomMemes2_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
      2 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      3 0x019d370ada720786cda2c4f78f0f4657af2ff92e      1
      4 0x0216ac50fdb6b46f6b74254b84ecdd1d431d670c      1
      5 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      6 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      7 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
      8 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
      9 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     10 0x0e2f031a1142ab3919685cf82aa764d9c5c0ea86      1
     11 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     12 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     13 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     14 0x197fe3ee16556577180f6578050802106e8bc446      1
     15 0x1c172d05b75178fc669d74407243cc932030f139      1
     16 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     17 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     18 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     19 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     20 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     21 0x2da903666829f302b0501f76144339213259c260      1
     22 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     23 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
     24 0x30f2a414945ba487f6a9ca909d0cc0919c6a1812      1
     25 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     26 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
     27 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     28 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     29 0x405e2c5ae7d4b570dc1c3358a0a5c293e9bcea47      1
     30 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     31 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
     32 0x455ce1afc1403b728789b4a8c5aa512600b668d8      1
     33 0x4581c619ae0556b774f71adab6831a86da1aef17      1
     34 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
     35 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
     36 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     37 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     38 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     39 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     40 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
     41 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     42 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
     43 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
     44 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     45 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     46 0x6b4fedc18a1e8cc0523e56cc9b45080e1ea00005      1
     47 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
     48 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
     49 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     50 0x76d01054ff91afc2d515f7fea9a3e3313e248615      1
     51 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
     52 0x843708d85621273f3bbc643b348da3a60d5b0334      1
     53 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
     54 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     55 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     56 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
     57 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
     58 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     59 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
     60 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
     61 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
     62 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
     63 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
     64 0x99f8f74b1808bd7f1e9e76b7d82151b35dfdd6ee      1
     65 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
     66 0xa8879c580a54f190ed53b43d30de269097ad7543      1
     67 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
     68 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
     69 0xafc093b1c8419f05d4de6ff54d38121c0d733752      1
     70 0xb2bc498a214282efa54877ecd082165d4cf86df4      1
     71 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
     72 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
     73 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
     74 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
     75 0xc72dd02e34129c57f0e68fb9e0df1fe185d71857      1
     76 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
     77 0xcbb49b18f2e9d002bd79fc1495fcb6f6f87f1e0a      1
     78 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
     79 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
     80 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
     81 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
     82 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
     83 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
     84 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
     85 0xdc819a832bfa55c933238a474954f3646da275ed      1
     86 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
     87 0xddc27a0a0c24d60b607dbc7b008f2016585e57bf      1
     88 0xe26027e219998c0acfbd00b74795dc850aee244a      1
     89 0xe4000d4f3f0e59ca00b803f54784fe0654a192f4      1
     90 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
     91 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
     92 0xea9f3a983d965e582c34eb852c18babac52050d8      1
     93 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
     94 0xee91d62eb5aaea933efbfd0790613af5db305006      1
     95 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
     96 0xf4dde6ea673fb810b6f503bf441908bb30b929f8      1
     97 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
     98 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
     99 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    100 0xf8be957f65e67fb0342992a51c30290d5758f880      1

## Allow Random Memes 3 Phase 1

``` r
c(allow_memesRandom3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_randomMemes3_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007fcf5590179336c826a60c80976623c7bbc29      1
      2 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
      3 0x074396d67d04c48fdbef50af95df216185b63cd5      1
      4 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
      5 0x0d69a096b2c66310309f0ead1d6c97c4dfe87086      1
      6 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
      7 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
      8 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
      9 0x1566ae673ae80725bcce901b486c336e6acef465      1
     10 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     11 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     12 0x190fb3b4118e5633820184fe9217049a88f07fcf      1
     13 0x1aa760cf97f1389e03a2c8b4ae7aa1d748d1b666      1
     14 0x1cb89e486db5774ba084f683796286848df489d0      1
     15 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     16 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     17 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     18 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     19 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     20 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
     21 0x386e0ad23a9f182d884ee2b8f61ec8c626b94385      1
     22 0x3b0b262b187001522c34edcafc378500133ab230      1
     23 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     24 0x3d6301815a5b5188e50a90e268179447d1c58b70      1
     25 0x3e5543a342446999ac11609af4beb82339ca41ae      1
     26 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     27 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     28 0x461b826d3c6108a420ac696631dff5a6a425e01e      1
     29 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     30 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     31 0x497452b2b3f5f5f2f4e82b36b67ccdc01964d404      1
     32 0x4a15e3d354043fdefbb8fa77a55da211f4058190      1
     33 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
     34 0x4e1829da30cd7ae2f3e9915cb8c9f3f203ac7d83      1
     35 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
     36 0x5659de064e163a7178b2d3b802ff13274d7d8e69      1
     37 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     38 0x58428a7fee1031744fa48f040b551d98b3924e5d      1
     39 0x59e5e7b519ba5314f095bcd988ec214d31e8162d      1
     40 0x5a3a1461310884df894b7e973305f690fc5779d0      1
     41 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
     42 0x5bff4631326de92282197696366df57198e38dd3      1
     43 0x5c5edb285b7451b2155ec13c5d2eaff2ec6779ca      1
     44 0x5d25087405105bab12624c73488ec186066a6376      1
     45 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
     46 0x5fb77d2d3b3dd881f5d4bc6da7a0f18fbd694463      1
     47 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
     48 0x668df49e79e6a828b27f455297291a8bd2fe0531      1
     49 0x6767b1e546dcb7499652a1fc4bd6f1e36992623b      1
     50 0x6a5ad95a3b0d6d4739de4370f51c8670a4d53700      1
     51 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     52 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
     53 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
     54 0x7deacee45d121a35d3c8db640ee7f9b437b4b2c8      1
     55 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
     56 0x848f8b6c67f4197a53ae3869d3200fac175a00af      1
     57 0x85914d049aa004d037cae65bad275ed4147f172e      1
     58 0x86125855ea134f1da31015663fd3c19a657a8d16      1
     59 0x877d45306e8c7506e9a20c9b7a79bdda97c4e7fe      1
     60 0x8e611b4f46a353aae8fa55af10b5cec24f5f1db8      1
     61 0x8f4b933491e1319799229ed9d36b179bb859d705      1
     62 0x93549cf8047b3e305fb10df7110da213e9429f9c      1
     63 0x958914bc3fc61629dcc5c11ce9d2e1dc254f3e57      1
     64 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
     65 0x99f70f9aa62bd71a7e0719afc2d6c67c6aaaadbc      1
     66 0x9fc80955aee9e3eb8c511a50a72b7e589700ffd6      1
     67 0xa03d04ff89308adf54badf18b46fee9980093524      1
     68 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
     69 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
     70 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
     71 0xae220d647426e368ac321efec83a9d696bf84e7a      1
     72 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
     73 0xb00eb5b7217fde1ffd59468fb7807eae9c22e743      1
     74 0xb01b3a335db49192a31af43a88c458e5070ca5e1      1
     75 0xb18e3c41faf4139b89b4ebf1f5ef645a3ad0ec7f      1
     76 0xb33fb83c645ac2a12b138636fe59604312092484      1
     77 0xb4cb9505397a55870234b11b622f0fe7cba064ce      1
     78 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
     79 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
     80 0xb97527632a95074f60148759035b21e4fd4c9a8e      1
     81 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
     82 0xca288359ba6319c7e7c7377a670183acb3781cda      1
     83 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
     84 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
     85 0xcfc9be33af04bb18f6e7bec4e1b1ffcd06900842      1
     86 0xd413f436a036b9773d7adccaac10242e27b9da74      1
     87 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
     88 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
     89 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
     90 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
     91 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
     92 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
     93 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
     94 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
     95 0xe7a8a504a82f0a829656261a54d0443f85e19604      1
     96 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
     97 0xf0c11838870ca444f5dc0ba0271a36bb9d9f81ec      1
     98 0xf3e6fbbd35e2ea84bdfdce9be96ffdb2b8bd1fc8      1
     99 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    100 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1

## Allow Random Memes 4 Phase 1

``` r
c(allow_memesRandom4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_randomMemes4_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00613f2dcccb7ac28fa1806fabb6d6c392417c23      1
      2 0x009f284bb658c55d292904ee733c50827dbb8e5a      1
      3 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      4 0x0262adbcaec7d20a384b1edae24a602adecb6e64      1
      5 0x02ef385f506b63bcd9ed1c9520924668a8f4e34c      1
      6 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      7 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
      8 0x0a327ca208535c9ca44cad4cb754cab9a9e1c0b8      1
      9 0x0ce6a432eaf0fa856a7c774170999db207748240      1
     10 0x0dc7fc49f79b310b3ea8919ea15429d35c565d11      1
     11 0x17de8591163a8a9b6752efb9d05cb7290e887a6c      1
     12 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     13 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     14 0x204a79ff4ecac8164c717bd46c02cd493a159789      1
     15 0x2249f5016b1b7a306c5e98765bf94fd0a70d8b45      1
     16 0x22a5456b6f82e14bed5bf182812085fe44621e62      1
     17 0x27d75bd3192bc64a93dde5220c6ae26ab23f79fd      1
     18 0x2edef9b9a483c206b3d966b318cdc6453f0dc2a5      1
     19 0x34b93462e65303f3857460971584fd0d908f2f45      1
     20 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     21 0x35a7378f37918969f7a733d46519de9bbdc6fb04      1
     22 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     23 0x3d7d85485b37360c7ad7ce17e549fba5473a0c0c      1
     24 0x3deed956b999b83361b85bff31d388c35125411d      1
     25 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     26 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     27 0x47836f027d427e78c06f24eabfdd1d2089fdfd5a      1
     28 0x478bb542f7658d635abba67edb987806dff5b83d      1
     29 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     30 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     31 0x4d207177677fb1128281a54fad993d3736c7f02a      1
     32 0x4d42d679cc113ff293f96e1b820209c661ee32e6      1
     33 0x567b71b80fe93afe1db3581e6faf5f00fd483885      1
     34 0x58f606e6a973e57165bb07057a75f047f42455a7      1
     35 0x59aa4d2140ec130a76cd69cb0974cec4dbd110a3      1
     36 0x5bbb102049e1205a2a31901a377c7252b194b294      1
     37 0x5dd47005f8a429b228a5d30b5345a1b64fa78c0c      1
     38 0x5fbd3561c19a53af07db9a376b08e62b75d1ad43      1
     39 0x6792ed1f7e5a22b9403bb389156c742d5f2d9d6f      1
     40 0x6ab0bfded6af31cb0a5987f08564ecfbee691757      1
     41 0x71175572caa47abfb535216decb155ed50567726      1
     42 0x7426b39865d11207b8f795b10d70843fc3289051      1
     43 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
     44 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     45 0x767a58ba5b0e404f5befbde4d7f00926df568fe4      1
     46 0x79b5259dc0fcb44f9751bf9f78f5ef46a7958460      1
     47 0x7c8a0a4bab02b7c6928952fc806776bad68b4340      1
     48 0x7f32973cfdef197e56cb5ddd2c9412505a629c92      1
     49 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
     50 0x81787ecf8d91c1c6827f28e68e0ac3979a1263fd      1
     51 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
     52 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
     53 0x86d09318844786f26df5663de29306e673e668d5      1
     54 0x8e22df65a6cebf6709e1ca7f44c544a1c2fb5bad      1
     55 0x9aaddc5040d2093dc0dafc0803cbfca822341bca      1
     56 0x9ac16a11b23b2fee4848cc2b16516d73d9a8f57a      1
     57 0x9acb83a514399c9f5201f55ddab61481d0430943      1
     58 0x9ef13e6bfa6d38065207b5aaf757d124c25fb294      1
     59 0xa3658a013c223fa60989f057a3d0a1153f88b0c2      1
     60 0xa5b992a3374ba87a44917cf9c2e20c296d52f7e9      1
     61 0xa610d3211a72ab1e1ecc6e6b19b3c357fe0b4289      1
     62 0xa67077ec8f947e8299335538167f625f3e407fff      1
     63 0xaa43b3ee536455939ac6155993351a9b34f72ced      1
     64 0xab6ca2017548a170699890214bfd66583a0c1754      1
     65 0xb49806cf8dcdb108d318d6f4f7ab087851445d94      1
     66 0xb7bd84b71fbe6f2ade449508b2b78cae45a18dc0      1
     67 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
     68 0xb948ea974cb95c9c73c51deac5d9e4464e769a44      1
     69 0xb97e95c65384bcab44973e10324bd49b7ab49510      1
     70 0xbe741931854a817a6510231c13b80cd462fa9c0b      1
     71 0xbed504b31411ca3eee66fa3b55088a6d5b6f8174      1
     72 0xc2543fe877e3cef40ae489d247dc605ffac4399b      1
     73 0xc2fa34a6fcb085f5e0295f233fae7fc90fbafe85      1
     74 0xc3c393367bca317d3aa37132e72ab2bd94957df3      1
     75 0xc449f005667bef849261b35accf931a4bace48fb      1
     76 0xc93d22d9f4049447f94a0ae8bd25d2c2abee66ef      1
     77 0xca2db534a407cb75ac2bb333083b8e78b4f6f8fe      1
     78 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
     79 0xcb748f312b8e0557587862225697aae325052f7d      1
     80 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
     81 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
     82 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
     83 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
     84 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
     85 0xdd2d31348acdef864309a9c535160e7fe914bb7d      1
     86 0xdf1cf9e3debfefd56286ef4024646f49cc540e37      1
     87 0xe0753cfcabb86c2828b79a3ddd4faf6af0db0eb4      1
     88 0xe28bc349f666a4281bbfed1e485e8dfad90bb3d2      1
     89 0xe373d8e1cdafd885029faefa4b0d66813d353353      1
     90 0xe5110e73cc510cb6aec9a71eef74d27bf4f9bb38      1
     91 0xe51748456ea9759c19d232a32f96ba3c1c110776      1
     92 0xe77d44e642c53db943aa0a71ef60cbff719e644e      1
     93 0xe79c263403b0071093bd7968ad1b6ea94d3bcf1c      1
     94 0xe8170bc2483571d06aadf73ed5f90e0719c6afa4      1
     95 0xedc1040e47f75ff85867ff4358c6229801af31f8      1
     96 0xf45796cd502ea437190a96696d22be253e2e2cb1      1
     97 0xf5321850a731d785f500901ec5ca494f1876c262      1
     98 0xf5559d32946212a8f718c8d6c79c19c6200c9d6e      1
     99 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    100 0xfe4da73ff6543b17b2b18e0e5d94bc87bd79f527      1

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
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
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
