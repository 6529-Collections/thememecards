
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12149       Length:12149       Min.   :1   Length:12149      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :2                     
         name          
     Length:12149      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17832869 # https://etherscan.io/block/17832869
block_hash <- "0x2e45a9c808911764aaef378cc07c7396d1921d51112cd9c8dcbb782a1eb3b966"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4610 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SuperRare","Foundation","Foundation2","KnownOrigin","PunkontheRun","DominikGCollection","Astashofpresents","SANCTUARY","ALCHEMIE","ALCHEMIEED","Rarible","BastienxDominikG","CuriousCabins","MasterBrewsNFT","Kolectiv","NGBONEYARDS","NGBONEYARDS2"),address_remove=address_remove,address_max=1)
allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw                <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles            <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 142 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
      3 0x01e6b50557ef1a2ca58a621998ace3f278e808b8      1
      4 0x04ecb5a1565aab40632698244a92119ca56367a4      1
      5 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
      6 0x05d2e466b841fe014e1ce2d1f826707b15f8914d      1
      7 0x07cd1e459b19e78782113136be8048649026800a      1
      8 0x086d21b3125e01338ed4757b9ce8b7b44130e07d      1
      9 0x0ae02e7723ff745bc759ecfc0726633e09f42c9b      1
     10 0x0e25ccf5ab9315e99843153a4b3e8dab496ab762      1
     11 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     12 0x100218b74f49f091c3308a20da71518ff9549232      1
     13 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     14 0x151e86b0f89ed7940231b6f198e1656664b8cfe9      1
     15 0x15e0c6551eb54f473ed81e8e057d5fe1863e9e03      1
     16 0x16c1bb500bd49fe4460c54edd0050b24602a5c99      1
     17 0x17e566d94b9e9471eaaa1fd48fed92666fe0e6c0      1
     18 0x189cd6f032fd6e90c064d38b58ed106157902b1d      1
     19 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     20 0x1a4a731feeb16bef1a29dcc06583294d039f4913      1
     21 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     22 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     23 0x1c836beaaa88cc00b15d515eff70a0dd803abb91      1
     24 0x1f87338e6689159fbbce349230b4087cdd18e838      1
     25 0x2800d55c7d96206a3e51977b418a8aa555708726      1
     26 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     27 0x2999377cd7a7b5fc9fd61db33610c891602ce037      1
     28 0x2c41d74ce31a0b30a040163ec5e076750948a199      1
     29 0x2ce86016f2e87f21dc990f88520afa8e68dee366      1
     30 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     31 0x306b43bb90555c40bf16865b00ef2968f05c411d      1
     32 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     33 0x33e625fd43d4a536c18b3c78232e40bef365e6f0      1
     34 0x3649d9c86deb6758b7cf761392c06ab827d4ce03      1
     35 0x3818e586fdb26138ec7474a704f8cbb94a36b6fc      1
     36 0x3ba2b77cfcb417a116a2f6f1806a9a04322410a0      1
     37 0x3cca02f925ee47a0333d3e40b98956479b580d89      1
     38 0x3d79e7dc7fb949f60c1660bab6a100aa6ee1311a      1
     39 0x3ed9707718a0a41464c905aed5fbc5a067236c0e      1
     40 0x43247c7db36054ea50c64bb1d4bafb55703552eb      1
     41 0x4371e8d8d75e839bcca1f19cff4bf1c8dd374cbb      1
     42 0x4a77d5aae67ac24302b3e14c1ced945950def38c      1
     43 0x4fc31591586075be5de33f223bfff060283babd4      1
     44 0x5018d05f2062590b74e6d59fef9b6dd9c357db96      1
     45 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
     46 0x564a544b7fbb31c976b353dff3ec4015e0871619      1
     47 0x5a667f33a24bdcb0dee1bacf61d828cdc51e496e      1
     48 0x5e3e5ab72f8b5994f5c5b3af86e5c650df232991      1
     49 0x5f73b065c8412b43e3db33cad0efc5038040a597      1
     50 0x5fd403fc29ddb91842d3c3f42e4dd1be2a1f5e37      1
     51 0x5fdcca53617f4d2b9134b29090c87d01058e27e9      1
     52 0x60acf8d95fd365122e56f414b2c13d9dc7742ad7      1
     53 0x61cb99126da77c109d5788893f379c3c4a5bd704      1
     54 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     55 0x649f2e682329a8cd3f80c2a0023963dfef5bb925      1
     56 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     57 0x6663af183cbdc2263ad236181692eed8d002efd3      1
     58 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     59 0x688bc734e0f452dd46c6b36f23959ea25f683177      1
     60 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
     61 0x6acbd57b033ff316c4c93502d39e9469698e6b73      1
     62 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     63 0x6d256bed8a2dffff8eb3a687c9e4001fe33c4a32      1
     64 0x6f59480730fd205328a3e81e2f17f33dbb6290e2      1
     65 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     66 0x72cc82900823c29310985decdd607a68f9bc8418      1
     67 0x74d5a2dac81f729791d8590c77e4c9e45fb38588      1
     68 0x7583534d2f2c3699b1acd11b9f2fac9c35acc45c      1
     69 0x75cd5c5f37e05797a61cffefb3d3f334f2d7fc1b      1
     70 0x75f025606496bf75ddfe94bc912c42b48b0241c0      1
     71 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
     72 0x7a3010b00d9866c80cadafeca573490e432ba3ac      1
     73 0x7a3359215183b6ac74c528f9ba3e540718b44043      1
     74 0x7e4ee449aff87b237bae6acb83fadf795bdba4c6      1
     75 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     76 0x8054e8f8902287708a63856597382820cfe83167      1
     77 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     78 0x8699b998a5dd490a7bc25f4e61f6e6206acd0894      1
     79 0x876d4aa80fd4af365b03df32df12c7ccdb146301      1
     80 0x877b37d3e5467b4aae7687dd3480a46c8d3e16be      1
     81 0x900e7ad1ab18cebb4c21f71795364e9b636831ca      1
     82 0x919f02bd8ef0eb66ebc5c07fbcee449b14b0f000      1
     83 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
     84 0x93bcef827efce86f36483e879c15df577ac4ce7c      1
     85 0x94aff1b7568c63d38c8f07981fba16d83b2b22ba      1
     86 0x9649e370ee6facc62e1849eab6f4be7a2b5f4a13      1
     87 0x98f1bda2d45ffaca73fcf2154e9e007871e14934      1
     88 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
     89 0x9ac820dace4e2f60a534a49410517a3b5f96ef9e      1
     90 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
     91 0xa6ad96f63e1ed44d7f182f60a85aaef37595b971      1
     92 0xac2fb3f2988249a6b4a2c3ee268d8bc9c9655b9d      1
     93 0xae02854338fe5892d4d7489bc1465f99a20951c2      1
     94 0xb0e984d4bf1830dcb6c8c4e0f81d0869d2f037b1      1
     95 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
     96 0xbb6a783707f9fff6eb5d97d7053ef06151f0b125      1
     97 0xbf51802db173061762cf93e39218f526980cd718      1
     98 0xc2e125e1e229541da36814d35820c335e8dd7b06      1
     99 0xc302270eca1221b33281a4b9f6689de8eeab97f9      1
    100 0xc4daa2ad524550bf26b3edd5b1f366ddcb00ffeb      1
    101 0xc59d13dd703f16f4ef7cd8eddb19fba768817b30      1
    102 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    103 0xce450592a164e47e2824eba2b25ad23a47564639      1
    104 0xcf0df56e67f469f390e5c458c9debd875b6a5eb3      1
    105 0xcfdd5f95287303f755bc6966996ca3c73dfb0f34      1
    106 0xd2dbe21025dede10517a8be905deb0af0bf2cfbc      1
    107 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    108 0xd4849bec09de2ab271ef92df0b6e7faaabf07adf      1
    109 0xd8fef4bb781ad488b124b86b0bc7701454060160      1
    110 0xdd0c94c21b9e1c266a01bc1eb9d2619100fe1909      1
    111 0xde76b5e354e9a9026502ea2d37e4d6b45d28be00      1
    112 0xdef769bcf57df5a2400ab5f9dd3aad5981079689      1
    113 0xe1df5b8c0ca6c53bb0eaf67e5853baa64f3a2d02      1
    114 0xe2123387af7c45c5b980f91a690222e9d7856bb4      1
    115 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    116 0xe457ed545018721692ff6168d7b623ad6f18ebcc      1
    117 0xe48c612139749a96a8f9929f70258bc6b796ddc4      1
    118 0xe51ff9ef0cb02a8715729de3da80e0f438033b14      1
    119 0xe621d85919f808b57c5a0426af2f7374ead55a89      1
    120 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    121 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    122 0xe890380af2b1fe0aab440e6ea07e9aad5048cf62      1
    123 0xecf6b95c45bc8558a3a61e9c1cba4d369a6f63b3      1
    124 0xf03ef3e139a037363a9a8e623999a69276449039      1
    125 0xf110b384ca4f89b1377f16d10d47866487b63056      1
    126 0xf113618fe77faad8867287d2dc121deb1bd33312      1
    127 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    128 0xf214620443981a3606e9a26636a3dc241424760b      1
    129 0xf2439241881964006369c0e2377d45f3740f48a0      1
    130 0xf37ccfa6b94c291382a51be4e9f275741d89376c      1
    131 0xf56c99cf2ef73dc57f3c871de578e9a642558318      1
    132 0xf655aea8b9986e1b5d3d1b3d6f17c77eb759510d      1
    133 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    134 0xf875bcce56edc4c06bea8a57a1fdd599c584299f      1
    135 0xf95f8d46c678f138406b15a795e959365d94a1eb      1
    136 0xfaaf5d2292f5e351acbcada98e92338d41a7cb4b      1
    137 0xfcca02e2dbd1b08a48718db14d35a1f6d16075c5      1
    138 0xfd74ffe6eb9d1b11c0efdec077492630abe0a100      1
    139 0xfe457ae893ce8d464d7f1aad9a558493d0aa9ea5      1
    140 0xfec4213278555d692679c918423816fa99bb74a8      1
    141 0xfee1f4596b40b13a8b8723478ff87bd7c62b5980      1
    142 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1

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

## Allow Random Memes Phase 1

``` r
c(allow_memesRandom_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      4 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
      5 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
      6 0x05eec49e27704acf5c5ce31ecfcde4e382d78ba0      1
      7 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
      8 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
      9 0x0a49fbe88cfc413181dbe93c9e3b6184b2264071      1
     10 0x0b9f898921b2bb8cd2d7d30faffec2c90200cc8c      1
     11 0x0c664c03eebcecb6c21e3b3bc77c9dffed5bd694      1
     12 0x0cbe1fba05102c34365a742af159efc5a93d1a68      1
     13 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     14 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
     15 0x0ea9f4f7c371e7044d187b3da7850ccaa6370a0c      1
     16 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     17 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     18 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     19 0x1566ae673ae80725bcce901b486c336e6acef465      1
     20 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     21 0x15d7972819b3906fc430b1e7bc26c39e4b9e023a      1
     22 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     23 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     24 0x185d5f60fb7c59d344ba939361908916cbffe1dd      1
     25 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
     26 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     27 0x1d522bd33cdd1f60be71b0b7b2efe4e9f20a5263      1
     28 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     29 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     30 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     31 0x208b4a8ef875a5e4645e23f27343f47fd887d9c5      1
     32 0x231595e3673a10e846803194f4982e1cf3389161      1
     33 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     34 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     35 0x25acc72796bdf4579755708fdbc8409622d224f7      1
     36 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
     37 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     38 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     39 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     40 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     41 0x2d646486397bbdd894a9bb62d91fc5bdcb8d9f45      1
     42 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     43 0x307fa253ab864cbf57483415909b37c36df3b8c8      1
     44 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
     45 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     46 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
     47 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
     48 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     49 0x34dea787524a930670862a27bc7234a1897e1bf2      1
     50 0x35bb964878d7b6ddfa69cf0b97ee63fa3c9d9b49      1
     51 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     52 0x36ff6a01782501553486a4efe6ea6e07f8f3ae28      1
     53 0x387f802c6a9ecffc8b8f6bb7b80ea0db4cef9810      1
     54 0x38cb81cea002c1a7659762e57b2878a5b93969f6      1
     55 0x38d779b6dc61acdf864cd289f3594ad05088df95      1
     56 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     57 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
     58 0x3a30fa661820bf261b39f41a63916cad97b20e60      1
     59 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     60 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     61 0x3b0b262b187001522c34edcafc378500133ab230      1
     62 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     63 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     64 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
     65 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     66 0x4581c619ae0556b774f71adab6831a86da1aef17      1
     67 0x462fcc4b3fc64e0dfd751424bc3b070d79efb300      1
     68 0x46abfa031be839b1599513887a27a403e8d6598d      1
     69 0x46e6aa05e0867d5f0feb749e81e005f5567ab317      1
     70 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     71 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
     72 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     73 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     74 0x48f1b764b8cd851025b88fa28f92cb855b9079c0      1
     75 0x4b2c1ce6a01981dc3ee79825bdc3f3cd7932bf11      1
     76 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     77 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     78 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     79 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
     80 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
     81 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
     82 0x4fed7d0eb3bf1bf1ba69320962c81b132ad4474f      1
     83 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
     84 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     85 0x51409bee46664b9044731824578b9d3213bfe647      1
     86 0x52f051280e850f32cbfafd8d78c31edec4c3248c      1
     87 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     88 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     89 0x55d4a9e5cde6fbe40991e7570e01899891f6b0d2      1
     90 0x56517e41ef464db3753ecfdd2dbcdd2f045b573c      1
     91 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
     92 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     93 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     94 0x593cadf136c68c720d446461c1bfee600647c6b8      1
     95 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     96 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     97 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     98 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
     99 0x5d25087405105bab12624c73488ec186066a6376      1
    100 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
    101 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
    102 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    103 0x60e5299f49cdbc13d152323105af462071b22c87      1
    104 0x61e814fe997f0b2816fb9ac3c7df3aaa38d8ebb6      1
    105 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    106 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    107 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
    108 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    109 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    110 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    111 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
    112 0x6767b1e546dcb7499652a1fc4bd6f1e36992623b      1
    113 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    114 0x6a278927f03422419901b77be4d47700b1f3599c      1
    115 0x6a3d9412a705f5531b029ef4a7b8040e1eb84ad3      1
    116 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    117 0x6b9bcb0b854d7c7430baa55167d367d28a8dbf1b      1
    118 0x6d25c0fa070e625dac76b29bcf0917303cd52e7b      1
    119 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    120 0x6d8e6b20d4c132276b0855179214bd2606c9c6bd      1
    121 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    122 0x714b194ba18daf0288848054bec788de08c00cea      1
    123 0x71784687d4c74338bf284bea22956c74fbe6d631      1
    124 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    125 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    126 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
    127 0x72f52de160ece454a2d75a410f85f4978045882d      1
    128 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    129 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    130 0x74e225269effacee134aadb51aa135ad066f55b8      1
    131 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    132 0x76d01054ff91afc2d515f7fea9a3e3313e248615      1
    133 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
    134 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    135 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    136 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
    137 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    138 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    139 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    140 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
    141 0x7c8f072015d4e29c24088fe55e62381406bd71ec      1
    142 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
    143 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    144 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    145 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
    146 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    147 0x7fc3dd768031af344ebd6cc27c449fa57a582917      1
    148 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    149 0x8169cecd991392586828011079f29d05b6a9edcf      1
    150 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    151 0x848f8b6c67f4197a53ae3869d3200fac175a00af      1
    152 0x853c69418605529a68907aaf7789270e3cf69d97      1
    153 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    154 0x85914d049aa004d037cae65bad275ed4147f172e      1
    155 0x860f0aa48ec759df1034d72e0311482a8b01db83      1
    156 0x86286d93d79a9e5b78de97cbbc8dcaba0f2489fc      1
    157 0x8699793e22403b355185d8ff76e7392f98aafa46      1
    158 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
    159 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    160 0x879a8a1868d620584afa0c9c8807628d553c6bbe      1
    161 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
    162 0x89fad20f29de44b28c66abcd47113227e8308479      1
    163 0x8ccb07293755004942f4451aeba897db44631061      1
    164 0x8d6cfe2ff08d8b6766eaef33990b78f8990b4520      1
    165 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    166 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    167 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    168 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    169 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    170 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    171 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    172 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    173 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    174 0x99980dd0184773ea492c23033e1309609a7e75fe      1
    175 0x9a659894e5d115846767db0e1685744c452e7a6e      1
    176 0x9acb83a514399c9f5201f55ddab61481d0430943      1
    177 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    178 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    179 0x9f281c5b04c091096ac468a9388f0ee6b0b8b1f5      1
    180 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    181 0xa03d04ff89308adf54badf18b46fee9980093524      1
    182 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
    183 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
    184 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    185 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    186 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    187 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    188 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    189 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    190 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
    191 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    192 0xa8e54b46ae93e14eedae486a9efcd4c7b5a5be20      1
    193 0xaad4210b800f14660ef4068029d428936ebd21fd      1
    194 0xaae8c50d76c76fb8947c9a203103d28b55862977      1
    195 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    196 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    197 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    198 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    199 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    200 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    201 0xb209e4bd577cecb120fcd1797ee399ddd6533ac5      1
    202 0xb258e243a526e79c4b5c8dd5df490e42eb7927b3      1
    203 0xb2bc498a214282efa54877ecd082165d4cf86df4      1
    204 0xb3eb9bc116fcf22c3d6d5f920855d4bf34a9b0ba      1
    205 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
    206 0xb774f65d9eab67c9a587eadb5b79d0a02bfa5c42      1
    207 0xb848a0b0ba24a233e2f05d0a382e1c2035bfc5f0      1
    208 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    209 0xb90aa714bd30e6f135ec15a0cd2605af1590d184      1
    210 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    211 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    212 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    213 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
    214 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    215 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    216 0xbf99363007b348b7a019e70cce02e8ba62e95129      1
    217 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
    218 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    219 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    220 0xc14e891f453b14df5449a6644bc88dab2a5e5622      1
    221 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    222 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
    223 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    224 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    225 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    226 0xc5ed0799c911bf8147680756825725eb038451c8      1
    227 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
    228 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    229 0xc89503a4e57619a2ac87a821732ff4afdb2bced2      1
    230 0xc8f8e2f59dd95ff67c3d39109eca2e2a017d4c8a      1
    231 0xc97a5623578a832354988e7e40869f5207193d53      1
    232 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    233 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    234 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    235 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    236 0xcda2efb1de50aa4476b4a20c36bfffdf97b5ae80      1
    237 0xce56a746edaf84e65aa6a2bbb45b2328f6a99935      1
    238 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    239 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    240 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    241 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    242 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    243 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
    244 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    245 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    246 0xd530282d853169a23822762af0dcab045d6295d3      1
    247 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    248 0xd70150a7a5a42d4cd25974dae077a79a1547fcf2      1
    249 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    250 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    251 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    252 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    253 0xdafa9e3dae493f0b9d6872eff4fda0f40d1b7488      1
    254 0xdc0d28f3d09b292d41b836db991b157d641ad64f      1
    255 0xdc36237208adb46959e77a0052843ce5446afab4      1
    256 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    257 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    258 0xe0e263fdda4dd2432343b1ea2e49086cc3f22745      1
    259 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    260 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    261 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    262 0xe4d3cbd20bd9ab79a70a1612853154cb80b02961      1
    263 0xe5cd0fc813d020644c496dd964a32eb9ac17e50d      1
    264 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    265 0xe64777dcdca11fd8bde96ed05e7560ae789504b6      1
    266 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
    267 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
    268 0xe831977b52714501b52bada9034021a7cac79709      1
    269 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    270 0xeb0bd6bfa6d109205f857caab59f651fe7631094      1
    271 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    272 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
    273 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    274 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    275 0xeee4ebaa421ed7aa32b925bbe2fe7506e33e7cd4      1
    276 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    277 0xf3e6fbbd35e2ea84bdfdce9be96ffdb2b8bd1fc8      1
    278 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
    279 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    280 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    281 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    282 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    283 0xf5d1654521931b52a325d275f3cafa2585a20b2a      1
    284 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    285 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    286 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    287 0xf868a2de45bc38844b16c9e63fda5e1de1d54a22      1
    288 0xfa3dabf38f872f50c62c3d39920d173d215806ec      1
    289 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    290 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    291 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    292 0xfc2ca2de8b849f7344ffd138f10d618597834a09      1
    293 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    294 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1
    295 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    296 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    297 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1
    298 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1
    299 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    300 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

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
