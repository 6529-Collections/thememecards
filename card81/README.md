
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:1034        Length:1034        Min.   :1   Length:1034       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:1034       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16860969 # https://etherscan.io/block/16860969
block_hash <- "0x738b491afbc6fae305b8174fd7e752e1fa6042acd5984c9f8b543ace4ba1d547"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4802 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)



allow_cachou            <- pick(snapshot, contracts=c("SuperRare","Foundation","CACHOU","CACHOUindustry","CACHOUDEVIL","TheShackles","TheShacklesEditions","CACHOUTIANLONG","CACHOUTIANLONGEditions","LOSTCollectors","Dreamer21","Ling","WastedWild","EShell"), address_remove=address_remove,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memes_phase1      <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=150,address_max=1)


allow_memes_phase2      <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_subtract=allow_memes_phase1,address_pick=200,address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Singles Phase 1

``` r
c(allow_cachou) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 137 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      3 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
      4 0x095801e80db80722b2731251b8a3ecf8bd9334df      1
      5 0x09c4eea8ad5e12e1979a9edda897c4cac13baed9      1
      6 0x0b312c51b6ee233e24fb10b9978aa377466aae2f      1
      7 0x0b4e8e4d3b3d72d1fa798b74f09cae5c71c16c83      1
      8 0x0bcccf68fafe41b5494d7fe81ab018a2a2c06843      1
      9 0x0d6ce4f0e1fa53af3f624e17da5f47791dcaf70c      1
     10 0x0f045a0d77d24c326316e0315354e7df28b4ac50      1
     11 0x0f2911f70ac60e6cc90a647d7c0da7e778c997d5      1
     12 0x0f3ed7a3519dc3b3f4a80d0922c300875a527a60      1
     13 0x12a0e25e62c1dbd32e505446062b26aecb65f028      1
     14 0x15412f8724f267ff0f32331412897180c5aeaf62      1
     15 0x17cbd516166720b8c75fe75235ed0218d9cfdfbe      1
     16 0x18851a9b795365ad17359ca365aba188f8ab523f      1
     17 0x1b1feec29f0b3f831d1bacf4117b0e7c80218622      1
     18 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     19 0x1e2abc87514bcdd99731aa3a5b34b02a13d1b7d9      1
     20 0x212d0945fabe82a312f0ea6c61a50b4d66897029      1
     21 0x25665e8b95792c23d6167d2d0903fdc6bfef87e9      1
     22 0x2d89cea61de8020c97dd0e6355a97946388050ff      1
     23 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     24 0x2e6435daceaa1e7e14e0715a5b8648abdebae061      1
     25 0x30fee618f0f92ee681c55202ae811c7ce9c0d70e      1
     26 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
     27 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     28 0x3426258a59d7fe51c3f22fb631ea8e0251a9744e      1
     29 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     30 0x37ba996dd7eb738af38af7839a17ad19cad5f155      1
     31 0x37feeac37afa67d5211937b4fca166f91724ae80      1
     32 0x39e55841bb66e376f224caf7e5668117e58a2d3c      1
     33 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     34 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     35 0x41b062468bb41372d512e8b8fb69662326f5c93c      1
     36 0x427b543dfbf2e14f5d7a22a6c619d4bded7ce279      1
     37 0x49f9c2be1aa71afac6f2f2ce5b884d8cda10e26d      1
     38 0x4bc3d7deb34289ed6247671676b8ab33a682d2dd      1
     39 0x518d0f7a7432fe475ca36f3eb6a4a7f408a8bd61      1
     40 0x52ddc2257730ae442b139b245b60f01cc2064682      1
     41 0x5399a09fc5d0054fe1d0c15b2dd135fcf7cc3ade      1
     42 0x53fbd91479117e9f54b91a06ae91f0ce6f2046de      1
     43 0x54f2a20e81f826de41a3eba33c5a1a839b0cd5eb      1
     44 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
     45 0x5cce801daa68740e725a0d6851511a1c8ea360e9      1
     46 0x5d2bf5b4b03ed7726627f8829fa70ff339ea6c7d      1
     47 0x61cb99126da77c109d5788893f379c3c4a5bd704      1
     48 0x66096e2dae9d79df90eadd26b6e1e4b58f68b2ee      1
     49 0x6649e95a9070f2e2e5cd9efeb885eb7a24768ebc      1
     50 0x6b534bbcf64c4814fa652cc55fa7cc754cdbb723      1
     51 0x6ff2ed25cda2f6e5fa1fc0da5839ec554f6590fa      1
     52 0x7077fe5a6017a5ed98a0b8b048bd7e523633d6b6      1
     53 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     54 0x73b03def245f5c8ab4f58a40df4ae28ace64b1c4      1
     55 0x746b46d87543eaf779144c8870888119d6ae0c2a      1
     56 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
     57 0x773d715200ab5c07f39db9772e3c83c48534a585      1
     58 0x7838950fc3a25234c03a0e63b2aaca978ab1a602      1
     59 0x798f73c7df3932f5c429e618c03828627e51ed63      1
     60 0x7afe1e49e6a44b2f1374a20eee4406a8fa691519      1
     61 0x7b48904cac96a301cfa71ef937013184d78636c4      1
     62 0x7fed61bf417ccb1a51272b85a32265d80bf64aa2      1
     63 0x8300f444462e55d11dba5823a1584badfc293fd9      1
     64 0x845a23ff9f59f8f788f9b94181e5326fcb8c9f6a      1
     65 0x85f845b718fd3a4454a982b78acb080739146dc3      1
     66 0x87ab523c26d13a1d193cc18ea6819fc2a3dc917f      1
     67 0x87e32d4631d728d6ad7ebcd133b11febc9da9b93      1
     68 0x89119dac068cfc98bf2ffb7d15948e0901d997dc      1
     69 0x892857d3a0e6ca504da54470a0d71593525ebc22      1
     70 0x8a3db9109f2ef0d234b6ca0b7a08f9bc63efd9a2      1
     71 0x8ac2e10d2bf056094e48e4800de5393aeaffb4b4      1
     72 0x8ace19df5ced984325370e960534f025ff2da891      1
     73 0x8b501a6090fd3c6b905b1a0099e2e216a5a2d40d      1
     74 0x8b7b30e0e9af7415b7b7361a178f8f422fc20e3f      1
     75 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
     76 0x8e59cb2f964925bccaadcc87e2189936c3dc6dab      1
     77 0x8eb82be5fc2e64e0b57ceb639df68610b29864e6      1
     78 0x90326fb170594cc50aa3c4578c19554f9174e924      1
     79 0x92587b407e48b4b9db9f581d28509c1d73536722      1
     80 0x92e9b91aa2171694d740e7066f787739ca1af9de      1
     81 0x9430120244b1c4d99d9d2dd3cc6b21c4ed27b768      1
     82 0x95b239149db2bb2679825a503de54884f08b6249      1
     83 0x97064190190d4e2fb284a2d55dd53504e291c4cd      1
     84 0x9769334fc882775f4951865aa473481880669d47      1
     85 0x9977518a2f7a48a55bb08c3c7f9e9d3e3aea92e2      1
     86 0x9b8b0eb6d23b9f656385680062728419126c6974      1
     87 0x9be3ebd8246dcf239184b6594a244a5522fcbd92      1
     88 0x9d8c0c8fc820c52bc1739a29e2fc15736d5dacb4      1
     89 0xa337d966e8a1fb59846e620a35d54f071f6fb3b4      1
     90 0xa3917c68d49dfe0323321b4a7f2ec88074647dd0      1
     91 0xa3de15c0c1c98dd089e7d1184488da98753e39ce      1
     92 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
     93 0xa629b23bf24e4dbe1dfc912cf7e67c3c2921fefc      1
     94 0xa8b1641a6c0fff08b95062cae59efb20b56045ed      1
     95 0xad31ad089f44bf44792decddf37c14650eb0d7b5      1
     96 0xb16204b3732a8713d4f14292ee37e64c659128a2      1
     97 0xb16f04f418711c8a52d281d1f9989cf930ffd4c3      1
     98 0xb5e1fc4af4dd6ab3282d16499420954b192e1849      1
     99 0xb623c629a0c21382d1664f47fdf31ff701db848e      1
    100 0xb7a2401d9f546c62ed813eb14c92a830c56f9fc7      1
    101 0xb86fab301780126985b0848cdcb1b686806c3e6c      1
    102 0xbb3641572cbefc4c62e54fdbc891a453a1cea76c      1
    103 0xbb398bfea1f80ce4df8b15c44f40d0caf0707849      1
    104 0xbdd4315d434f07a5a5b426249de85569ccd4d693      1
    105 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    106 0xc302a0b32ac8f249e591f59191bed10146c86612      1
    107 0xc4e2784896ff5196d7d98db5f332dbbd02786357      1
    108 0xc4e7423f38b0d7408b77751df07dc3a49d1b880b      1
    109 0xce3ba669ae1374e5cca031aa27128e27f58a94a7      1
    110 0xd1482b5368ddae4f2dcb19c1ea6f9a20f9230259      1
    111 0xd1e1e5dbc165c6550f935e6f08ed77ca97e351be      1
    112 0xd2a943860fb1165e68c412e7b43f0ad29cdccfd6      1
    113 0xd478bc416e91bad691af181d7d88a5bf2f5915ee      1
    114 0xd74f728f4dc133d5eeecee9ce1cf693c9db2be21      1
    115 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    116 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    117 0xdf2f491c6b2930a0f1435db9602312d40728d246      1
    118 0xe178a727abe28f69a26988e39d1157afcdfe5c4a      1
    119 0xe1c1c3867181cdb15f5c011120b4b7abcce91c5c      1
    120 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    121 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    122 0xe4a3b8f7a222c2c8093470746a6cec2cfb71307b      1
    123 0xe5f7874c94f5e2f13093054d3deff37a8730834f      1
    124 0xe8c8271baa2d476fecd5ae2727ece63d2ae661df      1
    125 0xeaf8cf26b813c421c000f76f14a80fd9976c4aa2      1
    126 0xed826934a3bfcf00630b60c6f16bd1c97b0ddebf      1
    127 0xee0046b3b5ab5f4495b13496652bd83779b64b5e      1
    128 0xf146f3bf137683c6d2fe266f6a2dc352a615eb0c      1
    129 0xf2439241881964006369c0e2377d45f3740f48a0      1
    130 0xf3fb78ff3534fc77ef2a1fb9e2a26bea77a615d2      1
    131 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    132 0xf7a290e382956aeec002628b7959e39327641874      1
    133 0xf875bcce56edc4c06bea8a57a1fdd599c584299f      1
    134 0xf9c97b1b9f29f86f724431d625b34af89e3f21ff      1
    135 0xfac6d61834b277275b9a50630c9e9f64e0ae4c67      1
    136 0xff53b23729401a257da6b64f4e769a73759be6ca      1
    137 0xffa83732cce3376d1ce9a06c307cfd8fb596533f      1

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
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    10 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    11 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    12 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    13 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    14 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    15 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    16 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    17 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    31 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    32 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    33 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    34 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    35 0x69e68074f1aada957edd39c5eae0069973343f30      1
    36 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    37 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    38 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    39 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    40 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    41 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    42 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    43 0x82139687faae8a29851902783e02e699de0e0846      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    46 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    47 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    48 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    49 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
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
    68 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
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

## Allow Memes Phase 1

``` r
c(allow_memes_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_150memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x018305f64f71290f16ef245dd063a7470fde44ba      1
      2 0x0573383fcc26eb066010d28d96ab02f7c5cde67b      1
      3 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
      4 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
      5 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
      6 0x0e2f031a1142ab3919685cf82aa764d9c5c0ea86      1
      7 0x0f9c4213c040a39db2ba6f833472f739e61710b4      1
      8 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
      9 0x1187ec27eb94e65717af65f3ae8c326bd8bb47c1      1
     10 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     11 0x16b5ec9e34bd0a916822d6785b1c600ac911d6dd      1
     12 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     13 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     14 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     15 0x1be1eac561b34c9b5e45eba9c3fb51fc5fc2796e      1
     16 0x1d522bd33cdd1f60be71b0b7b2efe4e9f20a5263      1
     17 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     18 0x20aa168e6c793646f60737399c8466dd643d4281      1
     19 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
     20 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
     21 0x27bb5366ef655819337d6ffd29a55905608c853b      1
     22 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     23 0x29c02a5e462e187ec7e1c7874d012633f12c89d0      1
     24 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
     25 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
     26 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     27 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     28 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     29 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     30 0x2da903666829f302b0501f76144339213259c260      1
     31 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     32 0x30b33f902981b5e567d3bb1a3cc847b35f626609      1
     33 0x32dd9f6885332ccf8633a18cb0c2e25e68fe03d1      1
     34 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
     35 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     36 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     37 0x36eebc48da111b14b010a97abecf8bb68d10095c      1
     38 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     39 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
     40 0x3b0b262b187001522c34edcafc378500133ab230      1
     41 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
     42 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     43 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     44 0x3df271e2ac2ac5e1be34fe135baca471cdcdb575      1
     45 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     46 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
     47 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     48 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     49 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
     50 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
     51 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
     52 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     53 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     54 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
     55 0x593cadf136c68c720d446461c1bfee600647c6b8      1
     56 0x5a3a1461310884df894b7e973305f690fc5779d0      1
     57 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     58 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
     59 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
     60 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
     61 0x62fc78469063720d0ea442baf2984c20b50f0c05      1
     62 0x632734882ed0127fbdf2666478df42aa916bdc84      1
     63 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
     64 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     65 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
     66 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
     67 0x660105ea6435e8d4c887a3c830b8812746eada30      1
     68 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     69 0x66fa06bd24294ab3bf123153e49b9065cb7672e7      1
     70 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
     71 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     72 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
     73 0x6d35392121a6687fe19d6159601b17517f73d0ea      1
     74 0x6dd97134b53f40d239842716b2cf0179a11876e9      1
     75 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     76 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
     77 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
     78 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     79 0x7988dd1dda1f35542a2b8d5f7de575563ebf068e      1
     80 0x79d1dd0770fec45768a285da42c2521603c9b91c      1
     81 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
     82 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
     83 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     84 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
     85 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
     86 0x862464bbd83d5743f9cf873c6ea3d187d0f909bf      1
     87 0x88f038389cbe95a917042bdb0f3afc434c680edc      1
     88 0x896b94f4f27f12369698c302e2049cae86936bbb      1
     89 0x8b6a3e1f151fbd8a45539e0942918e63d35c6cf4      1
     90 0x8bc3757a675be4c2e459813769992ec2c60baaaf      1
     91 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
     92 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
     93 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
     94 0xa32c38646299818ccedc6401818c2e1639c39c08      1
     95 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
     96 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
     97 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
     98 0xa8879c580a54f190ed53b43d30de269097ad7543      1
     99 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
    100 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    101 0xac1a04679039a1718d3820fbc254ce29269af784      1
    102 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    103 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    104 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    105 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    106 0xb33fb83c645ac2a12b138636fe59604312092484      1
    107 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    108 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    109 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    110 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    111 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    112 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    113 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    114 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    115 0xc4c65d75dd6579671db24990bad1f502d7c96c8c      1
    116 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
    117 0xc952ec84dd5d8ec32937a8e4105922c664564450      1
    118 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    119 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    120 0xc9ceeabc1ac7b96fc45a1c94edaa3b10197cedfa      1
    121 0xca3dff8c740dee29528916eb049cea48f500d387      1
    122 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    123 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    124 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    125 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    126 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    127 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
    128 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    129 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
    130 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
    131 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    132 0xdc819a832bfa55c933238a474954f3646da275ed      1
    133 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    134 0xdd5cecf2835d465d490f79ad9773a848805a3219      1
    135 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    136 0xe05adcb63a66e6e590961133694a382936c85d9d      1
    137 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    138 0xe5cd0fc813d020644c496dd964a32eb9ac17e50d      1
    139 0xe60458f765bc61e78940c5a275e9523d1f049690      1
    140 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    141 0xe84d612c36ae63c639e7cdbb0aa9ed207e58e658      1
    142 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    143 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    144 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    145 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    146 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
    147 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    148 0xf81489f074a9f70c294164e07692559269f3defc      1
    149 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    150 0xff3bc8de74bb1d2f9066c9687f62bf810c66c5ea      1

## Allow Memes Phase 2

``` r
c(allow_memes_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_200memes_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x01e8e9927d7c6b71671865f05783c8cbe04cc559      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
      5 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      6 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
      7 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
      8 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
      9 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     10 0x0b9f898921b2bb8cd2d7d30faffec2c90200cc8c      1
     11 0x0c6306c1ff8f0ca8432b761085d5ce74160a499a      1
     12 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     13 0x0ce390f18af702cca546297845a4a51d102123cf      1
     14 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     15 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     16 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     17 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     18 0x14bee91efb79a8eb2332c31177fd3a61481ccc99      1
     19 0x1566ae673ae80725bcce901b486c336e6acef465      1
     20 0x1cb89e486db5774ba084f683796286848df489d0      1
     21 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     22 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     23 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     24 0x21f023839e7b6fee67d33e4548791fa388564a02      1
     25 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     26 0x231595e3673a10e846803194f4982e1cf3389161      1
     27 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     28 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
     29 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     30 0x2c23b2ea134c3dd6d9a48676a9a41c6ade71adfc      1
     31 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     32 0x2d646486397bbdd894a9bb62d91fc5bdcb8d9f45      1
     33 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
     34 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
     35 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
     36 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     37 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     38 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     39 0x346aaf2e75cfccb82cff0fcb8d2cdc21d5656358      1
     40 0x3472ef70e0d641b2ca48209462d7bf18668e2584      1
     41 0x36ff6a01782501553486a4efe6ea6e07f8f3ae28      1
     42 0x378dcff8e6be1778b04f5712d686517bb6a01927      1
     43 0x3839a6c85a38de97abd0ecc092ef3311a7f1311e      1
     44 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
     45 0x3f4373afdde3d7de3ac433acc7de685338c3980e      1
     46 0x411dd34f17d5b6398f155f768ed27c32ad194862      1
     47 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
     48 0x438ee34be7c6deefbdf0afc21c0d5375b912e0d8      1
     49 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     50 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
     51 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     52 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     53 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     54 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     55 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     56 0x4c8a8c3fcf77f37101d25930e7a086b4e0ec45ce      1
     57 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     58 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     59 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
     60 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
     61 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
     62 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     63 0x5458c15ff220bf75b92a76229fae88f93df9c6c6      1
     64 0x557bd9e829f35f1d853c2c85341458892753fa1f      1
     65 0x55adbdf8a6396356de42aad937ea79caa2cab88f      1
     66 0x55bae3706b678ee2d5a6d6d2faec8a41854aaf9a      1
     67 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
     68 0x5d20e6c0db887b5c3281edfce5bb7677565e2b8f      1
     69 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
     70 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
     71 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
     72 0x641cb2098a7003cf829c7069cf3316c4c54bac60      1
     73 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
     74 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
     75 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
     76 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
     77 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
     78 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     79 0x6d8e6b20d4c132276b0855179214bd2606c9c6bd      1
     80 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
     81 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     82 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     83 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     84 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
     85 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
     86 0x72f52de160ece454a2d75a410f85f4978045882d      1
     87 0x75005a05bc163b85991b9c9facbcf3155372422e      1
     88 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
     89 0x78fc8aa7bb4ee047257a245ca590fac4ae4aa97b      1
     90 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
     91 0x7ae4784a907460858231609f565bd9580f609b05      1
     92 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
     93 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
     94 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     95 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
     96 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
     97 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
     98 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
     99 0x7faa22f45695a77ad4042c4cfb2c0330caa6a329      1
    100 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    101 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    102 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    103 0x848f8b6c67f4197a53ae3869d3200fac175a00af      1
    104 0x86c71b7d27d00d7814a763f9e6f16fdb09f7e4df      1
    105 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
    106 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
    107 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
    108 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    109 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    110 0x8ccb07293755004942f4451aeba897db44631061      1
    111 0x8d12c02671848b17c18322027a2578ea7afbb702      1
    112 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    113 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    114 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    115 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    116 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    117 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
    118 0x9cfe0d47672f0d9891dc312d242349d52d8aba8d      1
    119 0x9ea50708e47a237182a56c481a4d11dd021b7ec2      1
    120 0x9eb69d42dc42c0b20bc93caf1a15a92afa5c4569      1
    121 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    122 0xa03d04ff89308adf54badf18b46fee9980093524      1
    123 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    124 0xa41341bab6588948da8503d1dfb0a0ab0ea269cb      1
    125 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    126 0xa73769aed346319287410811639ac3bec8464d55      1
    127 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    128 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    129 0xa8450d71b98ca4955c63519ef593ba886c5b2b4f      1
    130 0xa8806ccb79069c976e37ed1950ada0eacef6a289      1
    131 0xa8e54b46ae93e14eedae486a9efcd4c7b5a5be20      1
    132 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    133 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    134 0xaf8b04fb70bac8a686aa429fb59428e829564924      1
    135 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
    136 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    137 0xb59a6d337f8d687447fb311b8138340b8c617715      1
    138 0xb6c7e03ede87692deb1de7e40723b952e0fd3773      1
    139 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    140 0xb8e82b32ea205b0546d0ece1a175ad00e404dfa1      1
    141 0xb90aa714bd30e6f135ec15a0cd2605af1590d184      1
    142 0xb98af149b33dee33c97650e877a497bd6a934d54      1
    143 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
    144 0xbbc6e7b905d7794a2318e22ec011bf36b09e1d2b      1
    145 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    146 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    147 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
    148 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    149 0xbe9a677708192ae85e54fb38457b3b4f01c281cc      1
    150 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    151 0xbfe384d79969bcb583b8a0e5fedc314aee480e7e      1
    152 0xc04208f289d3842ac168f2c373b3164e1d872650      1
    153 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    154 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    155 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
    156 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    157 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    158 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    159 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
    160 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    161 0xc45920062985116eaac6589058ed337066d6f2e6      1
    162 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    163 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    164 0xc7e8a6abb9b5c76c75c9bb4f77715793f7f8205e      1
    165 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    166 0xc97a5623578a832354988e7e40869f5207193d53      1
    167 0xca288359ba6319c7e7c7377a670183acb3781cda      1
    168 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    169 0xca6b710cbef9ffe90d0ab865b76d6e6bba4db5f9      1
    170 0xcf6492639384eaf2dfdfb62c22c07928693c852a      1
    171 0xd1b643c63d4dfcf4fae32b58d87843553a64b58e      1
    172 0xd33744da3013927fad387d24f57cfa241735ded9      1
    173 0xd3f5ccd478e59c82ab75f393843015e08892a94d      1
    174 0xd787c0eb515dc96419dfd906b7aa27e8b6209f31      1
    175 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    176 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
    177 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    178 0xdc78107155918e230246439e4159fea4c477eae9      1
    179 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    180 0xdfd818378e60a1b7e26cf165cc57de8ff0a65478      1
    181 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    182 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    183 0xe3013b1d58b257684ed9862dbeef81d863e2e0f3      1
    184 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    185 0xe59e088489a85a6de407768deb671c7e493fd799      1
    186 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    187 0xeb54ad6b8c3e2d43d72b6274627747ce5bfecb66      1
    188 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    189 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
    190 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    191 0xf3998eced96ae3cfe880ac9d2637b4118da48e22      1
    192 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    193 0xf44b63f62c7b6c59a883fdc67bdcd692995b8bbd      1
    194 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    195 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    196 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    197 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1
    198 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    199 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
    200 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

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
