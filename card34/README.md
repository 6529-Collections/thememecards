
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "15913893.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:514         Length:514         Min.   :1.000   Length:514        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.018                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:514        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 15917269 # https://etherscan.io/block/countdown/15917269
block_hash <- "0x28bd108d213cf03245ac95d09ee14dbcf930e545537800731f15eefc342f1484"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4495 

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
  "0xcda72070e455bb31c7690a170224ce43623d0b6f"


)

hodlers_remove <- c(
  ""
)

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=10)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=20)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_jord        <- pick(snapshot, contracts=c("AirdropsEditions","JordHammondxEditions","AbstractVietnam"), address_remove=address_remove, address_max=1)
```

## Airdrop

``` r
c(airdrop_gradient, airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     2 0x1566ae673ae80725bcce901b486c336e6acef465      1
     3 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     4 0x1a6f0dddb884854355023a2dfe26e9174f8e0290      1
     5 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     6 0x388160a99390392278afdba240046b8b5e73f77b      1
     7 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     8 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     9 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    10 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    11 0x57c25777bd6dffb3251306c0a6449bebb58a7af0      1
    12 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    13 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    14 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    15 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    16 0x7b5af6790381f932abae790e8b0d0ff50e287f8e      1
    17 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    18 0x9b96980c1c018cb617f6653f6892e19ecf4f81e1      1
    19 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    20 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    21 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    22 0xb0728e38704fcc7ab3fc87d7aaff434e6b4b6ea8      1
    23 0xb1b81fd772169a18504aee662cbd5ebb4886e0d4      1
    24 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    25 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    26 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    27 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    28 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    29 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    30 0xdd5cecf2835d465d490f79ad9773a848805a3219      1

## Allow

``` r
c(allow_gradient, allow_raw, allow_singles, allow_jord) %>%
tally() %T>%
readr::write_csv(file="allow.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 202 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      2
      2 0xb5374cac6cad6b025246f19d20b0d4151b640558      2
      3 0x000cd27f10dffac73201258eaa3925c0452051a0      1
      4 0x040580e951625d17514faa1aae86a893ba68cc42      1
      5 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      6 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      7 0x07603b5ad072eda88142ff88dca6133fd74893f6      1
      8 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
      9 0x0f932519de6f2c77265b38d1a4ea64e684fd94b3      1
     10 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     11 0x12ce144ec0e5fddde3c3cab43bdf0bcd71032a46      1
     12 0x12d1bc5a9d1e50ab0d2d62a31c7342abd343ad79      1
     13 0x17bf4672860289adbc797130a4bfc71a5a92072b      1
     14 0x1994fd0421f07a6d031bbe84d4e6377cf031ac0a      1
     15 0x1f5c84e19b7d45aae5ff5b058726f25fbbdbff23      1
     16 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     17 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     18 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     19 0x23324ed44904260fe555b18e5ba95c6030b9227d      1
     20 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     21 0x2506cd8c7bee35ebe54670bd28eb00692eacd426      1
     22 0x251be313e35d620a36b148e1e301e7082e0d11c9      1
     23 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     24 0x276dfeec6f5772156b19116d76d1e3deb3b6f0b4      1
     25 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     26 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     27 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     28 0x2b9acea4ed4f4bbf5545c4665504deab89aadb7b      1
     29 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     30 0x2d701eadefa8da5d99701fc18f291acb4a78ee73      1
     31 0x2d913709fa6b87502af62b3b2f883cfb7024b655      1
     32 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     33 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     34 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
     35 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
     36 0x363646835664ef67fba5a050d75ffdcbe4fd1b3d      1
     37 0x369615bc57975b06c418f42d3fda3754a385b97b      1
     38 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     39 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     40 0x3a36c7281c0fe04358aeee6bee6c812057fabac5      1
     41 0x3a6372b2013f9876a84761187d933dee0653e377      1
     42 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     43 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     44 0x3c7a0fb79e04c3c7fdf66452a3ad073998d49104      1
     45 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     46 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     47 0x3e3dffb5d694582713e1d77cfa74e55876ce8c45      1
     48 0x3ef480243ca002a3c08fe814d01bec2624d123e0      1
     49 0x417a604a4bffbf89546fd8bc6bfd1b20c133c32e      1
     50 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     51 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
     52 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
     53 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     54 0x47d3968b93a0a051eacae35867f73cc465526411      1
     55 0x483a36e454e711ef5ef080539dfc1218ff09606e      1
     56 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     57 0x4969cf3d82afaebb1604da6ad393c51bb797ee2c      1
     58 0x4c9f829f7e317ff75c5ab562b502c45995ee1740      1
     59 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
     60 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
     61 0x5231b639babbd14c518176f12a6bb8cd85fd6e70      1
     62 0x52cc2c8db59411310e130b47d478472d9f7e4597      1
     63 0x53006f95def268f88dc1b8216654ab56f3afd052      1
     64 0x537d65f890159b2b5c85c6e17918d45c1eca85c5      1
     65 0x555a9d7a5b796bce7d2318dff0e4c5120f5e06ed      1
     66 0x556dd954955aabccb479ce054369d50758cf58ef      1
     67 0x55ac7cd8ddb63502ced9afe13e7e5f66012cb5e7      1
     68 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     69 0x57f02141f2d91a024032536ec61c974be76eb7c9      1
     70 0x58c7f158b78e896c94c0c9787c8703612e6c496f      1
     71 0x5aae5802046733fa533ae09bf644ce359d83fcc7      1
     72 0x5c33243547e2213a6f953bd3f2377533b9b30a33      1
     73 0x60322b225e06bf2f0c36c45d0b06090e891ca91c      1
     74 0x607ec4eb37c210f4f94542482fa1c64bb6cb9dd0      1
     75 0x608f0a54b97604a528b48c7164f6990a03e241ca      1
     76 0x6177adacfde59c3ace55356250b6504488ac513d      1
     77 0x619db8e961f9b20b208da059a9156ef85e5cfd05      1
     78 0x6479f7157f06e6610174b1029388b8d4193c00a0      1
     79 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     80 0x64f7e4e59a5614411a1a1675de29874a923bb3ee      1
     81 0x650e075037442bf40738ecbadc12014c37215563      1
     82 0x65ad94bb30fb9949f20a02f641d118d20866b706      1
     83 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     84 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
     85 0x696cf243ac1879895c57a11d9aba0255c950155c      1
     86 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
     87 0x69e68074f1aada957edd39c5eae0069973343f30      1
     88 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
     89 0x6be3cf8b90ef967ee423a7c4810fae0e3a95c52b      1
     90 0x6c0e639dcfc73e810bf66c90a9005d4ebd8a7e5d      1
     91 0x6cb3c33ce8366c0bc5f3781870a0bed413a4dd80      1
     92 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     93 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     94 0x6fd33d554a2dabeeb4f3aab0d5f28884f2e9d511      1
     95 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
     96 0x732739de18df630b354ec365a927a4f4f2bf28e5      1
     97 0x73bb5d143e2a81ea002684bcd39371fdf1eea406      1
     98 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
     99 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    100 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    101 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    102 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    103 0x7ab977f469496930f43c7918022a0915f0bd9de4      1
    104 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    105 0x7f3b59c7615c3b4a6d2b64217a9fe289c2e5dc7b      1
    106 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    107 0x80a7747adadd60de888da9d7e9054e8d33817d02      1
    108 0x8105e062d6b94c951e93739aee33018bd6a6d1c7      1
    109 0x82139687faae8a29851902783e02e699de0e0846      1
    110 0x847784f76cd7fced05d478552ea72d0e441fcd86      1
    111 0x85e37cd123d2889410d9fd1f434c9936e882e5c6      1
    112 0x86d12e8541598a0ca0017c7dd7222d60380d2345      1
    113 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    114 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    115 0x8d384ca5e5e07f1afae406158cb9930a11cfbc51      1
    116 0x8e273acf2b9be5db2d537e9d3144f518aba22c7d      1
    117 0x8e2a6185d7c37aaab6375f9737ba0a764cde36e0      1
    118 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    119 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    120 0x92ef6abe00f0fdc60f5c0a950772f0e8bf277e71      1
    121 0x947c4e6b63d1440d864044df89369be6a62406cd      1
    122 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    123 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    124 0x994d0c5e289c9750849b916cd4961e1e7ddb451e      1
    125 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    126 0x9dea4aba5aad3cbf01dfd5ef02bb14e00063625d      1
    127 0x9efcc0a6834f43c3fd5da0a00d4cded19614f6eb      1
    128 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    129 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    130 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
    131 0xa709d0293bdc12d2417946fb7f55231b6066c177      1
    132 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    133 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    134 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    135 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    136 0xacab2b6c992f7ff130650e614de7a4b52b0ff55b      1
    137 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    138 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    139 0xb35e4e46f6bb4a0701b7afe137dc4a06170681fc      1
    140 0xb42573ddb86b68429c8f791ed0f2b79d2ca95588      1
    141 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    142 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    143 0xb94f7ed85b83a65709557433df1c3c8f19f7c94f      1
    144 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    145 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    146 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    147 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    148 0xbe356c83377eab13a4d78ca89f1bf99b54bbd792      1
    149 0xbec0ddf1ce342bc00ad090c79c351941ee1303c6      1
    150 0xbefc8ec3a651e87b809bb35be95923085606a7f7      1
    151 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    152 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
    153 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    154 0xc15753ae3b6099b8a3366b836433d6542645b876      1
    155 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    156 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    157 0xc3086007a83a86b81b42958539d17881c1642195      1
    158 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    159 0xc77a38504bc507990c4782031abfe847113970a6      1
    160 0xc88291a6491e951b14568a97afcedd576dcb87ba      1
    161 0xc96f569ec842cafd7d51c1522101451ae3007525      1
    162 0xc976f3841c1e39ef84b5766691ee1ad1951ca65f      1
    163 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    164 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    165 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    166 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    167 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    168 0xd6208d782261c3df0015d15e4156ce80a32b6c22      1
    169 0xd75cd3dfea2d1688961e62b08b333692df0e66ec      1
    170 0xd841d14773a2e79629615929f3150875e94a80dc      1
    171 0xda27f8e647040571883bba66b15fc53293bd16c0      1
    172 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    173 0xdae386aeb8848cdafd00f2c045516e30222571dd      1
    174 0xdb561a899557404581e6180fe6d4178577dc117b      1
    175 0xdecad6231bd1d59174fef87c33589c9611a293d4      1
    176 0xe1994b2fd8310218a06875b4b3d7cbca8ec83e4c      1
    177 0xe27807deadb0fd98af2438fac1ee71e252fb926d      1
    178 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    179 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    180 0xe5881d7339e2885203e1d061d740f268737a4bf8      1
    181 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    182 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    183 0xee0ac187684f8cf9bf8e76f93cdeb215a8bb4e29      1
    184 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    185 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    186 0xf041c4f026547f7fbea6904f77bea16997024751      1
    187 0xf136d2a2744b917363ec39bd17641419af788ce0      1
    188 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    189 0xf2212df817c93eba40a9a746562cf04aed8e4325      1
    190 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    191 0xf532920bb32122d6475ad4cc7634dc3a69631902      1
    192 0xf86a588f3604e3b12899f710e3d572f76ffb94b8      1
    193 0xf99b11016b4e9ab00226446af3715610359fa8cd      1
    194 0xf9f085e6b135872bd1c1cb3a80d7127741a9a6ce      1
    195 0xfb0eb3703b3331838d02ffbb7537d7fdef21a6ab      1
    196 0xfb5046f70531e73697329fb491a124a14bf32a42      1
    197 0xfc681305295ae89991d3aac5c444fb6d96b3ffae      1
    198 0xfd22004806a6846ea67ad883356be810f0428793      1
    199 0xfdabf3b545aae3d49eb79d185beee3ee49859219      1
    200 0xff1358d5a84cb340acebabcde034b25b18540dc0      1
    201 0xff5fc3640912c289e9ee5a93dd8e8999e2da94c7      1
    202 0xffb8a15dc8796af3e7bec74e528f1bd810b854ed      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.1 (2022-06-23)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
