
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:40762       Length:40762       Min.   : 1.000   Length:40762      
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.003                     
                                           3rd Qu.: 1.000                     
                                           Max.   :14.000                     
         name          
     Length:40762      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17782869 # https://etherscan.io/block/17782869
block_hash <- "0xe51025110741dd0017550e22a2ec30afe0a63ac2bd6999ec0a67671f505f64b3"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4469 

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

allow_artist_phase1      <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","ModernRenaissanceFrens","Noble"), address_remove=address_remove,address_max=1)
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

    # A tibble: 139 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01db485f57dc000e761b85641f78c9d212a2eeab      1
      2 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      3 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      4 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
      5 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
      6 0x066ad9510d992444a856731d6af0888c2b2e5d76      1
      7 0x06807d7e07290be6adbacd6f209c4ced6f183d74      1
      8 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
      9 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     10 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     11 0x183b6267cb9f985a8aa7fc912672a21d644a4102      1
     12 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     13 0x1eb575e1b7212a05907a692de46986da95c5f22b      1
     14 0x1fd8df37b360d7366db649703014e637ee214f4a      1
     15 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     16 0x228458226d1f04a01720022cc04ba207c1ae8f25      1
     17 0x23602ca06e977c86339ffddad74966e824ab691e      1
     18 0x23aa9e2efd276d26a6c53623affb072b516545e6      1
     19 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     20 0x26f87915216d0d7375c9febd8a63bb90d1b503c5      1
     21 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     22 0x2dce07502002ecc96d17f948ed52f978edd362d2      1
     23 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     24 0x307afd37899600749f4bd41b65f9807526ef8fd6      1
     25 0x32084ce62e8955da04ce2a2969bd6edbb4b346a5      1
     26 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     27 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     28 0x35c92a2447de0576399aa34b74d2cb26af950a69      1
     29 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     30 0x3726ea72a2bef8d6e53d0a40db95f55a9704c2ed      1
     31 0x374707d9f32ba9afb604939c40a9075738a3f576      1
     32 0x37ddced0a71823204ee0296d5208fc1a5161d75b      1
     33 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     34 0x3d0ca1db549ab8df8a681d2a9e3e72d6da828df4      1
     35 0x3d81a716c76c30b6e2f272461bf9c844aee7469f      1
     36 0x3e397e679fb1adb0ac1facf4679cc222a9dea1a3      1
     37 0x403089b876128b97012aee31e3f91b4f6cfebede      1
     38 0x43098f5c9ae90f5132e14f2901a67ff9ef38a67d      1
     39 0x4360076b45bf475c31cda132b25e2920ce237f32      1
     40 0x44c2a80f8b924858db469a5b35128908e55c531f      1
     41 0x45492c6bc6ab97e4320e6f844c560be62737a303      1
     42 0x478454599df852e11524083605457875e50ceea8      1
     43 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     44 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     45 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     46 0x50ce2d90b63aa4861d4fade0d498fb957c7c9b1d      1
     47 0x554db854d62c403256506bde75f527244a5c7849      1
     48 0x570ff649bb5ae8e032a4f9456aa1ad74cc3f3e8f      1
     49 0x5843236ce0d3e08ecc8898a995212ab4c4a11107      1
     50 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     51 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     52 0x64bf7a7b25a5b15c1572aaf464204a8a528123f4      1
     53 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     54 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
     55 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     56 0x6a3de9a0cb759f634a6a6f66c926d71b055482c4      1
     57 0x6af7812fc932da2d34738a33f295de54011fd16b      1
     58 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     59 0x6dd20703b80b0b97f87d44f377ea76b692504a13      1
     60 0x6de871e6e9d6271c701f0dc031e3a4cd35681264      1
     61 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
     62 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     63 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     64 0x7a04e11f6708d9dc4398b04a15cdb3329c449ef8      1
     65 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     66 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
     67 0x82abb5df200d2998db4e80639b145d052ca40062      1
     68 0x847f33f5c02b0ce987c337fcb190650a91995859      1
     69 0x88d7d41e29612b643c445983c988d447d8ca37bc      1
     70 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
     71 0x98949eaa1189181b765e5e6b8eb854484c8ea059      1
     72 0x98a853c794f8ea6dcac3c6b48c1730580ebde7b7      1
     73 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
     74 0x9a68254a63b08710901e721352342d3b2d30901c      1
     75 0x9aa91eeed8e34d7ed2145d651f76fae3e15371d3      1
     76 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
     77 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
     78 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
     79 0x9fc57c82655dff05aa137da61289e728ecfd510a      1
     80 0xa24111399b765325e151f5f4cd11197cf4afa9b2      1
     81 0xa266e8b5c812c6a9d63b8862599f2963d19a797e      1
     82 0xa3c88e140367547d11cff2912d00caa3fc442d67      1
     83 0xa523da93344dd163c32a8cd9a31459ead1d86b0a      1
     84 0xa72ac4dc61f794d76c408d68d3995d7f75982a3b      1
     85 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
     86 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     87 0xae33247dcd75250c4823db8433c1ed40bd63a27c      1
     88 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     89 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
     90 0xb3dd460c98ddc459cb28fe087d6a2b3190ebc599      1
     91 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
     92 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
     93 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
     94 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
     95 0xc01b3274cee6dba296add4002972f579c8999d0a      1
     96 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     97 0xc4f370c6d3164a56971692362a9e488c0992a29d      1
     98 0xc5633bc0bcc3897117841f434a8f5f95a724a7b8      1
     99 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    100 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    101 0xcd27b5c0b30cc6b4168b7c092d0b43791c9033ac      1
    102 0xcec2baca25afaf9bbecfd9a800ca2c71576cbf9f      1
    103 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    104 0xcf7b0637b0ceb93cf36298ab0dfb48b47edd74e2      1
    105 0xd05b9e776181e3e7c75d6b9f193e8454678fb59a      1
    106 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    107 0xd37905283adf21d3e628b423cbe20c4583ba9979      1
    108 0xd3c09506172b829ed2e9a1316c0c8ae074418c70      1
    109 0xda75f63b9cb1cf7668bb0ee34fa718bc1cb5bbc1      1
    110 0xdd8adbed16cfd91c817f04480f6d9c9322f89eca      1
    111 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    112 0xe06fc98f40fddd6429b3788bb1c605a675809323      1
    113 0xe2f00c430b36d8a6b389f909340d0f2dff93b876      1
    114 0xe4fc2f11f9f7fce3900c82260765f4d10d0eae0c      1
    115 0xe8dbd6474fb0128863f5d3204af5ef363d90adb0      1
    116 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    117 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    118 0xeb1830e98bbd7f526c68bf4bc7916cbca0ec797f      1
    119 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    120 0xec13a4d52d35b564cd418d5bd1a31cefebaf3e65      1
    121 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    122 0xed6ff3a2c8a13f2223f6a7b3985e54e1f8dc064b      1
    123 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    124 0xeda70c1b2226ebf9ba5a439226f56ca983493174      1
    125 0xf058d7ef1a35a94539807a36ffd9e242a9a900ec      1
    126 0xf0be2fa2afee6a3480fc45eb96a80aab643a7eb5      1
    127 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    128 0xf2439241881964006369c0e2377d45f3740f48a0      1
    129 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    130 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    131 0xf5ac7d8d0ab759cddd507742a082612a37edbbbd      1
    132 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    133 0xf7225294b6c87790880926821378f39c60c8baff      1
    134 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    135 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    136 0xfa9c29e6bd27cb30712dd90aa24aa7b09939b20c      1
    137 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    138 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1
    139 0xffdfc9753bc7b1f6e023ff0b33f29dbae26c9a91      1

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
      1 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
      2 0x0e81a8a496f9cb7e1d29382e91f06a6f7416cb9a      1
      3 0x111818a51c4177e8980566beea68fe334be7b76a      1
      4 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
      5 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
      6 0x20163ec2afd9698433a031b0a7dcd70ed77ba8c7      1
      7 0x21f023839e7b6fee67d33e4548791fa388564a02      1
      8 0x289256fa6de33947fd292a9e94a738c3d986f8e5      1
      9 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     10 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     11 0x2c45f65d254e85584b47ec82450c91a30cfc6be1      1
     12 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     13 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     14 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
     15 0x388160a99390392278afdba240046b8b5e73f77b      1
     16 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     17 0x3d91ad906c3969f86c588c988f721d5a735c6f81      1
     18 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     19 0x45133db3eadd8718f5c3a868ec2f7148e460dcfd      1
     20 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     21 0x4a0e071be98b9514aad70f5d2595a8ab23a7f053      1
     22 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
     23 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
     24 0x52690f90740621f89f58521433e9b0921d626708      1
     25 0x527bb834cc5c8ff730c673880e51372282b06e14      1
     26 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     27 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
     28 0x5d0d23ae81ebe1b6ac96411b2c7bc3bf7463fa25      1
     29 0x5f0f6f695febf386aa93126237b48c424961797b      1
     30 0x6140f00e4ff3936702e68744f2b5978885464cbb      1
     31 0x614b89f072ea263a9387460963142e73548fbaf1      1
     32 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
     33 0x69e68074f1aada957edd39c5eae0069973343f30      1
     34 0x6b6d5c57533d22d0de14db59fcd28b9ea5296090      1
     35 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     36 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
     37 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
     38 0x72cea5aaaabd1cc5232bd2117e5d21e83cbd0e51      1
     39 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     40 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
     41 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
     42 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
     43 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
     44 0x8874174a2366668d54fea6343f71709389563c8a      1
     45 0x8f6504b48c66eea4e9fbf64fe220e02f1cc4934e      1
     46 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
     47 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
     48 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
     49 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     50 0x95999c47c3e32a337ef108d657675c2757a606ed      1
     51 0x97ece7185467c78293f3b796bde3704421d4fa69      1
     52 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
     53 0x9e5198418081fe00026ca0ddedbfd3915ab98298      1
     54 0x9e640badecb7c628c6188b74488823e879f42a1a      1
     55 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
     56 0xa04f4a4b7306cb72f30828834db01699362a4989      1
     57 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
     58 0xa309e257db5c325e4b83510fcc950449447e6bda      1
     59 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
     60 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
     61 0xa5ce27ca5e31b1de92db2d2bc109b3e23cf1d4c4      1
     62 0xa7c342af335ea7e0747452ae2e3c1901498a9a76      1
     63 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
     64 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     65 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
     66 0xadd72e24a9e9117aa16d253cb421cb93b00240e3      1
     67 0xadebdeab678647a457743ea3af98f8b804e45c24      1
     68 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
     69 0xb56ae8a727cf38f1f4716aeda6749d2af340d8f4      1
     70 0xb68e75f5424e4dd8b97fb42c4965d08718349d69      1
     71 0xb8700157128ff544c46cf35dec5a19120baa8eb2      1
     72 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
     73 0xc04208f289d3842ac168f2c373b3164e1d872650      1
     74 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     75 0xc1b0307ff325527511310c7d6fd3248188742f86      1
     76 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
     77 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
     78 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
     79 0xc7d2d152dda8cf7a7860288c3db9a1a4ce29162b      1
     80 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
     81 0xcb85b581caa71949784d2e826cf9df391c244b33      1
     82 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
     83 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
     84 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
     85 0xd74e767c77d2e9f9e467e7914f2379da81b63a44      1
     86 0xd981a38d80de1c1528349b841833d3a9a745cb37      1
     87 0xda34ee56031bacb68cc3ce57339c2a11c28d8eb3      1
     88 0xdc78107155918e230246439e4159fea4c477eae9      1
     89 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
     90 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
     91 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
     92 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
     93 0xeeeb5dd03eaf4ab6b72949191d8ffcfe27f7d973      1
     94 0xf054274dd74987395d28136e53f39ef4f7b19994      1
     95 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
     96 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
     97 0xf624e9324f9b330cc0289775d7b91e945e881134      1
     98 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
     99 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1
    100 0xfff39900273ffb1045c7cfde62df1720b63fd6bd      1

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
      3 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
      4 0x04294157cfba9ff0892f48f8345ea3539995f449      1
      5 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
      6 0x080ffeaf914180e18f69092d66de11925434b540      1
      7 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
      8 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
      9 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     10 0x100f28c9b8b1fda897eadc775caf74e0f3560f73      1
     11 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
     12 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
     13 0x20542dffc2f7a51cbc087f54fcafd9ffd8b5c899      1
     14 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     15 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     16 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     17 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     18 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     19 0x32e263cb9003f7fef329080bdcbc82f5cfd6c02f      1
     20 0x37c14f24c73a9e151575a0af3dd2dd5cbbca8a29      1
     21 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     22 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
     23 0x43b9778940c53abf2ca96ba900ff181ec09d7fcc      1
     24 0x44b4d430e7c56e4ca04c5799e561063ccc1f1df2      1
     25 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     26 0x49d6531aea6dced6595d50da685274766e4cfc8b      1
     27 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
     28 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     29 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
     30 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
     31 0x52f051280e850f32cbfafd8d78c31edec4c3248c      1
     32 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
     33 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     34 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
     35 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
     36 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
     37 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
     38 0x62c868061cddbe38e01e7a8295fdcec72c10ca06      1
     39 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
     40 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
     41 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
     42 0x76ea151c88fd91902f7bf3f829db65dc9ba5d45b      1
     43 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     44 0x8169cecd991392586828011079f29d05b6a9edcf      1
     45 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
     46 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     47 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     48 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
     49 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
     50 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
     51 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     52 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
     53 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
     54 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
     55 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
     56 0xaa0ae8cd18c3aeb0bf4e0bb3507b2f86f7021ded      1
     57 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
     58 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
     59 0xb099a3c3043b262518d9b6cd4d5c9f589b15e2ad      1
     60 0xb196931ec22517b0510705eb56d5652fe73877f0      1
     61 0xb2bc498a214282efa54877ecd082165d4cf86df4      1
     62 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
     63 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
     64 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
     65 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
     66 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
     67 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
     68 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
     69 0xc4f5b7207d510e2f35d045b278a29f57dbd9d15f      1
     70 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
     71 0xc89503a4e57619a2ac87a821732ff4afdb2bced2      1
     72 0xcbb49b18f2e9d002bd79fc1495fcb6f6f87f1e0a      1
     73 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
     74 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
     75 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
     76 0xd36590461162795ee33099b2076a0d4e017ae17c      1
     77 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
     78 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
     79 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
     80 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
     81 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
     82 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
     83 0xe0e263fdda4dd2432343b1ea2e49086cc3f22745      1
     84 0xe23e0bd6e320c87367b0e4b797b42dc9b4fe7ca0      1
     85 0xe4000d4f3f0e59ca00b803f54784fe0654a192f4      1
     86 0xe59e088489a85a6de407768deb671c7e493fd799      1
     87 0xe5ecabd057db846332957105e7d7bbc8369e088c      1
     88 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
     89 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
     90 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
     91 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
     92 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
     93 0xefb3da5189a6169a61176d1357579e135a1d1187      1
     94 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
     95 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
     96 0xf620422e1e2cbf123386ea664f536d1016e5a5d0      1
     97 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
     98 0xf8be957f65e67fb0342992a51c30290d5758f880      1
     99 0xfeea8258077cc06444679958185f4198dd4cd324      1
    100 0xff3bc8de74bb1d2f9066c9687f62bf810c66c5ea      1

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
      1 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
      2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      3 0x05b99f8f59d67b5e7b1dea82b1370cab65ed91b9      1
      4 0x085d07d65b41158a1545eecf05316edb5d163b54      1
      5 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
      6 0x0d08c74268c1260d9b50175c41738e1a45819700      1
      7 0x1003b47b2980ae3cf433165f81296ef747e4f484      1
      8 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
      9 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     10 0x13c39b3e6e6159bc233fad90825f1f9f84fcb102      1
     11 0x13e073537f9d820eb7b8bcd41edbf7f9df85024d      1
     12 0x17d6df353c4062b6097ff8c872ff6ce31381988e      1
     13 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
     14 0x1d05a2ec18c7d4707ed4cd40e7e76a680e4618e3      1
     15 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     16 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     17 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     18 0x2ff074271386511555f0682139943cda5fa5937d      1
     19 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     20 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
     21 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     22 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     23 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
     24 0x48be4681972473b498e8b686d38e04826c26fc4f      1
     25 0x4ab5189a020ccd88f05c36de69e4d868d29c75a9      1
     26 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     27 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     28 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     29 0x527136cf29a1df9024a6493f219a1da64398fdc5      1
     30 0x533fa4a37aaffdb02456d374237091520790383e      1
     31 0x53887f0dee06c6459bc928f9f39beccac3947325      1
     32 0x58c5b881fb2794d04f0dc0ec65f94a119cd43aa0      1
     33 0x592e480e0066a51eb981b532e275d6576e5730fd      1
     34 0x593cadf136c68c720d446461c1bfee600647c6b8      1
     35 0x5bff4631326de92282197696366df57198e38dd3      1
     36 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
     37 0x5dafb2d1f8b20501da62ba00c0c283293aa0b70c      1
     38 0x60bc11840966ae3946ad1904a8efb6622226be25      1
     39 0x63a612c0b8dfc559318e39daae6d0c7d09212965      1
     40 0x664f45ed5084abcf2f8e1a95e320b06cc700591b      1
     41 0x69cc363055d8c3d5e211865e805e145ab9208d57      1
     42 0x6cb9275b70a362d8fe5d445268e92186d61ac8bb      1
     43 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
     44 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
     45 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
     46 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     47 0x75850227f61183759cbbfe2b3fb196e798ce0b95      1
     48 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
     49 0x782ff3f609427680db175365c24c238f89fdd276      1
     50 0x7988dd1dda1f35542a2b8d5f7de575563ebf068e      1
     51 0x7c76cf8894d42ed11b53236da54f05add1d44073      1
     52 0x7decf7a31168778f311c57b9a948abaa7321001e      1
     53 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
     54 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
     55 0x86125855ea134f1da31015663fd3c19a657a8d16      1
     56 0x892eaf46a9c1cb75c3f230420dcbee6320c26932      1
     57 0x896b94f4f27f12369698c302e2049cae86936bbb      1
     58 0x8f4b933491e1319799229ed9d36b179bb859d705      1
     59 0x915c6cb77d302b8c514561feee12b5cb6532a97e      1
     60 0x942dfb0c7e87fb5f07e25ec7ff805e7f973cf929      1
     61 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
     62 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
     63 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
     64 0xa1cecd5e4f6683189c3d4f832a16600c20e5b42a      1
     65 0xa1f52dcd0bf4867f3f9d8937b00fd8d2af3e94b2      1
     66 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
     67 0xa222204acf1be4077d34102fab38a759060b77c2      1
     68 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
     69 0xa76bda01601ebdd50512c2103a329db572d7a77a      1
     70 0xae220d647426e368ac321efec83a9d696bf84e7a      1
     71 0xb4cb9505397a55870234b11b622f0fe7cba064ce      1
     72 0xb70c3049982c09c18dcbf8596ccef6f5b3c239a3      1
     73 0xba264037d93f4e06d5b19e9a73f29fa64689e1ba      1
     74 0xbb8fafa8a629c4dce022d95e098ccccee1acd942      1
     75 0xbccc968eeebffc2586a50eef0ee1cf12273c9604      1
     76 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
     77 0xc14480bd7bb6d36377c75286b80452654f5b7a79      1
     78 0xc14e891f453b14df5449a6644bc88dab2a5e5622      1
     79 0xc30e9fda779efe2081dae6529c0d07fc16769c8f      1
     80 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
     81 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
     82 0xceb775d4b09c2fba2fe51efcab6e39a7da1528c3      1
     83 0xd9ba239a881f718072e57d9810846c8d705f93a4      1
     84 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
     85 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
     86 0xde328c96ae88f37733a8d2302a0eea537d557b24      1
     87 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
     88 0xe422e3c0031396d5eae414e6a0f70c5535b6ec6c      1
     89 0xe61c204d9ab083240a7c8522e636298261ff354e      1
     90 0xe69cae620953aef05e9276f35a7d3cb598bf4b20      1
     91 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
     92 0xe7985c511d1bf0c7668757650b974513628dea7c      1
     93 0xeb775bf133c142e873f2ba6925d53107550e8703      1
     94 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
     95 0xee958e45f3464d712b8830deb5875c8ac105f698      1
     96 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
     97 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
     98 0xfc2ca2de8b849f7344ffd138f10d618597834a09      1
     99 0xfd8835df382b69b3cd498aba0d745fbf6a421d13      1
    100 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1

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
      1 0x010d09ddd06776f28361556f02d3cc137960876e      1
      2 0x01dd451ebd199aa835da8e31d9a321d0921dc26c      1
      3 0x02009370ff755704e9acbd96042c1ab832d6067e      1
      4 0x0851ced43aab7bc38b0fa4fbc4e3849634d2ca67      1
      5 0x08fc70adf6b0950749b7647f67616589b1853a53      1
      6 0x0f22657ddcf96c4d8e7cd22f0ab18fbfb579e551      1
      7 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
      8 0x12d0f6e748483771383bbb1a0ba1fcaeb2af298f      1
      9 0x14443fb547300ff12ae7230d13f531536e648c02      1
     10 0x1adef8b7253c43caaf50500913d422ae688367bd      1
     11 0x1b59628a95a55e374b8ff4274b4e88372bae9484      1
     12 0x1e6c02d4f25dff92b8355abbac1b9522e0a7e8e9      1
     13 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     14 0x22fc06a488b236753a89caeed39b58046b153069      1
     15 0x277884f6f7c8a96aa25cd5d4ff0a6354c57017a4      1
     16 0x27abb15accaf87e2795342a4ef445e1789bc1f7c      1
     17 0x27bc503afb91d9e7fef952446e6ae847f4963cd2      1
     18 0x28b0ec2af8d6d30e02888b543652365180c6b3c9      1
     19 0x2db19ee7f7e0eae5730e7a393fcc899d70df9f25      1
     20 0x35a7378f37918969f7a733d46519de9bbdc6fb04      1
     21 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     22 0x3c554c31c2f1938bdde970ac9add36f6264b55bd      1
     23 0x3d9e77bd53dc0c2a3110732e30d0b969713fa4c2      1
     24 0x3e89a0f26657013bf251ced30d5d198c3b0d2466      1
     25 0x42f34449209059717e6c48ed0110783a7df82abf      1
     26 0x435e598e5fd4477fbd0379e2f56afd73f272574d      1
     27 0x45d2bafe56c85433e0b9f9b50dd124ea3041f223      1
     28 0x4612f6ccdeece14255a757395477907d5f33038c      1
     29 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
     30 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     31 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     32 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     33 0x4d8cb131e2835855e37eb277943f308054f4242f      1
     34 0x4d8eb365bc03225e74bfb90ada1c2ada2b6cb8cf      1
     35 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
     36 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
     37 0x541237a3e049f2ef1105694b491152f450aba4db      1
     38 0x5591f6258fd9eb2ad0d3a4e88245d06e1e8661bb      1
     39 0x565f34ec537c1e49d7821d713d392027559fe5f6      1
     40 0x56d90629aa29830e361e113e53a979f95afe93c1      1
     41 0x5a381564aeda17faf048d0eba3ad7e7015463f78      1
     42 0x5ad1592518eadc34a22263fe5e56c19c46ffab8b      1
     43 0x5b3c2035f87ecd710d363a8c9c982f53259c6edd      1
     44 0x5b8660a36d3af77830a35355658bde886dad95b2      1
     45 0x5bbb102049e1205a2a31901a377c7252b194b294      1
     46 0x5dd47005f8a429b228a5d30b5345a1b64fa78c0c      1
     47 0x6177adacfde59c3ace55356250b6504488ac513d      1
     48 0x652c09da7cfc545ae2c7cbc36799cf90fc8c0bff      1
     49 0x70e7ee4dbbf6f91ef363cd701f4b98b70ec1ba52      1
     50 0x73db79faa288b10aa850931d2794ac58ffdf5ba7      1
     51 0x7426b39865d11207b8f795b10d70843fc3289051      1
     52 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
     53 0x7660f818934c9e703bcad4de31c441f6efcf02e1      1
     54 0x780ba6c53069f5fed88a59ae3e7a78710e889594      1
     55 0x787898f7ec41d2e1c57f235e83375cfd98433d0d      1
     56 0x79b5259dc0fcb44f9751bf9f78f5ef46a7958460      1
     57 0x79b57a64b074368ae74a518b3fa2b30f83461f0c      1
     58 0x79f4ee296b33f4fb4cd78efbe99e3a0eab5d3c7e      1
     59 0x7b71fe55b075571f08122059afab5ef708ce36d6      1
     60 0x7d6b500409bf4148ed04aa9a359fb0959a7d506b      1
     61 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
     62 0x81787ecf8d91c1c6827f28e68e0ac3979a1263fd      1
     63 0x83baef95f31e72010310d48777f86c6aaf5fb931      1
     64 0x853c69418605529a68907aaf7789270e3cf69d97      1
     65 0x869c9fe2eeb90de5da26e115c3e3dadb7c461f5f      1
     66 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
     67 0x90451243ad2d7da2229fcaa263f460dff2e375ce      1
     68 0x906cc8125a4c21c2b7f634efd5a5702383b925ea      1
     69 0x9969db4034a136650cdb07955cdf5635499a4012      1
     70 0x9aaddc5040d2093dc0dafc0803cbfca822341bca      1
     71 0x9ab8c999fb53663ebd351c2f2f248f16269189c6      1
     72 0x9bbd708653f4f105d5eadd19607b7f360fa787af      1
     73 0xa495e370e5987babb57580319c2ecb8e52a1239b      1
     74 0xab6ca2017548a170699890214bfd66583a0c1754      1
     75 0xabf107de3e01c7c257e64e0a18d60a733aad395d      1
     76 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
     77 0xb16525385824e8cf877be513386e75d875043ffd      1
     78 0xb49806cf8dcdb108d318d6f4f7ab087851445d94      1
     79 0xb948ea974cb95c9c73c51deac5d9e4464e769a44      1
     80 0xba944a0293e614c9437083477eb466ab81fc908b      1
     81 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
     82 0xc74284819fb551f9827fe084e91c34cf27f92388      1
     83 0xc7d178d2582a0889d3ed5926ca5ac7c9d2a97317      1
     84 0xceeab2af38e6b086cdce120c49f93b65f0b92b76      1
     85 0xd34d42d1335ab3ce12c8dc85ebe21cf1fcdefed8      1
     86 0xd96a41af259755aebbd7d13afef763dfe8197e04      1
     87 0xe048ca2aa2b5fe1991487bbe46bbfafaf4234402      1
     88 0xe25e6dec6cc0375947479520968cb52c24704cec      1
     89 0xe2f3e38565314fbece1c6aef5554852891a5268f      1
     90 0xe47000d2dfaddb0180cbc07da3e2b5c66a612b63      1
     91 0xe48ab528f2b51fa68e22d57069cffafcd4aa2b6c      1
     92 0xe539a21fbdf04c289c167c3ff2f97754080c235f      1
     93 0xe7380b7be1ed475de78b07033a8ef4dfb10205a5      1
     94 0xe7b0c0405fe7f6d057ef2a4c3af5afada4b33fd7      1
     95 0xe7bfc67952b0a48f4ce3db309ab1adda322763dc      1
     96 0xe8df91ff1ce483ee7827ae58b02975d88611be0c      1
     97 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
     98 0xefc2702d50fb43cec83434d205c89124db029566      1
     99 0xf1914657d98373e3b3eadab8e82b0e0431e61d0a      1
    100 0xfc4672341c78f1ebc0c23fdbcbb03b61c6f2d50f      1

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
