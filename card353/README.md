
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:404         Length:404         Min.   : 1.000   Length:404        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.327                     
                                           3rd Qu.: 1.000                     
                                           Max.   :19.000                     
         name          
     Length:404        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22296969 # https://etherscan.io/block/22296969
block_hash <- "0x6a55a0caddb181e1dd8511370409f3b1e6e30afa8b1d2ace40efba73a53784cc"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4853 

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

allow_artist1    <- pick(snapshot, contracts=c("IntrepidNights","IntrepidOcean","IntrepidOutback","IntrepidExtras","NorthernExposure","SuperReal","FireintheSky","LegacyContract","Foundation","IntrepidFalls","MakersPlace","ShadowsandRevelations"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("IntrepidMetropolisEditions","IntrepidMfersEditions","IntrepidExtrasEditions","LightningStrikesThriceEditions","Rarible","raremfersEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("IntrepidNights","IntrepidOcean","IntrepidOutback","IntrepidExtras","NorthernExposure","SuperReal","FireintheSky","LegacyContract","Foundation","IntrepidFalls","MakersPlace","ShadowsandRevelations","IntrepidMetropolisEditions","IntrepidMfersEditions","IntrepidExtrasEditions","LightningStrikesThriceEditions","Rarible","raremfersEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 66 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00e87b69cf3686e028f96d14cd79ec30983e0f53      1
     2 0x024acdae4b00cf47430f4805ebff53b397f7ae83      1
     3 0x0282e055d3b2f7c0ad656952ac1bb989fb6d0086      1
     4 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
     5 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     6 0x11360f0c5552443b33720a44408aba01a809905e      1
     7 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     8 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     9 0x133a123687c6c4d12abcd18a8dacf2b7871d9548      1
    10 0x17b1cb1ad28e8e8b038139e95cf6223ee7e8b572      1
    11 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
    12 0x1800bb3bb01ec636480cb45c61f45ae34c97170d      1
    13 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
    14 0x1e9b5a63fc1ebeb6a8302f81fb244cb5c46fcec4      1
    15 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    16 0x2b82ad50033363fffc3cf001743cbbbb83cdc11a      1
    17 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
    18 0x2de986a374cba9d56f062c4106063455779676fc      1
    19 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    20 0x38a6133018714e4f21b914acace1cf7b41d4c574      1
    21 0x39fe6d7319433f4a7870644461aeeb21d9e2ae3c      1
    22 0x3a49309413793b32f6a308769220147fedbffa5f      1
    23 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    24 0x3bd3ce01c82a12d7cff7c85a9e8bb27ae42fb548      1
    25 0x3d123ccbb151d403b9817be5505b1d0dba8ed576      1
    26 0x40c839b831c90173dc7fbce49a25274a4688ddd9      1
    27 0x424b3adf010edde7c981873082c61f44a9fee413      1
    28 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
    29 0x4908b92cb86c6a660f3d291117236b94c565d4d1      1
    30 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
    31 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
    32 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    33 0x59068075a799594db03c0255eed68e8e121155c8      1
    34 0x5959002cb524181d5526714c2804c3775212d823      1
    35 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
    36 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    37 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    38 0x68a7ac13477aad590982293feeeb786a00276cf2      1
    39 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    40 0x6e513ada916670389097752d05bf609d1246b4d2      1
    41 0x80d762e5a4dc2bc237f594a7f696b5e216067036      1
    42 0x8177598e08ee8199c160a48c7a0af31ab54bb59f      1
    43 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    44 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
    45 0x8497277c9339170a0420e86da8352e0c084624cd      1
    46 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    47 0x9187804eed28934cc59049297162554a592e96f0      1
    48 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
    49 0x974c16605e95d7c742d0a4ba79bd8e13ce4732f6      1
    50 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    51 0xa71c36cc728706516a50605bb873cda1abd4e927      1
    52 0xad9e9c4e50efec42432f6aa70ff52f528da94105      1
    53 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    54 0xc8dd40ba1bdb6a3f956904f02b14db24013b8b5d      1
    55 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    56 0xd050c97e76f882187c781feb2e74345227e2f710      1
    57 0xd13109c75b49c9d297ec5c76cc6f409e401c8bc3      1
    58 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    59 0xe659c74753c241943687d8931358a851f11312ff      1
    60 0xe91f0085f5c3e67c8ebd5c8f6e7f4d884452dbaa      1
    61 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    62 0xf217de9e1442b1f61cee9dac2a07bea96d83e06c      1
    63 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    64 0xf41125ed2a8c74d54d71f8b6454af03e6e3dcaf8      1
    65 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    66 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 151 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x040df97224a2d65df277ab4adb58596a42a9698c      1
      2 0x04400db7f33d5330f62c8dbd4840920adc273615      1
      3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      4 0x0666028d0c4eb0ad6257b63e697590ed130fc1a0      1
      5 0x06f549f0e578018095e93325bfaccdae3ca21df3      1
      6 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
      7 0x0871deb34bfd2052b1c10dc4f6c0912a2a47e927      1
      8 0x0a8138c495cd47367e635b94feb7612a230221a4      1
      9 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     10 0x1b5c7c62d2c4b3e65a9ca49a17c7ea1b6d9df696      1
     11 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     12 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     13 0x21df37900c14bd8af6b12552a1ec2779fb978160      1
     14 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     15 0x255148be130862531729937152d82c51142a5d56      1
     16 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     17 0x295e8fbe2853da5b3c2136360dcb072672e90e1b      1
     18 0x2a76f7df64889a1f20f5b6aa87ebffa9a38ab925      1
     19 0x2bf0246bdee6b027d2177ee84905c25b174fc622      1
     20 0x2dbd9b95c185c8e40df7ffb5b0ea8e62c85d3bd3      1
     21 0x2eae4738fe75c88cf1d6521433d30cfd85961992      1
     22 0x30b7b41e299f90ae78a11b764ad2804ab2bf272b      1
     23 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     24 0x31cd4492f926f3ba9df29d5fc5e2cdc5870250c1      1
     25 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     26 0x3a5e247429201d70fae0ff28e41fd8a1ca50af5a      1
     27 0x3aad83fd218881e391b65fdac31dc3cfc1da8cde      1
     28 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     29 0x3dca4687174473c6c3e28a954b4590389d418848      1
     30 0x3f7a83dcbd24fe560ba0741f9815b368fcf86ece      1
     31 0x408125b99f2f17b90c7b30b2e9db7baa88e6b8d1      1
     32 0x41146e2b9dfdfc47724a20cb86a15dc206c97e44      1
     33 0x437c3056e488b3ad705039770a8c9d7446258de6      1
     34 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     35 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     36 0x4c327ee3ae154220513cddd0292d166bdb414426      1
     37 0x512e63b6e620d4b413c20625c023165a3727acd6      1
     38 0x51e25bb517e9cfd207c9c733a1a4a915984e17cb      1
     39 0x56b5822f18396c247f8065eb6d03b08ad0450469      1
     40 0x58d5605e30f18ea85c112718cc430b3c1c006ead      1
     41 0x59d44f2d8e608cee2c3e57da5a5de3bf192a36c3      1
     42 0x5aac6d3c1fc68b2cd4e45537e22f4a0ea4f7ea98      1
     43 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     44 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
     45 0x6584b48ccebd9c3e4413a1dd4cdbc38b2500cdf3      1
     46 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
     47 0x6b93809e145d2301fb21d955e4241444f784dcd9      1
     48 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
     49 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     50 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     51 0x71eece7c9a6e68bed5ca071046cdccec20ff9808      1
     52 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     53 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
     54 0x7c2145e13c6917296d2e95bd5b1f5706c1a99f72      1
     55 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
     56 0x7d5deabd408de86ff8bd8c0fa77aaae49dc56b70      1
     57 0x7dddf1aec698bc8d28510b793dbc82bcd7b886b8      1
     58 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
     59 0x822daeca93e59bd604441536656dcfafd22e51a3      1
     60 0x82d0efe3e244d97d763ea17b66dcaeaed13ba679      1
     61 0x8403f9abc432ea851e1e681605ee27ab2e2adda1      1
     62 0x8450cf769ccf7fd060936ba2b023ca8f9903f9c4      1
     63 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
     64 0x8acf2f104a11fbe14a836c8904df8871b3cc9c62      1
     65 0x8bf824d7e8a2dbbe6f47bd09721a8a3fdce09d63      1
     66 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
     67 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
     68 0x8ce2b8f8b37b350feb8236f244ccb8ccb6988b36      1
     69 0x8fb583e71e0d05b063cf89a82997e90b0e26b7d5      1
     70 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
     71 0x91ff59d038c28edd0ec8ca7a667ee26d225d26a1      1
     72 0x92f6d408a4f90c225252de2da371638137bf6a54      1
     73 0x94020ea06109f487ce45174cfa4da4513d38e098      1
     74 0x9471f70f2518846f7a076636d64e5a22787da105      1
     75 0x962ad4a1276d2f985f8afabd3da59bd32e5c36f2      1
     76 0x9773cc2b33c0791d1f35a93f6b895c6ede1feb54      1
     77 0x99522c0f67d45c69e2ecd58be9671cd3675f3761      1
     78 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
     79 0x9cb79c20939d6ac3143910926754fbc67840be8a      1
     80 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
     81 0xa16977865ab1e2eeab1068e71890b36de4bf95f3      1
     82 0xa431d9ae84cce1c1c6d28f8258b5b95bab930210      1
     83 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
     84 0xaa1e92ddd28c835fe66689771d35f38947950fd4      1
     85 0xadd72e24a9e9117aa16d253cb421cb93b00240e3      1
     86 0xaf9cbe42d3fd186b5d0d04b0a1d424e6562fd7ba      1
     87 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     88 0xb2eca685836330080e672e87802a7c63f0bc16b5      1
     89 0xb3e37aebf26dc322857f66f5c889118ed6bfadb4      1
     90 0xb68e75f5424e4dd8b97fb42c4965d08718349d69      1
     91 0xb6cf777e3696a502107417265c92d1b075636a10      1
     92 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
     93 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
     94 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
     95 0xb8aabda1486b40aa761fe3c643e3bdd21649e6a0      1
     96 0xbd37494bdad7580c86d110cfa3a54295b52acd1f      1
     97 0xbfc3c19d3be0b0f54dca49fb136d08eea86a0229      1
     98 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
     99 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    100 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    101 0xc2aaec38c14b81332f6741f9fdc211c6bb6f894c      1
    102 0xc2d28150e302ec9c4d93468488be54062b4e8094      1
    103 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    104 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    105 0xc41aaaa3d4dc57c86ec4f4f1caecf1a94aaeec73      1
    106 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    107 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    108 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    109 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    110 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    111 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    112 0xca683e3dd02bffcf62cad1c4345f3e21de683da0      1
    113 0xca6983947efc848df45c9e84b279438e24727d2e      1
    114 0xcadc7fcfd1c12da24be866d30be91b44cbff3914      1
    115 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    116 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    117 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    118 0xce5993291aada8cb38d9014f626a405e0f1128a4      1
    119 0xd04fff37bfefd4b2439322620262a25cc17d0b91      1
    120 0xd41df4a30fbe6b282ed6294a704fb33090557566      1
    121 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    122 0xd6ef6221989578889ef0916c046e2edd3a2b0523      1
    123 0xd72cf23146ec9a9ef1a9db0dbb21f30ef7a76615      1
    124 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    125 0xdbe41dbeabb006df451db3a03736bb802a843733      1
    126 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    127 0xe1c7580accbccc3490345ed5c6833ec170200599      1
    128 0xe33723bc52bf7c13a29ef9aface30712c9826e98      1
    129 0xe3e06e9ec56ac1e7f3bced968b920d46ae1b460a      1
    130 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    131 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    132 0xe82725c3c97e0381b595b9949ae079ba3da4fdb1      1
    133 0xe874ba46982d7cbab5931a50c2dc81a7aeb80344      1
    134 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    135 0xe9e7f10a129ed34be0cab9e1fb0c8a309d3526d9      1
    136 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    137 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    138 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    139 0xeef7af481f91d23a8e9c05948aed6c2e209dec27      1
    140 0xef7468c7044e6d732927d58ba535b2ad30799cbf      1
    141 0xf06faa3828566d9ef4ca78727d032ec519b07339      1
    142 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    143 0xf4baba092bb9aaf76e0c03b856398b9ebed0819f      1
    144 0xf52a30142877e3b7d30f3f6bb2c6bc55b8888710      1
    145 0xf5501aa67b218ef340b23da0e7bcec77e70cf716      1
    146 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    147 0xf61f2de859eed38cde9f36fbdf3de03d84871f6e      1
    148 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    149 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    150 0xf9b16150b794a665f5348069751a1fc9e68de60c      1
    151 0xfc0c476530d9742cb116027c04559d0dc26bbd12      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 217 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00e87b69cf3686e028f96d14cd79ec30983e0f53      1
      2 0x024acdae4b00cf47430f4805ebff53b397f7ae83      1
      3 0x0282e055d3b2f7c0ad656952ac1bb989fb6d0086      1
      4 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      5 0x040df97224a2d65df277ab4adb58596a42a9698c      1
      6 0x04400db7f33d5330f62c8dbd4840920adc273615      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x0666028d0c4eb0ad6257b63e697590ed130fc1a0      1
      9 0x06f549f0e578018095e93325bfaccdae3ca21df3      1
     10 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     11 0x0843e80240f9304f3fe7059f25f3e1a66886a0eb      1
     12 0x0871deb34bfd2052b1c10dc4f6c0912a2a47e927      1
     13 0x0a8138c495cd47367e635b94feb7612a230221a4      1
     14 0x11360f0c5552443b33720a44408aba01a809905e      1
     15 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     16 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     17 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     18 0x133a123687c6c4d12abcd18a8dacf2b7871d9548      1
     19 0x17b1cb1ad28e8e8b038139e95cf6223ee7e8b572      1
     20 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     21 0x1800bb3bb01ec636480cb45c61f45ae34c97170d      1
     22 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     23 0x1b5c7c62d2c4b3e65a9ca49a17c7ea1b6d9df696      1
     24 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     25 0x1e9b5a63fc1ebeb6a8302f81fb244cb5c46fcec4      1
     26 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     27 0x21df37900c14bd8af6b12552a1ec2779fb978160      1
     28 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     29 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     30 0x255148be130862531729937152d82c51142a5d56      1
     31 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     32 0x295e8fbe2853da5b3c2136360dcb072672e90e1b      1
     33 0x2a76f7df64889a1f20f5b6aa87ebffa9a38ab925      1
     34 0x2b82ad50033363fffc3cf001743cbbbb83cdc11a      1
     35 0x2bf0246bdee6b027d2177ee84905c25b174fc622      1
     36 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
     37 0x2dbd9b95c185c8e40df7ffb5b0ea8e62c85d3bd3      1
     38 0x2de986a374cba9d56f062c4106063455779676fc      1
     39 0x2eae4738fe75c88cf1d6521433d30cfd85961992      1
     40 0x30b7b41e299f90ae78a11b764ad2804ab2bf272b      1
     41 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     42 0x31cd4492f926f3ba9df29d5fc5e2cdc5870250c1      1
     43 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     44 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     45 0x38a6133018714e4f21b914acace1cf7b41d4c574      1
     46 0x39fe6d7319433f4a7870644461aeeb21d9e2ae3c      1
     47 0x3a49309413793b32f6a308769220147fedbffa5f      1
     48 0x3a5e247429201d70fae0ff28e41fd8a1ca50af5a      1
     49 0x3aad83fd218881e391b65fdac31dc3cfc1da8cde      1
     50 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     51 0x3bd3ce01c82a12d7cff7c85a9e8bb27ae42fb548      1
     52 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     53 0x3d123ccbb151d403b9817be5505b1d0dba8ed576      1
     54 0x3dca4687174473c6c3e28a954b4590389d418848      1
     55 0x3f7a83dcbd24fe560ba0741f9815b368fcf86ece      1
     56 0x408125b99f2f17b90c7b30b2e9db7baa88e6b8d1      1
     57 0x40c839b831c90173dc7fbce49a25274a4688ddd9      1
     58 0x41146e2b9dfdfc47724a20cb86a15dc206c97e44      1
     59 0x424b3adf010edde7c981873082c61f44a9fee413      1
     60 0x437c3056e488b3ad705039770a8c9d7446258de6      1
     61 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     62 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     63 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
     64 0x4908b92cb86c6a660f3d291117236b94c565d4d1      1
     65 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
     66 0x4c327ee3ae154220513cddd0292d166bdb414426      1
     67 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
     68 0x512e63b6e620d4b413c20625c023165a3727acd6      1
     69 0x51e25bb517e9cfd207c9c733a1a4a915984e17cb      1
     70 0x53006f95def268f88dc1b8216654ab56f3afd052      1
     71 0x56b5822f18396c247f8065eb6d03b08ad0450469      1
     72 0x58d5605e30f18ea85c112718cc430b3c1c006ead      1
     73 0x59068075a799594db03c0255eed68e8e121155c8      1
     74 0x5959002cb524181d5526714c2804c3775212d823      1
     75 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     76 0x59d44f2d8e608cee2c3e57da5a5de3bf192a36c3      1
     77 0x5aac6d3c1fc68b2cd4e45537e22f4a0ea4f7ea98      1
     78 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     79 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
     80 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
     81 0x6584b48ccebd9c3e4413a1dd4cdbc38b2500cdf3      1
     82 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     83 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
     84 0x68a7ac13477aad590982293feeeb786a00276cf2      1
     85 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     86 0x6b93809e145d2301fb21d955e4241444f784dcd9      1
     87 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
     88 0x6e513ada916670389097752d05bf609d1246b4d2      1
     89 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     90 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     91 0x71eece7c9a6e68bed5ca071046cdccec20ff9808      1
     92 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     93 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
     94 0x7c2145e13c6917296d2e95bd5b1f5706c1a99f72      1
     95 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
     96 0x7d5deabd408de86ff8bd8c0fa77aaae49dc56b70      1
     97 0x7dddf1aec698bc8d28510b793dbc82bcd7b886b8      1
     98 0x80d762e5a4dc2bc237f594a7f696b5e216067036      1
     99 0x8177598e08ee8199c160a48c7a0af31ab54bb59f      1
    100 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    101 0x822daeca93e59bd604441536656dcfafd22e51a3      1
    102 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    103 0x82d0efe3e244d97d763ea17b66dcaeaed13ba679      1
    104 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
    105 0x8403f9abc432ea851e1e681605ee27ab2e2adda1      1
    106 0x8450cf769ccf7fd060936ba2b023ca8f9903f9c4      1
    107 0x8497277c9339170a0420e86da8352e0c084624cd      1
    108 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
    109 0x8acf2f104a11fbe14a836c8904df8871b3cc9c62      1
    110 0x8bf824d7e8a2dbbe6f47bd09721a8a3fdce09d63      1
    111 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    112 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    113 0x8ce2b8f8b37b350feb8236f244ccb8ccb6988b36      1
    114 0x8fb583e71e0d05b063cf89a82997e90b0e26b7d5      1
    115 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    116 0x9187804eed28934cc59049297162554a592e96f0      1
    117 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    118 0x91ff59d038c28edd0ec8ca7a667ee26d225d26a1      1
    119 0x92f6d408a4f90c225252de2da371638137bf6a54      1
    120 0x94020ea06109f487ce45174cfa4da4513d38e098      1
    121 0x9471f70f2518846f7a076636d64e5a22787da105      1
    122 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
    123 0x962ad4a1276d2f985f8afabd3da59bd32e5c36f2      1
    124 0x974c16605e95d7c742d0a4ba79bd8e13ce4732f6      1
    125 0x9773cc2b33c0791d1f35a93f6b895c6ede1feb54      1
    126 0x99522c0f67d45c69e2ecd58be9671cd3675f3761      1
    127 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    128 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    129 0x9cb79c20939d6ac3143910926754fbc67840be8a      1
    130 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
    131 0xa16977865ab1e2eeab1068e71890b36de4bf95f3      1
    132 0xa431d9ae84cce1c1c6d28f8258b5b95bab930210      1
    133 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    134 0xa71c36cc728706516a50605bb873cda1abd4e927      1
    135 0xaa1e92ddd28c835fe66689771d35f38947950fd4      1
    136 0xad9e9c4e50efec42432f6aa70ff52f528da94105      1
    137 0xadd72e24a9e9117aa16d253cb421cb93b00240e3      1
    138 0xaf9cbe42d3fd186b5d0d04b0a1d424e6562fd7ba      1
    139 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    140 0xb2eca685836330080e672e87802a7c63f0bc16b5      1
    141 0xb3e37aebf26dc322857f66f5c889118ed6bfadb4      1
    142 0xb68e75f5424e4dd8b97fb42c4965d08718349d69      1
    143 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    144 0xb6cf777e3696a502107417265c92d1b075636a10      1
    145 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    146 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    147 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    148 0xb8aabda1486b40aa761fe3c643e3bdd21649e6a0      1
    149 0xbd37494bdad7580c86d110cfa3a54295b52acd1f      1
    150 0xbfc3c19d3be0b0f54dca49fb136d08eea86a0229      1
    151 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    152 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    153 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    154 0xc2aaec38c14b81332f6741f9fdc211c6bb6f894c      1
    155 0xc2d28150e302ec9c4d93468488be54062b4e8094      1
    156 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    157 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    158 0xc41aaaa3d4dc57c86ec4f4f1caecf1a94aaeec73      1
    159 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    160 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    161 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    162 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    163 0xc8dd40ba1bdb6a3f956904f02b14db24013b8b5d      1
    164 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    165 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    166 0xca683e3dd02bffcf62cad1c4345f3e21de683da0      1
    167 0xca6983947efc848df45c9e84b279438e24727d2e      1
    168 0xcadc7fcfd1c12da24be866d30be91b44cbff3914      1
    169 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    170 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    171 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    172 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    173 0xce5993291aada8cb38d9014f626a405e0f1128a4      1
    174 0xd04fff37bfefd4b2439322620262a25cc17d0b91      1
    175 0xd050c97e76f882187c781feb2e74345227e2f710      1
    176 0xd13109c75b49c9d297ec5c76cc6f409e401c8bc3      1
    177 0xd41df4a30fbe6b282ed6294a704fb33090557566      1
    178 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    179 0xd6ef6221989578889ef0916c046e2edd3a2b0523      1
    180 0xd72cf23146ec9a9ef1a9db0dbb21f30ef7a76615      1
    181 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    182 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    183 0xdbe41dbeabb006df451db3a03736bb802a843733      1
    184 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    185 0xe1c7580accbccc3490345ed5c6833ec170200599      1
    186 0xe33723bc52bf7c13a29ef9aface30712c9826e98      1
    187 0xe3e06e9ec56ac1e7f3bced968b920d46ae1b460a      1
    188 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    189 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    190 0xe659c74753c241943687d8931358a851f11312ff      1
    191 0xe82725c3c97e0381b595b9949ae079ba3da4fdb1      1
    192 0xe874ba46982d7cbab5931a50c2dc81a7aeb80344      1
    193 0xe91f0085f5c3e67c8ebd5c8f6e7f4d884452dbaa      1
    194 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    195 0xe9e7f10a129ed34be0cab9e1fb0c8a309d3526d9      1
    196 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    197 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    198 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    199 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    200 0xeef7af481f91d23a8e9c05948aed6c2e209dec27      1
    201 0xef7468c7044e6d732927d58ba535b2ad30799cbf      1
    202 0xf06faa3828566d9ef4ca78727d032ec519b07339      1
    203 0xf217de9e1442b1f61cee9dac2a07bea96d83e06c      1
    204 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    205 0xf41125ed2a8c74d54d71f8b6454af03e6e3dcaf8      1
    206 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    207 0xf4baba092bb9aaf76e0c03b856398b9ebed0819f      1
    208 0xf52a30142877e3b7d30f3f6bb2c6bc55b8888710      1
    209 0xf5501aa67b218ef340b23da0e7bcec77e70cf716      1
    210 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    211 0xf61f2de859eed38cde9f36fbdf3de03d84871f6e      1
    212 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    213 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    214 0xf9b16150b794a665f5348069751a1fc9e68de60c      1
    215 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    216 0xfc0c476530d9742cb116027c04559d0dc26bbd12      1
    217 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1

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
