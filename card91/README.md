
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:27036       Length:27036       Min.   :1   Length:27036      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :2                     
         name          
     Length:27036      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17121969 # https://etherscan.io/block/17121969
block_hash <- "0x6b8169ad3f5a0a9c4d3c1754e0666f91537fdfd962642589811a18f583e8891a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4447 

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

airdrop_fidel    <- pick(snapshot, contracts=c("BlackWhiteNights","BlackWhiteDays","TheAfterparty","TheCommission","BlackWhiteRedux","BlackWhiteGratitude","BlackWhiteHumanity","BlackWhiteEditions"), address_remove=address_remove,address_pick=10,address_max=1)


allow_fidel_phase1     <- pick(snapshot, contracts=c("BlackWhiteNights","BlackWhiteDays","TheAfterparty","TheCommission","BlackWhiteRedux","BlackWhiteGratitude","BlackWhiteHumanity","BlackWhiteEditions"),address_remove=address_remove,address_subtract=airdrop_fidel,address_max=1)
allow_memes50_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=50,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_fidel_phase2     <- pick(snapshot, contracts=c("Foundation","TheBeautifulPeople","S.H.E.","NaturesReign","Kaboom","BlackWhiteDropsEditions","ShotBots","BlackNWhiteEditionsbyFidel"),address_remove=address_remove,address_subtract = c(airdrop_fidel, allow_fidel_phase1),address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_fidel) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0fc94e98a5e15ce4f77eec5a5a9b764d65ab4196      1
     2 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     3 0x47975656e27b1d2e2c215b0600afd543c25b9a8f      1
     4 0x65cb2108eb9f1dd127bdff5eac452f971bd21cb5      1
     5 0x91a3ebce4e0d326539c6e2720f25449774cb615e      1
     6 0x99768daa30519851f224d0e4e47546bd1733c219      1
     7 0xc7cbf5e1a25d53b12f8ea53cd0b8ddd0c999e127      1
     8 0xcf7e084d9068634b94d18cbe979743a591f84e3c      1
     9 0xd0b4045c2bbf09f41e460b6ec938db540fb23160      1
    10 0xd62469de1d8da954cd5a054961f1393b4f871c9e      1

## Allow Artist Phase 1

``` r
c(allow_fidel_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 193 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00bd53553e743c004ee6d855493a47e8a138a01b      1
      2 0x015d7b05ad83fd73b7eb433a963bd98c439848b6      1
      3 0x01d6868a0d95aca185e00144c8dae7aa58e20031      1
      4 0x0205ae39c4d79c3cd106223e306142232c650480      1
      5 0x021806134145ebe5bffafaeb5b7915c7ec79ea9a      1
      6 0x024acdae4b00cf47430f4805ebff53b397f7ae83      1
      7 0x02d2731dba8769765bedff6272f326b0d00506ce      1
      8 0x053b366216a9fc4c6219b11325f50708267501b2      1
      9 0x07b7d08a4e33d0455886aea596f999e58225712b      1
     10 0x07e7ea24f934d70ef813c1aac9468e84c6803eda      1
     11 0x081ffb021d5076040e39d891a01abaab0ffb192b      1
     12 0x08b75de942deb591f0e943aaa9ed32ee90578e30      1
     13 0x096a2a700e1324ab2575b2cae44ee7cc0df89ad9      1
     14 0x0bed5daf7070108bd289c79ff4cfea144c986c3f      1
     15 0x0cb7a1134f45f084a060743908c468f916da9f07      1
     16 0x0d4b3eb499583e89adea793a1acfa16e41d2a73c      1
     17 0x0f6b0e0ce002b09b60610241ef2518974af0b377      1
     18 0x0f6eed39497beaa1ffac2b649e2cd211ef10322c      1
     19 0x0f88da751ef08789caab8062086b0acbbef5e0c1      1
     20 0x0f92aa8b715f49f32b62d51fff9e09f61d5b153e      1
     21 0x1069dbb0855bc6e5aa6ae5594e1fa4cd504224af      1
     22 0x13a3699c55feceb170bf0bec02aea8f2e6f1dc85      1
     23 0x1943e9cb9d9a352a3214032bf58b7d0020f977c0      1
     24 0x1b0b416ab0540ba7ca143da93dd5bbd766ee1103      1
     25 0x1be3edd704be69a7f9e44b7ad842dca0757c1816      1
     26 0x1c676b055c6d3f88ee6a7662a11b75abb652b067      1
     27 0x1c7471f01889137b32258b8e96b365f3e79a7b07      1
     28 0x1c8e4f45e77b8e697540c0d58e60b67aec2b57a7      1
     29 0x2121788d2d8838f832a4fb8aef331b9d704bdaac      1
     30 0x21e63b1c81b8c73b29b1b3b49b13ad3aadee35a6      1
     31 0x22ac056472751431af20d570fb1cba1520a7c590      1
     32 0x230be5d9194451ca78d6c8f995f2554a80086956      1
     33 0x23c1e1c64620d68643a70525e04e8b828b8a604b      1
     34 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
     35 0x25fbd6a2d1462e5a2fd122b2f9d1f1e14a25a396      1
     36 0x2658d1ce0939f84ef71878a2f1c4f2ad24391cf3      1
     37 0x2777129e77c1ccf6c6987117bacfadcbd573f3a9      1
     38 0x27dc66acd2a5ea322b75e10906812bd988ca07a7      1
     39 0x282a30ca247906913978b8ee81c8d132f1658261      1
     40 0x29d5cea7d511810f3ff754886b898fce16a6d8fd      1
     41 0x29eff9f9917ab2fea4bdab986b1ae6f95c6c904d      1
     42 0x2c36d622f710d60c2d0b44da56e88141969e2d58      1
     43 0x2ee4fc37dec95a8cd808c0242b0a1f683bfa612f      1
     44 0x2f20ce5b7c12caca0103dc7361ec5138773143a0      1
     45 0x327df5f355e174da4cce38293795caa8f6a7a203      1
     46 0x3548e60d3365b1e2ce8ad00fe4714b48a576aeec      1
     47 0x379f2c9ee2ec7e52993222ca95a067f20418cb91      1
     48 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     49 0x39413151075680dd51802af27b28c348ac38f9e2      1
     50 0x39fff19ec4d04cf52bbb96b38a9254de17600ee3      1
     51 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
     52 0x3b99cc55c357829fa8ac9bd21ab2ce43f4b56a9c      1
     53 0x3bd1b8da812f605b523415d548744267c42d9721      1
     54 0x3ccd78c99ac513b2a06e2dffa21b2d5afe01eb8f      1
     55 0x40b6a29438905bb54c00cdcb50e5a8fa48cdb608      1
     56 0x40e39358413e4e88d12ea2ddb42eaec8e3331cc0      1
     57 0x417e33e9c34ea04734b2bc9eb1d8547691697743      1
     58 0x44d55b72eb910937b44570e0a5c3f054dec28e78      1
     59 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
     60 0x4597662363ffb334a8cad253e3a5bd647d73cab2      1
     61 0x45c322172ec1b1ba2e0bfa632857d6b801cbbb4d      1
     62 0x45cc77ec1e05d3f4962d16d57e6d253d6d557efc      1
     63 0x46c72258ef3266bd874e391e7a55666a532aecba      1
     64 0x4a24e1d81d994f21b640c7009ab8d3d88d462a25      1
     65 0x4a5eee3398f7618ee94b740ed98dbfc33fe39e37      1
     66 0x4af694afac543c2586feb46a3fd2b3b9f3d94f7f      1
     67 0x4ba9ce953a2247866c899e1d43a5c8e612124e84      1
     68 0x4d7848a4635ac0c8cf0126b8bf1c4d87dbabc005      1
     69 0x4e1bab3bd9b274988ae7bb8746be9211f793bbbb      1
     70 0x4fd571ef37ecd361b76d0111d11c38e28dffde5c      1
     71 0x50520e497baa947d1030eee207e1e6426c53834e      1
     72 0x5053e7503dbd326084960967c428e9ff95cb1689      1
     73 0x53b3af62c5ffe969ca5c6eb3e2b3eb1fa1230e40      1
     74 0x54421cb37c79918b18825a7b86cfac235f5b5f17      1
     75 0x54eef9bcd9e939e1ff4d946e3eabebac1a482e5b      1
     76 0x575011835453868d388a8aeac7817f5ab2419337      1
     77 0x59a2add327fd2229a644721e68214949d4a2a490      1
     78 0x5acc46f67f8dda42f85d6dbccd44d8c75b959f0f      1
     79 0x5aef41f37ddaa3d932222123ae2132a255cee552      1
     80 0x5f0c9383e6df013c11c393c5ba5797aa888a6b85      1
     81 0x5f5b5a6b4661c2a4280984b3921ffd3d58bd42f1      1
     82 0x62d0cc156688f851cb032cc450afbda03bb5bfd0      1
     83 0x630c61a36a6d0616a306067eea69b439bab93a66      1
     84 0x63418dedd3d85efa35f1ce564a68869d8c9122c5      1
     85 0x6372d3c71aa13be4942b1c854b01608f97572e3a      1
     86 0x653b0c1836e271d7d9870ffa9224e3d7d37ee227      1
     87 0x67ee7e7ede0a58949f89847f62a5c0965f4320c9      1
     88 0x682ccd1058ad6547d919402ae4c6d7a4f820b97c      1
     89 0x684188e0344d3cba4fa9525b256b6ea035b3965b      1
     90 0x6bed7266a93cca2aba3ec4bf746f305bc99f2a5c      1
     91 0x6fa7c46fe0d528e2584aca4dd3032d4e0639ce84      1
     92 0x73cddf71b29954f393afab3e577370e7a4c5fb4b      1
     93 0x76e7db1a3dc0f5f4fc4d5c055c361db861e500f4      1
     94 0x77bf350a791cf3b166e0f38b41cec390d37d3f63      1
     95 0x78155675ccf17f0a8ec4f8623a08fa0bb623b5ce      1
     96 0x79344695e2d6e6d2ceac36eafb22d8c36b0b1a22      1
     97 0x79c0d9dfe084dc84eefd4ec352ef9bf76cc19796      1
     98 0x7a6a5d51a06df3adeb0e06e03039d77912a1f1c6      1
     99 0x7ba3d1c4f46516fb975fb012f7db04381188e907      1
    100 0x7dfa60256af8b9099739593e6b65f2cc9844ee3e      1
    101 0x82af7581909a9a9dd85c41180de73925c39fbf38      1
    102 0x8497277c9339170a0420e86da8352e0c084624cd      1
    103 0x8981a87033bc2344f2704ce3f8f0c5d1bf009dfb      1
    104 0x89a591cde66526765c2a58e09a0970bcf0d6df0f      1
    105 0x8bcb31e0d773c8f376f64e79e3f094ddfa677821      1
    106 0x908c3bae3621aeb8e0068f32d271cba80ba1e7fa      1
    107 0x90ce1770f872040fcc086d2d7c906019b7107d55      1
    108 0x91006ced6152bcad20decc9f4c1158f47c512331      1
    109 0x912f1b9fa4f5389caeff938f1f88220b78d10502      1
    110 0x94edd3529abd1d3e50bf6de9fa8ba0aba5f09007      1
    111 0x95c0b05cdf55b7d37cde68b51a42efdd244fa5b6      1
    112 0x96f818e40febb6135746ec0b97f561b6edbb92c9      1
    113 0x9885247667560796c88d385c609619a8de1ac1c4      1
    114 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    115 0x9a305848e8d25fa7631a4be8490d8e50592585ba      1
    116 0x9bdce45a073e42b157b808e9b5a97f0e467889fb      1
    117 0x9cc91312e37970706bca3097e5dde8dbc9fb33f3      1
    118 0x9d8d7220d060fd12ca33336b7239688e366327de      1
    119 0x9eeeb983617c1c27bb3f1b2cfcfcde018fa80992      1
    120 0x9f119ef5d53fb91500ddf064e80d6587ca2b5546      1
    121 0x9fdfcf5d1fcccdccc01613cac7e93ad33b7ac72f      1
    122 0x9ff099113c37e54af22e380328d8a3d93f027228      1
    123 0x9ff3d60cef60e835f142ce5e13a180c70ac89549      1
    124 0x9ff77d193c091ae350b8ce26d50d46e392631292      1
    125 0xa01701fb730631b6cf0f2e1f6efddd13241d11a1      1
    126 0xa05bca6f6d9c0bc2c3a597ac3f800eaf502aca66      1
    127 0xa37c028d16f4072da8f421da678a616347a57e36      1
    128 0xa3f7fe5f82ea5db2e8f163afb99494b3f0c278a6      1
    129 0xa4c17e127cc4180b0a2a8e80408cfb41548f696a      1
    130 0xa7d236d84a4ef528a1217b1f540c40ac05111862      1
    131 0xaa1c29505160181e493f4b373f0c3206cc2a32a3      1
    132 0xaaa15e56eba0b82a6156f2a6e3d149207b6d2451      1
    133 0xadc3c1ac7b171b5a2f7e58c52203c1af37e76ab4      1
    134 0xae4d837caa0c53579f8a156633355df5058b02f3      1
    135 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    136 0xb1decffa6f7f48fae36eef34cac37d939cd0ba96      1
    137 0xb347dd642e9b20be829abd986a3c16d3e5cf1ffc      1
    138 0xb4786b7ae950b8bebd9f070be75280e2a4b084c7      1
    139 0xb5a85c2e97d9644257333de56f0473f3bfab50d3      1
    140 0xb77925f795b8a90aeaa024ee0ad16113d76efafd      1
    141 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    142 0xb865f158793cf99deb33e304d9ea261a324952d5      1
    143 0xbcb8c532636f9b6b59ef417203ce60c1577bbe19      1
    144 0xbd3fd7b44ca24741db067ace1decee9b164e31ca      1
    145 0xbe0371968a14f7f151d4b34b7a3227281f32a51d      1
    146 0xbeffa23f69a125b8d3e2adc63700e056870e2774      1
    147 0xbf7a23a47cb36f4bbb84912227a0db2470b815ed      1
    148 0xc3de35bf9007b20e7e4049580dbc4e549f5836d6      1
    149 0xc5ee5b961bf790970f550fc3adee548f6ecc44e4      1
    150 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    151 0xc8a3840d6449619284036823ad2284af3d769614      1
    152 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    153 0xc9c3c89e3c40e40f8c002a8a78c6fd7f09dfff0a      1
    154 0xcad3dbcf4227db2a0a787d03104bfc62c8b8a200      1
    155 0xcad624913b39b0835c2231a1295fb298cbf3db24      1
    156 0xcc203f9c4e7a1a6eb4b2a3fe583ec44ac58d09e3      1
    157 0xcc850a4f5ba7035b62dff2620e563021849a47e6      1
    158 0xcd6f331bf025046287acde58cc0362d6d4e5f302      1
    159 0xce088f8e8676f8baf3ae94f413e96c7096238830      1
    160 0xcf30fd1d6e803a343d8fe8f1dbf2968a1d37f4b5      1
    161 0xd334e924e15dfcfff03045f671cf71ce65c6e0f1      1
    162 0xd3acb55e8a58a29139e9404dee5f0bae5a21c7da      1
    163 0xd8d40f04f851641792f3f9ef6f8bcd85715642d1      1
    164 0xd8eba85535c56c8dcc0a120b0a27951a55c275a5      1
    165 0xdbe35abd5e3abb8692d2d8239823f2ec43cc8d6e      1
    166 0xddd7a710da74f1d511f071af4d2a4cfcad46c88d      1
    167 0xdf98322a314921fc6863806dae62a157081b0a5a      1
    168 0xe002d40634465d0786e8e8645a6bd174a095dda4      1
    169 0xe173a3d72b9c0f4f3fcb2bf883b47a182ae14642      1
    170 0xe9976059d92d5bdf8464b55aa048c4343fa01886      1
    171 0xea02b8e433d73ba0a44762faf0254b0183e359fb      1
    172 0xea512c5e43b234cbfa01b1b816af17ae721bfaa2      1
    173 0xea69c215e664360042dd5d13e7b9119787ad8374      1
    174 0xeb992bb4e8c0bd9479ca173a8f07e64580f8bb79      1
    175 0xebeb3706fcf5968401b1ed0d76b834c9a99cae58      1
    176 0xecdc1c32e4b0bff00aff1d8f809bdd8b33a58969      1
    177 0xecea3d767c36b2a0fec4e4cdd38599b122a5a11b      1
    178 0xed98af55bf714ed4f5131f3625c59bc1506a2ce9      1
    179 0xedea74198803befad9cf17896a28d37a63381c90      1
    180 0xee6a9dafc13e3ebb6146fc97b2d37369a81e46cc      1
    181 0xefb4ddb1bf1542bce7655967f81803a69bf3d890      1
    182 0xf17c382e51d9acd6eeee4df02076227c81256058      1
    183 0xf19b975ab5b1ab459ee989f1875c80fd24359b4e      1
    184 0xf48c6a3bc94a34c8abcf411cd86966bea0930dbd      1
    185 0xf5f26a6041f27377d9488406f60a26daed4d4c20      1
    186 0xf61bb10076ac3dbc91bf21840f94b122027caf21      1
    187 0xf7b6409b12a540947688f054dbf5b629fb3fc7d8      1
    188 0xf920aa82abd9a6c020226746126d07ade50814d3      1
    189 0xfad7d46f2f44c2571260713a0154ff3ac4d0c02e      1
    190 0xfc0416f3d0ccaff12a16e38215c45742e91e4472      1
    191 0xfe64fb91afb33d3dc3ce49335ce302fdbc03ff36      1
    192 0xfeeebb9fdc5cbc32a305fe9320708bf167f7a7bf      1
    193 0xff31a9f206215f0d059f90f37fa357c36d479159      1

## Allow Memes Phase 1

``` r
c(allow_memes50_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random50memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x05b8a95852979273231c0357801e248f1ebf7e08      1
     2 0x0a6f270ff37facd07aa85e48add3e73bb8101686      1
     3 0x0c664c03eebcecb6c21e3b3bc77c9dffed5bd694      1
     4 0x16498ea32896f5f64298b891be6e232f1ce8c7a2      1
     5 0x295d960d752771579118c21a3d82f6c7cb3a06af      1
     6 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     7 0x39531d59fb7f1bbb0d47ab8b4fc6fa640f888a85      1
     8 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
     9 0x3c0fbce46ecc02f24e3916b368366300a1848ce4      1
    10 0x3ebae94838f725343f58102e0903a917d4cd4265      1
    11 0x428ba87cc89d457ea0754b7fa8bf39cfb53ed63a      1
    12 0x44fbae5935520647eb98115e1c2f09a0d642e2b7      1
    13 0x46e6aa05e0867d5f0feb749e81e005f5567ab317      1
    14 0x478bb542f7658d635abba67edb987806dff5b83d      1
    15 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
    16 0x513e0a472522e4bd8023c032bbf01e7fb1fe0df3      1
    17 0x5301661d5a55e3712537949c65b86ea72cf41519      1
    18 0x542fce2f47cbbbba00ba59f844b8e1aaaed1f84d      1
    19 0x56c31c02e723277750739f04963610bdc53ccde3      1
    20 0x5dd47005f8a429b228a5d30b5345a1b64fa78c0c      1
    21 0x6728ecda881529b46eeb630baa30dd33dc76b070      1
    22 0x6b9eb48af8d9d4ef58e3e37390f5cfdf6525f5dd      1
    23 0x72d037f63a6c782f85c059fc917a12e71b2e0c73      1
    24 0x752accb20c027c53f685d199762dfc3afa86e1ef      1
    25 0x7c8a0a4bab02b7c6928952fc806776bad68b4340      1
    26 0x8125174cb26ff90b2a3b7a47a3614ab4261bd346      1
    27 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    28 0x84b833ac214d8bc9d0159b5416879e2bb8cd876f      1
    29 0x8605cf36085697fcda20be5488df08f407f73430      1
    30 0x87193685a9428d8d11a862a89426800ee3c01199      1
    31 0x9576638139843c636a0522f3bff3a85ac7b07172      1
    32 0xa9e3eddf2616b3d2b142f439575a7811e54223e9      1
    33 0xac1a04679039a1718d3820fbc254ce29269af784      1
    34 0xaca470227af72b3db690804d628af2c8e97abd57      1
    35 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    36 0xc37016f3023d0218718ca22a423d9dd00eb58275      1
    37 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    38 0xcaaf72105cb8dd7a1234988a635e243e1621ad3b      1
    39 0xcf9bfcf8c34930933328725c8958f36d709e9496      1
    40 0xd393503d6be38401054135780f57cf308e230dd2      1
    41 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    42 0xdcfe06271a89d454ff3302bab78d564ad6952607      1
    43 0xe5e508c953114f8688b9d6072cb1196cf6487006      1
    44 0xeb946bfac0d78e7f0d12d53e5f5b890bff25c7db      1
    45 0xed19d89f9b6e7631eb12bf085e197ce68acd47d9      1
    46 0xeda4aeabae2d559e7c5aed03cc288bbf44a03134      1
    47 0xee791b90b9d03c177185aa7ea5df8fd855d0c0d5      1
    48 0xf5e71ddd19a51c83926e257c23c693387667610d      1
    49 0xf65c46b0abacce6f59066dcce9ad88bbc78019b8      1
    50 0xff8991b1fa6b84e141bde964d442ceae0348040e      1

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
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     8 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    11 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    12 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    13 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    14 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    15 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    16 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    17 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    18 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    19 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    29 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    30 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    31 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    32 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    33 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    34 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    35 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    36 0x69e68074f1aada957edd39c5eae0069973343f30      1
    37 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    38 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    39 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    40 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    41 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    42 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    43 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    44 0x82139687faae8a29851902783e02e699de0e0846      1
    45 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    46 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    47 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
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

## Allow Artist Phase 2

``` r
c(allow_fidel_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 444 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007010802691f26b78db20196183048679af57a2      1
      2 0x009bbb4cc28f8949ae5ee4eba86210b57aa57602      1
      3 0x0164735c5f5531940969cdc211e1e4ef70e6ea63      1
      4 0x02081f465b8bd66058d50ef1a985d0c7af695363      1
      5 0x035fdcf8114f4995118b06d2b1d12a2d6581cef6      1
      6 0x03e88cec58a29c52e27f0751bf47e52202c0a1e4      1
      7 0x0469cf0317a2464ff44e39313b0e2c8782879d40      1
      8 0x0503bf05c49f96faac2b0f5fd394672ed4603c52      1
      9 0x0512a4cb384a6bf5f7ade5065318a080ae5ae01e      1
     10 0x0575cb152919ba51349ba082f2aa7ff438b69ae7      1
     11 0x0578d60835c03952a291439365e9ab4e611a949a      1
     12 0x059a5e1eb49a78863bbd9e0aee4019ad908505af      1
     13 0x05a8aa0ed1e1bc598c23b415f67cd774b530546c      1
     14 0x06b84187c76ff53101de5459ae3d5b55c122b376      1
     15 0x06e56657de205231b6c25d7f303db66546dd0ec9      1
     16 0x071d693190e16110a5854e4cef93f70d4ea594f2      1
     17 0x0759068a53b4f87eab32df8c10b37c912d1e4cf7      1
     18 0x07b957db0167d0401e5001d7e3c7a6b3b628ee7c      1
     19 0x09404fc75de1417895f6f6ab74f56f0ffc1f221b      1
     20 0x0a35c0d624855cf97e8eeb1ab2b0b53c6280d317      1
     21 0x0a4c398ee1741b638cfd399e90ab4910589717de      1
     22 0x0b2095ed0377f44e7ee71883469a2983b8ebc96c      1
     23 0x0b3a18849a95779ed5c2234a97277b41268145fb      1
     24 0x0b7ffa88c6efccb6325a7b5b0825eac274aaabd5      1
     25 0x0c29f63bf648b68661bcaa6b13cd7f4695702038      1
     26 0x0c9ade83fbac4280e71d9e0cd81457f8351c7436      1
     27 0x0d11c569b0f522597377bd47d40421772885d513      1
     28 0x0d1e5b9b586a09ff964b1541a5274902f77abe2d      1
     29 0x0d2481f95d617f8f11c6215d9ef73200125ecdc5      1
     30 0x0e123560827a92eda09132efcc93bc8dae619034      1
     31 0x0ef539b6feb1b63d72983eea075db83bf6554c70      1
     32 0x0fa183e5e6a9e00d03b4cea47715896eb043c803      1
     33 0x1059c7e69c743ed1cdd10380fd3127536d5eafc3      1
     34 0x1128ac0fa232c6697427e3a235284bb81f38d32c      1
     35 0x11a9583750806c3f521254c8e930991cd6139b30      1
     36 0x11f130460633344183dad9a354ac0e40a45e5eb7      1
     37 0x13f6f1134d1e6db48756253f1796916962f67eb6      1
     38 0x1418ecc087ed0b5e1d24e62e7e7ea6a9cc620c2b      1
     39 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     40 0x1428f693ff1fc51a997ca13ddae3a812f1abc1eb      1
     41 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     42 0x15ef9bb5d59fc204caab300d0c23d28da34e3535      1
     43 0x16722731f337609fc2554bb606170427b6456a51      1
     44 0x16bcfd6007ef8dc2d94489c52addbec26a93ec63      1
     45 0x17835a73fafd1246c6edcaaf86b7b8a46fa064fb      1
     46 0x18fed07f929bbe96a6055aadad18233dcd5d1344      1
     47 0x19006e3680182c96ca0fcdc54e2c095b766b0777      1
     48 0x19bc4d9ca5238f49b9192d51532c2d05cb267a36      1
     49 0x1affc07ed34a188d3517e2cc6141f5586c4c4005      1
     50 0x1b6e58c8904f320d4f55e17704a9fe05f50bce47      1
     51 0x1bb68c381b2b10215030e4b10f908441d2c2e3d0      1
     52 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     53 0x1cbc7d3b0542bef6f7a532debfa9864b94025cdc      1
     54 0x1d7c23f37e15dbf275df373de214783be8a65ec4      1
     55 0x1ddf0fe04a7b328c1b1ccdba222c1e8dbdf94e4f      1
     56 0x1dfc27e6c52a8f64097e2970ae7f33fa30d525ba      1
     57 0x1e0b0b57009fc831ff4ac6666d85c458533eb32d      1
     58 0x1ebaa0c708d11ff835249d16172952d5fdbf4af2      1
     59 0x1fec04ffa71ab633cdbc9780dbd1a3f431511597      1
     60 0x20b39e410043bc90a3345a26e84d2d1e7559037c      1
     61 0x212c73f5a74793f96a58b6592db52a216b41ca71      1
     62 0x2154006e30c6834706e701d82e3d0889b373155e      1
     63 0x2164d6b2ef215a4f46912bdd18f27a25041ebe29      1
     64 0x21e4a16ea782962b1877f9014a165e3c330bf709      1
     65 0x233c4ccc47aeea13d4036abd4183c3752b62a0da      1
     66 0x23c37b17f87c5033c160f113e7eb65d9a9b857de      1
     67 0x23d137a6c9e511d10c9f9736a1d5e0c347ba0de0      1
     68 0x23d610243a74f47fa8d8b7f8fdf73dd4989f4dc1      1
     69 0x24b775e0d16ed56c74e7140d0dc7e578e971107b      1
     70 0x25375a8513cd1d7021f9e2ac75b11fc57b958aa0      1
     71 0x25e61c470d59814ba5067431bf086cf2810622fb      1
     72 0x260f47f318a9db3d3148d5892d1dc95f0c68ebaf      1
     73 0x26f2503f042490994a1363cbf6cb354dc968daf4      1
     74 0x273d6f310611d1fa990850a05d306d49796a1686      1
     75 0x287bef2b56fbba046845898fe6c9ad15dce75c6b      1
     76 0x28a33a73c2ebd848aeda3792be629ae1addb4e7a      1
     77 0x28bc82be4681a77141acdbebb6dd3f0158eaed43      1
     78 0x2a310cf4f3257c2fa708a1b92570df8ea46eb80a      1
     79 0x2c41782073a2d3d49e3992f7f49145053fb28c6d      1
     80 0x2c87dd7ce631d9ad3121f2dd313780dab2f02ea8      1
     81 0x2cb99cf52a8a50625c5ad45b2f7f464bb259f0bd      1
     82 0x2db7eb00e25c8c34cc232c2094aad3f9c3828b19      1
     83 0x2e407a65dc925cb9884cff588e0ef1814e221466      1
     84 0x2f0607aa5d12743aef4e8c642b5d94374588558c      1
     85 0x308bccff44490825f9b66e952475e32735180258      1
     86 0x317b18cade045e34d63b04007fb7dd8697fcffb1      1
     87 0x3215d92f7f2a9f135df283202bfb713c7cd572d5      1
     88 0x3281465b77bde60e7053f0a941549b8390b5532b      1
     89 0x32a8d92204a68f2e2b5f2689da291fcec988f62b      1
     90 0x3319466e1007723740a9db76b18d0debac9459d1      1
     91 0x3332e26e91bffd80428b1a894520f624125ada32      1
     92 0x33929665820e1e66b53a3480722fa618a172a145      1
     93 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     94 0x33bfd3ba3bd175148d1bb1ee87e865253fd265e5      1
     95 0x33f8c34e142b77f67f6ead8492074744c7910a44      1
     96 0x3437aaad1646ebd15fe5b30ca861f5686289767e      1
     97 0x3472fa1a1067bce3c74dd66574b271742a1df8ef      1
     98 0x34da36c73288fccf24bc9a1954c2dcc6c07af928      1
     99 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
    100 0x36e40e5c762730b4f6d7e8fff1464b9949537fc9      1
    101 0x379b4ead1e51c971b2f4eba8e7e457b443dafe40      1
    102 0x3a01c4986916dd19b302f8dfb666fac3dd30f907      1
    103 0x3a82b6e85c37b46fc1282c955ac233b6ce2d6425      1
    104 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
    105 0x3b1da043309de11c58dac634895f635d3cbdb239      1
    106 0x3b7af94161cd8923c6610f3da38d1a6b8fbae8f7      1
    107 0x3bc7a3bb4649e9a3341a867a790100fe28132c50      1
    108 0x3bd96c7dba428e33bafd16f59fe4a3c6bf7c336e      1
    109 0x3c28dac79d3f1a393412c159253af276a850420a      1
    110 0x3c4fbefc1f215d7caf8c393efa1f9d54fdc62f19      1
    111 0x3c5fa9bd10e826e6af97905592afaa24a784957d      1
    112 0x3cd31cad695a4acc223d04a095b59ce5d10c903f      1
    113 0x3cd8fbafc740a4bb24bf558158b241f996f02642      1
    114 0x3d1000342da006b5187a74007efbfd1de71b72c9      1
    115 0x3d1df78367d956c4fafc766cfecb9fb2a7fc479c      1
    116 0x3dfc8afcc5c319ad5e1008d34f9b49fe8073687c      1
    117 0x3e388acc80bdae4d9d4781db9c2f616b6b1c73a0      1
    118 0x3e8b095a111d6751652d691d3b678dafcd932d23      1
    119 0x3ecf068b735c6b5cc7597e72ceced9bded196b82      1
    120 0x3efb47da7f6f0f440076ca2711af4348e50f4902      1
    121 0x40c20cdce81fe67ad56efb00f3d4b71551df2527      1
    122 0x414a30e85c94137fcd381f4b37ca5b1d78411b64      1
    123 0x41b52cf255d0708b399ee6a48253e45aff929b60      1
    124 0x41c81295975ed09965a2bedbf99722ee66d850db      1
    125 0x420b66668234a5488d2c4d834eb91ab4221e3ed3      1
    126 0x422263976a305ae639ece9da4bfbc41b697058f6      1
    127 0x4228879aa4e67ec9436446953888d1155d18be40      1
    128 0x424c57b610faa50afaa38704a240d5966d341339      1
    129 0x426cb7b474f417be388ad148addbb0954b1c2d0b      1
    130 0x429b260122b850f8691c73912f4a1f10f8ce9a38      1
    131 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
    132 0x4302bdcc5d1e865ceb78c8ad39915cb4cba12fc3      1
    133 0x443b04df6bd28f5b94a6ca1ee52992e75389cc8d      1
    134 0x447f32a96644bf414925c5da108ae5659b196ef3      1
    135 0x453954a7a4945ea5049f61689f74dfc591576059      1
    136 0x4557ebc539137eec5ce6b76d80aeccad6af0aab7      1
    137 0x455b2166f63567cf67b013964108a745d41e32f0      1
    138 0x457576b2408cca7acca4047dc5a9680302e39591      1
    139 0x458adbbd7fd10aa2c193a53d928e8cbce668365f      1
    140 0x466b8086c9d42802353ad1c559a2bcba51c4db29      1
    141 0x46b1f27178eb81408dd1d5f6f3824f39d46fc597      1
    142 0x46b83de30a99721b41d1f93fa7b1bf40c343a40f      1
    143 0x47062d53fb6cf7686477c400a3c9eede9a874010      1
    144 0x47574a8cf0e8fdfe0c774229df63179ef445114f      1
    145 0x478f791291c169b897e70e00faca877ecc3db6b7      1
    146 0x4808d5a2b6423abc0060465fdfd338c58e758690      1
    147 0x482eebfbe8e1d5da0094d0f11864b24a6006f5f0      1
    148 0x491fd53e5e0d8b4a5f28d008856060cda5380aaf      1
    149 0x497ba09db3da9ce111cfb1741a19faa384bd9b1b      1
    150 0x498d18b5b3c0b7f56635f3f52d4c466618ed21ad      1
    151 0x4999ccfb826ef0a5ba9d0039ca29ae3c7378855f      1
    152 0x49ce497f2799d244827c9773e5728f234bd6c1ae      1
    153 0x4b44f1d47fc0184293a85d5b0135bf12ac29c451      1
    154 0x4c3cf2c272125361733f559c8468665331b41d45      1
    155 0x4c49977d285c29a6d7caf3f1fc563103e79f9010      1
    156 0x4ca7d8d396ae9e9a5014285c3bf9db43da737825      1
    157 0x4cdb8fca4305b64af8057e5e485a87a014005ba0      1
    158 0x4e000a7382ea82584b0659d87c3e5030bb022006      1
    159 0x4f763999c10e6d3b837707ac946b998ff4fd86f9      1
    160 0x4f845ea0e5ce77b8711037e05e73d6c14547f0d5      1
    161 0x4f980eb8e0c2723477e29044f72fcb5964241a2c      1
    162 0x5033912dea959de023819bc4e39093df966af0b3      1
    163 0x505b4f873997b8685596b9466404caec175aa5b1      1
    164 0x510e55779c4dce3f76c54c298319a80b8378edbe      1
    165 0x51b00ca3fbb5473fa20e825285c0146d99d8f756      1
    166 0x5292d2e181436c2904fa0648b6466333f75e7850      1
    167 0x52c768f8574ebb07644ff2364c7cab3cdf66ff82      1
    168 0x52fd624e3c42c33022ccca829b8d58f98b8dd422      1
    169 0x551b543639d5d89e7e655556b5f0fcd26b1f867c      1
    170 0x56640ba14fb770f1ed151886ac7bb6adaa96955c      1
    171 0x56c675575ac3b2ffb55595c6ec54cba8931600e7      1
    172 0x57f908c42f5a0e5dbbd3c35a137cc242cf89add6      1
    173 0x585ba3bd0be3898d6f0cecfc0d52ae82f51f0c60      1
    174 0x58744ba154e5dae2d027a1675ad8a52bd4ade310      1
    175 0x58cfabf524c46d43f1235e08ab8fe5e74cc8fd14      1
    176 0x5970c59bc7d265b990b66d69f4eb90c28b08e547      1
    177 0x5a19b457037d0f6ea84d63605268c9b339c1301f      1
    178 0x5b046272cb9fde317ab73a836546e52b1f2d81f3      1
    179 0x5b38576cf213064ef9582e2b90ad30adc3de6add      1
    180 0x5b4a937b81f197d93908c5f0001ac7d729714e89      1
    181 0x5bbfbc4e659e40fd281b3863f138289ccba74c52      1
    182 0x5d5641ffc02c05391d2588e18167651e01abb22d      1
    183 0x5e15052dfa1b27dfc5985d51d51dc0539c1c10ca      1
    184 0x5e4b41b770a8e2bec9d6dc294f09ce10c444a2b0      1
    185 0x5e952a2bcce9d5f6a7426288b1de214599cd7779      1
    186 0x5ebd388330b306d6e10b16dce93220a24cbb8203      1
    187 0x5ecc8919024828855b8c1dcc418cb7dabfda9bf6      1
    188 0x5f73611539deb6533b8b5a3dba4d5b2ccfe24852      1
    189 0x6007df9ef81359399b3242f1bfe0b64f33ec9ab9      1
    190 0x60be12ba88c4986e56b6c8d28d69e457199875a0      1
    191 0x613bf31b822c7c8c1f44fe854935ae9b7e2cd1cc      1
    192 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
    193 0x6215e1f18f9def7539003140939237a01a6020c0      1
    194 0x6280aeb92c12ef44e04a053d3d23e563b4217382      1
    195 0x641d629d72027e1b6d88991b7b52dfac2767536b      1
    196 0x64b78c860ed090da88159f1bf1120f322989b6ba      1
    197 0x651db4c28970a5b8c2f6bb343b2c367f62e4dc71      1
    198 0x655442965667d9569015f850463224cd6a895771      1
    199 0x67353d0d86237b69eb521906f7e05b15b7454c63      1
    200 0x6741e11b56a87cd960d76b05cb2b7852f6990dcc      1
    201 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    202 0x6a1a97cb6643b6f144300339c4fca7edd8dc7a77      1
    203 0x6b3cd32a261c778d9846bdb7bf8574e1fa4f2af7      1
    204 0x6b6fd3fd1dd23c04b6b7cd5abce39688667564eb      1
    205 0x6b983ad223dcd216c339cd1c3102d0e5f2b61674      1
    206 0x6badc7b3eb76d04ab357569244be7281e9a4dc50      1
    207 0x6c4c3ad5aef4384d4440f5315434f55a2d6c79e1      1
    208 0x6e04e0f8b04c8d50c105e8a6b62eb5e9e2c8bfc1      1
    209 0x6f16717cb8972e8441f6185d9f8f9259a72e768b      1
    210 0x6f4ed5952e0a76e792e80698e9df47c477c29770      1
    211 0x6f85a819e5a739105f652ee025d0910803cfa744      1
    212 0x700e1c20fbb2a273f7dec0cec6333f99b9141ed3      1
    213 0x7017f1cc2cfcea0a7d670e7a45361b615f578446      1
    214 0x707b503a5a3ceb5543e101c3763de267d11c24b5      1
    215 0x71a2ddaad83cc50705431f956f341a7e209c6046      1
    216 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    217 0x730a2b599580492ee02cbb9cfd4c0424c67377d9      1
    218 0x736c38958622ccbc6d935cbfb183beb7d79d1a69      1
    219 0x739d3de2fb4d78ef53a4bc9b555ca9fc6799b1b3      1
    220 0x73b7c1fc8ad7197bac326fd87bfc064abeff91e2      1
    221 0x73f877c2be845faf12243b1b015a446776acd557      1
    222 0x7411f5ba7a20e0d353942462860a47c557e35f6a      1
    223 0x751fa7d126ef37c5b1875f91cdc6fdf9bed41e67      1
    224 0x765fcb06c17a40b87621b29be6ebc61a9773b850      1
    225 0x76811e6575f8b247ae49604e764ac1e033c3623c      1
    226 0x778fe26477cf3cc45389c4abed651bdbcc03ff43      1
    227 0x7891596b9c9e6fe809a81ae56e39a5e710259bb5      1
    228 0x79ff435184674986312275a031999d689be9d104      1
    229 0x7bc49ff7d9554f00b811dc9534576266e03f64ed      1
    230 0x7c384ee61356dc5081b0bab2437f5bdcc40fbcde      1
    231 0x7cd5d741ae97a7fb6bafe39ff64474ad8e1c7b34      1
    232 0x7fc0d310c72cc0dc845c4cab53ed5d9e0205d43f      1
    233 0x7fc300f7ba61919cf45969c57341122947f99ab9      1
    234 0x8028bfbbb68ac004e0704161cd080409a8cf2950      1
    235 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    236 0x80cf63b40741e041515e6ba6a4d327088540c6a6      1
    237 0x814ccd3404dbafe6285a0f63b12d2ac45ad86b5a      1
    238 0x8160f2472d3435be3f6596ad507d2d4c28953e7a      1
    239 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    240 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    241 0x825d18c3ec1d54f61f463f1f97ad070f1a876f5c      1
    242 0x846c13feb4b4e71772905e0a51b10fbf640f555b      1
    243 0x84c714b82187af434856f775ce088cf9a2070c9a      1
    244 0x85f14052e804d62a5c484523425e11db506975dc      1
    245 0x8648aaa69c55e23780cf4b189591d6e56f2a3536      1
    246 0x8709f3c5c69b4196783883d1675a9cfc538e84d5      1
    247 0x88ef4c50b3f6c91dd34805ad337de88d30ccfab1      1
    248 0x891e5af31e648eab7a87e956d36572190a642bb5      1
    249 0x893203e9cf0d4a5dfb446e723711dd5d46df8604      1
    250 0x8c2c66405a22e3b4bc0a8f863b7a11f04d796561      1
    251 0x8c3dcdf58083c18fce9ca97a3ce2c6774d9a8a4d      1
    252 0x8cd32534e41a1aec03626c9680ece89eeeb9e9b0      1
    253 0x8ce69f3f36465366de7dce5fc2137ebe37a24122      1
    254 0x8df8a54488c4a5b92c50048b9e2160d2f34142fa      1
    255 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    256 0x8efe64b02878c097f9e49ad9043129350bfde171      1
    257 0x8f1b8f21ce954e2b1c3ccd962093a53c69a303fa      1
    258 0x8f30f459133abfa3ad40842d7887f4cf01d7a957      1
    259 0x90bca73e72322b1affaf211a631dbc1483081f50      1
    260 0x91be179d95fc62e91ce65305df94785d04bfa3cd      1
    261 0x92058906c65bc9cf1b77f2f76de0687854cae050      1
    262 0x92993440e4c216fc7c55f9beac9d203b1984e74c      1
    263 0x930bdded3fb39ed97aa3d4a23738295774ae5f8a      1
    264 0x938e202f15af8c8a782588262c3ed0565a7f9411      1
    265 0x93f5b00b244229f74dbee7f885b93be6b2f3f9ea      1
    266 0x95e8360699191a3fa48cc211e48f1f70f1f3a8d2      1
    267 0x96f85671a7cd0e8546ad1917c5625975641c26ae      1
    268 0x97636c21c73da46d4374dd222de4bc849b8db297      1
    269 0x9996e496a36b42897b44a5b0df62a376c3390098      1
    270 0x9a52b14ea7049850796c27062df5da8fbb99080d      1
    271 0x9a6b26dd292a136926d7c97a3c7ef72f96cd7fc6      1
    272 0x9b6124f1dcf8e6c9cc317b6b4d26d67cf058eec3      1
    273 0x9bf10a9a1254edadef6a2a4a34e51b34a5928cf9      1
    274 0x9c2b5e674e0e46991d4c7a7d5e4acbd7936c3efe      1
    275 0x9c499c9516e152e2c574f04e4100cfdaba698505      1
    276 0x9dbf616eae9ffae9ba0db327ede2df05b04f6111      1
    277 0x9f98db8f45dd211c835389ac2c7489537391c3bd      1
    278 0x9fe2a1733b00b36ff4ca2c2883940e4d92f6e024      1
    279 0xa1a8a29c4dec5e676ad053c52fc011a98124dcae      1
    280 0xa1c2018fbed0b56d3548aabe70a55c0a8012210c      1
    281 0xa220b76ea43722dc04344894c3b1f606049b46b7      1
    282 0xa29597016513333ad8da8c55dc990256082d1485      1
    283 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    284 0xa7b6c6f60a7053b812653d538f0e2a80e8fb04be      1
    285 0xa7e275e987382cf363bbc2aa120645745d20d069      1
    286 0xa89f52874836555c0dc5e183e4d1c6f04c0a3f07      1
    287 0xa8b3a54d79cc8b3d35a581fae518177149739f05      1
    288 0xa97f690e11d65ee0c3b4e7ef9ca3995375b97f89      1
    289 0xa9c28eb83763d33ebeb279109f1b3913774fb01c      1
    290 0xaa77fffad72ace7cbaff69084201ab6966187886      1
    291 0xaa8f7ab21dd397720b287962c219e065663cc401      1
    292 0xab603d02b3cd369a17159b3eca483882f8bea540      1
    293 0xac159f82531aa004885bce8ab0bf160007a72735      1
    294 0xacbb94170951051d9ddceb0caaa2e95c76570e5a      1
    295 0xad29dec528e3463a9ac69d1efa60979dddb52806      1
    296 0xae9934cdb0d403b2247378bf53b597faf7655bb0      1
    297 0xaff4372a4ced3bba7858a7072066f363be2be078      1
    298 0xb07238615d6f11f8989ffb29b3ea30b4730663ea      1
    299 0xb174495dcb1608aebf66fdbe8fe2c02fbd759c06      1
    300 0xb26541d4148eb559a4be102b27d284695543f778      1
    301 0xb272ee9411bb3f43fe477815440075c44367d061      1
    302 0xb40030153c069a4dd5ac06a974d08b08cdc1be23      1
    303 0xb51196f0388be4dfbf3d73edffcf3f52c1aeae0f      1
    304 0xb5589dd7f9fb0c3da5724221e39d651f0ce8203d      1
    305 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    306 0xb622007d605e71294469536ad334fc7dfd6c0bb0      1
    307 0xb86392996b0748e416c6de5396cee4162e757f9e      1
    308 0xb86a670b14a6bc329ffe16561b7b295fd2e41083      1
    309 0xb8bed200829990863051875f29c190893c5472f1      1
    310 0xb916887d50a9b044f8f953154fb1db2b6f02ef55      1
    311 0xb96137d88aed51bb684f1575b748bb63426ba058      1
    312 0xb9b17e34baf5fe755dbbc9b04a3dbdb66b1c044a      1
    313 0xbabffc0d1d242ba458f567f9ab100e39c7dbb098      1
    314 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    315 0xbafb6bef1c3bcce2e8814de13703a06229afcb51      1
    316 0xbb43ad94df990fa4983a8808e88d8d01cc64c0ea      1
    317 0xbb5e3399daf10fbc5094e3f4d3b9aedb7547856c      1
    318 0xbba0b4b1abd7f28a6a2406b212681df8c3fe4bfe      1
    319 0xbd20eb0341aeb3de2bc417d607faca515122868a      1
    320 0xbda89025bb87a7f3d55051d0dd6bcc7d8955672e      1
    321 0xbe147cfc76ad88440682f429a64383bab3f12680      1
    322 0xbe39ad6d10802b6ac0943eab3e6c6b1884a054c4      1
    323 0xbe63999e4c43be8efd030e36746f6bd187743493      1
    324 0xbeae60c31df0de3c69dbc7914711f264e85ba9f1      1
    325 0xbefa7bdbcb8221136761ca67be3709751156eb82      1
    326 0xbfbdb4574eeddd9f810e1c993a6c1ddebd6fe62b      1
    327 0xbfcf0663ec8eabd2090fdcb36534fc8352bdc042      1
    328 0xc07a9e0f539284075d336dcfee98b4c5c8243b3b      1
    329 0xc11a63c3adf4da3fa84d95f77e23307c29884a2d      1
    330 0xc2547446c439be477dce96702fb1f9dfb61bab3a      1
    331 0xc432b12b53087b41470a8566909319be55c8351f      1
    332 0xc4ec5fcfe2c231877c80b8cea5ba80a73adbc7bf      1
    333 0xc50872a182916ee706f812aa5c5ab8ee0c787dbe      1
    334 0xc526460b16dd67cbbd13f7fbfcaf4f002d0cf383      1
    335 0xc59af5b5730fd0d7541af26d5e7f9dd13a514947      1
    336 0xc5b35ff8dc087d09a9a802dea8efd8dabd88ca8c      1
    337 0xc6411ca5a80deb7abc0827da82e2ddb9c906614a      1
    338 0xc642985464c82b86d5127f38033070bb786212e0      1
    339 0xc6c3752bdab737b8cfa174e6ecfa620ec0cc162e      1
    340 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    341 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    342 0xc85f395094c0b3e2b0d429fef8e0d5886289989b      1
    343 0xc93c7f71581dfeaab59bed908888dac5689f312a      1
    344 0xc99bca6c4eb3fd8ccdc3899280cdcdd5d9e7df4e      1
    345 0xc9fd2496ccffca7dba8b457d51ddc8f875222135      1
    346 0xcb93518a3a2aae499e50dcd978c55ac512928308      1
    347 0xcbc37d1120d7fa7013bcf35582cb12ea17b2bd4e      1
    348 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    349 0xcc4dc565f6fd9d6dc3c1312355b852fb0c66630b      1
    350 0xccb51071f1741c32fe90754d21a8549460e7d679      1
    351 0xcceacc20980952cb03b35019267467bd67141b48      1
    352 0xcea88107f6e95881e9c8ef6d81a3ba16bb2c7fef      1
    353 0xceddb1dc65ea17e9da256bae539c688a7cb2c7b3      1
    354 0xcf05e57e96efc3cd584152ecc7b5b9a0bb3c47c9      1
    355 0xd01a0a1bcc88320abf30f1d971d9ff66b4b54fec      1
    356 0xd0c1c0ae1e9f0056a3814c01e78f48dafe6b14c3      1
    357 0xd0d9597369fc67b9eb2703d6e9ce5fac0a77f33b      1
    358 0xd143d240ff50cd14942d842247ac1a11f9d3334f      1
    359 0xd211d53f9979655e9a1e74cb92436cca09c30fc9      1
    360 0xd28eda225b899480611bd3eeb0f673805c3af4dc      1
    361 0xd2f6c47ef429a57d072b9e1652ed3a68f8c7ba01      1
    362 0xd2f86cbed86db85c859903ff5f110b15d95b1350      1
    363 0xd3401a5d42a06165523499b3d71e6074829e852d      1
    364 0xd3e01898106cdc85c364e14b743e3c71cf5304b5      1
    365 0xd444f003f3b7a5d52d5d388eec1fbd8dbfb95026      1
    366 0xd514088abe24f9198a2cddd04a964e2f5c5e1789      1
    367 0xd514dc9996c400b58d5654bec86bcef6200dafa1      1
    368 0xd54d1376a86817f8bb382d4e0bb419c28ca6ac9a      1
    369 0xd5df3f5022572f461459fc62dbbfaa54191fc551      1
    370 0xd61a6fb2c107a02459f069a6701debb45ef21fd4      1
    371 0xd6dee643da993cf6be961f571cfaaacc444eccac      1
    372 0xd8d788f4baf491db505a496f9bd5c1d566d1c30f      1
    373 0xd8e70983780f3c0c0042626ec364a6e7eb151dc1      1
    374 0xd911c34256255ea1df33e4ce2cd207a30f012465      1
    375 0xda1ff596b0ab79d36cf0edeaf79be6b0c14228c2      1
    376 0xdad60277896e3577f2e4de935bb033a1010e69e6      1
    377 0xdae6b3ea322ea51d18a766f269cc86a13592c082      1
    378 0xdae6ca75bb2afd213e5887513d8b1789122eaaea      1
    379 0xdb9547208f1f2010a99387b1f6da8ccff82d8246      1
    380 0xdbd17b04d70e2e47914c880c8907654831cb6ff6      1
    381 0xdbf07056b17392264d27fd344ccfffa4679fcb76      1
    382 0xdc0b3127a71367227311f02fe4dda2a6ccdbae78      1
    383 0xdc3f185290e1a77e1fef5cddbec873d0781156c1      1
    384 0xdc8253c1628134347dc95976c0302e8cce5df914      1
    385 0xdd30e95b92809ad8261603bbd87f2e370ee96df8      1
    386 0xddf421665499d7caf40aa96092996d2b4fcbe95a      1
    387 0xddfe8fffa7b385df8d1575167b6d5fa6b9e65e65      1
    388 0xde246893514be08be3c8a21dc2080bd4cce48f36      1
    389 0xe1b08f07a20a2369a38f7e8eda07e634a50552df      1
    390 0xe1d6912ad1d47ff82a198e163e7620981e122e7d      1
    391 0xe212d0f608278fcb1770759f12af71640ba0421a      1
    392 0xe2acfa2f1c3dca055899caf07cde854f49f8d8fa      1
    393 0xe2ffc157dd90c98a7813de9d3b176d81f1411463      1
    394 0xe4f29109df1b22d6804e4e55ab068166b2310a6c      1
    395 0xe6512da31a32de181d555c13fca48598446d9b48      1
    396 0xe65435797497d5be25f33d70d26698ec32dd6a0d      1
    397 0xe73ac9d4621afcae41ff1ec38ddb50c0f0c98282      1
    398 0xe862ea505479295b9a85d8b49d89c548f4343a03      1
    399 0xe8b3695a2a883323c6b883cb9ca670f8cfe40dd4      1
    400 0xe8c269d79b71b5b0804d53b9ab0d77f0b94335e5      1
    401 0xe952af3ceb8df334a0dc5919c819da95f1d439f7      1
    402 0xe98223afd1db99d60480a7c2dac815275aab22db      1
    403 0xe98490ad8cf3240a94d88732ed819d2b20337d2e      1
    404 0xea1ae6983aab58e4d328aed1e3ef13d0b6063445      1
    405 0xea338eefec1e9e0f34b8646f8b6fddd71cf0117d      1
    406 0xea9063fb8485c5307662abf3723b204b7fde7dd4      1
    407 0xec5e8e92bda7411ceced2c2b8828c4527e870c2d      1
    408 0xec972f5feff6d89735da9a7bc6f7aca6d2ff6822      1
    409 0xed7621c7f25ed3d92ca17df6643559cafc11d088      1
    410 0xed922496c93fb30ed8cec921013215733efe5a19      1
    411 0xef185d878085455818ffb6758c9828e865699ecf      1
    412 0xef7a0e3788b7f897ae8f43d264870bfc7046a05f      1
    413 0xefca67508d92424bf76ca0ce7c07c0bac6f8200b      1
    414 0xf012b5afd31d8aa22b3b97740e41731e3e30f7d8      1
    415 0xf16bf1c992e8ff1011d87f7d48a55746f0966bca      1
    416 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    417 0xf25738f44135b1914408dfe5fd7aea637e2406aa      1
    418 0xf2d34ef188d20d36ad720489306dba86c8321ad3      1
    419 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    420 0xf3b1362861e99eda3833c32c7f993868ac3c006b      1
    421 0xf3dd89ce705348378793f43b5bf5339fb7edf178      1
    422 0xf4accdfa928bf863d097ecf4c4bb57ad77aa0cb2      1
    423 0xf4cb9abfa949d0e6a2b925fc09b06f8aa8c90639      1
    424 0xf55cbf08ae9424c572598b29064b2fc3f67add04      1
    425 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    426 0xf6886c191b0353cc5fb92595015521601d6c8b22      1
    427 0xf71f9e4f69721c53fb47a5ce9620edd4716bf4c8      1
    428 0xf73db6fadd067e7a201f267f9c0f77cd669dc1cb      1
    429 0xf7ceba227538f8dfb81f913e1651659be73274c9      1
    430 0xf835b7463864caa15ee7962d8f8d282721d2b42a      1
    431 0xf86b20df39dc2167508898d3acc67c9e9456488e      1
    432 0xf8b1f60f5291fd3b9881631b1a36f567e5b56c60      1
    433 0xf8c0c6b86a34eafbde860b1780704a843bf12471      1
    434 0xf8cc081d772c8fa0dfc2d5544b11777b84e4e2ad      1
    435 0xf9a4683ba0da8e1d0fe0751b2f9400563cc3876a      1
    436 0xfa075e0967d2f8dd109082ee63339900f92d3db5      1
    437 0xfb40aed4d996aaeb566c3d86e52a86afa7a608de      1
    438 0xfb469965ec425d5b83da58dd75a3a77ac06eafa1      1
    439 0xfbfab36b7f886e0e30e06fb8b8f24d5e4c892775      1
    440 0xfc075a5fc4ecaeb1ce66995e054bacd7c422f2ca      1
    441 0xfd1351fbf403f2ddb38cd37745f11f9ad0420392      1
    442 0xfe14c5409e7e95cf022f81027c120f2c3c0acf8e      1
    443 0xfe2b9d9283235e655ae21dee7ff98bbd60ffb711      1
    444 0xfef17b3cb3a89931e6a9cf01fd6a2dd344a01cee      1

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
