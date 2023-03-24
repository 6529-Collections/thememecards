
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:26362       Length:26362       Min.   : 1.000   Length:26362      
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.002                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:26362      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16889069 # https://etherscan.io/block/16889069
block_hash <- "0xd763b7fd21c3aecbc73aca94c53ceda9627602bc5782e1184986325b28ded3d6"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4751 

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



allow_luna              <- pick(snapshot, contracts=c("SuperRare","Foundation","Foundation2","Foundation3","MoPAr","OfLoveLifePoetryEditions","MysteriaEditions","FortheCultureEditions","LunasEditions","TwelveDaysEditions","ArtQuestEditions"), address_remove=address_remove,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memes_phase1      <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=200,address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Singles Phase 1

``` r
c(allow_luna) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 224 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x022d861f369a3c6202b44901933047592e6f60e2      1
      2 0x0231ebdb3dc5fd302c3b7b8e4d9056ba32be79f3      1
      3 0x0241dd5c847c103ea20802de8f127ce2c01a35b2      1
      4 0x0277f4d5a48823f9d261cfc5d10bd99e494b3493      1
      5 0x03a06b3929320a14ee0203a104927f726ec23445      1
      6 0x04a0911c638ae9977965b1d2504d5d61bfe2f61c      1
      7 0x04afa47203132436cd4aafa10547304b25f7006b      1
      8 0x0685560236cff0854a6afd33830fdef17448c1b3      1
      9 0x07c68b6062a884bc08a63ec332eb07d024f24d60      1
     10 0x0837c9dbd6e0dc2f1dcc1ceb4cb526aea17ea1d3      1
     11 0x087042b6e9716c2f04aaa45e48e7e87d63467184      1
     12 0x09c400217ef6278ca839c49b9e3b9908ff6d1163      1
     13 0x0a8c2ee08760251705f5aaf7bb0e7b490029bc27      1
     14 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     15 0x0b175a06961b0e11c73a3ac9a0947bdb6769cf00      1
     16 0x0cdd65d3e6e80da2e5a11f7c1cedace730372d7e      1
     17 0x0df2bbaf1beb551e89c099311274ddfc14a3f4df      1
     18 0x0e7a408a775af29310970de51b59501e21eee87a      1
     19 0x0f52c53accb80550341d9e3d7b028bafc5cacec9      1
     20 0x0fd8eb397c2aae7c35222dd6623e08f239cebf8f      1
     21 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     22 0x11f6ff77722501a04a31688dfd0e324a7edaa477      1
     23 0x1275651c38b2fc6663fbfa28974d74ea74bc89da      1
     24 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     25 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     26 0x155c18fcdc5f593728c5d5d158509271835decd5      1
     27 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     28 0x17b128b6771f5d26de3348db0cd222328cc1eb47      1
     29 0x19461698453e26b98cee5b984e1a86e13c0f68be      1
     30 0x19bcedef9cf94283a1b005ee1a491261e7d70f9f      1
     31 0x1c7471f01889137b32258b8e96b365f3e79a7b07      1
     32 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     33 0x1e29d4993be9aac1e421a7eeae175af7835e30c1      1
     34 0x1e62e441d2e1d83354aa811464236625adf4c543      1
     35 0x1e66637df0dc4171ec1a542929967c8a659ee09c      1
     36 0x1ec396ac5964b69c394d2f97f91908e15b3a046f      1
     37 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     38 0x230f4981634d3c96ee9066f17652dc8b9049015f      1
     39 0x23a650462c3502a03e58192c9253437705140f8b      1
     40 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     41 0x269c6fb228eb602bd01d14dbc402555c665e11be      1
     42 0x28eda986ab374ca5103b8cf05f72e6fdd0dabfb6      1
     43 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     44 0x2b9af13e9259059769d3e9bdc390986f9393e7c4      1
     45 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     46 0x2c93b00ff220c5b0fcaef85d6ff01d1f1fd990df      1
     47 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
     48 0x2d36fcb71196beff330a07197a78d87bd1447b58      1
     49 0x30b19318b745b928fb8f9565b8786715a3731667      1
     50 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     51 0x31939da12adfeedddedb7444c3741fcc7d270afe      1
     52 0x3245938d5c684301d60275426447a860392072ec      1
     53 0x332739ab37ca55fee20b9af275b090c9e58f7482      1
     54 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     55 0x35105eb9fbd3c3f03c77d8bd4f4480fc0cf6b02a      1
     56 0x367c78d58c09c34968fffc397975a6d934dbff86      1
     57 0x36d479e3abbd5ab72364f1f88a8624da758dd935      1
     58 0x3a7f1f3716143ca8b45df4d7c6656ecdaf743d01      1
     59 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     60 0x3ac9e19507b2bebd1ea1d29f51c5ea36d221e780      1
     61 0x3f4f2609bad184b1412a382800b8a5cd5c0648dd      1
     62 0x40415c1c0e7a8ff3e184b51a94a2fc5b92df9434      1
     63 0x4135166be458f819241479c82db816add5e7657d      1
     64 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     65 0x4307eea49213f03bf1f6410f3a17b6445fa92872      1
     66 0x44d5a0b1e165a17c16d940ca774a28e62d4b44b0      1
     67 0x44eef0439ea9076f24b9586b591a07cc6bcaa7a3      1
     68 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
     69 0x490f5f3905b65fc725c9454094deeed477d145a3      1
     70 0x516656beacd4c41f4e1a2273263ff4995d0fd1d3      1
     71 0x52439c4de8c10ef8904e96ad00171c0e86169dde      1
     72 0x5291419fe2327c1dc8bc80c55575242d378b5e5b      1
     73 0x543025f669433452143ed5954e040c46f32c7aac      1
     74 0x5470a0ad35a6066fdd1b47d18b6bb04bffb9f59a      1
     75 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
     76 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
     77 0x5a20d2477401d87df1884506d49f9d21316d0bb9      1
     78 0x5b7a286a33bc50b148d8b085d1160a0a6789f0d0      1
     79 0x5bd18902e2ea470c3dba572fde02b5ab236ddd76      1
     80 0x5c5bb26835287b1d7b0fdb5e0c7ea890c509f851      1
     81 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
     82 0x5e130cb7f8cdcfb5a15018ee5846769703ec4478      1
     83 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     84 0x5ffd8de19910efff95df729c54699aebcee8f747      1
     85 0x618976ac08a1c947f48f1dcb9b6f04cba4168cb8      1
     86 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     87 0x61f9dc3a073f06a5c70cd48c7a85ea4203087c9d      1
     88 0x65ab98c13cbec04a61cf53e50fa1318152fa0d5c      1
     89 0x65d899980a6e7e9f792c8145d9d9edd8061ebc78      1
     90 0x668ac82f52bb4835736fd5ba9ba698155d1195d3      1
     91 0x67acf191bf1d0bd9b8be3107434232f9963bd4b0      1
     92 0x6c03bf00e9037c35c5b9dd56bb2069a1c8e76b65      1
     93 0x6c359ab465ec8f893fb6c4243810f4d8933d71b5      1
     94 0x6d075df8e96c8b9aa84a69ca1a16249b7c20459f      1
     95 0x704e0bf0dde8a915aa2dac6d432c650d63275713      1
     96 0x71a0496f59c0e2bb91e48bedd97dc233fe76319f      1
     97 0x71d5960771b989c475073d0041dbd15e2c56ac8a      1
     98 0x76409ee20d9a4a3b89ca3bd6c7585c014fff9363      1
     99 0x768f7248296e71f694c6afe3ea06923093341f0d      1
    100 0x778e3b5ab41f6fa20bb5812fc1ef5929bcbc422a      1
    101 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
    102 0x7915e43086cd78be341df73726c0947b6334b978      1
    103 0x7b226c49f570610021ed39b9fb48bc0558521f8d      1
    104 0x7b5658e5212377020b953d232875fc59ee6edbef      1
    105 0x7b8f1c54e8068960ae703d62aad4fc1e1b23adad      1
    106 0x7be8c264c9dceba3a35990c78d5c4220d8724b6e      1
    107 0x7c75bfc335b797230c44fbb68886c89532548304      1
    108 0x7e45275b686582026d25b96dd4aef9cb42bcb2d4      1
    109 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
    110 0x7ff7b375c9492c70a37c2e68aedff67d015dc6e3      1
    111 0x80d1a063ac15ec75938fe81ff673316797490a34      1
    112 0x8125174cb26ff90b2a3b7a47a3614ab4261bd346      1
    113 0x81745b7339d5067e82b93ca6bbad125f214525d3      1
    114 0x86485c2a48e119b54063337ef685a37dd38e20c3      1
    115 0x8808f7d1eb8e6e2af630d06abeb9b4e8cab9cb0e      1
    116 0x8850431bbd96eda2d50f371f64b2407ad4de4d87      1
    117 0x8889ebb11295f456541901f50bcb5f382047caac      1
    118 0x888f9b2d893356de0d2e1f5ecae7f1ecab8b5b3b      1
    119 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    120 0x8b3c7267f73832a227490ae729a128ecb4c3e2db      1
    121 0x8b5a1a96fa5a485a202b0797c26635b5ad687c60      1
    122 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    123 0x8c319e28ac70b3d7c9f57d97b37583992dec88ba      1
    124 0x8cecf20682e64a5728fd5e8589e63a986f99ee05      1
    125 0x8cf7774f906989e900d5b75bf8787412c9f4b0a4      1
    126 0x8da476c64056bcc7b9be720387804c48666c1855      1
    127 0x8dfd856af8b868bdc73b4eddbf34511310402c03      1
    128 0x907fdeb5b1c8a8554b8ac46459bf6f610e524db7      1
    129 0x93e6832b6d190d1d7373408833231b1dc3bff79f      1
    130 0x943a93487116cd4bd5f2b84e34002437c5d932db      1
    131 0x97004e87aeee1c25814ec736fcbb21adcc010f52      1
    132 0x9addd91874b08f6856bf6b4a2263665d2bc0d933      1
    133 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    134 0x9d5393c69a8caed1af722aba7ce1e42b5672160b      1
    135 0x9e8fdfc3d41b26a2870c293dc7e1be8675fa8b7b      1
    136 0xa02bfd4376c12983a9749e609b209a87ed3e287c      1
    137 0xa0443253fbd322ce7cf2414b0045e6a8ee83cd4d      1
    138 0xa22414e4af5767448624017c7e0151d22490412b      1
    139 0xa24111399b765325e151f5f4cd11197cf4afa9b2      1
    140 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    141 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
    142 0xa46468b3d736e42184577660b0c1a50d4f03e599      1
    143 0xa53fd5fdcc1599164df96ec6d60c572c673d7fb0      1
    144 0xa54e9bf1087418723c310f6749f6d7a7b732afe8      1
    145 0xa7a3a06e9a649939f60be309831b5e0ea6cc2513      1
    146 0xa83ba9e5b492035d0551c7d5ea9bb51e687dfb86      1
    147 0xa88fe6fa01fcc112bb2164c6e37d63395b923e5f      1
    148 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    149 0xabfd22b8d9d7dc14a55d3107e2443bbe804af599      1
    150 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    151 0xae1a530128950b1735674a975e1622872e556b59      1
    152 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    153 0xb2660365b668cfd11ee116dd5f395c4ae579cfca      1
    154 0xb409a168495fd670b031ba74c4a8c17c80275383      1
    155 0xb5e9aef6b2578f7c33135513fe5e8a378d70cea0      1
    156 0xb60fb603f5c32e0277e016df0b2e1d977f59d294      1
    157 0xb638d0e35f6d1e93d29af9af667a726a38a29f96      1
    158 0xb7e4907f7322bbd7c9402d383c85e316bd636ec8      1
    159 0xb8814269ce2a8e05b949d581fdeaa1182c626f39      1
    160 0xb8b86f77a34d5679daa6df4ae830294c6e108b88      1
    161 0xb9bd55eb41a48dd8b3141a09c4c6d829b581cfeb      1
    162 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
    163 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    164 0xbeefcf5e985b9550a15368db9966bec693132636      1
    165 0xc044de7f0f2001ac5ba0d9cb2d10201c05f88402      1
    166 0xc2f14b7be2d39161718b14e07ca2a5c0a0c9a14e      1
    167 0xc40bd3061395ded819d640852b042375506b766e      1
    168 0xc540001a5a7f78fa1ad87b9dc89a63a609195104      1
    169 0xc585d35fb8c9d136d6443a30fd88ccbb5f4cb86d      1
    170 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    171 0xc646fd64bfe82b3c8675adee51b39ff200c51c9a      1
    172 0xc674af056805d186cf7c36495bde22e0d57a3cdf      1
    173 0xc9282392ae3c16536d2c9b1cfb7b60067fe1830f      1
    174 0xca50cc37abaa58d19e3a23ccb086f17f8384cb3c      1
    175 0xcb4de7db08b4dd0a9954fcfa0a09762f58436df0      1
    176 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    177 0xce0ce10bc6251dcde4cafaa189a4a9539731506a      1
    178 0xce8b5f3f8b8520391c27c466f27273bce6bea4df      1
    179 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    180 0xcf9a828d1d6a014a75f1e98e9cefed89bda0dbec      1
    181 0xd0157b9ebe373975cc9bae67e4f4b92e178f072e      1
    182 0xd1e3b84381003cbb944365fc5f559173ec8fa07f      1
    183 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    184 0xd514088abe24f9198a2cddd04a964e2f5c5e1789      1
    185 0xd58518b059895d0628f065f42d2d90d37f347f1a      1
    186 0xd6599331b6b48fa6bf8cbd4d7daa4bce4d4ea337      1
    187 0xd74ba4ca906bdca1a657fb3083ab59199fb006a2      1
    188 0xd9270fd770b3e06f7b4a7452f531f035826a2eac      1
    189 0xdaff7ccb1b7b9d008b977be2611a1149c797f754      1
    190 0xdbc53b3cc2251c10660774b08166325d3742ce88      1
    191 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
    192 0xe0580e24364dd221045c68a674f12666bd3e4253      1
    193 0xe07b14c7cbbb29bf506eafab590623cd349c99f9      1
    194 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    195 0xe2123387af7c45c5b980f91a690222e9d7856bb4      1
    196 0xe3a7d5310b0dd27990ac71e31f2fe677ebbf1b41      1
    197 0xe40a2a4c6dfc11f40f2bee39a3785f2c33c1d33b      1
    198 0xe581b4cbe18172d4ed99e68f9e126a91e09ab304      1
    199 0xe84d4f3463632587d03a26bb68979ef317bda4d7      1
    200 0xe86055c91cacaca88daf63a7f12ab0a219b2e320      1
    201 0xe876ac6b3f67dee6bc86252c94d6a2426462b8f7      1
    202 0xe9495233d947723f55f958f94665535605d74103      1
    203 0xea512c5e43b234cbfa01b1b816af17ae721bfaa2      1
    204 0xeaf04e1ee5de6fb3bcf55bb19ec4173e2b263261      1
    205 0xed31be2c881ae9f91846e3d249157b9242ed7967      1
    206 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    207 0xefc8a4ad63faf1c9c1e9529f10daf3ec33621fa6      1
    208 0xf0aaa42aa80204bbf08906ed1ba5c581cc151465      1
    209 0xf0ab84e8fe7c5d9ebc23f1f2d704dfb4b093af5e      1
    210 0xf110b384ca4f89b1377f16d10d47866487b63056      1
    211 0xf2370b2afb43f853915ae89e5cf5f5bf92efd946      1
    212 0xf2c9294c70b0e8262df7d5436026df0ed393c4aa      1
    213 0xf3cebdbea06d5dafaa224abf28c8d7bdad3c915e      1
    214 0xf4c715ff428aae9bd49c326b33dbc6bed0557a54      1
    215 0xf533a9a5bddf9d79801468d7078584d94eb316cd      1
    216 0xf81a48410c448b1d587be9bbb3b23938e9f4bfd4      1
    217 0xf839be76b49d329886216a024a7d3ea2a3ca2ce0      1
    218 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    219 0xfad6eacaf5e3b8ec9e21397aa3b13adaa138cc80      1
    220 0xfbd6b76704f94c2682b749a02c36f320dd56fe90      1
    221 0xfc1a6f445975681eacf2fd7c3b558213b081ee48      1
    222 0xfcbd526ad12eff1cbb19efe1dfffe03f687cdeea      1
    223 0xfe288e17e5722306baf6f49635bdacccd973fd3b      1
    224 0xff0777f5412dbe89ec398816c3915b40c27dbd81      1

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
     8 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
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
    26 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    27 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    28 0x59068075a799594db03c0255eed68e8e121155c8      1
    29 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    30 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    31 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    32 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    33 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    34 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    35 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    36 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    37 0x69e68074f1aada957edd39c5eae0069973343f30      1
    38 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    39 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    40 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    41 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    42 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    43 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    44 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    45 0x82139687faae8a29851902783e02e699de0e0846      1
    46 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    47 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    48 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    49 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    50 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    51 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    61 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    62 0xbf814810b44978de273191fd612aa47f7b69d564      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    66 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    67 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    68 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
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
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00000006008c43e3f51eacf86e7a63fa4dedb2da      1
      2 0x011e1d90260f42b4e54f7c64d8618aedc9f3ec70      1
      3 0x024e0c1879db65cf94269dec6461060a181dc1c8      1
      4 0x05de6d83bd2b4e0a71767e42c151b4823ab7bf3c      1
      5 0x07670dd0d0d00ed6ec47cd4073c7ad5ddda32405      1
      6 0x0780a20cdde4cebc291b2bd53b897204f360f4a0      1
      7 0x08942872046fa44bc6456e491e8de11de8bae73e      1
      8 0x0898f7a85af9964a413f5f4ff6b7d6f8f9f8f753      1
      9 0x09b4e8c1af40a44331cad3efa80feb013fe99ea3      1
     10 0x09bc19cd33faf805e55b708c2db3de87979b3950      1
     11 0x0af5cfecd1b1be7cd808bf466470db20cb65c51d      1
     12 0x0c03361423dca95f837959a87c5e1370b1ebca26      1
     13 0x0d2657935cc0721b5e4f1d2255c01c1b9c6d1cba      1
     14 0x0f32de315329dc7d64516cdc35e68c673ba33f84      1
     15 0x0fc08f56248f6667904c896307796f748700c7b6      1
     16 0x1157ae6204947fa577d93cc833dd72de2c34bd0c      1
     17 0x120a634d360c163f4fb9c78793fdc6d971a41012      1
     18 0x12c209cfb63bcaee0e31b76a345fb01e25026c2b      1
     19 0x140271fd98e2c9d4338978bd983b3c2e4b20f6f3      1
     20 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     21 0x149d93dfc34bc8092ad9562a6672394d5edb68a4      1
     22 0x1566ae673ae80725bcce901b486c336e6acef465      1
     23 0x160855e3be2936fc26f6948607014c35b5c15e50      1
     24 0x16dee223fc168abff7b979813cdf15866eed7e8d      1
     25 0x18a7ae469892d8272fc16464c935ac977d19b04f      1
     26 0x19899e40bb768dd09f24db2d8538c98d0cbda62b      1
     27 0x1a0335b908dce97a4ed4f265f2b649a2133fcbba      1
     28 0x1a8e059d53bf0f50983c4e03e4931e8535b1aa1b      1
     29 0x1d8a9e8d00f6b7f65dac2078475119bc02da0682      1
     30 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     31 0x20111f285d896e87ff96c9cac66c053ccd79eafc      1
     32 0x2023e02221550d3d75fbe33243948ea76a09a6c3      1
     33 0x21a51b7c3ba93e70884e34482a9790e6b72ff9f0      1
     34 0x235ef6f6bcc455bb284ebefca68754c030bdc1ad      1
     35 0x273061aa9b9ddddce5b7c2d1eb7237611d558d4f      1
     36 0x2737878346290ebb67ab876e976b1fd31f7cf4ac      1
     37 0x27d06aaa0bfb51dbb2d415591f5f4993e6394311      1
     38 0x282173335ae1f328b34eb9acadc052e59e70a89f      1
     39 0x2aa82c35f3bd48bd4e5141ce595a0628d2435aaf      1
     40 0x30ab90490b6e0119213ce729a274d8b655362e1e      1
     41 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     42 0x331017600f8d211462e60e664a5c3332b0f77353      1
     43 0x336734a67020ee0f36fbdeb32517a5d8751bd0c2      1
     44 0x3433faf996a75d642a1e8dc82b8d70381e252881      1
     45 0x34974e768a3d4bbff41cd352ccbfc56c57b1bcb5      1
     46 0x34cde3eba2fde05fa45598e7cf52c90d9d6cb461      1
     47 0x34f49d19462bca4169d66921312e2562f1502cef      1
     48 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
     49 0x35aa7d7bc64c2b03895ee6315a14944ccfdf9aa5      1
     50 0x370c4fafd8b9a621d01df979866c5259a2f74211      1
     51 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     52 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
     53 0x3deb49a2c1ab7ee9f66f919286587f6e021c352c      1
     54 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
     55 0x3f849f47f5b372d80407e442f360ad7b17f5fac4      1
     56 0x406a3399c50adcd50d62b925fc16f84dd91439a0      1
     57 0x407e3b220c084973192cc76244ff53baf88363ff      1
     58 0x41cdf42b4d22b3edfea997a2f03f32b156b135a0      1
     59 0x425052c13e1785b736e69324be5685e4d396a3a0      1
     60 0x42617123b481f61df6b5b8a6484ba0a4e6929279      1
     61 0x437053537d149d94ad17b4c2a7e7c796cf304efe      1
     62 0x440758f4664c99d00ce72bc452b40dce8b7023b4      1
     63 0x45557a43891a053d64da64d846b39b049f767d98      1
     64 0x45d2bafe56c85433e0b9f9b50dd124ea3041f223      1
     65 0x49f7e12f7b780b8856ef09986b66aa6bfeb39a49      1
     66 0x4b85b8e5d4d2e3220218f69500c4804721ee4cfc      1
     67 0x4bf57c3b1396470882fe7f448a7c9c16f25960ba      1
     68 0x4c85ece5bbf191d3e1b76934c8372d4ae3bf6927      1
     69 0x4c9995900790d3b33f2c49342a6b82add8c94827      1
     70 0x4d42d679cc113ff293f96e1b820209c661ee32e6      1
     71 0x4e12b79845ecd48ba5165e8fc9e95436811c2b58      1
     72 0x54dd50ee1ef8d93bbf60241f5d91da631897e714      1
     73 0x54f2a20e81f826de41a3eba33c5a1a839b0cd5eb      1
     74 0x55c06d471a6f04b3933947ce0af9211b85f3cf82      1
     75 0x573555dc420c87082143e0b9af96b3413c7514a0      1
     76 0x57be31b403ed3dcd8033a19d85afbdbb8ce3c3a8      1
     77 0x583b7fa299d2e40c235f34bbb16e7e8fdd6d379f      1
     78 0x59a1c02fb7070dc79b942b3d85c53606adef7465      1
     79 0x59e027dd6c466c397a51aa94b59b6b055e19f228      1
     80 0x5bd3bf853b9970d93da64d7628919997c1a06a6c      1
     81 0x5c43c2d884fc7f08f69ff6168ac7c3134d3bae8c      1
     82 0x5caa9135d438e512780bd920fbce06a547bebb6e      1
     83 0x5d25087405105bab12624c73488ec186066a6376      1
     84 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
     85 0x60a3f3e01c97e9f8f6414793abc577633ff01f49      1
     86 0x60d4d538ff67e6bd2bd14d3aa724014ef01abb93      1
     87 0x65f7e6d54400315479cf47ef8e6c5a4a8475395b      1
     88 0x677eee9c51328095ee201887cb06fc189c505080      1
     89 0x69a0841248dff9b5bf7c58b673ae837f9f2eb6cd      1
     90 0x6a32300ecfc84accfa9a4e1200457ee4a22c7d3c      1
     91 0x6ed5a435495480774dfc44cc5bc85333f1b0646a      1
     92 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
     93 0x74e225269effacee134aadb51aa135ad066f55b8      1
     94 0x752accb20c027c53f685d199762dfc3afa86e1ef      1
     95 0x7619ee03a91ea6b931d9e4e8408451b93115364e      1
     96 0x78c74a90e80b4d93b911f979839cf764be00b4d7      1
     97 0x794f0e25c5910cb3f8a82c9fb9cb44bc069674f0      1
     98 0x79e561168835c783240a0637320d308897bd0922      1
     99 0x7a71dc357646a8149a920dbd26f66f07769f01d9      1
    100 0x7a96945926435a144ef3015ebd65cbc6102c19e6      1
    101 0x7ac55b39ef52df3cda2e6189965f60802a501db3      1
    102 0x7b220358c3ec9ec03b08120dccec56f16fbf33a4      1
    103 0x7b86a910a6c797316dda30b78098e331e08b9f34      1
    104 0x7ca83da48606cbfbbf12af0294dfb7433c0393ea      1
    105 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    106 0x7d4d3e72d540474e24bfdd19644e8df14581664f      1
    107 0x7d656ce2a065b331abd295d07f466f6249ac7231      1
    108 0x7fb1f91574e2d39c41a85395afc7ac2e4c0e6b9d      1
    109 0x8018bbadf493a80051ab73b83df31d1bf37790b6      1
    110 0x81dffaa511734738378a73e8b7e0ad859a504411      1
    111 0x84b833ac214d8bc9d0159b5416879e2bb8cd876f      1
    112 0x853c69418605529a68907aaf7789270e3cf69d97      1
    113 0x85c0c09570c955f6e07522e9ad6041716cbbb7fe      1
    114 0x8605cf36085697fcda20be5488df08f407f73430      1
    115 0x860e3777df77395f9e22c5d36a37f063dfdc07bc      1
    116 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
    117 0x8699793e22403b355185d8ff76e7392f98aafa46      1
    118 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    119 0x8865de06e60964c74deacd501fe9092d2b2c1215      1
    120 0x88888888eef9d7648d15ad26c68d1653464856b8      1
    121 0x88bd0324fc0b474b3c5860669492a1b75ecb565b      1
    122 0x88c5677336e252c7a53c29dd20375edc802a5919      1
    123 0x8c0a11eb047c1097c821c159b8e0c2c5f37f81bf      1
    124 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    125 0x8c39a66ccc0f22ade61c59c7f21bb6bcee4921f1      1
    126 0x8c47286ffca3d75cc3b15ffe11093abe01913a3a      1
    127 0x8f95801d603842c5c60f74d01f76257dc8d2b446      1
    128 0x8fe8cd04ad2cffe0d09cc45386659ccf4dcfee48      1
    129 0x906b346a7d88d8ec5eb0749fed160702f58bf425      1
    130 0x906cc8125a4c21c2b7f634efd5a5702383b925ea      1
    131 0x92041c816fa960e2352f49ac118771307c416a17      1
    132 0x9212d88943b58975afbf0af31510c0c00414efc6      1
    133 0x92cc4d43c986d478ac45f08fff096b8a71397eff      1
    134 0x92df93709383a40e5ee70e66da100a7aa850dfe2      1
    135 0x9553803357f9441ee426e701a1bd8031f814fcf0      1
    136 0x95f9f2fc223110ae09a8140c9a60b848434a652b      1
    137 0x9746f004a75b79f518271e94170bcb13a2c2a559      1
    138 0x9b8e318d31931a23434433d9555c53d3469feef0      1
    139 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    140 0xa1b32581ec3bb4c85f47806bfa8566ae08989870      1
    141 0xa2379150a951733abab8a79debfaeaa5f6194a66      1
    142 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
    143 0xa43b30fb0333e5d68cbe36e20c6accab28f82cc3      1
    144 0xa54b0b7bbb93559963efc25c232beac0895e13e5      1
    145 0xa87d967ddb0eacd4ce9e1ae7e4fdb8f95f884d81      1
    146 0xa8c9d8691f97af5d22cf986d36faea60a50aaf38      1
    147 0xa975ebaf140d206080dee102552dbf88d057af18      1
    148 0xab25a2c1a37e1f1fd689a8a9ea61eddee054f0ce      1
    149 0xac0f76a8c30e23bc197d26465e7be319713c5224      1
    150 0xac0f787d5eb1f44832ecb14ce1ad3382088e63d7      1
    151 0xac7c12f4057076d547666700cfab976a12456774      1
    152 0xace379cbf7f46f7b9f5a8b6da185457a505b6af7      1
    153 0xaedeafe1ecf674c40763f883be317ae6cfa0dfff      1
    154 0xaffebd20ab189db1d41dd28351f10a343153a872      1
    155 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    156 0xb29548cf5ab742069387c1f4599cdca5e0273e79      1
    157 0xb451f2e4e0ee68a72b76c15ef50102fb127ab2be      1
    158 0xb5bd1ca7ef13954faf50286742aeb54d072ce21d      1
    159 0xb63e374ee7a67e59203fa8931eb926a0fd9c10e7      1
    160 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    161 0xbd79b7e0cae4500eb02e6f212ddf2681ac8fb6d4      1
    162 0xc23e873f9c9b968ccdff0e413710d865ef18b860      1
    163 0xc3be75af49a71bd36b08c513cc4520b5e5290928      1
    164 0xc54b39cd6cf3d86220508b61b41feea53ce797d4      1
    165 0xc68bb9728103765ace6d43d98bce3281af3850cb      1
    166 0xc72c604a60fd660256be2d7f4ebec13ce7f50daf      1
    167 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    168 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    169 0xcddb35f12845511e4aa5d624eb7378220701ea4e      1
    170 0xcdeac38dc448d16997f8d3f4486f78d3f6b0ddc9      1
    171 0xcf2cd14ccff2b399ca3ddaea5bb5c6f6be409aa5      1
    172 0xd2b991e008922efc8a9f7c4115fb12a2dff4a203      1
    173 0xd350c0d869af20e0b83b80dd3e212572417f5a69      1
    174 0xd62dd370c5e839d9d82ba60aa68559d5ff0df708      1
    175 0xd86eab09bb728c6df320dfd03f5a5390e0990bb6      1
    176 0xd94faee3abe47dae9a6dcf2837b0afd843eca95d      1
    177 0xdae3dcf64f91ed899de50b8d45d6cab7cdf12ff2      1
    178 0xdb945002b39af8c78dac5ba02123ad32cde12a10      1
    179 0xdba5e808593f9438963c0414a225ceaef40ca1e5      1
    180 0xdc594f0eda0e5efac7097c5f1f5c450f856c58e1      1
    181 0xdc819a832bfa55c933238a474954f3646da275ed      1
    182 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    183 0xe513e29ff7ede7e01eb96e2d3fbe1b1189386d06      1
    184 0xe64777dcdca11fd8bde96ed05e7560ae789504b6      1
    185 0xe6927cfdedadfe07928f7ea31ce1166ae8217c34      1
    186 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    187 0xe8fa17f9956e859cfd013b4c7df33cd704f0e7fe      1
    188 0xe9d2d1e909e962873260cabde18719c67ea61052      1
    189 0xe9f1287bec6d70d950add3752d735420398b810c      1
    190 0xf045e55802a8f0aead20d6f537bff4fd9fc354b2      1
    191 0xf063be86ef61e4e9b6baa6e6123838da32639096      1
    192 0xf0c55da3a8274f6843434df0da437ed4ac0dd310      1
    193 0xf22343eb7646c9139045f2ff01fb3049d0ed3685      1
    194 0xf353b57ffe0506a44950805395e5412f42181dd0      1
    195 0xf5ab63bfaeeddfcd37f696a9b8697e424bb22788      1
    196 0xf7b060806ace46cea4df0e688f871c9664383a8d      1
    197 0xf81489f074a9f70c294164e07692559269f3defc      1
    198 0xfbe02db58e0e0e02ec8b7fac182b0bbfab81d621      1
    199 0xfc9dd877930e9799beb663523bd31eefe3c99597      1
    200 0xfcda163ed7c3cc87dd57e2576c26694e974bd85a      1

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
