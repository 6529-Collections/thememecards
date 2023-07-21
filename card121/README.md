
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:36938       Length:36938       Min.   :1   Length:36938      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:36938      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17732969 # https://etherscan.io/block/17732969
block_hash <- "0xe67eb4e8e95c1fba83f8cbd6cbf48e96326215585aafcf9367b65798469e89fb"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4810 

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

allow_artist_phase1     <- pick(snapshot, contracts=c("SuperRare","Foundation","Iridium","Infiniteloop","NightReflection","NightReflection2","NightReflection3","NightReflection4","NightReflection5","NightReflection6","NightReflection7","NightReflection8","NightReflection9","NightReflection10","NightReflection11","NightReflection12","NightReflectionEditions","NightReflectionEditions2","NightReflectionEditions3"), address_remove=address_remove,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=140,address_max=1)


allow_raw                 <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles             <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 88 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0416683fd1e6084078d0714bb91c2eab3af4ade3      1
     2 0x062164d0c571002b9b34a9a71ddaeeb0b6f93132      1
     3 0x0bd3292ab67d05187f0087c2e0ae010518d4e830      1
     4 0x0c7940b3c5b58c801de112665c0026b810815b4f      1
     5 0x1372a72a6e62f55007d2c9236e829a1ad98c8f06      1
     6 0x1384902d1cf61760ff19d02d009d3f699378a49b      1
     7 0x1725070316139c7f097095df61b9e0f3e0edb9e2      1
     8 0x180669310d089fafcbee40479ee719752d680d4e      1
     9 0x189cd6f032fd6e90c064d38b58ed106157902b1d      1
    10 0x205798065be2bf10ba02a2303396b38befdc3c06      1
    11 0x25372ddbb42e2c6ca1176361ccca9cb218752aff      1
    12 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
    13 0x2c775bcdaaf5ebebe73313f50671c6e09f05f824      1
    14 0x2d913709fa6b87502af62b3b2f883cfb7024b655      1
    15 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
    16 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    17 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    18 0x333da5a1696539399ef5d2f3dcda7e3ab4401d0c      1
    19 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
    20 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
    21 0x3b23b754a8f59c9adaf721302a40862e387aaa4c      1
    22 0x3c7a0fb79e04c3c7fdf66452a3ad073998d49104      1
    23 0x43fe21a5f285fe6dd5b64ebd21e183a5af535c11      1
    24 0x4d8d2ddc4560252ae2edefbaaa6dd12144155014      1
    25 0x53ac73e26a51c445d8b6f92f89389fa606663804      1
    26 0x602532759c9261a82778e96911a5fded198ffb7b      1
    27 0x602edf528211015934fbb9e7fc043f0fa296ec40      1
    28 0x6177adacfde59c3ace55356250b6504488ac513d      1
    29 0x6a71efbc3d99cb376730fbd27fa0a00efee4810f      1
    30 0x6a7b7a3b2993242fd0f2a66caac3666525acf1ba      1
    31 0x6b060f4f45f8d761fe98c32628a33c2b7fb5dd30      1
    32 0x6b309067325e259ca329e242e1ffc3735aab1797      1
    33 0x6c78bea76dacbf14eb37de0d70855ef67d73e30b      1
    34 0x6f54c650190b44ac9efe531380417ccf12d2d06a      1
    35 0x701ce8a7211a3ab12f23825d194f70f6ba5f1afc      1
    36 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    37 0x7252921bd62996de2fc352710aea0295a4143218      1
    38 0x768057610a07396e05046af01e7c9b912e4a81f3      1
    39 0x76a1c65b58e65729f3e527869406c3eda0f3913a      1
    40 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    41 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    42 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    43 0x7e26074f33e0c36a3a1d7e339a6361f0f692a4f4      1
    44 0x8058d889c48b7a79cfdaa54dd6d623b09c9146a3      1
    45 0x80a7747adadd60de888da9d7e9054e8d33817d02      1
    46 0x8300f444462e55d11dba5823a1584badfc293fd9      1
    47 0x8497277c9339170a0420e86da8352e0c084624cd      1
    48 0x8b94e8e61f331d49ffce99eb0f6b2e20f35dbe54      1
    49 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    50 0x920722ab0afb4b3e65329edab2efbe6fcba1b15b      1
    51 0x92ef6abe00f0fdc60f5c0a950772f0e8bf277e71      1
    52 0x951ba5a2f0ca424def12ff47ff56a46a19b9ece2      1
    53 0x953044e85efcf894bce94a9b942a2ee0760beffb      1
    54 0x96dd7f0efc83b2e6863085a797014c1d139a0bd5      1
    55 0x9a452aa36888f6d345a543a421a326e68f7f34b4      1
    56 0xa15144bba8c85af0bd2d07fd4e88c9fabba6ecf1      1
    57 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    58 0xa7faf09d160d777c98beef579087266f6da167c9      1
    59 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    60 0xa96df9d72fa43cbc5f9abacf40c5b0c0b9fb0684      1
    61 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    62 0xb4532c764c26dab6d69b5b62a201fd624ca9a6ba      1
    63 0xb5c1ca8b257cc4b5df761ccd78083debb5a3ab30      1
    64 0xb6fef67b00d644fbb5e4368aed9f85ec2d134fd5      1
    65 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    66 0xbd88526a40bd28acb6761a238d7751d3f1bb5fb8      1
    67 0xc34aa5ddd90a6af4d89ebe4d0026f439bf11368d      1
    68 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    69 0xd16091bbd6bb0f76241de83e4d496e1773089e33      1
    70 0xd2927a91570146218ed700566df516d67c5ecfab      1
    71 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    72 0xd5a1d54bc0b1f25b41e84d9949513c1da0e5f2c5      1
    73 0xd669155d39e22ee851f4558a3230ef7de057426c      1
    74 0xd6756f7032cf147d9672968f1e27416071341a42      1
    75 0xd8c4ea00789fa0ac7bf4d07ddcab1ec0f2303471      1
    76 0xd9870cb1001b16c99a80cb252d10950332a65988      1
    77 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1
    78 0xdf4247b46f6bc51467ff91c59a66712cff6f5a14      1
    79 0xe627311229899b45ba356ecfe02bc137d62abb84      1
    80 0xe98ce1e7d062169899f206d76ee6da5c7d9d7d0d      1
    81 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    82 0xed9788430cd53dd218c80063b2b788a99d95065d      1
    83 0xefcb35a72e5bcf7b50bc739e92c38b9ccdeb2bd4      1
    84 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    85 0xf4a1aa2186781a47d815438ef32c066427b81359      1
    86 0xf4e07c5d16ed7e0427c477bf8d36b321d5b1a048      1
    87 0xfed473cd045563542414e31af846a43bf58fc59a      1
    88 0xff53ddc9e7cc87a21f19e10858edcba53b009832      1

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
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
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
    42 0x82139687faae8a29851902783e02e699de0e0846      1
    43 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    44 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    45 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    46 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
    47 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    48 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    49 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    50 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    51 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    52 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    53 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    54 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    55 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    56 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    57 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    58 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    59 0xbf814810b44978de273191fd612aa47f7b69d564      1
    60 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    61 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    62 0xc4e8b752c53df925013e03fe4d2273a8ccb6c545      1
    63 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    64 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    65 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
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
readr::write_csv(file="allow_randommemes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 140 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00dcdd43bd25f78ae91c12f3c295dc11d3065e00      1
      2 0x01072faa900f4ca3b40944ca815f39837bce193b      1
      3 0x01970cb86f2d27134263638863f1f08c698e84af      1
      4 0x024aef1375c1701693b2a0c8e8ad7badf0dce493      1
      5 0x0320cf8858eb58636405ca3f260fea793c6fd168      1
      6 0x03bd5e165e78d2ea367e316bb4db8da59b843bd2      1
      7 0x0571cb3b7b1ee738e189678a22eb3edd342ed5e1      1
      8 0x05a487a677d9f72f3d1c3e420a19248a98265a55      1
      9 0x05b00d69d50426020749e1e1c60901e129d3e43b      1
     10 0x087e14b18e87367a7cc58f04989d749b8a0e71a1      1
     11 0x0dcee254468df83cfe9dfb2236bf253459cdb079      1
     12 0x0f4a34a2cae2c2e1881f8d960265894455993579      1
     13 0x1365d2e329df3785e37ffd88edd934033e8ffdcc      1
     14 0x13a2b24101f087068bda48c8589c1954509e66ac      1
     15 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     16 0x1610634c1fcda56b9934d67f885b30dc73f938cb      1
     17 0x164a4787cb79ee24998d3a5a4fc01cec7a3ef524      1
     18 0x18eb4700b3949143a906e6b0a7388e45cc140b93      1
     19 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     20 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     21 0x1c91f3034c235f24d39157e9f052d152f4c3857e      1
     22 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     23 0x1febadf986428f0888469b0a5e8f6d791496e3c0      1
     24 0x204a79ff4ecac8164c717bd46c02cd493a159789      1
     25 0x22aa3f5d1daffe1a9df298e79a0cf2f98c1b92ff      1
     26 0x260c776f721e6281e51f21aada74f6407ccda236      1
     27 0x26b7dbcf563bcb1505ab60c6948c2c73f20ead91      1
     28 0x270ee8f5a362832b21569a0c1afa38798a9dbf69      1
     29 0x286cd2ff7ad1337baa783c345080e5af9bba0b6e      1
     30 0x296132e19ae5737400ca45bcc8c8555982d41d53      1
     31 0x2d2767ecff71d594823ef329a82ddb9f318cfe4f      1
     32 0x2db19ee7f7e0eae5730e7a393fcc899d70df9f25      1
     33 0x2dc23418844eacd31a7d91a5571e000960553283      1
     34 0x302522150ef22faafa6bf6530b330b5a4bc38369      1
     35 0x30c1f8d8b7d34acd0827e4fdc7b2d98c32287ba1      1
     36 0x32766d81f86cfbf4485545c9a367b4e2c224cdcc      1
     37 0x3544311b3c5f21edcadc9f5515f3acd74f502fff      1
     38 0x35c1993379e9dcb3b1c152b2d7116dfcd373def4      1
     39 0x36dd9e834ebcf94efda21b142577636b62d4770e      1
     40 0x386e0ad23a9f182d884ee2b8f61ec8c626b94385      1
     41 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     42 0x3a8ce7edbffac61efcd7697cce69c05634eb0d98      1
     43 0x3addc2ce68d7f94af141a3bbcb86aaca1ec4494f      1
     44 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     45 0x3b402b7d44b647367686f20b9905cf4a6eb6349b      1
     46 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     47 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
     48 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     49 0x41e399b9243bd6f772e30cf79dcbaba791143d6c      1
     50 0x421ae97f38789280fd114478a748a0f943e6bdd9      1
     51 0x450844ab03c3c9aef4e81aa785f6730decc2fa64      1
     52 0x4705bc2775cee0fe266dc2a376010c0eb1bcb474      1
     53 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     54 0x4938d68c208aa1f0a34798a4343c584c58d259cb      1
     55 0x49c1c6683a54d962c8f2d9d36d05ef107e9033ea      1
     56 0x49d18648e4aa35368a683c24881dc94225518751      1
     57 0x4a9a40b2c44b7d32ecf4e0cc66f5304518ec0a59      1
     58 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
     59 0x4ecf4bc90510794f5a4074136efb716485ae0da1      1
     60 0x54be3b98564f0a89237ff10d5c7a053edf2af10c      1
     61 0x56879fdf9792ae08e890528815a317ae53bb6ddb      1
     62 0x5a0e828703aa063b375933dd9ab1c4a147f14abf      1
     63 0x5a1848a228391145d5b43e0ed0b52fcc51e2ba75      1
     64 0x5a3a1461310884df894b7e973305f690fc5779d0      1
     65 0x5aae5802046733fa533ae09bf644ce359d83fcc7      1
     66 0x5c3d9b559e5d8f476044d930423a894d30eea5c8      1
     67 0x5d5fe1910a289b24455af49ee3e41405f94b921b      1
     68 0x5d999a7dcf597169a7f8dfb03c7935da39c163a7      1
     69 0x5e7472223d5d28af97832934dcb9e5408953cefb      1
     70 0x5f22dbd716ec054a9802d1d0632902398d097c36      1
     71 0x607bb997dd7eef475d694e9391a082941a490792      1
     72 0x6c5d86dfafd74031ea5ea1c7315c8010304df411      1
     73 0x6dc83272ac6e09044cbab1e8f279e908b25d0c4b      1
     74 0x6ed7f81208839e31e11840049201201c469a7a56      1
     75 0x721fe9de376bd927bb15fd25de41bf3d420b8c07      1
     76 0x734ed31ad4853ecb36e8554ec35408652642f3a0      1
     77 0x75182ab9bea2966bdf3eacbbc2cefba953474c65      1
     78 0x75c9c9de970362127f6b88d1ceec8be945d7a7ae      1
     79 0x78a9e315c6d6fe8ea0ee1cc40ead04477f02bed1      1
     80 0x7cf91c0ef074f4e3fbeebda55dde44ddbd20443e      1
     81 0x7eed3caf47728c4b22bf9b70e708b513b5350f55      1
     82 0x8332e69cff9b084d3cff0165a9f254cb2dcd98b9      1
     83 0x83e8708aba59195bbb2c7fcaa83af1c536d37b25      1
     84 0x8511a533d0fd5a46d8c403549277213c95f8bd34      1
     85 0x8924ac1196e2ae89603925b6e001dd14e3f7fdff      1
     86 0x8bace3a49a375027868cdd34e84521eed1f1b01d      1
     87 0x8e5f71dc1d9b1dd976137d61fadd5a4318a8ea39      1
     88 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
     89 0x8ec2f31fcb64c018909f10a8d6c5756c5a3c3ab7      1
     90 0x903b1b960ecfb37d994fa0cc537777d8ed766656      1
     91 0x92ba8c06142ce9f805a35898190cc53b3a809ab7      1
     92 0x951e3f7596982adb595ba2162e82c2915e5a92ab      1
     93 0x9b8e318d31931a23434433d9555c53d3469feef0      1
     94 0xa1114eaa6d983390c49e9e29235c75f516674643      1
     95 0xa31c35b347aac1b4110841da7179ca55e8029918      1
     96 0xa352bc969fef8b6938d74b388062f2220ae437c9      1
     97 0xa42098ea9ba9296a80a49c11e22a66c07a5e8212      1
     98 0xa55048cc0369e412cf7db3d9bb30afb51d091a12      1
     99 0xa5bc4fd313afedc439a3202fa54cede5c687ab24      1
    100 0xaf37d6b9bfe0f5e4954ba1f2d0734aac32b2ae34      1
    101 0xb0d1adc856bf8278d87fb2811340e1bef200d48a      1
    102 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
    103 0xb48abc0d58d2d0dfbfbd60bd87c4b3fceaed0350      1
    104 0xb60d52448a5c76c87853c011bb4b8ec3114a3277      1
    105 0xb9cad3fa5b7beb0033ee8636c40c7c5755819af5      1
    106 0xba85979ca3c100c2b446e32696d66f3aac613102      1
    107 0xbc1cf0677fc5ed5d797668dfe0453b895d350b37      1
    108 0xbc1e0021f9d22582b34b584b8516dff417e7ab71      1
    109 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    110 0xbeb15cb4e910fe9e730c48f55a4de49212203d10      1
    111 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
    112 0xc23e873f9c9b968ccdff0e413710d865ef18b860      1
    113 0xc2baa32b57a91b81d35ef3052319873bbd16cd12      1
    114 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
    115 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    116 0xcc42e6ec5d10c64551b8af534ae0362343be2a8f      1
    117 0xcc5797a24417a337b93706fb432b0e0252b20e11      1
    118 0xcc95fb46ce61d885ff396288277aea4c13950e09      1
    119 0xccc4a6cf64675179d69abfd0aaddf06d3b7e80fc      1
    120 0xcde7970b78ee805ff4b0c32a7d3f23e40bcc9554      1
    121 0xcdf4930c1e3167faa772fa463ae8ff44d61273f0      1
    122 0xd6ce702632f94069e138d30836194b121f047e79      1
    123 0xd8ef37a3b1f727deaeb07194b333da4be9c75bec      1
    124 0xdb416840b876a6cfd878ba6af02238d146dc016f      1
    125 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    126 0xdf7cc19cb1066c80760dc1b5464c4798dacfe407      1
    127 0xe0753cfcabb86c2828b79a3ddd4faf6af0db0eb4      1
    128 0xe496a4f9472a4935ebf4ff78a18ae21e2728ffaf      1
    129 0xe6d2a9b56701d3180d218717141670ad720e479f      1
    130 0xe957b0829c6ae6ac461d7365b7927e1ff604114b      1
    131 0xeb379827548ece3cbfa0fdda3290785d1d7c3eab      1
    132 0xec2bf4a144a11a4cf38be451b9db0633bb851230      1
    133 0xed17480ff5fdf27d8a06f0b228fd445d708f154d      1
    134 0xefc2702d50fb43cec83434d205c89124db029566      1
    135 0xf4b27acb9a65dcb2fbfe8fb44516b09ac1f39822      1
    136 0xf6cb64ad324d2ff8ff26ff90360e2835086a9b3a      1
    137 0xf9f40128e49d2941fc7c906a9eca8bb65b54d60d      1
    138 0xfab0b832caebe5b628bc7ef381ec855f6aa4cf71      1
    139 0xfc1dceb7648510b54cd3f0e19288a1def9699833      1
    140 0xfc7108ef9d567e25691eccd0b01fa77042b36167      1

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
