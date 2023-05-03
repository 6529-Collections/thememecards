
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:315         Length:315         Min.   :1   Length:315        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:315        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17171969 # https://etherscan.io/block/17171969
block_hash <- "0x49ba78f9bee0bb77268c65b385c898aa673a21ae6406a2ed33ad6ebe4e14fb0e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4848 

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


allow_jeffsoto_phase1     <- pick(snapshot, contracts=c("SuperRare","Foundation","MakersPlace","Sotofish"), address_remove=address_remove,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_jeffsoto_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 124 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x012e168acb9fdc90a4ed9fd6ca2834d9bf5b579e      1
      2 0x074cebe17640d7327ec6513be2077a397d6f6724      1
      3 0x08ee24d316768d033bc596876aeaea2ac8742c5c      1
      4 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
      5 0x0bf76b912b51e95abdc144ad374327f0ea269e90      1
      6 0x0fb90b14e4bf3a2e5182b9b3cbd03e8d33b5b863      1
      7 0x10b84eba5c1466947bc8fcbd7ae91487ee0e4951      1
      8 0x115ab9e1dbe84030719835dd3d4b74503be8921b      1
      9 0x13a716dd20763000cbf87f999548c3d67f936d35      1
     10 0x1772cebc04a90cb0ffeb23e6c26e2bc587cc8412      1
     11 0x179926bef60a59d4a8ac497a2158acd828cfc308      1
     12 0x1a52e67b158508989bc98903027f3a3939a60c56      1
     13 0x1a82b7c38fdb6be636a78b33479c8459881fb39a      1
     14 0x1ab786ea6828ff401477d6d351408cde2ff0b938      1
     15 0x1ab8421565b5296d509e7a43ec7a7f826df7201b      1
     16 0x1b7844cfae4c823ac6389855d47106a70c84f067      1
     17 0x1f1f0ee0b493a67c79e5d77213cba8bd33cbe5f5      1
     18 0x1f648b364a8c8cdc679d0c77e60fd263fd1d9da8      1
     19 0x21a815cb23c114f4a4d03467c22bdcf6a29ad159      1
     20 0x22c0ef877e33eeadfc47393893e488a3f813d98e      1
     21 0x2607aba1ae13fa3a020ad78d987f8a476783a61e      1
     22 0x27b68466b01350351cc7765a6ddbaf11fb2aa992      1
     23 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
     24 0x2a41282434f89f5bbf272b1091a7a0cefd22ccd8      1
     25 0x2a418fd4bc65e1677905112651bcbffe65934ee5      1
     26 0x2c43f35b203611ac70141be3179f680c05257ea3      1
     27 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
     28 0x2e150a103010e8e65d81d7ab3096aaf4df93d83c      1
     29 0x339af8ecbcd046cc4d2465c55f82657347ed80c0      1
     30 0x33c2656b7b33461f346f697d998d89a110eb42ef      1
     31 0x33eaaee9d0a6192d11bbb370aee0e5d22c3335b5      1
     32 0x3675dcbb0687f0760d93aa8ce58501ed4f832390      1
     33 0x37438d1d81f09d9fb3f7df14d7b73bb3ce16bcb5      1
     34 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     35 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     36 0x3b25768b5f1c7a8478950d06395251dd3341b1d1      1
     37 0x3cbb3ce4a5c98758c38178a6e1fa81b73e3d812f      1
     38 0x3e95a6f411d9b16bb94c7ed51805cee01d9738df      1
     39 0x4159a0b7fccd0ddba3cc5f2b5040262c825bd005      1
     40 0x4a6ee3dc237682793c11f30b3ea6d6315d8640d5      1
     41 0x4b69be6035cc2d1435e5651cb561de92c8b8847c      1
     42 0x4d453e9c59a46d5db9dbe7199cd4236f62cb129c      1
     43 0x4dec3c8944c52eead1e2d80d45a65c936d7afa60      1
     44 0x4e2e67836d10b02b1dce78591af6dfd1e2d7bcba      1
     45 0x4e8ffb396224bb6788f970370bcb236b0e2af645      1
     46 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
     47 0x544ec526b8a4bb1f6e465f3c8d135d32c7a41db7      1
     48 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     49 0x58c04fb55a4b606844992403eb0c2ef8a92fdb16      1
     50 0x5904c40a2166475efdf427aa5658282e1bd08d9a      1
     51 0x6244ccf4b0d9896dedc15f647f08e27f717fef3e      1
     52 0x66c32e682c76f31472e6bf2990d8e9b11658e83a      1
     53 0x684c3fc473be503960dceda1e8f1dc05d8a0155c      1
     54 0x6908cc437c8bea5c19a81e487a1528635ec2b197      1
     55 0x6a59eb70cb8d099f2227343fb3dcb8275599a1df      1
     56 0x6b755be25d004fea5d2fdb63b2e0e36a7e311bf0      1
     57 0x6cef15e37392f13d0873dc18497ca8087c681e01      1
     58 0x6dbdb10f63e920414cefb656b6a08a9603306f94      1
     59 0x6e24ac7a957ba929e48e298c75f6b76d0cdfa901      1
     60 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
     61 0x71356112b4ebe1cd2d0caf59d437bb70574e22da      1
     62 0x727f25672f4f2815831ed496c87b33faeb639238      1
     63 0x74743ef06b03aeba7689eed8186f69a4f6a540d2      1
     64 0x74a1320e0e3af9296287029d631f1cf4b25fd91e      1
     65 0x7601cab98439ee3d92940feff18039bda9eef466      1
     66 0x7a1682faf72e41e1e23a11a4cc248c1d4444eb9f      1
     67 0x7c0830c2fc55e23d23eea9ae8536bdb43dcaba4c      1
     68 0x7c6553933a471b43ce3a76a02245c5162c82522c      1
     69 0x7d7ea735b7287d844c3a9312bc1114c6131781d0      1
     70 0x7d9fcaa76790857f78197199ba858706f97f493a      1
     71 0x80099c469ea4de62abd8f8719c781db5b06379af      1
     72 0x810ae546f9f7a63b1e5fbf2aa55584971c713d0e      1
     73 0x8136f147fcc72d8e74a78ea47993b79b99062ce7      1
     74 0x85b44ff8ac9fda464fce6e968d195187dc2ddb57      1
     75 0x8756696419e48335102094ff41c3a5915a24636a      1
     76 0x8fc082b2e73f89c6aacd7154871e11e102b554bf      1
     77 0x9305a3a61dc1169845a543d4b4bcf09c0729bc83      1
     78 0x93670564b92634dea224e7f6932343fa93c22554      1
     79 0x9478c744401bb8b130d542257dcf632930883b1f      1
     80 0x94c1b861359a8d15b481d65cabcb188a7dc63d48      1
     81 0x9769334fc882775f4951865aa473481880669d47      1
     82 0x9b1495a03cfcfefc70832f7da92b0e40e0a0a3e6      1
     83 0x9beee1b059d25dec08338b7183465cf769a74faa      1
     84 0x9e1e85b61ecac344bf8cb8698399db1becd880d5      1
     85 0x9ef4ca1a90361aee93c4638d142ba04a5a8fb08b      1
     86 0x9f5259fe40f34941acd627388dc24d4dcf9b117c      1
     87 0xa455c7834035a586fe049526bb124eac7a1b6dfd      1
     88 0xa698cade532bf7491f0f887284f8e10e2de97198      1
     89 0xa9f958b93cacb63e70040c48e749ce7cc4ef7002      1
     90 0xac843da064c044804f906664b8432f561e0704ce      1
     91 0xaea00081c711b79d00ba89e27d99d898a96629d2      1
     92 0xaf0c5758634fa73dde83a256f2d0c8fbbf95ac4c      1
     93 0xb005dc24f04f601d43f958392051b42cd3f12083      1
     94 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
     95 0xb45bc276ebb2406d79b0fe5efc04254e84447f42      1
     96 0xb622ea682c54b56261c4d393fb165746ffe14010      1
     97 0xb62e9f2e6babc43f2e0e38fcf951dc546706b191      1
     98 0xbe15da6d570edc579bada879e0fb25ffed3f73af      1
     99 0xc015d9a92bb97f5e1cd8b0b1ffefc6b8ea94fc02      1
    100 0xc142995f05436c91bba172b7d17522a301323236      1
    101 0xc229d7d3dd662a1b107e29aa84bb0c8ff609cf3a      1
    102 0xc3feb2e38750b847fc5f73c40f02b389183ce905      1
    103 0xc723f9bb6c7e8077e4f34942377280ebf098f9b5      1
    104 0xc79fee930f23e235bbfc6508e11589eac11d6263      1
    105 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
    106 0xcf5988cedfc2cbe576fd486edeaee513bf6f1fea      1
    107 0xd14a49a9689459f932b0d29b63bf39fff8268771      1
    108 0xd52fba65a52214a65c78dc7f95c9fee1f13a5955      1
    109 0xd60ca521c64ac27d31f8dc5a0a568ca694009232      1
    110 0xd6408ab0f0de1cdde1355ad0a8db980e6a06e2b2      1
    111 0xd70650cc0e0f9238bcccb14f44db775f9740c9b9      1
    112 0xd74bbcf7f1ba5b56ca2672149294a379f787e790      1
    113 0xd871e77ee04d0753438868ef562389262a38710c      1
    114 0xde06c0e9ed2f98e630363f35ee174296fae2f48f      1
    115 0xdfe279c336e767beb9fb1c19c1695e2000a2c720      1
    116 0xe1e57c6d779adea0f34cd3b84fb6bae3a54d1cfd      1
    117 0xe20fb5ae209a1eaebaf6cfb50be12e528b0e0d84      1
    118 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    119 0xe8f704418f3be2a20d38e3f1cf7531e3bfe364c4      1
    120 0xf30f05253b242ef85514ac65dc261d01d91a8188      1
    121 0xf3ca6eff1d1dee751c37dbf4047e35a9081868b9      1
    122 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
    123 0xf51dd21fc5ead51c10cb6266628a39027492a65e      1
    124 0xf9caa0e790a9a89fbd84e0cf1b455eeb1dc50d1d      1

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
