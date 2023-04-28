
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:32369       Length:32369       Min.   :1   Length:32369      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:32369      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17136069 # https://etherscan.io/block/17136069
block_hash <- "0x622a258e16d2e6c9b5958783e0298808f38c372acb908997e5cb6ac04102fbca"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4526 

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

airdrop_angela    <- pick(snapshot, contracts=c("SuperRare","Foundation","Foundation2"), address_remove=address_remove,address_pick=10,address_max=1)


allow_angela_phase1     <- pick(snapshot, contracts=c("SuperRare","Foundation","Foundation2","Niftygateway"),address_remove=address_remove,address_subtract=airdrop_angela,address_max=1)
allow_memes50_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=50,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)



allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_angela) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0dac0d611db5955bbe881cf8d75d4c80271cae83      1
     2 0x1a3bfc4ef279975059221e5285db047905165a5b      1
     3 0x25b12eea057708adcb521ef7da0d4112523372fa      1
     4 0x2a6b17667bb9b1f9263982b05225ef8b8c76db65      1
     5 0x607ec4eb37c210f4f94542482fa1c64bb6cb9dd0      1
     6 0x7fd7e17bb15ceaf850376453dea842386f07aa12      1
     7 0x82139687faae8a29851902783e02e699de0e0846      1
     8 0x92767f92d180d66c6420d90fbb369fb12b7398ae      1
     9 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
    10 0xb6fc90c84b3f3acc84973123c45c93b7db8606a6      1

## Allow Artist Phase 1

``` r
c(allow_angela_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 36 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x07081c03375900e92a77311c48165a7d70c901d5      1
     4 0x0eb5447831105f0e2f9756e9e32caea309919cfa      1
     5 0x31cc0de78029e99da9c1f5755ccd39684740ce35      1
     6 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     7 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     8 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     9 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    10 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    11 0x6365031aa79ad92a6e5014d8b0698f13bcf08b00      1
    12 0x741a302b74c773a2c5f0d8812f353176f54fcb2f      1
    13 0x7635daa2c1f2f9d4be9b8d91ebd25001f74b78a4      1
    14 0x76b045846db3c0bc9a63b552a32e7542704bca90      1
    15 0x76d078d7e5755b66ff50166863329d27f2566b43      1
    16 0x77e62fb482027eca49b160e6f7b699fd3621f68f      1
    17 0x7b8a6e7777793614f27799199e32e80f03d18dbe      1
    18 0x80bddfc2bd0b7c7fbc9691859948060c5bf86d59      1
    19 0x82f23de5a474fc904041c357576afe53c30dd250      1
    20 0x95eb24f50de40058b460a6ba195428f729abca32      1
    21 0x999933757ebba904757dc18b6f863930951cda74      1
    22 0x9d10a9d5d666568e2a0e251ab08be2ba1876e0a0      1
    23 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    24 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    25 0xbc871a67e5e97c651be7eac8418d4f9a17debe3c      1
    26 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    27 0xceb72dd636147544f72784fe8c9f469c606a213f      1
    28 0xda6f9c53acd07513a618d7f73d681b8a0061c8df      1
    29 0xde3b81bce794f0031292431bd029df68931c030e      1
    30 0xe0d209b7324d982bacb757f724ebe00a9792ac28      1
    31 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    32 0xeca312bb2ba53ca9c271ed4f945f030eb9ff65b5      1
    33 0xeef498f448b2efbb35017118a6013019a84892d9      1
    34 0xf409188b865d08d70ca4720b62ffdfee501ce951      1
    35 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    36 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1

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
     1 0x0266dfd843efd2bad7e3875ee47b46b37a8ff6ce      1
     2 0x0755fba838d560b3e2bb41a9747e4be44824ee1a      1
     3 0x1e7ba17cc3f031a0c5795739317f6b2022ca39f5      1
     4 0x205819b6f631c73c36f899b2f58ef996d713a1d3      1
     5 0x2e0b873d26f6d90f347757ed0d041bc65e02a89f      1
     6 0x2e10fec445f90ffb0be093b133a6f68e55d95764      1
     7 0x368dc50bbb5928fc72847af3bf10ba5e4a9b322f      1
     8 0x38d779b6dc61acdf864cd289f3594ad05088df95      1
     9 0x39fbe769de5c86daba06d3ecf889f2812b8038a8      1
    10 0x3cf5ec55ec96f649a05c3a380d8a2b972a04abff      1
    11 0x3d6301815a5b5188e50a90e268179447d1c58b70      1
    12 0x416eba0df5f99cd0c98226d883bc094a1f934c97      1
    13 0x41859d1ce2b9683abc75df4c5a4f1ab7b38326ff      1
    14 0x4435bc1299b1398fd58f129854de26d60ead6ab4      1
    15 0x4b37c9a6670b58fa7fe5ca6ea4d56c7e9d1bc245      1
    16 0x5170ccf3f96c73aaa1f560912e91049ca77aade6      1
    17 0x593620ba826321a0d063ffa5d1b991a021088300      1
    18 0x5b975e28fe5f763dea46246029147e83d69c8e4b      1
    19 0x5d7c9186e70bff5b65668db66ea7bb9bbdb81d3f      1
    20 0x620910b020fd12e7b1ff8c0d52eda66bf31213d4      1
    21 0x677f01b794374395cb2f4c7d85c49611d920f426      1
    22 0x685403a926846d4fd11ab93b75d5fff5a7187190      1
    23 0x7581a5a10ab7ab75d4cec577e4669893818fbbb6      1
    24 0x7797a063c4222714867ee97e61f53cc93e54fb60      1
    25 0x7f47a6f4b6b1b621b04aa1cab4bd75e17cc7e0b6      1
    26 0x7fb5cb066fea693bf39f9707b496514731232071      1
    27 0x82f23de5a474fc904041c357576afe53c30dd250      1
    28 0x85c0c09570c955f6e07522e9ad6041716cbbb7fe      1
    29 0x86cc7ad354441bae1304f55c1f6df8ba98d44af2      1
    30 0x8f8a1e5113b11926950185d74567dbafb0aece0b      1
    31 0xa342e537bea996a2c96864c31dd9b3d9e40eb2fb      1
    32 0xa56131d1c79a35efeedd812bb521b81c6712e407      1
    33 0xa66e796b33016963bf6eaa062fa81d4254f33519      1
    34 0xab9f44a5c2b5fa488302668e49c1cf473dc43d94      1
    35 0xb0cd2661a534a3fe22adf297476f2ced8cc56df9      1
    36 0xba9ae18995f5cfc98196850d40b58445c08cb485      1
    37 0xbf057af2c08cd61c9449de0b3efe826a438f2393      1
    38 0xc0fd0f8ed41be78b054ec7cab04b52ffc4d3f5b8      1
    39 0xc7b72bb1acdce3eb45f9b38f43cd31e4927c725b      1
    40 0xd8a010c840fa18cfbcf613b197dfb09888830a00      1
    41 0xddfc9f53ff7a0d0522d9510348d162479377082b      1
    42 0xe003256f4cc1ae9d7545326efe8f05f12f066c81      1
    43 0xe1779c9d409b2ab99a10433dc1f1394127732a1a      1
    44 0xe36067a4c51ff6cb7fb6f97d1931bb27a468960b      1
    45 0xe38e8198a4e87c5f0f6bf96d9d3cd3053010a5f7      1
    46 0xe768687cf9ff40a1319f4847d3afd5b9fb6a4700      1
    47 0xe82d3382efe2798d4abd2d824824990ccb75e4fe      1
    48 0xec6aefe1d661ad80a359533e04daf05101c09c2f      1
    49 0xfa75e74d9c12bf8c7844101c06614f1d87e8eaf4      1
    50 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1

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
