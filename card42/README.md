
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16114269.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:59974       Length:59974       Min.   :1   Length:59974      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:59974      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16117942 # https://etherscan.io/block/16117942
block_hash <- "0x4772038b5ef4b30401d5bdc2160a1832b899882ab60ecf8a3bc7aeb60724c035"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4532 

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

airdrop_gradient   <- pick(snapshot, contracts=c("gradient"), address_pick=79,address_max=1)
airdrop_memes1     <- pick(snapshot, contracts=c("memes1"),address_remove=address_remove, address_pick=50,address_max=1)
airdrop_memes2     <- pick(snapshot, contracts=c("memes2"),address_remove=address_remove, address_pick=38,address_max=1)
airdrop_memes3     <- pick(snapshot, contracts=c("memes3"),address_remove=address_remove, address_pick=50,address_max=1)
airdrop_memes4     <- pick(snapshot, contracts=c("memes4"),address_remove=address_remove, address_pick=75,address_max=1)
airdrop_memes5     <- pick(snapshot, contracts=c("memes5"),address_remove=address_remove, address_pick=413,address_max=1)
airdrop_unic       <- pick(snapshot, contracts=c("UNIC"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=57,address_max=1)
airdrop_nouns      <- pick(snapshot, contracts=c("Nouns"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=9,address_max=1)
airdrop_mfers      <- pick(snapshot, contracts=c("mfers"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=168,address_max=1)
airdrop_moonbirds  <- pick(snapshot, contracts=c("Moonbirds"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=197,address_max=1)
airdrop_toadz      <- pick(snapshot, contracts=c("CrypToadz"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=123,address_max=1)
airdrop_cdbs       <- pick(snapshot, contracts=c("CryptoDickbutts"), address_remove=address_remove,address_subtract = c(airdrop_memes1, airdrop_memes2, airdrop_memes3, airdrop_memes4, airdrop_memes5), address_pick=54,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     7 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    10 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    13 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    24 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    25 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    26 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    27 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    28 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    29 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    30 0x69e68074f1aada957edd39c5eae0069973343f30      1
    31 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    32 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    33 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    34 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    35 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    36 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    37 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    38 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    39 0x82139687faae8a29851902783e02e699de0e0846      1
    40 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    41 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    42 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
    43 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    44 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    45 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    46 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    47 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    48 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    52 0xb5374cac6cad6b025246f19d20b0d4151b640558      1
    53 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    54 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    55 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    56 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    57 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    58 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    59 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    60 0xbc30e90dc528ece58c1a51b6fb6d572838416489      1
    61 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    62 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    63 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    64 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    65 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    66 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    67 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    68 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    69 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    70 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    71 0xdb561a899557404581e6180fe6d4178577dc117b      1
    72 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    73 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    74 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    75 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    76 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    77 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    78 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    79 0xfd22004806a6846ea67ad883356be810f0428793      1

## Airdrop Memes1

``` r
c(airdrop_memes1) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x144c704bf25f1865e4b24fd6596ffed7d92470b0      1
     2 0x16dee223fc168abff7b979813cdf15866eed7e8d      1
     3 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     4 0x1a4370fdd7173d0a41ff7c63a8e0249479ba0225      1
     5 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     6 0x213f6b2680dce3e4d0924acff3e4e34520ef9ba1      1
     7 0x23602ca06e977c86339ffddad74966e824ab691e      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27fd36e07bca70e64bdc1425e6bb5383d5089b62      1
    10 0x2a42cf4e1cfc33850fd8d8da24f86cd89b82ad95      1
    11 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    12 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    13 0x388160a99390392278afdba240046b8b5e73f77b      1
    14 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    15 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    16 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    17 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    18 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    19 0x54913cc8ea17731d62589039dd0152f306473843      1
    20 0x5c95598fe454882fec1139c3ed1284255b49c8b3      1
    21 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    22 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    23 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    24 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    25 0x69e68074f1aada957edd39c5eae0069973343f30      1
    26 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    27 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    28 0x82139687faae8a29851902783e02e699de0e0846      1
    29 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    30 0x8874174a2366668d54fea6343f71709389563c8a      1
    31 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    32 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    33 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    34 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    35 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    36 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    37 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    38 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    39 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    40 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    41 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    42 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    43 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    44 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
    45 0xe22059d454c705f70ffe2a5706844a3a27a2bec8      1
    46 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    47 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    48 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    49 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    50 0xf1f476f144df01480297dca47efca565e8b0c9f1      1

## Airdrop Memes2

``` r
c(airdrop_memes2) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 38 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     2 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     3 0x17e31bf839acb700e0f584797574a2c1fde46d0b      1
     4 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     5 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     6 0x29273917eb777b8d227e64ea423d07e6246088fd      1
     7 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     8 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     9 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    10 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    11 0x47a79ee280b4a6a8d68a3ba16e4082a765a2b20f      1
    12 0x4c8a8c3fcf77f37101d25930e7a086b4e0ec45ce      1
    13 0x52cc2c8db59411310e130b47d478472d9f7e4597      1
    14 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    15 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    16 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    17 0x69fde561275b85dbcd5081d1121bcae64fb83858      1
    18 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    19 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    20 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    21 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    22 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    23 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    24 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    25 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    26 0x9b96980c1c018cb617f6653f6892e19ecf4f81e1      1
    27 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    28 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    29 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    30 0xb62b86353c9c38665f2a3843ea4eb6f7eef9e5ec      1
    31 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    32 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    33 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    34 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    35 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    36 0xc7a295b1b2df1efb82aa204958b98ac30171cb85      1
    37 0xdffd6968900b3f4f667b69c5f9df65d9d7796a1c      1
    38 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1

## Airdrop Memes3

``` r
c(airdrop_memes3) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes3.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
     2 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
     3 0x1b84f9899ebc7d495b70a40ec16b36847f388e7e      1
     4 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     5 0x237a86eaed0df656d8bc4ae619760ade5b4705f7      1
     6 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     7 0x2d0ddb67b7d551afa7c8fa4d31f86da9cc947450      1
     8 0x2d1543711bd262108fd8a1afb43d8871f76c134c      1
     9 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    10 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    11 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
    12 0x4e21d3cec38509cf7a3ff9ec53c32be893e9f2c8      1
    13 0x51097b2224ac6daf640206f31d437faeee951d54      1
    14 0x60acf8d95fd365122e56f414b2c13d9dc7742ad7      1
    15 0x614b89f072ea263a9387460963142e73548fbaf1      1
    16 0x631d96ce9876d48cd2300473ff4124f5e0e084e8      1
    17 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    18 0x6afdf83501af209d2455e49ed9179c209852a701      1
    19 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    20 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    21 0x753c892626c4fa09d1aedc0e9b52bda97a4afa00      1
    22 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    23 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    24 0x7b50b1f2634fa38d078eb067a4f8f22a666b49a2      1
    25 0x832e47e53df5c771be86a3e03b49cde53088b156      1
    26 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    27 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    28 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    29 0x9e32560cbc32e1ea4c737d65f043a0d8f8310422      1
    30 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    31 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    32 0xa4df50f02a778bf281ea0db761900d354449eb17      1
    33 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    34 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    35 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    36 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    37 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    38 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    39 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    40 0xcb85b581caa71949784d2e826cf9df391c244b33      1
    41 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
    42 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    43 0xd69e257ae6088b717ae6d2ddec9297703b4fb725      1
    44 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    45 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    46 0xe25b24cebed3055236e369570a437a99e1d32602      1
    47 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    48 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    49 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    50 0xfdd41f8325e243f200a34cd2fa28594efd5e4c6f      1

## Airdrop Memes4

``` r
c(airdrop_memes4) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes4.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
     2 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
     3 0x047c4d7af709ae19d3bbbd5f9e10461d2554da00      1
     4 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     5 0x089fce8be711cabc806d69b1d0d5aebd52a06455      1
     6 0x08a474203d14053eabc30a5bf877bd22a6554f0c      1
     7 0x099a3fc56b898696764e08829e6426fac2308cc7      1
     8 0x0af5cfecd1b1be7cd808bf466470db20cb65c51d      1
     9 0x0d69a096b2c66310309f0ead1d6c97c4dfe87086      1
    10 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
    11 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
    12 0x159e5998669ec6628a6b670c2ef1ddbd93084698      1
    13 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
    14 0x1c3c1c24d4cf6b2a73f4bbeee33d9d5ec8048aa2      1
    15 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    16 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    17 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    18 0x2da903666829f302b0501f76144339213259c260      1
    19 0x2f5fc65b9c6d1fee8ba4ca23fb2d7c363c95133c      1
    20 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
    21 0x37ea6c993c5f42e1fc3455d5781e4a580760970a      1
    22 0x40e6d6e798dc878e12987ed978f23c2391f1f570      1
    23 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    24 0x557c60995797fa7b47be105227a2e46148d85750      1
    25 0x5bedb9331ba476e786f85849307d1164c3e37f84      1
    26 0x5d25087405105bab12624c73488ec186066a6376      1
    27 0x5e3118e91b0a02a5e2b92e49700c63434e94f0b2      1
    28 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    29 0x616ed054e0e0fdbfcad3fa2f42daed3d7d4ee448      1
    30 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    31 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
    32 0x6666083ba28027fd6db24a3c1bf288c0f3f95807      1
    33 0x70e680b9493685f72e76243c09993fca768eedf1      1
    34 0x79fb29852d012e00ebab0e253400ddb189475ea2      1
    35 0x7af061a6ec9616135231cb8de2ed2a0a4140e90b      1
    36 0x7c8f072015d4e29c24088fe55e62381406bd71ec      1
    37 0x81a2c6f2676cb7a2e17ca83cf702a633c71619b9      1
    38 0x85914d049aa004d037cae65bad275ed4147f172e      1
    39 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    40 0x887b86b6b6957f7bbea88b8cefd392f39236a88c      1
    41 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    42 0x9613a1f63bb8c3cfe6eb36cfd7b731e5fd07d322      1
    43 0x9e23d2db65a1b8b6f31fb57c47148907545b3ff5      1
    44 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    45 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    46 0xa883b60c5ead0efd604b41a7c0509c3c6b81739e      1
    47 0xa8879c580a54f190ed53b43d30de269097ad7543      1
    48 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    49 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    50 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    51 0xb2e7f7cf519020c8b6ff32a088fec95b03ccc715      1
    52 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    53 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    54 0xbc5a7cf988f7e7e8f3893cd0ee1d0786cd4af889      1
    55 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    56 0xc9ceeabc1ac7b96fc45a1c94edaa3b10197cedfa      1
    57 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    58 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    59 0xd1386560f0fa070b6b79e9968e8197cf17f3b8ae      1
    60 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    61 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    62 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    63 0xd41df4a30fbe6b282ed6294a704fb33090557566      1
    64 0xd62af0477b1991197da39a38cbf9c7199f4737f3      1
    65 0xdbeca36c48306867d9285e0e0d5ed1f5aec28a35      1
    66 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    67 0xdf1e3abb229d42a182ad61ce8a63355a8a3eb0f8      1
    68 0xe28470253f0c9c7afbae7f61795d6b1ca4644b2f      1
    69 0xe8eb234375d59df64823ffda70207e26334ceeb5      1
    70 0xeb0c4e7244100075c227cb60ea8107cae39c3211      1
    71 0xeb9d9836b14bb9eef7cc0f40f87f83f6f959cf52      1
    72 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    73 0xef04cb0003ca57dc7bb563d5ea3248241b145bdb      1
    74 0xfc7d068f069e712dd35b439bf6ed9df8f58d07de      1
    75 0xff7e887f2b7c7594e601ed1c2e44b55bb80f774b      1

## Airdrop Memes5

``` r
c(airdrop_memes5) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes5.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 413 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00baeaa84510584a6e4624964b75321ddb6766cc      1
      2 0x00e484da1156202e9dd341ad7ea9c908bb919e96      1
      3 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      4 0x018f5cbe8a37b03f5bf733016fee38106d39713a      1
      5 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      6 0x034c44c29c6ccba94c9bf10eacc5a3c66404cbc9      1
      7 0x03ee832367e29a5cd001f65093283eabb5382b62      1
      8 0x062164d0c571002b9b34a9a71ddaeeb0b6f93132      1
      9 0x062f25c17e026104d8e5d4dc15e3fc70d4a51d24      1
     10 0x0637213177486b93a4198719a49751d77af0b43a      1
     11 0x07219c7136f49847a6be9fd1ce27f8afd6a074db      1
     12 0x077e3a0e5018baf7c5f2fc6ae0e8ba2b7962dd3d      1
     13 0x07b7b8a6977cb5e9823167831e8cf3397fc59283      1
     14 0x08e146bb525bd8391a4e22e665325de0f499e9c0      1
     15 0x0974ff7e79d93c462421f23520c0e6ba300dcb97      1
     16 0x09c9b389c532bc182bdc15ca831edfbcf6beb955      1
     17 0x0b6eae3681cc726f6d8ad39c38d7fb89018e1c26      1
     18 0x0b983c0502ccb2dc598847b467a1b36c1ae8d8c2      1
     19 0x0beda5116cd204c428379b5d852dadc04f3bc384      1
     20 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     21 0x0e0e34094e42f47fe25da3cc1b441faf15f8551a      1
     22 0x0eab1c845c03f2eb67e43428e7f5b06a278e0880      1
     23 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     24 0x0f189ad88e9cb528595dc379357bb5e4bf92353a      1
     25 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     26 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     27 0x0fe61de274aed1803e66ba1b863d966079cb9116      1
     28 0x1010595f96ab62b31bfeac411ec5f8f60db5dc23      1
     29 0x107752288b215467de25c16119787d715ec8e26e      1
     30 0x10c9b09ad7d9f2f686c157effca2cfdc8779f49e      1
     31 0x11b7b38d5b96d02d6381e646ad8ca55db74dadc8      1
     32 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     33 0x15237b9fc8c244246abee701f07e42185c5111c3      1
     34 0x16feaa6e54cc66a87fe292125740a528f133f4e5      1
     35 0x1a4385c06550076264ab8d98452c7c55f47c9814      1
     36 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     37 0x1bc12afb82b6a44b4c4cf2702f2574d90273ac13      1
     38 0x1db8cec74589448f3e427b2d7f90e7d7e13f0927      1
     39 0x1dd789345c1bad32ea2c3dd8989f38e5362f4f31      1
     40 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     41 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     42 0x1e3a64ce9b4a573674284ab590b4fc538746fa21      1
     43 0x1e5fe56aa8f66909fc7f2239749897e171380b65      1
     44 0x1ec9dd67af275fa69b6ecf7e4c3c4c0968a28fa8      1
     45 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
     46 0x1ef710af2d02018cfc96521f83e54a516a6ba765      1
     47 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     48 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     49 0x21301d901db04724597d1b6012ac49878157580d      1
     50 0x2181faeb13f51aa1e236069296372756f6dcc0dd      1
     51 0x23cd0c136f4bbfac9c23eebd97d5fcfaf1230582      1
     52 0x248a063b143be4acb88b61828e9e90185c4da6d4      1
     53 0x2504407012559909eb9ea18273d2617d0c658243      1
     54 0x25eab155483a132d058a973de055cf8168dd7d5a      1
     55 0x260a5ffd75a1484982a4fa2ebd5a8ce7102395a8      1
     56 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     57 0x26e7f147daa6eb33458ee531c6c22e34c7adc4c1      1
     58 0x2762c7fe932b8688508fff24ececbbca92a128c4      1
     59 0x27bc503afb91d9e7fef952446e6ae847f4963cd2      1
     60 0x2a132850def7e157e5e36c3df337d29e9e8f91c7      1
     61 0x2aef0a84308fd2f40eed5260b126411e371e970f      1
     62 0x2b2c869539188c28c202d086fe99c554493f72dd      1
     63 0x2bdde9adbfd5e4982e42717727bc5ac4da3e1d79      1
     64 0x2be21793155e0d5b3dd715c348f7e712471e0873      1
     65 0x2bf308d324985dd121c366fd3fc398ec19d6651e      1
     66 0x2c4376b9414fefecd89c9b770416f7516e8af863      1
     67 0x2c4423646c318d06d635c168a788c42d8c33d51b      1
     68 0x2c4a6fd81e52d256ec34db1e03a9af627b2d62c5      1
     69 0x2cd198f6884f0984492bce36069a3fdd5b119609      1
     70 0x2d3fd9ee49189959368448a5cbc63213121d66b5      1
     71 0x2de5ec82bdb8bc7662e9994fb24ea6e6f79d9d69      1
     72 0x2e31db5f6262e174cb8674cdce036dea306bfe00      1
     73 0x2eea4cc2322c6959f2c2133e2b6c1ca5e38ac241      1
     74 0x2f1390ec03a03e57a1406bdef57c9cf487f62b78      1
     75 0x2fa7854c06ffaef353b526b05e424a9a3fd7a4b7      1
     76 0x2fe5b1e4a32e794959dfc5261b1dbfd9bffb982e      1
     77 0x30d6630d31171745017c8ab7cdcaae42bcc4d748      1
     78 0x31004f28babda1f632cb445944892b04a411e7f7      1
     79 0x31aca8a351e67696eeccf0b7af03d41ee80998eb      1
     80 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     81 0x32ed7ce154b816d2d71ccac6b384a5261aeacf60      1
     82 0x340ee74b7257c6b11b7bf47fd279558ea9e143f8      1
     83 0x346b54cdf75cd6dd2f41f5557695f210253a6e94      1
     84 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     85 0x35d1a3dd6c50deaebb7eacc6077a586b5dde09d5      1
     86 0x35f7f29f154a28674c8a50d435384744b02cf42c      1
     87 0x362cf4be53248d0872ccbfba570254c08e2f3d09      1
     88 0x36dd9e834ebcf94efda21b142577636b62d4770e      1
     89 0x374c78ed12692c076c620efed38d02014746fd32      1
     90 0x376f96833b85f5a212adddbce0d172340621d1c5      1
     91 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     92 0x37ae7b1bb67190c1a0eea775d62df884402f2b4e      1
     93 0x37c196fbd9a4355af684ff988ff4a17ccb181ebe      1
     94 0x38090615458c10d067dc19b5f1ccf7d4b4dd8bd6      1
     95 0x381334a57eb45ee79183ed126c7a688d600240e3      1
     96 0x3818fd9c204ef253c5710e80f2f769ba66ca426c      1
     97 0x3ae8d9ba72848bf5356f8cc56450532c9bbaf9fc      1
     98 0x3bcfd0fdf680ff94dcfb9953b5e6058af531246c      1
     99 0x3bdc75b240aeebd7a266edf867b8ad5c30760a3c      1
    100 0x3bfcdda69bfef9ab5da3c3e75c0ff0e0a3074a5d      1
    101 0x3c28adf9c1511893d9b1477cdbd12278245674a9      1
    102 0x3ccd78c99ac513b2a06e2dffa21b2d5afe01eb8f      1
    103 0x3d13af1580148b96a095d7ffa85c84d6ca975cc0      1
    104 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    105 0x3d7d85485b37360c7ad7ce17e549fba5473a0c0c      1
    106 0x3dc3be0a9278028fde0e3ff65c6154e2e36dee9d      1
    107 0x3f701d50e68faae2b49fae9fc1262bbd360a9ea9      1
    108 0x3f849f47f5b372d80407e442f360ad7b17f5fac4      1
    109 0x3faaf95bcb99c3af12a7e77da7f59d25f77152a1      1
    110 0x4184f399b416f5d2ae4fcd9a842253a9896e73cc      1
    111 0x41aec789f14e50b2628fc2fbcc29f909d9a97236      1
    112 0x41b68202dc64904e09d2e4f0a94879c741f27db2      1
    113 0x41cdf42b4d22b3edfea997a2f03f32b156b135a0      1
    114 0x423232b60d97d4a989a47ee5fefd47c8dc4b0e1d      1
    115 0x42617123b481f61df6b5b8a6484ba0a4e6929279      1
    116 0x429683282e5a28b81aab1f9126b8d5e4847dfbeb      1
    117 0x42a0cb5083883e48753fd7e286db9b40c5f836b1      1
    118 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
    119 0x4435bc1299b1398fd58f129854de26d60ead6ab4      1
    120 0x455867c5b24bf1a29ee02e0195b9ff36bea17ca8      1
    121 0x463c1103c366090f54d0a806c8cc885aa293bcce      1
    122 0x464278144ff19c4939f63418b79f3e6a239158c6      1
    123 0x465ff8830cc567452ccb449f03d120d17e8edf5b      1
    124 0x46abfa031be839b1599513887a27a403e8d6598d      1
    125 0x46d43db499b97e4933d43989d619fc4e193b67af      1
    126 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
    127 0x482b9ea86052bcf19ec00a13188abd9522398911      1
    128 0x483388c064fdcd036ad9df3807332023fb1cbbe7      1
    129 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
    130 0x4839f0a75ede31d26613c7cdccba89be774e9856      1
    131 0x48bb7d2efb44e76fdf95820c7eb04d63242cfeab      1
    132 0x48f044229918cf94343b6a37f657998686d7fd7c      1
    133 0x496e2d69b25c130c6ff71b27c5beee5613eb1dca      1
    134 0x49fa36c6db702b950aa1d241924fc5200e2321f9      1
    135 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
    136 0x4b7bc0032accc405701324d72fcfbfcb7c3cd763      1
    137 0x4bae43a2c32978c6ed56fa9500917dd47664b44c      1
    138 0x4cdb9c3499f31ccb63da5374877ee2111440f648      1
    139 0x4d477f1aabcfc2fc3fc9b802e861c013e0123ad9      1
    140 0x4d6227fcba4c25feac2b6ea347ebd7851b781edf      1
    141 0x4d855291488daf31305a0fb436b39506b21f21e4      1
    142 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    143 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
    144 0x4f4bd77781ceee4914b6bc16633d5437eabca5d3      1
    145 0x4ffc5f22770ab6046c8d66dabae3a9cd1e7a03e7      1
    146 0x50cde770461ef53b62e083313d64b5b274b4bb78      1
    147 0x511b2eaf8bf2944968374f4d5f705573c00927ee      1
    148 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
    149 0x51661d54e0b6653446c602fd4d973d5205f22dc3      1
    150 0x51733185d7d1f5f32c2bec2bf120b84b8d284a70      1
    151 0x5179e27381a79dcbd2c5d0279af7ef1a8c46f854      1
    152 0x51fb095c8d7ff783cf884d011834192c46bd9398      1
    153 0x52147b125db7595bf949ce28b2330365844dc9a6      1
    154 0x523931dcc31e1b202ad6061bb76a360bc3d0a8e3      1
    155 0x527136cf29a1df9024a6493f219a1da64398fdc5      1
    156 0x5328cd117d42fbeb105794b5cd994df420065462      1
    157 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
    158 0x538f08b2448d69d85b312274b15faa468464939e      1
    159 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
    160 0x5462d644992ee95fe2c122a851e8f36ae906f61a      1
    161 0x5591f6258fd9eb2ad0d3a4e88245d06e1e8661bb      1
    162 0x567b71b80fe93afe1db3581e6faf5f00fd483885      1
    163 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    164 0x57d5cd5d0fcf182fb46fe0ef7c0ec4cc3a4bde79      1
    165 0x584aa82e6ad7ca8861d20ad73ccb58e7d167eb6a      1
    166 0x587d714453eccd6f49818da4c17121c092506cce      1
    167 0x587de9b9f765b056999fce807f310c58aea97c10      1
    168 0x59405cb53fb84706f3c4b522c57d71f125fe5182      1
    169 0x5b8dcbad4df5e00c20c35a0339f12259ba40f2a6      1
    170 0x5bd8edd2cc0688655698094fe8e2882982889cb7      1
    171 0x5c18d25064b9abc166ec38d15408d5569176e8cd      1
    172 0x5c231f4d4019d93faab1d358f062ab1777e0886c      1
    173 0x5c509ce530f9ac85ffdf09c93c27654d0bc24b59      1
    174 0x5d3087d1aa04235610cad6198598977b9b8156d3      1
    175 0x5d3dd154c5e582b1b1ac90ebb0c05744038e32e0      1
    176 0x6177adacfde59c3ace55356250b6504488ac513d      1
    177 0x61e814fe997f0b2816fb9ac3c7df3aaa38d8ebb6      1
    178 0x6270d239ac3dd7ba9192367e50780031c82c6be1      1
    179 0x6273c7a95a5a205f06780886943be548c2a882f6      1
    180 0x631ebefd35fa59ac01e9c0514cdbcf9cb70b18bf      1
    181 0x63f51cd8ac4757d9fbb52da4e0b467f4fbc23a21      1
    182 0x6576a26763cefa8f37005c067f7902960dec6b8e      1
    183 0x65a5c6ba0f55c1c0e333bfe0db783aa07e8310ff      1
    184 0x65aa8f1fb08111d2114858f26301556247c5caf7      1
    185 0x667680be3606a5131ef90f055ea3740d84eee721      1
    186 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    187 0x6790e906925f6f35d81588af6bdb07b50ed0e436      1
    188 0x6839c96cc542806d378e2c7e1e7ef12c399dc8ad      1
    189 0x685403a926846d4fd11ab93b75d5fff5a7187190      1
    190 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
    191 0x69217947efbb9684c32bf2211185e6684412bed6      1
    192 0x693d28cbfb6a6788dfa8b034f75d9ff21edda9f0      1
    193 0x6959177e16aef00c42030649734cb06aa50689df      1
    194 0x69a463ef66c4966e8ce368ee050fe2d0c79deb02      1
    195 0x69c38c760634c23f3a3d9be441eccbd2e50e5f73      1
    196 0x6a17d3dd92a8d8fdf8a1f27c36a9e046121fc69c      1
    197 0x6a93af5fed6f11bc56cb66a5c199208f2e02380e      1
    198 0x6b64c16bfb518855d1c6bb7a92487dbd9cc13b45      1
    199 0x6e074612ae417ab80a574fa5fc78d21765fe2f30      1
    200 0x6e81af1bcd504a51da5bd6a0d7df70d7674ce90e      1
    201 0x6e9075da365be17041e704a16d39652a7a54b862      1
    202 0x6f74d4be637b90cc02e03cec3f0fe27ce8593c70      1
    203 0x700fa0b05cd310d4e2ad6df00edeb45ddacf48e1      1
    204 0x70c9786568ec1cad07b0b47c24bb6861d4cb5613      1
    205 0x7120a2b756ed48b6f433f70481b16b353df58b88      1
    206 0x7143f19d9df476227013260f015b06359918ef3f      1
    207 0x715fd9d95c5302e4f7febd8476e3f9c6db4c918a      1
    208 0x72406fbc5e70814b8ab21c9f383702eaf005ca72      1
    209 0x72c33c85107685728da420ca77e390aa0a07d681      1
    210 0x738027f2d60d05b3b136893fd9c97a75f737bb8e      1
    211 0x73b5f3bf3d5f3fbfcda7c35a5d4234dff824ae24      1
    212 0x73fa5354bbcab713090b9a28153435a5674a81cc      1
    213 0x745147f70b5912e6dd134e0502b3914e6f6d98a0      1
    214 0x7611045510c7a3d150d6752d106f0b4f3991ee20      1
    215 0x7624f7f6418a12fc4f87c9c04f913c1f9ef674ed      1
    216 0x76742f3a5853df8555f080174f2bb94a16926863      1
    217 0x780193b15b2db9b9f4846dcef7ebfb3e2b751a8a      1
    218 0x781264bdcf075c3bb26aff8c2143801580c03052      1
    219 0x78887448976b93443125cd9ec40a9485684c759b      1
    220 0x78a4b60e4e66a4c2a6d9535dbd037121e5788d9f      1
    221 0x78eca41c3085bf0eca6470ab202f51510e5f86df      1
    222 0x7937e14b74fcc147d0c27b5736fe65a83e40c85a      1
    223 0x79cb5243e06fc6bae0cf2eb96c74d6c959312062      1
    224 0x7a2af8b20345f59ba1f119e7cc1a1dc185aa031b      1
    225 0x7ae4a3281b93f4f3352f7722c24f7730c0fb182d      1
    226 0x7c00c9f0e7aed440c0c730a9bd9ee4f49de20d5c      1
    227 0x7ed4e9ba501582cca3cda90ee017bc2c26a773de      1
    228 0x7ef61cacd0c785eacdfe17649d1c5bcba676a858      1
    229 0x7f270624480605ef2a2212eed24b886aae1e2e7f      1
    230 0x7f32973cfdef197e56cb5ddd2c9412505a629c92      1
    231 0x7fee3d50ae036f3e72071ddba811f58472995edc      1
    232 0x80915e89ffe836216866d16ec4f693053f205179      1
    233 0x8181dd699e486493027e4e21bf6d0c7b7c94055e      1
    234 0x81c2eee2902eda6db28a448d8a814f221718ba2d      1
    235 0x82abc1094e0a91d242db3d6dfefa33f423e6bbe2      1
    236 0x833eab3f58cf58323b8e133cf69503698c3a21f1      1
    237 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    238 0x841120ff6d1e9f29e4ffbdb50eda696bbfd98ef6      1
    239 0x8447e981341dd44f7a6caf26e6452d3fb12d8592      1
    240 0x8451b49f03c80b8d4d116090ddf10047cd173d55      1
    241 0x85ee400a599154468bcf7db6a762e446e593b9a5      1
    242 0x8601e92e12619c02f5ac3af0aafe9043d7c2d551      1
    243 0x86ba22e3e4a2ec1ae984e9a53e64001c7631c660      1
    244 0x89c8f05d27de16138ee1f0f4e973ff6b0c5603cc      1
    245 0x89ecdef2be3d9c6857c969cd2a1e29a578cae154      1
    246 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    247 0x8a9bfdc136d5d1166497882af5d8c9718f27fbed      1
    248 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    249 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    250 0x8c4138dcead987bb50929ecfbeb39ef3e28b4c76      1
    251 0x8dc0b2384ee8cae3a90a9a50b25199074faf46b5      1
    252 0x8dc287825d71f758bab052608ba8a4f156f84176      1
    253 0x8eb9a1d790e68b0c2ad6dd3945466cff8119d743      1
    254 0x9052e1b507c2b00ad3acdab4e47eb2dcb98dd59f      1
    255 0x91198e5b88051d7d61ae43f322641e6674697af7      1
    256 0x92a7bd65c8b2a9c9d98be8eaa92de46d1fbdefaf      1
    257 0x92c70f31efa47bd08b65f36079fba5668f3050c4      1
    258 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
    259 0x931d0dcc21410c0410faafc6b7913c59e948e020      1
    260 0x942d0f8ee4bf3011390fb33c8c43fee8285a3ae1      1
    261 0x9440ac78a82c97cac3e433404b541cd0ee6ef4ed      1
    262 0x949ba575b86a5a86c4190a33bf4a49ec9f704215      1
    263 0x94ede2010554cca011c8bf52dca82dbcf2f8f2ee      1
    264 0x95d7242abc063214bd0a733a5929239ef788a4fe      1
    265 0x95e8a4792d7e5bf6ce3973184400cc9c37b6ae67      1
    266 0x965822e01fa12eeda149ba86a72f67f2e3b39fcd      1
    267 0x969c65094269c90da10a9cb08390846fcbb40047      1
    268 0x97d635d9b8c4c7e4956bf3e900d3b26816e188b6      1
    269 0x986646495d18f93a20800049192df0a9c8bb026b      1
    270 0x98e9de3364f73824b06fc6531c42b85e67a4a98c      1
    271 0x999baf549d6807b2c713ca112d81a0818eb09b80      1
    272 0x9a4041dfc91b7e7816db55a6e1e91a5672d576b8      1
    273 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    274 0x9aa824a302597e3477d0435cfc5b1e1b9eb23449      1
    275 0x9ac0a2c799713233e8329e7267b2f7b4ecfb2c5f      1
    276 0x9bf10a9a1254edadef6a2a4a34e51b34a5928cf9      1
    277 0x9c44bf6f8a7291b7a04bebcac7c5cd7fa7771ab1      1
    278 0x9c844663e064b946d7bccbbd3c16aa6d16241ae9      1
    279 0x9f041fbbc6fd007115dae9bd1ce6001b26747797      1
    280 0xa0b574393d2da912e8d30b3723d82b4bf742ef93      1
    281 0xa14964479ebf9cd336011ad80652b08cd83dfe3a      1
    282 0xa1b32581ec3bb4c85f47806bfa8566ae08989870      1
    283 0xa25c3aac588c85ea3b5b4aa84d7f812f34354f66      1
    284 0xa362a9676db12606d339d10433c1af3618bca1d4      1
    285 0xa42098ea9ba9296a80a49c11e22a66c07a5e8212      1
    286 0xa7a7bdd730d69ac2d3215703faf03e1a5c600e8b      1
    287 0xa7aa7bd3f048097b635cdbbb7158110c3516f81d      1
    288 0xa7c3f481810149f3853993abc4e85c49023cd2b6      1
    289 0xa80ceef34a29f36f1d0b96e2f519936faccbb890      1
    290 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    291 0xa97204eb6b0f3e58e8acff48a2317511e7628949      1
    292 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    293 0xaa95257bebda7f2f46284d8ffb8b69d00e4abc53      1
    294 0xab7244011d76fe5e94a3ebab59715272f0150a7d      1
    295 0xac3357599fbfe80c24908a1dc00ff8323745aea5      1
    296 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    297 0xb0b3d3cb5d9469286b9f75824bd16a6bb133fe09      1
    298 0xb15de4aeb7a84282aa2541a0bdaf44d18d74060f      1
    299 0xb16525385824e8cf877be513386e75d875043ffd      1
    300 0xb1eb435fec01c94098a692a9930af6dc12f24903      1
    301 0xb1f83385711225cf381f1611ae09d74e8a8d0f0c      1
    302 0xb20898ad9ae01fa34fb6746de24dbd599c886e22      1
    303 0xb349aee2ceff036660cb6d8d59289c0bb6b5bd59      1
    304 0xb37a40407fa99bb451ecf5ac3e693abab0dd4328      1
    305 0xb3bf133950d87f84508fc64735ed40a4f2797aa6      1
    306 0xb40266773f74dc193e59f791c6e2f99f7d8dcb90      1
    307 0xb6037b6feb9406942649885959346755b3ad04c8      1
    308 0xb633497a4791f8460fe42ea9b85de41eb813c421      1
    309 0xb6ad61b7558c718269a9129bd9ed9839d88a126a      1
    310 0xb6ea44d485a6e1468830689bdde59a2a1810de83      1
    311 0xb731023122a845bb721ac4ff29bd0d1be25fe755      1
    312 0xb7ac5afecc5466b4f5888c601c7fc20e2b5bd96d      1
    313 0xb867ba86746b4b3268508b014a3fefd2a938cb27      1
    314 0xb92d81c5d52f10947e3f8947a8b9622d38d65056      1
    315 0xba0b5b765d3d638eb560c7e08fe17f360d8e5005      1
    316 0xba78cd28f7132958235d278ff3c5dc5e6d34cc15      1
    317 0xbab3c738620527f4614c0dbcf18411328555f24d      1
    318 0xbab9abed42ad306ca536be9f095d41cb9ec26f9d      1
    319 0xbb033b792b7153711bcdf8c9d3cf0f0e28896995      1
    320 0xbc3e09e1304a2ed5e72fba9cdf5d673fa82bdbd5      1
    321 0xbc74c3adc2aa6a85bda3eca5b0e235ca08532772      1
    322 0xbc94e04a219ebd4d12b881fb0d6ff019d15a8885      1
    323 0xbeb15cb4e910fe9e730c48f55a4de49212203d10      1
    324 0xbf6c665ba1429715654eaaa3a85a29f4861ee44d      1
    325 0xbf9d462c9c6880d44001bcb8ca4a38a351d583d3      1
    326 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    327 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    328 0xc2b3cb204e96270d763c70c75ae1a77c739b0e6d      1
    329 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    330 0xc53f977136e1aa9031343fad39dcb4c11a1eb3c6      1
    331 0xc66266b53618542db45be7f66dd8212e86d4882a      1
    332 0xc7354a20424f7800ce2892de0d1eb1eaeaa9d8ec      1
    333 0xc7b15ace7bc2cbc2a3e47f44ad272d7b752433b8      1
    334 0xc8ece128e77dfe3a3bbd2c7d54101f2238f8b611      1
    335 0xc933285888e27624210df72c4d21514e4a2b3203      1
    336 0xca33ea12e2a6bb8e5bdde2a102bc61d78760c96e      1
    337 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
    338 0xcadf672af45192a94c97ae2c0e5dfd2a74a8ab1d      1
    339 0xcbe5b92259fa3b33420fd2565a845ee53aa60a0e      1
    340 0xcc3672611df7393a84b4de0541bf71c7cf056728      1
    341 0xccd104aec182930e8e95a1d0fe5a60c69e2103e7      1
    342 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    343 0xcde7970b78ee805ff4b0c32a7d3f23e40bcc9554      1
    344 0xcdf70b6a000839b8be0472736ffc6a32e1c529ef      1
    345 0xcec0226557b7fa4842a37375fd75fa33882c1783      1
    346 0xcf92ffb816f65c8eed8bfd571e9c3eaed5180dbd      1
    347 0xd0e8060e52d8c23984f22ac899d18fc53aa8bcbd      1
    348 0xd146ac8ddc2520dcc60674722e776728958f9c91      1
    349 0xd1a25924d140dfacc8abca7e83ddea7e26ae0ee5      1
    350 0xd37697f3c50d7108fef25d16cafe229753224a05      1
    351 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    352 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    353 0xd4d365f2c4c136d0ba998565fc8cf28c5bb3d700      1
    354 0xd51e0ee811c61aa77e1198fd40843633e67f948e      1
    355 0xd57729ba3b5a1a53dbddeeab17e26c9cf3af5e59      1
    356 0xd5b1bf6c5ba8416d699e016bb99646ca5dbab8d7      1
    357 0xd706f4bc6be1e63c5866ac4198857f092bfadb67      1
    358 0xd73bd59e7c0d075c9a83cad272cf17c46f7f2784      1
    359 0xd761295d455e472ba07ecadded370efca55e6cc0      1
    360 0xd7dd9612a21f7c249fb7f33e9c2e9144345e162b      1
    361 0xd88fdf946aefb208ada91269c605d86114883581      1
    362 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    363 0xda21d116a3f1a001c3e3a300dd75422263c8dcd6      1
    364 0xda75f63b9cb1cf7668bb0ee34fa718bc1cb5bbc1      1
    365 0xdb11536b4b5006557626c9acef2da5fce5bc438c      1
    366 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    367 0xdde01147aac2152dd0776bc3ed84cc96474df52d      1
    368 0xddfc9f53ff7a0d0522d9510348d162479377082b      1
    369 0xde08b96895c99f9983c631f7a9b7d504bebeeb94      1
    370 0xde3c597480051d659a3611569a3affa367000d0d      1
    371 0xde500d84a2bb86911a84bb7c9eda4b4b8f9f08ba      1
    372 0xdf37ac79f8e9b698c4ab57d30a41e7f272331391      1
    373 0xe181725de73c2a58325e5b08af5524a7d81fdae2      1
    374 0xe1976fd4def8ef7a89ed3149239fd982daf6f892      1
    375 0xe239488f1efc96a27408ba60c102103d4d1909d6      1
    376 0xe2cacedac44bd674d9c5e816422cbd603db9cc1c      1
    377 0xe3c8cc0f15eea9c22ce51af8c2ec6e1978389e7b      1
    378 0xe50ae5655d227767ec3974a11da7e4f67476b96f      1
    379 0xe539a21fbdf04c289c167c3ff2f97754080c235f      1
    380 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    381 0xe69aaab4c32313191788d12753f016880690ca3b      1
    382 0xe6a9d02ed5ba17a74440b8d4bc82d1bcd698dca3      1
    383 0xe6c26d2c2376e8997057fdc9ec0076249a6851ce      1
    384 0xe6d50cb6e7423c742f409db65f19518018b45968      1
    385 0xe6e4d92009406d08851c2e65ce6dd324ad76a87e      1
    386 0xe79c263403b0071093bd7968ad1b6ea94d3bcf1c      1
    387 0xe91c4ec4a439ec9d6b11ef8d1df11b35142df7c9      1
    388 0xeaddbae2651e21b4ae261ffbfcc81970854c4ef6      1
    389 0xeaffb626014495a3331ed7539fd229893f57cb15      1
    390 0xed034b287ea77a14970f1c0c8682a80a9468dbb3      1
    391 0xeda91b88e63dea1efb3d53bc3c7fdf7551b7b726      1
    392 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    393 0xf0753b5515c095cdfce5a0d58af15dc5aa46fa94      1
    394 0xf0b95bc02c2e391f92a89a7657814453e4f9d76e      1
    395 0xf0c11838870ca444f5dc0ba0271a36bb9d9f81ec      1
    396 0xf182e15bea2f645cb3db36def2bdaf7bf933421f      1
    397 0xf1f1e267a26648cf9f39f0402ec75b72cb292587      1
    398 0xf2626eadce87913c29e63b95e39552e1bbe26b44      1
    399 0xf3a80df4e39486545b2c3e79a79c8500dbd71ae3      1
    400 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    401 0xf6ac9ef1d7f29d257e91dc3aecbdd4a53fe3bd16      1
    402 0xf859068452f14ca012f3b2c9604bc8edf3283db9      1
    403 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    404 0xf91bc13d8ca640f623d46a333dc8fa0d84556b6b      1
    405 0xf956541bc70b58ac2d517fc1e31d428cfed5a226      1
    406 0xf993d5474cd607e26b57e1de1556bee36de2d0e9      1
    407 0xfb74eee69be0c2bd8fb42ee67b6b6c4e05cd9ddd      1
    408 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1
    409 0xfcb33fbe33e1f0f066b41c24d91e994d8fd8e526      1
    410 0xfcd4f06d2d4e873e9379fdc95b8570d79ecfc828      1
    411 0xfcf2c8a6b01af46a4bfcadbabe49b98294fce61e      1
    412 0xff8aa25aa94faa65fb6e3ed72159709c4a993bc3      1
    413 0xffb8a15dc8796af3e7bec74e528f1bd810b854ed      1

## Airdrop Unic

``` r
c(airdrop_unic) %>%
tally() %T>%
readr::write_csv(file="airdrop_unic.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 57 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x013868846eb88e0d6b4c235547a9a3d526004c10      1
     2 0x059858cfdb4b8d47e632990e65c90cd5ff35ea1f      1
     3 0x13054f73973928edee0ac07cdb01d8bd8d791504      1
     4 0x1bf555e99b9056e75f5752ebf6593c4929bf5d50      1
     5 0x1cf25f9c307c3b1048e9a4ba0bc5053a99386f62      1
     6 0x257b2ec44566384b70e60e010174d54a05be0f60      1
     7 0x2684a5daa74f60f6e7511bb317e4ada4754b1427      1
     8 0x26ee1ea70a38fee396fc5db7d9d8c75b93dda0da      1
     9 0x275c83d76b1b0cc42bd066e327c4e578a327ebcc      1
    10 0x2eea9e62430221a920b8a5d9ade88e8ebc3d303d      1
    11 0x3245d30c08bb3629d5848b1a440ab8b040eab291      1
    12 0x32cde1a4c5a1344bd555c90bd864b8248bf00aef      1
    13 0x388892c25255c89f73ce9e22515f4b996f1768b7      1
    14 0x388cbeec5a031e4aad12599b04a4aa4f088e0f73      1
    15 0x3e960c18474442fcf4e00ab1d9a6b9b5cc0a9777      1
    16 0x4c1c6bf9c6f8beb6bcc7d798cc0dc48ec4ed3d50      1
    17 0x4edec3c48638b33214be7d52d2d9358273aa4e33      1
    18 0x540b1f837983e38236a0ea27c44f19232091c445      1
    19 0x55f119c32916ea8fafda92734baeedb368b6bc31      1
    20 0x565196152f8a4066a51b498a44a9981145d441ff      1
    21 0x586f30154c3b685ea8d472126e5b7507e14c2736      1
    22 0x587c501dc57cb66ec7afccf220dda7340159a403      1
    23 0x61fcfd41c9031e5af6300da75b25f5f83ca2d647      1
    24 0x623cc8f01211cb7341c9bea76a4c024b0106ccaa      1
    25 0x6281a2ba2120ee1fcc6100becafe472291ba8e6f      1
    26 0x66f024c0a38efcc8cd6b951e1e3354a385061057      1
    27 0x6b95b4d18b6de097f7f5b8ded98682af61a84921      1
    28 0x721ca5428ec12b148b34458e0f8557f4c9624630      1
    29 0x7537f404ae396bdbdf0e9d79f81940be55577466      1
    30 0x7859f3da4b5fa22b8e0176925ef997c97f18c400      1
    31 0x7d639b54904816df6ef24f10ad421a874621b512      1
    32 0x8c6bca11091c00cf68e3aa1aad1538a66bc7e565      1
    33 0x9a6342516b1b209535c3a904ffd5055a45e48e29      1
    34 0x9e50db953210a5291d4ab984166abf21b5355054      1
    35 0x9ee370a169196b497e50498cd4ffcbff190d0f9f      1
    36 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
    37 0xa10d31b628bf5f9fc8e02e91a7564bb2a1e5fea0      1
    38 0xb0bf7fd6f02258b47f9f0d2dcc469a43f6f62c37      1
    39 0xb1df5eda984ad80a27a9c34f0237c4c6325548f8      1
    40 0xb2a1a7c670df98a600194b525014926a2b50a334      1
    41 0xb2f2a830e81cb79042a470689e99ddeb1f7df5f5      1
    42 0xb4f44c9f6dcde6a04fac6954191f02cc567ee6ea      1
    43 0xb6461bf5223dfe357745895ef4473024e9dc2e20      1
    44 0xb7560b71de74edbbda141ef1e2205da77ff53dd6      1
    45 0xb8f902ec644498a26ba3115ba4fe15ae0b94305f      1
    46 0xbf4bb8906478162e5b603fd34203c2477d1d34c1      1
    47 0xc31377d6fba90cdac89bebc49da2e2f7f392cafb      1
    48 0xc5167c971a6c92f74b7999fcd3c66c1ea68ba32b      1
    49 0xcd0cdbbea19a7932487feee640279a23db9d7de0      1
    50 0xde2baaa31b71caa10782c5396ccaad24df6cf943      1
    51 0xe50074856d95f6011ac34d7088273a2a02e2ffe7      1
    52 0xec82fa656f3e27265c53fc0aafa53cec4bf3ff07      1
    53 0xee34bbb4882c0ca569f70901217e6aeb3b347b62      1
    54 0xf4b27acb9a65dcb2fbfe8fb44516b09ac1f39822      1
    55 0xf58d6a3178b34d10a0dd1bbfc7860d0f58fa4d0d      1
    56 0xf7733612846b22d575cf1073f92979592159d2cc      1
    57 0xfa9adf59f9b51101425b487ec77f7d087bfeba5a      1

## Airdrop Nouns

``` r
c(airdrop_nouns) %>%
tally() %T>%
readr::write_csv(file="airdrop_nouns.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x19dad8fc772a33450eee08d602581e27c4649417      1
    2 0x1b7844cfae4c823ac6389855d47106a70c84f067      1
    3 0x57524efc73e194e4f19dc1606fb7a3e1dbf719b3      1
    4 0x7825d1967c8dc8a315e59893499f0b0a00ed0e5d      1
    5 0x93757e12bfb1c0d4d2e319b621e27cb499bd35be      1
    6 0xa068d2ce57234a6324575ecc088f0d3b2c100974      1
    7 0xaf9a1e6a9b79fc77eafa40396e8a15d4d92867ca      1
    8 0xd2a744047bb754040911ae14cb5bfe289812e9a9      1
    9 0xf627e5f4bad95a956468d8bb6ee20b119f992e96      1

## Airdrop mfers

``` r
c(airdrop_mfers) %>%
tally() %T>%
readr::write_csv(file="airdrop_mfers.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 168 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x024a88896b6a85bdacb2de0f23e7fdfca7091f5c      1
      2 0x027a9ce347d6964f9a9452299b04288e94c3b201      1
      3 0x03ecf76f0607ec754f3074c441bcc1857cceca19      1
      4 0x04b5eaafc9c6fbaef1d9851b902bbb527e76171e      1
      5 0x068848a875fc633a9514ccf5379949d85bb11966      1
      6 0x06e54a1da420a9162cfb33399db2b8185989f8e9      1
      7 0x089660619d26b6ca62eacd74e2ddd6089024fccc      1
      8 0x098b1c2a2f36e3db6d61899378ecdaf6fbff6dc8      1
      9 0x0a605f26a5b6c37b27fddb155ccb495fd4f128c0      1
     10 0x0af325ae0c8f165c9ec84e2d00e994d1c86d1e98      1
     11 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     12 0x0ea770824779b7bbc4f530705e64b291f5c43af1      1
     13 0x112e372bb5ec17c1ea6b1d679ea72afbd2a94404      1
     14 0x114837ea03ed4e5bdbd6471cfa4dd80d68563dc5      1
     15 0x129cfe876fcc7f8bb8901268d69d4c678fdec8e6      1
     16 0x12e08b091a8b6419502449bbb369709c1e4a60e0      1
     17 0x1428f05a7f94d9228b2a78293c99b337d7d2af6a      1
     18 0x147a689478cdd24b5452271972e18eef05ef359b      1
     19 0x14f8c1f8e31cac72e3026700ab61dfadb31f44b9      1
     20 0x18bc4293dd7f72d9dccac45e207093df3c23cd17      1
     21 0x1e2306fb9949e61938b70e6c576cde92bb35bb6f      1
     22 0x1e3a634344cab3ff7a3a72394b3a594a58d36ee5      1
     23 0x1e6223c7317fb33c1005535d8a5f9e3bc3671c35      1
     24 0x1eb1fe371cfd35d56e4660ecd93b825669b1d4b3      1
     25 0x1eb322c016815ee5b29c071586c1b75be5934576      1
     26 0x1fc444e3e4c60e864bfbaa25953b42fa73695cf8      1
     27 0x20793b0042448ee50a7208fd287792c6823cc695      1
     28 0x248388148ad1fdd1d4f97f7bf79ec271f4a621d1      1
     29 0x27adfe8457bc9bedeab962f5509ba303da0e2f17      1
     30 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     31 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
     32 0x28a8e6441f7d9fd324fb13b477748b9c30d34f0a      1
     33 0x28b581562f4a2cecea191f0e5a6c87b3a631fb6e      1
     34 0x296132e19ae5737400ca45bcc8c8555982d41d53      1
     35 0x2e89f8b69e755b0e6b36fde2d06d98252e10036c      1
     36 0x2f63c3f6cc7ab18c497d767036ac7f18f2e11fdc      1
     37 0x3176202d2ddee94c063e20174bfcc54af821e938      1
     38 0x33ed1d514beab23acd9418ed6205c5f863601009      1
     39 0x34cde3eba2fde05fa45598e7cf52c90d9d6cb461      1
     40 0x367c78d58c09c34968fffc397975a6d934dbff86      1
     41 0x398526505f0a577a399871b41fbe75ec8308bfc2      1
     42 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
     43 0x3ffbed9340a2ab0c157a61771d7ccfbd7790c9cd      1
     44 0x414723d4f305f1dcd71d72bacbb9988f7625ebb7      1
     45 0x418cd73af8131dc641c7a79147ff41741317f9a3      1
     46 0x42cef12222672e260d275b80a0ed0bc40896af67      1
     47 0x46bc4aae45feba36eb29ee91527dd9615911eed6      1
     48 0x49dfe490c49b15625f98c543fd55e8a2fe5fa5c2      1
     49 0x4a2bc6f59948174714657735af944261ceddf67a      1
     50 0x4b038c047f79e0cfd3762b9e8b4cb017709315fa      1
     51 0x4c85ece5bbf191d3e1b76934c8372d4ae3bf6927      1
     52 0x5136e623126d3572933fbafe59ae97f13dd9687a      1
     53 0x533dee7f5e471b34ce9ec908f6c010dc2a160413      1
     54 0x533e4539864ca274d93c286584bdf2127f093d3c      1
     55 0x536d83e66759cceb0d3b5d6b70d61696404ead7d      1
     56 0x5641519cc28def80d631baa28b949f17a6a22ad1      1
     57 0x56b5823cd02c37dcba1f858035e7857c571acba1      1
     58 0x59208cbba4e46011629644aa607c0dc161a02d16      1
     59 0x5d86e77c2921c577ec0aad66a78b3faf8a3b66af      1
     60 0x5d904c98df910e30516a3520d7ef4b601b73bf96      1
     61 0x63452484ede33876ea5e18f7c549cf183e31d352      1
     62 0x64ed2178bc37031140fa32c74c205fb1428cc245      1
     63 0x6712ad2943bfbb71ad9e6e44b061e0a5a78bcfbe      1
     64 0x6a9b682290dfd41f19a6211c0b219050affbfbcc      1
     65 0x6b9ff2dc17cee986f796ce9630f8acda75f82e7c      1
     66 0x6bea09d715e9b89e2a083937ea957c1e6af6d3cf      1
     67 0x6df1e93a9858b68ce575707355594dec7108efd9      1
     68 0x6f7a49da0f184509814039956137dadb9ccda4f8      1
     69 0x70e3c8cad61e0850f553db3646123007b26a026e      1
     70 0x71f631b8efe9959f46bcc5b409d74c52a3b67935      1
     71 0x72971f6f324a4f3160076218339b9038d2ccc5af      1
     72 0x74d92fab35473ce7d728a7bc2e8c7e9e9250d5e1      1
     73 0x763f71beeac7b99585bdcafc7a25fec8c4a82594      1
     74 0x77297ab28bc330c345b5d41146c504bc2a2a54e4      1
     75 0x77e8c7caf4171fe1d28243dfa148084e092b7943      1
     76 0x7bb62c8fcc15011bf7b5d2aa24bb8473d0dcbd85      1
     77 0x7c4f9133852935cbc555d907cf981408cd2d4834      1
     78 0x7d0ddc336fc0cfb5a13efe490de40decfe0900df      1
     79 0x7f7c864dac139b5a36550481180b834ef283e056      1
     80 0x7fb1f91574e2d39c41a85395afc7ac2e4c0e6b9d      1
     81 0x82f0e1087fd3abd4eafc3916a42b0e7edd3c1889      1
     82 0x839869f27ddf564995726a32c15a56aae85a354a      1
     83 0x84ba38696f4f2dbcb88ec2c5f82d24a074d2e6fe      1
     84 0x85c0c09570c955f6e07522e9ad6041716cbbb7fe      1
     85 0x86c7de90e8ac6edd89a5180dcee677a2986f8b5e      1
     86 0x87accabbdedce63379af9d246bb979a3a481534c      1
     87 0x87ae3929744864734cd009ad3be47d97fccb54d5      1
     88 0x888801e8331d0d6098d53a5571e9444ec351d888      1
     89 0x8a0ba098c376e331d6fe74875ce242c47d99a226      1
     90 0x8b6e7845660998f52ba01bec90aa6e7e0ac07baa      1
     91 0x8d5828f1f143de2a6c1ce7402b6346ff4749439a      1
     92 0x8d8f3e5e8c77563a7020804f7ee1df9f05359dde      1
     93 0x8eba867e62ca60a885c72444e0676641dd0b942f      1
     94 0x8fcdff1c8a5f19d56e9959ed72419aed8f7e2e44      1
     95 0x91322107acffcac9285a056f3f6c51efe8c7f0b2      1
     96 0x927c34ba85e813c4d26cdafde939370087c373f6      1
     97 0x92890034a3dcb1c38c1184775788ba0f95c23f03      1
     98 0x93243bf5f577c1e325987862267d0314f72e0050      1
     99 0x952dd5887bd9a061768cec90e2ac8f3c1ed21480      1
    100 0x96ceed8be02c2f346c8fef7d830a24b5ee2a435a      1
    101 0x9722d3bb786b5a29c4f8b32fbbe8ed4a609843a5      1
    102 0x9aafbd399b42c5ce73b30b840ce5f5c66afd00e5      1
    103 0x9c43b8fc9585f2aeb9193429d57174915b742779      1
    104 0x9d5eb9a24e9ae397e1c001eb9f0c2fe5f6101eb7      1
    105 0x9db32b92a3026544c8d5ec2222700bb0a4d5a901      1
    106 0x9f2dee49af076fddc50ffcdb9d7b5b69ba22f754      1
    107 0xa32f735fee61c1707188588b7c08263e60f3d125      1
    108 0xa36d30fc6fd9b6e23e2285474cdcb9096a72840c      1
    109 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
    110 0xa4b321b3458673dc34620e355185813c1d415ff0      1
    111 0xa89a9184063cb88189916d78b08582bbce5b377e      1
    112 0xa97a9534a6b9279ebb350bfde06ff60aa6eb039c      1
    113 0xa9ddbf3d289d0417b821bb3b0d7e987375183aa3      1
    114 0xacd58027b4cf764469a46f4c28ea47fcda3b8c42      1
    115 0xae1788699d4c34808d1d56479c618f2c024a5b52      1
    116 0xaed960d3aaaf8f71897f3040148fd0eef69a30d9      1
    117 0xb31ef912c68b1f52b693d7e23dfa4a7f22f209be      1
    118 0xb5ce9f0158b02bbad38eb706bc3956d5b8b55288      1
    119 0xb5e1fc4af4dd6ab3282d16499420954b192e1849      1
    120 0xb6d67e7aa69aed737883ef83416e5a694e09c7e3      1
    121 0xb73a795f4b55dc779658e11037e373d66b3094c7      1
    122 0xba8db35dae41465a45ab494a5a841c73ed1c047e      1
    123 0xbae1b94df2349058d793e6c16e3c97d06bca75eb      1
    124 0xbd6c1f8d5692d49325fdf12daa00dac59224de08      1
    125 0xbdb45a4954958b40e23abeab1dcc9acd264df6d4      1
    126 0xbe527aa383c9979d0133d27b8ea1c43b694d6f9a      1
    127 0xbf265ca7c7ace01469959aad580171ad6d43d8f3      1
    128 0xc02f6d78e6d1cd6522c57caa5aa99070aa1daf8b      1
    129 0xc13280bcfb2e63649f65a80d812c6ac1d84886ec      1
    130 0xc23485ae4315c409f0175a782841e0f10f12b0ac      1
    131 0xc63a5ad9e61b64f18492cb1619c9c1941b4df3e3      1
    132 0xc64ff776e44379863bae2df4b96c78d5dfa379a9      1
    133 0xc8ba28507aa7f1825db28a7882402d616ad7f562      1
    134 0xca52008eb8ba3efd6c069a272946cb206f947cd8      1
    135 0xcc0b630c334c283458520b649c7ea5beefd002d7      1
    136 0xcd2bf6794a240a0126356d1fe6cc1f6ace3cd64b      1
    137 0xce185e5bff20263f210c9bb867f1b0ea1a718d3b      1
    138 0xce1eb9636c412245c92352d6fdfeca9f97a89a4a      1
    139 0xcebac4869edf0ba8d75deab264e3a4c5117aacd5      1
    140 0xcf1bfd19b1ab67a251ed749c7084e68778b9405b      1
    141 0xcf372762fe08682528cba6bfa367a768d7b0afb4      1
    142 0xd0695f478fa223ebc319b3402ed75bb9e1c035c1      1
    143 0xd165ecde7e7825923c8a53cc0ce8f0d75e200dd0      1
    144 0xd2ea8c9440b63fa16e0e82ae96b80ced580bd8ab      1
    145 0xd340035d759010ddefa3839bee22d324be3fcea1      1
    146 0xd36d0ace6b18f13760376751af10741336131ce8      1
    147 0xd393503d6be38401054135780f57cf308e230dd2      1
    148 0xd5fab5d372081b4f20fd4636bde96e9061aaa8a4      1
    149 0xd782fcbc3bba63a08828be5f4aa59ce93aff7697      1
    150 0xdab1ebab37c06e0b2088342b5f1d1fc7ec8de915      1
    151 0xdf46e8fc41bbcafd355b680a380f02758f330e35      1
    152 0xe10c1e3f6835abc46a28abdf4d51f11a7aefc388      1
    153 0xe1ab2fc3fa943a7d237befbb898153f9a6f04971      1
    154 0xe2d6b30e63ecb93fc0ffc1e9bc1327e8b16246c3      1
    155 0xe3916f1dd2b564f1fcbd0d28b86992120a896a58      1
    156 0xe4a718104474da81790a305bcec5cf98c0359a10      1
    157 0xe9697aab0eba88aa15e59704224ec5d9615fc55b      1
    158 0xead71ff7bcce7c3dd572b3cd6fb7bc4c6359af5f      1
    159 0xed3f868fa921974f4addbfc982b1298c82dc6cd5      1
    160 0xed4a96ec302c7588bfe84c13888c79ac8a321dca      1
    161 0xf08755720ffee568d320c7daa9f5822b82c4f7a8      1
    162 0xf0d2517091b83220b14d19a616cbf7ff8a24c196      1
    163 0xf4363de08eeff1ec9f8d74395e6b362eac339b5b      1
    164 0xf49ec27f278295b94d7c3badaaa954f0af278fe0      1
    165 0xfa5ce7e0c0892a841ad402dee23f95848cc73759      1
    166 0xfc49524f4d0a1585e611ba478d225dc7831e2e7c      1
    167 0xfe18ebf2d3e54af293bb27b0bce19e2857831708      1
    168 0xfef8bf42f7658cfb496365cfb1d75f6e5b29edfd      1

## Airdrop moonbirds

``` r
c(airdrop_moonbirds) %>%
tally() %T>%
readr::write_csv(file="airdrop_moonbirds.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 197 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x004849f2a278863184389d97ba4f77bcb5ae2a63      1
      2 0x021767dd74beef783fc28ad858048cdd9ed7e3bb      1
      3 0x0240ced4de4edd91884027f2e06a1780331fab74      1
      4 0x02943529d1e692e3281c77f37af56972b09e1498      1
      5 0x09309b99f3b43648734caeeaa752f77c679a2918      1
      6 0x098cbf21f980856ae9a85d23cdf6133a6ee857aa      1
      7 0x0dd1d1583a10e3cc8cb93499c2e2b69dbd5dddac      1
      8 0x1064a9f6003cb2637a5fed88501198457fe2aae1      1
      9 0x1180d72e3ba3dd5d96b84006a77b3c549750bc57      1
     10 0x128e723f96b3134204176c060167dd2ee96d7d93      1
     11 0x12c209cfb63bcaee0e31b76a345fb01e25026c2b      1
     12 0x14b27e03a394cadec25b2b20573e1ce88f482e91      1
     13 0x16ccedd50d9eab1aeabf84657547278c229d2211      1
     14 0x19748446a67b690ef1dd13ee61a615e9028bc6e0      1
     15 0x19fae508dfeeb651212047ed358ad1c1e22d53b4      1
     16 0x1cad859e18f5b29b0dfe036c284f09152d081aca      1
     17 0x1cd15d9ec87e926bcdc4345c11987914c57acb1f      1
     18 0x1d342a31ff4c98c991005d88b545e704ad13cf20      1
     19 0x1d9c01a592040bdca76e8b6de46d7d97b503085e      1
     20 0x1e39ebad76f75a514ebe5432f4efa2e8e50bc52c      1
     21 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     22 0x1ef7cc3ddbb2576781692c30fd2a66e97884d872      1
     23 0x214f4e982e39be59a4ce2c7c9deeb4a21cb5eb83      1
     24 0x2336ee0447b55103e8c01cfca82067445d8fa571      1
     25 0x233eb8b7c66acb80f52a62047e8c680447f82ebd      1
     26 0x28bd50226a8523ccfaa2ff2873eef032a7feeafa      1
     27 0x29e2e562991e4fb58c0545396435d5c121375621      1
     28 0x2b0d374573cac356f4d2d74be3aa62b23db6e4ae      1
     29 0x2bd17fa766dcbc697c8fcb29f311da53fde0074f      1
     30 0x2d313cc3694ada1ddbf466a9ad3e5138b5f8336e      1
     31 0x2dbd479974967b8a0717d8b84c2172d8b26cdf44      1
     32 0x2dc820ae955aa5a34cd073f72b76e127cd7bed1d      1
     33 0x2ea9b710e4038dfd78a7fba61b0ca1d3bba7b1d4      1
     34 0x304ffdd9d5e2bd23ecfd99a1da32a0472de766cb      1
     35 0x30a933add6fd601d152aa4aaf7620b5e27e88888      1
     36 0x31e7bb24ac5e3684345514206d1f2fcdbfed557e      1
     37 0x3204010a20b485ac98be2d1b0a6137de48b42410      1
     38 0x32a348ead7ed30d35b96b9f87320fef862033574      1
     39 0x33608f433eb5d007abcf0daf01ff687e2148cdb9      1
     40 0x3369404003d19249f96ca5aefb19baa267b48af2      1
     41 0x35080cda95a7a6959effc293f4a4274110b77777      1
     42 0x381e6beb053a494b7520eeb4f6e26d4ccb503a05      1
     43 0x3956ff98921f92f02d3a07348883a3cb07681352      1
     44 0x3b2dd499b5c4fdf97f794525a9586f62e334622d      1
     45 0x3d62c8822d882c5fe903ae8bb71919c9273cc321      1
     46 0x3da0bf57b7a57fcccb32c4a54396a2cbe2f43ba3      1
     47 0x3f6d1e260fd0f69be8fe8ce1c425079b4ff4ae11      1
     48 0x3feec5cc114811ba1110ce2319a6a9a0d3e8326f      1
     49 0x405aa7ec468c6a8fd1e3cd39f7845cad0a6f519a      1
     50 0x421ae97f38789280fd114478a748a0f943e6bdd9      1
     51 0x423a9a8aff6d51d7aae8055ffa0072afd26ad1f0      1
     52 0x432461e992085f5d4d970fb0a8be85cde4a32b62      1
     53 0x45de56ecf71d7b76b304dde350d34e9abc7368ca      1
     54 0x4683d126f02e6940922d2a42938b73eb96b889fc      1
     55 0x46e46a9617d0d3c8db757e7c21f27d3aa9c0f179      1
     56 0x4735eb38fd11f2f964b5e37e4ce817355c85dd86      1
     57 0x48dfe20b0ab4718648e30e7aae92ee681f48d9ef      1
     58 0x49ffbb2c43ad5298db85912f87e3781fe2ed1770      1
     59 0x4c420110bdc4dbf16e8d5e0b559168dace5f58b5      1
     60 0x4e0358255ad25c4306fa0ee6657ce0c52ce22f53      1
     61 0x4ef27b561587555a3203578f878fa8479af6a7b2      1
     62 0x4f55ae5521c82f09e98657eeb24450a24ea6e513      1
     63 0x518d6c448af9265d7c7b5433bdb67efdff10126c      1
     64 0x52ea368e6ce3a5ff4dc2aec948e4427f8ffc6ca0      1
     65 0x539c0b07899c27b3805f422520f5d9de10121d63      1
     66 0x53a0ef064d77cc26175b8750b6777462231783b1      1
     67 0x542fce2f47cbbbba00ba59f844b8e1aaaed1f84d      1
     68 0x54cee53d9bcc28554b365ef45b2e899036bbae20      1
     69 0x5616be58eddc7c2aa61d1513f140af1b77ab5b10      1
     70 0x564d86a1a690a7c148b4c278525a6c58721b3524      1
     71 0x5c0ac43fa40cdbfe6d86e9041052b2eabb799227      1
     72 0x5d7c9186e70bff5b65668db66ea7bb9bbdb81d3f      1
     73 0x5d86e77c2921c577ec0aad66a78b3faf8a3b66af      1
     74 0x5df5078ae2e65f05874e57929ca50e0ac689b378      1
     75 0x5ef082f4897430987db4f182fafdfd7a442b064c      1
     76 0x5f883431d7467e42b1df641ae7b57144b01bbbfa      1
     77 0x6266db517a8d1e2b4c61ecb48cde4d05a5e44990      1
     78 0x629fd2b73c8e01888597e3d2344ec6098575af13      1
     79 0x684ec4c96305a949276bbdb0b41ed4c0d5a61d01      1
     80 0x6a310fc070041c7373ee44a03d78298e3a69da52      1
     81 0x6a8c69dc2040c1c594be2f3947e15669536dd59f      1
     82 0x6bad7a9cc04871d133291d418b28e35569ad56f4      1
     83 0x6d480e89a4ba91fcc27024a658543c8452d4258b      1
     84 0x6dab25d23ec3f62f27578fcda132192fcf046dd5      1
     85 0x6facad44e126f2cacde44a0b2b85ce1b643495d1      1
     86 0x7369759f17a452e6f8b52b553c2671d0a09e62fe      1
     87 0x739c6cd894603ddb5e974f597d49bf5ae72727fb      1
     88 0x73f35522ecedd8c0c26707b7601eea19fbaeaf09      1
     89 0x74fb6c75203c1aeeed4e7483a588da73b1d083c1      1
     90 0x79473b5482ba8de8de5c1e2ad08113e1ec528951      1
     91 0x79c4f941f7629bf271d17e396f2ec122b7e39ca5      1
     92 0x7d462af7fb6aa7b5b91fd5ca5bdabb7fd6124b26      1
     93 0x7d9e921c8334486732918fb3a034df44538eb57e      1
     94 0x7dd18efd62a5ec66abbacfb4938be3ac6692931c      1
     95 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
     96 0x7f261cf6d968de4341183aeca33521e2c3a19bd9      1
     97 0x7f873ec58d567e78a2c1dca322cbdb6d490ccb7d      1
     98 0x80b946cf5052620cd1e227ee501aa8f1cb896df3      1
     99 0x813a3005b071791b98292c74cc0700239c52db25      1
    100 0x81a669f6e4f11d5a34ebaf1d85b38daa63cf3ef4      1
    101 0x824ae3bfe2771151db8c7b7defb96a9f67695ade      1
    102 0x82a6ac4702376735906988150bde2a7b5182789c      1
    103 0x82dccf7c49e6bf670f64fa3ac05d4463a77b1932      1
    104 0x83a7d09d613ce63d9b9c5ebb16dc48d7f8ca8942      1
    105 0x8760eb56f3d25b799c1da35731c43e61709bead0      1
    106 0x87d6bc940ac423a39bb4ee75e2ad0f842c91da1d      1
    107 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    108 0x8a4a90ce447bd8048af3aee4fc7375803f99abbc      1
    109 0x8ef33d3c463dbc5f2791e8f82bb73474652901d7      1
    110 0x8feaa088c9eccaf9dfc4bc4f10775104f36fc12d      1
    111 0x91041fb738c4763a81bf3aa41a3bcc9e716e95e8      1
    112 0x91b49b932dd4b88523a390de9c4b964d463376a0      1
    113 0x92041c816fa960e2352f49ac118771307c416a17      1
    114 0x951e1d85810c7b25dbc0ecceecb4248b4e202412      1
    115 0x96376031c90856c341d784d9d96584569f21dd97      1
    116 0x967be09d0652a0be582f217630963df129157d55      1
    117 0x989cd4a3a88c86300726edb5d5516a517ed47599      1
    118 0x98efd2126b1b76a5f426715ffbdd286c3f7e740d      1
    119 0x9929fef8c8f3e09e6e7d9caa69e50ce185a95a69      1
    120 0x99e14da3a40ae897ebd88e972683f0046b4d7fe6      1
    121 0x9a442c2920b70ec87414319ac93d525b2f7719d5      1
    122 0x9d95d36f15c3cd3028d53a58ab6c60a1a7815179      1
    123 0xa061fbfa7dc7ee9f838a717e8b55fbc34641bf6e      1
    124 0xa2d51b5832d7f5972e82ad985c408134d3473817      1
    125 0xa39573625512970f0f438e0fe3a660ba6c4fe857      1
    126 0xa3fbabd1ee57af8e779531c75444cb4c259214f6      1
    127 0xa4322339b2726a561abf9b911f76ab00429e7a4a      1
    128 0xa4ba5692a7980c803cab398258663f01d0d93ba6      1
    129 0xa4c8d9e4ec5f2831701a81389465498b83f9457d      1
    130 0xa4d091b1db12e733e1e27f637bb2305672365b85      1
    131 0xa60bee3be7d40ff6cb0d166dde96cc315ae0d39b      1
    132 0xa647a51a1af1ccae559f3ea0addbf2a42285a3b1      1
    133 0xa81d194cfc13364eee82abf32c8451264287aadd      1
    134 0xa88220a76054be964c7d0e039643138a16ea17e1      1
    135 0xaa2cfa73b429d1189a8d72d0d9a7b1557590eef4      1
    136 0xaa3cb22341b2118afdd12f127913762ae66142c9      1
    137 0xaae33d661bb2604158748ea861280231600cfe61      1
    138 0xab1ed04237efedbf10ef3cab7a6409aea6bedd35      1
    139 0xabd5a552615b981575c579f1b10c2aa686e3ce51      1
    140 0xabe01e6713511c6600b559c691c526a019d9f266      1
    141 0xac0f76a8c30e23bc197d26465e7be319713c5224      1
    142 0xac7c12f4057076d547666700cfab976a12456774      1
    143 0xad2decfccb3922daa50f0675668cd8f7ec1e9f14      1
    144 0xad7a78e1221d5c0923bd5458fd73c8631cb0642c      1
    145 0xb09316b71c0ae83f1306c5a8dc67f32450e0adf9      1
    146 0xb24d892235be6a4d28ce931f0fb86c105a54ac11      1
    147 0xb2f6cd355b598b9633d249d3afd686cbdd84f214      1
    148 0xb6479265a11e71e3c1c735daf5364af2a7f961de      1
    149 0xb84a18fc0cc6a85471901df54fe98b14582273bf      1
    150 0xb88f61e6fbda83fbfffabe364112137480398018      1
    151 0xba0ec1a6f870be67e5b99c1940d2a9ef91f86ac7      1
    152 0xbb1d6ee66888123aeaf75349a6e6026979519acd      1
    153 0xbe81d93f812b833684704fd2af8e28d27f51e541      1
    154 0xbf3cdf6f7173358adc01e3b5f833ae03508739a3      1
    155 0xbf43ce4a5f58e67f86b4efa94d6f0623903ebc43      1
    156 0xbf6db207503fba4895ce079d987f9e855ed09b8e      1
    157 0xbfbaa97b4f95758df42f6ea6b2a2efba0460efb3      1
    158 0xc14d1d58f7d40eb287ea898595dc0f6177765db7      1
    159 0xc1d43fa553b9fbaa95637d28d39862dc339db0ea      1
    160 0xc3f5c46f6b7876fbc60eb24517780e41c8583b6c      1
    161 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
    162 0xc771a024f3c7ad7391dc9443f6af94268a3862a0      1
    163 0xcba35e68a8e99cd32a31e37aa2e57e2eaa727c1b      1
    164 0xcce55e8d17968af05c590c5c96342c297ed67a24      1
    165 0xce115f60db71f392fb5f903bfc1dc4849116eb00      1
    166 0xce2c67c341a57b91439c99fa2c39268189a61dc6      1
    167 0xd2fc0d9f79ef243956c10e0c6e76e588d70ae8bc      1
    168 0xd3abc1d4b2a4c2cad16416b41551753c7df796bd      1
    169 0xd752b961a0e911f4a6f0d2e36701a0633b3954ab      1
    170 0xd90fd51c9fa7d51b9c53c158231ff8498ede1140      1
    171 0xdb6aab3f6e320926ade666f03a34af31ed08372a      1
    172 0xdc6babb7e90b27e3f6be77d35eb03aa612b92efd      1
    173 0xdf44cb56916b7692fb13a33cf9707ca840a11142      1
    174 0xe15e568806bed33113a94b8881d94ded3330056e      1
    175 0xe2a323110f58ee47cd6ccdfe303ac75aa1748cb4      1
    176 0xe34efbce9bb931b6c2083237b4f93b1870e41582      1
    177 0xe4d50d9489a3ccf9d11c12720dbfb0f32bab0cff      1
    178 0xe6a5f1690fcda05d9ba0a663b6e7ddf3c97eb7b1      1
    179 0xe70718e56f98b3c6c6e1bb6cdeb53e3cbb940ea1      1
    180 0xe864b7ee62daa13c80f652f9ae6fa0e486a9283b      1
    181 0xeb217dd645ecd13c84bc4baddf9d2c3aefca4849      1
    182 0xed6618cda6a25482b99fe8bc70f039034575ee7c      1
    183 0xf1b55dc69a19dc0779c097a11425d9d712057500      1
    184 0xf274ee9f95c21943a1600068ffffb5da1fef74b6      1
    185 0xf42a00d406266b711f23dc2c4f5e801887ffbcb1      1
    186 0xf51ec5d9534573f78fa3c87c3becdb2ca2b179aa      1
    187 0xf58c03997171ec5e5ccaaba7338d0575d5616823      1
    188 0xf5ab63bfaeeddfcd37f696a9b8697e424bb22788      1
    189 0xf772e3055d823058c961aca526a69565784f6e21      1
    190 0xf94751812572e965e781609d3f55c6d0a06f23a4      1
    191 0xfa3f7701b84b9dc0d9adef849e80197a2be4b0d6      1
    192 0xfb674b7fdcda0851360e5f7cb8f80a29478897c9      1
    193 0xfb77f548c7f806a9b948a81aa0c37ab40d15ba04      1
    194 0xfbce265272c92c3ce57b62ae0aeeee332c55b136      1
    195 0xfc0bcb2315629d03ebf10c291551355aee5cfcb6      1
    196 0xfd246baa363402b3d4e43241a1b9000371e02c50      1
    197 0xff26d669bae4ef50c06b49e1df6eb03d54497be9      1

## Airdrop toadz

``` r
c(airdrop_toadz) %>%
tally() %T>%
readr::write_csv(file="airdrop_toadz.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 123 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00063ddb30be7bc2292583d5f143e9d6e6228440      1
      2 0x0116bc6d6248a99cfcc0d644ed3250a7c45b3c94      1
      3 0x038826bb3557b7225512b60e30f10ca7fa178d93      1
      4 0x057d1e845402b371eb793135f877966b47577e28      1
      5 0x0c1d244c3b60070b92d5f199a5b5575a34ee408a      1
      6 0x0ce6593ab499c601da02fc7881ba0db5950b14d0      1
      7 0x118abd3f43d1a1724dd5d30ccaa6eb0e39913262      1
      8 0x173eebaccd2114a7bb7aeb6740e8c2dce8263382      1
      9 0x177751396d8236569c5c7b04232c7b7281a3b9f3      1
     10 0x17f4d9e092d23d81860d8950c18fdf1dcce75232      1
     11 0x19d74f3ab452b027c0ff27612f611477f0a3b1ad      1
     12 0x1a51e5e5c4fe902254d6bf77bf94b3e648d545b9      1
     13 0x1e2f87542ba49c77d71cd28d9d8e70f0b642288a      1
     14 0x1eb44b180681e39c3a362090e43cd6e8ce16588f      1
     15 0x207f6db2f9f4e1903e14c29e76585cc6428446c5      1
     16 0x24f0a0a76f6057829f292a09287df79d24249186      1
     17 0x29341e12ec5fcc461ddd205f45eacd0caec2603d      1
     18 0x29cc37bb515745a75eb9ec13d2db1b7bc69447fb      1
     19 0x2b53ba63ce6dd93f1bb2bb3245ac7631f1338366      1
     20 0x2ed5ab4d78053b9cae07ffd772c445265a280225      1
     21 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     22 0x330274cb14b1192e275edfd57986cb1bba302807      1
     23 0x33569c101562e1faf5b24581057e5cee4c8288d7      1
     24 0x357fd6924cde3df99a9e83f1ace7a9d85cb18aa9      1
     25 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     26 0x3804107dbabaf63d3b8e2b11fe43fa3caa811fe9      1
     27 0x3bd8f8600a20aeed0c9bb7f5f0f55e1ab429ff5b      1
     28 0x3c176a32895eedcd93db8d97682cd08fc1305c52      1
     29 0x3ce53518159c99c7f47dcbc0426fe375f824b945      1
     30 0x3e34060586e19a0bf0be74267888fbdb0a8f7e5a      1
     31 0x4042def3d9fab6e6deeb72ae3fb3175b55e50a5b      1
     32 0x4044730384489eaeeb0fee9c6ac5096ef2976e21      1
     33 0x4293541c243253d77afbecc5635c59c08b091467      1
     34 0x49115021e9dda2d00b3b307712a99a7d92bfa7d4      1
     35 0x4986d87d1c8a10eea7e3ac5d54b4c037afb95c24      1
     36 0x4b3b34a1512376c58633537c96812d1fd256f3f3      1
     37 0x4bf3805b23c99f8e0a5797e86fd0232a04a2a629      1
     38 0x4df00689952974edbe1cc05d2710ea1b2b6e185f      1
     39 0x4e142fe48c71092e78be1f1082fa8ce0cb15c354      1
     40 0x4e2c1c305d33c79614082ac0bb8ca7c68d8779e2      1
     41 0x514c9c0f1bc120f57183bfe4ae092ff0be13a31f      1
     42 0x583b7fa299d2e40c235f34bbb16e7e8fdd6d379f      1
     43 0x5a46c8d3845d820ae5b69cf7081c5b7c01ed2635      1
     44 0x5f603dfd9f82b7cfea8a03ad7c207a3fb3e5a8a5      1
     45 0x6098c75215c7034666e484f8b3452db921fcc234      1
     46 0x644d272c21cf58451c02c8bbf5136bd530e58d50      1
     47 0x6456f20453a383004f17b921cd770d187fcd30ef      1
     48 0x64d109081b38c4884708d816baf0dca36e75cb8f      1
     49 0x65fdaa0eb1d6753305e5812fdabf157069e2668d      1
     50 0x6670421786cb9b4fb09181141edd3aa1e5389975      1
     51 0x677eee9c51328095ee201887cb06fc189c505080      1
     52 0x69cc8b11d3baf57cc5691450de6eba3e483807d2      1
     53 0x6a3944e4658f6473111f5b22ede4da422e1689d3      1
     54 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
     55 0x6e6b6e8a74f973db6718b35152867c11d3b788d9      1
     56 0x73348abfbf5b502037903f98cc1787240934d142      1
     57 0x734700e10d952207966f78d02f79bc9c14e13669      1
     58 0x75966fa1459d053983a7117b2818c01d99b9bd4d      1
     59 0x7792bf1ab8bbfc74078afac293acbef2e00f8625      1
     60 0x7877c464f6601486053d3046d85ec20167363820      1
     61 0x78de8f6c33ec16d7bc5e681c885ac910404a14cf      1
     62 0x7aafb5a543c1171405f1ab67451b08b8ba33b2db      1
     63 0x7b058f2f060018ac8d0edd1fcafdac14130d2b7c      1
     64 0x7bb24f9ae8843590fabf42e049577e2ba68afa0e      1
     65 0x7ca8e6b21de829c094f44ff9b410ca8e5524484f      1
     66 0x7d43a3d2196c1e56818278e2254354f2163e19f6      1
     67 0x7d60a758f47822ec591b24df5a9d58fac84cb07e      1
     68 0x7fd29e547dc2d2ec3773457295a98893a0db2e05      1
     69 0x807ea4c5d7945dfea05d358473fee6042e92cf37      1
     70 0x87511230812b5389af25bebc3f0dfa8850b6f7ef      1
     71 0x888c1b86000bbbdb2223a0b89e3c82aec9d94297      1
     72 0x89b5deaf887ef9e3360f5e3347e6bcdd738778ba      1
     73 0x89d0e842b3c76d376c9b6af2f3b2a9abd2b22897      1
     74 0x8a738c9224c6c4a35f6224a40d83dcf1aa1aa52c      1
     75 0x8d52872f8c0635f8ba68f5c8c2162ee463ce6fdf      1
     76 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
     77 0x9a5d5f6d17b9d9093f1107dd6da934c292a601cc      1
     78 0x9b49c839bc165294cdb3b1702b6ee04fd7ae18c6      1
     79 0x9c46a675350ce1f7ca616bf4fb4e0a542295e302      1
     80 0x9df2fb10b4cf836699386b5474b9103aa8b99f51      1
     81 0x9f2587840ee943c06dca4cbd15104afbf3835888      1
     82 0xa18aa7d05f50b3c013c3de9ae63217a78e40859f      1
     83 0xa1b669414d40e68c11652b1cd82381f2a6495b89      1
     84 0xa6d738f0ddf963fa9d4e5952224cab2ce7400753      1
     85 0xa887b1f658aac0ed2a011d5b352cca49157dcbcc      1
     86 0xaa47a4ffc979363232c99b99fada0f2734b0aeee      1
     87 0xaa7dad6b93f50629f3938294f46aba53e6993dc3      1
     88 0xaaf3c82bbd30d08d06fc8d16c4186e8692784da1      1
     89 0xab7816594a3c30e88a434ee2adec32e26c52caee      1
     90 0xabb20917a30d3330839e20432fb6c656390b8a52      1
     91 0xb03d065b057048065e84ac5120a6a499441cab80      1
     92 0xb0807e628889575bf368ca4cde070a903a4b9ea2      1
     93 0xb21027569ddae670aa521ed90d6ecb5de515e8d4      1
     94 0xb4c1d3f047583c596a0dbf423b37118f42da97c1      1
     95 0xb923cf46609d7fbcf1b923092c7636d05506cbc9      1
     96 0xbb1ec73938b9df4baab4f5c43af96385a862811e      1
     97 0xbbd0699184259cc130d5320d66aaefd35025ea16      1
     98 0xbcf9401833aa1b79109f9d66808ca2c9a0ab7104      1
     99 0xbf00caee3f4d0e654b5e1a557914d257f126d055      1
    100 0xbf7fddda65f8f2b4e51e3f4383df1df8a1a84e68      1
    101 0xbfe9e0e820a0099e1eb92e33e3e0e5be280215e5      1
    102 0xc2e2cdb43af50d4c0142c170117d618653f207e2      1
    103 0xc30840c29a8de630687010944c09ff03f2aa8827      1
    104 0xc32ba9452cb2bcfa2462686f7a2b62811e3a4058      1
    105 0xc6165f99edc566743a0528b1d8bfc6a038e8e4fc      1
    106 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    107 0xd0f8293536d112b2f1b7fc7adbbf5e54e83dc3bf      1
    108 0xd72a01ad4b5005c3e47559861b0964f5c28912bb      1
    109 0xd8b3de5e77e6c85f65d0f6d81a6f6670ad67d6ac      1
    110 0xdb2a2df6dd352cc1783b74de588406271ffc459d      1
    111 0xdc690a2188fdaac60f32df790930957c57dff05e      1
    112 0xde178a961723eb49654d20cb8482e54d46c58901      1
    113 0xe46105bbda606babb797b343b21f930668e83c1d      1
    114 0xe5f33c3d6f158dc9a29dd90926b8f69d3dc4f10b      1
    115 0xe7d6af20bc878ca06258d0cc54fb9407db06e337      1
    116 0xea2475b40ef433502f0c4f789b5dc596c8eaa8f2      1
    117 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    118 0xf6814dfef8526f94c31669a53f3188a3e9dbfaef      1
    119 0xf85a9f2d6a528ff9d5a583d8ee12b0aeebba971f      1
    120 0xf91a1142d9e4910fb5e6ea3b965440a1dcef12fe      1
    121 0xfaa3d6b9c564901468b9e0da4c346f63926192b9      1
    122 0xfb39a75697b375daba7bf3b109c31c4a271abad6      1
    123 0xfd8f2c73e0ec4105206161bb38383ea5ae4decdc      1

## Airdrop cdbs

``` r
c(airdrop_cdbs) %>%
tally() %T>%
readr::write_csv(file="airdrop_cdbs.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 54 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00e952276c03e71b7b6193a67ace9b3e60546042      1
     2 0x059793939effe3cd98edaffad580abc181b1a96b      1
     3 0x0e1ea689e197d99a6d99712c9d09342155bc1dbd      1
     4 0x0f01735fb8fa1a88100e5a060272f44c2496987f      1
     5 0x10eea07f522633557a4eb00bff16c2f466d49983      1
     6 0x12d61dc3d789a1c8c8a3b88d0f2e980d2d1c5cd8      1
     7 0x161df6693a3faf54ae5ea6790cc43ef8bc68dc72      1
     8 0x20f1191e4051299d1a4e54f8ba95b636ab427898      1
     9 0x31e89be0c67c100bfdb7faacdd53c5481887050c      1
    10 0x32fcc745555671b84ccf89cd573d3da69f95a971      1
    11 0x352216a5b428fbee3dc882a2c268a1669fb8b2a5      1
    12 0x3680eaf1f85bec9f120bcaaa9ef469fb849e1781      1
    13 0x381b4b6f8a2375a8f1f6d9f55ee3a876c974ea1a      1
    14 0x38c558ecc60ab6850ad922757bbd1311c02e1473      1
    15 0x3daa61eb2ad40589728e66d123dd76783b037597      1
    16 0x4d544c95f86ab807e25c7c5ecd0ccfccdea9e51f      1
    17 0x56e83e8fc74079a58662f443ac39de153f02504c      1
    18 0x5930a17e51b6b0f258bcc15240e2e3a7585f5802      1
    19 0x602d50f55db1590601b25847b92438835ca8c935      1
    20 0x606f1c2d1d21db97615e7f6b9ce2bab571226ddb      1
    21 0x64c4bf67f8782f1bca8a34172670c061edf7638c      1
    22 0x651d9dbc3a6016b31c75ef1076cf13723af1220c      1
    23 0x79b5259dc0fcb44f9751bf9f78f5ef46a7958460      1
    24 0x7abdad9a1c657904d4b183af092f6a0ebcb32d38      1
    25 0x83b00f9858c3a11ceee2f3f55c3b3c523a3c9620      1
    26 0x86deaa62535d64670c2afa1dbe3eb3cc5f418e9f      1
    27 0x88c5677336e252c7a53c29dd20375edc802a5919      1
    28 0x8ba1b5bf769ce974d059187dccbf9db91e628bd0      1
    29 0x8d12daa8d907ad9c86c1c317e83cc430e9685771      1
    30 0x8de3136631a45cb1f85b7b096b542a3aa9168a65      1
    31 0x9283099a29556fcf8fff5b2cea2d4f67cb7a7a8b      1
    32 0x96289cb2986c6d781006b0dce36b143b21ab8fe3      1
    33 0x98d7bc088fc1e7a2f9b3536fcfbca405f902467e      1
    34 0x9dc2f55492b723b034f9d6809cf36cbbd54e635a      1
    35 0xa0506d623ea4cce494f38c25b1ff47e823b120f9      1
    36 0xa219be7d2585095f48a03ea981730e31ffe601fa      1
    37 0xa5f09c6f40aa9b8f7ad70d64c42e20df1ad1f0f4      1
    38 0xac6fdcad572b38e9a8f272f10c98e5842b91da4c      1
    39 0xacd2975b111044c7acdcae546b966274f7620cd2      1
    40 0xc0f0e1e14bff3924124e153e791ba822572921de      1
    41 0xc58943cf2a1eefbb3e431889d12c15a6a268a057      1
    42 0xc7385fdb1174a95fa43d8f77e4dd283c6cc4bbb1      1
    43 0xcead3aa547dc6e7851795292ceb9d1690767ecf1      1
    44 0xcefb3ff344a0680b59896d8f273c473184c69281      1
    45 0xd1f55571cbb04139716a9a5076aa69626b6df009      1
    46 0xdd3e9d0ee979e5c1689a18992647312b42d6d8f3      1
    47 0xe1b289da74a6a179b36db7def1026172cc3baa31      1
    48 0xe1e1c69d71dd81dfac922ee18f9a2fcffad337a9      1
    49 0xe55a8a47e4fa55557a0cc2f535595ba4e5aedd27      1
    50 0xef220de4c66c1ed36836d266e8b6c169693492b5      1
    51 0xef8d7e154e6ab6bd5c73d18fbdc5bd1d7e0ae4bc      1
    52 0xf902d068920234957d2908b8b0156e61c0bea2c2      1
    53 0xfc963a7635870f238b75af2b80be7ff9c037b200      1
    54 0xfe6c92d283fb88c6a1cbebac4ef33b1cafd118d6      1

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
