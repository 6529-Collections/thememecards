
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16064269.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:18181       Length:18181       Min.   :  1.000   Length:18181      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.459                     
                                           3rd Qu.:  1.000                     
                                           Max.   :652.000                     
         name          
     Length:18181      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16067669 # https://etherscan.io/block/16067669
block_hash <- "0x04a977551a77336a09cb03638ffacb4674ccd0796ca3f15beeed472b4329e4a5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4652 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=20,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=20,address_max=1)
airdrop_daria     <- pick(snapshot, contracts=c("Foundation","Provoked","DiaryFantasies","KnownOrigin","IgniteNight"), address_remove=address_remove, address_pick=20,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_remove=address_remove, address_subtract=airdrop_gradient,address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_daria      <- pick(snapshot, contracts=c("Foundation","Provoked","DiaryFantasies","KnownOrigin","IgniteNight","TIMEPiecesEditions"), address_remove=address_remove, address_subtract=airdrop_daria,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     3 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     4 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     5 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     6 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
     7 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
     8 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
     9 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    10 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    11 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    12 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    13 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    14 0xb5374cac6cad6b025246f19d20b0d4151b640558      1
    15 0xbc30e90dc528ece58c1a51b6fb6d572838416489      1
    16 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    17 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    18 0xdb561a899557404581e6180fe6d4178577dc117b      1
    19 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    20 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0566295faf52797d4a29e5b78bf28e287e4ca2da      1
     2 0x0716620e0a3da5ae123a9eb56e7874b1caaa4f33      1
     3 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     4 0x170fb8f8f37776dc48184686dec386d2d1c41cc9      1
     5 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
     6 0x36a0d4dca6d768333f916368b73bc689438e18a1      1
     7 0x386e96e10447095a99b919defc955c6d4835c279      1
     8 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
     9 0x528e7db54e33c52277ebd5daac7fcde897ca5085      1
    10 0x5d181a27b5a2f444ddf4b89d65c88214d7413ada      1
    11 0x68d45e76a87e9f1706e0d36b7815c8dbdb7e6aea      1
    12 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    13 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    14 0x8231ff66a7bbdc14ad89782a51c939bbc4c92784      1
    15 0x953cc221d2d43ba9b7eb9bdf17617aea39cea774      1
    16 0xa37f12329ed276f53690ea68c286ffce1a443e18      1
    17 0xa5ff0d30460e7790cc4c7e37aad1ce86efc50e2b      1
    18 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    19 0xcdb2961d0e2dab84d12b41d17b1ba7879201b6ea      1
    20 0xe5e842c95933ef2d5537b4a18ffea68ed19eca01      1

## Airdrop Artist

``` r
c(airdrop_daria) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0e372eb8c7abe11c99a76ea2260cd344779df762      1
     2 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     3 0x46ab7dceafda44f501416b9862518c5ab06a3239      1
     4 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     5 0x62cb05fc74548cb01b5d6c9c846bd0c06a42be9e      1
     6 0x6e513ada916670389097752d05bf609d1246b4d2      1
     7 0x89cc08700dcba9d4bad5295dee2a111b90b39917      1
     8 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
     9 0x9893117455cf1e8ac3e58e41c5885ad5adb1c9ff      1
    10 0xa1631f34541e352cd70e1c6902ca586c57d2e040      1
    11 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    12 0xbd88526a40bd28acb6761a238d7751d3f1bb5fb8      1
    13 0xd2f86cbed86db85c859903ff5f110b15d95b1350      1
    14 0xd4e6fce81defd0c7065399f42f8ad8cf2ba4bbb2      1
    15 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    16 0xe48765a07d401372d6cb6f0fc243f74c2fca34e2      1
    17 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    18 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    19 0xf8a09bf413576a2e8b406dae09732aae29a1c8d4      1
    20 0xfa3eb468a3a83b059ffd3a070b951aa3114033b5      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 58 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x1566ae673ae80725bcce901b486c336e6acef465      1
     3 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     4 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     5 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     6 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     7 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
     8 0x32d53a21813debfcb33a2b1ff2a396bd3a06f818      1
     9 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    10 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    11 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    12 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    13 0x45855a3f4404aa08ffe14a366c75663f4ded2fac      1
    14 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    15 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    16 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    17 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    18 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    19 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    20 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    21 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    22 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    23 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    24 0x69e68074f1aada957edd39c5eae0069973343f30      1
    25 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    26 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    27 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    28 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    29 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    30 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    31 0x82139687faae8a29851902783e02e699de0e0846      1
    32 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    33 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
    34 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    35 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    36 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    37 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    38 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    39 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    40 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    41 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    42 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    43 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    44 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    45 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    46 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    47 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    48 0xc762b1081c56b3fa487c7372f7284d9558a84859      1
    49 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    50 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    51 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    52 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    53 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    54 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    55 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    56 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    57 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    58 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow RAW

``` r
c(allow_raw) %>%
tally() %T>%
readr::write_csv(file="allow_raw.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 3 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    3 0xb5374cac6cad6b025246f19d20b0d4151b640558      1

## Allow Signles

``` r
c(allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_singles.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 1 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1

## Allow Artist

``` r
c(allow_daria) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 46 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00aea56c78ebb54dbf1c52520c20302aad6b7355      1
     2 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     3 0x16cbf1cf5167c1297a00f48a3ad37b6667648067      1
     4 0x1bac59eb85200321bc436df4f46aca6faeaea923      1
     5 0x1e47bc8b6c585dfff2e166257767050666151f0a      1
     6 0x20256624fb4ea5a5ceb34f356c5baf0edea234a0      1
     7 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     8 0x304d2102f75b5cf667d0f12812593ea070e0c042      1
     9 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
    10 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    11 0x3a943f529de8461697702fd9bb8fcda5d9a8ce61      1
    12 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    13 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    14 0x3df03ae2b4e5a2b82192031e13bca98a7e3fcc18      1
    15 0x41de8ac571734c4cd433ec3a823dfe7646580fdf      1
    16 0x464bcd95e423f88491e35fb6ca1a2c5fd8aac0ab      1
    17 0x485b8ac36535fae56b2910780245dd69dda270bc      1
    18 0x55440b91a752bf21c6e271938c3f3d24c203e1fb      1
    19 0x57f02141f2d91a024032536ec61c974be76eb7c9      1
    20 0x5c1b5cf30a1cce5ae5e7144ef9432d6b2c37a3a8      1
    21 0x5cc1e6229708533ac0f5e9e98931334341ff24c2      1
    22 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    23 0x7e5b399e254665590266ac6a9e2a1e3336576cc0      1
    24 0x7f9a2c2faef7be3ae141717ab7a5be35f189a576      1
    25 0x84a56dfe47950fb3901ac181c1ac8d8cbc84d919      1
    26 0x8e5c31b10cb78c1f99622f4235ec096ad7315d5d      1
    27 0x93a781d6e6a8e8916ea766a40c46ed2064b55141      1
    28 0x9b4b51c7e560ff625997c7cae4f9fe4ff94b5f12      1
    29 0x9b894269d53239ae2185e47041816799f3d8cf4c      1
    30 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    31 0x9e315e9701908501f6dc68a2af6e28a20c75d970      1
    32 0x9f165ae9abd3c8c729f27c6c363b41dfedaccc0e      1
    33 0xa062265be62ddb86e47f1eaec423702dad5ba60c      1
    34 0xa2272469c824d2b34407c08c1b377f3027406fea      1
    35 0xab6ca2017548a170699890214bfd66583a0c1754      1
    36 0xb16ef63f13b5233a738a477a7207220f5d2168ce      1
    37 0xb510097b3e248903ddfe0f3e5dd3511a4102a1f9      1
    38 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    39 0xc7553b988a8b6293a62b918a365986af7f10b6ee      1
    40 0xd185e7e5e3a95e723736e2825ba0c2e9447f19d4      1
    41 0xd6dbcf1f3145f11f99d07bce1a8b9a8bff47f271      1
    42 0xe597dcab464993af05402dd8310f3f85291efdb6      1
    43 0xebfb711b1a42b51123aa3397eb372bbca11b4d97      1
    44 0xf3478bf2d1120590fbf4cf4544c70d20237037d8      1
    45 0xf794530e92d99690c70ce66c85d8b603c092e6ab      1
    46 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1

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
