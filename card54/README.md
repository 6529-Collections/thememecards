
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16410469.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:34485       Length:34485       Min.   :1   Length:34485      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :2                     
         name          
     Length:34485      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16411169 # https://etherscan.io/block/16411169
block_hash <- "0x8e6385adac835fccfb57da49b4d975e8f6ee5f883054c0c4c8775305ea118a73"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4730 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=9,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=20,address_max=1)
airdrop_mindzeye  <- pick(snapshot, contracts=c("SuperRare","Foundation","Italy"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_mindzeye    <- pick(snapshot, contracts=c("SuperRare","Foundation","Italy","PragueEditions"), address_remove=address_remove, address_subtract=airdrop_mindzeye,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
    2 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    3 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    4 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    5 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    6 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    7 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    8 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    9 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1

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
     1 0x030571c35b84e01613383d631c10cc8afaf83977      1
     2 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     3 0x1dd789345c1bad32ea2c3dd8989f38e5362f4f31      1
     4 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
     5 0x2feb2bdf85a303f9b13d0fd47dda36c0a82193af      1
     6 0x38745e1176ca9a141929ff67093c7f8ec77e6e73      1
     7 0x4536034e4412e42a0470a6143df01206e30c5995      1
     8 0x61f2d8e2b5ad7a47b2217197f5a5a3df25ae915d      1
     9 0x70e7a6621f4cb3c3e073d0539899f49fc88424c0      1
    10 0x73e9f114536c6807b6d9388bbf76f5404c621a77      1
    11 0x7bb3fad216678a47caeed210123fc9e9e263030e      1
    12 0x92b6e00f5d83cf43e2ea55865e8cdfca676c0d9c      1
    13 0x9617562c6f338ed18e2e8e88cbbffce6003bae0e      1
    14 0x96fe88a887f91aca95b7bdfd0b7bc045f79beead      1
    15 0xa149c11913c6afe72d07d1276df859dde0e4759b      1
    16 0xa47073efce1849e0db5f3d558cccc237a988c601      1
    17 0xaf664a54b35ebddd483ad6a7c8030229b4ed745c      1
    18 0xb923cf46609d7fbcf1b923092c7636d05506cbc9      1
    19 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    20 0xf49ec27f278295b94d7c3badaaa954f0af278fe0      1

## Airdrop Artist

``` r
c(airdrop_mindzeye) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 23 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     2 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     3 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     4 0x3c2b4bdcd2f742c55186fc599cb733a127e2b8ab      1
     5 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
     6 0x547e56e9b748ba28d4ddd354da89521f9f80eeb5      1
     7 0x5b0301950d850fcff4fc6a2c0b19382ca3e9646d      1
     8 0x5c396e679cc63df028e4c7eab88543cf18210fa1      1
     9 0x6456f20453a383004f17b921cd770d187fcd30ef      1
    10 0x76d078d7e5755b66ff50166863329d27f2566b43      1
    11 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    12 0x8497277c9339170a0420e86da8352e0c084624cd      1
    13 0x8be3fd8a423c38131aae0330b20ceec425e0e50e      1
    14 0x91b7f6b94554cf9e1dc041d9a15f5bec30b71166      1
    15 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
    16 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    17 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    18 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
    19 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    20 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    21 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    22 0xe793b3549bf6f293151b2378a8b098ad00c4653e      1
    23 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    17 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    25 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    26 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    27 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    28 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    36 0x71677455ef1479a96596cb7fb894d16dbe6e792a      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    40 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    41 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    42 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    43 0x82139687faae8a29851902783e02e699de0e0846      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    48 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    49 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    50 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    51 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    58 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    59 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    60 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    61 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    62 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    63 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    64 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    65 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    66 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    67 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    68 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    69 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    70 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    71 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    72 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    73 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    74 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    75 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    76 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    77 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    78 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    79 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Artist

``` r
c(allow_mindzeye) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x234f4e58df75cae6ef10c4635a22ba5b52dd561d      1
     2 0x287bbe2cf7dd6980b1d12d717895aad8972a4118      1
     3 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     4 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
     5 0x3deed956b999b83361b85bff31d388c35125411d      1
     6 0x49f9fb56043978fec51b200c84c64dc3959a3a68      1
     7 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
     8 0x684188e0344d3cba4fa9525b256b6ea035b3965b      1
     9 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    10 0x6c379f4159afbdd17ae4bb41b28480461f7101ce      1
    11 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    12 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    13 0x9e2771635a2f69c2db61e65b71303ee96e35904f      1
    14 0xa4af5d583bc25e1ea0dc08cf14b7b457757e8f52      1
    15 0xaf0286e500178ae109ee70ee016f25b659c611c5      1
    16 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    17 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    18 0xda846e13d8c2b08441581c559e79e49ac200f973      1
    19 0xe4c8335467ce3b1854dd828f14bbb33aa4771818      1
    20 0xfed473cd045563542414e31af846a43bf58fc59a      1

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
