
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16338969.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:30701       Length:30701       Min.   :1   Length:30701      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:30701      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16339669 # https://etherscan.io/block/16339669
block_hash <- "0x149e69dc11d9dfc36b52b7ab5a75982b1b543fb1fe16036fbc69a443e3015db9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4696 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=10,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=42,address_max=1)
airdrop_falcao      <- pick(snapshot, contracts=c("SuperRare","Foundation","Creativedaydreams","ArturCosmicWaves"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     2 0x44922eafa08f8ace07fd68b60713e399360451b3      1
     3 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     4 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     5 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
     6 0x69e68074f1aada957edd39c5eae0069973343f30      1
     7 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
     8 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
     9 0xdb561a899557404581e6180fe6d4178577dc117b      1
    10 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x09309b99f3b43648734caeeaa752f77c679a2918      1
     2 0x0aceea7577fdcedfa1d60588db15e18b8d763f3f      1
     3 0x116fceb99ee2f290c218970b5a2971cfe00d3f25      1
     4 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     5 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     6 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     7 0x29f901a1e829e19387e9db644ddaaa26c3c41d66      1
     8 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
     9 0x2f944738905f93ec3fba145463e0854b7da77f71      1
    10 0x39bba3c54a3412127bdfb0f218efea52b5d28300      1
    11 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
    12 0x4b3e77089c5eb6558b5355eab3bf2ee4156bf2a9      1
    13 0x5022cf478c7124d508ab4a304946cb4fa3d9c39e      1
    14 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
    15 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    16 0x6928bfb114d228dffa0604f990d4a964bf1b6e61      1
    17 0x6bb59e15545dc9ab0949410cb3d8402ced7fef98      1
    18 0x7577a232f7e33b15bbb0c0b3238995f499fbbbad      1
    19 0x7cf91c0ef074f4e3fbeebda55dde44ddbd20443e      1
    20 0x7e3569b7d799ffee7a4b446c29c3c19d1d5e63ad      1
    21 0x8511a533d0fd5a46d8c403549277213c95f8bd34      1
    22 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    23 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
    24 0x9929fef8c8f3e09e6e7d9caa69e50ce185a95a69      1
    25 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    26 0xa158ffb97cc5b65c7c762b31d3e8111688ee6940      1
    27 0xa47073efce1849e0db5f3d558cccc237a988c601      1
    28 0xaa2cfa73b429d1189a8d72d0d9a7b1557590eef4      1
    29 0xaf469c4a0914938e6149cf621c54fb4b1ec0c202      1
    30 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    31 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    32 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    33 0xc11a862ba1125a1bc6b4ff11b2ead6b8e73ea52f      1
    34 0xc665a60f22dda926b920deb8ffac0ef9d8a17460      1
    35 0xd41df4a30fbe6b282ed6294a704fb33090557566      1
    36 0xd4e1a607d3ba4ac49f8517f8cfe011b5fb57b6f9      1
    37 0xda599dedef2d8c00c01cd047bbbedc7399ad5908      1
    38 0xdc6babb7e90b27e3f6be77d35eb03aa612b92efd      1
    39 0xeafc83bd210706fc3930a32dcfd8920e953c3622      1
    40 0xee34bbb4882c0ca569f70901217e6aeb3b347b62      1
    41 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    42 0xf52393e120f918ffba50410b90a29b1f8250c879      1

## Airdrop Artist

``` r
c(airdrop_falcao) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 21 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1d28a703ab06bcef8666e4692fe63188b6fcbca2      1
     2 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     3 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     4 0x3407e0471c74b816094b454c485573e676085b30      1
     5 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     6 0x36e10f7bb46a7e8a9000e20ac6bdc20721d15f1a      1
     7 0x3734c5ff3db6378fd4fed019ec6ff347350a17c3      1
     8 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     9 0x42f34449209059717e6c48ed0110783a7df82abf      1
    10 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
    11 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
    12 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    13 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
    14 0x641b05e7b36ac69b66ef20cb6853cd185e300f43      1
    15 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
    16 0x7a76fcc57262b28d2a86fa4e5c9cb7ce8d386b3a      1
    17 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
    18 0x9769334fc882775f4951865aa473481880669d47      1
    19 0xa23cb68780be74b254a5f7210ec6cf1c76289953      1
    20 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    21 0xd9d23db5632262033ad9f73ac8fbba8d76d00188      1

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
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     7 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    10 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    17 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    18 0x44922eafa08f8ace07fd68b60713e399360451b3      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    24 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    25 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    26 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    27 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    28 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    29 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    30 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    31 0x69e68074f1aada957edd39c5eae0069973343f30      1
    32 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    33 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    34 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    35 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    36 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    37 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    38 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x82139687faae8a29851902783e02e699de0e0846      1
    41 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    42 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    43 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    44 0x8ea76483c888f5bda7d96cab9839488f691daf78      1
    45 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    46 0x928fea96ff6e9460b094ca6e387a87d6053780b6      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    49 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    50 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    51 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    52 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    53 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    54 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    55 0xb5374cac6cad6b025246f19d20b0d4151b640558      1
    56 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    61 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    62 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    66 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    67 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    68 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    69 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    70 0xdb561a899557404581e6180fe6d4178577dc117b      1
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
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    3 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1
    4 0xb5374cac6cad6b025246f19d20b0d4151b640558      1

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
