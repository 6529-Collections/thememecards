
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:24400       Length:24400       Min.   :1   Length:24400      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:24400      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17718669 # https://etherscan.io/block/17718669
block_hash <- "0xf2807227ec1eab1855edee58ece8f75c5920b769de19c07542f7ee561ceef547"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4782 

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


allow_gradient_phase1    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_memesRandom_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=50,address_max=1)


allow_raw                 <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles             <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

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
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x52cc2c8db59411310e130b47d478472d9f7e4597      1
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

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0400ad7432b46d22aaa70a1cae09d22ac600c8f4      1
     2 0x12dbcaf089d921da60fe5dfaef912f11633b7dc7      1
     3 0x1cb3acfd25eb1d1db28ae60cbb78bda352297266      1
     4 0x1dda870a07e2604fc107aa72753f2aa4dada6c2e      1
     5 0x1ed6c39c8b8f21e25cf766ab2c19d6d51542f815      1
     6 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     7 0x2151aba37c89da616ea293b46113731ea7aa63ac      1
     8 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
     9 0x49bc851b355479d1241d86607457feeb9026826f      1
    10 0x54c375c481f95ba43e2cecd6ef30631f55518f57      1
    11 0x58428a7fee1031744fa48f040b551d98b3924e5d      1
    12 0x5d15989394195207534a9ecbf582d712a2d2ebe8      1
    13 0x641f4c56d17988bb9ad657c6961baca41dcf5223      1
    14 0x668df49e79e6a828b27f455297291a8bd2fe0531      1
    15 0x681a64617e1083c36cbebbd8ac9e64938d3c2591      1
    16 0x6f7a49da0f184509814039956137dadb9ccda4f8      1
    17 0x768f9b057b70d50ed439220a6ba5f3065b041609      1
    18 0x7ec072d344fdab56b28c32fd9be1bbde2029e2b8      1
    19 0x8047672c2df5a47c98c139e8fb7b403a13802956      1
    20 0x80e2db20022f9a4c372897f97c09dbcb74c12820      1
    21 0x8d900c8aca5b3846c9ad2cbf916f20babd95f406      1
    22 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
    23 0x91a3ebce4e0d326539c6e2720f25449774cb615e      1
    24 0x96b615975a4017c2230e93727db65c7bf96cc2bb      1
    25 0x99fc4dd947bf9569deee3710b28fcf26797c6870      1
    26 0x9fce6fcdf1f17821deb1a7e078c3c7941ec465f4      1
    27 0xa03d04ff89308adf54badf18b46fee9980093524      1
    28 0xa0c1bca2da31d84180114965d2f80e37b63030ec      1
    29 0xa4f6e6735278cd9f66990a92778d09c50a7fc14c      1
    30 0xa5e6b86b278ae2511518e0837047ae9595777434      1
    31 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    32 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    33 0xb3eb9bc116fcf22c3d6d5f920855d4bf34a9b0ba      1
    34 0xb749a586080436e616f097f193ba9cb6a25e7ea6      1
    35 0xb79157bca1635f4731df5828f2a28a0cbfc799ce      1
    36 0xbebd1e62ba2412d9378847cfd7cc3f759de1504e      1
    37 0xc6165f99edc566743a0528b1d8bfc6a038e8e4fc      1
    38 0xcaaf72105cb8dd7a1234988a635e243e1621ad3b      1
    39 0xcb7c599cac4e944348145f5dad93b682c1b87d24      1
    40 0xcbaddb16544a736b48b455812d28ee71a54ad6c0      1
    41 0xcca3d4ece24a32ae1b037ea2fe14fb78163bcba6      1
    42 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    43 0xd5e7af4b667ad96403b26b6f44fc31190a2cd8dd      1
    44 0xd5f7931cde4e1491de0b0fc14d958302075875a3      1
    45 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1
    46 0xdd0c246165c8b4a90bc5a446da9ee0908a45966f      1
    47 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    48 0xf0b4e5f9bf9e02637748153b956819cd3e96326a      1
    49 0xf2ff5c8fbebc6049bbcf7dcd857171e54f8824cf      1
    50 0xf6cb64ad324d2ff8ff26ff90360e2835086a9b3a      1

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
