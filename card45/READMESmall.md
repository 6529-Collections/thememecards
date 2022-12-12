
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16165469_small.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:106         Length:106         Min.   :1   Length:106        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:106        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16167269 # https://etherscan.io/block/16167269
block_hash <- "0xfdbc91dc30682cbd6464ad643cf0c4c3496c0b87bab4fa657d902e84aa4f540e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4835 

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
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=50)
```

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memesRandom50.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1b84f9899ebc7d495b70a40ec16b36847f388e7e      1
     2 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     3 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     4 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
     5 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
     6 0x3592283e6b611f323db089b82946f6df2d1948ed      1
     7 0x388160a99390392278afdba240046b8b5e73f77b      1
     8 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     9 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    10 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    11 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    12 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    13 0x54913cc8ea17731d62589039dd0152f306473843      1
    14 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    15 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    16 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    17 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    18 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    19 0x69e68074f1aada957edd39c5eae0069973343f30      1
    20 0x69fde561275b85dbcd5081d1121bcae64fb83858      1
    21 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    22 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    23 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    24 0x82abb5df200d2998db4e80639b145d052ca40062      1
    25 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    26 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    27 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    28 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    29 0x9b96980c1c018cb617f6653f6892e19ecf4f81e1      1
    30 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    31 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    32 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    33 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    34 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    35 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    36 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    37 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    38 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    39 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    40 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    41 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    42 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    43 0xccc9bdd130f0c03fa5d12b9a85e9e66b087457ec      1
    44 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    45 0xd87ca052936bcc2b6283b87d2f0aa95cf0080584      1
    46 0xe1c22662f4f1320c6e5319d002b3c3d52f0d0135      1
    47 0xe25b24cebed3055236e369570a437a99e1d32602      1
    48 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    49 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    50 0xfeaf078e2599bc6844cd14c1b48a5ae84e91b06c      1

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
