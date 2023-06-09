
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:371         Length:371         Min.   :1   Length:371        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:371        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17434069 # https://etherscan.io/block/17434069
block_hash <- "0x629bf40890e8d731d82b4552531edb6e6131c19fb6374fc6432db00b1447606d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4405 

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

allow_memesRandom_phase1   <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=69,address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Random Memes Phase 1

``` r
c(allow_memesRandom_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random169memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0112b93de32e5353402bec6d177af4f8c8cd40fd      1
     2 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     3 0x0b983c0502ccb2dc598847b467a1b36c1ae8d8c2      1
     4 0x0c0d8f387030c631de001d99d40b2e519cf4d10f      1
     5 0x0e5c3e22b7239cef46f56741ff66369f3cfecb77      1
     6 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     7 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
     8 0x15237b9fc8c244246abee701f07e42185c5111c3      1
     9 0x16bf2b9490348b21bea767ff4a3d3c82e3059f12      1
    10 0x1c172d05b75178fc669d74407243cc932030f139      1
    11 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
    12 0x2227cb63299db1ac748347be2eecd8238efd593d      1
    13 0x2249f5016b1b7a306c5e98765bf94fd0a70d8b45      1
    14 0x231595e3673a10e846803194f4982e1cf3389161      1
    15 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
    16 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    17 0x3135d041d207a3c3c4edfeba5cd1956439b5de38      1
    18 0x332ab3f0dd19b601e988024bce17c9795a8c88f7      1
    19 0x373db7e01ebfb92d9095ae6b8f6e3d24ce6f4d4d      1
    20 0x3d055040d91414fbdc0a9eb0cb4e1bdf222fb1e1      1
    21 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
    22 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    23 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
    24 0x445816ca9bb0146940b802d49ce146787c73b5ca      1
    25 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    26 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    27 0x5b8dcbad4df5e00c20c35a0339f12259ba40f2a6      1
    28 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
    29 0x5f2ada0a1cfc9dd8996cf4be20b83edc9d88a5cf      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x6cb575052aa5c146de209ab813c3f6ab82424bcb      1
    32 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
    33 0x6e7d6b505deb2c67fb64ece9995a26b8ad70678f      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x74a228c2e1f7d8f4b0a336bc024021853da8a696      1
    36 0x7b343c896f85657976dfc88f282a69fb2e2a99ba      1
    37 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    38 0x8af0b9a9b751e086122bc340188bd9d99b8c7ec1      1
    39 0xa1486dae9f01e7da3bd8efbd12af0e1be1c73b60      1
    40 0xa7e1a7cae85805814f6b516f7e231c488666267d      1
    41 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    42 0xb40f68ae2afa5ff105a57e1e08239a51dd0962c4      1
    43 0xb98be1aba8ae2adef0daa18247273fa06021daae      1
    44 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    45 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    46 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    47 0xbf949494127d3cd72cd3399d4ab38312757f4d12      1
    48 0xc81635abbf6ec73d0271f237a78b6456d6766132      1
    49 0xc896866e927e6f8a416ba209976115e79fa0a66f      1
    50 0xcc946cb041c7d0a7493c9ea639353bfb70595812      1
    51 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    52 0xcdf03403e1fd9992f381f19ec0018c36a9e2f5c9      1
    53 0xceab2935e06c646e560e2f6083c55db6e8e12099      1
    54 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    55 0xceeab2af38e6b086cdce120c49f93b65f0b92b76      1
    56 0xd1e508c60d923cc9e93785da0bffe01710ccbac1      1
    57 0xd36590461162795ee33099b2076a0d4e017ae17c      1
    58 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    59 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    60 0xe0753cfcabb86c2828b79a3ddd4faf6af0db0eb4      1
    61 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    62 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    63 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    64 0xeaaeeb152d21122a8782bbb046edf48d0d9a389d      1
    65 0xeb775bf133c142e873f2ba6925d53107550e8703      1
    66 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
    67 0xf476cd75be8fdd197ae0b466a2ec2ae44da41897      1
    68 0xf7474070f2b9843df17e41213b6d89a1a6648b86      1
    69 0xfbfd507f49a08b9b5f0a71231f69a38864d4b9ba      1

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
