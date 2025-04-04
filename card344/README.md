
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:72          Length:72          Min.   :1   Length:72         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:72         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22156969 # https://etherscan.io/block/22156969
block_hash <- "0xe5482185f30fdef59c3e3918935ba653a954764eb8fe8df3a3670429391d3d5c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4543 

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
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)



allow_artist_all <- pick(snapshot, contracts=c("SuperRare","FishermensArt","NaturesGenerativeArt","RothkoReimagined","Foundation","MountainRoads","ChasingTrains"), address_remove=address_remove,address_max=1)
```

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 48 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x0763e484268042be6e5743c731a7adf47a48ec19      1
     3 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
     4 0x152fb9bd558d013439653d4b25ff1b18154c5d7f      1
     5 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     6 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     7 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     8 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     9 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    10 0x2eea5c25d45b7861b24c5a36b3535222fb6a0aff      1
    11 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    12 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    13 0x377fb14e0823020e9bda6c21ad9cb8d98fd40191      1
    14 0x3e0326556146dd34f4b0442989a48fcd87f0ffd2      1
    15 0x3ed5c3f9061437ede251e53b5e2e8edcc36192b5      1
    16 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
    17 0x4d7e6948fb5ed2d92897de0605cd642eff69fbf4      1
    18 0x4ece75a3b5e1ced63ff249561d2f7ecda754d162      1
    19 0x564a8e13d7dd23d5525160d204165bdbcb69b4db      1
    20 0x6301add4fb128de9778b8651a2a9278b86761423      1
    21 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    22 0x6ce4235c42e803eaf8e25ebfb6a6c3b35caa8192      1
    23 0x76d078d7e5755b66ff50166863329d27f2566b43      1
    24 0x7c7f7749d31a19039cd35fcdc603e0322e684770      1
    25 0x82ba2d6184cb07c46687a89a62c4a9b5c584e87d      1
    26 0x8497277c9339170a0420e86da8352e0c084624cd      1
    27 0x84f27d305dd129184f5c870bdf172e2c59e4473a      1
    28 0x85e37cd123d2889410d9fd1f434c9936e882e5c6      1
    29 0x86fef6ec5320f6cf9231f524ae89e198419cdc0f      1
    30 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    31 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    32 0xa556a5a50d2c786617263414878214a9159d1433      1
    33 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    34 0xb1af684799c96f5c740786be59dc834ff8ef5add      1
    35 0xb75e6a898f4d7dd32efea8d27094432b0f90618d      1
    36 0xbc014fba270583d2f06c79186afef07c989889fd      1
    37 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    38 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    39 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    40 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    41 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    42 0xcf9799f5292bf594d1716e941c67bf77c29e1a8b      1
    43 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    44 0xd98009d2d013c74d3ffdbdcab3494d0e8f8bbaae      1
    45 0xdcbc0c22eb301fd12266892bdc21b316b88f494a      1
    46 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    47 0xe8c4156a19951deff3203f116fc63274da746baa      1
    48 0xee0e516dc52834a3834fe2254fc40ee84b03c80d      1

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
