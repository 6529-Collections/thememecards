
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:38          Length:38          Min.   :1   Length:38         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:38         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18896969 # https://etherscan.io/block/18896969
block_hash <- "0x774d7975d6bbeab3e65113f4600c7818fe65c098e8409437ee80eb4e81b0e4fa"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4626 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","Foundation2","ATMOWATCHERS","KnownOrigin","KnownOriginEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x076610a4494528a637a4fa0cf1119c6d15a66754      1
     2 0x079073fb29f456d74ab5f49d7106e378205a346b      1
     3 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     4 0x0e7a408a775af29310970de51b59501e21eee87a      1
     5 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     6 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     7 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     8 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
     9 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
    10 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    11 0x4b9ddf66215e6989a8d2b6c0c1cd006717ff1828      1
    12 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
    13 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    14 0x95b2db3f49b8a5365afacc6342f5ece0d253f8e0      1
    15 0x9abc30f3fe9e2d8fb49bd9d2376a22da2dfd44f1      1
    16 0x9b144b726449507eff54faf4607fdb3d28512417      1
    17 0xa3b26caa88366cb15cacce46a75aff15af0c1fdd      1
    18 0xa7071299be9de85a7ac0c170e7f129856b7adc9f      1
    19 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    20 0xaeace7f1deba36e5fa1ef9d8eed3e666183c1180      1
    21 0xb89d68928ecae680c0a5b0444e361a5b62f69554      1
    22 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    23 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
    24 0xc5aca861be2856d25821f3ef0317950c369044fa      1
    25 0xca0ffa49c58a571f54b7bff66e8102b35d69fcf5      1
    26 0xcc21abf00a0c589c9818e6ac438d8759ca5e611d      1
    27 0xdb416840b876a6cfd878ba6af02238d146dc016f      1
    28 0xde84f3e420b8c7824eaa27c4aaedabcd43af5d69      1
    29 0xe6be2096dbe9560cfb01b45804514d075d6585bc      1
    30 0xf2a9c5390aa23abb1b6c50cd387eaa4e4cf986c5      1

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
