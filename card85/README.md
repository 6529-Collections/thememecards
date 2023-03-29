
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:15982       Length:15982       Min.   :1   Length:15982      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:15982      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16917269 # https://etherscan.io/block/16917269
block_hash <- "0xb39402875c86d60c3c984e73c2097fe79f115a437aa3c56a838fd289ceefaf08"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4634 

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




airdrop_memes      <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=20,address_max=1)
```

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_20memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x10cf494cdbceb726d21703d21131e131ff03ae33      1
     2 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     3 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
     4 0x2fbd96729bbc8b02c3084583ada26fffc15278f1      1
     5 0x422f3eac277a5f3adcafc5d4054c04ae35a18ec4      1
     6 0x452f438aad8b675232c1fd7ff8e940d72d8a9f45      1
     7 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     8 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
     9 0x94bd59027a36469827442108631d4a07d0e08846      1
    10 0xa85fcfc40f1a3d9315011efb8b2d3f1f99b99530      1
    11 0xb742c00783ca98be2896eccfc0d2ed6dc8e87c7e      1
    12 0xbaa4aa9933b4548cff0be2693e7af7e14e9ee49b      1
    13 0xbdc4972232f1251b1d7f0313cff6f5e8dad678c0      1
    14 0xc2fa34a6fcb085f5e0295f233fae7fc90fbafe85      1
    15 0xd1f0c92bd9b149b5681ff2ef2213e4322033d0b8      1
    16 0xd3641bd03a67af07550b049065a19f84333b4b5b      1
    17 0xeabed8538923d8b8e0616938f8dc657f3cdf74c6      1
    18 0xf1339ec6a48c55c2f80157778f85c1064d7c77c1      1
    19 0xf448f4da8f09ff6f9303337001fc48eb83f3e1ee      1
    20 0xfe6c92d283fb88c6a1cbebac4ef33b1cafd118d6      1

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
