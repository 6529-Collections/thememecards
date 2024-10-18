
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:22          Length:22          Min.   :1   Length:22         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:22         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20948869 # https://etherscan.io/block/20948869
block_hash <- "0x60c94bc4eee91177a40bca05dbc85bcf7e00ff63429dc314c69d4a21e6b64bc9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4834 

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

allow_artist1    <- pick(snapshot, contracts=c("ZTOWN","B4r0q","MAIFDeepFake","SuperRare","Upskydown"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LifeasaCarCrashEditions","AdamEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("ZTOWN","B4r0q","MAIFDeepFake","SuperRare","Upskydown","LifeasaCarCrashEditions","AdamEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     2 0x2531896dfb32418babf6c57edd40f8ee53c0f236      1
     3 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
     4 0x2e6a4988e6ea932673f6901d6b93c78ff182adae      1
     5 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     6 0x3fa5b646b19271033f059ec83de38738f3e3163d      1
     7 0x4bf7db76757302876d319ab727e26ab66753128c      1
     8 0x4e2e67836d10b02b1dce78591af6dfd1e2d7bcba      1
     9 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    10 0xc7cfe2de78360c25dde3c8c7525c4f4f6cae30dc      1
    11 0xd2ba592032a550b4ce5584a44db62744fa73210e      1
    12 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 8 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x16fdd091c5184a5a5ffb501b09d2b6bea17b6721      1
    2 0x1e96b3d06c7dccc94f122f435d989a3efae84ff4      1
    3 0x24c3833219ab8128644f37cbb51546cdfbce6c02      1
    4 0x40eb2d2c6f330ae244800accbb31f608f82cbcc9      1
    5 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    6 0x73277511ba2d8d0009eb92d7acc5948abf541849      1
    7 0xac5914f52d77198d16b0eaa6c976e48110adce08      1
    8 0xbb517a3ebf0aca5f7b420f8798ca676894bf8777      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x16fdd091c5184a5a5ffb501b09d2b6bea17b6721      1
     2 0x1e96b3d06c7dccc94f122f435d989a3efae84ff4      1
     3 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     4 0x24c3833219ab8128644f37cbb51546cdfbce6c02      1
     5 0x2531896dfb32418babf6c57edd40f8ee53c0f236      1
     6 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
     7 0x2e6a4988e6ea932673f6901d6b93c78ff182adae      1
     8 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     9 0x3fa5b646b19271033f059ec83de38738f3e3163d      1
    10 0x40eb2d2c6f330ae244800accbb31f608f82cbcc9      1
    11 0x4bf7db76757302876d319ab727e26ab66753128c      1
    12 0x4e2e67836d10b02b1dce78591af6dfd1e2d7bcba      1
    13 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    14 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    15 0x73277511ba2d8d0009eb92d7acc5948abf541849      1
    16 0xac5914f52d77198d16b0eaa6c976e48110adce08      1
    17 0xbb517a3ebf0aca5f7b420f8798ca676894bf8777      1
    18 0xc7cfe2de78360c25dde3c8c7525c4f4f6cae30dc      1
    19 0xd2ba592032a550b4ce5584a44db62744fa73210e      1
    20 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1

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
