
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:25          Length:25          Min.   :1   Length:25         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:25         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21246969 # https://etherscan.io/block/21246969
block_hash <- "0xabdd89d2b8753b380a23b2661d8f44e893d16559d12a3be8af53dcf80b2c46d0"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4699 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Thanksformyfrens","Goyong11"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("GoyongEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Thanksformyfrens","Goyong11","GoyongEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 14 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     2 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
     3 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
     4 0x47d9e2c5aaa56bb47b4ddee929840b11be55da77      1
     5 0x7b640407513bc16167ef3450fd6339803982e976      1
     6 0x8e02c045c6690749393e9c3b8fd6befa651cb718      1
     7 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
     8 0x9d3c936d7e2ff74b068862f439925b570e46775f      1
     9 0xadd2c95b591c28e48d8b6b94043ee6da40fffb5b      1
    10 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    11 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    12 0xd60f499d1a45e9aadf9633b460b2c96030eb827b      1
    13 0xe50f359b5bc6dac1d5eb19e1b2fde2cf322d6c02      1
    14 0xeaa561c4e6110589ff2af64c78182780e5725b27      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 2 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x4162526bc6b992da53756bcfd2071f9b61decbbb      1
    2 0x4a39ae58b605102913ac19b7c071da75b55b2674      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 16 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     2 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
     3 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
     4 0x4162526bc6b992da53756bcfd2071f9b61decbbb      1
     5 0x47d9e2c5aaa56bb47b4ddee929840b11be55da77      1
     6 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     7 0x7b640407513bc16167ef3450fd6339803982e976      1
     8 0x8e02c045c6690749393e9c3b8fd6befa651cb718      1
     9 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    10 0x9d3c936d7e2ff74b068862f439925b570e46775f      1
    11 0xadd2c95b591c28e48d8b6b94043ee6da40fffb5b      1
    12 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    13 0xd4a08cf067c83d1b2cc1d26831569b7850804be7      1
    14 0xd60f499d1a45e9aadf9633b460b2c96030eb827b      1
    15 0xe50f359b5bc6dac1d5eb19e1b2fde2cf322d6c02      1
    16 0xeaa561c4e6110589ff2af64c78182780e5725b27      1

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
