
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "15964158.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance           contract        
     Length:15962       Length:15962       Min.   :   1.000   Length:15962      
     Class :character   Class :character   1st Qu.:   1.000   Class :character  
     Mode  :character   Mode  :character   Median :   1.000   Mode  :character  
                                           Mean   :   1.706                     
                                           3rd Qu.:   1.000                     
                                           Max.   :4116.000                     
         name          
     Length:15962      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 15967569 # https://etherscan.io/block/countdown/15967569
block_hash <- "0xb4a877f65102609d56019bc2eb2dcb4208f9c0e4ff58350d62c458366e2d4d66"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4514 

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
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d"
)

hodlers_remove <- c(
  ""
)

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"),address_remove=address_remove, address_pick=20)
```

## Airdrop

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     2 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     3 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     4 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
     5 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
     6 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     7 0x575f6540c16a72696c14a17fa64f049992d661ab      1
     8 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
     9 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    10 0x9dbd781eeba135ad2a779926880adc89196a3265      1
    11 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    12 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    13 0xb9cf551e73bec54332d76a7542fdacbb77bfa430      1
    14 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    15 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    16 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    17 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    18 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    19 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    20 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

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
