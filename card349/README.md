
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:43          Length:43          Min.   :1.000   Length:43         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.163                     
                                           3rd Qu.:1.000                     
                                           Max.   :5.000                     
         name          
     Length:43         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22246369 # https://etherscan.io/block/22246369
block_hash <- "0x2ac336780fc24e8826e5ec15fb829d301dec0f3fa0bd3990a02f7bc8ee6512d7"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4743 

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

allow_artist1    <- pick(snapshot, contracts=c("MMXXII","ResHumanae","MementoVivere","Foundation","KnownOrigin","TrueColors"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("UnveiledEditions","MetaverseFurnituresEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("MMXXII","ResHumanae","MementoVivere","Foundation","KnownOrigin","TrueColors","UnveiledEditions","MetaverseFurnituresEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 21 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x156a4faaf9fe84a3bf85f5267ab32379936c22a4      1
     2 0x1732f2119ba40341cd9299062ef51454af98bfe8      1
     3 0x2cf4fb0ee32cc8a1b32b153798e003893eec8c20      1
     4 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     5 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
     6 0x60bc3d4a757315ff686b60f6fd7569aa4f3acb88      1
     7 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
     8 0x75a473c33bffb61e945d86b37113c0859965a789      1
     9 0x7f568aa844611b264073f4162aed6e646251b893      1
    10 0x82f23de5a474fc904041c357576afe53c30dd250      1
    11 0x93b63028dd964318ce6c8c0437be0097f35bc366      1
    12 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    13 0x9528a84713336eac7059e39bd272856d44c19269      1
    14 0x981b207d9aaad44aa55235aa753c7aecf4f4ee50      1
    15 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    16 0xa71f5fb588bfe3a60b65b21a25ea2cdcaf7e9e42      1
    17 0xab6ca2017548a170699890214bfd66583a0c1754      1
    18 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    19 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    20 0xf491a30709978ff8b4d99f2daf2dd22d8425e686      1
    21 0xfc48426da0338735945badef273736ccff53a358      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x08e91fd9717d66270ad0e76a950e9b396de30788      1
     2 0x29165e46a5ca80de8ae9f9f8fdc561a0eb13e46f      1
     3 0x56f4b60dc45eb4c26e6b0f8933c35b7853e5b809      1
     4 0x5a9981d3241910797d5f930b8dd80a0705a3929a      1
     5 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
     6 0x79e561168835c783240a0637320d308897bd0922      1
     7 0x8514aa48bbb79e9c231890a829ca33984d38ccf7      1
     8 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
     9 0xc9b7d4c81c8999fd7b7abb8e0925505a5e6a4909      1
    10 0xf3846ec842c2490d6289858e28038cfbec660689      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x08e91fd9717d66270ad0e76a950e9b396de30788      1
     2 0x156a4faaf9fe84a3bf85f5267ab32379936c22a4      1
     3 0x1732f2119ba40341cd9299062ef51454af98bfe8      1
     4 0x29165e46a5ca80de8ae9f9f8fdc561a0eb13e46f      1
     5 0x2cf4fb0ee32cc8a1b32b153798e003893eec8c20      1
     6 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     7 0x56f4b60dc45eb4c26e6b0f8933c35b7853e5b809      1
     8 0x5a9981d3241910797d5f930b8dd80a0705a3929a      1
     9 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
    10 0x60bc3d4a757315ff686b60f6fd7569aa4f3acb88      1
    11 0x65b27ba7362ce3f241dafdfc03ef24d080e41413      1
    12 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    13 0x75a473c33bffb61e945d86b37113c0859965a789      1
    14 0x79e561168835c783240a0637320d308897bd0922      1
    15 0x7f568aa844611b264073f4162aed6e646251b893      1
    16 0x82f23de5a474fc904041c357576afe53c30dd250      1
    17 0x8514aa48bbb79e9c231890a829ca33984d38ccf7      1
    18 0x93b63028dd964318ce6c8c0437be0097f35bc366      1
    19 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    20 0x9528a84713336eac7059e39bd272856d44c19269      1
    21 0x981b207d9aaad44aa55235aa753c7aecf4f4ee50      1
    22 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    23 0xa71f5fb588bfe3a60b65b21a25ea2cdcaf7e9e42      1
    24 0xab6ca2017548a170699890214bfd66583a0c1754      1
    25 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    26 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
    27 0xc9b7d4c81c8999fd7b7abb8e0925505a5e6a4909      1
    28 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    29 0xf3846ec842c2490d6289858e28038cfbec660689      1
    30 0xf491a30709978ff8b4d99f2daf2dd22d8425e686      1
    31 0xfc48426da0338735945badef273736ccff53a358      1

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
