
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:56          Length:56          Min.   :1.000   Length:56         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.054                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:56         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21700669 # https://etherscan.io/block/21700669
block_hash <- "0xc05ce77ad4d40cc9832368d87ed7caaee8f34e052de4cdcc32fc7715b39fad83"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4951 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","No0ne","NoWhere"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("InvasionEditions","No0whereEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","No0ne","NoWhere","InvasionEditions","No0whereEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 15 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     2 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
     3 0x14de5ffba8b58036212695bc0396cf88ee6c3b3d      1
     4 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     5 0x782adafbf47a604f146af4a059908e946eae539f      1
     6 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     7 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
     8 0xab535e4a0c7e3d9cf659482e3c160a8d16b0911e      1
     9 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    10 0xb159f3453da87efc0582ef3e6dae8f03482ec85f      1
    11 0xc8f42217b6b1768012b4431f922be0759fed7397      1
    12 0xe0fcae8d6bf57b96c3196ddbc0c8f3e73ccb7020      1
    13 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    14 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    15 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 34 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     2 0x0d9d3042c669f622c92f2bb485e24bf43d6dbf82      1
     3 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     4 0x29165e46a5ca80de8ae9f9f8fdc561a0eb13e46f      1
     5 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     6 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
     7 0x577c58ebed517748b752252de2a7cbdb92c3a739      1
     8 0x57aa36d30d315791a8222d5fbfd76c1608e961cc      1
     9 0x58847353d941840f261366561dab73bcce277b6c      1
    10 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
    11 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    12 0x61f5c63f6d56c0c7a83291aef8385f5de22efc58      1
    13 0x65a7a1cc03b05d8060f4e464ea79693ace125245      1
    14 0x853c210b5f6c8822823353cc21b65a32bfd6a75a      1
    15 0x8f791f061d7f36dc07de081ad88f87d71be1585e      1
    16 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    17 0xa46468b3d736e42184577660b0c1a50d4f03e599      1
    18 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
    19 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    20 0xb30c415a850477bb86e16410cd30865470ef9165      1
    21 0xb5fd638924ffd3a937f3e9724c46ae505cf784eb      1
    22 0xbe25e3c60b4969de23fb10930043e44f987cbc1b      1
    23 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    24 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    25 0xccc6a22733342c3559048159134eefbf8abc2fa1      1
    26 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    27 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
    28 0xd089f85515b0127bce20178ca1c1fe8c4765f91a      1
    29 0xd49e55b8078dec84a3c0315fd76d89047768d0a3      1
    30 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
    31 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    32 0xf2439241881964006369c0e2377d45f3740f48a0      1
    33 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    34 0xf566c12516d171b451dacb93c114753c07f980e0      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 49 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     2 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     3 0x0d9d3042c669f622c92f2bb485e24bf43d6dbf82      1
     4 0x0da1593b61973dc8eba3b29da378cff5911d46b8      1
     5 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     6 0x14de5ffba8b58036212695bc0396cf88ee6c3b3d      1
     7 0x29165e46a5ca80de8ae9f9f8fdc561a0eb13e46f      1
     8 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     9 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
    10 0x577c58ebed517748b752252de2a7cbdb92c3a739      1
    11 0x57aa36d30d315791a8222d5fbfd76c1608e961cc      1
    12 0x58847353d941840f261366561dab73bcce277b6c      1
    13 0x5936eeab739c6db9a0bb5de8b79a4bbee65200a0      1
    14 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    15 0x61f5c63f6d56c0c7a83291aef8385f5de22efc58      1
    16 0x65a7a1cc03b05d8060f4e464ea79693ace125245      1
    17 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    18 0x782adafbf47a604f146af4a059908e946eae539f      1
    19 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    20 0x853c210b5f6c8822823353cc21b65a32bfd6a75a      1
    21 0x8f791f061d7f36dc07de081ad88f87d71be1585e      1
    22 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    23 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    24 0xa46468b3d736e42184577660b0c1a50d4f03e599      1
    25 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
    26 0xab535e4a0c7e3d9cf659482e3c160a8d16b0911e      1
    27 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    28 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    29 0xb159f3453da87efc0582ef3e6dae8f03482ec85f      1
    30 0xb30c415a850477bb86e16410cd30865470ef9165      1
    31 0xb5fd638924ffd3a937f3e9724c46ae505cf784eb      1
    32 0xbe25e3c60b4969de23fb10930043e44f987cbc1b      1
    33 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    34 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    35 0xc8f42217b6b1768012b4431f922be0759fed7397      1
    36 0xccc6a22733342c3559048159134eefbf8abc2fa1      1
    37 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    38 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
    39 0xd089f85515b0127bce20178ca1c1fe8c4765f91a      1
    40 0xd49e55b8078dec84a3c0315fd76d89047768d0a3      1
    41 0xdd0c252067e1197ef2c4ec48ea024d705159ed0a      1
    42 0xe0fcae8d6bf57b96c3196ddbc0c8f3e73ccb7020      1
    43 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    44 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    45 0xefffdc05e7c5b305fbd504366b01f2d6424cb8c4      1
    46 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    47 0xf2439241881964006369c0e2377d45f3740f48a0      1
    48 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    49 0xf566c12516d171b451dacb93c114753c07f980e0      1

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
