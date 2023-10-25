
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:62          Length:62          Min.   :1.000   Length:62         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.081                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:62         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18396969 # https://etherscan.io/block/18396969
block_hash <- "0x41b009dd1141519a376fe1c6cd0a86814224f66209ed2f1cf18b48606761eac5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4446 

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

allow_artist1    <- pick(snapshot, contracts=c("BEFE","SUIT","UNCANNY","EVERYVERSE","SuperRare","Ninfa"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("BEFEEditions","NinfaEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("BEFE","SUIT","UNCANNY","EVERYVERSE","SuperRare","Ninfa","BEFEEditions","NinfaEditions"), address_remove=address_remove,address_max=1)
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
     1 0x0f1e63ad8985886300b980aa646398dc9e1ad7c4      1
     2 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     3 0x2b776b6418a7ae859c5e630afa3fb59e82b49fa8      1
     4 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     5 0x3d9456ad6463a77bd77123cb4836e463030bfab4      1
     6 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     7 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
     8 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
     9 0x6da0da6ad472b35deec827f5ba6e8963fb98a742      1
    10 0x745af2056a6fe8f5ec1e0f6bf532061f786cb9f4      1
    11 0x8497277c9339170a0420e86da8352e0c084624cd      1
    12 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    13 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    14 0xab6ca2017548a170699890214bfd66583a0c1754      1
    15 0xb722e190319010d6e412509b582a77dfd75a7858      1
    16 0xc734b794e323d0256695247c305ebc6964e57f60      1
    17 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    18 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    19 0xf0c3a1f1d9b79d5a8decb8e92973cdb56b7e81da      1
    20 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    21 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 16 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     2 0x134bb0bd854f4c422fc5377931ed29434180766f      1
     3 0x1db4556a7ed70112c755fc5706bb4980fafddfdf      1
     4 0x2c93b00ff220c5b0fcaef85d6ff01d1f1fd990df      1
     5 0x2ecc5e1f33df963f8164b46b4c342695d87e1f16      1
     6 0x473c923543d9c91209b087f456d4a796b52ef96c      1
     7 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     8 0x6e5033b09cb7de7dbfb6e7547cf274e09cff1ca9      1
     9 0x793274608864caf6f48e3c13201f35c45fed96a5      1
    10 0x8560cbe177a4b28320e22012df9c908aabe24dcf      1
    11 0x8c42c9f78fc6a34308d582cb48bc2198350f58de      1
    12 0x9c7b82c0302c7c945f9cf45a5c73e6f48ab84b14      1
    13 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    14 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    15 0xebfe1ab93d1122e065adcafd8c3174261e8e726f      1
    16 0xf0556eae3130c5d31cb4cd342f5b112c7bfc7416      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 37 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     2 0x0f1e63ad8985886300b980aa646398dc9e1ad7c4      1
     3 0x134bb0bd854f4c422fc5377931ed29434180766f      1
     4 0x1db4556a7ed70112c755fc5706bb4980fafddfdf      1
     5 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     6 0x2b776b6418a7ae859c5e630afa3fb59e82b49fa8      1
     7 0x2c93b00ff220c5b0fcaef85d6ff01d1f1fd990df      1
     8 0x2ecc5e1f33df963f8164b46b4c342695d87e1f16      1
     9 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    10 0x3d9456ad6463a77bd77123cb4836e463030bfab4      1
    11 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    12 0x473c923543d9c91209b087f456d4a796b52ef96c      1
    13 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
    14 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
    15 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    16 0x6da0da6ad472b35deec827f5ba6e8963fb98a742      1
    17 0x6e5033b09cb7de7dbfb6e7547cf274e09cff1ca9      1
    18 0x745af2056a6fe8f5ec1e0f6bf532061f786cb9f4      1
    19 0x793274608864caf6f48e3c13201f35c45fed96a5      1
    20 0x8497277c9339170a0420e86da8352e0c084624cd      1
    21 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    22 0x8560cbe177a4b28320e22012df9c908aabe24dcf      1
    23 0x8c42c9f78fc6a34308d582cb48bc2198350f58de      1
    24 0x9c7b82c0302c7c945f9cf45a5c73e6f48ab84b14      1
    25 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    26 0xa8174abc36b3298356d273e512cd87223a2b3ffa      1
    27 0xab6ca2017548a170699890214bfd66583a0c1754      1
    28 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    29 0xb722e190319010d6e412509b582a77dfd75a7858      1
    30 0xc734b794e323d0256695247c305ebc6964e57f60      1
    31 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    32 0xebfe1ab93d1122e065adcafd8c3174261e8e726f      1
    33 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    34 0xf0556eae3130c5d31cb4cd342f5b112c7bfc7416      1
    35 0xf0c3a1f1d9b79d5a8decb8e92973cdb56b7e81da      1
    36 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    37 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1

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
