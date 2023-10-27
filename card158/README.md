
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:126         Length:126         Min.   :1   Length:126        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:126        
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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Foundation","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("MakersPlaceEditions","JPGSPACE"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","Foundation","MakersPlace","MakersPlaceEditions","JPGSPACE"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 28 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     3 0x0a2dc66bde44a7a3401efc88f385387370716a3c      1
     4 0x234e228a33b516500388ffae36034fa5bd2f668b      1
     5 0x2bc4a480f8ad345b398abb23854e3062d5c57c51      1
     6 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     7 0x36a59b5cbfe6f593085cfac605500a598de8aa13      1
     8 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     9 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    10 0x54d7f921785ebe46010d83c73712e80dfaff1e81      1
    11 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    12 0x5bd2b53cb6b51b4bda9a23ef46b76a501ad12e63      1
    13 0x6b4b5c3f6a1e31b655c7903ee8501edbda737d90      1
    14 0x6d905b1a82afec95bf81de27223839304880f757      1
    15 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
    16 0x6eab20fba823777323112d772c30b3e9f0abedc5      1
    17 0x816730b2762e10d581c4b20aaf515f3f60baaf64      1
    18 0x82b8b659a4a98f69cb7899e1a07089ea3b90a894      1
    19 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    20 0x928efa31db4d805dc355945a5ab3c8c7a8db2fe2      1
    21 0x93958d6e3b79b6d15a11ba00876ce8edcb02ba95      1
    22 0x99c764441cd8f7554fd96e94924673821ab62c71      1
    23 0xa698cade532bf7491f0f887284f8e10e2de97198      1
    24 0xbd4d581a3912ca3afdde3aa515f22c1ab965df99      1
    25 0xdbc522931928f8ff7784c84e5fcec4ffdcd6e9eb      1
    26 0xe2ac25973b12979b3e7c31da82b0dea36d78374c      1
    27 0xf598aadf12a2fc4709b5db0c4c169715efaf2038      1
    28 0xf7ee6c2f811b52c72efd167a1bb3f4adaa1e0f89      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 52 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0dac0d611db5955bbe881cf8d75d4c80271cae83      1
     2 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     3 0x1c90d76468176a0caa4baf141677f253f73c83c2      1
     4 0x23c76086a4dbe50003fcb77172a3692a9e1082f0      1
     5 0x2c0593c26dfe61119df852978752873fda063b40      1
     6 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     7 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     8 0x3959b098941d0ca83b5031ce98c38a03e7a9f8f8      1
     9 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    10 0x3c27ca04199f8c72149131ef31e514dde80bc2d4      1
    11 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
    12 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    13 0x57d1e246d2e32f6f9d10ec55fc41e8b2e2988308      1
    14 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    15 0x60ce2c648e81d819c53d03bc6d7fa39ea5f40c6b      1
    16 0x699dd27a82e8e67d655c362a2e0d8a65f74ff918      1
    17 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    18 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
    19 0x7577c775655d9be6ec07406d9663cdbc90e441c3      1
    20 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    21 0x844fa4379f3c92f79b32f92574ed3694c2b7b49c      1
    22 0x863fc707e512ea089cd3e4aaf614d79b008ed882      1
    23 0x892857d3a0e6ca504da54470a0d71593525ebc22      1
    24 0x8bab29a0dad122b770f095f1e130ded4cec30a52      1
    25 0x8d897ff284c13dafced15d8ed8df275f34b9cd6b      1
    26 0x922ab4762d2b5760f87ef1d8347d85f38845fceb      1
    27 0x942c0685d660805cbeb5f5887f7df7917d4b9929      1
    28 0x964595fa0c054bcff59073fcefec471150b23572      1
    29 0x9788585f9bf84daf183cf17aa89e7a82b18fda0c      1
    30 0x99e2e69f98b164c399cc12d8382a82135eea6364      1
    31 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
    32 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    33 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
    34 0xae4c0384ce50297ba1f5251141e982fe7ad36829      1
    35 0xaeb8aabe5fef661d8091c27b4f18771dcea61a8c      1
    36 0xb1eea4d3a8978026650189883421085592616b02      1
    37 0xb439c92d4ecd9a20b3b4d73ea13431a2b2918146      1
    38 0xbe17f7d5800c9628075e1c4df3c3e147e9a280ee      1
    39 0xc23e1145356a9dd3aa6af8d7766281984ec24b76      1
    40 0xc5a54d16afe587c81cd70f1fa726c0d3e0c41092      1
    41 0xc75542e5e4c68e01ed6550e3c6b0c540c4be54b1      1
    42 0xcbd2f544073eb35b59a8eb19194a222a5361cd74      1
    43 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    44 0xd0c877b474cd51959931a7f70d7a6c60f50cdae7      1
    45 0xd3694e2a152bf65db6e35d8d1d0a13ace683b504      1
    46 0xda862691ab3f8fd3f28123b72146a571575e5e2a      1
    47 0xdab36a4ce12aa220e18c60ef0de489c232322a1a      1
    48 0xde270dd37103351cd45610539648ff9d0b32bb50      1
    49 0xe113e7b1e8ecd07973ea40978aed520241d17f27      1
    50 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    51 0xef5ab6248957a52cda981eff4f82c21d647fb082      1
    52 0xf421130974c355bb370345a5c082eb15e020b618      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 80 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     3 0x0a2dc66bde44a7a3401efc88f385387370716a3c      1
     4 0x0dac0d611db5955bbe881cf8d75d4c80271cae83      1
     5 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     6 0x1c90d76468176a0caa4baf141677f253f73c83c2      1
     7 0x234e228a33b516500388ffae36034fa5bd2f668b      1
     8 0x23c76086a4dbe50003fcb77172a3692a9e1082f0      1
     9 0x2bc4a480f8ad345b398abb23854e3062d5c57c51      1
    10 0x2c0593c26dfe61119df852978752873fda063b40      1
    11 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    12 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    13 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
    14 0x36a59b5cbfe6f593085cfac605500a598de8aa13      1
    15 0x3959b098941d0ca83b5031ce98c38a03e7a9f8f8      1
    16 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    17 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    18 0x3c27ca04199f8c72149131ef31e514dde80bc2d4      1
    19 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
    20 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    21 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    22 0x54d7f921785ebe46010d83c73712e80dfaff1e81      1
    23 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    24 0x57d1e246d2e32f6f9d10ec55fc41e8b2e2988308      1
    25 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    26 0x5bd2b53cb6b51b4bda9a23ef46b76a501ad12e63      1
    27 0x60ce2c648e81d819c53d03bc6d7fa39ea5f40c6b      1
    28 0x699dd27a82e8e67d655c362a2e0d8a65f74ff918      1
    29 0x6b4b5c3f6a1e31b655c7903ee8501edbda737d90      1
    30 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    31 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
    32 0x6d905b1a82afec95bf81de27223839304880f757      1
    33 0x6d9dd2b0fbf6e7d800f31c55f0f2f9089dfb7dc2      1
    34 0x6eab20fba823777323112d772c30b3e9f0abedc5      1
    35 0x7577c775655d9be6ec07406d9663cdbc90e441c3      1
    36 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    37 0x816730b2762e10d581c4b20aaf515f3f60baaf64      1
    38 0x82b8b659a4a98f69cb7899e1a07089ea3b90a894      1
    39 0x844fa4379f3c92f79b32f92574ed3694c2b7b49c      1
    40 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    41 0x863fc707e512ea089cd3e4aaf614d79b008ed882      1
    42 0x892857d3a0e6ca504da54470a0d71593525ebc22      1
    43 0x8bab29a0dad122b770f095f1e130ded4cec30a52      1
    44 0x8d897ff284c13dafced15d8ed8df275f34b9cd6b      1
    45 0x922ab4762d2b5760f87ef1d8347d85f38845fceb      1
    46 0x928efa31db4d805dc355945a5ab3c8c7a8db2fe2      1
    47 0x93958d6e3b79b6d15a11ba00876ce8edcb02ba95      1
    48 0x942c0685d660805cbeb5f5887f7df7917d4b9929      1
    49 0x964595fa0c054bcff59073fcefec471150b23572      1
    50 0x9788585f9bf84daf183cf17aa89e7a82b18fda0c      1
    51 0x99c764441cd8f7554fd96e94924673821ab62c71      1
    52 0x99e2e69f98b164c399cc12d8382a82135eea6364      1
    53 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
    54 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    55 0xa698cade532bf7491f0f887284f8e10e2de97198      1
    56 0xab0ca98528c1c7911cbfa8fb08abf5973f61e058      1
    57 0xae4c0384ce50297ba1f5251141e982fe7ad36829      1
    58 0xaeb8aabe5fef661d8091c27b4f18771dcea61a8c      1
    59 0xb1eea4d3a8978026650189883421085592616b02      1
    60 0xb439c92d4ecd9a20b3b4d73ea13431a2b2918146      1
    61 0xbd4d581a3912ca3afdde3aa515f22c1ab965df99      1
    62 0xbe17f7d5800c9628075e1c4df3c3e147e9a280ee      1
    63 0xc23e1145356a9dd3aa6af8d7766281984ec24b76      1
    64 0xc5a54d16afe587c81cd70f1fa726c0d3e0c41092      1
    65 0xc75542e5e4c68e01ed6550e3c6b0c540c4be54b1      1
    66 0xcbd2f544073eb35b59a8eb19194a222a5361cd74      1
    67 0xcf6f5a68d94165081a634ada997be3a93426c467      1
    68 0xd0c877b474cd51959931a7f70d7a6c60f50cdae7      1
    69 0xd3694e2a152bf65db6e35d8d1d0a13ace683b504      1
    70 0xda862691ab3f8fd3f28123b72146a571575e5e2a      1
    71 0xdab36a4ce12aa220e18c60ef0de489c232322a1a      1
    72 0xdbc522931928f8ff7784c84e5fcec4ffdcd6e9eb      1
    73 0xde270dd37103351cd45610539648ff9d0b32bb50      1
    74 0xe113e7b1e8ecd07973ea40978aed520241d17f27      1
    75 0xe2ac25973b12979b3e7c31da82b0dea36d78374c      1
    76 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    77 0xef5ab6248957a52cda981eff4f82c21d647fb082      1
    78 0xf421130974c355bb370345a5c082eb15e020b618      1
    79 0xf598aadf12a2fc4709b5db0c4c169715efaf2038      1
    80 0xf7ee6c2f811b52c72efd167a1bb3f4adaa1e0f89      1

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
