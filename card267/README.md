
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
                                           Mean   :1.036                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:56         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20546969 # https://etherscan.io/block/20546969
block_hash <- "0x870945634be441ce9f43755abe61cb20906a2960d6c26e49130fea9e8480ced6"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4519 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","ArtCity","Abstractmovie","Infinity","Myworld3","MakersPlace","RainbowMood","Geometriccollection"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DancingtogetherEditions","Editions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","ArtCity","Abstractmovie","Infinity","Myworld3","MakersPlace","RainbowMood","Geometriccollection","DancingtogetherEditions","Editions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 29 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x004c1e5be68337c33deff01e23de067c234bf874      1
     2 0x00ff192363430a35abbf968c535b64147e88abdb      1
     3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     4 0x18bcbab94d57a3b8e0935289c8f98b2c23bcd5c6      1
     5 0x195fce941ae0c972b8dc814d2059537b67f14038      1
     6 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     7 0x21301d901db04724597d1b6012ac49878157580d      1
     8 0x2b891ee897b87a496ee68d63d85f8d8bae5f43ad      1
     9 0x3deed956b999b83361b85bff31d388c35125411d      1
    10 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
    11 0x43cc80b0d94c86c0c1184d1a0eec42fbe51a00a9      1
    12 0x4f88c947e1c254cf0f56da819e024779fa202787      1
    13 0x50cde770461ef53b62e083313d64b5b274b4bb78      1
    14 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
    15 0x7cb2c57ef6716ab9ffd68811fb1ab258570a6704      1
    16 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    17 0xa175029bff19b26b4a2e6da68e8bb909d6005fec      1
    18 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    19 0xae48f64abeaa92618b8180c0fd33caebfed42f2b      1
    20 0xb43eebac012cb2f12e1ec258a6ece20a7aa4712f      1
    21 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    22 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    23 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    24 0xdaf288eea1a696ecb1dd37a1e6b2a4058a604e7c      1
    25 0xe5edd7fd0e6f7bafa001567a359bfa2879cb72d3      1
    26 0xe8c4156a19951deff3203f116fc63274da746baa      1
    27 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    28 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    29 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
    2 0x2993d0b5a4cdeed133565d1a09f0ad36be23f632      1
    3 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
    4 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    5 0x9ba9160e463f974444397e8f20a2470052f58823      1
    6 0xa79babfb8c58b113d80a72cf8c9cc90f5ef52424      1
    7 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    8 0xdb3652f22b5faf7798cdfdee2850babd841d4b46      1
    9 0xea9b13b35239482bf2bc05d7cbadf5ec47a0085f      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 38 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x004c1e5be68337c33deff01e23de067c234bf874      1
     2 0x00ff192363430a35abbf968c535b64147e88abdb      1
     3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     4 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
     5 0x18bcbab94d57a3b8e0935289c8f98b2c23bcd5c6      1
     6 0x195fce941ae0c972b8dc814d2059537b67f14038      1
     7 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     8 0x21301d901db04724597d1b6012ac49878157580d      1
     9 0x2993d0b5a4cdeed133565d1a09f0ad36be23f632      1
    10 0x2b891ee897b87a496ee68d63d85f8d8bae5f43ad      1
    11 0x3deed956b999b83361b85bff31d388c35125411d      1
    12 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
    13 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
    14 0x43cc80b0d94c86c0c1184d1a0eec42fbe51a00a9      1
    15 0x4f88c947e1c254cf0f56da819e024779fa202787      1
    16 0x50cde770461ef53b62e083313d64b5b274b4bb78      1
    17 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    18 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
    19 0x7cb2c57ef6716ab9ffd68811fb1ab258570a6704      1
    20 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    21 0x9ba9160e463f974444397e8f20a2470052f58823      1
    22 0xa175029bff19b26b4a2e6da68e8bb909d6005fec      1
    23 0xa79babfb8c58b113d80a72cf8c9cc90f5ef52424      1
    24 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    25 0xae48f64abeaa92618b8180c0fd33caebfed42f2b      1
    26 0xb43eebac012cb2f12e1ec258a6ece20a7aa4712f      1
    27 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    28 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    29 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    30 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    31 0xdaf288eea1a696ecb1dd37a1e6b2a4058a604e7c      1
    32 0xdb3652f22b5faf7798cdfdee2850babd841d4b46      1
    33 0xe5edd7fd0e6f7bafa001567a359bfa2879cb72d3      1
    34 0xe8c4156a19951deff3203f116fc63274da746baa      1
    35 0xea9b13b35239482bf2bc05d7cbadf5ec47a0085f      1
    36 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    37 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    38 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1

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
