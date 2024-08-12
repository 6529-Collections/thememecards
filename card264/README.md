
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:95          Length:95          Min.   :1   Length:95         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:95         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20495569 # https://etherscan.io/block/20495569
block_hash <- "0x61ae406d56b85b0fc03017833c0979e04df2445a6a251fefbd6347aaabe583b5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4635 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","KnownOrigin2","Foundation","WEARMEAT","IZZZI","EMOART","CollaborativePartnerships","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("izzzikiCollectionEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","KnownOrigin2","Foundation","WEARMEAT","IZZZI","EMOART","CollaborativePartnerships","MakersPlace","izzzikiCollectionEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 19 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x13c3fe624559a20e2d06702259b5e3a3990936be      1
     2 0x1610634c1fcda56b9934d67f885b30dc73f938cb      1
     3 0x1c458a942a386ca40e7cd1c1401df43b67785f7b      1
     4 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     5 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     6 0x34a978beab5b673c1e4d193f6b42d2c48ba2ba8b      1
     7 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     8 0x76caccc17c1aec94f3fd429c5edf5ae23ff11dd4      1
     9 0x7b4569b0429fac8312adcb0bdda606e9beb1acf8      1
    10 0x7ba3e8f17582462676c44dd143388ed4b6b20655      1
    11 0x92a7bd65c8b2a9c9d98be8eaa92de46d1fbdefaf      1
    12 0x989cfebbb7bfbb47427b368b032fc7d2ee78bd66      1
    13 0x9e283900715b62f8923b14fc2cb64088e2d5bd5d      1
    14 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    15 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    16 0xdff6b88d0372d71288103f3ac0a91a211a413794      1
    17 0xe4cd1de1a029f6edd3ae5f7ddc29e118cec2ee6a      1
    18 0xfa1bd73b3ce55cbebe92b837aff62b992e1db211      1
    19 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03eb93c963fd19932f7ed70177f001235eff637c      1
     2 0x050532e8fb2ef90c8cdbabdb3f8c7cbb3593e1e9      1
     3 0x0be1b9d26588bf2b082bd756910cdc89ff218671      1
     4 0x1037eff242bd3181beb54e0521b3edfeb0b20c50      1
     5 0x1427cc00080f17dd10aa59e629ea3e2de14608c0      1
     6 0x17410ac8e1e7c0296d3d1ee82c2bce9cf5250a5c      1
     7 0x258eb720341602ab2afb6780d5da29a0aa7493aa      1
     8 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     9 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
    10 0x36d479e3abbd5ab72364f1f88a8624da758dd935      1
    11 0x3859da187e4071a932070b31d97d5838c5441de5      1
    12 0x39ac5bb0ac8f547d9dabd402059754b089774291      1
    13 0x3c6deaf5c95717ed5442c9836dfc62c1e5e83164      1
    14 0x3e0de6286372a437340d32aad4a039be2dccb017      1
    15 0x41120313c3a305ac7d173efc89bd8a0fc850f843      1
    16 0x46ce3e3587feee846d5c22c3c12dfaf3625a7caf      1
    17 0x56b2ae2b6c42e68d2cbea13cf456f7769a15678f      1
    18 0x5982e934c9863ed0aff470ec053c5e05b82f663d      1
    19 0x607ec4eb37c210f4f94542482fa1c64bb6cb9dd0      1
    20 0x75ca0ef1f46a6fc7aad6b19d3cdc045850f29195      1
    21 0x79f4053c3bcf5851dd9f8e5beef2eb3eebaa5f33      1
    22 0x7f74b71b5c3a200d9ff327d1f6f8fb695393ec5c      1
    23 0x83890cf7c61ebaf717de7c7cdd0c1abf85abd559      1
    24 0x852c03de41993cf180bb8bba83e232a99e2fe635      1
    25 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    26 0x91b52e9833dc4c681b5419f3e96093e85bbe8f91      1
    27 0x92c5adf5a4f709b55bb67150f3af69b3419647ac      1
    28 0xa88235065d97a56719ea7d4fe72f8f953c984c0b      1
    29 0xab1db92824f95e4438b6f28acf0a65fb91e1743d      1
    30 0xae20f0454cc764619489087fb7ef4c3e890062a0      1
    31 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    32 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    33 0xc2ecfcaf014d89cd015f73a080a986387eb9457c      1
    34 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    35 0xcd89b56b28914e694bc254e541579daa4ca4d91d      1
    36 0xd21a40166324f30bd584069887906082184fdbee      1
    37 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    38 0xe3061dc4116f689e1314d5bd40aa5d37861a13e0      1
    39 0xea2ddc5fce9fa081c929b52079d4342caa5224a9      1
    40 0xee119e2fea524c93aea9962de5308070682801b1      1
    41 0xf674099c77321b5d8371fdd2d6b429df0bbefd1b      1
    42 0xf7c9282a18169b6ae7ae8ec369266d0c7af55364      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 61 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03eb93c963fd19932f7ed70177f001235eff637c      1
     2 0x050532e8fb2ef90c8cdbabdb3f8c7cbb3593e1e9      1
     3 0x0be1b9d26588bf2b082bd756910cdc89ff218671      1
     4 0x1037eff242bd3181beb54e0521b3edfeb0b20c50      1
     5 0x13c3fe624559a20e2d06702259b5e3a3990936be      1
     6 0x1427cc00080f17dd10aa59e629ea3e2de14608c0      1
     7 0x1610634c1fcda56b9934d67f885b30dc73f938cb      1
     8 0x17410ac8e1e7c0296d3d1ee82c2bce9cf5250a5c      1
     9 0x1c458a942a386ca40e7cd1c1401df43b67785f7b      1
    10 0x258eb720341602ab2afb6780d5da29a0aa7493aa      1
    11 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
    12 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
    13 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
    14 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    15 0x34a978beab5b673c1e4d193f6b42d2c48ba2ba8b      1
    16 0x36d479e3abbd5ab72364f1f88a8624da758dd935      1
    17 0x3859da187e4071a932070b31d97d5838c5441de5      1
    18 0x39ac5bb0ac8f547d9dabd402059754b089774291      1
    19 0x3c6deaf5c95717ed5442c9836dfc62c1e5e83164      1
    20 0x3e0de6286372a437340d32aad4a039be2dccb017      1
    21 0x41120313c3a305ac7d173efc89bd8a0fc850f843      1
    22 0x46ce3e3587feee846d5c22c3c12dfaf3625a7caf      1
    23 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    24 0x56b2ae2b6c42e68d2cbea13cf456f7769a15678f      1
    25 0x5982e934c9863ed0aff470ec053c5e05b82f663d      1
    26 0x607ec4eb37c210f4f94542482fa1c64bb6cb9dd0      1
    27 0x75ca0ef1f46a6fc7aad6b19d3cdc045850f29195      1
    28 0x76caccc17c1aec94f3fd429c5edf5ae23ff11dd4      1
    29 0x79f4053c3bcf5851dd9f8e5beef2eb3eebaa5f33      1
    30 0x7b4569b0429fac8312adcb0bdda606e9beb1acf8      1
    31 0x7ba3e8f17582462676c44dd143388ed4b6b20655      1
    32 0x7f74b71b5c3a200d9ff327d1f6f8fb695393ec5c      1
    33 0x83890cf7c61ebaf717de7c7cdd0c1abf85abd559      1
    34 0x852c03de41993cf180bb8bba83e232a99e2fe635      1
    35 0x86a35e57968f0d6591b1886594c6b7721fc96943      1
    36 0x91b52e9833dc4c681b5419f3e96093e85bbe8f91      1
    37 0x92a7bd65c8b2a9c9d98be8eaa92de46d1fbdefaf      1
    38 0x92c5adf5a4f709b55bb67150f3af69b3419647ac      1
    39 0x989cfebbb7bfbb47427b368b032fc7d2ee78bd66      1
    40 0x9e283900715b62f8923b14fc2cb64088e2d5bd5d      1
    41 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    42 0xa88235065d97a56719ea7d4fe72f8f953c984c0b      1
    43 0xab1db92824f95e4438b6f28acf0a65fb91e1743d      1
    44 0xae20f0454cc764619489087fb7ef4c3e890062a0      1
    45 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    46 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    47 0xc2ecfcaf014d89cd015f73a080a986387eb9457c      1
    48 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    49 0xcd89b56b28914e694bc254e541579daa4ca4d91d      1
    50 0xd21a40166324f30bd584069887906082184fdbee      1
    51 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    52 0xdff6b88d0372d71288103f3ac0a91a211a413794      1
    53 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    54 0xe3061dc4116f689e1314d5bd40aa5d37861a13e0      1
    55 0xe4cd1de1a029f6edd3ae5f7ddc29e118cec2ee6a      1
    56 0xea2ddc5fce9fa081c929b52079d4342caa5224a9      1
    57 0xee119e2fea524c93aea9962de5308070682801b1      1
    58 0xf674099c77321b5d8371fdd2d6b429df0bbefd1b      1
    59 0xf7c9282a18169b6ae7ae8ec369266d0c7af55364      1
    60 0xfa1bd73b3ce55cbebe92b837aff62b992e1db211      1
    61 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

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
