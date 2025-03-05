
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:89          Length:89          Min.   :1.000   Length:89         
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.056                     
                                           3rd Qu.:1.000                     
                                           Max.   :6.000                     
         name          
     Length:89         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21950469 # https://etherscan.io/block/21950469
block_hash <- "0x6c02da31b26b9cac495ac7957a519675b6a894d5a9c8d775a73847496acc0f7e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4633 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","BelieveInFilm","LEICA","Cannes","PierPressure"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("BelieveInFilmEditions","ManhattanProjectEditions","mfersinEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","BelieveInFilm","LEICA","Cannes","PierPressure","BelieveInFilmEditions","ManhattanProjectEditions","mfersinEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x09ab0b019fd9c44316678b5a62ced52d179b0c1a      1
     2 0x0a74a2bebc6ff38ba93c603817802a940933ab58      1
     3 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     4 0x26e3cf189460f6bfe3d255354861bcc6dfd18149      1
     5 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     6 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     7 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
     8 0x9cef0be42a0ac827950110e4f87885cdb3becc76      1
     9 0xad37805e0a724e2c334acf49d255a0a35b76fe7c      1
    10 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    11 0xd21309b3885181d94ebf2561a2e6ac6fd8fb300d      1
    12 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    13 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 44 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03d1a11f5e98a02ecd31962dc787000451e0c1d2      1
     2 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     3 0x164c3df08784171e0c4b61b7682263371f1eeb74      1
     4 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
     5 0x31c477dadd0cf1ce949c1926841e9ce2f3b5742d      1
     6 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     7 0x35f2383cfec184f9edb1c787f865b0d3ba46367d      1
     8 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     9 0x65de6475258e3736e5c7d502d2c0a45710c9ec37      1
    10 0x66533dd5cde19b4a79b4d394f6272dc4612b1abc      1
    11 0x6785bdf80569e9bcb249083bfe95304640996cff      1
    12 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    13 0x70708ae84455018b45b6f3ef24ef0f761331072f      1
    14 0x771699e05bb2e147df732638d052b044dc16eeb6      1
    15 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    16 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    17 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    18 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
    19 0x91672ef7ea2a548d1b5616d9a681168dba9237ac      1
    20 0x96c3bb1c9da186c87a102f39a750745ff0da843a      1
    21 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    22 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    23 0x9df947cd8a9810964473339c38a95201ecbe112d      1
    24 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    25 0xa1cdee77355b0bc144be1272978453e82d2fa10d      1
    26 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    27 0xa8d67e13ac97cba918decdcd78f71fca8ab2d1a8      1
    28 0xab495668377136f794ddbcff55825dad7c35abda      1
    29 0xbd330355e09d28881bf26efaa0841f021350456b      1
    30 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    31 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    32 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    33 0xc8123bae791f267f953e09d2eb50284514370b11      1
    34 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    35 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    36 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    37 0xd94897f5dfb4f54545f42661c76da53537aab339      1
    38 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    39 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    40 0xe8ff464b954d12db575ad0e5e5a7dc9c041ee6d6      1
    41 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    42 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    43 0xf95752fd023fd8802abdd9cbe8e9965f623f8a84      1
    44 0xfded90a3b1348425577688866f798f94d77a0d02      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 57 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03d1a11f5e98a02ecd31962dc787000451e0c1d2      1
     2 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     3 0x09ab0b019fd9c44316678b5a62ced52d179b0c1a      1
     4 0x0a74a2bebc6ff38ba93c603817802a940933ab58      1
     5 0x164c3df08784171e0c4b61b7682263371f1eeb74      1
     6 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
     7 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     8 0x26e3cf189460f6bfe3d255354861bcc6dfd18149      1
     9 0x31c477dadd0cf1ce949c1926841e9ce2f3b5742d      1
    10 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
    11 0x35f2383cfec184f9edb1c787f865b0d3ba46367d      1
    12 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    13 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
    14 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    15 0x65de6475258e3736e5c7d502d2c0a45710c9ec37      1
    16 0x66533dd5cde19b4a79b4d394f6272dc4612b1abc      1
    17 0x6785bdf80569e9bcb249083bfe95304640996cff      1
    18 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    19 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    20 0x70708ae84455018b45b6f3ef24ef0f761331072f      1
    21 0x771699e05bb2e147df732638d052b044dc16eeb6      1
    22 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    23 0x8030e495ea55d8ec8ae1e10bf7498bf7f0eac1aa      1
    24 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    25 0x8397e2d5fadaa6e0817e376df5ba1d016a2f804f      1
    26 0x91672ef7ea2a548d1b5616d9a681168dba9237ac      1
    27 0x96c3bb1c9da186c87a102f39a750745ff0da843a      1
    28 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
    29 0x9b7c0814eb341e6a4ca673148e8b577b13e70787      1
    30 0x9cef0be42a0ac827950110e4f87885cdb3becc76      1
    31 0x9df947cd8a9810964473339c38a95201ecbe112d      1
    32 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    33 0xa1cdee77355b0bc144be1272978453e82d2fa10d      1
    34 0xa32c38646299818ccedc6401818c2e1639c39c08      1
    35 0xa8d67e13ac97cba918decdcd78f71fca8ab2d1a8      1
    36 0xab495668377136f794ddbcff55825dad7c35abda      1
    37 0xad37805e0a724e2c334acf49d255a0a35b76fe7c      1
    38 0xbd330355e09d28881bf26efaa0841f021350456b      1
    39 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    40 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    41 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    42 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    43 0xc8123bae791f267f953e09d2eb50284514370b11      1
    44 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    45 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    46 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    47 0xd21309b3885181d94ebf2561a2e6ac6fd8fb300d      1
    48 0xd94897f5dfb4f54545f42661c76da53537aab339      1
    49 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    50 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    51 0xe8ff464b954d12db575ad0e5e5a7dc9c041ee6d6      1
    52 0xf1d83de0ead95747f4ac184b36659c0d538309c8      1
    53 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    54 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    55 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    56 0xf95752fd023fd8802abdd9cbe8e9965f623f8a84      1
    57 0xfded90a3b1348425577688866f798f94d77a0d02      1

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
