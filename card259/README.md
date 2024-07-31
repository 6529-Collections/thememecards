
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:138         Length:138         Min.   :1   Length:138        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:138        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20394269 # https://etherscan.io/block/20394269
block_hash <- "0xbc21836273c388066b3789e4cb91d9790a0d11ae3e01c6924a66fadcc317ddd4"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4600 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Intertwine","TheRemedyVolIII","Symphony","REMINISCE","Dialed","Sketchbook","Flow","TheRemedyVolII","JuicexJoeMediums","REBORN","MERGED","TheRemedy"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("JUICEDUPPEditions","JuiceBrunsxTinyAnthemEditions"," DBKEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Intertwine","TheRemedyVolIII","Symphony","REMINISCE","Dialed","Sketchbook","Flow","TheRemedyVolII","JuicexJoeMediums","REBORN","MERGED","TheRemedy","JUICEDUPPEditions","JuiceBrunsxTinyAnthemEditions"," DBKEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 30 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
     3 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     4 0x1a1f337faa595a3603b4a3e67776150e7883954d      1
     5 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
     6 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     7 0x2063c384e0cc27d32375fad27111d026e86e111e      1
     8 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
     9 0x377fb14e0823020e9bda6c21ad9cb8d98fd40191      1
    10 0x3deed956b999b83361b85bff31d388c35125411d      1
    11 0x4abb43963ceb40cb44bc0ee79318539986d1c5a9      1
    12 0x4e323ee346748d7a11c020a38e4feec377779533      1
    13 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    14 0x59ff9ef6e31ca54ba033aa75f39e9cd2f3f22305      1
    15 0x5cbef4dce6ed8b75cea9637df4c4b946705a0171      1
    16 0x5ccaf5a246051c35816ccaa241012dfc0e56b812      1
    17 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    18 0x986ea1305f40d64864dc6e338715b20dafd1f819      1
    19 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    20 0xa9878b7310af525355ef79447c06b0b01f171240      1
    21 0xab7473da0af4658ae2879cb641d82e7652721486      1
    22 0xb46d0e74676886d00522692bd1d9eb5a56207659      1
    23 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    24 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    25 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    26 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    27 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    28 0xe3f663418251186888935dc1c4979fa3a3da1bac      1
    29 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    30 0xf1fcffba10de3cd282baf4b6e49393ba03dd3be7      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 38 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
     2 0x0c96e77528ab52422495a5474697e7630504c739      1
     3 0x1602c6f0228761fd380b4e4cb6d7aa04c5683762      1
     4 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     5 0x20345c3a4afa7a35f917068dd3def812c367f051      1
     6 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     7 0x2894f4fe2c4dfc83b20445cf1511d9892fc7ba73      1
     8 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
     9 0x42b46951ae492fde820016ad6432c1360a227dd1      1
    10 0x4e7e1c73c116649c1c684acb6ec98bac4fbb4ef6      1
    11 0x52743358514133ea9932e7ba453e8f732f8c080a      1
    12 0x58c7f158b78e896c94c0c9787c8703612e6c496f      1
    13 0x5959002cb524181d5526714c2804c3775212d823      1
    14 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    15 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    16 0x6359a5b7839da788045c456217e2f9eda4e8848b      1
    17 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
    18 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    19 0x6b6cf175b3dc7a732a1cea4a3eeccafee8d2dc85      1
    20 0x736353be76d2419496b0a4869985dffb0fe1173a      1
    21 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    22 0x7ea8cef1e44cf4c7e194ac8e7ac4d00cb0d97a6d      1
    23 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    24 0x855ce7c2a55bc1c0e206d992bc68f90892994387      1
    25 0x897996600a1177dac84899e3d8d3f04d1b7db82e      1
    26 0x8b0fa4148021fa96d2fa5ee760068eb6dcfb2634      1
    27 0x90c41e17c70340a5999c250554d3bff9ba063bc4      1
    28 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    29 0x97e7124bbce0baf75a7c4688b98a0c1ddfafb3f5      1
    30 0x9b25142253717e120cb77868b8a805d56ab62369      1
    31 0x9c802062173b02a486086bf693186353334aedc6      1
    32 0xb6cf777e3696a502107417265c92d1b075636a10      1
    33 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    34 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    35 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    36 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    37 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    38 0xfed473cd045563542414e31af846a43bf58fc59a      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x011a6f601ac7307a8e091e7d62b14b00802af217      1
     3 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
     4 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     5 0x0c96e77528ab52422495a5474697e7630504c739      1
     6 0x1602c6f0228761fd380b4e4cb6d7aa04c5683762      1
     7 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     8 0x1a1f337faa595a3603b4a3e67776150e7883954d      1
     9 0x1b052edc4df735f0a5e62c1335c94667e7fdc55e      1
    10 0x1d25029d92a7051f46a13be9a512443c966d3542      1
    11 0x20345c3a4afa7a35f917068dd3def812c367f051      1
    12 0x2063c384e0cc27d32375fad27111d026e86e111e      1
    13 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    14 0x2894f4fe2c4dfc83b20445cf1511d9892fc7ba73      1
    15 0x37009f9ae9dcc0a6d59204e1aeaf9da4968caec8      1
    16 0x377fb14e0823020e9bda6c21ad9cb8d98fd40191      1
    17 0x3deed956b999b83361b85bff31d388c35125411d      1
    18 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
    19 0x42b46951ae492fde820016ad6432c1360a227dd1      1
    20 0x4abb43963ceb40cb44bc0ee79318539986d1c5a9      1
    21 0x4e323ee346748d7a11c020a38e4feec377779533      1
    22 0x4e7e1c73c116649c1c684acb6ec98bac4fbb4ef6      1
    23 0x52743358514133ea9932e7ba453e8f732f8c080a      1
    24 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    25 0x58c7f158b78e896c94c0c9787c8703612e6c496f      1
    26 0x5959002cb524181d5526714c2804c3775212d823      1
    27 0x59ff9ef6e31ca54ba033aa75f39e9cd2f3f22305      1
    28 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    29 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    30 0x5cbef4dce6ed8b75cea9637df4c4b946705a0171      1
    31 0x5ccaf5a246051c35816ccaa241012dfc0e56b812      1
    32 0x6359a5b7839da788045c456217e2f9eda4e8848b      1
    33 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
    34 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    35 0x6b6cf175b3dc7a732a1cea4a3eeccafee8d2dc85      1
    36 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    37 0x736353be76d2419496b0a4869985dffb0fe1173a      1
    38 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    39 0x7ea8cef1e44cf4c7e194ac8e7ac4d00cb0d97a6d      1
    40 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    41 0x855ce7c2a55bc1c0e206d992bc68f90892994387      1
    42 0x897996600a1177dac84899e3d8d3f04d1b7db82e      1
    43 0x8b0fa4148021fa96d2fa5ee760068eb6dcfb2634      1
    44 0x90c41e17c70340a5999c250554d3bff9ba063bc4      1
    45 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    46 0x97e7124bbce0baf75a7c4688b98a0c1ddfafb3f5      1
    47 0x986ea1305f40d64864dc6e338715b20dafd1f819      1
    48 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    49 0x9b25142253717e120cb77868b8a805d56ab62369      1
    50 0x9c802062173b02a486086bf693186353334aedc6      1
    51 0xa9878b7310af525355ef79447c06b0b01f171240      1
    52 0xab7473da0af4658ae2879cb641d82e7652721486      1
    53 0xb46d0e74676886d00522692bd1d9eb5a56207659      1
    54 0xb6cf777e3696a502107417265c92d1b075636a10      1
    55 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    56 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    57 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    58 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    59 0xd3c69ec93a9841796a2f41b342aa1d1f2a4f508c      1
    60 0xd9550ece0d90a8e08b323283e656eb11be69560d      1
    61 0xe3f663418251186888935dc1c4979fa3a3da1bac      1
    62 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    63 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    64 0xf1fcffba10de3cd282baf4b6e49393ba03dd3be7      1
    65 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    66 0xf74aa398050f15c7c58750ce71cf0be5440ba47a      1
    67 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    68 0xfed473cd045563542414e31af846a43bf58fc59a      1

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
