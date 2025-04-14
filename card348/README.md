
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:78          Length:78          Min.   :1   Length:78         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:78         
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



allow_artist_all <- pick(snapshot, contracts=c("TranquilReboot","DIGITIZED","Glitched","Overhyped","GINGERX","ANIMATIONS","ILLUSTRATIONS21","THEWATCHER21","Illustrations"), address_remove=address_remove,address_max=1)
```

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
     1 0x0d6d94bdf57798c9020246c91990f785ead32361      1
     2 0x0f44ba79089d8e6a089575187d7e1d9a81e71a05      1
     3 0x1d5908e2626e2f0cf1dace3abd439b8a5006a734      1
     4 0x1d81a65936491a54323e862026ebc11e24e725d2      1
     5 0x1e2e044acfe5fd733e756aff97ce4eee65dd5f99      1
     6 0x1ffdccfc0dadf7c27a679aa740132af837505e2b      1
     7 0x23fea296223e93ea843e229b3d4acc97e77ac7da      1
     8 0x320abb862a2d2f5e1a7b5325103786f6d11dd0be      1
     9 0x339540f676fc2b5026d9f20ef9cc526b966dbe2d      1
    10 0x3522f1a4e3694462b8f27748332768a89fa668f5      1
    11 0x3b9ffe03ece0938d4f8ddea48fd5a610f9df32ff      1
    12 0x3c29e2be0f55418dd146e6b1234448c9b8b00386      1
    13 0x3db92e26837328c1300e3f818c5da7fc9a7d9648      1
    14 0x3ecf068b735c6b5cc7597e72ceced9bded196b82      1
    15 0x4806a418588d22de0c6a4f7f00d4bf43b2b61dd4      1
    16 0x49317b33a82fb576fba1462364d02569e766e33a      1
    17 0x4fd9328511776b0a182b402d244a395e1e1841fb      1
    18 0x54da68b4bf387e6816c1a111c51035ffe6d0f7f4      1
    19 0x56fbc0997f5c9ccd10ee513e0c5e7bd2eeeb1db9      1
    20 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    21 0x65b18fb1948ab3c31d21014a0dac36c07b4247d1      1
    22 0x67deaf99a925e5b62a50eadbad290bb847449eaa      1
    23 0x69078dafdbbe55fdf89e50b874d54b91e53fc8ef      1
    24 0x698b7dcb16b624f104b37a2578acaede89f37fc9      1
    25 0x6f66b95a0c512f3497fb46660e0bc3b94b989f8d      1
    26 0x700e3990defb8a2efa87a7a9165c5dec57f69576      1
    27 0x71a04638a6248a888365bdd7860fb4d05da040e4      1
    28 0x88452be7bc0565e7953d6eba1ce16f0d0a35cd4c      1
    29 0x89378baa5d736b4fadb5a6c3ec030196ee8c0d2f      1
    30 0x951038bb372d16180c0afb0f46ab283059154004      1
    31 0x9812d49d733cd79a1269bfc689dff681106349fa      1
    32 0x98b922021bb2aa87697e12030d60a1cc8b57c975      1
    33 0x9a497fcc397c429217e71860c753a2a96c79b555      1
    34 0x9f9a98ca647915c4546d364b5f02e5549dbc69f6      1
    35 0xa13cf82dff85c7745c83acdfb09d90181a0b8fb1      1
    36 0xaa6fd0c3dd82e34b1a0ce7a526235336442fd678      1
    37 0xab7473da0af4658ae2879cb641d82e7652721486      1
    38 0xae48f64abeaa92618b8180c0fd33caebfed42f2b      1
    39 0xb3516a72374ef4813977da92e3a5c174fb6f24fc      1
    40 0xb3b986b8c95c522d6c7e2a5c804b89837a05a161      1
    41 0xb4532c764c26dab6d69b5b62a201fd624ca9a6ba      1
    42 0xb6aaa9ba6065bd515e3a7d9fca61e9072799b7d5      1
    43 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    44 0xc3b615216362aa20384d74b0deb082c9a6f1ec20      1
    45 0xc4f94270364674f3bcce1487d98a959e72427dba      1
    46 0xc7502c148b7cf1f9fe9e7a255a77f9c1fd7dccaf      1
    47 0xc9ac5fedff6825a70c9adab5478d83be822e1fe2      1
    48 0xd290a4aec18dc12982b24d2918cf238a09b81cf3      1
    49 0xd854f27ab70baadfb154315b2a4dd340a9db0900      1
    50 0xe5fb59d05213a3d181ca18e4e5f9bf0cd2674941      1
    51 0xe7a4b75f1cf09dac63062f20c4d8e7684d954319      1
    52 0xedcf5d6ddc5c8b64dd6927cab50a5b7fb3e50abd      1
    53 0xf1a26d0d9355aea843544d941586b6bd8ded7732      1
    54 0xf46b076ff8ac8ae163fcd6d9a6e076903845cd09      1
    55 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    56 0xf96ff41ce50d4ad337adb07e32431dff9a97bb2f      1
    57 0xfc0bd27fc12d5c71adfa5320ffe7e383891e85a7      1

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
