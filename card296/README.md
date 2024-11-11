
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:124         Length:124         Min.   :1.000   Length:124        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.024                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:124        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21146969 # https://etherscan.io/block/21146969
block_hash <- "0x8b2ddc2b9e19d97c1fb918554c644b8258f741aba80d271def6d6b4aa9d91cac"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4854 

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

allow_artist1    <- pick(snapshot, contracts=c("AYearInColour","KnownOrigin","KnownOrigin2","Meditations2023","Exhibitions","NeoAbstraction","PhygitalCollection","FULLCIRCLE","Rarible","SensoryPerceptionExploration","PortraitsinSound"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("1958Editions","AYearInColourCommunityEditions","PORTALSEditions","AEONOE23Editions","PulseoftheUniverseEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("AYearInColour","KnownOrigin","KnownOrigin2","Meditations2023","Exhibitions","NeoAbstraction","PhygitalCollection","FULLCIRCLE","Rarible","SensoryPerceptionExploration","PortraitsinSound","1958Editions","AYearInColourCommunityEditions","PORTALSEditions","AEONOE23Editions","PulseoftheUniverseEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 51 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x13ddf16f41f9a634eaca69be176773fe5c629c1e      1
     2 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     3 0x16f41b1b8620b024f8fbbb843fe5aae49cc9a207      1
     4 0x27e41b99b8c32a39474ab554dd000e19561afa2e      1
     5 0x3925e567b3ed59d1d5444ac902f96fb949c5fcc2      1
     6 0x3f75a3ca179d2c144539917024aa911abbb7ad9c      1
     7 0x42ed6eaefaa6d717743dfd61299e467692cfea68      1
     8 0x4518344525d32415f3ebda186bdb2c375d9443d6      1
     9 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
    10 0x55aa430141908bd51749ddabfeb8a0e11e9544e9      1
    11 0x56b7b5f84eb16430c4de187dac21d19559287107      1
    12 0x5948428d0711442f6ae3c8d06e3a9895f33cecd2      1
    13 0x59c3d094c6ec333e034b87c06687ec09523ad6a4      1
    14 0x5d0905b6b670c1409eda5cc3414737c0c360ae2a      1
    15 0x601f82bbc160b6e51d4e16d342366d688d7e1ea3      1
    16 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    17 0x7e4fa6a2f352661933ffcfc3ec33d3b667c5da0b      1
    18 0x8592c855f55e9c233581ac7411a1db4efa08ad03      1
    19 0x895d49ea015b1e9da4af776239d0f8b029d660b2      1
    20 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
    21 0x95aa4c96fff08ed0514a84e94a08db2f12b0ba85      1
    22 0x99986b5e808fd039b992fad30075f517979d490c      1
    23 0x9d985b8d2e0135f2d355a96154ae975c8373d39a      1
    24 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    25 0xa4a6ac22e5af1fb902bf1edfa2b044f48470d27f      1
    26 0xab557245f9de18a7a2b59e8bc4f361dd83d16807      1
    27 0xab6ca2017548a170699890214bfd66583a0c1754      1
    28 0xb1af684799c96f5c740786be59dc834ff8ef5add      1
    29 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    30 0xb74edfde0752ea5ce2352fb404617c42f945aaf6      1
    31 0xbc8492a5815218920b0c28fd558d427d77c91ab2      1
    32 0xbf80137831a4739314bf28111e68884b6bee7beb      1
    33 0xc2587a1685b194831c0e4a8927df0c33437fca3f      1
    34 0xcb1dd0f59fdf86658016827c78b858dfafa39ccc      1
    35 0xcf8488afeb807facf3cc683b0bf9d08e323c229d      1
    36 0xd0d521cf3c2125b35c9a74fdb3b839214b1491a7      1
    37 0xd14a49a9689459f932b0d29b63bf39fff8268771      1
    38 0xd26632a9403f0cc471be1916a843e65192e59555      1
    39 0xd40b35ac45adc2bf7083252c4a36f4058d6ec8c5      1
    40 0xd5917a0a13bbc615c5ad4553266fd9373b5c7283      1
    41 0xdd7784c63bd8b967ae3edeaa11f5847c3d2c1001      1
    42 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    43 0xe27dc3aa4352244dd8baab17e8813339456f8a2c      1
    44 0xed9f922304a7bc4cd1f1c3611060d8486fbd7c4b      1
    45 0xeeee61d1a5df13e4a0b0afbc65b0d91a6b7015e6      1
    46 0xef77bc2f8cb4ec186c277089eb164c1aa7a1e14a      1
    47 0xefe3f8fa7fa99d77f8e50c8484184906cfe02d12      1
    48 0xf07a2439296e07bc4320af924e655a01fb69d89c      1
    49 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    50 0xf5d97e38671c3239f67520a8787022b225425102      1
    51 0xfee1f4596b40b13a8b8723478ff87bd7c62b5980      1

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
     1 0x19748446a67b690ef1dd13ee61a615e9028bc6e0      1
     2 0x20fe609aa0cb04bdec2311c2d276a10bd10b17b6      1
     3 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     4 0x2604bccf91c4b2c5d1aa4a5f389e32bbf3d77c6e      1
     5 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
     6 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
     7 0x513a8a2577056d82b9e9d46c96625a4d4d05687e      1
     8 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
     9 0x6b027a7035a1760bf2bc603f31817db9199114f9      1
    10 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    11 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    12 0x9e1191773d527973fbbed04b1be96fc99a63bf1c      1
    13 0xbf687f8acfabd1e190c82244f473cc6a4dd7830b      1
    14 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    15 0xe99322e71900ef0d4e93aa1fae17666c1294640a      1
    16 0xf0492cf26db7cf1994bfe0e104659997531cdd8d      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 67 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x13ddf16f41f9a634eaca69be176773fe5c629c1e      1
     2 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     3 0x16f41b1b8620b024f8fbbb843fe5aae49cc9a207      1
     4 0x19748446a67b690ef1dd13ee61a615e9028bc6e0      1
     5 0x20fe609aa0cb04bdec2311c2d276a10bd10b17b6      1
     6 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     7 0x2604bccf91c4b2c5d1aa4a5f389e32bbf3d77c6e      1
     8 0x27e41b99b8c32a39474ab554dd000e19561afa2e      1
     9 0x3925e567b3ed59d1d5444ac902f96fb949c5fcc2      1
    10 0x3f75a3ca179d2c144539917024aa911abbb7ad9c      1
    11 0x42ed6eaefaa6d717743dfd61299e467692cfea68      1
    12 0x4518344525d32415f3ebda186bdb2c375d9443d6      1
    13 0x45faddea1c17a97cf61dd605226d511fd09dff7f      1
    14 0x4b2ef7127640964bf5cef3d1bf8d8f72d8d386f5      1
    15 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
    16 0x513a8a2577056d82b9e9d46c96625a4d4d05687e      1
    17 0x55aa430141908bd51749ddabfeb8a0e11e9544e9      1
    18 0x568a2e5713458a263da2b86cae2f6af0693799fd      1
    19 0x56b7b5f84eb16430c4de187dac21d19559287107      1
    20 0x5948428d0711442f6ae3c8d06e3a9895f33cecd2      1
    21 0x59c3d094c6ec333e034b87c06687ec09523ad6a4      1
    22 0x5d0905b6b670c1409eda5cc3414737c0c360ae2a      1
    23 0x601f82bbc160b6e51d4e16d342366d688d7e1ea3      1
    24 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    25 0x6b027a7035a1760bf2bc603f31817db9199114f9      1
    26 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    27 0x7e4fa6a2f352661933ffcfc3ec33d3b667c5da0b      1
    28 0x8592c855f55e9c233581ac7411a1db4efa08ad03      1
    29 0x895d49ea015b1e9da4af776239d0f8b029d660b2      1
    30 0x8c94072567b278b9fa12192cca899b3ce0ed5fdc      1
    31 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    32 0x95aa4c96fff08ed0514a84e94a08db2f12b0ba85      1
    33 0x99986b5e808fd039b992fad30075f517979d490c      1
    34 0x9d985b8d2e0135f2d355a96154ae975c8373d39a      1
    35 0x9e1191773d527973fbbed04b1be96fc99a63bf1c      1
    36 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    37 0xa4a6ac22e5af1fb902bf1edfa2b044f48470d27f      1
    38 0xab557245f9de18a7a2b59e8bc4f361dd83d16807      1
    39 0xab6ca2017548a170699890214bfd66583a0c1754      1
    40 0xb1af684799c96f5c740786be59dc834ff8ef5add      1
    41 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    42 0xb74edfde0752ea5ce2352fb404617c42f945aaf6      1
    43 0xbc8492a5815218920b0c28fd558d427d77c91ab2      1
    44 0xbf687f8acfabd1e190c82244f473cc6a4dd7830b      1
    45 0xbf80137831a4739314bf28111e68884b6bee7beb      1
    46 0xc2587a1685b194831c0e4a8927df0c33437fca3f      1
    47 0xcb1dd0f59fdf86658016827c78b858dfafa39ccc      1
    48 0xcf8488afeb807facf3cc683b0bf9d08e323c229d      1
    49 0xd0d521cf3c2125b35c9a74fdb3b839214b1491a7      1
    50 0xd14a49a9689459f932b0d29b63bf39fff8268771      1
    51 0xd26632a9403f0cc471be1916a843e65192e59555      1
    52 0xd40b35ac45adc2bf7083252c4a36f4058d6ec8c5      1
    53 0xd5917a0a13bbc615c5ad4553266fd9373b5c7283      1
    54 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    55 0xdd7784c63bd8b967ae3edeaa11f5847c3d2c1001      1
    56 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    57 0xe27dc3aa4352244dd8baab17e8813339456f8a2c      1
    58 0xe99322e71900ef0d4e93aa1fae17666c1294640a      1
    59 0xed9f922304a7bc4cd1f1c3611060d8486fbd7c4b      1
    60 0xeeee61d1a5df13e4a0b0afbc65b0d91a6b7015e6      1
    61 0xef77bc2f8cb4ec186c277089eb164c1aa7a1e14a      1
    62 0xefe3f8fa7fa99d77f8e50c8484184906cfe02d12      1
    63 0xf0492cf26db7cf1994bfe0e104659997531cdd8d      1
    64 0xf07a2439296e07bc4320af924e655a01fb69d89c      1
    65 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    66 0xf5d97e38671c3239f67520a8787022b225425102      1
    67 0xfee1f4596b40b13a8b8723478ff87bd7c62b5980      1

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
