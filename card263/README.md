
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:94          Length:94          Min.   :1   Length:94         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:94         
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20446069 # https://etherscan.io/block/20446069
block_hash <- "0x05b7c76555f100cc8b568edf509caed82ade6b72095227215b57890b50bc82c1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4552 

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

allow_artist1    <- pick(snapshot, contracts=c("OUTRUN","Foundation","Rarible","VisionsofDeath","VisionsofBirth","OrbsofNeoDinamo"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("RaribleEditions","FragmentedVisionsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("OUTRUN","Foundation","Rarible","VisionsofDeath","VisionsofBirth","OrbsofNeoDinamo","RaribleEditions","FragmentedVisionsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0dda698d2fe2fc1fb8f5b54ee9cd77fbd5a1d08b      1
     2 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     3 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     4 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
     5 0x5ec53b2ff1dfbe04dc8e236cbe6f63182fb2bd20      1
     6 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     7 0x6186290b28d511bff971631c916244a9fc539cfe      1
     8 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
     9 0x841e54526835b674392e2b8f0ec19be0577add87      1
    10 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    11 0x95476885487cbd038bac833933bf044a75d4b9e3      1
    12 0x95e8a4792d7e5bf6ce3973184400cc9c37b6ae67      1
    13 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    14 0xa3ded378678d735eb14ad78db64887386932c62b      1
    15 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    16 0xa94987a39943b0d27941191bf237e28802e46658      1
    17 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
    18 0xbb8b83694226889963ae8fb15d488074e5662b4a      1
    19 0xce719191f22db9498250f37672bf3f421bd42c41      1
    20 0xd69a4f613c03e991678d990a06904fab28662f08      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 20 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0360b9acda197703cb3040715c75659c38f13cf7      1
     2 0x06d2d2c9e98299549bf5b55f071dc4e4eb508023      1
     3 0x10c872453abd2f49f46da316ad09ae86f97512f4      1
     4 0x1896b1a2836d24c7a0714cd5593c4b4064d3b031      1
     5 0x2152f4224c45efb1611826764b246ba3dc0c8ca8      1
     6 0x578b076f33c021ca8ec8873be00c734559a99057      1
     7 0x6a8194df91bcda38da1230ad49117eb811c713ad      1
     8 0x72c28bebcbf15d7437d5446b40b9b51c8a34c588      1
     9 0x85a97ed213589e10e94256f1ec939bcf6e73dca7      1
    10 0x87ac2eb52b8929dc7f05d4d3586cce67453f0522      1
    11 0x979f8cdc881fed1fc76f809eea3859b01264861c      1
    12 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    13 0xa16e340120cb68069ad2ba3ea9f41eafa0828338      1
    14 0xa679c6154b8d4619af9f83f0bf9a13a680e01ecf      1
    15 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
    16 0xbec00274dc8b2775ba887d18b980a95e4898ad6d      1
    17 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    18 0xed28f5c9848579089f29cc3fffbfe25df50e887e      1
    19 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    20 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 40 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0360b9acda197703cb3040715c75659c38f13cf7      1
     2 0x06d2d2c9e98299549bf5b55f071dc4e4eb508023      1
     3 0x0dda698d2fe2fc1fb8f5b54ee9cd77fbd5a1d08b      1
     4 0x10c872453abd2f49f46da316ad09ae86f97512f4      1
     5 0x1896b1a2836d24c7a0714cd5593c4b4064d3b031      1
     6 0x2152f4224c45efb1611826764b246ba3dc0c8ca8      1
     7 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     8 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
     9 0x578b076f33c021ca8ec8873be00c734559a99057      1
    10 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    11 0x5ec53b2ff1dfbe04dc8e236cbe6f63182fb2bd20      1
    12 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
    13 0x6186290b28d511bff971631c916244a9fc539cfe      1
    14 0x6a8194df91bcda38da1230ad49117eb811c713ad      1
    15 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
    16 0x72c28bebcbf15d7437d5446b40b9b51c8a34c588      1
    17 0x841e54526835b674392e2b8f0ec19be0577add87      1
    18 0x85a97ed213589e10e94256f1ec939bcf6e73dca7      1
    19 0x87ac2eb52b8929dc7f05d4d3586cce67453f0522      1
    20 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    21 0x95476885487cbd038bac833933bf044a75d4b9e3      1
    22 0x95e8a4792d7e5bf6ce3973184400cc9c37b6ae67      1
    23 0x979f8cdc881fed1fc76f809eea3859b01264861c      1
    24 0x995e0505603a19ee5c469d2359427bea68c6e953      1
    25 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    26 0xa16e340120cb68069ad2ba3ea9f41eafa0828338      1
    27 0xa3ded378678d735eb14ad78db64887386932c62b      1
    28 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    29 0xa679c6154b8d4619af9f83f0bf9a13a680e01ecf      1
    30 0xa8357ee17cb3ff5a6b9694ddc8fde0ed2ce9d788      1
    31 0xa94987a39943b0d27941191bf237e28802e46658      1
    32 0xb1af13b2e4be55ca69829a23c09a1f5663adf8d0      1
    33 0xbb8b83694226889963ae8fb15d488074e5662b4a      1
    34 0xbec00274dc8b2775ba887d18b980a95e4898ad6d      1
    35 0xce719191f22db9498250f37672bf3f421bd42c41      1
    36 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    37 0xd69a4f613c03e991678d990a06904fab28662f08      1
    38 0xed28f5c9848579089f29cc3fffbfe25df50e887e      1
    39 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    40 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1

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
