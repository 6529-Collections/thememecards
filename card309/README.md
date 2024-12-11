
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:102         Length:102         Min.   :1   Length:102        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:102        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21346969 # https://etherscan.io/block/21346969
block_hash <- "0x1f4d6ca4e3aecd35da78070a9d654acf33f62c217620335a791e7401e57620c1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4536 

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


allow_artist_all <- pick(snapshot, contracts=c("Foundation","IOR","PATRICORTMANN","MakersPlace","SEED","NINFA"), address_remove=address_remove,address_max=1)
```

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 63 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00c1b5be9c09fb88e9bcb09af977750c88f503b6      1
     2 0x00c7616b53e7c5042a74ffafde70c80ff6e94e7c      1
     3 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     4 0x060232cc1148b1fee30aeef999aa199be10db3c7      1
     5 0x0da6df9f5710cb24de6cf018e4294d8b82dbaa02      1
     6 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     7 0x290c7876044443f6567268db0d8995f8ecd262cb      1
     8 0x2c47174a27827c3ab4a9d62de88fca4c799f268b      1
     9 0x2ce780d7c743a57791b835a9d6f998b15bbba5a4      1
    10 0x2df8063c53ebd6270acde4e996a925c531fcfdfe      1
    11 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    12 0x3f2e183df2f414bf9657a67b5784e91adb02c07b      1
    13 0x401460b90748b2d571c094a46feebf5990dd8e43      1
    14 0x4e719fd4525ba99f22a769f198825824ce00442f      1
    15 0x526cf160f2a47b1cc1e5019dade58a1a1df6bed5      1
    16 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    17 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    18 0x57482499d9893a2a8abc15e1b0da0d2dc40a5c0d      1
    19 0x589ad95db08979d176ff66e2880b185680967b55      1
    20 0x5bc3bb3db7136e131a9bd12eb6c22b4cd0380a74      1
    21 0x6223febd62e2773a372d1972db503cd5ed4c1312      1
    22 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    23 0x68e68aa23aa4d759bce6ab641d57f5e7656029cf      1
    24 0x6907ef6d0433fc70ed1a4727801460cceb08564b      1
    25 0x6a8cb5f54ab7dbbb6ef3b669ba1143d7d49d5e85      1
    26 0x6cbf1b5dc4fe5b8b71ba156fb6d3342ad6a4e8fa      1
    27 0x762da606029d3120735aa1eec15464e265db7a3c      1
    28 0x7c3eb18b553830be9b82a137dfacca7c663ef7a6      1
    29 0x7d85806c589d9e1301898932a32108f15c6daf14      1
    30 0x822a6d21fbdfc766a0f52a9a74789e4414863dc8      1
    31 0x8274bfd903ec76ee72b63fc246d5ef0bac2b9cef      1
    32 0x82b8b659a4a98f69cb7899e1a07089ea3b90a894      1
    33 0x844fa4379f3c92f79b32f92574ed3694c2b7b49c      1
    34 0x856d1b7cdb66e29938cdddf11293904802ce451b      1
    35 0x8c990b7e14dec4fbd53998ec4e971dad4816dc2d      1
    36 0x8d897ff284c13dafced15d8ed8df275f34b9cd6b      1
    37 0x910ca5528ff6de3e72c7979ee2abc1a4400d77e4      1
    38 0x96686d6fdb3f978b969df765f990a9927c41eef7      1
    39 0x9697d6e3133d745f684c875048fd3054529484b2      1
    40 0x9a7e88c20b7955d8b62f4b68775c3d653819b45b      1
    41 0x9aef7c447f6bc8d010b22aff52d5b67785ed942c      1
    42 0x9c9c4f1b3b40f22d909db22cd221967f9f17ab96      1
    43 0xa4974dd4d40a04e87670671bc4f8076ae74b2940      1
    44 0xa9a7ba36a680baf220425a6a95557ee9fa1fb66f      1
    45 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    46 0xab6ca2017548a170699890214bfd66583a0c1754      1
    47 0xab7473da0af4658ae2879cb641d82e7652721486      1
    48 0xadfeb395469613b3ab99d788811e2ccd3058e51c      1
    49 0xb8633564f28a45e77caf40bcfcdbec02f543e34c      1
    50 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    51 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    52 0xca2eec2a2c7f49e83dfb12cee2d2c0faab14b661      1
    53 0xca35f3dfc3fd9ffff8241b93db8f71d46d2122b8      1
    54 0xcadab23e1a0f7e3cab2d1c754412c7f913beb1f0      1
    55 0xce63b511ac3855b7d5a0c92c965fd62b9ae4c0e3      1
    56 0xd4572825a90b1ec0ded17e88b8b923a97a71c5b4      1
    57 0xd4a24e4d69d60b4c39308908e68f6103f0587228      1
    58 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    59 0xe5f94006325cc051e448c3a02f88f7b7ac269d93      1
    60 0xe8a7a64554a0fc0d1352fc7c3e17b656caa84391      1
    61 0xea1f1e2bb9300acd01080795f1af1c9485659aa1      1
    62 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    63 0xf608f836cab2b4a0b7cf4ed35633b9d939352cec      1

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
