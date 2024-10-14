
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:186         Length:186         Min.   :1   Length:186        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:186        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20948869 # https://etherscan.io/block/20948869
block_hash <- "0x60c94bc4eee91177a40bca05dbc85bcf7e00ff63429dc314c69d4a21e6b64bc9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4834 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","TornadoofMemes","OpenYourMind","Daydreamer","Surrealist","PepeArt","Foundation"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DaydreamerEditions","StoneandTimeEditions","KnownOriginEditions","NGUniversalVibrations"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","TornadoofMemes","OpenYourMind","Daydreamer","Surrealist","PepeArt","Foundation","DaydreamerEditions","StoneandTimeEditions","KnownOriginEditions","NGUniversalVibrations"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x007bd75451f4763deb8db3227040b92e90df4a2d      1
     2 0x0e324d3f5ac3c601bdfc7e0a08d7e22d83a91bbf      1
     3 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     4 0x1439dc5e6ed0b0a93d52b2afde061db7d7b5d082      1
     5 0x478bb542f7658d635abba67edb987806dff5b83d      1
     6 0x5bee2b0e902db594be8a9beb8e20f0be0f370c6d      1
     7 0x63f7e811a4dd446cc34a19fb7cc66fbd5af0fd06      1
     8 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
     9 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    10 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    11 0x762da606029d3120735aa1eec15464e265db7a3c      1
    12 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    13 0x8230a4cb486d9ae98710f4eff9d94bbc2f26e771      1
    14 0x830f98708905694d3f7c0240da87843226df552f      1
    15 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
    16 0xa181cf51a149ac04b1105358c8406ed65367ecfc      1
    17 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    18 0xab6ca2017548a170699890214bfd66583a0c1754      1
    19 0xc00d315682a2173a6e13469027b83f02d86632f1      1
    20 0xe24bdd48c9c9e432834d94df134c93eedb0ef7ce      1
    21 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    22 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 14 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0e4d48eab133598b6b4427b7a7edf84fe79318ff      1
     2 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     3 0x214f84f54d5b5b926cf54b15dc04bb74c721be77      1
     4 0x3deed956b999b83361b85bff31d388c35125411d      1
     5 0x3eb066892f0b5a19fb90ffe60703c56fb05d09da      1
     6 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
     7 0x70f729e89eabacacf11293e40a700524112514d3      1
     8 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
     9 0x87f928cf25b212c627936c790850a9f4a4eb07cb      1
    10 0xab90ddde2cf6a4753106a058acb4cc7412a58ae7      1
    11 0xad119a5520aab0664859a847b8117da7aea55948      1
    12 0xc7a3b47faa8b43e107a17d960214e8f6ba8d9619      1
    13 0xcfa986eca1aad9a61436a419924cd09d861a9685      1
    14 0xd4091d661a44648d61bd3bb51e129d0d60892056      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 36 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x007bd75451f4763deb8db3227040b92e90df4a2d      1
     2 0x0e324d3f5ac3c601bdfc7e0a08d7e22d83a91bbf      1
     3 0x0e4d48eab133598b6b4427b7a7edf84fe79318ff      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x1439dc5e6ed0b0a93d52b2afde061db7d7b5d082      1
     6 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     7 0x214f84f54d5b5b926cf54b15dc04bb74c721be77      1
     8 0x3deed956b999b83361b85bff31d388c35125411d      1
     9 0x3eb066892f0b5a19fb90ffe60703c56fb05d09da      1
    10 0x478bb542f7658d635abba67edb987806dff5b83d      1
    11 0x5bee2b0e902db594be8a9beb8e20f0be0f370c6d      1
    12 0x63f7e811a4dd446cc34a19fb7cc66fbd5af0fd06      1
    13 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
    14 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
    15 0x70f729e89eabacacf11293e40a700524112514d3      1
    16 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    17 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    18 0x762da606029d3120735aa1eec15464e265db7a3c      1
    19 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    20 0x8230a4cb486d9ae98710f4eff9d94bbc2f26e771      1
    21 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    22 0x830f98708905694d3f7c0240da87843226df552f      1
    23 0x87f928cf25b212c627936c790850a9f4a4eb07cb      1
    24 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
    25 0xa181cf51a149ac04b1105358c8406ed65367ecfc      1
    26 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    27 0xab6ca2017548a170699890214bfd66583a0c1754      1
    28 0xab90ddde2cf6a4753106a058acb4cc7412a58ae7      1
    29 0xad119a5520aab0664859a847b8117da7aea55948      1
    30 0xc00d315682a2173a6e13469027b83f02d86632f1      1
    31 0xc7a3b47faa8b43e107a17d960214e8f6ba8d9619      1
    32 0xcfa986eca1aad9a61436a419924cd09d861a9685      1
    33 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    34 0xe24bdd48c9c9e432834d94df134c93eedb0ef7ce      1
    35 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    36 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1

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
