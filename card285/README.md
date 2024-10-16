
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:88          Length:88          Min.   :1   Length:88         
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:88         
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



allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation","WinterCollection","BagerStillLifePhotography","0Collection","BagerStillLifeCollection","BagerKAYACollectionEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 47 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0775ea2a88db33038c4069e41dda8d963b64c2a8      1
     2 0x0ac30c6aaf2ffc01938d92aca03f71f913636134      1
     3 0x0b6d8a663c2978ce7ced221125769e092ee70023      1
     4 0x0d08ad2ab7893c04ecb460cbb6822b11c9e8904a      1
     5 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     6 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     7 0x180b7bb5ebb9bdff88a3e5bda4d747f665ec973f      1
     8 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
     9 0x24422361687270c1ac2dd3f336e1bc130849617b      1
    10 0x288af93bb8b388563b411f071bc136e9e4a56e51      1
    11 0x2bc4a480f8ad345b398abb23854e3062d5c57c51      1
    12 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
    13 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
    14 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    15 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    16 0x3deed956b999b83361b85bff31d388c35125411d      1
    17 0x403afdf9ea925d3b48e719a44610da1679a57651      1
    18 0x47baba9b83c7cd484297e771d99076e904647511      1
    19 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    20 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
    21 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    22 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    23 0x6327343b3a12fbd25488da0e82bd4512ad080423      1
    24 0x641d99580f6cf034e1734287a9e8dae4356641ca      1
    25 0x69a949d2330bfac2fdbee90ccfa9923343326b19      1
    26 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
    27 0x7e4100f2571994d0d937c25e87c5f58d79db53dc      1
    28 0x826be0f079a18c7f318efbead5f90df70a7b2e29      1
    29 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    30 0x95bb5c078baa7380d9c6bfcb9237c6c8c32d5184      1
    31 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    32 0xad128857e59a11ac345d40c6fb0c0832d7c31c56      1
    33 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
    34 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    35 0xc449f005667bef849261b35accf931a4bace48fb      1
    36 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
    37 0xc5da215997d4439b5d120e8926dce0b3de73e624      1
    38 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    39 0xc875c65601a98761c5dd38ae8ca34f718277bf3f      1
    40 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    41 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    42 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    43 0xd5f7500f921b9ab9f8aa21dd0713d1b9bf051766      1
    44 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    45 0xdb01f24fa773fef587b2a05a8ce27977871367a7      1
    46 0xe16df6503acd3c79b6e032f62c61752bec16eef2      1
    47 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1

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
