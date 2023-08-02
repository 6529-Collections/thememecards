
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:20193       Length:20193       Min.   :1   Length:20193      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:20193      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17818569 # https://etherscan.io/block/17818569
block_hash <- "0x892c9212da2f57301d2d748f20c3505b658672e54cd771a8a9c402936d717f18"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4326 

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
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=100,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random100Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x03d8f62d29cdbfd778e670fec066d575ca87e050      1
      3 0x05b00d69d50426020749e1e1c60901e129d3e43b      1
      4 0x08882cd344a56c3da51a19def6cdcc9b290e7c80      1
      5 0x0c544f463600eb7d94b3694f3f3eb171c2f1a93c      1
      6 0x0edc79adf90a6f6960932488ae70e8c22db9c208      1
      7 0x0ff14dad100343a01cb7599aa7485c4892378e74      1
      8 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
      9 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     10 0x130c02f83a93c4a8c5d77b153c555d7864a855bd      1
     11 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
     12 0x161df6693a3faf54ae5ea6790cc43ef8bc68dc72      1
     13 0x17cbfe407a0a2285594fe4d8a88038c9b00b876e      1
     14 0x1ac0a9f1b13dc29537767c3472bf5f155b98459c      1
     15 0x1b1e6ac220c6c845316a5aba856df282d4048e8a      1
     16 0x1ed73cd65a1ebef57ef61022017f998705787fdc      1
     17 0x24714a78ab5f39c7445497764466466497bb416b      1
     18 0x27389671e4a6f66025738e8bf57ec28a73d8a667      1
     19 0x2bc52900a118390ea8a97b29eb5e7e75f1cd912c      1
     20 0x2f5926896bc7f9432392099bbc66526cea300077      1
     21 0x30c1f8d8b7d34acd0827e4fdc7b2d98c32287ba1      1
     22 0x311eefb3864993fcc3140e4284aba29615368bf9      1
     23 0x3a5e247429201d70fae0ff28e41fd8a1ca50af5a      1
     24 0x3ce8c42817c045bb83a17f2a73ec31633808d977      1
     25 0x412d974ca20038402a4e50d586fee6ba6a9279ed      1
     26 0x49e88c0236734eb01243ffc9e84915b3a2878640      1
     27 0x4c327ee3ae154220513cddd0292d166bdb414426      1
     28 0x4c9ab1064da9c0d384530131498e3c34617a1508      1
     29 0x4cbba098a091a53d7402bc84aa8abc5591ed9fe0      1
     30 0x4eabe483241860e09a0fde2acefd07d5bc2114ea      1
     31 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
     32 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
     33 0x4faeb85b41468ac56640d0f2baf2241a242ee5cf      1
     34 0x51733185d7d1f5f32c2bec2bf120b84b8d284a70      1
     35 0x5301661d5a55e3712537949c65b86ea72cf41519      1
     36 0x536cb7bfdb6b66ca30c7ed4a307762f194e05fd8      1
     37 0x573efb51f83d4265763f530295d4afb25a487bb7      1
     38 0x57a1ffd4a8fff660ef1dcafa0d1520ed0e9e6d87      1
     39 0x5af981bf8574aa977adecd67e323c1275596086f      1
     40 0x5b4bb3c2bb6e2ec707cc39a86dd398c0a9f69add      1
     41 0x5c6531d7d9b6538d37ebb7dd4e05df1401dc4666      1
     42 0x5cc1e645f4cfae6c00a4f055c12a3d80fef925aa      1
     43 0x61b4468832a1a78876e5c6f1797b2da9360c73d5      1
     44 0x64951c5e4ed6ab0ed319720fa3a6cc32949cc649      1
     45 0x65a5c6ba0f55c1c0e333bfe0db783aa07e8310ff      1
     46 0x66477d6f62dbd802de7196d052c650731c5bdd10      1
     47 0x688976844782ca545968d368060744900c5f49b7      1
     48 0x6b8f5e3aa817dac35d211342819fc60d99e5f0fe      1
     49 0x6da2d31f358966219e0bb2543e4d2ae44d8f2fff      1
     50 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     51 0x7024ee7932e4d879409f9618b560cc82cf093a7a      1
     52 0x7132f37e94e8752ed37f3239597e3d927ccc2d83      1
     53 0x72bc237cf598505dc00d4fcc23b1663e206455a9      1
     54 0x7485ac6d8534691993348d51ab0f131a19fff763      1
     55 0x76163cb574a5d9b08f24762ac6f1fbf33d20bb2f      1
     56 0x7a24e9d8c0dac537bf29bfdd4939345fc6c6d195      1
     57 0x7c2f06ab61a94fe9760cc8d9ad35a33c0472cb8e      1
     58 0x7f8b215572f16f982c626f044cde114152dd0e6a      1
     59 0x82d32aaec5b77528d920843d84cfdf45e84ae9b4      1
     60 0x8699793e22403b355185d8ff76e7392f98aafa46      1
     61 0x885869cb03bfa16e2e21e0e0d1b64b8ae9374f4d      1
     62 0x8b095365e9831412147b5dc07e644dcf87325106      1
     63 0x8b90e7133ac7baff72671ff430f74e3855f2086b      1
     64 0x8c847c9b48be308ab7002011acfcc728483d8b21      1
     65 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
     66 0x951e1d85810c7b25dbc0ecceecb4248b4e202412      1
     67 0x967be09d0652a0be582f217630963df129157d55      1
     68 0xa0664d876a0708b015acdfa8df46d1f47e068fcd      1
     69 0xa5ed8da239f400141427800da33b602a039f2254      1
     70 0xa62c919b2b8cb9f2297f3ec7ee13265faeff304c      1
     71 0xa6c579879252d8abb6e9150e4aa0196ba81c7b27      1
     72 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
     73 0xa84ff8c3feaf71a7090e90f676ef1c9c94f7dc92      1
     74 0xaea3e4bd4a5250ec413e31b95126f3f997493a8b      1
     75 0xaf803180a4e75eae08697b36fa0bbce292fd3949      1
     76 0xb3bf133950d87f84508fc64735ed40a4f2797aa6      1
     77 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
     78 0xb53349160e38739b37e4bbfcf950ed26e26fcb41      1
     79 0xb9fa078a43da2664c8499c5d2073f2526f96f106      1
     80 0xbc463a622a228d98375d575110c0ada0dd4255b7      1
     81 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
     82 0xbdb8ff99d03f45df9a38a20fa80da44c5ad88b24      1
     83 0xbf814810b44978de273191fd612aa47f7b69d564      1
     84 0xbf94e22ce9f77633aaa26443d7e67835e0b7f1a3      1
     85 0xc422294fc110d20e70e8fa2e09efb930fd392b88      1
     86 0xc9dbad3feaa6603b4ea53b89e1a737ad2cdd54d6      1
     87 0xcab81f14a3fc98034a05bab30f8d0e53e978c833      1
     88 0xcbca960d11fe36a79092980bcae336a1e9a3b119      1
     89 0xce1966d5329bc0f3043dc09c6502c237d866ab05      1
     90 0xd537a2b2b97d104f2d9c7a84377fc11573629085      1
     91 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
     92 0xde75ae6ca63d3ac4d2a04d6d9c07162f331041a8      1
     93 0xe2d6b30e63ecb93fc0ffc1e9bc1327e8b16246c3      1
     94 0xe50a5145483c22242be79bb231963b21cfd94041      1
     95 0xf0b95bc02c2e391f92a89a7657814453e4f9d76e      1
     96 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
     97 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
     98 0xfaa5d029d4085c604f2ceb8a7a028c22e7398be3      1
     99 0xfaaf6e583546295ce96316ec26424f37885668b9      1
    100 0xfd5dc6839fdd1fa8317a52a268956212c2556edb      1

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
