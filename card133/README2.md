
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:15657       Length:15657       Min.   :1   Length:15657      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:15657      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17932969 # https://etherscan.io/block/17932969
block_hash <- "0x2694ce64597ab76ac3f38b27788ebb691478b3ffbe33c1cf7a5862211ecc4f35"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4675 

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
      1 0x013905d71f866961e991a4eca105518234b2cbc0      1
      2 0x047d7e0a147026e41169f0f168986caafaa0c6c7      1
      3 0x062164d0c571002b9b34a9a71ddaeeb0b6f93132      1
      4 0x0623a3b40d4a7a95e410d203fae7d8f9c92ed480      1
      5 0x0bd31f087f85de4a84a985c61501b69654595f5c      1
      6 0x0ef024d299cb56805f2437cd00b8a361a7b06d54      1
      7 0x10c9cca806ba4a8f580ffedecd9c2e8a49e6ef91      1
      8 0x11b7b38d5b96d02d6381e646ad8ca55db74dadc8      1
      9 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     10 0x1439b4d5a72343b68f12398c649df99d76b2af53      1
     11 0x1a3bfc4ef279975059221e5285db047905165a5b      1
     12 0x1c38f0d96357168d8d13afdf8225f55f743bc4af      1
     13 0x1f8a8ac54244e4016a376152b2f92d467552fa7b      1
     14 0x20111f285d896e87ff96c9cac66c053ccd79eafc      1
     15 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     16 0x2507e2f200fbbff420793585b2a10ee4031845ae      1
     17 0x2604fb7b2c561d6a277a0ec8c2308a26cee18272      1
     18 0x28bd50226a8523ccfaa2ff2873eef032a7feeafa      1
     19 0x295d960d752771579118c21a3d82f6c7cb3a06af      1
     20 0x29f901a1e829e19387e9db644ddaaa26c3c41d66      1
     21 0x2c4090a54bd6eff78bcbc0b49ed17e0f152389c8      1
     22 0x303052148a78a692d7aa89bad454f3e91785488d      1
     23 0x332ab3f0dd19b601e988024bce17c9795a8c88f7      1
     24 0x33914463964f994f093bfbaef4db18110dad60d7      1
     25 0x37ae7b1bb67190c1a0eea775d62df884402f2b4e      1
     26 0x3819660cb4d48b192b4973cb8323d6cb1404d930      1
     27 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     28 0x3b3a01fc0dd12698f19959ed0d0d042b81ff13dd      1
     29 0x3d38729c81dc8a19a44e7ad929c3b8580eef97c6      1
     30 0x3d5296326f1d61afb52f01eeb931a0d329e24b36      1
     31 0x414723d4f305f1dcd71d72bacbb9988f7625ebb7      1
     32 0x42548a143764550be44273dffb098103625fd965      1
     33 0x44e204e872dae8ac81584247f48f317494bf8184      1
     34 0x465d06aa6b0dcd4f2d9a38d14b20a042b92efbc0      1
     35 0x47a3443937af19aca502042617165767b278ee35      1
     36 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     37 0x482b9ea86052bcf19ec00a13188abd9522398911      1
     38 0x4e3a10f142075389848ba955ab6282d9338839de      1
     39 0x4f494d0f18153cd75429588dc67e0975b67dcf46      1
     40 0x571d1ad0867b02f9c90c1efe64e32e8d50006a4c      1
     41 0x57a4c7c267afdf07bc8c9f8faa46a43ad5ccb688      1
     42 0x590f4ca9b70860d1b89be8a7e69f22e59f6dcf6f      1
     43 0x595a1b38c53d8fb5fbdf68196f22ef79fe59c9e9      1
     44 0x5b4eeceaffa265ac08847966af19dc0d19f2cad1      1
     45 0x5dd47005f8a429b228a5d30b5345a1b64fa78c0c      1
     46 0x5ee559349f01e5032324d6804d9ae4fd89041795      1
     47 0x61240103d0ef3bf2b6f34513eaf8cdd96971e8d3      1
     48 0x618f55773cdd0dbcff63a49f3957e22db32052af      1
     49 0x64c4bf67f8782f1bca8a34172670c061edf7638c      1
     50 0x6b9ff2dc17cee986f796ce9630f8acda75f82e7c      1
     51 0x7674a62ad59b4b499630a786de6de0fc2f48f036      1
     52 0x7b7173fb7e7d2bf457e1e57346779b63e6612e6e      1
     53 0x7fde0088a3b772ba42ae088e2d3b0b9026e26dd9      1
     54 0x8181dd699e486493027e4e21bf6d0c7b7c94055e      1
     55 0x832df0f1a2a40286b4ffa79676fcbc8b78f154ed      1
     56 0x84f8bff48b845e7cc2f5a9175860ef6b0016c1d6      1
     57 0x8510a4ab79cd9b8c32e5a96d2f1124566f17a80f      1
     58 0x8577243a29f7dd5780ab8843b00a2710ab410e87      1
     59 0x85c3b194e2d4bd89d10fe1e175b209898f8858f5      1
     60 0x860e3777df77395f9e22c5d36a37f063dfdc07bc      1
     61 0x87c6fd1979abb85c430d89fa97cd731b440687b2      1
     62 0x8808528b90944d93fd2b5c79fac546259eaf7d1b      1
     63 0x97cb3692848e43828c8289105e6c84e66bcbd169      1
     64 0x9b5f50146a361f82b1fedf63c5d04d4918a730c3      1
     65 0x9fce6fcdf1f17821deb1a7e078c3c7941ec465f4      1
     66 0xa595e9525b0b2dbf91054689e385f5611207b9c1      1
     67 0xa626d27bbe486fa00f58df20a881c7ad224c411d      1
     68 0xa6ddbccdbf36bf20e499afb82e6a8aade59e8af2      1
     69 0xad887834294a92d5d6590608de07002be6fa3384      1
     70 0xb1b7c542196d532dcde45fbabf1aaaedab620357      1
     71 0xb53aee8de8c3589293ea2142e466348fe62b8b99      1
     72 0xb5ce9f0158b02bbad38eb706bc3956d5b8b55288      1
     73 0xb60d52448a5c76c87853c011bb4b8ec3114a3277      1
     74 0xb6297ee5d471cb13c03496ee38dd31893b6b62e1      1
     75 0xb8a69fd9b077b1588cc10d807efc4618df22b99c      1
     76 0xc1bc1b32b437b81345b84f77d300d95bd24cbb95      1
     77 0xc31ba2d3275a397c43b1a4ab707cd79adf310233      1
     78 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
     79 0xc3f1d077fa590018d3924ce8da1a085b4eae506d      1
     80 0xc64ff776e44379863bae2df4b96c78d5dfa379a9      1
     81 0xc6eb0fbb058f6988fb6a7217f8089b89abd00b4a      1
     82 0xcec0226557b7fa4842a37375fd75fa33882c1783      1
     83 0xd8c247ee44dfc6898fe4e224979f64e367d2c9ea      1
     84 0xda7ba2645e540d39c0e4b3c51a17126fff2b7e45      1
     85 0xdae3dcf64f91ed899de50b8d45d6cab7cdf12ff2      1
     86 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
     87 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
     88 0xe1d8963bead06af2f756542b5bb673751a39be5b      1
     89 0xe7a8a504a82f0a829656261a54d0443f85e19604      1
     90 0xe941c022f3a890aa2ea84081d6b5aa3b4f244850      1
     91 0xeb946bfac0d78e7f0d12d53e5f5b890bff25c7db      1
     92 0xf1133ca2ff1b4e5df7121566c0520199f8937e78      1
     93 0xf2e03bb3bae583d72d750b3ef443372f92cf2c24      1
     94 0xf2ff5c8fbebc6049bbcf7dcd857171e54f8824cf      1
     95 0xf857584f7784fbcb2874d9d9c3014725b18dd0df      1
     96 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1
     97 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1
     98 0xfe6c92d283fb88c6a1cbebac4ef33b1cafd118d6      1
     99 0xff56d36ff30751e43804dbb1b904afbc115b3b53      1
    100 0xff8c476a67b2903e077b16cdb823710ea3d4bc7f      1

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
