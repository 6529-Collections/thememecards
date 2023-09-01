
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:14545       Length:14545       Min.   :1   Length:14545      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:14545      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18033069 # https://etherscan.io/block/18033069
block_hash <- "0x0d35f2e95ebb81cdac589a856486c16b49d44e59304b12dc7ad6c09cea6cb954"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4759 

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
      1 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      2 0x02da7d98fa0782400b5adc7e8319de1e52a60d1a      1
      3 0x0623a3b40d4a7a95e410d203fae7d8f9c92ed480      1
      4 0x0b071354be55d703b46f20e279853ca8f4e28ee4      1
      5 0x0c9d4d222911f3930ba6449df1f75541cd3ecba2      1
      6 0x105965f34e29f55efade78337e3eea727faa8f67      1
      7 0x13821b361f739128e4f7863ff4d17522ddd41504      1
      8 0x14b76991c8ecda8cbe4a5d330df992880a986fd0      1
      9 0x16c3676077017b856dff4514170332abd7dc2729      1
     10 0x1af42e861037f45e1afde49917d545cda5171a72      1
     11 0x1f9f7531a3d5102f9cbafe7425fc945fbf58678c      1
     12 0x20a1e6ac98f4406aedec1623e7446f357d78e7c3      1
     13 0x20f4a287f781c1a522e918ccfac8e99c40fd3eda      1
     14 0x25a6a662aa143ab46b71a50908dd8a74ea56cab6      1
     15 0x2a3bccbbea5bd5f788cdadf2963f8e29c653cad7      1
     16 0x2b8f8968655179ca8ba428c66e89c443d1adeb7a      1
     17 0x2c1725bff8496ab8098406639ee34e5b015f74a2      1
     18 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     19 0x3177a46ef4afca9361816e9dc000f967bfce427c      1
     20 0x3299b96d78aada4191e4aad22e585fb613f3b420      1
     21 0x32bc332d4b1a58661bc693fcd66d4f90915baf71      1
     22 0x365f7b46973f27740c08a077b66ec810cab39306      1
     23 0x3b1311363bff7c9d43263a2f1c325753210d3327      1
     24 0x3ba53331f1084ad49b5b13da7882165b52879ba9      1
     25 0x3d5f1ad033c4e7c6a6d433d50d694c488dcaebcf      1
     26 0x3fd7783b11d972801d2dccdf5609fb8842234c5d      1
     27 0x41e12756498322b479f889af2b2f4b29a85d5605      1
     28 0x422f3eac277a5f3adcafc5d4054c04ae35a18ec4      1
     29 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
     30 0x4a6eacd00e3a586550bb3b52731d51ce7f53f490      1
     31 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     32 0x4fa5cf05c793c8d7818f64751e4c842d17e0a655      1
     33 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
     34 0x56e6d67c98129ffa739f17088173e027c0e9379e      1
     35 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     36 0x5bd906bc2c38ddf3abd65b6e81e5f2d2a4ca98ae      1
     37 0x5e65a0ae97928b30f44c6fb852c27cbb47acdeda      1
     38 0x5fb7778b16e23930c3ede1c0a3d69652b350fb03      1
     39 0x64355910c6cf1bb7b1a70260d4947a3805fdca4d      1
     40 0x6670421786cb9b4fb09181141edd3aa1e5389975      1
     41 0x69471066140252f1a7529b71b56cd06b6f390eb2      1
     42 0x6d5c8f445936aefa1021c6d53e86c4ad5545e48d      1
     43 0x6f3591e9f589e7d70b56b4a5ee974d569fae3e64      1
     44 0x7034adf76d5b3fe6ca953799ba4aa05e511db56a      1
     45 0x7086f9b9ef5ab6653385f6751ee21bd82febe1a7      1
     46 0x72d037f63a6c782f85c059fc917a12e71b2e0c73      1
     47 0x790c60d199c9395dffb605e39a357c4da4ba9deb      1
     48 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
     49 0x7b768c81353cd5ff73b74e72dc7409cf789436a3      1
     50 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
     51 0x8569e2c88afe9a5e0f6e7a8181172e1c427303a6      1
     52 0x8dbbca57ea56290efa14d835bbfd34faf1d89753      1
     53 0x8e5f71dc1d9b1dd976137d61fadd5a4318a8ea39      1
     54 0x91198e5b88051d7d61ae43f322641e6674697af7      1
     55 0x93fbd02200d8684ec0c0a48eaf07ba9546762889      1
     56 0x963a3ab2248e8a70869e1c8eedbdadd67b006518      1
     57 0x9ac16a11b23b2fee4848cc2b16516d73d9a8f57a      1
     58 0x9c5f3693958aa03d3279a78f04fe516f40287092      1
     59 0x9c786733e9af618b459250831833ede285f56301      1
     60 0xa033962385ca19c7a7109fafee356daa7a9291d3      1
     61 0xa2321ce998844cac8a2f67a481fa3e23e4c6f039      1
     62 0xa30d095a87068b90063d8b2c114bb553579d39e7      1
     63 0xa3e826a5d1631baa4cf77d02ed829d2c6fcbc9e9      1
     64 0xa579f766406d0e18d8c7ff81d6e2db1dc3063943      1
     65 0xae5ae13a008b662336ff3674d920819102ae4256      1
     66 0xaf153e755f59bb62ba8a5b7e5ffdb71c0ac43305      1
     67 0xb0824a58f3d26b6df4a037fc85b548f2a6eec096      1
     68 0xb0f85862f5bf2162d0a7608730a5b1ee1a15ee3a      1
     69 0xb4d0e8be45395e72e9c1af25f59d854bc7dc5b48      1
     70 0xb509189877c46f92cb784d59e1fb6dfc03cd7ede      1
     71 0xb524ac0d100f6d35a82ab5a1c78ce2edf5bedc39      1
     72 0xb8a1277b827d69ff3626f0853c370ebdfb035ac5      1
     73 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
     74 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
     75 0xbee1500a2fcab995558cf0cca86b986b06f7cce4      1
     76 0xc25d23d642b05b9546d85574b53f8b62b6b11a7b      1
     77 0xc2d14c396822ba325aa8c8d0f6213828cb69d925      1
     78 0xc37016f3023d0218718ca22a423d9dd00eb58275      1
     79 0xc968fffb7c3dd8c870d7149f454c4730844a60e5      1
     80 0xd16164c0982fdf729fd9f9652ab80d57e365333c      1
     81 0xd1d99f286adc9ca0bdd7c0b234277aff95372dd8      1
     82 0xd53f450865fc6ea2d9bf38231aac358eeb137c84      1
     83 0xdbb3f1bdc0353eba1c57dce84fc6a5c3975b321f      1
     84 0xdc2d5126c0147863e97e2a5c91553ac45ffa5374      1
     85 0xddfcb75f00f2cea10097a0ecc44a32a9c4b62cc9      1
     86 0xde1f3ca8d0666dd3d53a608a9da1c4d472d135bb      1
     87 0xde40d2cb9db9b1cc295e18fb2b12612a66b5c5d6      1
     88 0xdf5b0e3887ec6cb55c261d4bc051de8dbf7d8650      1
     89 0xdf7c9d087fd7e77c095c5826ed8d9bc4a5d0be3e      1
     90 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
     91 0xe33db7fd452e40c93f25a0ed2f7e5724414bd255      1
     92 0xe55de13319ed4de98f740ba608ca49d27a420982      1
     93 0xe77d44e642c53db943aa0a71ef60cbff719e644e      1
     94 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
     95 0xed3f868fa921974f4addbfc982b1298c82dc6cd5      1
     96 0xf253c885f7e5f2818c03547b16f5e9068d4735aa      1
     97 0xf71c063492a833c6124236a746da25f6f240e44d      1
     98 0xfa496bc5a6a81318e926cfd19340c7aee83d36db      1
     99 0xff546e41d928472bd99fafd30913e41a7f6106d3      1
    100 0xff6899914cc3ed62c31920d85391ada61e6cd4f6      1

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
