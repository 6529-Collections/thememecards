
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memesphase2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:29636       Length:29636       Min.   :1   Length:29636      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:29636      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16789269 # https://etherscan.io/block/16789269
block_hash <- "0x9ebdd75e923ad2478528f7f6026a9c64bf295d345f67a2ffee2f6d79e007bbe9"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4788 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

allow_memes1       <- pick(snapshot, contracts=c("memes1"), address_pick=69)
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_69phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     2 0x05d23f26b6dafc7043fe94fbe85379e6bd0bcedc      1
     3 0x06fa75bfaa972845e7d78a6d928357f15a341d6e      1
     4 0x08a0f178506f3007263464fc68822365f6dff3a2      1
     5 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     6 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     7 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     8 0x0e757c27de6feed6d9f43120943ef07d89335483      1
     9 0x0ea8767de750442ec4a34d81bffcfd4417396f47      1
    10 0x1187ec27eb94e65717af65f3ae8c326bd8bb47c1      1
    11 0x1a4280869e14004dbfe81d1e8483c8055c9ac4ad      1
    12 0x280676491188f56fa386d9833d84702ac1e24c71      1
    13 0x297baa7d8b38330349250799af489e35ac1ef9c1      1
    14 0x2cac9e6d1d630a35cb8019779daf4c5fd96135ca      1
    15 0x2d19d78b7172464f295c200b18225f566899f2e6      1
    16 0x2db5a6d2a3d544f12a0c898ff25b1a18fcf9a554      1
    17 0x301be4f5965cc6d386a3aad84f1344ecf1042c3e      1
    18 0x34b93462e65303f3857460971584fd0d908f2f45      1
    19 0x3d20c3f372207074df1a0ab5272c7168ce23d178      1
    20 0x4581c619ae0556b774f71adab6831a86da1aef17      1
    21 0x486b93068f8c9f8fc41e46ad22e4170852618e15      1
    22 0x4b0c1beea2e8a8197fc48b0425a50a7204412989      1
    23 0x4e090c18a4955331c0197e3f8003c6a4ccba48c4      1
    24 0x4f13eabaf32ef4ad3837ae88fcfc6c69edba0377      1
    25 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    26 0x6b12a74690f773d6b6c14a982bad2000bde4bb1a      1
    27 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    28 0x6da2d31f358966219e0bb2543e4d2ae44d8f2fff      1
    29 0x6ec04c89f16804a17cc260866e8f7087cdeac433      1
    30 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    31 0x733983a4c7e778ca9f57cac6ee361c4a9b24e4b1      1
    32 0x75e60fd5f88faa26ecccc1212a3c8d6be4d41ec2      1
    33 0x782adafbf47a604f146af4a059908e946eae539f      1
    34 0x790fe1277cbaffd9eb9abe2e0fc7cf5e1d01988e      1
    35 0x7cef7db4289005fd56dfa6c73bb2be6ec13bc250      1
    36 0x8b338b8380755d416d892a0d7793bef75f6105ea      1
    37 0x8b90e7133ac7baff72671ff430f74e3855f2086b      1
    38 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    39 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    40 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    41 0x98462fa6f7fe16966330c8f6b93a880716d54970      1
    42 0x9b16cc7f4ae8c718d630fe6fe474b85d2a0cbac3      1
    43 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    44 0xa2917120c698fb5f2a03e3fd3524bda85a3eaef6      1
    45 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    46 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    47 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    48 0xb46c9366eb79ac9a61c6d2803f9975c6844b95fa      1
    49 0xbd6006b93f10b484d22d5a9d4e17bfd0ade4f614      1
    50 0xc6d4b36751233d1c509bc94bdaaa8133b3a281f3      1
    51 0xcf7c7758512aaf43979b64812cb79bd3f60f4413      1
    52 0xd170ffecc42ed18ce1a163c5ce6087a4c2a68bee      1
    53 0xd1b643c63d4dfcf4fae32b58d87843553a64b58e      1
    54 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    55 0xd57729ba3b5a1a53dbddeeab17e26c9cf3af5e59      1
    56 0xd7060289c22fb47569aa287ed4130a553f1c086d      1
    57 0xdb5cf3013a437ce5437b241b9f39b04f5d0bbb71      1
    58 0xdc0d28f3d09b292d41b836db991b157d641ad64f      1
    59 0xdcaa90d9f3b75cda80764326f6594b58d0585d21      1
    60 0xdd4dd1a8fd014861a7a705f31f0dbb0528043515      1
    61 0xe05adcb63a66e6e590961133694a382936c85d9d      1
    62 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    63 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    64 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    65 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    66 0xefb3da5189a6169a61176d1357579e135a1d1187      1
    67 0xf7474070f2b9843df17e41213b6d89a1a6648b86      1
    68 0xfbe02db58e0e0e02ec8b7fac182b0bbfab81d621      1
    69 0xfcc6cce3ed9b313adfceb11bbac0ac734cd78556      1

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
