
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:124         Length:124         Min.   :1   Length:124        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:124        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21046969 # https://etherscan.io/block/21046969
block_hash <- "0xe247e2663dc9c6a8f03a8eeeb0977bcfbc91469caf255aa90da600e90742d563"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4754 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Emptyexpectations","Letithappen","IAMALIVE","Bullrunartist","Stilllife","Ojosdegirasol","LateNightIllusions","Darknessinterlude","Skuuls","AstralDesires","Ninfa"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ObsessionEditions","UnusualbeautyEditions","ChristophEditions","NG1Editions","NG2Editions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Emptyexpectations","Letithappen","IAMALIVE","Bullrunartist","Stilllife","Ojosdegirasol","LateNightIllusions","Darknessinterlude","Skuuls","AstralDesires","Ninfa","ObsessionEditions","UnusualbeautyEditions","ChristophEditions","NG1Editions","NG2Editions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 45 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01503dc708ce3c55017194847a07acb679d49f47      1
     2 0x12c5e83175bff0a8e3aaf9536bc45e88d567491c      1
     3 0x2733cbe186440511cd0aeef6dd37a67dac40899c      1
     4 0x2886254556ff9b1572845865a0270a3a7f52b4ff      1
     5 0x309a1ef07c771edbeed38d66364d4f20ae537bdc      1
     6 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
     7 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     8 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
     9 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
    10 0x44d5a0b1e165a17c16d940ca774a28e62d4b44b0      1
    11 0x4e323ee346748d7a11c020a38e4feec377779533      1
    12 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
    13 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    14 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    15 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
    16 0x621cf04ba3c8316cfcbb027c798b8f93d8f2ec93      1
    17 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    18 0x6c21023560fdd4609de27c13ddc0471a6a6669d2      1
    19 0x6c5e59db14999d19745ac43b2f56677f445ec996      1
    20 0x6d6e91f00688275db669408369263cd58664b127      1
    21 0x7399bf2ebd6b6b4cd1211304d2e9c0b3d8fead5b      1
    22 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    23 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    24 0x864a2b0f12b90a8dfd38f5e93ac6dc169dc0df5c      1
    25 0x8c80312a069158fd944e05a853060090c74f2dac      1
    26 0x8c9d9fc19515b544f84b597af87da1f7d60c8646      1
    27 0x8d6d452d5d4eb29d09f20c26446c1d45e1ee82e5      1
    28 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    29 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    30 0x9419ece5ebcb9cf0d261801a582cda1d5349cb5d      1
    31 0x973231cf471f768f0939042e0591da1b15f4799f      1
    32 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    33 0xbbbbb912acb75a4756e7f480e40b2e7e94a7b8cc      1
    34 0xbcd30018fe0357d226ed7d7b26e3ff0dc94d8f53      1
    35 0xbddf499207d29e920c0500642567b43238b30fd3      1
    36 0xc78af48fcff8bd8bddf94ab198e5e6e25a828dc4      1
    37 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    38 0xc8f970c572d5969888e4d3cfb4380ad485e843ad      1
    39 0xe4100c6331483d4dd192e30f240e86ae58b287b7      1
    40 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    41 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    42 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    43 0xf05155f792819710da259c103d30e4b70178ea9f      1
    44 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    45 0xfd3384ad230e4dbecb2b13e75b755c873d30f1b2      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 36 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     2 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
     3 0x09e557999346e9ddf77c74ab449ff45370c69564      1
     4 0x0d87718e2a629c18d5bb4fea1ab913200b94b8aa      1
     5 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     6 0x1641964aed9635ab1f8ece9294142d6ab4ff81a5      1
     7 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     8 0x229fd4fc7ca05e61d626ea0d8fe2622ab01fcf6f      1
     9 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
    10 0x3cbf4ef353ab30627eed997da3b707668f8c668d      1
    11 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
    12 0x46c3adc31f3c8b8ee11b6468b7c31a21a2564611      1
    13 0x515c40f024739884da860d4fea9ba7d6b91485f3      1
    14 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
    15 0x582969a9b2016c8f151362edd78119c034498129      1
    16 0x698e9a51378ed9b292e1ea27cd2ba419aee0e4a4      1
    17 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    18 0x70c155e89bf28f4048e345741349119c88c7c40c      1
    19 0x73deb2cf82f241922294e50944d2d3830c5ca650      1
    20 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    21 0x8ec8b7e3ed62ef955bfc3f6f947e57c10b87b280      1
    22 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    23 0x9b38fde3b04ec8da2a48a273c5ef40cc2437a8ed      1
    24 0xa15b854360feb78d69aa3e68f66e9c9ec2355689      1
    25 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    26 0xa53fd5fdcc1599164df96ec6d60c572c673d7fb0      1
    27 0xa670e4ef947dd7607922f0716355566d558acd11      1
    28 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    29 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    30 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    31 0xd0c31e46c73b386432a3ddf768587df604dd52be      1
    32 0xd20432412ee610f60046fbf00f099fe8d2aa6ee0      1
    33 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    34 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    35 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    36 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 81 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01503dc708ce3c55017194847a07acb679d49f47      1
     2 0x02d61cc47cc9a19b2a023c33c094269d9af07862      1
     3 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
     4 0x09e557999346e9ddf77c74ab449ff45370c69564      1
     5 0x0d87718e2a629c18d5bb4fea1ab913200b94b8aa      1
     6 0x12c5e83175bff0a8e3aaf9536bc45e88d567491c      1
     7 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     8 0x1641964aed9635ab1f8ece9294142d6ab4ff81a5      1
     9 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
    10 0x229fd4fc7ca05e61d626ea0d8fe2622ab01fcf6f      1
    11 0x2733cbe186440511cd0aeef6dd37a67dac40899c      1
    12 0x2886254556ff9b1572845865a0270a3a7f52b4ff      1
    13 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
    14 0x309a1ef07c771edbeed38d66364d4f20ae537bdc      1
    15 0x335e19b3f5dcfa2e398ee7d5ff430a1f8ccc88b2      1
    16 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
    17 0x3a661a18cb0258119636dfdde098648c6ad5ba62      1
    18 0x3cbf4ef353ab30627eed997da3b707668f8c668d      1
    19 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
    20 0x3f526ece92247050be2c66cae5ab6c2cb8bc2a4b      1
    21 0x44d5a0b1e165a17c16d940ca774a28e62d4b44b0      1
    22 0x46c3adc31f3c8b8ee11b6468b7c31a21a2564611      1
    23 0x4e323ee346748d7a11c020a38e4feec377779533      1
    24 0x515c40f024739884da860d4fea9ba7d6b91485f3      1
    25 0x552f01d67b352aaa38bc675e30ced97f2451df63      1
    26 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
    27 0x582969a9b2016c8f151362edd78119c034498129      1
    28 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    29 0x5cbb31e39ca93ca13d8e9f686bf749b14076bc65      1
    30 0x61c888e1aeb86420d7ccb15705fa192d19fbf363      1
    31 0x621cf04ba3c8316cfcbb027c798b8f93d8f2ec93      1
    32 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    33 0x698e9a51378ed9b292e1ea27cd2ba419aee0e4a4      1
    34 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    35 0x6c21023560fdd4609de27c13ddc0471a6a6669d2      1
    36 0x6c5e59db14999d19745ac43b2f56677f445ec996      1
    37 0x6d6e91f00688275db669408369263cd58664b127      1
    38 0x70c155e89bf28f4048e345741349119c88c7c40c      1
    39 0x7399bf2ebd6b6b4cd1211304d2e9c0b3d8fead5b      1
    40 0x73deb2cf82f241922294e50944d2d3830c5ca650      1
    41 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    42 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    43 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    44 0x864a2b0f12b90a8dfd38f5e93ac6dc169dc0df5c      1
    45 0x8c80312a069158fd944e05a853060090c74f2dac      1
    46 0x8c9d9fc19515b544f84b597af87da1f7d60c8646      1
    47 0x8d6d452d5d4eb29d09f20c26446c1d45e1ee82e5      1
    48 0x8ec8b7e3ed62ef955bfc3f6f947e57c10b87b280      1
    49 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    50 0x91aca13b42ac160c9e79a3cdcbb42cf68c1f15b4      1
    51 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    52 0x9419ece5ebcb9cf0d261801a582cda1d5349cb5d      1
    53 0x973231cf471f768f0939042e0591da1b15f4799f      1
    54 0x9b38fde3b04ec8da2a48a273c5ef40cc2437a8ed      1
    55 0xa15b854360feb78d69aa3e68f66e9c9ec2355689      1
    56 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    57 0xa53fd5fdcc1599164df96ec6d60c572c673d7fb0      1
    58 0xa670e4ef947dd7607922f0716355566d558acd11      1
    59 0xa7ecfc1e491baeec24b33ed24bc84916af54f240      1
    60 0xb893ae8a1824604f6df4dfde52e2754921ba1a73      1
    61 0xbbbbb912acb75a4756e7f480e40b2e7e94a7b8cc      1
    62 0xbcd30018fe0357d226ed7d7b26e3ff0dc94d8f53      1
    63 0xbddf499207d29e920c0500642567b43238b30fd3      1
    64 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    65 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    66 0xc78af48fcff8bd8bddf94ab198e5e6e25a828dc4      1
    67 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    68 0xc8f970c572d5969888e4d3cfb4380ad485e843ad      1
    69 0xd0c31e46c73b386432a3ddf768587df604dd52be      1
    70 0xd20432412ee610f60046fbf00f099fe8d2aa6ee0      1
    71 0xe4100c6331483d4dd192e30f240e86ae58b287b7      1
    72 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    73 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    74 0xe75764a435f7831e8dfb960dbd85f01f78b84f47      1
    75 0xef34feb024f0a6f4d5cf3a7cd30bb0f041a6af58      1
    76 0xf05155f792819710da259c103d30e4b70178ea9f      1
    77 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    78 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    79 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    80 0xfd3384ad230e4dbecb2b13e75b755c873d30f1b2      1
    81 0xfd861c7e3bd41a83b50f9dcb054600d18a7dfb93      1

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
