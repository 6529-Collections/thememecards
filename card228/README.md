
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:119         Length:119         Min.   : 1.000   Length:119        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.395                     
                                           3rd Qu.: 1.000                     
                                           Max.   :31.000                     
         name          
     Length:119        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19795269 # https://etherscan.io/block/19795269
block_hash <- "0x41cd67497e0c82a0a0f24cce7056bc0490c1c73d378ef542996b56276dd97d62"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4521 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","MakersPlace","WorldofWitch"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("ETHEREALWORLDSEditions","RajaNandepuOpenEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","MakersPlace","WorldofWitch","ETHEREALWORLDSEditions","RajaNandepuOpenEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 26 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     2 0x0bfcb71d35981749f0de041362465cf9b4ae94a9      1
     3 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
     4 0x2c474fed08b074fdad8a9977a5fdce69d5a97bf3      1
     5 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
     6 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     7 0x3b875f957e6aad80edb28a46e370e57c50ce04b3      1
     8 0x3bf5616a2336bc63a6182c631a98028070c96efb      1
     9 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    10 0x60282772ab3e7f9e71b6eabd698ce4de82a18776      1
    11 0x614c3ce52760343fccb7d908eda39f114620a1e7      1
    12 0x63c89c7212df4e3ffc8c6d9ed8014dd3a2d9fa5c      1
    13 0x6dde7372072036eefaf880600dee87d5019ad2d2      1
    14 0x6e185d23e259c942899f55fe801d33b61033a5a4      1
    15 0x71ed56a9f862d80a84d5b40939a8b226f48226e6      1
    16 0x7d4da7e8f40305a2d970a276e5180b149698a7ff      1
    17 0x8f4f373e9c2cd1c7e1158234fb42e48ebe6b7485      1
    18 0x9075b54df817a1631ce3c496bd1fcd7a76c7fe99      1
    19 0x9d3f2735904881ea4f1e1097a7ce07e503fab0d5      1
    20 0xaed030182cb9d62e0cd6d8cce9cc0d7583d0543a      1
    21 0xafac83e20f79038738debdfc75bd6f0f4e2b3e0e      1
    22 0xb8d068fa0936490290f855f44d6949c70dcda35e      1
    23 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    24 0xd890e836f1e276a6cc7ad1612066f41890a46f89      1
    25 0xf86780ede26501cfc4649b4c1d7629e68a10f314      1
    26 0xfc3a1c70ea5efe506ac20d77e01e6308705be750      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 59 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0320de3378dcde180758ad2d41c0e1c6dcbb441d      1
     2 0x07a846a3fec9ee15bc74a4de83724c247838790a      1
     3 0x0ba9d9bf30a86945e3218bfcbca346c5d7f81838      1
     4 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     5 0x1d4752ec4aaaf60d0d6817fed1d2784fe2080218      1
     6 0x236511e76f6d333486926a446a791c2326e628a7      1
     7 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     8 0x3325204f7af94091a2d3e7b572e0593452aaeb86      1
     9 0x3347fee5910775b1c4b164d411dc517d7499a68a      1
    10 0x362cf4be53248d0872ccbfba570254c08e2f3d09      1
    11 0x37d1a8fcd1da37dadc564041e4707daa73162a88      1
    12 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
    13 0x3b86a9b8b98ff8056c5ef596a5b0ce61811097d0      1
    14 0x409c9f9dc413d7b026c677512aae81323b2c76a9      1
    15 0x475226b2a2b68401ed0472761ce8d4cdc09d2999      1
    16 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    17 0x4a32a2a2640abad3d4fe15100afc625355733077      1
    18 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
    19 0x5158b779e35f25e35857c93dd0630cae1bf09b77      1
    20 0x5256bdd9578dd8624affb33ef0168765dc3e4857      1
    21 0x53a1ff1d3a863bdf7e07f20c4ad21667865a59cd      1
    22 0x5641ae487adcafe5bc50fca8110b4df69b31c6e3      1
    23 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    24 0x580f9d192e2a487d419955ee4c59c78279dab8e3      1
    25 0x5ea1d56d0dde1ca5b50c277275855f69edefa169      1
    26 0x609e121163e0da4415adb0f61a1dd07f1a90fb98      1
    27 0x65b89f14c1aadd7e24dd0bd1ca080ce964e1237e      1
    28 0x6754efbfd9bdce63faf8fd6d3372fd12a7cf1c7e      1
    29 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
    30 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
    31 0x73050f3368bfc79fd57f7fae54dcb00cb9e68774      1
    32 0x7e3e126ee75b008518794fcf312cafd54248c4f1      1
    33 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    34 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    35 0x814757b1515bb6e07f66eeb4feb0e922abaddd97      1
    36 0x81ee84d1b0831ef72594af2a0c218e5d04f7e4cb      1
    37 0x8f4177caeebc7b25ac4bbab7e5f38830237ccb3b      1
    38 0x947a0b406122ee65d6e6439f489b9015dbc97262      1
    39 0x9627823b76decf20a2f422f53d765565e40f1e1d      1
    40 0x9711b1c5097920a8023e7775650fdd1cd85752c5      1
    41 0x9cfb24dd0368da75fd35dcd3c2a2883cb30ad272      1
    42 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    43 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    44 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    45 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    46 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
    47 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    48 0xca8745eebd47d93f8bdd7e149dee2b0b6454b84d      1
    49 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    50 0xd1b856ee12bd00922cae8dd86ab068f8c0f95224      1
    51 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    52 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    53 0xdfbdb9b9174862ecb1010c39ca72409c1d63b18f      1
    54 0xe6d3c0fb35738f1c8ce362c5d6f90b5f65fa7eb0      1
    55 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    56 0xefe05247f86b53280b19c9a0dff6ba102da91f57      1
    57 0xf986a622f19bde7776e96927c77369c0fd843950      1
    58 0xf9ba5a5ec068a347a021efd32977e49a2724feb0      1
    59 0xff53ddc9e7cc87a21f19e10858edcba53b009832      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 85 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0320de3378dcde180758ad2d41c0e1c6dcbb441d      1
     2 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     3 0x07a846a3fec9ee15bc74a4de83724c247838790a      1
     4 0x0ba9d9bf30a86945e3218bfcbca346c5d7f81838      1
     5 0x0bfcb71d35981749f0de041362465cf9b4ae94a9      1
     6 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     7 0x1d4752ec4aaaf60d0d6817fed1d2784fe2080218      1
     8 0x236511e76f6d333486926a446a791c2326e628a7      1
     9 0x25fa06431225d7acb36d59de8b7cf57b609a9edf      1
    10 0x2c474fed08b074fdad8a9977a5fdce69d5a97bf3      1
    11 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    12 0x33033761bb4305e48eae6d1a8412eb5175373ecc      1
    13 0x3325204f7af94091a2d3e7b572e0593452aaeb86      1
    14 0x3347fee5910775b1c4b164d411dc517d7499a68a      1
    15 0x362cf4be53248d0872ccbfba570254c08e2f3d09      1
    16 0x37d1a8fcd1da37dadc564041e4707daa73162a88      1
    17 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    18 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
    19 0x3b86a9b8b98ff8056c5ef596a5b0ce61811097d0      1
    20 0x3b875f957e6aad80edb28a46e370e57c50ce04b3      1
    21 0x3bf5616a2336bc63a6182c631a98028070c96efb      1
    22 0x409c9f9dc413d7b026c677512aae81323b2c76a9      1
    23 0x475226b2a2b68401ed0472761ce8d4cdc09d2999      1
    24 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    25 0x4a32a2a2640abad3d4fe15100afc625355733077      1
    26 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
    27 0x5158b779e35f25e35857c93dd0630cae1bf09b77      1
    28 0x5256bdd9578dd8624affb33ef0168765dc3e4857      1
    29 0x5294057157f9c4c4c63850644c9494d3e63f0b06      1
    30 0x53a1ff1d3a863bdf7e07f20c4ad21667865a59cd      1
    31 0x5641ae487adcafe5bc50fca8110b4df69b31c6e3      1
    32 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    33 0x580f9d192e2a487d419955ee4c59c78279dab8e3      1
    34 0x5ea1d56d0dde1ca5b50c277275855f69edefa169      1
    35 0x60282772ab3e7f9e71b6eabd698ce4de82a18776      1
    36 0x609e121163e0da4415adb0f61a1dd07f1a90fb98      1
    37 0x614c3ce52760343fccb7d908eda39f114620a1e7      1
    38 0x63c89c7212df4e3ffc8c6d9ed8014dd3a2d9fa5c      1
    39 0x65b89f14c1aadd7e24dd0bd1ca080ce964e1237e      1
    40 0x6754efbfd9bdce63faf8fd6d3372fd12a7cf1c7e      1
    41 0x6afbba8699e53d47bb986dda1dd39c73635cf73f      1
    42 0x6dde7372072036eefaf880600dee87d5019ad2d2      1
    43 0x6e185d23e259c942899f55fe801d33b61033a5a4      1
    44 0x709415e0d03beb6baf26487ea9d552da35133ddb      1
    45 0x71ed56a9f862d80a84d5b40939a8b226f48226e6      1
    46 0x73050f3368bfc79fd57f7fae54dcb00cb9e68774      1
    47 0x7d4da7e8f40305a2d970a276e5180b149698a7ff      1
    48 0x7e3e126ee75b008518794fcf312cafd54248c4f1      1
    49 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    50 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    51 0x814757b1515bb6e07f66eeb4feb0e922abaddd97      1
    52 0x81ee84d1b0831ef72594af2a0c218e5d04f7e4cb      1
    53 0x8f4177caeebc7b25ac4bbab7e5f38830237ccb3b      1
    54 0x8f4f373e9c2cd1c7e1158234fb42e48ebe6b7485      1
    55 0x9075b54df817a1631ce3c496bd1fcd7a76c7fe99      1
    56 0x947a0b406122ee65d6e6439f489b9015dbc97262      1
    57 0x9627823b76decf20a2f422f53d765565e40f1e1d      1
    58 0x9711b1c5097920a8023e7775650fdd1cd85752c5      1
    59 0x9cfb24dd0368da75fd35dcd3c2a2883cb30ad272      1
    60 0x9d3f2735904881ea4f1e1097a7ce07e503fab0d5      1
    61 0xa27be4084d7548d8019931877dd9bb75cc028696      1
    62 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    63 0xac4aa20a4faf40ae000c08d4e12b16ea081863a1      1
    64 0xaed030182cb9d62e0cd6d8cce9cc0d7583d0543a      1
    65 0xafac83e20f79038738debdfc75bd6f0f4e2b3e0e      1
    66 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    67 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
    68 0xb8d068fa0936490290f855f44d6949c70dcda35e      1
    69 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    70 0xca8745eebd47d93f8bdd7e149dee2b0b6454b84d      1
    71 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    72 0xd1b856ee12bd00922cae8dd86ab068f8c0f95224      1
    73 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    74 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    75 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    76 0xd890e836f1e276a6cc7ad1612066f41890a46f89      1
    77 0xdfbdb9b9174862ecb1010c39ca72409c1d63b18f      1
    78 0xe6d3c0fb35738f1c8ce362c5d6f90b5f65fa7eb0      1
    79 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    80 0xefe05247f86b53280b19c9a0dff6ba102da91f57      1
    81 0xf86780ede26501cfc4649b4c1d7629e68a10f314      1
    82 0xf986a622f19bde7776e96927c77369c0fd843950      1
    83 0xf9ba5a5ec068a347a021efd32977e49a2724feb0      1
    84 0xfc3a1c70ea5efe506ac20d77e01e6308705be750      1
    85 0xff53ddc9e7cc87a21f19e10858edcba53b009832      1

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
