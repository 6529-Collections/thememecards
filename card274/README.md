
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:398         Length:398         Min.   :1.000   Length:398        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.013                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:398        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20646969 # https://etherscan.io/block/20646969
block_hash <- "0xc98f55ad4b575058ce0576d11bee700682fbff79c5cc8abcb84315a71d3c235d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4752 

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

allow_artist1    <- pick(snapshot, contracts=c("CATCHYOURDREAM"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PAWANGANDRIMUENGEditions","PEPEDAILYEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("CATCHYOURDREAM","PAWANGANDRIMUENGEditions","PEPEDAILYEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 1 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x646aedb1e877fa189b5aaa6c36009a8970be93ce      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02434d3db6dff6a6639c71e8aff4fd9bcff90084      1
     2 0x03299cc26ba3c3ee6696b718695e0b46fbac5daf      1
     3 0x0d3788ab5c4b420e0add0dc8f8fa48cb77ef44aa      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x13664e34c55a8682c45da19401b721094b5f37e9      1
     6 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     7 0x153214fca16c8cf75a46e01094394a3ecbff70ab      1
     8 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     9 0x164a8379198a8d7155a1f0eab9baa256078d378e      1
    10 0x1a59b9df94ec4ed462abb0494f2e7dad3f5e90a9      1
    11 0x1bb22b6e8e93840930bbbc7c1d00490678bde5ba      1
    12 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
    13 0x202e53e88aa60190857f30de4395d3d9614791e0      1
    14 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
    15 0x2219828f21142b99fc3d24340a91a4cef8996353      1
    16 0x23a2da513890fa7d2fa79aa8004c8202ce7fe82f      1
    17 0x2535707d432c35b3326738eea5e8b2441216cf2e      1
    18 0x2ba63aff9312d33e85ae81ec9db0152ac3dafcf6      1
    19 0x2cf84928261f655a47d04ec714d3bedf9375de46      1
    20 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
    21 0x309a9126a80077cafb5a5f5ce3a0e831e3b850bd      1
    22 0x317ca4f9807af7289f89f182cfd014ca1d1ad04a      1
    23 0x39d763077f1058c3eec01c9a7b7493b07c807dc9      1
    24 0x3c465de32bbaca3a6cda285b93417dd0ed60c7e7      1
    25 0x41225847e1e44dda729e7c76be84d9a5a385e3d0      1
    26 0x41b3c6952298b27b942e5c25cdc0d9d9763bcb80      1
    27 0x426a55bedd1a168c88fa7b1ed350b04462fab08e      1
    28 0x42c3c994efa321dc920fc7d850f122834944eb86      1
    29 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
    30 0x4306dd0c18b58e34e587003841d736a449392d49      1
    31 0x451e974a4ed9794d2e8869b00649711d8c18763f      1
    32 0x478bb542f7658d635abba67edb987806dff5b83d      1
    33 0x4893f283ad236f4a8c52cc5bf981bb81ad36cce9      1
    34 0x49fc0038fccd22a07a729c1e037b462e7b993f85      1
    35 0x4b2f8046fa3326b2517d437a5c61520d0747955b      1
    36 0x4cc74e62e75187301350e2855db7e05757d26e9d      1
    37 0x4cdb3e72aff9efdd49e130cf76419938eef08420      1
    38 0x5577d19fee0a4936174a23c1fde0b0dafd4cd64a      1
    39 0x575ae2013667b3e3c8e5636c38e846a185aad99b      1
    40 0x601a5e26e349ac66f2f03c91b60ca3b1de5c56ae      1
    41 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    42 0x65fb47ce9108b5c937caa3e9707fcb581cf14b0e      1
    43 0x66120e32be12fbe6ff56da77ea9b2208bdcad3bc      1
    44 0x73e3e415ac392d50b85e14ad39e4292345d87ed5      1
    45 0x762e6807dbac86d9d0f1093f3280d4df8dfd77ef      1
    46 0x7dad608e8d62a8326ac73aa5bd35000875c8171c      1
    47 0x85e4e6ef0d6357b48da6de546c2841851da886a2      1
    48 0x873440b72f96d02cf2b80890f111546416b8aeac      1
    49 0x8a8194afb110292642a98e712c4a453c57fc9e42      1
    50 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
    51 0x8dc290e89ee68bcd5ad8cc99f2db775632bc51b2      1
    52 0x8eb07e2f87198fe19961fb4ac8046e54639b0b83      1
    53 0x935d11916b54a9d2c9eba242454066cddfd6f508      1
    54 0x94c4ef6f9b7777b614360b97c3d8565b3b106741      1
    55 0x9571b14092c202c2e5827404a3f220372cd02f34      1
    56 0x965a59359f0b59e03d622b1c384a71aa18f77af3      1
    57 0x9b13a09f34795f0ef8bf7e175a91370e9c355720      1
    58 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    59 0xab6b0eb346947452c7feb7ad4f5b7aeb5b4eeca8      1
    60 0xb07952a55bf9c45c268f37c3631823df50ac721a      1
    61 0xb3b41365f9179ac0711727ade3e0879c0aaa273b      1
    62 0xbaa61d2a44e287d00633d65e469532fbe4595827      1
    63 0xbb3c07b71b3d37b0554cbae7b6739361b68e940b      1
    64 0xbc0d4052559fe8afc471385c287317b3ebf3b65a      1
    65 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    66 0xbde724c14f102873d544a18c2a408678d53cf855      1
    67 0xbfd0e91509ba27d375f965b70db06ccc28a50ae0      1
    68 0xc110b47a7d56b8ef2fa924be65db8d7860cf0a17      1
    69 0xc4464dc30eb53437a1e84f380f813f61ae7e174d      1
    70 0xcdfc963c08d0cd653ad6de3b485d15bda3173d5f      1
    71 0xd0ac50d9f7516be16e2449539394a3967bea03c7      1
    72 0xd3aee2deac7037d2a3375f3d04ae448530c661c0      1
    73 0xd6dcbf4b2eee9774facd9e3fec990d68bbf9bec3      1
    74 0xd80f0d03579d1cae085ba34c3e909c33e710a52d      1
    75 0xd9390b46a1749dee5325a490490491db9a826d1f      1
    76 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    77 0xddeaec88e4a183f5acc7d7cfd6f69e300bb6d455      1
    78 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    79 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    80 0xe3497b16ee2efd1d954ed88ca4f3c4c97fcf71bd      1
    81 0xe3a21de9a7a83ccd54d5f8ce5d3164e2fe2c4614      1
    82 0xe8b91e677e3bae3e71755e279d39da02a981fe71      1
    83 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    84 0xf20c9b18e1b8bbc063da9ef18005d5760cbf1876      1
    85 0xf2f2b54ea97004caf3de45222b49ca2abddbd232      1
    86 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    87 0xf5501aa67b218ef340b23da0e7bcec77e70cf716      1
    88 0xf78a1568bdb5f0c639243c9fb4f3429984015b7c      1
    89 0xfc7991b8d994ca7979807b72d979d63beebc9944      1
    90 0xfd87dec249cf8c5f166f2a4105d68d50a6b2b10a      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 91 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02434d3db6dff6a6639c71e8aff4fd9bcff90084      1
     2 0x03299cc26ba3c3ee6696b718695e0b46fbac5daf      1
     3 0x0d3788ab5c4b420e0add0dc8f8fa48cb77ef44aa      1
     4 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     5 0x13664e34c55a8682c45da19401b721094b5f37e9      1
     6 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     7 0x153214fca16c8cf75a46e01094394a3ecbff70ab      1
     8 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     9 0x164a8379198a8d7155a1f0eab9baa256078d378e      1
    10 0x1a59b9df94ec4ed462abb0494f2e7dad3f5e90a9      1
    11 0x1bb22b6e8e93840930bbbc7c1d00490678bde5ba      1
    12 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
    13 0x202e53e88aa60190857f30de4395d3d9614791e0      1
    14 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
    15 0x2219828f21142b99fc3d24340a91a4cef8996353      1
    16 0x23a2da513890fa7d2fa79aa8004c8202ce7fe82f      1
    17 0x2535707d432c35b3326738eea5e8b2441216cf2e      1
    18 0x2ba63aff9312d33e85ae81ec9db0152ac3dafcf6      1
    19 0x2cf84928261f655a47d04ec714d3bedf9375de46      1
    20 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
    21 0x309a9126a80077cafb5a5f5ce3a0e831e3b850bd      1
    22 0x317ca4f9807af7289f89f182cfd014ca1d1ad04a      1
    23 0x39d763077f1058c3eec01c9a7b7493b07c807dc9      1
    24 0x3c465de32bbaca3a6cda285b93417dd0ed60c7e7      1
    25 0x41225847e1e44dda729e7c76be84d9a5a385e3d0      1
    26 0x41b3c6952298b27b942e5c25cdc0d9d9763bcb80      1
    27 0x426a55bedd1a168c88fa7b1ed350b04462fab08e      1
    28 0x42c3c994efa321dc920fc7d850f122834944eb86      1
    29 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
    30 0x4306dd0c18b58e34e587003841d736a449392d49      1
    31 0x451e974a4ed9794d2e8869b00649711d8c18763f      1
    32 0x478bb542f7658d635abba67edb987806dff5b83d      1
    33 0x4893f283ad236f4a8c52cc5bf981bb81ad36cce9      1
    34 0x49fc0038fccd22a07a729c1e037b462e7b993f85      1
    35 0x4b2f8046fa3326b2517d437a5c61520d0747955b      1
    36 0x4cc74e62e75187301350e2855db7e05757d26e9d      1
    37 0x4cdb3e72aff9efdd49e130cf76419938eef08420      1
    38 0x5577d19fee0a4936174a23c1fde0b0dafd4cd64a      1
    39 0x575ae2013667b3e3c8e5636c38e846a185aad99b      1
    40 0x601a5e26e349ac66f2f03c91b60ca3b1de5c56ae      1
    41 0x646aedb1e877fa189b5aaa6c36009a8970be93ce      1
    42 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    43 0x65fb47ce9108b5c937caa3e9707fcb581cf14b0e      1
    44 0x66120e32be12fbe6ff56da77ea9b2208bdcad3bc      1
    45 0x73e3e415ac392d50b85e14ad39e4292345d87ed5      1
    46 0x762e6807dbac86d9d0f1093f3280d4df8dfd77ef      1
    47 0x7dad608e8d62a8326ac73aa5bd35000875c8171c      1
    48 0x85e4e6ef0d6357b48da6de546c2841851da886a2      1
    49 0x873440b72f96d02cf2b80890f111546416b8aeac      1
    50 0x8a8194afb110292642a98e712c4a453c57fc9e42      1
    51 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
    52 0x8dc290e89ee68bcd5ad8cc99f2db775632bc51b2      1
    53 0x8eb07e2f87198fe19961fb4ac8046e54639b0b83      1
    54 0x935d11916b54a9d2c9eba242454066cddfd6f508      1
    55 0x94c4ef6f9b7777b614360b97c3d8565b3b106741      1
    56 0x9571b14092c202c2e5827404a3f220372cd02f34      1
    57 0x965a59359f0b59e03d622b1c384a71aa18f77af3      1
    58 0x9b13a09f34795f0ef8bf7e175a91370e9c355720      1
    59 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    60 0xab6b0eb346947452c7feb7ad4f5b7aeb5b4eeca8      1
    61 0xb07952a55bf9c45c268f37c3631823df50ac721a      1
    62 0xb3b41365f9179ac0711727ade3e0879c0aaa273b      1
    63 0xbaa61d2a44e287d00633d65e469532fbe4595827      1
    64 0xbb3c07b71b3d37b0554cbae7b6739361b68e940b      1
    65 0xbc0d4052559fe8afc471385c287317b3ebf3b65a      1
    66 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    67 0xbde724c14f102873d544a18c2a408678d53cf855      1
    68 0xbfd0e91509ba27d375f965b70db06ccc28a50ae0      1
    69 0xc110b47a7d56b8ef2fa924be65db8d7860cf0a17      1
    70 0xc4464dc30eb53437a1e84f380f813f61ae7e174d      1
    71 0xcdfc963c08d0cd653ad6de3b485d15bda3173d5f      1
    72 0xd0ac50d9f7516be16e2449539394a3967bea03c7      1
    73 0xd3aee2deac7037d2a3375f3d04ae448530c661c0      1
    74 0xd6dcbf4b2eee9774facd9e3fec990d68bbf9bec3      1
    75 0xd80f0d03579d1cae085ba34c3e909c33e710a52d      1
    76 0xd9390b46a1749dee5325a490490491db9a826d1f      1
    77 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    78 0xddeaec88e4a183f5acc7d7cfd6f69e300bb6d455      1
    79 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    80 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    81 0xe3497b16ee2efd1d954ed88ca4f3c4c97fcf71bd      1
    82 0xe3a21de9a7a83ccd54d5f8ce5d3164e2fe2c4614      1
    83 0xe8b91e677e3bae3e71755e279d39da02a981fe71      1
    84 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    85 0xf20c9b18e1b8bbc063da9ef18005d5760cbf1876      1
    86 0xf2f2b54ea97004caf3de45222b49ca2abddbd232      1
    87 0xf4aa421960cce8cf25190af71acc6949e848fe46      1
    88 0xf5501aa67b218ef340b23da0e7bcec77e70cf716      1
    89 0xf78a1568bdb5f0c639243c9fb4f3429984015b7c      1
    90 0xfc7991b8d994ca7979807b72d979d63beebc9944      1
    91 0xfd87dec249cf8c5f166f2a4105d68d50a6b2b10a      1

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
