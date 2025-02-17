
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:159         Length:159         Min.   :1.000   Length:159        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.006                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:159        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21850069 # https://etherscan.io/block/21850069
block_hash <- "0x9ccea5132664531a79562c2d434571d13b430489b5e5aff6cfdb0cc4c3f6f21e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4643 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","ThePaintingLab","KEYSTONE","AerialSymphony","Foundation","KnownOrigin","Neoformism","Tormius","TalesofMythius"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("TormiusNumberedEditions","TormiusEditions","PEEKABOOOPENEDITIONBYTORMIUS","AIR23Editions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","ThePaintingLab","KEYSTONE","AerialSymphony","Foundation","KnownOrigin","Neoformism","Tormius","TalesofMythius","TormiusNumberedEditions","TormiusEditions","PEEKABOOOPENEDITIONBYTORMIUS","AIR23Editions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 17 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     2 0x34a8ab9a37f62f982d13041af3ef519e1a17340e      1
     3 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     4 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     5 0x4b27cedf4b6ef94098dcee773fb9e5fe83c1aaa8      1
     6 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
     7 0x6f9001efdf9f34ce25daeb32ca6a12e74007616b      1
     8 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     9 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
    10 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
    11 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    12 0xca11c6b5268d41ef7667c6e25eeaee9e5d890cad      1
    13 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    14 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    15 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    16 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    17 0xef70b950aecd42aa586fdc6141f2e25f44cf74b4      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 96 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     2 0x0688567b794a2a50909ca6fe92bcbe7a018556b6      1
     3 0x09a87eb0a7724387bfbff9728c146c8b184d609d      1
     4 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
     5 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     6 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     7 0x0f8e06470e8db860da48567eea9a142d18a8fc5d      1
     8 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     9 0x11adfa21ba61f68761301be2d1833884c7218c36      1
    10 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
    11 0x186031dedc522f75c430b21e1f9c4b28c1114bb4      1
    12 0x1a2ce0ea70d8f26435350e91cf46eb3278c7bb59      1
    13 0x1b286518f6ae3eda6111f0bf13d3409e2e5b9e94      1
    14 0x1fffac0101e4604a83bbb5bb0d783c965c2c6f21      1
    15 0x23c1a96bd2eafe831af384449b8802fc56b8d792      1
    16 0x26e7f147daa6eb33458ee531c6c22e34c7adc4c1      1
    17 0x3102b509a2beafa180e2dfd1cb6bf3e82a3fbe25      1
    18 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    19 0x3508407bb377124e4348ccaff3a740a8e1d1572d      1
    20 0x3976196e7f8569ba7994e79889ee2e8d33efd889      1
    21 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    22 0x3b7f083a2b7987c012934e5f9a268eb7a4d6710a      1
    23 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
    24 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
    25 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    26 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    27 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
    28 0x4b92e36ced303eeb59bd6c5dac555b2be13fe2b6      1
    29 0x4f7d60750ca31cc926e884049a644915bbbb0023      1
    30 0x560962063aa6b854dc04b83fe2e23ca02d885264      1
    31 0x56e507da02d59190a59824308d1f63d40e0278af      1
    32 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    33 0x5be798bf258ac5319e74428a66e635df491de2b2      1
    34 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    35 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    36 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
    37 0x6306d0cdfadd6095a313e8484275b6cc7036166c      1
    38 0x651171f6734696222c2b1fb8f54b0fcdc896cc8a      1
    39 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    40 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    41 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
    42 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
    43 0x6e2e8082f34f953befbd808b6363c0b00408e29a      1
    44 0x6fe936422846dcc13f3a8a9aad4be6ac3c15230c      1
    45 0x7056d3ff77c3ac9811bba12783ba3c51a40e6664      1
    46 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    47 0x782adafbf47a604f146af4a059908e946eae539f      1
    48 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
    49 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
    50 0x79ca64b3ee1784699850c07a53b2acd11ac0e12d      1
    51 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    52 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    53 0x815cf32608ccd66b205a519dcaf5b6f5dd19fced      1
    54 0x827e1537171289e51b26b982d530d2210d3676fa      1
    55 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
    56 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
    57 0x84f190d26ce9b2dfdc4b851cdeca634f0ef59e63      1
    58 0x86e806af0c7b364ea7580922996a33edf0251640      1
    59 0x8d2a43ff7e015c55c2be316a52bec176a4328a9b      1
    60 0x8da1d3a6a886ea0168c2d80df670eb6159f76220      1
    61 0x90d25a0bf3f4c8aca69d4fd7e8e7df9d4a89e7e8      1
    62 0x928881096f57781e91c35c8c7090cb0aeed2213b      1
    63 0x93dddb9ff5fe1d8d2d3ea6d74b191d3dfb4c5843      1
    64 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
    65 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
    66 0x983953213fd82e8d0bab2fc7c803ae5dff9d0de1      1
    67 0x9c87a1065994f156f0b7b87aaa8b3c5f7bd67e02      1
    68 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    69 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    70 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    71 0xad2bcb8dfaa66d67bd883f8c870e374027b42aab      1
    72 0xb68c97e8c1b2f032bafa999655f68341fee1705a      1
    73 0xb6cf777e3696a502107417265c92d1b075636a10      1
    74 0xb73d416e440e5cc1cd9e4d4e1973cd58367c36ae      1
    75 0xba0860b8e03ed32d9d71417b838d7c327bb2d1e5      1
    76 0xc77e83b862a7c1527c23b04834f694d17fd0637c      1
    77 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
    78 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    79 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
    80 0xd46e3aa949a76463f720ff29d5a58d8a936db8af      1
    81 0xd5914caaf0c1396d5fd548e1f205e62ef08b090d      1
    82 0xd598e0ccbbc94714422d544f1caf162234c296ba      1
    83 0xdb7cae8c85150de7702b55b3290f1e63008dbb69      1
    84 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    85 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    86 0xe96ead0ad625122677684448155ba4ae2700814d      1
    87 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    88 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    89 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    90 0xf1f96bc2efeac69051a995321cd2fa11a1bdc9a9      1
    91 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    92 0xf302c7ede4e4f1a2e6982fb8623991bb61ffafd5      1
    93 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    94 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    95 0xfc33080e56aad6d5856b46f206d2be9d70366ca5      1
    96 0xfc883e99a03fc4d32ee76f09b90674fe09f8debc      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 113 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      2 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      3 0x0688567b794a2a50909ca6fe92bcbe7a018556b6      1
      4 0x09a87eb0a7724387bfbff9728c146c8b184d609d      1
      5 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
      6 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
      7 0x0daac372398974373b29cb61c31deb11afa7ce23      1
      8 0x0f8e06470e8db860da48567eea9a142d18a8fc5d      1
      9 0x119fcd292b95551e8af472fc4721ac34cc5f48e5      1
     10 0x11adfa21ba61f68761301be2d1833884c7218c36      1
     11 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     12 0x186031dedc522f75c430b21e1f9c4b28c1114bb4      1
     13 0x1a2ce0ea70d8f26435350e91cf46eb3278c7bb59      1
     14 0x1b286518f6ae3eda6111f0bf13d3409e2e5b9e94      1
     15 0x1fffac0101e4604a83bbb5bb0d783c965c2c6f21      1
     16 0x23c1a96bd2eafe831af384449b8802fc56b8d792      1
     17 0x26e7f147daa6eb33458ee531c6c22e34c7adc4c1      1
     18 0x3102b509a2beafa180e2dfd1cb6bf3e82a3fbe25      1
     19 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     20 0x34a8ab9a37f62f982d13041af3ef519e1a17340e      1
     21 0x3508407bb377124e4348ccaff3a740a8e1d1572d      1
     22 0x3976196e7f8569ba7994e79889ee2e8d33efd889      1
     23 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     24 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     25 0x3b7f083a2b7987c012934e5f9a268eb7a4d6710a      1
     26 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     27 0x42cd7103b8c7b195516a72be6a0d65df71747fad      1
     28 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     29 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     30 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
     31 0x4ac69caa7bc279fec93db0d10793eb8516c7a9d1      1
     32 0x4b27cedf4b6ef94098dcee773fb9e5fe83c1aaa8      1
     33 0x4b92e36ced303eeb59bd6c5dac555b2be13fe2b6      1
     34 0x4f7d60750ca31cc926e884049a644915bbbb0023      1
     35 0x560962063aa6b854dc04b83fe2e23ca02d885264      1
     36 0x56e507da02d59190a59824308d1f63d40e0278af      1
     37 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
     38 0x5be798bf258ac5319e74428a66e635df491de2b2      1
     39 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
     40 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
     41 0x618c7671366caaab6701f06ae9925ae682c6bf97      1
     42 0x6306d0cdfadd6095a313e8484275b6cc7036166c      1
     43 0x651171f6734696222c2b1fb8f54b0fcdc896cc8a      1
     44 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     45 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     46 0x692997405c1cebbc56ff3416fa0b59ab97fac696      1
     47 0x6a290fd02a31c78a225b98f3d587d61f4aeba437      1
     48 0x6a682375545982da6d5c926dc65e3a8d360720e2      1
     49 0x6e2e8082f34f953befbd808b6363c0b00408e29a      1
     50 0x6f9001efdf9f34ce25daeb32ca6a12e74007616b      1
     51 0x6fe936422846dcc13f3a8a9aad4be6ac3c15230c      1
     52 0x7056d3ff77c3ac9811bba12783ba3c51a40e6664      1
     53 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     54 0x782adafbf47a604f146af4a059908e946eae539f      1
     55 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
     56 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     57 0x79ca64b3ee1784699850c07a53b2acd11ac0e12d      1
     58 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
     59 0x7be90f10f9c6cd02f32401c4929e5a5dbaa0a51b      1
     60 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     61 0x810b7d663f688b1d146c149a9d0718547b103a65      1
     62 0x815cf32608ccd66b205a519dcaf5b6f5dd19fced      1
     63 0x827e1537171289e51b26b982d530d2210d3676fa      1
     64 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
     65 0x848ba349b050fd8b703cc5fef2c85ae7d8350556      1
     66 0x84f190d26ce9b2dfdc4b851cdeca634f0ef59e63      1
     67 0x86e806af0c7b364ea7580922996a33edf0251640      1
     68 0x8d2a43ff7e015c55c2be316a52bec176a4328a9b      1
     69 0x8da1d3a6a886ea0168c2d80df670eb6159f76220      1
     70 0x90d25a0bf3f4c8aca69d4fd7e8e7df9d4a89e7e8      1
     71 0x928881096f57781e91c35c8c7090cb0aeed2213b      1
     72 0x93dddb9ff5fe1d8d2d3ea6d74b191d3dfb4c5843      1
     73 0x9523183432407ad8cc75f5b30a2be3ab035511b2      1
     74 0x9720dbe3f2716ea6fbf56ab4469b902e4acaa182      1
     75 0x983953213fd82e8d0bab2fc7c803ae5dff9d0de1      1
     76 0x9c87a1065994f156f0b7b87aaa8b3c5f7bd67e02      1
     77 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
     78 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
     79 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
     80 0xad2bcb8dfaa66d67bd883f8c870e374027b42aab      1
     81 0xb68c97e8c1b2f032bafa999655f68341fee1705a      1
     82 0xb6cf777e3696a502107417265c92d1b075636a10      1
     83 0xb73d416e440e5cc1cd9e4d4e1973cd58367c36ae      1
     84 0xba0860b8e03ed32d9d71417b838d7c327bb2d1e5      1
     85 0xc4d4125c83d1108df969dda998227fca80fd82dc      1
     86 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
     87 0xc77e83b862a7c1527c23b04834f694d17fd0637c      1
     88 0xca11c6b5268d41ef7667c6e25eeaee9e5d890cad      1
     89 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
     90 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
     91 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
     92 0xd45fc1cbc8237ed160f6a8dc2a1730484502d3e8      1
     93 0xd46e3aa949a76463f720ff29d5a58d8a936db8af      1
     94 0xd5914caaf0c1396d5fd548e1f205e62ef08b090d      1
     95 0xd598e0ccbbc94714422d544f1caf162234c296ba      1
     96 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
     97 0xdb7cae8c85150de7702b55b3290f1e63008dbb69      1
     98 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
     99 0xe1c9b7038a03dc898cd161827709a3c241af991e      1
    100 0xe681e5232da0c9ba5d28e6775bb3b6945eaf50f4      1
    101 0xe74419fda5425b88fe288923f5df60a5cda057be      1
    102 0xe96ead0ad625122677684448155ba4ae2700814d      1
    103 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    104 0xec63e88ba3d5482cbfa59872d2d889ba57796e17      1
    105 0xef70b950aecd42aa586fdc6141f2e25f44cf74b4      1
    106 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    107 0xf1f96bc2efeac69051a995321cd2fa11a1bdc9a9      1
    108 0xf297ffe8bd936dbbef991c913ea4a3c7bb2f3bb2      1
    109 0xf302c7ede4e4f1a2e6982fb8623991bb61ffafd5      1
    110 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    111 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    112 0xfc33080e56aad6d5856b46f206d2be9d70366ca5      1
    113 0xfc883e99a03fc4d32ee76f09b90674fe09f8debc      1

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
