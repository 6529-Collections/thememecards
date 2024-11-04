
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:116         Length:116         Min.   : 1.000   Length:116        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.155                     
                                           3rd Qu.: 1.000                     
                                           Max.   :19.000                     
         name          
     Length:116        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21096969 # https://etherscan.io/block/21096969
block_hash <- "0xbc3393d0d1f4a9505c3e83f66711cb7e123de6dd624b1495653f82fd001d985a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4554 

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

allow_artist1    <- pick(snapshot, contracts=c("NatureTwins","Foundation","InnerGarden","Levitation","Flowermania","JellyPals"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("Raribe"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("NatureTwins","Foundation","InnerGarden","Levitation","Flowermania","JellyPals","Raribe"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 83 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0ec7273a197b219af34df5a552794a1f0fddedbb      1
     3 0x0fa1f531a3a95b6f4014c07da138612d0721a130      1
     4 0x10bf6c24b2f3b730ddf724ecb9a8c658751a7da9      1
     5 0x11c597eee3cb6e28c52126db37175cad435402bd      1
     6 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     7 0x13119e06c532d4a474379d742b7ab8eb4523a73a      1
     8 0x1590886f763773ed89e2cb377ecc2545a8761924      1
     9 0x16b22a0c2e78f8c549d875ada23f32dcd5282a3b      1
    10 0x17d10cb975d2af519a6f2247806ccbbd0fb07438      1
    11 0x182d6114fff5922cbd22dbc8ac5d8f28a36185f6      1
    12 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
    13 0x1e08caaffb9169b8482d16b7cb673bb1bc190789      1
    14 0x1e7418f55940a15e36748258b35e22fcc1e9eb9f      1
    15 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
    16 0x2327dbcc6c2a3821a97c3d9004954970900441e2      1
    17 0x269d94e3eae94b5845971d1e2699e8e7b7bd55a7      1
    18 0x2a9aff0420b165ac421f5f2d8dcb6bae9675f168      1
    19 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
    20 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
    21 0x2fb55f58dd40af75dc019c2a3d09eb6878d376ea      1
    22 0x344a4c67ee8ae84b89bcde443f982c07ae5b1358      1
    23 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
    24 0x37af7cce2e846aa7daacc39b7001edd4b3eb028b      1
    25 0x38483d6e85e0fb044b3079209d93b289c2157ccd      1
    26 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    27 0x3aac7005fa7ab719e87538be7695ff592aa1cd84      1
    28 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    29 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    30 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
    31 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
    32 0x42185868b93cdb10f1ce2072bf81902aaab6d14f      1
    33 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    34 0x4a22910f4db3e7bf000eacfa06126d5db7f0efd5      1
    35 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
    36 0x58300539ee6ece54c9b49ee0e9be84c5c4bdd680      1
    37 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    38 0x5ac6b1ae54725d64f86da06c4a57ba583c68b3ac      1
    39 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
    40 0x5fb0b57cc661dfccba305d99ac46e251d1105c25      1
    41 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    42 0x6bae6cfacf0a3a5c54640761a9d294a7e6981616      1
    43 0x6e672528f7abc31bcec8bcf16c31aba3abebdad3      1
    44 0x7e8d7786f2e6c8a8c96a2d8adcc21200baf77e8c      1
    45 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    46 0x8611abf2e6fc4ba00e9e3d10cdb1e5382cde1f84      1
    47 0x876e78f1c7af7032a1e93ec78d7a8ee05cb144cb      1
    48 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    49 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    50 0x9192683edea0264fda574ddf101a362207169e2f      1
    51 0x922ec1109aa5b49822b72437d2d25f6ba749e585      1
    52 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    53 0x95d2cb5592bef123c7bfbb08972a4899274ab99a      1
    54 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    55 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    56 0xa5111579e6afa4d24493fd9d49ff5b8522a0276c      1
    57 0xa728da5f229b59fc8df19af2f1bcece8ef7a5bf9      1
    58 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    59 0xaae8b0adf77b41cc955709f779822b2da2b02408      1
    60 0xb0d1524b2517c522b6d16d8d906e8c6395e31bb3      1
    61 0xb2b40044be8d564911176c7c31fee1d083d6d7a7      1
    62 0xb69e7f59cb8bb0e1ba5487864a33572acb5467f9      1
    63 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
    64 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    65 0xbd521bb21bcd69efb700c6ee267d304e53493385      1
    66 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    67 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    68 0xcdd370f39bcb0c5622f591a3b7b824812625ac77      1
    69 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    70 0xd20bf69b592b9176e46ec229f82d7f1bf65fc9b2      1
    71 0xd8f7fe7f4b8b58fd04734a73a2d2f7b044ef5aa4      1
    72 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    73 0xe406b5c003898d0b9e7c42d44cb795cc55659dff      1
    74 0xead9c28bccd7f5d7bd1b9b3c1f054faa7b4a6c1e      1
    75 0xecd670ddd133db17d5c1ef873dc666391dbb4c9e      1
    76 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    77 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    78 0xf89b04ee9acaf9ce22e4a0f1fd492be6122a882b      1
    79 0xf9c6793c7a18ef3c7a2fb1d56761cf475c1238c1      1
    80 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    81 0xfbbc953b46ae6ae37392999707f996a868d40f20      1
    82 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    83 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 1 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x8adefe43c8b3429dfcf0436a5f54c40169f4b393      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 84 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0ec7273a197b219af34df5a552794a1f0fddedbb      1
     3 0x0fa1f531a3a95b6f4014c07da138612d0721a130      1
     4 0x10bf6c24b2f3b730ddf724ecb9a8c658751a7da9      1
     5 0x11c597eee3cb6e28c52126db37175cad435402bd      1
     6 0x12e7df22c809ac3fb4a4fcbda3b492ee9e5bf93d      1
     7 0x13119e06c532d4a474379d742b7ab8eb4523a73a      1
     8 0x1590886f763773ed89e2cb377ecc2545a8761924      1
     9 0x16b22a0c2e78f8c549d875ada23f32dcd5282a3b      1
    10 0x17d10cb975d2af519a6f2247806ccbbd0fb07438      1
    11 0x182d6114fff5922cbd22dbc8ac5d8f28a36185f6      1
    12 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
    13 0x1e08caaffb9169b8482d16b7cb673bb1bc190789      1
    14 0x1e7418f55940a15e36748258b35e22fcc1e9eb9f      1
    15 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
    16 0x2327dbcc6c2a3821a97c3d9004954970900441e2      1
    17 0x269d94e3eae94b5845971d1e2699e8e7b7bd55a7      1
    18 0x2a9aff0420b165ac421f5f2d8dcb6bae9675f168      1
    19 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
    20 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
    21 0x2fb55f58dd40af75dc019c2a3d09eb6878d376ea      1
    22 0x344a4c67ee8ae84b89bcde443f982c07ae5b1358      1
    23 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
    24 0x37af7cce2e846aa7daacc39b7001edd4b3eb028b      1
    25 0x38483d6e85e0fb044b3079209d93b289c2157ccd      1
    26 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    27 0x3aac7005fa7ab719e87538be7695ff592aa1cd84      1
    28 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    29 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    30 0x3ef5f421e155ed29e6fa815fd875bfcd5f22ecd1      1
    31 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
    32 0x42185868b93cdb10f1ce2072bf81902aaab6d14f      1
    33 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    34 0x4a22910f4db3e7bf000eacfa06126d5db7f0efd5      1
    35 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
    36 0x58300539ee6ece54c9b49ee0e9be84c5c4bdd680      1
    37 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    38 0x5ac6b1ae54725d64f86da06c4a57ba583c68b3ac      1
    39 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
    40 0x5fb0b57cc661dfccba305d99ac46e251d1105c25      1
    41 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    42 0x6bae6cfacf0a3a5c54640761a9d294a7e6981616      1
    43 0x6e672528f7abc31bcec8bcf16c31aba3abebdad3      1
    44 0x7e8d7786f2e6c8a8c96a2d8adcc21200baf77e8c      1
    45 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    46 0x8611abf2e6fc4ba00e9e3d10cdb1e5382cde1f84      1
    47 0x876e78f1c7af7032a1e93ec78d7a8ee05cb144cb      1
    48 0x8adefe43c8b3429dfcf0436a5f54c40169f4b393      1
    49 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    50 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    51 0x9192683edea0264fda574ddf101a362207169e2f      1
    52 0x922ec1109aa5b49822b72437d2d25f6ba749e585      1
    53 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    54 0x95d2cb5592bef123c7bfbb08972a4899274ab99a      1
    55 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    56 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    57 0xa5111579e6afa4d24493fd9d49ff5b8522a0276c      1
    58 0xa728da5f229b59fc8df19af2f1bcece8ef7a5bf9      1
    59 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    60 0xaae8b0adf77b41cc955709f779822b2da2b02408      1
    61 0xb0d1524b2517c522b6d16d8d906e8c6395e31bb3      1
    62 0xb2b40044be8d564911176c7c31fee1d083d6d7a7      1
    63 0xb69e7f59cb8bb0e1ba5487864a33572acb5467f9      1
    64 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
    65 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    66 0xbd521bb21bcd69efb700c6ee267d304e53493385      1
    67 0xc97705a8e073c9c0ba05d71aede57903a4a359f6      1
    68 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    69 0xcdd370f39bcb0c5622f591a3b7b824812625ac77      1
    70 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    71 0xd20bf69b592b9176e46ec229f82d7f1bf65fc9b2      1
    72 0xd8f7fe7f4b8b58fd04734a73a2d2f7b044ef5aa4      1
    73 0xe1d23eddf8f0f597a8582d86586999ffc41bb030      1
    74 0xe406b5c003898d0b9e7c42d44cb795cc55659dff      1
    75 0xead9c28bccd7f5d7bd1b9b3c1f054faa7b4a6c1e      1
    76 0xecd670ddd133db17d5c1ef873dc666391dbb4c9e      1
    77 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    78 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    79 0xf89b04ee9acaf9ce22e4a0f1fd492be6122a882b      1
    80 0xf9c6793c7a18ef3c7a2fb1d56761cf475c1238c1      1
    81 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    82 0xfbbc953b46ae6ae37392999707f996a868d40f20      1
    83 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    84 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1

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
