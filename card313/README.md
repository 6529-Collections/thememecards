
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:333         Length:333         Min.   :1.000   Length:333        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.084                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:333        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21548869 # https://etherscan.io/block/21548869
block_hash <- "0xf91aac14e8725a3457cde0615100616fb2cb7ca95c04fe4a7f39fadd07471427"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4638 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","HallwaysofAlways","FracturedMemory","TerraSonalis","Visions","ShatteredSpectre","ADreamofMrNemo","Motus","Griseo"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("TheTaleofCruciforeEditions","WhispersEditions","HypnoiseEditions","TheTowerEditions","PendulumEditions","DeadEndEditions","IntermissionsEditions","SpiritsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","HallwaysofAlways","FracturedMemory","TerraSonalis","Visions","ShatteredSpectre","ADreamofMrNemo","Motus","Griseo","TheTaleofCruciforeEditions","WhispersEditions","HypnoiseEditions","TheTowerEditions","PendulumEditions","DeadEndEditions","IntermissionsEditions","SpiritsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 41 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     2 0x0d31b3d8831b33305bfec2676e19d981241a5f53      1
     3 0x12b03f8b233d29a3c8c749a4f7866f22a16ce230      1
     4 0x13069d6d6a14d3b5f99f7a55ebfe71d1205edd58      1
     5 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     6 0x1979cc4393fff5f55d46d8c81ab453a9b27dc319      1
     7 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     8 0x2c3b3fea418bf2d82ab2036ee3a35cd9648fab7a      1
     9 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
    10 0x3c189a5528986b828da80ef0cf6fc4ddb314a0d5      1
    11 0x40032e3476ea15c543b35ce3a7679864f2c240d7      1
    12 0x5932ff2c8b86dde43c9920beb1e58c278d9e0d42      1
    13 0x5ccfb4a3e07fe1eea3cf893df2ac35d9c2665fed      1
    14 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
    15 0x69ddc4c20425dfdedb958d88e334859bc97fd715      1
    16 0x6b216a545ba93fcec51e2ce255f6248f57ea980d      1
    17 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    18 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
    19 0x73cddf71b29954f393afab3e577370e7a4c5fb4b      1
    20 0x7b59793ad075e4ce1e35181054759c080b8d965d      1
    21 0x844aea3ad1c43d2e53948dea6e99983b68073a93      1
    22 0x953448062cbc361c4a49144bd1d43a294e4b61eb      1
    23 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    24 0x9d455affe240a25adc6cd75293ca6ab2a010ab0f      1
    25 0xab6ca2017548a170699890214bfd66583a0c1754      1
    26 0xad18dc2068c0e09d49d9289654829567d734bfee      1
    27 0xb4ccdfa84eceaea5e6cebe0807b018638618f55d      1
    28 0xbb031e0a0cbcfd021eb105b3400e9b1548000e4c      1
    29 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    30 0xcdd6d919a40e60962ca3c96445aac9e078da2ebb      1
    31 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    32 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    33 0xd4a5bf76d0300359efa7cfb428ec739e538a7fe3      1
    34 0xe1ff19610020d72930aee1b9c047e35b7fd0080e      1
    35 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    36 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    37 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    38 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    39 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    40 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    41 0xfb7c1d49e006eaddff2385c7ef8b0c5cf49d038a      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0179f6a0fe55245ff0f26b447ae60b7b388fe4c8      1
     2 0x0699978658affeb9add6ea1fb7bb95605b001ca1      1
     3 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
     4 0x0f535c627fb8255f4771082e7f1d0d18c8545260      1
     5 0x1184fa9a6f73a5789c0f0d0618c31e0fe7ef459b      1
     6 0x1c2f697e86defcae21e2b91b2b94b08e1505f773      1
     7 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     8 0x2510f670b15b92c4b3c62220fe4725d576d641a8      1
     9 0x2b9a677752998afbc99f1eed6e4b684ad5c6765a      1
    10 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
    11 0x2d62b7b4ab2aea16f53f78af3544088300040126      1
    12 0x3480d164ceba4eb8ea17e7f64810707a55e1f76b      1
    13 0x3c8744121b34e8bc108d92abca0a8bb75d7ca09c      1
    14 0x3cca02f925ee47a0333d3e40b98956479b580d89      1
    15 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
    16 0x49bb41acc0652b73d256cbfbd6d03a380b66c9b4      1
    17 0x49dd1856f13f703d68a72947e9d143366c15bb49      1
    18 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
    19 0x4bc1a7813efe373c3a2e6c6336fd41650e4ea5d6      1
    20 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
    21 0x5091081516c8695dfa1bfde3d7b33480c2cb2fca      1
    22 0x53201ff143aa386449ed2951c91af2b303512365      1
    23 0x64ced50a327703b5ba13f781e23b254416b82473      1
    24 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    25 0x6a89560c4acd7100a8bf099819d213261865a346      1
    26 0x6c211b644efc393cf0ab57a60eb0576d5d7ec5ea      1
    27 0x707bfeed11b63d3360de40313f01092aa9b365c1      1
    28 0x776ee1896bbcf967aeb1aac49b88e53868408c1f      1
    29 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
    30 0x7a71d126254d6ef6942e0b3d513dd5eb09d35c28      1
    31 0x7e23a0a9fd1105ca4b1dc9b7e0f0aab3b7071caa      1
    32 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    33 0x8057d9eb5c7c01a7636ebff97d12d716b82fa23c      1
    34 0x81745b7339d5067e82b93ca6bbad125f214525d3      1
    35 0x841412043d4e6b727bae0ea5ddb5cc73dcb699f2      1
    36 0x854a8a5fc02cf0472061e5f9bf909fde28fc5f0e      1
    37 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    38 0x8798e94c1991c62abe1f2b050c258e4aad9f9951      1
    39 0x8e838ae5e4528bdb1e6a6a102ab3f31def399c82      1
    40 0x92be9a3f9f5b3ba7d8d18430723d3c4f82539e74      1
    41 0x9712927bf5eeddde93746371c950dc16195ac063      1
    42 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
    43 0x9b08042e20dc4e883c41e89813be918d6729099a      1
    44 0xa22414e4af5767448624017c7e0151d22490412b      1
    45 0xa236ea2194d56d54874730dcd2de9a39a47f57f9      1
    46 0xa6fe71927b49147d8adf5526b7416e0adf824004      1
    47 0xae4c0384ce50297ba1f5251141e982fe7ad36829      1
    48 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
    49 0xb4d7264fd36ea63210d4647a174feb04956eca4e      1
    50 0xb717d37d306c68a53186e9a9f2d99e85cf4954f2      1
    51 0xba3284b2218af3693ef80e6df41a97a35d1ea705      1
    52 0xc0ae85cd4ba82cbc440b2f7633fcaf8f4b29bab1      1
    53 0xc1202b2da243467882439944885339f9fd71279c      1
    54 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    55 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
    56 0xca6983947efc848df45c9e84b279438e24727d2e      1
    57 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    58 0xd21837e6932d2d0576175b97d33c5049001181aa      1
    59 0xd2deddb6c02ef4732020069d753278b07ca432df      1
    60 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
    61 0xe481c5b15a7221e7d7a064df9fd74fc6f0ea19cb      1
    62 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    63 0xe7ed7fd989e569717a34c9f7ab910414bcad1541      1
    64 0xeaf6c384ebcd826dd6a5d8f305a40683a1cf77f0      1
    65 0xf07a68610ad27bfedabae3d2f16927f8e5be66b6      1
    66 0xf19b975ab5b1ab459ee989f1875c80fd24359b4e      1
    67 0xfe669af70d6a784d4c226f711a182e5d1917b111      1
    68 0xfe8d12bb904e686843e02f7ebc969651a80accde      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 109 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0179f6a0fe55245ff0f26b447ae60b7b388fe4c8      1
      2 0x0699978658affeb9add6ea1fb7bb95605b001ca1      1
      3 0x06a739bcb9d33bbad020cf3ebbb07c528bce2b53      1
      4 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
      5 0x0d31b3d8831b33305bfec2676e19d981241a5f53      1
      6 0x0f535c627fb8255f4771082e7f1d0d18c8545260      1
      7 0x1184fa9a6f73a5789c0f0d0618c31e0fe7ef459b      1
      8 0x12b03f8b233d29a3c8c749a4f7866f22a16ce230      1
      9 0x13069d6d6a14d3b5f99f7a55ebfe71d1205edd58      1
     10 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     11 0x1979cc4393fff5f55d46d8c81ab453a9b27dc319      1
     12 0x1c2f697e86defcae21e2b91b2b94b08e1505f773      1
     13 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     14 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     15 0x2510f670b15b92c4b3c62220fe4725d576d641a8      1
     16 0x2b9a677752998afbc99f1eed6e4b684ad5c6765a      1
     17 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     18 0x2c3b3fea418bf2d82ab2036ee3a35cd9648fab7a      1
     19 0x2d62b7b4ab2aea16f53f78af3544088300040126      1
     20 0x3480d164ceba4eb8ea17e7f64810707a55e1f76b      1
     21 0x35a958858d46bcd9984360911bbaae44f4ef8f50      1
     22 0x3c189a5528986b828da80ef0cf6fc4ddb314a0d5      1
     23 0x3c8744121b34e8bc108d92abca0a8bb75d7ca09c      1
     24 0x3cca02f925ee47a0333d3e40b98956479b580d89      1
     25 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
     26 0x40032e3476ea15c543b35ce3a7679864f2c240d7      1
     27 0x49bb41acc0652b73d256cbfbd6d03a380b66c9b4      1
     28 0x49dd1856f13f703d68a72947e9d143366c15bb49      1
     29 0x4b3dcc15a8ab43128210fe3327bc830c36a15541      1
     30 0x4bc1a7813efe373c3a2e6c6336fd41650e4ea5d6      1
     31 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     32 0x5091081516c8695dfa1bfde3d7b33480c2cb2fca      1
     33 0x53201ff143aa386449ed2951c91af2b303512365      1
     34 0x5932ff2c8b86dde43c9920beb1e58c278d9e0d42      1
     35 0x5ccfb4a3e07fe1eea3cf893df2ac35d9c2665fed      1
     36 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
     37 0x64ced50a327703b5ba13f781e23b254416b82473      1
     38 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     39 0x69ddc4c20425dfdedb958d88e334859bc97fd715      1
     40 0x6a89560c4acd7100a8bf099819d213261865a346      1
     41 0x6b216a545ba93fcec51e2ce255f6248f57ea980d      1
     42 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     43 0x6c211b644efc393cf0ab57a60eb0576d5d7ec5ea      1
     44 0x707bfeed11b63d3360de40313f01092aa9b365c1      1
     45 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
     46 0x73cddf71b29954f393afab3e577370e7a4c5fb4b      1
     47 0x776ee1896bbcf967aeb1aac49b88e53868408c1f      1
     48 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     49 0x7a71d126254d6ef6942e0b3d513dd5eb09d35c28      1
     50 0x7b59793ad075e4ce1e35181054759c080b8d965d      1
     51 0x7e23a0a9fd1105ca4b1dc9b7e0f0aab3b7071caa      1
     52 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     53 0x8057d9eb5c7c01a7636ebff97d12d716b82fa23c      1
     54 0x81745b7339d5067e82b93ca6bbad125f214525d3      1
     55 0x841412043d4e6b727bae0ea5ddb5cc73dcb699f2      1
     56 0x844aea3ad1c43d2e53948dea6e99983b68073a93      1
     57 0x854a8a5fc02cf0472061e5f9bf909fde28fc5f0e      1
     58 0x85a30ede5e46029457e20d55427ee8673fad7762      1
     59 0x8798e94c1991c62abe1f2b050c258e4aad9f9951      1
     60 0x8e838ae5e4528bdb1e6a6a102ab3f31def399c82      1
     61 0x92be9a3f9f5b3ba7d8d18430723d3c4f82539e74      1
     62 0x953448062cbc361c4a49144bd1d43a294e4b61eb      1
     63 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
     64 0x9712927bf5eeddde93746371c950dc16195ac063      1
     65 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
     66 0x9b08042e20dc4e883c41e89813be918d6729099a      1
     67 0x9d455affe240a25adc6cd75293ca6ab2a010ab0f      1
     68 0xa22414e4af5767448624017c7e0151d22490412b      1
     69 0xa236ea2194d56d54874730dcd2de9a39a47f57f9      1
     70 0xa6fe71927b49147d8adf5526b7416e0adf824004      1
     71 0xab6ca2017548a170699890214bfd66583a0c1754      1
     72 0xad18dc2068c0e09d49d9289654829567d734bfee      1
     73 0xae4c0384ce50297ba1f5251141e982fe7ad36829      1
     74 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
     75 0xb4ccdfa84eceaea5e6cebe0807b018638618f55d      1
     76 0xb4d7264fd36ea63210d4647a174feb04956eca4e      1
     77 0xb717d37d306c68a53186e9a9f2d99e85cf4954f2      1
     78 0xba3284b2218af3693ef80e6df41a97a35d1ea705      1
     79 0xbb031e0a0cbcfd021eb105b3400e9b1548000e4c      1
     80 0xc0ae85cd4ba82cbc440b2f7633fcaf8f4b29bab1      1
     81 0xc1202b2da243467882439944885339f9fd71279c      1
     82 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
     83 0xc643c9411a6b489e9833b16631140f42bbfcb6d1      1
     84 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
     85 0xca6983947efc848df45c9e84b279438e24727d2e      1
     86 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
     87 0xcdd6d919a40e60962ca3c96445aac9e078da2ebb      1
     88 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
     89 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
     90 0xd21837e6932d2d0576175b97d33c5049001181aa      1
     91 0xd2deddb6c02ef4732020069d753278b07ca432df      1
     92 0xd4a5bf76d0300359efa7cfb428ec739e538a7fe3      1
     93 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
     94 0xe1ff19610020d72930aee1b9c047e35b7fd0080e      1
     95 0xe481c5b15a7221e7d7a064df9fd74fc6f0ea19cb      1
     96 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
     97 0xe7ed7fd989e569717a34c9f7ab910414bcad1541      1
     98 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
     99 0xeaf6c384ebcd826dd6a5d8f305a40683a1cf77f0      1
    100 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    101 0xf07a68610ad27bfedabae3d2f16927f8e5be66b6      1
    102 0xf19b975ab5b1ab459ee989f1875c80fd24359b4e      1
    103 0xf207e69a7cc3b4a00fec3039891a77b3df8c3577      1
    104 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    105 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    106 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    107 0xfb7c1d49e006eaddff2385c7ef8b0c5cf49d038a      1
    108 0xfe669af70d6a784d4c226f711a182e5d1917b111      1
    109 0xfe8d12bb904e686843e02f7ebc969651a80accde      1

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
