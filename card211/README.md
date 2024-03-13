
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:332         Length:332         Min.   : 1.00   Length:332        
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.31                     
                                           3rd Qu.: 1.00                     
                                           Max.   :10.00                     
         name          
     Length:332        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19396969 # https://etherscan.io/block/19396969
block_hash <- "0xfb8ad61cfdc13e2882277e41c1dba92e5bfd23bc20baeca6af00b2f504ace5d0"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5041 

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

allow_artist1    <- pick(snapshot, contracts=c("IntrepidNights","IntrepidOcean","IntrepidOutback","IntrepidExtras","NorthernExposure","SuperReal","FireintheSky","LegacyContract","Foundation","IntrepidFalls","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("IntrepidMetropolisEditions","IntrepidMfersEditions","IntrepidExtrasEditions","LightningStrikesThriceEditions","Rarible"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("IntrepidNights","IntrepidOcean","IntrepidOutback","IntrepidExtras","NorthernExposure","SuperReal","FireintheSky","LegacyContract","Foundation","IntrepidFalls","MakersPlace","IntrepidMetropolisEditions","IntrepidMfersEditions","IntrepidExtrasEditions","LightningStrikesThriceEditions","Rarible"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 66 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00e87b69cf3686e028f96d14cd79ec30983e0f53      1
     2 0x024acdae4b00cf47430f4805ebff53b397f7ae83      1
     3 0x0282e055d3b2f7c0ad656952ac1bb989fb6d0086      1
     4 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
     5 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     6 0x0d08ad2ab7893c04ecb460cbb6822b11c9e8904a      1
     7 0x11360f0c5552443b33720a44408aba01a809905e      1
     8 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     9 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
    10 0x133a123687c6c4d12abcd18a8dacf2b7871d9548      1
    11 0x17b1cb1ad28e8e8b038139e95cf6223ee7e8b572      1
    12 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
    13 0x1800bb3bb01ec636480cb45c61f45ae34c97170d      1
    14 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    15 0x2b82ad50033363fffc3cf001743cbbbb83cdc11a      1
    16 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
    17 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    18 0x38a6133018714e4f21b914acace1cf7b41d4c574      1
    19 0x39fe6d7319433f4a7870644461aeeb21d9e2ae3c      1
    20 0x3a49309413793b32f6a308769220147fedbffa5f      1
    21 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
    22 0x3bd3ce01c82a12d7cff7c85a9e8bb27ae42fb548      1
    23 0x3d123ccbb151d403b9817be5505b1d0dba8ed576      1
    24 0x408125b99f2f17b90c7b30b2e9db7baa88e6b8d1      1
    25 0x40c839b831c90173dc7fbce49a25274a4688ddd9      1
    26 0x424b3adf010edde7c981873082c61f44a9fee413      1
    27 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
    28 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
    29 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
    30 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    31 0x59068075a799594db03c0255eed68e8e121155c8      1
    32 0x5959002cb524181d5526714c2804c3775212d823      1
    33 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
    34 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
    35 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    36 0x68a7ac13477aad590982293feeeb786a00276cf2      1
    37 0x6a0af60c1906cc788d301acfcbf4b8b54e4a9a10      1
    38 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    39 0x6e513ada916670389097752d05bf609d1246b4d2      1
    40 0x80d762e5a4dc2bc237f594a7f696b5e216067036      1
    41 0x8177598e08ee8199c160a48c7a0af31ab54bb59f      1
    42 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
    43 0x8497277c9339170a0420e86da8352e0c084624cd      1
    44 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    45 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    46 0x9187804eed28934cc59049297162554a592e96f0      1
    47 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
    48 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    49 0xad9e9c4e50efec42432f6aa70ff52f528da94105      1
    50 0xae72ac850878f7f1c033569289e2fd2378231e07      1
    51 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    52 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    53 0xc8dd40ba1bdb6a3f956904f02b14db24013b8b5d      1
    54 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    55 0xd050c97e76f882187c781feb2e74345227e2f710      1
    56 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    57 0xdb24f4bdf3f171f8f691cf9d2d4cb4e77675453b      1
    58 0xe659c74753c241943687d8931358a851f11312ff      1
    59 0xe91f0085f5c3e67c8ebd5c8f6e7f4d884452dbaa      1
    60 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    61 0xf217de9e1442b1f61cee9dac2a07bea96d83e06c      1
    62 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    63 0xf41125ed2a8c74d54d71f8b6454af03e6e3dcaf8      1
    64 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    65 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    66 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 99 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x040df97224a2d65df277ab4adb58596a42a9698c      1
     2 0x04400db7f33d5330f62c8dbd4840920adc273615      1
     3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     4 0x06f549f0e578018095e93325bfaccdae3ca21df3      1
     5 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     6 0x140ce4fc34487ac55838316373eeb87dd10b65c7      1
     7 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     8 0x1b5c7c62d2c4b3e65a9ca49a17c7ea1b6d9df696      1
     9 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
    10 0x21df37900c14bd8af6b12552a1ec2779fb978160      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x255148be130862531729937152d82c51142a5d56      1
    13 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
    14 0x2968da8a29d342e35dab40315638f22f425078ea      1
    15 0x2bf0246bdee6b027d2177ee84905c25b174fc622      1
    16 0x2eae4738fe75c88cf1d6521433d30cfd85961992      1
    17 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    18 0x31cd4492f926f3ba9df29d5fc5e2cdc5870250c1      1
    19 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
    20 0x3aad83fd218881e391b65fdac31dc3cfc1da8cde      1
    21 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
    22 0x3dca4687174473c6c3e28a954b4590389d418848      1
    23 0x3f1e2953ec44a6d73ab54e5c6a07b099e7dac7d2      1
    24 0x41146e2b9dfdfc47724a20cb86a15dc206c97e44      1
    25 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
    26 0x4c327ee3ae154220513cddd0292d166bdb414426      1
    27 0x512e63b6e620d4b413c20625c023165a3727acd6      1
    28 0x56b5822f18396c247f8065eb6d03b08ad0450469      1
    29 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
    30 0x6584b48ccebd9c3e4413a1dd4cdbc38b2500cdf3      1
    31 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
    32 0x6b93809e145d2301fb21d955e4241444f784dcd9      1
    33 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
    34 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    35 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    36 0x768057610a07396e05046af01e7c9b912e4a81f3      1
    37 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    38 0x7c2145e13c6917296d2e95bd5b1f5706c1a99f72      1
    39 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
    40 0x82d0efe3e244d97d763ea17b66dcaeaed13ba679      1
    41 0x8403f9abc432ea851e1e681605ee27ab2e2adda1      1
    42 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
    43 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
    44 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
    45 0x92f6d408a4f90c225252de2da371638137bf6a54      1
    46 0x94020ea06109f487ce45174cfa4da4513d38e098      1
    47 0x962ad4a1276d2f985f8afabd3da59bd32e5c36f2      1
    48 0x9773cc2b33c0791d1f35a93f6b895c6ede1feb54      1
    49 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    50 0x9cb79c20939d6ac3143910926754fbc67840be8a      1
    51 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
    52 0xa16977865ab1e2eeab1068e71890b36de4bf95f3      1
    53 0xa431d9ae84cce1c1c6d28f8258b5b95bab930210      1
    54 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    55 0xab0ac99d34b23dbfd106f539bd8a265275f28c87      1
    56 0xaf9cbe42d3fd186b5d0d04b0a1d424e6562fd7ba      1
    57 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    58 0xb3e37aebf26dc322857f66f5c889118ed6bfadb4      1
    59 0xb6cf777e3696a502107417265c92d1b075636a10      1
    60 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    61 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    62 0xb8aabda1486b40aa761fe3c643e3bdd21649e6a0      1
    63 0xbe0c0df368687e580fa2c3ad3c9412981f0273fb      1
    64 0xbfc3c19d3be0b0f54dca49fb136d08eea86a0229      1
    65 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    66 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    67 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    68 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    69 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    70 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    71 0xca6983947efc848df45c9e84b279438e24727d2e      1
    72 0xcadc7fcfd1c12da24be866d30be91b44cbff3914      1
    73 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    74 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    75 0xd04fff37bfefd4b2439322620262a25cc17d0b91      1
    76 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    77 0xd6ef6221989578889ef0916c046e2edd3a2b0523      1
    78 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    79 0xdbe41dbeabb006df451db3a03736bb802a843733      1
    80 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    81 0xe1c7580accbccc3490345ed5c6833ec170200599      1
    82 0xe3e06e9ec56ac1e7f3bced968b920d46ae1b460a      1
    83 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    84 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    85 0xe874ba46982d7cbab5931a50c2dc81a7aeb80344      1
    86 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    87 0xe9e7f10a129ed34be0cab9e1fb0c8a309d3526d9      1
    88 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    89 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    90 0xef7468c7044e6d732927d58ba535b2ad30799cbf      1
    91 0xf06faa3828566d9ef4ca78727d032ec519b07339      1
    92 0xf0cf0a85c08527d8207a3ef45c5dc5af38a61da5      1
    93 0xf4baba092bb9aaf76e0c03b856398b9ebed0819f      1
    94 0xf52a30142877e3b7d30f3f6bb2c6bc55b8888710      1
    95 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    96 0xf61f2de859eed38cde9f36fbdf3de03d84871f6e      1
    97 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    98 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    99 0xf9b16150b794a665f5348069751a1fc9e68de60c      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 165 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00e87b69cf3686e028f96d14cd79ec30983e0f53      1
      2 0x024acdae4b00cf47430f4805ebff53b397f7ae83      1
      3 0x0282e055d3b2f7c0ad656952ac1bb989fb6d0086      1
      4 0x03183fcd2d6a40b466375cdcafe2cd61d0cda659      1
      5 0x040df97224a2d65df277ab4adb58596a42a9698c      1
      6 0x04400db7f33d5330f62c8dbd4840920adc273615      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x06f549f0e578018095e93325bfaccdae3ca21df3      1
      9 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     10 0x0d08ad2ab7893c04ecb460cbb6822b11c9e8904a      1
     11 0x11360f0c5552443b33720a44408aba01a809905e      1
     12 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     13 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     14 0x12c4908dc21e88a8f268c3f363098eed251c1aa3      1
     15 0x133a123687c6c4d12abcd18a8dacf2b7871d9548      1
     16 0x140ce4fc34487ac55838316373eeb87dd10b65c7      1
     17 0x17b1cb1ad28e8e8b038139e95cf6223ee7e8b572      1
     18 0x17bb4076ab59e0ef20ad5a873ab4b5341bf01b78      1
     19 0x1800bb3bb01ec636480cb45c61f45ae34c97170d      1
     20 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     21 0x1b5c7c62d2c4b3e65a9ca49a17c7ea1b6d9df696      1
     22 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     23 0x21df37900c14bd8af6b12552a1ec2779fb978160      1
     24 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     25 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     26 0x255148be130862531729937152d82c51142a5d56      1
     27 0x263ab80489c5f2c1e777f0366b501f2675bd9526      1
     28 0x2968da8a29d342e35dab40315638f22f425078ea      1
     29 0x2b82ad50033363fffc3cf001743cbbbb83cdc11a      1
     30 0x2bf0246bdee6b027d2177ee84905c25b174fc622      1
     31 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
     32 0x2eae4738fe75c88cf1d6521433d30cfd85961992      1
     33 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     34 0x31cd4492f926f3ba9df29d5fc5e2cdc5870250c1      1
     35 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     36 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     37 0x38a6133018714e4f21b914acace1cf7b41d4c574      1
     38 0x39fe6d7319433f4a7870644461aeeb21d9e2ae3c      1
     39 0x3a49309413793b32f6a308769220147fedbffa5f      1
     40 0x3aad83fd218881e391b65fdac31dc3cfc1da8cde      1
     41 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     42 0x3bd3ce01c82a12d7cff7c85a9e8bb27ae42fb548      1
     43 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     44 0x3d123ccbb151d403b9817be5505b1d0dba8ed576      1
     45 0x3dca4687174473c6c3e28a954b4590389d418848      1
     46 0x3f1e2953ec44a6d73ab54e5c6a07b099e7dac7d2      1
     47 0x408125b99f2f17b90c7b30b2e9db7baa88e6b8d1      1
     48 0x40c839b831c90173dc7fbce49a25274a4688ddd9      1
     49 0x41146e2b9dfdfc47724a20cb86a15dc206c97e44      1
     50 0x424b3adf010edde7c981873082c61f44a9fee413      1
     51 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     52 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
     53 0x49ca963ef75bceba8e4a5f4ceab5fd326bef6123      1
     54 0x4c327ee3ae154220513cddd0292d166bdb414426      1
     55 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
     56 0x512e63b6e620d4b413c20625c023165a3727acd6      1
     57 0x53006f95def268f88dc1b8216654ab56f3afd052      1
     58 0x56b5822f18396c247f8065eb6d03b08ad0450469      1
     59 0x59068075a799594db03c0255eed68e8e121155c8      1
     60 0x5959002cb524181d5526714c2804c3775212d823      1
     61 0x599bf597fda4fc5e02a697906fb2b0b7ed9e964b      1
     62 0x61e935bb49ee40dae181d1e795d0deb84fbaadf6      1
     63 0x646ef8e3c8e79a10daf8f32621a3e468501cc3e4      1
     64 0x6584b48ccebd9c3e4413a1dd4cdbc38b2500cdf3      1
     65 0x66567f4ec7743e58713f44f083da3de78a52556a      1
     66 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
     67 0x68a7ac13477aad590982293feeeb786a00276cf2      1
     68 0x6a0af60c1906cc788d301acfcbf4b8b54e4a9a10      1
     69 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     70 0x6b93809e145d2301fb21d955e4241444f784dcd9      1
     71 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
     72 0x6e513ada916670389097752d05bf609d1246b4d2      1
     73 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     74 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     75 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     76 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
     77 0x7c2145e13c6917296d2e95bd5b1f5706c1a99f72      1
     78 0x7caa9f43822e288782e3e8797c8a16774c689b3d      1
     79 0x80d762e5a4dc2bc237f594a7f696b5e216067036      1
     80 0x8177598e08ee8199c160a48c7a0af31ab54bb59f      1
     81 0x82d0efe3e244d97d763ea17b66dcaeaed13ba679      1
     82 0x82f59af2695e1e9823773ef055614f54cc5616c4      1
     83 0x8403f9abc432ea851e1e681605ee27ab2e2adda1      1
     84 0x8497277c9339170a0420e86da8352e0c084624cd      1
     85 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
     86 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
     87 0x8c2dabfea8751dd7901e35a015dc6c943ffa0d7c      1
     88 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
     89 0x9187804eed28934cc59049297162554a592e96f0      1
     90 0x91e8be20d0dfb2293b97317e906223c1fa833a48      1
     91 0x92f6d408a4f90c225252de2da371638137bf6a54      1
     92 0x94020ea06109f487ce45174cfa4da4513d38e098      1
     93 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
     94 0x962ad4a1276d2f985f8afabd3da59bd32e5c36f2      1
     95 0x9773cc2b33c0791d1f35a93f6b895c6ede1feb54      1
     96 0x996e1d4ce3e1e558889832832004b2466153adbe      1
     97 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
     98 0x9cb79c20939d6ac3143910926754fbc67840be8a      1
     99 0xa0bd213eba4a6117d2ad12d86dee6876eb5f034c      1
    100 0xa16977865ab1e2eeab1068e71890b36de4bf95f3      1
    101 0xa431d9ae84cce1c1c6d28f8258b5b95bab930210      1
    102 0xa44b68fc590a2fd1cf48f2689a8aa2c8b62d2261      1
    103 0xab0ac99d34b23dbfd106f539bd8a265275f28c87      1
    104 0xad9e9c4e50efec42432f6aa70ff52f528da94105      1
    105 0xae72ac850878f7f1c033569289e2fd2378231e07      1
    106 0xaf9cbe42d3fd186b5d0d04b0a1d424e6562fd7ba      1
    107 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    108 0xb3e37aebf26dc322857f66f5c889118ed6bfadb4      1
    109 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    110 0xb6cf777e3696a502107417265c92d1b075636a10      1
    111 0xb775df51776d6767f8c2f5a14b667e300f60447f      1
    112 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    113 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    114 0xb8aabda1486b40aa761fe3c643e3bdd21649e6a0      1
    115 0xbe0c0df368687e580fa2c3ad3c9412981f0273fb      1
    116 0xbfc3c19d3be0b0f54dca49fb136d08eea86a0229      1
    117 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    118 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    119 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    120 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    121 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    122 0xc8dd40ba1bdb6a3f956904f02b14db24013b8b5d      1
    123 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    124 0xca6983947efc848df45c9e84b279438e24727d2e      1
    125 0xcadc7fcfd1c12da24be866d30be91b44cbff3914      1
    126 0xcb85cf75d53fceb37af86b475903817db2b99d17      1
    127 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    128 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    129 0xd04fff37bfefd4b2439322620262a25cc17d0b91      1
    130 0xd050c97e76f882187c781feb2e74345227e2f710      1
    131 0xd4baa5b1cfd74512a4243b09b544e6fb19832389      1
    132 0xd6ef6221989578889ef0916c046e2edd3a2b0523      1
    133 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    134 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    135 0xdb24f4bdf3f171f8f691cf9d2d4cb4e77675453b      1
    136 0xdbe41dbeabb006df451db3a03736bb802a843733      1
    137 0xde31b36631c7bd5883722cc8b7f11dc8fb061dc3      1
    138 0xe1c7580accbccc3490345ed5c6833ec170200599      1
    139 0xe3e06e9ec56ac1e7f3bced968b920d46ae1b460a      1
    140 0xe483f7c09b4707e423f621c0d7453fb2c7eabc03      1
    141 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    142 0xe659c74753c241943687d8931358a851f11312ff      1
    143 0xe874ba46982d7cbab5931a50c2dc81a7aeb80344      1
    144 0xe91f0085f5c3e67c8ebd5c8f6e7f4d884452dbaa      1
    145 0xe9b5a2529ad21454cb5e4d172dcb4bc501789463      1
    146 0xe9e7f10a129ed34be0cab9e1fb0c8a309d3526d9      1
    147 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    148 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    149 0xeebfd0ae2ce773db3dadad8d51ad59ec11567f16      1
    150 0xef7468c7044e6d732927d58ba535b2ad30799cbf      1
    151 0xf06faa3828566d9ef4ca78727d032ec519b07339      1
    152 0xf0cf0a85c08527d8207a3ef45c5dc5af38a61da5      1
    153 0xf217de9e1442b1f61cee9dac2a07bea96d83e06c      1
    154 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    155 0xf41125ed2a8c74d54d71f8b6454af03e6e3dcaf8      1
    156 0xf4baba092bb9aaf76e0c03b856398b9ebed0819f      1
    157 0xf52a30142877e3b7d30f3f6bb2c6bc55b8888710      1
    158 0xf5d4a3b9fb77d1d5600a10e0e1c4efd12f9be751      1
    159 0xf61f2de859eed38cde9f36fbdf3de03d84871f6e      1
    160 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    161 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1
    162 0xf9b16150b794a665f5348069751a1fc9e68de60c      1
    163 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    164 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1
    165 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1

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
