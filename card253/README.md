
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:179         Length:179         Min.   : 1.000   Length:179        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.525                     
                                           3rd Qu.: 1.000                     
                                           Max.   :11.000                     
         name          
     Length:179        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20292969 # https://etherscan.io/block/20292969
block_hash <- "0xb99f4f0a940a081b36c083b21b843a865f75623ca82c789d3c8379167736b1af"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4426 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","3DREAMS","MemoriesOfTheFuture"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("LazaroLongingEditions","KnownOriginEditions","LazaroxSpecialsEditions","BittersweetJournalsEditions","EditionsxLazaro"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SuperRare2","3DREAMS","MemoriesOfTheFuture","LazaroLongingEditions","KnownOriginEditions","LazaroxSpecialsEditions","BittersweetJournalsEditions","EditionsxLazaro"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
     2 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     3 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
     4 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     5 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     6 0x660c56a3f42d9527324af8a51cbe78e8f2db17ae      1
     7 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     8 0x8a2c9f1ecdab5d32811f7daf495e89b4058fb233      1
     9 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    10 0xf159dc86d3989f76e3ee343bb30e672bc081bb88      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 104 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      3 0x03b8d4bbdbb2b1e469ba0d453703d57435b21b30      1
      4 0x04434c2cf7d7d90255348079e840b4d50a62588e      1
      5 0x0669d8c1184fdbd44e9f1dd547646b3e9dd96f73      1
      6 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
      7 0x0e7a408a775af29310970de51b59501e21eee87a      1
      8 0x135577ff665d0ca46ddc1b43d3abe3a36c3ce42d      1
      9 0x14de5ffba8b58036212695bc0396cf88ee6c3b3d      1
     10 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     11 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     12 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     13 0x1b30259ba46767238a061129eb1162672ae00ad1      1
     14 0x1c6e3c966488416aabb5a819597da7d07853270c      1
     15 0x1db4556a7ed70112c755fc5706bb4980fafddfdf      1
     16 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     17 0x1f9724f3054d8f9fd28349067f796e1491d7d1c9      1
     18 0x20ab4a36a64396f128a482a7b589740aeecf936c      1
     19 0x23145f55648b1399356b519e96fc369f85a1f82d      1
     20 0x240646ce546dad743bb7a1fc352e258679aaeb12      1
     21 0x2839476ff27306b43c24c30f7a8ee41cdbf9761b      1
     22 0x2accfbabe7789dd053d9c0f06ba1a77354ceef83      1
     23 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     24 0x2ca8856397f32cb981e8b72a5de28546ad8c8259      1
     25 0x2d5c46a34bf42b2296df0886812ebca22a0d49a6      1
     26 0x2ed2175669343c1bc2cc0bb9abcbbe381860c43f      1
     27 0x319af5203839970ccb04d2eda95c427941c498ae      1
     28 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     29 0x34433cb54d406d231cd511567f33ce1914fc888e      1
     30 0x35e3885ed5f7ff3937de0172bb6a6093de19113e      1
     31 0x373a630b3431bf2a53b7174d9d9a8e1f5568fc04      1
     32 0x38039da6bc26c101316af426cbf7f43834957c46      1
     33 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     34 0x3a730f192cfda073603496231672ee458b7a487d      1
     35 0x3ea0d0708768bb72596ade00117013f01564b04d      1
     36 0x46028de05f5fb0c415a63d73df9cb71450be8f48      1
     37 0x4688b6f3c384c3cffe95c92f599213de4a9fa9ca      1
     38 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     39 0x4979758ee93edb1ad2b846d1526dffac0ebf952a      1
     40 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
     41 0x4c726b57df44c4a59b797f174ba35339f8688a5d      1
     42 0x4d19d8c941383d0028cd81303a2beb6c1bd95c89      1
     43 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     44 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
     45 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
     46 0x4fff7a19e4ab4fcd5d643b79b26b2c8db5e63b9b      1
     47 0x534d1b43b61a79c4c8d968f6aba470708ffa8027      1
     48 0x56e507da02d59190a59824308d1f63d40e0278af      1
     49 0x5f20a4e2f51d3453867250733cfac2cf52a2815f      1
     50 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     51 0x5fbf0e11696421f4a8945bb8db634cbb1cd71b7e      1
     52 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     53 0x6263b9edc7a9ee22789ec45995d17d9d21b8a609      1
     54 0x685a4e566c8472d7ac06cc9feae08a4f2b7c8269      1
     55 0x72e48f58a1e77abbc7757d60ebb7f94ad1730839      1
     56 0x73a9a8932dc806696b32e45f5580963feff1b8ad      1
     57 0x74c7e104d699193d7cbeb8ae4b5e1174468e3aed      1
     58 0x75bf5309c8b1682bf32b3f897a44981ca17a338f      1
     59 0x7670700748cc4dc1102272b700bfad4d0b575edd      1
     60 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     61 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     62 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     63 0x8148c9a29368249127be6f970d87ac6ef75a98c2      1
     64 0x869c9009a0d8279b63d83040a1acc96a6ad8bf89      1
     65 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
     66 0x8ec916eddd4745b273fa1c8edeea06420e827213      1
     67 0x8f99a46f5fa4c133c48682ae9270326c02c3f82b      1
     68 0x906b8dd75e84a64ca6460c522424dfe685be9450      1
     69 0x931b5ad8d05dc53d543ac4e5827dc1127d53a740      1
     70 0x97a349bd702512599144b61f873c4dcc04af1cd8      1
     71 0x991f2ee7bccc5d8cb9bbd60d3d7118ef4a03538a      1
     72 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     73 0xa1a8a29c4dec5e676ad053c52fc011a98124dcae      1
     74 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
     75 0xa6f28242fde10bd04017646e893a4f4ee39e8b99      1
     76 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
     77 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
     78 0xb022aad48cc818e4eeac1da96820e8c092785152      1
     79 0xb06b51b34b9e1825bf964ac2c0ad3f118df9c1b8      1
     80 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
     81 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
     82 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     83 0xba2b0fe6f96d70a29ab2da1afeaecdc2208135c8      1
     84 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
     85 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
     86 0xc8fe8f3a13339aa7e8e64d301627a5117c308d2e      1
     87 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
     88 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
     89 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
     90 0xcd28cc2cafc7449c21d8d94c676a630b7bbb9522      1
     91 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
     92 0xd2d669e927189998b2fc08124f4ffe91a2486cb0      1
     93 0xd473142a93c38e56729d3342e589e51cc4a6021f      1
     94 0xd67da3d36085b966b1fd7f72dd8bd7e5dc630f7d      1
     95 0xd826aa5f8880f0ace6bb0fc70d4e1220c71f3dee      1
     96 0xd8ad39fb56624e0b54e29de732d20f90fd9a0278      1
     97 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
     98 0xe05e78b35c31b3bbff1382cef1aed97b9798ce09      1
     99 0xe44fe60806c676e13cc5951595076b77a7117d2a      1
    100 0xe5128691cdcd8c742bea0a2c88273aa4cf5ce25b      1
    101 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    102 0xfb40034905b34753b7d74223e496518d43548336      1
    103 0xfc05b97aad7aaea23f04b0e481f50006e0fe744c      1
    104 0xfdcd7184142909f30713693fe6f7e014b91f7815      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
      2 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      3 0x03b8d4bbdbb2b1e469ba0d453703d57435b21b30      1
      4 0x04434c2cf7d7d90255348079e840b4d50a62588e      1
      5 0x0669d8c1184fdbd44e9f1dd547646b3e9dd96f73      1
      6 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
      7 0x0e7a408a775af29310970de51b59501e21eee87a      1
      8 0x135577ff665d0ca46ddc1b43d3abe3a36c3ce42d      1
      9 0x14de5ffba8b58036212695bc0396cf88ee6c3b3d      1
     10 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     11 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     12 0x18faba48193595f040e11af39c6e3ec3f6ad7086      1
     13 0x1b30259ba46767238a061129eb1162672ae00ad1      1
     14 0x1c6e3c966488416aabb5a819597da7d07853270c      1
     15 0x1db4556a7ed70112c755fc5706bb4980fafddfdf      1
     16 0x1ec9b60aaf97dfa9073534b95bf81f0923e599a1      1
     17 0x1f9724f3054d8f9fd28349067f796e1491d7d1c9      1
     18 0x20ab4a36a64396f128a482a7b589740aeecf936c      1
     19 0x23145f55648b1399356b519e96fc369f85a1f82d      1
     20 0x240646ce546dad743bb7a1fc352e258679aaeb12      1
     21 0x2839476ff27306b43c24c30f7a8ee41cdbf9761b      1
     22 0x2accfbabe7789dd053d9c0f06ba1a77354ceef83      1
     23 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
     24 0x2ca8856397f32cb981e8b72a5de28546ad8c8259      1
     25 0x2d5c46a34bf42b2296df0886812ebca22a0d49a6      1
     26 0x2ed2175669343c1bc2cc0bb9abcbbe381860c43f      1
     27 0x2f5963b32f07418cffc805d4a77f1d0bb5e38a81      1
     28 0x319af5203839970ccb04d2eda95c427941c498ae      1
     29 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     30 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     31 0x34433cb54d406d231cd511567f33ce1914fc888e      1
     32 0x35e3885ed5f7ff3937de0172bb6a6093de19113e      1
     33 0x373a630b3431bf2a53b7174d9d9a8e1f5568fc04      1
     34 0x38039da6bc26c101316af426cbf7f43834957c46      1
     35 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     36 0x3a730f192cfda073603496231672ee458b7a487d      1
     37 0x3ea0d0708768bb72596ade00117013f01564b04d      1
     38 0x46028de05f5fb0c415a63d73df9cb71450be8f48      1
     39 0x4688b6f3c384c3cffe95c92f599213de4a9fa9ca      1
     40 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     41 0x4979758ee93edb1ad2b846d1526dffac0ebf952a      1
     42 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
     43 0x4c726b57df44c4a59b797f174ba35339f8688a5d      1
     44 0x4d19d8c941383d0028cd81303a2beb6c1bd95c89      1
     45 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     46 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
     47 0x4e514d9fcb58503dba5bb2b0f477792c03c8426d      1
     48 0x4fd4c35020cfd14f167bc1d888eb903d7a650768      1
     49 0x4fff7a19e4ab4fcd5d643b79b26b2c8db5e63b9b      1
     50 0x534d1b43b61a79c4c8d968f6aba470708ffa8027      1
     51 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
     52 0x56e507da02d59190a59824308d1f63d40e0278af      1
     53 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     54 0x5f20a4e2f51d3453867250733cfac2cf52a2815f      1
     55 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
     56 0x5fbf0e11696421f4a8945bb8db634cbb1cd71b7e      1
     57 0x62050220633afe524d98fe3fccadbeacc2b96110      1
     58 0x6263b9edc7a9ee22789ec45995d17d9d21b8a609      1
     59 0x660c56a3f42d9527324af8a51cbe78e8f2db17ae      1
     60 0x685a4e566c8472d7ac06cc9feae08a4f2b7c8269      1
     61 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     62 0x72e48f58a1e77abbc7757d60ebb7f94ad1730839      1
     63 0x73a9a8932dc806696b32e45f5580963feff1b8ad      1
     64 0x74c7e104d699193d7cbeb8ae4b5e1174468e3aed      1
     65 0x75bf5309c8b1682bf32b3f897a44981ca17a338f      1
     66 0x7670700748cc4dc1102272b700bfad4d0b575edd      1
     67 0x7a41821e9ad5ccefe77611e58eed762a5a4d7572      1
     68 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     69 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     70 0x8148c9a29368249127be6f970d87ac6ef75a98c2      1
     71 0x869c9009a0d8279b63d83040a1acc96a6ad8bf89      1
     72 0x8a2c9f1ecdab5d32811f7daf495e89b4058fb233      1
     73 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
     74 0x8ec916eddd4745b273fa1c8edeea06420e827213      1
     75 0x8f99a46f5fa4c133c48682ae9270326c02c3f82b      1
     76 0x906b8dd75e84a64ca6460c522424dfe685be9450      1
     77 0x931b5ad8d05dc53d543ac4e5827dc1127d53a740      1
     78 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
     79 0x97a349bd702512599144b61f873c4dcc04af1cd8      1
     80 0x991f2ee7bccc5d8cb9bbd60d3d7118ef4a03538a      1
     81 0x9e9c627329f324c6b07e4c28982696445a5dc404      1
     82 0xa1a8a29c4dec5e676ad053c52fc011a98124dcae      1
     83 0xa589954b768e6105e9f2e84a4015f259373a3a17      1
     84 0xa6f28242fde10bd04017646e893a4f4ee39e8b99      1
     85 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
     86 0xaf6461736ccf7756429a8e4c8db43d70026f8575      1
     87 0xb022aad48cc818e4eeac1da96820e8c092785152      1
     88 0xb06b51b34b9e1825bf964ac2c0ad3f118df9c1b8      1
     89 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
     90 0xb94480dad90c3b41a9c24d88f78533973e18fa75      1
     91 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     92 0xba2b0fe6f96d70a29ab2da1afeaecdc2208135c8      1
     93 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
     94 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
     95 0xc8fe8f3a13339aa7e8e64d301627a5117c308d2e      1
     96 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
     97 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
     98 0xccb8d38662984543ec4b54c47eefa73fe4df3649      1
     99 0xcd28cc2cafc7449c21d8d94c676a630b7bbb9522      1
    100 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
    101 0xd2d669e927189998b2fc08124f4ffe91a2486cb0      1
    102 0xd473142a93c38e56729d3342e589e51cc4a6021f      1
    103 0xd67da3d36085b966b1fd7f72dd8bd7e5dc630f7d      1
    104 0xd826aa5f8880f0ace6bb0fc70d4e1220c71f3dee      1
    105 0xd8ad39fb56624e0b54e29de732d20f90fd9a0278      1
    106 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
    107 0xe05e78b35c31b3bbff1382cef1aed97b9798ce09      1
    108 0xe44fe60806c676e13cc5951595076b77a7117d2a      1
    109 0xe5128691cdcd8c742bea0a2c88273aa4cf5ce25b      1
    110 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    111 0xf159dc86d3989f76e3ee343bb30e672bc081bb88      1
    112 0xfb40034905b34753b7d74223e496518d43548336      1
    113 0xfc05b97aad7aaea23f04b0e481f50006e0fe744c      1
    114 0xfdcd7184142909f30713693fe6f7e014b91f7815      1

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
