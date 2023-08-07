
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:18579       Length:18579       Min.   :1   Length:18579      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:18579      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17854469 # https://etherscan.io/block/17854469
block_hash <- "0xfcefd185520f401f961884e7b6abb5f0ea378ea8c2955a4334138a448cba59cf"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4709 

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
      1 0x0a8da356dc80da3a114d63e752cb42923b124019      1
      2 0x0c7fd3cefc75842987ca65a65c46c1faa823591d      1
      3 0x0dbfd05fd48d62056ae630bb714e5d9f2255b5cb      1
      4 0x0e66ebd413be5b4f82ef045f917da50a1324d223      1
      5 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
      6 0x112d62b9b1deaa943e8befb7270a9167c7b95838      1
      7 0x118aced72cd640407c1153d78c419215af9a5300      1
      8 0x12909009d651d40d6ae00b150db3107bc5654603      1
      9 0x13722725d7ed86b99d5f7fe5cb4226b6e42e7d41      1
     10 0x138961b0aa1305f43a350d56253abfa16c8076dc      1
     11 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     12 0x14baaa573906c24ec9dbdbd3d092fdb869b2d0f4      1
     13 0x17de8591163a8a9b6752efb9d05cb7290e887a6c      1
     14 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
     15 0x1fdf89dd0eba85603cbde7f9f5ce5d830ffc7643      1
     16 0x22236c0c2501d5ac173058ef0e151b5e21d116d6      1
     17 0x234c9dc1ff5caf605cc702efa51a4a6be5a66649      1
     18 0x28bd50226a8523ccfaa2ff2873eef032a7feeafa      1
     19 0x2be21793155e0d5b3dd715c348f7e712471e0873      1
     20 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     21 0x2f0a7eeca5747c9f8f2fd3257e42cc3a5a5d4cda      1
     22 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     23 0x3775669465352353f599f73586494747463f851b      1
     24 0x39079407a3be76fa77603858d0d8c753c9bb3925      1
     25 0x393effe40fe7ecd23446551d89443e9008aefbcc      1
     26 0x3d2e7f3e4253f57e5ae7638fd14cc8e16715ec76      1
     27 0x3d5f1ad033c4e7c6a6d433d50d694c488dcaebcf      1
     28 0x41955ab7d12f9f6c03de972b91d9b895d9c2eaf8      1
     29 0x439e4e4624a6ce7be5cd0ae90709c43ea0ef082c      1
     30 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     31 0x4b038c047f79e0cfd3762b9e8b4cb017709315fa      1
     32 0x5262ecd1843983833b6f5203c835f3053ecb1317      1
     33 0x57d5cd5d0fcf182fb46fe0ef7c0ec4cc3a4bde79      1
     34 0x5b4bb3c2bb6e2ec707cc39a86dd398c0a9f69add      1
     35 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     36 0x5dfa3092be17f441f85a4b3218a7675f0efcf9a3      1
     37 0x5f5b5a6b4661c2a4280984b3921ffd3d58bd42f1      1
     38 0x5fd22779c58f667cde1e9600576529984495604a      1
     39 0x5fea57b6495ef3f3bd38518e03905bee92d40388      1
     40 0x60a4f65da916f1d23970f460b5ee05ef84e76f55      1
     41 0x61b2889889591267229b8d05a7893ebe3822a8a0      1
     42 0x620910b020fd12e7b1ff8c0d52eda66bf31213d4      1
     43 0x63452484ede33876ea5e18f7c549cf183e31d352      1
     44 0x6de4d35786c9bf77d48d8bcb2e8df2f93bd83aed      1
     45 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
     46 0x7051d33aadadb8c7f83bc3d7efbb015cef3257df      1
     47 0x709c13074ed89a5d8efe996b4f1e2315d833f431      1
     48 0x7426b39865d11207b8f795b10d70843fc3289051      1
     49 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
     50 0x81eb6617ddc5753830b05f8d3c24da3ee72c4395      1
     51 0x8450cf769ccf7fd060936ba2b023ca8f9903f9c4      1
     52 0x84af839e3b2a2ba48b99da7ad663b0e519561332      1
     53 0x8c01de512c9fb73c0e9fccdc1ae4d2644db65030      1
     54 0x8c39a66ccc0f22ade61c59c7f21bb6bcee4921f1      1
     55 0x9127c9221b22ea3789c90383284c72dcd7d9b9fb      1
     56 0x912e844820eafe9a6c2df0615dcfea91ff32ce75      1
     57 0x938e202f15af8c8a782588262c3ed0565a7f9411      1
     58 0x94a5705a0f0927099638fb068c6e3ad77501d837      1
     59 0x951038bb372d16180c0afb0f46ab283059154004      1
     60 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
     61 0x9cd27b1142ca7dcf60167355791a4089463347ae      1
     62 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
     63 0xa3943ddbfb21ece9b47066f1aa200b1ebdcd3c45      1
     64 0xa7b32f0c55bfe631a9470a6b3312a6685ffd9f45      1
     65 0xa87428a3a4e3b9159e6303c89cf7723d7e237e96      1
     66 0xab90ddde2cf6a4753106a058acb4cc7412a58ae7      1
     67 0xaca470227af72b3db690804d628af2c8e97abd57      1
     68 0xace5b7a9f3a88eadfc4b529a24b7ea0f57859ff4      1
     69 0xb7edc97386c4889e70a28e469e9be078ee08cf43      1
     70 0xbe527aa383c9979d0133d27b8ea1c43b694d6f9a      1
     71 0xc01a5daafdf53e29a90f0c9637eb086ef8099069      1
     72 0xc05869d4aff23777690f2ffeb6794cb879355630      1
     73 0xc3556a0ef4ccfb13ae57489f1e685cd1dd16b372      1
     74 0xc4f94270364674f3bcce1487d98a959e72427dba      1
     75 0xc5af7c22ce278cc5ffc008a59612d4db8bd14c7c      1
     76 0xc60a2326a50606c84e00f2e4c1035c8d39fe467a      1
     77 0xc7380be6ec377a6e66f34fb89d04a21b3cce2e4f      1
     78 0xc8a4dcbb7a60ac12580b54ce3681b26a604c0fdd      1
     79 0xca85460d23bfb2e0079bab5aac84921d9d0762a4      1
     80 0xcb69c5838afd73317abe43ea23574ddf7a6e51b7      1
     81 0xcead3aa547dc6e7851795292ceb9d1690767ecf1      1
     82 0xd038d146dfab6b92df8ce2c92369f09375fc5b32      1
     83 0xd60b8d278a98e068eaf3508e95cf1b0089961288      1
     84 0xd76d392d17379c22ca5e49d167bad1dcaf7eba0d      1
     85 0xdb9488168d9dd32373a8f28791c22a141cf28afe      1
     86 0xdc27361494e95f566409f379abd08813e73468eb      1
     87 0xe003256f4cc1ae9d7545326efe8f05f12f066c81      1
     88 0xe50a5145483c22242be79bb231963b21cfd94041      1
     89 0xea05f13196bd437b92c682a86620f58b0f673cec      1
     90 0xea6a4c5514d847c1a474a7b61d305b7ec11f1373      1
     91 0xf4363de08eeff1ec9f8d74395e6b362eac339b5b      1
     92 0xf5851672ab7fc8729de7472dd273066b5e3b4de5      1
     93 0xf5e71ddd19a51c83926e257c23c693387667610d      1
     94 0xf61cecb051af46421c79b3457f1e19397cde936f      1
     95 0xf6a050e28c9da904f5ea04226bc43a92a2ffe4ed      1
     96 0xf6d0fdb8dd8bfe21722c39fdb9fcee5bb50e70b6      1
     97 0xf77bbc1fa9ff768dbec37418e2785d0a336815f4      1
     98 0xf7c98f4a76ddca190fc4cdbf7246d6c908242750      1
     99 0xfaaf6e583546295ce96316ec26424f37885668b9      1
    100 0xfe912f7f98df6946f4517257a13dd297723a28c2      1

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
