
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:156         Length:156         Min.   : 1.00   Length:156        
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.16                     
                                           3rd Qu.: 1.00                     
                                           Max.   :10.00                     
         name          
     Length:156        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21346969 # https://etherscan.io/block/21346969
block_hash <- "0x1f4d6ca4e3aecd35da78070a9d654acf33f62c217620335a791e7401e57620c1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4536 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","Foundation","APortraitofSouls","LongingtheClouds","BLOOM","BlackRabbit","BlueGirl"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("SilentDialogueEditions","NightwingsEditions","SemblanceofLifeEditions","HymnofBeautyEditions","GarisEdelweissEditions","SeedSeriesEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","KnownOrigin","Foundation","APortraitofSouls","LongingtheClouds","BLOOM","BlackRabbit","BlueGirl","SilentDialogueEditions","NightwingsEditions","SemblanceofLifeEditions","HymnofBeautyEditions","GarisEdelweissEditions","SeedSeriesEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 25 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
     2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     3 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     4 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     5 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     6 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     7 0x500075b14ecfa2636c18b4b8e383dc47989cb355      1
     8 0x5932ff2c8b86dde43c9920beb1e58c278d9e0d42      1
     9 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    10 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    11 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
    12 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    13 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    14 0x943c452f19dc971db2fb944d98efb907fd6edc61      1
    15 0x9dabb5e4a4936633586d5886ac0c6eb224600b90      1
    16 0xac26715669c961ae9d50952eede66549930ed561      1
    17 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    18 0xc4cae9e74593011f5d5a7b7eabd2d2a8c7e3d4ea      1
    19 0xc5a54d16afe587c81cd70f1fa726c0d3e0c41092      1
    20 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    21 0xe829b57163a5e932101c18d6c88de32c170c3b4b      1
    22 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    23 0xefbf7a6fa61a1602eb7630c92e79e5b6e63909e1      1
    24 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    25 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 81 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00179248ae956f3c6225387c5a04dad4022a13bd      1
     2 0x00a8ce8fd79d8a6a6b2d260b08a3b60956f0fcb1      1
     3 0x03a852bacc4090fa4ae6ea17bfec463f4d53b986      1
     4 0x046ed5bba6d28333bbbdc907fbf8a136f2711f17      1
     5 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     6 0x054b1c68364259942195c6b50a2b7037de64df7b      1
     7 0x05d2e466b841fe014e1ce2d1f826707b15f8914d      1
     8 0x069da1bad547d67abf26c42358d3a6a8a78deca0      1
     9 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
    10 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
    11 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
    12 0x16f3774d4b22ae11bc7c568a4d847e926304cbe7      1
    13 0x19dd2a922e6da1b6354ded1e9cc070140eef0bad      1
    14 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
    15 0x207debe3bc3529139d1b33d6b8d487450df5dfc7      1
    16 0x2160926bfcfcf7f25bd62592fd60794720da86db      1
    17 0x223f6b0e03d1e78e8750122053b508e50426ecfc      1
    18 0x224dab0505667cb7fcfca0a27dbc6e27dada7b7d      1
    19 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
    20 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
    21 0x2a9ed80127e36610f8ddbecddcd318cbdc8c9ff4      1
    22 0x2fa2eb271bb8e87c6f783204a56fd07092611aaa      1
    23 0x3062a51d1474a7f2ad7c31c1c7ed44d7c2bf57ea      1
    24 0x338a0e3a80fdbd75ccf6521a0ce65993965a5d2e      1
    25 0x33a8003b240416a02e51a2fd908c18929b207cf0      1
    26 0x38ab7734a25970f3a7f7ec792b3faf8058f8f67f      1
    27 0x3f35a01f3cbc4d62a49085497afda768c73a91cf      1
    28 0x3f5f363c5f0d6f5b18d396b23e5dc311647034b9      1
    29 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
    30 0x423fa6f71071926cf8044084d8b0086cd053061e      1
    31 0x4252c3d4fdbe3dc20e8874c3bab6903d5cb0a04d      1
    32 0x48bf3fdc07b4c8ddd0d9d0d4bfb55779bec00c42      1
    33 0x4eda5e83e19c68e1858b923760dc8ae442902e65      1
    34 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
    35 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
    36 0x582e2a3ba50c264c9bd2cf85f86315fe1c232549      1
    37 0x66df0a3c697f25134a73fd3030f2c3a9a6861bbc      1
    38 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
    39 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    40 0x748640b4fcad2cd41969387f4b10369b6d687436      1
    41 0x762da606029d3120735aa1eec15464e265db7a3c      1
    42 0x7bcec44c7486143ee875103de66006047cae8df7      1
    43 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    44 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
    45 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
    46 0x8b66b420c4f3137b2bb4aa6a291bd81790bfece9      1
    47 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
    48 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
    49 0x9539e51947fccdd17012f6f7180d8b061977b660      1
    50 0x95e3c7af64ffcdda13630c7c10646775dc638275      1
    51 0x98025f7776884d38093508e31f4e3119e2979cad      1
    52 0x981a48f901e65a523e64015a86956b011e93f527      1
    53 0x9aa43bf353b95da83772b2f026a2d723786c4e90      1
    54 0xa41cd41474f7b59736da76b7fe12b9b8d9afb94f      1
    55 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
    56 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    57 0xa7c3b6c27e782e18b01c1a1a577548add6d9bec9      1
    58 0xab4081aa5d3f58a47432f119acb4e468bbbe0e53      1
    59 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    60 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    61 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    62 0xaef0ed58588953c0cc5bb24b123b405af164a8cf      1
    63 0xb24f59e19608c716e2322558c33fd8b1ba2b0318      1
    64 0xb2de1063fb4c0e1a3c1f45d703d658936307cfa3      1
    65 0xb2e1e63516e755359aba6d8d10a6e98d47abdc92      1
    66 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
    67 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    68 0xbf37313456e4bd0d1cb1bcfa37cf07c3d5a270f4      1
    69 0xc1f74ddd2570d8970082489f35b7a114b12cdfd2      1
    70 0xc41dec3cf3a042a3684be9279d5067ab9d813033      1
    71 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
    72 0xdc184b91eba750a4fb62c4d8e642e9ead9b6cb55      1
    73 0xdcfe06271a89d454ff3302bab78d564ad6952607      1
    74 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
    75 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
    76 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    77 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    78 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    79 0xeebff2d3f395aaf95ac43c813290ac75711ee28f      1
    80 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1
    81 0xfce6abe45b291522cd0d7ceddf1dac2ee0283a6b      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 106 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00179248ae956f3c6225387c5a04dad4022a13bd      1
      2 0x00a8ce8fd79d8a6a6b2d260b08a3b60956f0fcb1      1
      3 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      4 0x03a852bacc4090fa4ae6ea17bfec463f4d53b986      1
      5 0x046ed5bba6d28333bbbdc907fbf8a136f2711f17      1
      6 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      7 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      8 0x054b1c68364259942195c6b50a2b7037de64df7b      1
      9 0x05d2e466b841fe014e1ce2d1f826707b15f8914d      1
     10 0x069da1bad547d67abf26c42358d3a6a8a78deca0      1
     11 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     12 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     13 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     14 0x16f3774d4b22ae11bc7c568a4d847e926304cbe7      1
     15 0x19dd2a922e6da1b6354ded1e9cc070140eef0bad      1
     16 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     17 0x207debe3bc3529139d1b33d6b8d487450df5dfc7      1
     18 0x2160926bfcfcf7f25bd62592fd60794720da86db      1
     19 0x223f6b0e03d1e78e8750122053b508e50426ecfc      1
     20 0x224dab0505667cb7fcfca0a27dbc6e27dada7b7d      1
     21 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     22 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
     23 0x2a9ed80127e36610f8ddbecddcd318cbdc8c9ff4      1
     24 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     25 0x2fa2eb271bb8e87c6f783204a56fd07092611aaa      1
     26 0x3062a51d1474a7f2ad7c31c1c7ed44d7c2bf57ea      1
     27 0x338a0e3a80fdbd75ccf6521a0ce65993965a5d2e      1
     28 0x33a8003b240416a02e51a2fd908c18929b207cf0      1
     29 0x38ab7734a25970f3a7f7ec792b3faf8058f8f67f      1
     30 0x3f35a01f3cbc4d62a49085497afda768c73a91cf      1
     31 0x3f5f363c5f0d6f5b18d396b23e5dc311647034b9      1
     32 0x3fbb8791109a19f5b488aa530cc0ac64b6c0d9f1      1
     33 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     34 0x423fa6f71071926cf8044084d8b0086cd053061e      1
     35 0x4252c3d4fdbe3dc20e8874c3bab6903d5cb0a04d      1
     36 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
     37 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     38 0x48bf3fdc07b4c8ddd0d9d0d4bfb55779bec00c42      1
     39 0x4eda5e83e19c68e1858b923760dc8ae442902e65      1
     40 0x500075b14ecfa2636c18b4b8e383dc47989cb355      1
     41 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     42 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
     43 0x582e2a3ba50c264c9bd2cf85f86315fe1c232549      1
     44 0x5932ff2c8b86dde43c9920beb1e58c278d9e0d42      1
     45 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
     46 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
     47 0x66df0a3c697f25134a73fd3030f2c3a9a6861bbc      1
     48 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     49 0x6c299fd0fbf1ec228f7e8d7fb6fab4ff8e8752f2      1
     50 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     51 0x748640b4fcad2cd41969387f4b10369b6d687436      1
     52 0x762da606029d3120735aa1eec15464e265db7a3c      1
     53 0x7bcec44c7486143ee875103de66006047cae8df7      1
     54 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
     55 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
     56 0x81cc15402a47f18e210aa57fb9370a1b5525047e      1
     57 0x880c8fb6a59fb13ee7894b4fc18b7cbb529ed24e      1
     58 0x8b66b420c4f3137b2bb4aa6a291bd81790bfece9      1
     59 0x8e01d001a2a328e8b5b16520196e539cd2c37cf5      1
     60 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
     61 0x8fc4e14c1c2ab50be594208b0e5f1a22056316d3      1
     62 0x943c452f19dc971db2fb944d98efb907fd6edc61      1
     63 0x9539e51947fccdd17012f6f7180d8b061977b660      1
     64 0x95e3c7af64ffcdda13630c7c10646775dc638275      1
     65 0x98025f7776884d38093508e31f4e3119e2979cad      1
     66 0x981a48f901e65a523e64015a86956b011e93f527      1
     67 0x9aa43bf353b95da83772b2f026a2d723786c4e90      1
     68 0x9dabb5e4a4936633586d5886ac0c6eb224600b90      1
     69 0xa41cd41474f7b59736da76b7fe12b9b8d9afb94f      1
     70 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     71 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
     72 0xa7c3b6c27e782e18b01c1a1a577548add6d9bec9      1
     73 0xab4081aa5d3f58a47432f119acb4e468bbbe0e53      1
     74 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     75 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
     76 0xac26715669c961ae9d50952eede66549930ed561      1
     77 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
     78 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
     79 0xaef0ed58588953c0cc5bb24b123b405af164a8cf      1
     80 0xb24f59e19608c716e2322558c33fd8b1ba2b0318      1
     81 0xb2de1063fb4c0e1a3c1f45d703d658936307cfa3      1
     82 0xb2e1e63516e755359aba6d8d10a6e98d47abdc92      1
     83 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     84 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
     85 0xbf37313456e4bd0d1cb1bcfa37cf07c3d5a270f4      1
     86 0xc1f74ddd2570d8970082489f35b7a114b12cdfd2      1
     87 0xc41dec3cf3a042a3684be9279d5067ab9d813033      1
     88 0xc4cae9e74593011f5d5a7b7eabd2d2a8c7e3d4ea      1
     89 0xc5a54d16afe587c81cd70f1fa726c0d3e0c41092      1
     90 0xc91444fa4c5809e07d9541a9671ff9a9dfdf3b84      1
     91 0xdc184b91eba750a4fb62c4d8e642e9ead9b6cb55      1
     92 0xdcfe06271a89d454ff3302bab78d564ad6952607      1
     93 0xde5d9486616a0632cd2fe65f7fd5e86c885c7dca      1
     94 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
     95 0xdf6160fe3e5a7d6df67c2bb3d7e100b47efa243c      1
     96 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
     97 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
     98 0xe829b57163a5e932101c18d6c88de32c170c3b4b      1
     99 0xe86c12eca33b154d6eb3e367fe50918929c4bc30      1
    100 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    101 0xeebff2d3f395aaf95ac43c813290ac75711ee28f      1
    102 0xefbf7a6fa61a1602eb7630c92e79e5b6e63909e1      1
    103 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    104 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    105 0xfb52fa34d865c0398a123ff4cfa2d54c1a453ce3      1
    106 0xfce6abe45b291522cd0d7ceddf1dac2ee0283a6b      1

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
