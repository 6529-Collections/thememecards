
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:141         Length:141         Min.   : 1.000   Length:141        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.177                     
                                           3rd Qu.: 1.000                     
                                           Max.   :22.000                     
         name          
     Length:141        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20996969 # https://etherscan.io/block/20996969
block_hash <- "0xea9b1e82db889bcff21072c0ca8805f16d45cb0ddf706d0b1a7d41572c0ee012"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4769 

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

allow_artist1    <- pick(snapshot, contracts=c("TheSpaceFantasy","TheSpaceFantasyII","Foundation","MachineDreamsElectricWhispers","Melancholy","LateNightMusings","wiresandtrees","TheJourneys","TalesfromTheCosmos","TheDreamfields","TheSensory"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KnownOriginEditions","KnownOrigin2Editions","UnderFallingStarsEditions","LateNightHoursEditions","NGEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("TheSpaceFantasy","TheSpaceFantasyII","Foundation","MachineDreamsElectricWhispers","Melancholy","LateNightMusings","wiresandtrees","TheJourneys","TalesfromTheCosmos","TheDreamfields","TheSensory","KnownOriginEditions","KnownOrigin2Editions","UnderFallingStarsEditions","LateNightHoursEditions","NGEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 32 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x108bd290152849b520b7c788394359b9867a7fe0      1
     2 0x113d754ff2e6ca9fd6ab51932493e4f9dabdf596      1
     3 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     4 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
     5 0x12ae2d61b207549c8b37cfdd978327f9713643fa      1
     6 0x16f3774d4b22ae11bc7c568a4d847e926304cbe7      1
     7 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     8 0x2cbe14b7f60fbe6a323cba7db56f2d916c137f3c      1
     9 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    10 0x444569ae8a0324b9b32b3abdddb98ccb13036dd4      1
    11 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    12 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    13 0x4a74bd3f1136e33ddb218ef3c25eb8b7e396562b      1
    14 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
    15 0x59068075a799594db03c0255eed68e8e121155c8      1
    16 0x5f827663f7b14c5957ed6412a69eeadf5efd332a      1
    17 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
    18 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    19 0x908991ee204fd0717f5013b90b89513412723e42      1
    20 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    21 0xaabb23b2cbd0feb84f23bcbd659e3f3e9f9fdff3      1
    22 0xaaf773699e7e05ee899f2687674ae1b35b9ffc65      1
    23 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    24 0xae060f1ac3ed0b80cdd7e6f271c139fedc5a3d2f      1
    25 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    26 0xcb14228737c6b38c0d060bf7cf5ff8f9090936fc      1
    27 0xcdcdefa21877a575dda426c0c2a2887c5f93b132      1
    28 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    29 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    30 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    31 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    32 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 58 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     3 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     4 0x1123e0ea9738187897ae67966f353ca4516db6ed      1
     5 0x118827432004f01f7fec4cdfbcee2bb99cfe2b7c      1
     6 0x1485c1586b763231c4040e2f1fa7e51059111e5d      1
     7 0x193c82adf4e514d443735710fdc79f50de6f58a0      1
     8 0x2c3244f7761540e41859d9a446b489b08a85a058      1
     9 0x310b73b2873f0be20ec08a401e649a27410b6295      1
    10 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    11 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
    12 0x38b44d3f02447a5a0f20e4b60654e578cd891f23      1
    13 0x4298e663517593284ad4fe199b21815bd48a9969      1
    14 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    15 0x4d3db4dbbd6bec60e48b2810f4edea6a4c86bf30      1
    16 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
    17 0x53508d1e1bc0e002bde092364bd9a4c6290b2c1e      1
    18 0x5d37f453c005338758b6b6789184a343df35228e      1
    19 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    20 0x61680e1ac97c4cca7d259db934c48d4efa1617bd      1
    21 0x64aa471f59653b13f08559ff7ce39453bcd415dd      1
    22 0x65a7a1cc03b05d8060f4e464ea79693ace125245      1
    23 0x6ab85425b5cfdb5070a90791093cf233744471d7      1
    24 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    25 0x7c79201fc4e7b3e3208ec1d992dec7d36d9ce8b0      1
    26 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    27 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    28 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    29 0x81a52aeb543caf449530fab7f830bfea02539911      1
    30 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    31 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    32 0x87415604e043d6f688eb0e9d8e6b41809d3b8a61      1
    33 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    34 0x92c5adf5a4f709b55bb67150f3af69b3419647ac      1
    35 0x92e9b91aa2171694d740e7066f787739ca1af9de      1
    36 0x93958d6e3b79b6d15a11ba00876ce8edcb02ba95      1
    37 0x94c47ed35fd041953352cd483b66ad1dba05e31f      1
    38 0x99999990d598b918799f38163204bbc30611b6b6      1
    39 0x9ae17287014ece43e547f0c4d04d78f870dbf130      1
    40 0xa122d07800f776076a98ce983c012ce436e0f6ba      1
    41 0xa1c0c0ff6eaa63e7b0c7bf441fc146afdb08fc7b      1
    42 0xa2ed517eefc5365b05a970622af0f01586453510      1
    43 0xa70ab9757558421def791cf5fb377500a685e27b      1
    44 0xa9531d3b5483bef6fae3b4be62ccd83697ab92b1      1
    45 0xacfd8aff4dbdd3df2a42f0eb89f3128ea89b9927      1
    46 0xae7620f3a14dcaa88b14f487c15e1acb59ba4117      1
    47 0xb6456996b447e0ae843eaf07b4cd94e26a252f47      1
    48 0xb9440f8d49599ce585beff70e6e337fdbd496aa7      1
    49 0xbc53089acf9fe8eb0c70de5839a7826a3ece1bab      1
    50 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    51 0xd4d05fe29a112ebd5cc8f463f716c204ba5f70ce      1
    52 0xd77e11794f1af7292e7e0461eac96549e7ca8d58      1
    53 0xdbee3104f8790208e2980e4acdbba3d8f0c44848      1
    54 0xe30acddc6782d82c0cbe00349c27cb4e78c51510      1
    55 0xeb5ba6fc79ebb6561d8a18b90676a99049a9bee5      1
    56 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    57 0xf23fab88e84071d4cded0318f81c1a5c9d8bf09f      1
    58 0xf2439241881964006369c0e2377d45f3740f48a0      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     2 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     3 0x0f9027bac736804cfc68c0246f6283c4851d43b8      1
     4 0x108bd290152849b520b7c788394359b9867a7fe0      1
     5 0x1123e0ea9738187897ae67966f353ca4516db6ed      1
     6 0x113d754ff2e6ca9fd6ab51932493e4f9dabdf596      1
     7 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     8 0x118827432004f01f7fec4cdfbcee2bb99cfe2b7c      1
     9 0x1257ea6f17f3bd82b323789cf08b79191cc82b6d      1
    10 0x12ae2d61b207549c8b37cfdd978327f9713643fa      1
    11 0x1485c1586b763231c4040e2f1fa7e51059111e5d      1
    12 0x16f3774d4b22ae11bc7c568a4d847e926304cbe7      1
    13 0x193c82adf4e514d443735710fdc79f50de6f58a0      1
    14 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
    15 0x2c3244f7761540e41859d9a446b489b08a85a058      1
    16 0x2cbe14b7f60fbe6a323cba7db56f2d916c137f3c      1
    17 0x310b73b2873f0be20ec08a401e649a27410b6295      1
    18 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
    19 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
    20 0x38b44d3f02447a5a0f20e4b60654e578cd891f23      1
    21 0x3e1f1fc109b0130a8ba13a1546c713d030b8613f      1
    22 0x4298e663517593284ad4fe199b21815bd48a9969      1
    23 0x444569ae8a0324b9b32b3abdddb98ccb13036dd4      1
    24 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    25 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
    26 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    27 0x4a74bd3f1136e33ddb218ef3c25eb8b7e396562b      1
    28 0x4d3db4dbbd6bec60e48b2810f4edea6a4c86bf30      1
    29 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
    30 0x53508d1e1bc0e002bde092364bd9a4c6290b2c1e      1
    31 0x57c415bd4c74a6602bd78ceece26524585bed01c      1
    32 0x59068075a799594db03c0255eed68e8e121155c8      1
    33 0x5d37f453c005338758b6b6789184a343df35228e      1
    34 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    35 0x5f827663f7b14c5957ed6412a69eeadf5efd332a      1
    36 0x61680e1ac97c4cca7d259db934c48d4efa1617bd      1
    37 0x64aa471f59653b13f08559ff7ce39453bcd415dd      1
    38 0x65a7a1cc03b05d8060f4e464ea79693ace125245      1
    39 0x6ab85425b5cfdb5070a90791093cf233744471d7      1
    40 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    41 0x7c0ca862b604c61a7b7e78647ad95563f60956c2      1
    42 0x7c79201fc4e7b3e3208ec1d992dec7d36d9ce8b0      1
    43 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    44 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    45 0x7e5124202b1a177cbc254b2c2262541682f20601      1
    46 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
    47 0x81a52aeb543caf449530fab7f830bfea02539911      1
    48 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    49 0x85e5514fc67b6a2c228330cdcae543783a440947      1
    50 0x87415604e043d6f688eb0e9d8e6b41809d3b8a61      1
    51 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    52 0x908991ee204fd0717f5013b90b89513412723e42      1
    53 0x92c5adf5a4f709b55bb67150f3af69b3419647ac      1
    54 0x92e9b91aa2171694d740e7066f787739ca1af9de      1
    55 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    56 0x93958d6e3b79b6d15a11ba00876ce8edcb02ba95      1
    57 0x94c47ed35fd041953352cd483b66ad1dba05e31f      1
    58 0x99999990d598b918799f38163204bbc30611b6b6      1
    59 0x9ae17287014ece43e547f0c4d04d78f870dbf130      1
    60 0xa122d07800f776076a98ce983c012ce436e0f6ba      1
    61 0xa1c0c0ff6eaa63e7b0c7bf441fc146afdb08fc7b      1
    62 0xa2ed517eefc5365b05a970622af0f01586453510      1
    63 0xa70ab9757558421def791cf5fb377500a685e27b      1
    64 0xa9531d3b5483bef6fae3b4be62ccd83697ab92b1      1
    65 0xaabb23b2cbd0feb84f23bcbd659e3f3e9f9fdff3      1
    66 0xaaf773699e7e05ee899f2687674ae1b35b9ffc65      1
    67 0xacfd8aff4dbdd3df2a42f0eb89f3128ea89b9927      1
    68 0xada53dd3c03a5b20dccf8ee6615cf31a94c8fa51      1
    69 0xae060f1ac3ed0b80cdd7e6f271c139fedc5a3d2f      1
    70 0xae7620f3a14dcaa88b14f487c15e1acb59ba4117      1
    71 0xb618aacb9dcdc21ca69d310a6fc04674d293a193      1
    72 0xb6456996b447e0ae843eaf07b4cd94e26a252f47      1
    73 0xb9440f8d49599ce585beff70e6e337fdbd496aa7      1
    74 0xbc53089acf9fe8eb0c70de5839a7826a3ece1bab      1
    75 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    76 0xcb14228737c6b38c0d060bf7cf5ff8f9090936fc      1
    77 0xcdcdefa21877a575dda426c0c2a2887c5f93b132      1
    78 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    79 0xd4d05fe29a112ebd5cc8f463f716c204ba5f70ce      1
    80 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    81 0xd77e11794f1af7292e7e0461eac96549e7ca8d58      1
    82 0xda37896e56f12d640230a9e5115756a5cda9a581      1
    83 0xdbee3104f8790208e2980e4acdbba3d8f0c44848      1
    84 0xe30acddc6782d82c0cbe00349c27cb4e78c51510      1
    85 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    86 0xeb5ba6fc79ebb6561d8a18b90676a99049a9bee5      1
    87 0xf17b07497c09f11211975b0ffa2d9ec5cf96252c      1
    88 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    89 0xf23fab88e84071d4cded0318f81c1a5c9d8bf09f      1
    90 0xf2439241881964006369c0e2377d45f3740f48a0      1

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
