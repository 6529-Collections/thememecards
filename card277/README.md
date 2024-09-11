
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:320         Length:320         Min.   :1   Length:320        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:320        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20696969 # https://etherscan.io/block/20696969
block_hash <- "0x72f0056b59d3606add884c7e1610979db89367ccb307ea1fcc5fe6a564a48d04"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4617 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","JEFFsART","TheElements1stSoloExhibition","JeffAphisitCollection","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DestinyDrawEditions","ArtAttackEditions","MerryXMasEditions","BoyfromtheSouthEditions","TheWorldcatcherEditions","SpaceTimeEditions","StaywithmeEditions","Bidderseditions","DarkmodeEditions","JeffAphisitEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","JEFFsART","TheElements1stSoloExhibition","JeffAphisitCollection","MakersPlace","DestinyDrawEditions","ArtAttackEditions","MerryXMasEditions","BoyfromtheSouthEditions","TheWorldcatcherEditions","SpaceTimeEditions","StaywithmeEditions","Bidderseditions","DarkmodeEditions","JeffAphisitEditions"), address_remove=address_remove,address_max=1)
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
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     3 0x0ae1d436e7af8222f07e06869b8b4d20ae8e2011      1
     4 0x10990db1491361c825fdb2883be8fcfb8a35a4ca      1
     5 0x2325079108ce42689af06157854f2f48cb268aba      1
     6 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     7 0x2ffbfe97126261b600ae6771f13556cc6104c1dd      1
     8 0x300cc15e8110cadd2a8faf575aed087175504254      1
     9 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    10 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
    11 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    12 0x4c16ca014fe5f3adc4b346defd2c40387518d033      1
    13 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    14 0x4fffe1f377999580d589617e160714215fd99650      1
    15 0x5291419fe2327c1dc8bc80c55575242d378b5e5b      1
    16 0x5fd36a4a4bcfe5ea059706a6a09c26b62be4059a      1
    17 0x611471bc56a47c85aa4f16b109cb2a2d0be6854d      1
    18 0x695589d2bc5a63db1e830b1da7a07de73e58c6a8      1
    19 0x6b92686c40747c85809a6772d0eda8e22a77c60c      1
    20 0x76b849d7c7ad2803ca81fca5f0ca539063bb22bd      1
    21 0x78a30a4e7872ea1150980c435537cd83c0f5e313      1
    22 0x816734db8bc22fe34d9e2bfc3f3d86e638c232df      1
    23 0x9eea822800921cd4425d86f0e797749b5220ce0f      1
    24 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
    25 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    26 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    27 0xc883a79e8e4594c4f89434edb754a10da2311139      1
    28 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    29 0xce4c497b87e239fc20385f759fa69fab3abcb37b      1
    30 0xd6cd34f0d27d3942fb7aab193566bd76f694bf59      1
    31 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    32 0xf6056610a26e6bd3700f56f728ce0f03165ef9aa      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 138 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02091d775b8b5cb58617b9968584013c04e39154      1
      2 0x02c976157397deabece424d26a4402fcf991db75      1
      3 0x0309e40b63792dcaa137d987d97dd6479664c731      1
      4 0x031633884306abaf5d5003ca5a631aec9ef258aa      1
      5 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
      6 0x055cb3f8f05e07af027d760650cd9a28a64cb69f      1
      7 0x0583ee9adc0f2684607c32e6d9691e350010bb11      1
      8 0x05996b1b5a1245774dcfcf10edc287bea1a66778      1
      9 0x06273686ef746fc428c97d47928c9f51b323d47e      1
     10 0x063b41a093b9f458ba83dd4b976b196fea0dd98c      1
     11 0x081d91b7dae3d70be58862c040c8189fa380d577      1
     12 0x0bbaf4af7344754ca26b5def315756744f5d9e8b      1
     13 0x0c368d683c616073d48fa26f7796dfe3f8d46526      1
     14 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     15 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     16 0x0f94325dcc0e118fb3454bceff6bcd814ad2b58f      1
     17 0x10e332891e38ac2d40860120754192705c5896d8      1
     18 0x134d53790308fbd6d8b9e442e23f609b476869ac      1
     19 0x13883dc4326fe65a3ae0383d509166dfad620cc4      1
     20 0x1510ee5e66f5aeca4d541dccb28c7b232a1a33fb      1
     21 0x18e2f7903516423c6a5841864565c4078198d64c      1
     22 0x1b3f7964929dae299f194d46b7386b0df8f10db3      1
     23 0x1d54ee6c2e15b168f35a578ca2bcf49f6a46c473      1
     24 0x220969688ccf551dce74f2c1249be2ee2813e060      1
     25 0x22a12b41cdc8dd4c74117166f9fc2666e4eed534      1
     26 0x26424d78c9c433af01188c6f3581af44bfc15e87      1
     27 0x2ae1424c8082646bd0175102229caaed66e94f82      1
     28 0x2ae9070b029d05d8e6516aec0475002c53595a9d      1
     29 0x2ccff304ef578b238ee82e1d1d53c34e80b48ad6      1
     30 0x2e7735adb640c4a1f6c72657c66fbd892d487d5d      1
     31 0x2f33f719b891f29abaee8c317975df4162cd61db      1
     32 0x307a693c69515ca0fad41f672be3555c7a7c871e      1
     33 0x31004f28babda1f632cb445944892b04a411e7f7      1
     34 0x31961840ad769dbb293803ce63029cdfcd33abbf      1
     35 0x34104a743eec28e80913c85eb3bf0d1a3e049ef4      1
     36 0x35c70ec98adef35503fd3c034371ea723b96c9de      1
     37 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     38 0x3d6130c64eb3977d48092dbc28d1d084c6f7aed2      1
     39 0x42ec4f1c4c0e2f273ddbe1d97a909f4e6e9247fe      1
     40 0x43c241fb9d076a202cc5063e3e47319694276048      1
     41 0x4949b46633b810bdd745b028062b30f6b647ec60      1
     42 0x4ab5fb4186cfdc39028c8eef7a705942dfc08567      1
     43 0x4b946f3513a795f82f53f59377644517070631d2      1
     44 0x4d8f12386561667cfcc600ce2cc161a0d337da8b      1
     45 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
     46 0x4fff7a19e4ab4fcd5d643b79b26b2c8db5e63b9b      1
     47 0x500bcbbe0ff8ee8fe7c50f7606650e39a50cfd4e      1
     48 0x5189701de8dcc286007b4d8ce470b3f157068fe9      1
     49 0x521f3253dee5aa0b5ec750befc896496f36e7f15      1
     50 0x5987314ae8b44b2cc2f54a07160792aae083fba7      1
     51 0x5c2f42c2cf2ee5e0729f98e72063c70985a101ba      1
     52 0x5dd5063a9b03588bb8ff16d83a9ae4f17637a749      1
     53 0x60eda54e5cf0ad97a6a886bb277620e6aa15384a      1
     54 0x611c69ad2c41b64cd58dcd79579e0c4a5dffd52c      1
     55 0x6246a3ac0dab2f8401b31f04d4045bf9d3468ec2      1
     56 0x63f42bfc17b6ff3a7f487c406b8e006d0d4970c3      1
     57 0x64a5c1a76ad070f8f475c1ce0e7fca24e1964da5      1
     58 0x6519cc343b07b3cc7d67d2ba35ae6b56da91f135      1
     59 0x655a7ba052e5acde220a482777981c050fa087fe      1
     60 0x661219126f8a5d7a52ae87545dc200da3ee68ff6      1
     61 0x67b8d359653ad77d8d178d28960d700301616b5e      1
     62 0x6800b7a17017d3915c4993fcc46753fe278a20ab      1
     63 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     64 0x6e703f9264375b88791c958053be7834ed49d769      1
     65 0x7077fe5a6017a5ed98a0b8b048bd7e523633d6b6      1
     66 0x7208e87ec5e2e661a7e61e976c10ea4b2307c3f4      1
     67 0x7b03763bb30c226ba75491633d6440e960c2db81      1
     68 0x7b4b7036941046c6783a5212560ab00b46b95739      1
     69 0x7b800021e2529d26f45f4f7a3d0d91a3706ace19      1
     70 0x7ccca7806333674665983defdf13bde7e1c87301      1
     71 0x7dc4e09a175a687cbe4c2b282a356f5f63a17848      1
     72 0x7e4a92228ee4244635273acd024ac6e42f07b6c7      1
     73 0x7e6248626a6548d8ce90e1b7ac8eab64396225da      1
     74 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     75 0x8038b9420c93fd466a1c230e034f71da6588feb4      1
     76 0x8342a36e9cf61252e231619d2f38bd367df8f1a5      1
     77 0x847025f13eb4981456fad35b72b279d21968d578      1
     78 0x85b53bddd65c621c8d44d6f8bd1130f8fefeaa6c      1
     79 0x86b203594a23ee188579f9f31c1f307ffbf4c548      1
     80 0x892f0099fbeccc61bafc5e3980658a1d47a1e823      1
     81 0x89bb75c2876df108c71d0c7dfd3e1ea52b341705      1
     82 0x92730185034d49ce10f0b0670f2ee3857415f178      1
     83 0x933e054ca6638b4e7a6b0895fd67bff01ec5cc36      1
     84 0x952c851566c9a8cd61962ccfbc7354d14e036668      1
     85 0x9658d095406b17b45b3109c7b8b57cfa641a2e15      1
     86 0x98d31e06789783d5c1dd2e18013f04c9dab78656      1
     87 0x99fdd2a67ba5ec9fb98f451f0e88d0fbec1ee121      1
     88 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
     89 0x9b06c93cdf51c23f5bd26ef4e6cec4fbf3dfd16a      1
     90 0x9bc2879384ec9543718c93bdc769050286f91281      1
     91 0xa328cb4232a1b7db10f08e97c6e973ca90b3eeca      1
     92 0xa61186ef4cc522028546cb79761c4c8dd65c6c3b      1
     93 0xa7695409c5fef39a8367759a279386302a683b9a      1
     94 0xa8042dbae807af7664000a5d8b85be7b92f25252      1
     95 0xa8679b6fad644ab1a818e533f9c87302a46df37f      1
     96 0xaadb66fee1a106cef8171f23602665c08ddd2854      1
     97 0xab0a13710c5efd1f149212782593bce505aea44c      1
     98 0xab42c346917f95431799271394c3e061c005886d      1
     99 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    100 0xaf23f34544c2e39170122db18d0638f20d825814      1
    101 0xaf2d7878c99601f1a397bfa99bfafe000aa9efe2      1
    102 0xb0bf6f6fd3c8bd194d94d39a41fcb99fdffb5273      1
    103 0xb684afe21d28dc6bf235ca7c7da3c21df19b3bcd      1
    104 0xb8271fb41b49ff2fc72cd0ac97b3102d8ba0cabc      1
    105 0xba2792a3ce6f460bf78de72f8878e3ffe5409fc0      1
    106 0xbb2844146f40689b1fe38460935e31830204c104      1
    107 0xbb7b86161d49a62dc0fb9f5124500c7384dd1339      1
    108 0xbcbdaec5cc5f6630f4abf250029d5f97ba28d2a8      1
    109 0xbf66030c5b6bf2d957c780f4b0813fbce10b115e      1
    110 0xc15b235bc24f739afbfc43687b6173ffd58d1b4a      1
    111 0xc2188ee69329527a49ae9310535489ebb1f1dd89      1
    112 0xc5e7019c93b08ac011fa75500a5ea423ec6a67a2      1
    113 0xcc9dc1abeca9371e568f66b43d427a0a6de0e039      1
    114 0xccb3b4e89181ef6cfa4378a608aab9d25c06e841      1
    115 0xcd3ab83abd3f9cccc23ad59de44be5e1fc087c11      1
    116 0xce450bec5517006c0d4cfb9898d4b792f917a242      1
    117 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    118 0xd1d33b233d9462c0e7ccf9962c9d941a57af3872      1
    119 0xd84c9774ddf51a5d1fcfbe022f9d93e36eb4c23c      1
    120 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    121 0xdae151333b5798d35e873aedc36f193d19ddfad1      1
    122 0xdb07569be914d6a3ee3d1c18c077a03a6df66f36      1
    123 0xdbd85d3373ce7c3954bddee710ed1a682b064b07      1
    124 0xdf24bf4366f053ae1bb4d1a1200051e590d3b328      1
    125 0xe0fa7bf6827221a5275505d163a2989f6f0dfcfb      1
    126 0xe5e508c953114f8688b9d6072cb1196cf6487006      1
    127 0xe7d687f75778471118898c589e5f7ba1580dd97f      1
    128 0xe925b83e68f0d221934f91659babef80a2450069      1
    129 0xe9ad6041bfd863415fd483bc2471db2dfd065a0b      1
    130 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    131 0xee38297ed7c0e96849ec8581128033f99f48650d      1
    132 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    133 0xf6645949d476016963f13796c74db1942079dcb1      1
    134 0xf7937f825a2aecdcb962925f12b64943d0b2496a      1
    135 0xf943b9daac52568c3f5b5ca41b0c31ab2d737c42      1
    136 0xf9d6fce6cce28c0cd62c5caf045f4f6233888989      1
    137 0xfd6e50c8e434a1f49af4f20cc0640ba3da68f84f      1
    138 0xfe5141550f13157ca8d0f96e55096a770308fb8b      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 170 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02091d775b8b5cb58617b9968584013c04e39154      1
      2 0x02c976157397deabece424d26a4402fcf991db75      1
      3 0x0309e40b63792dcaa137d987d97dd6479664c731      1
      4 0x031633884306abaf5d5003ca5a631aec9ef258aa      1
      5 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      6 0x04830005c7dbd3671f2c4072c61d4e82b3c27674      1
      7 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      8 0x055cb3f8f05e07af027d760650cd9a28a64cb69f      1
      9 0x0583ee9adc0f2684607c32e6d9691e350010bb11      1
     10 0x05996b1b5a1245774dcfcf10edc287bea1a66778      1
     11 0x06273686ef746fc428c97d47928c9f51b323d47e      1
     12 0x063b41a093b9f458ba83dd4b976b196fea0dd98c      1
     13 0x081d91b7dae3d70be58862c040c8189fa380d577      1
     14 0x0ae1d436e7af8222f07e06869b8b4d20ae8e2011      1
     15 0x0bbaf4af7344754ca26b5def315756744f5d9e8b      1
     16 0x0c368d683c616073d48fa26f7796dfe3f8d46526      1
     17 0x0d71ec30c323037bdac7bd174d4a95cec5a4ee0d      1
     18 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     19 0x0f94325dcc0e118fb3454bceff6bcd814ad2b58f      1
     20 0x10990db1491361c825fdb2883be8fcfb8a35a4ca      1
     21 0x10e332891e38ac2d40860120754192705c5896d8      1
     22 0x134d53790308fbd6d8b9e442e23f609b476869ac      1
     23 0x13883dc4326fe65a3ae0383d509166dfad620cc4      1
     24 0x1510ee5e66f5aeca4d541dccb28c7b232a1a33fb      1
     25 0x18e2f7903516423c6a5841864565c4078198d64c      1
     26 0x1b3f7964929dae299f194d46b7386b0df8f10db3      1
     27 0x1d54ee6c2e15b168f35a578ca2bcf49f6a46c473      1
     28 0x220969688ccf551dce74f2c1249be2ee2813e060      1
     29 0x22a12b41cdc8dd4c74117166f9fc2666e4eed534      1
     30 0x2325079108ce42689af06157854f2f48cb268aba      1
     31 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     32 0x26424d78c9c433af01188c6f3581af44bfc15e87      1
     33 0x2ae1424c8082646bd0175102229caaed66e94f82      1
     34 0x2ae9070b029d05d8e6516aec0475002c53595a9d      1
     35 0x2ccff304ef578b238ee82e1d1d53c34e80b48ad6      1
     36 0x2e7735adb640c4a1f6c72657c66fbd892d487d5d      1
     37 0x2f33f719b891f29abaee8c317975df4162cd61db      1
     38 0x2ffbfe97126261b600ae6771f13556cc6104c1dd      1
     39 0x300cc15e8110cadd2a8faf575aed087175504254      1
     40 0x307a693c69515ca0fad41f672be3555c7a7c871e      1
     41 0x31004f28babda1f632cb445944892b04a411e7f7      1
     42 0x31961840ad769dbb293803ce63029cdfcd33abbf      1
     43 0x34104a743eec28e80913c85eb3bf0d1a3e049ef4      1
     44 0x35c70ec98adef35503fd3c034371ea723b96c9de      1
     45 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     46 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     47 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     48 0x3d6130c64eb3977d48092dbc28d1d084c6f7aed2      1
     49 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     50 0x42ec4f1c4c0e2f273ddbe1d97a909f4e6e9247fe      1
     51 0x43c241fb9d076a202cc5063e3e47319694276048      1
     52 0x4949b46633b810bdd745b028062b30f6b647ec60      1
     53 0x4ab5fb4186cfdc39028c8eef7a705942dfc08567      1
     54 0x4b946f3513a795f82f53f59377644517070631d2      1
     55 0x4c16ca014fe5f3adc4b346defd2c40387518d033      1
     56 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     57 0x4d8f12386561667cfcc600ce2cc161a0d337da8b      1
     58 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
     59 0x4fff7a19e4ab4fcd5d643b79b26b2c8db5e63b9b      1
     60 0x4fffe1f377999580d589617e160714215fd99650      1
     61 0x500bcbbe0ff8ee8fe7c50f7606650e39a50cfd4e      1
     62 0x5189701de8dcc286007b4d8ce470b3f157068fe9      1
     63 0x521f3253dee5aa0b5ec750befc896496f36e7f15      1
     64 0x5291419fe2327c1dc8bc80c55575242d378b5e5b      1
     65 0x5987314ae8b44b2cc2f54a07160792aae083fba7      1
     66 0x5c2f42c2cf2ee5e0729f98e72063c70985a101ba      1
     67 0x5dd5063a9b03588bb8ff16d83a9ae4f17637a749      1
     68 0x5fd36a4a4bcfe5ea059706a6a09c26b62be4059a      1
     69 0x60eda54e5cf0ad97a6a886bb277620e6aa15384a      1
     70 0x611471bc56a47c85aa4f16b109cb2a2d0be6854d      1
     71 0x611c69ad2c41b64cd58dcd79579e0c4a5dffd52c      1
     72 0x6246a3ac0dab2f8401b31f04d4045bf9d3468ec2      1
     73 0x63f42bfc17b6ff3a7f487c406b8e006d0d4970c3      1
     74 0x64a5c1a76ad070f8f475c1ce0e7fca24e1964da5      1
     75 0x6519cc343b07b3cc7d67d2ba35ae6b56da91f135      1
     76 0x655a7ba052e5acde220a482777981c050fa087fe      1
     77 0x661219126f8a5d7a52ae87545dc200da3ee68ff6      1
     78 0x67b8d359653ad77d8d178d28960d700301616b5e      1
     79 0x6800b7a17017d3915c4993fcc46753fe278a20ab      1
     80 0x695589d2bc5a63db1e830b1da7a07de73e58c6a8      1
     81 0x6a08986aad2967bd659457472fefd9bcbd417316      1
     82 0x6b92686c40747c85809a6772d0eda8e22a77c60c      1
     83 0x6e703f9264375b88791c958053be7834ed49d769      1
     84 0x7077fe5a6017a5ed98a0b8b048bd7e523633d6b6      1
     85 0x7208e87ec5e2e661a7e61e976c10ea4b2307c3f4      1
     86 0x76b849d7c7ad2803ca81fca5f0ca539063bb22bd      1
     87 0x78a30a4e7872ea1150980c435537cd83c0f5e313      1
     88 0x7b03763bb30c226ba75491633d6440e960c2db81      1
     89 0x7b4b7036941046c6783a5212560ab00b46b95739      1
     90 0x7b800021e2529d26f45f4f7a3d0d91a3706ace19      1
     91 0x7ccca7806333674665983defdf13bde7e1c87301      1
     92 0x7dc4e09a175a687cbe4c2b282a356f5f63a17848      1
     93 0x7e4a92228ee4244635273acd024ac6e42f07b6c7      1
     94 0x7e6248626a6548d8ce90e1b7ac8eab64396225da      1
     95 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     96 0x8038b9420c93fd466a1c230e034f71da6588feb4      1
     97 0x816734db8bc22fe34d9e2bfc3f3d86e638c232df      1
     98 0x8342a36e9cf61252e231619d2f38bd367df8f1a5      1
     99 0x847025f13eb4981456fad35b72b279d21968d578      1
    100 0x85b53bddd65c621c8d44d6f8bd1130f8fefeaa6c      1
    101 0x86b203594a23ee188579f9f31c1f307ffbf4c548      1
    102 0x892f0099fbeccc61bafc5e3980658a1d47a1e823      1
    103 0x89bb75c2876df108c71d0c7dfd3e1ea52b341705      1
    104 0x92730185034d49ce10f0b0670f2ee3857415f178      1
    105 0x933e054ca6638b4e7a6b0895fd67bff01ec5cc36      1
    106 0x952c851566c9a8cd61962ccfbc7354d14e036668      1
    107 0x9658d095406b17b45b3109c7b8b57cfa641a2e15      1
    108 0x98d31e06789783d5c1dd2e18013f04c9dab78656      1
    109 0x99fdd2a67ba5ec9fb98f451f0e88d0fbec1ee121      1
    110 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
    111 0x9b06c93cdf51c23f5bd26ef4e6cec4fbf3dfd16a      1
    112 0x9bc2879384ec9543718c93bdc769050286f91281      1
    113 0x9eea822800921cd4425d86f0e797749b5220ce0f      1
    114 0x9fae8562dc4fbcdd9d26d6d10a7fb88ce60a028e      1
    115 0xa328cb4232a1b7db10f08e97c6e973ca90b3eeca      1
    116 0xa61186ef4cc522028546cb79761c4c8dd65c6c3b      1
    117 0xa7695409c5fef39a8367759a279386302a683b9a      1
    118 0xa8042dbae807af7664000a5d8b85be7b92f25252      1
    119 0xa8679b6fad644ab1a818e533f9c87302a46df37f      1
    120 0xaadb66fee1a106cef8171f23602665c08ddd2854      1
    121 0xab0a13710c5efd1f149212782593bce505aea44c      1
    122 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    123 0xab42c346917f95431799271394c3e061c005886d      1
    124 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    125 0xaf23f34544c2e39170122db18d0638f20d825814      1
    126 0xaf2d7878c99601f1a397bfa99bfafe000aa9efe2      1
    127 0xb0bf6f6fd3c8bd194d94d39a41fcb99fdffb5273      1
    128 0xb684afe21d28dc6bf235ca7c7da3c21df19b3bcd      1
    129 0xb8271fb41b49ff2fc72cd0ac97b3102d8ba0cabc      1
    130 0xba2792a3ce6f460bf78de72f8878e3ffe5409fc0      1
    131 0xbb2844146f40689b1fe38460935e31830204c104      1
    132 0xbb7b86161d49a62dc0fb9f5124500c7384dd1339      1
    133 0xbcbdaec5cc5f6630f4abf250029d5f97ba28d2a8      1
    134 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    135 0xbf66030c5b6bf2d957c780f4b0813fbce10b115e      1
    136 0xc15b235bc24f739afbfc43687b6173ffd58d1b4a      1
    137 0xc2188ee69329527a49ae9310535489ebb1f1dd89      1
    138 0xc5e7019c93b08ac011fa75500a5ea423ec6a67a2      1
    139 0xc883a79e8e4594c4f89434edb754a10da2311139      1
    140 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    141 0xcc9dc1abeca9371e568f66b43d427a0a6de0e039      1
    142 0xccb3b4e89181ef6cfa4378a608aab9d25c06e841      1
    143 0xcd3ab83abd3f9cccc23ad59de44be5e1fc087c11      1
    144 0xce450bec5517006c0d4cfb9898d4b792f917a242      1
    145 0xce4c497b87e239fc20385f759fa69fab3abcb37b      1
    146 0xd19286bdd1a2b06df3c1105407c38511ffb87959      1
    147 0xd1d33b233d9462c0e7ccf9962c9d941a57af3872      1
    148 0xd6cd34f0d27d3942fb7aab193566bd76f694bf59      1
    149 0xd84c9774ddf51a5d1fcfbe022f9d93e36eb4c23c      1
    150 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    151 0xdae151333b5798d35e873aedc36f193d19ddfad1      1
    152 0xdb07569be914d6a3ee3d1c18c077a03a6df66f36      1
    153 0xdbd85d3373ce7c3954bddee710ed1a682b064b07      1
    154 0xdf24bf4366f053ae1bb4d1a1200051e590d3b328      1
    155 0xe0fa7bf6827221a5275505d163a2989f6f0dfcfb      1
    156 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    157 0xe5e508c953114f8688b9d6072cb1196cf6487006      1
    158 0xe7d687f75778471118898c589e5f7ba1580dd97f      1
    159 0xe925b83e68f0d221934f91659babef80a2450069      1
    160 0xe9ad6041bfd863415fd483bc2471db2dfd065a0b      1
    161 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    162 0xee38297ed7c0e96849ec8581128033f99f48650d      1
    163 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    164 0xf6056610a26e6bd3700f56f728ce0f03165ef9aa      1
    165 0xf6645949d476016963f13796c74db1942079dcb1      1
    166 0xf7937f825a2aecdcb962925f12b64943d0b2496a      1
    167 0xf943b9daac52568c3f5b5ca41b0c31ab2d737c42      1
    168 0xf9d6fce6cce28c0cd62c5caf045f4f6233888989      1
    169 0xfd6e50c8e434a1f49af4f20cc0640ba3da68f84f      1
    170 0xfe5141550f13157ca8d0f96e55096a770308fb8b      1

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
