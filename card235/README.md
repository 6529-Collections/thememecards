
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:411         Length:411         Min.   :1   Length:411        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:411        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19894669 # https://etherscan.io/block/19894669
block_hash <- "0x780b514de68019150f7bfbf6eda04d03922625649544e4a20db8e2b53739f7be"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4510 

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

allow_artist1    <- pick(snapshot, contracts=c("Choen","OBSIDIAN","DreamWorld","PopCultureConsumption","SHE","LateNight"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KnownOriginEditions","CLKicksAssEditions","IntimateThingiesEditions","VibrateHarderStrongerEditions","ChoenLeeKOEditions","OpenEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Choen","OBSIDIAN","DreamWorld","PopCultureConsumption","SHE","LateNight","KnownOriginEditions","CLKicksAssEditions","IntimateThingiesEditions","VibrateHarderStrongerEditions","ChoenLeeKOEditions","OpenEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 54 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0378aa12b626f0cfd18334c10cdb8ee6043d4c64      1
     2 0x06cc86506e45941defcf9b8339eb6c55d038464b      1
     3 0x07f6329cdd8c29a241a0ad8503436c6652f6b509      1
     4 0x0971ccf9d28d09d64411d47cdaf210b6e5826731      1
     5 0x107beb3d304cd99535994cf85689bb6713185418      1
     6 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     7 0x18717190340a530a4b74bfa823cdc70fdc7d813b      1
     8 0x1f51119db1dbd797c2b0eb21e47303ac3e008798      1
     9 0x202ff78c06536a8ac03ab7a46623010333e774ca      1
    10 0x21f315ab7c8f3ab9356fd36c29abc2ab639129ba      1
    11 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
    12 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
    13 0x2a8f1c9d50e7647a30b2694e676fbc508d9241f9      1
    14 0x2b99edc6500a2e9313523064c1d77abc92b5108f      1
    15 0x2c65a1617d109ba9c9fe2ff2dfcf5bad8c9770fe      1
    16 0x3f42d65bede435fa3b9c82e91174b61672b79298      1
    17 0x49317b33a82fb576fba1462364d02569e766e33a      1
    18 0x51794aa02aa38f9427b78259f1a4c2107c8e00ac      1
    19 0x54becc7560a7be76d72ed76a1f5fee6c5a2a7ab6      1
    20 0x596dbef7dffaedd60c1df11bc51df3d529821b58      1
    21 0x633fe643a9e50959133af8e1330489a1c7288f12      1
    22 0x64b5ba690b067687643f66f579ea94786aa04c39      1
    23 0x7b7ec26a15eb19995be1323251f4494b50f1c6e5      1
    24 0x8e5150e73d4ff74706df51bf4329199e0cad7350      1
    25 0x9237c2ec55dd857536a1e5bd57ed020afa71f814      1
    26 0x951038bb372d16180c0afb0f46ab283059154004      1
    27 0x98a06dcdcdef33f584144a5c5b8fcf4d4716fc24      1
    28 0x9fffd9164f5c5808d9e58b9f0493b50f3f737ff2      1
    29 0xa014fbfc0822e5cc380475cb89881e8a41da6357      1
    30 0xa11cf77b3da9670e37bd849b4e71cea8ad4aa5c6      1
    31 0xab6ca2017548a170699890214bfd66583a0c1754      1
    32 0xab7473da0af4658ae2879cb641d82e7652721486      1
    33 0xab81377a955fd33034db726bebc7b610bfbdb156      1
    34 0xabb30d469bdf0b8e38c99c2f531e6a3eb35c22d3      1
    35 0xafe27d2049c037915fb73ed30dcebccec516e1c3      1
    36 0xb378a22e54370e114a55b9d1584482e3895a5df6      1
    37 0xd1547d1bdf27f87d31894899d45df1896ec3d537      1
    38 0xd394d4c661ebfaf82f2444350dfa7eb1f8965763      1
    39 0xd4bae95335c3cb22938b32f1a95a1d42cbf1e74e      1
    40 0xdb2538cb865bcf9fa4f830d8d8973037616591f3      1
    41 0xdb881c8fedd6b4c660905f9a41f6d577be51b668      1
    42 0xdb9d5b439493b6858a7b8d11cbf5885a39a07913      1
    43 0xe1a5b000a6dca024437bfaaf536c3756164b9e5c      1
    44 0xe40dcd3e8587c72cf741293c55d0ecb1fca5a852      1
    45 0xe99a3ebb5f7ceead6de9feee8ff4771802d90a12      1
    46 0xecd0d8de2e887f95d38e8ee2b22e41f3ae9df859      1
    47 0xf1aae566f12e1333a7d7e0a994ad2c1298d4b20c      1
    48 0xf244f986339c50e161f9fe90935580788e4f7aea      1
    49 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    50 0xf4f736875d51c8dd2de361373fccbff62ecc6312      1
    51 0xf93b6f2847c4b121453e0f08985a2b7301c53730      1
    52 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    53 0xfcf42185fb42cfbad2fc3c1ac0d87ecef37f4185      1
    54 0xfcf77ac2cef5eb373d8eb9163f518126cce44f47      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 103 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x009b78ea24b49225ff8d18605ab1eac66fad2caa      1
      2 0x0156c381d411ac0157cd0c171f35b511aa601d3a      1
      3 0x0998fc77b101e4c538f57bc7616c5ecd77deab5a      1
      4 0x09c7b90c32d5c028f468b494969d8249c1eee184      1
      5 0x0a361b9d21f96f70a12bfa790363239a84ab2444      1
      6 0x0b2095ed0377f44e7ee71883469a2983b8ebc96c      1
      7 0x0e2d091fbe9a37ea634277fd94e52e1092bd78a7      1
      8 0x0ff14dad100343a01cb7599aa7485c4892378e74      1
      9 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     10 0x1776cfdcffc21cd51b35d1efaf5b3db4848da1d7      1
     11 0x1b6e35906052964ca519c12a4e1667facd7213fd      1
     12 0x22eef23d58355f08034551f66c194c2752d494c6      1
     13 0x26a186f21b291421aa8bbb42068062f04231a763      1
     14 0x2881f9c535c8145617fdec5b2dc1b870efc4fe57      1
     15 0x29f9ef8286dcc4f9a94340278db01f12c3483988      1
     16 0x2a69c074614319af4ddd130b7f8a84f5b995c21f      1
     17 0x2b615388171efa3814d0ba59c9433189d5f9c5f5      1
     18 0x2c01bece5ca59c32a16f2027347d326cf2758db8      1
     19 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     20 0x316a35ebc7bfb945ab84e8bf6167585602306192      1
     21 0x31f3e61407ab33ecf0df1f3d1ef3383cd500722e      1
     22 0x3216c0744441967e8edd07c0d2dc21c1202cbdb0      1
     23 0x354c2ab3f7a23f74cddc745b26aea53ec1602203      1
     24 0x406a6feb062f3a0709483b8ceab9215dc5be4bd5      1
     25 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
     26 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     27 0x4400595f54c06e119508092a9a6d1cf3d90f2528      1
     28 0x44a84d2253535a4b4285dfdc83d69cc6df18d05f      1
     29 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     30 0x47417c04607445401d54b68e644ef76c67dbf0c1      1
     31 0x486058f0d5c838454a7b5905c3b1ece98b76972c      1
     32 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     33 0x4b97d19596c3b5ecf3737fde98f965876bcae065      1
     34 0x4bff957b07b45acc6dc89c29a4d769c64e4dce53      1
     35 0x4d2fb20b25e39cc6db1e673acbb8c6c467b7c594      1
     36 0x4dc9990baeac4b52a42026738ef58edd9b46f0ae      1
     37 0x4dd7f16dea28f8e68201549dbf0df2e504a36494      1
     38 0x512c579153ac6fd961a7d9e7b19281b855aafbe1      1
     39 0x5290dbe2d1f7060ba2988a4e04b3b4679bc5ceee      1
     40 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
     41 0x559acc031f14350dc7b309baa2195fb2def0351c      1
     42 0x564beabdc1afc0751f1721890856ad4c5e644840      1
     43 0x56d494c7461d3cebea0f1af2be4cfd93c89f9c9c      1
     44 0x57d9be95b12a687d1cc6f327b57338cd85aeea8e      1
     45 0x5812500e79c78d0525c4dd60bb50012d3969e995      1
     46 0x5864bd03561da804ad0dd5a9f901a59217ebe7e4      1
     47 0x592609863e5f7211547419469fe1b662c1516884      1
     48 0x5f8b9b4541ecef965424f1db923806aad626add2      1
     49 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     50 0x62e3e1f39025fdadd7454704c13b7b2ae8c1a8ea      1
     51 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     52 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
     53 0x6f5c58a96f32675453d49585daf03fdfe0d1c784      1
     54 0x75a473c33bffb61e945d86b37113c0859965a789      1
     55 0x801eba41000359eea35d7180099c8072e2c29ecb      1
     56 0x8145f0d31a2e109849cf3fcf80244cf13f1d67e6      1
     57 0x836659081a55d2e5a455ce99b279604678065011      1
     58 0x882e605dbbc419c0159dab599b4f91c15617911b      1
     59 0x8889ebb11295f456541901f50bcb5f382047caac      1
     60 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
     61 0x8f6f410f8e92304690493df40c521a3dab160109      1
     62 0x8f791f061d7f36dc07de081ad88f87d71be1585e      1
     63 0x8fbc5ec90fcb4bd279d166c743a27093d8e56fe7      1
     64 0x950fec7e16506e2f3738d95cc20c2b55e8a970f5      1
     65 0x962b404f03d61b9d578373f2c0eef26f9dda998c      1
     66 0xa355065597f1c213160e664b65beda6cabf07bb0      1
     67 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     68 0xa4646c18ae72c301666d2b094607e6ccbe73bdd0      1
     69 0xa787502e447f236986d898475c1f09de7a1f5c64      1
     70 0xabe2d16a44e5c1165a2e00f281cff9f7a4b3cfda      1
     71 0xad9646c6e0d59e598f5d69e1d3b8dedd46b2da08      1
     72 0xadb1ece7e393132c4a8cfa86a291b039109c4142      1
     73 0xaf8c481eb42defb31ae67319382624a68e28f5e8      1
     74 0xafb2e109b8210e9141fc4656e502605172b0d8c2      1
     75 0xb1c72fee77254725d365be0f9cc1667f94ee7967      1
     76 0xb2d9b21c367a48a540ac555711578aa27d95218f      1
     77 0xb30db59074609f95ac814e0dd03bdd1422523c9e      1
     78 0xb377e3aadc9eb4e72c5cab17050fcdf0931f0fea      1
     79 0xb5dee3d78bb9b876a84e0ae6e43852e080001355      1
     80 0xbbd91ee1a5616922fa25108b4c9eb62aee2f64c5      1
     81 0xbded72add80598afd9e2ec3c5e5fe6aab48b0f89      1
     82 0xc753aebe8aa0998d2e4bc1a9eef467b239a0401d      1
     83 0xcc749c4b3585b8cbb244afbb6c4c790dab4204e0      1
     84 0xcd6cd394b8473b37bbc645d56482b82386f8abaa      1
     85 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
     86 0xd7afbcb452066763184483b5227f8ac9a61203d3      1
     87 0xd8fc6e0dbbda3ade572af2326bc3368415fe23b6      1
     88 0xdd4c061952f784f3f20b34cd55391126dc1e07e0      1
     89 0xe274f53f1d8ac9cf25031e543cb0089b2c475689      1
     90 0xe397f52048e00c4b7377683a392edd80ce91ad02      1
     91 0xe5840064e6ce4923eac2ee381f5ab660617778e7      1
     92 0xe72040e2c1ad97f45390cb68dbe834deef070cde      1
     93 0xe773382a7574de1c82b1f67099e680c043048708      1
     94 0xe8a0cc457405c250a3142db639b3e058fd431510      1
     95 0xe985696a7e48496ebd07cec064b2a965c2d3ce1d      1
     96 0xea290fe57d2916e737fe6795479ddf24dca42075      1
     97 0xea41be885d925260ec1aaac5439ad0a39f74952d      1
     98 0xeec963ad9a7f483a5eb7d845394a424f829b66ee      1
     99 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    100 0xf8af408c09773b093e3cdd850c9b559840e5ad1a      1
    101 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    102 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    103 0xfc3d6045934890e2f59283f8f373998121725291      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 157 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x009b78ea24b49225ff8d18605ab1eac66fad2caa      1
      2 0x0156c381d411ac0157cd0c171f35b511aa601d3a      1
      3 0x0378aa12b626f0cfd18334c10cdb8ee6043d4c64      1
      4 0x06cc86506e45941defcf9b8339eb6c55d038464b      1
      5 0x07f6329cdd8c29a241a0ad8503436c6652f6b509      1
      6 0x0971ccf9d28d09d64411d47cdaf210b6e5826731      1
      7 0x0998fc77b101e4c538f57bc7616c5ecd77deab5a      1
      8 0x09c7b90c32d5c028f468b494969d8249c1eee184      1
      9 0x0a361b9d21f96f70a12bfa790363239a84ab2444      1
     10 0x0b2095ed0377f44e7ee71883469a2983b8ebc96c      1
     11 0x0e2d091fbe9a37ea634277fd94e52e1092bd78a7      1
     12 0x0ff14dad100343a01cb7599aa7485c4892378e74      1
     13 0x107beb3d304cd99535994cf85689bb6713185418      1
     14 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     15 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     16 0x1776cfdcffc21cd51b35d1efaf5b3db4848da1d7      1
     17 0x18717190340a530a4b74bfa823cdc70fdc7d813b      1
     18 0x1b6e35906052964ca519c12a4e1667facd7213fd      1
     19 0x1f51119db1dbd797c2b0eb21e47303ac3e008798      1
     20 0x202ff78c06536a8ac03ab7a46623010333e774ca      1
     21 0x21f315ab7c8f3ab9356fd36c29abc2ab639129ba      1
     22 0x22eef23d58355f08034551f66c194c2752d494c6      1
     23 0x26a186f21b291421aa8bbb42068062f04231a763      1
     24 0x2881f9c535c8145617fdec5b2dc1b870efc4fe57      1
     25 0x29bbb2014ac9c1e73ce88f6be44be8da900f9758      1
     26 0x29d7e1da8daca7a9d1edaec80255bbaf0eef594a      1
     27 0x29f9ef8286dcc4f9a94340278db01f12c3483988      1
     28 0x2a69c074614319af4ddd130b7f8a84f5b995c21f      1
     29 0x2a8f1c9d50e7647a30b2694e676fbc508d9241f9      1
     30 0x2b615388171efa3814d0ba59c9433189d5f9c5f5      1
     31 0x2b99edc6500a2e9313523064c1d77abc92b5108f      1
     32 0x2c01bece5ca59c32a16f2027347d326cf2758db8      1
     33 0x2c65a1617d109ba9c9fe2ff2dfcf5bad8c9770fe      1
     34 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     35 0x316a35ebc7bfb945ab84e8bf6167585602306192      1
     36 0x31f3e61407ab33ecf0df1f3d1ef3383cd500722e      1
     37 0x3216c0744441967e8edd07c0d2dc21c1202cbdb0      1
     38 0x354c2ab3f7a23f74cddc745b26aea53ec1602203      1
     39 0x3f42d65bede435fa3b9c82e91174b61672b79298      1
     40 0x406a6feb062f3a0709483b8ceab9215dc5be4bd5      1
     41 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
     42 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     43 0x4400595f54c06e119508092a9a6d1cf3d90f2528      1
     44 0x44a84d2253535a4b4285dfdc83d69cc6df18d05f      1
     45 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     46 0x47417c04607445401d54b68e644ef76c67dbf0c1      1
     47 0x486058f0d5c838454a7b5905c3b1ece98b76972c      1
     48 0x49317b33a82fb576fba1462364d02569e766e33a      1
     49 0x4b6b892b6878e61f421066a01fc03d0648228f82      1
     50 0x4b97d19596c3b5ecf3737fde98f965876bcae065      1
     51 0x4bff957b07b45acc6dc89c29a4d769c64e4dce53      1
     52 0x4d2fb20b25e39cc6db1e673acbb8c6c467b7c594      1
     53 0x4dc9990baeac4b52a42026738ef58edd9b46f0ae      1
     54 0x4dd7f16dea28f8e68201549dbf0df2e504a36494      1
     55 0x512c579153ac6fd961a7d9e7b19281b855aafbe1      1
     56 0x51794aa02aa38f9427b78259f1a4c2107c8e00ac      1
     57 0x5290dbe2d1f7060ba2988a4e04b3b4679bc5ceee      1
     58 0x54aa7e866378e4ff3140aaf167571111cc9cb5d7      1
     59 0x54becc7560a7be76d72ed76a1f5fee6c5a2a7ab6      1
     60 0x559acc031f14350dc7b309baa2195fb2def0351c      1
     61 0x564beabdc1afc0751f1721890856ad4c5e644840      1
     62 0x56d494c7461d3cebea0f1af2be4cfd93c89f9c9c      1
     63 0x57d9be95b12a687d1cc6f327b57338cd85aeea8e      1
     64 0x5812500e79c78d0525c4dd60bb50012d3969e995      1
     65 0x5864bd03561da804ad0dd5a9f901a59217ebe7e4      1
     66 0x592609863e5f7211547419469fe1b662c1516884      1
     67 0x596dbef7dffaedd60c1df11bc51df3d529821b58      1
     68 0x5f8b9b4541ecef965424f1db923806aad626add2      1
     69 0x6002ebe4144d7f3b0cd10e2ac0c0f357eb1c1c51      1
     70 0x62e3e1f39025fdadd7454704c13b7b2ae8c1a8ea      1
     71 0x633fe643a9e50959133af8e1330489a1c7288f12      1
     72 0x64b5ba690b067687643f66f579ea94786aa04c39      1
     73 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
     74 0x67c87c11250a3dd2c459d896d862b0c599db4248      1
     75 0x6f5c58a96f32675453d49585daf03fdfe0d1c784      1
     76 0x75a473c33bffb61e945d86b37113c0859965a789      1
     77 0x7b7ec26a15eb19995be1323251f4494b50f1c6e5      1
     78 0x801eba41000359eea35d7180099c8072e2c29ecb      1
     79 0x8145f0d31a2e109849cf3fcf80244cf13f1d67e6      1
     80 0x836659081a55d2e5a455ce99b279604678065011      1
     81 0x882e605dbbc419c0159dab599b4f91c15617911b      1
     82 0x8889ebb11295f456541901f50bcb5f382047caac      1
     83 0x89d53e8612e8416bf960dc9444ce0e2a0878a582      1
     84 0x8e5150e73d4ff74706df51bf4329199e0cad7350      1
     85 0x8f6f410f8e92304690493df40c521a3dab160109      1
     86 0x8f791f061d7f36dc07de081ad88f87d71be1585e      1
     87 0x8fbc5ec90fcb4bd279d166c743a27093d8e56fe7      1
     88 0x9237c2ec55dd857536a1e5bd57ed020afa71f814      1
     89 0x950fec7e16506e2f3738d95cc20c2b55e8a970f5      1
     90 0x951038bb372d16180c0afb0f46ab283059154004      1
     91 0x962b404f03d61b9d578373f2c0eef26f9dda998c      1
     92 0x98a06dcdcdef33f584144a5c5b8fcf4d4716fc24      1
     93 0x9fffd9164f5c5808d9e58b9f0493b50f3f737ff2      1
     94 0xa014fbfc0822e5cc380475cb89881e8a41da6357      1
     95 0xa11cf77b3da9670e37bd849b4e71cea8ad4aa5c6      1
     96 0xa355065597f1c213160e664b65beda6cabf07bb0      1
     97 0xa432dc73ee0d60c193c543de42b6ae1e195020ee      1
     98 0xa4646c18ae72c301666d2b094607e6ccbe73bdd0      1
     99 0xa787502e447f236986d898475c1f09de7a1f5c64      1
    100 0xab6ca2017548a170699890214bfd66583a0c1754      1
    101 0xab7473da0af4658ae2879cb641d82e7652721486      1
    102 0xab81377a955fd33034db726bebc7b610bfbdb156      1
    103 0xabb30d469bdf0b8e38c99c2f531e6a3eb35c22d3      1
    104 0xabe2d16a44e5c1165a2e00f281cff9f7a4b3cfda      1
    105 0xad9646c6e0d59e598f5d69e1d3b8dedd46b2da08      1
    106 0xadb1ece7e393132c4a8cfa86a291b039109c4142      1
    107 0xaf8c481eb42defb31ae67319382624a68e28f5e8      1
    108 0xafb2e109b8210e9141fc4656e502605172b0d8c2      1
    109 0xafe27d2049c037915fb73ed30dcebccec516e1c3      1
    110 0xb1c72fee77254725d365be0f9cc1667f94ee7967      1
    111 0xb2d9b21c367a48a540ac555711578aa27d95218f      1
    112 0xb30db59074609f95ac814e0dd03bdd1422523c9e      1
    113 0xb377e3aadc9eb4e72c5cab17050fcdf0931f0fea      1
    114 0xb378a22e54370e114a55b9d1584482e3895a5df6      1
    115 0xb5dee3d78bb9b876a84e0ae6e43852e080001355      1
    116 0xbbd91ee1a5616922fa25108b4c9eb62aee2f64c5      1
    117 0xbded72add80598afd9e2ec3c5e5fe6aab48b0f89      1
    118 0xc753aebe8aa0998d2e4bc1a9eef467b239a0401d      1
    119 0xcc749c4b3585b8cbb244afbb6c4c790dab4204e0      1
    120 0xcd6cd394b8473b37bbc645d56482b82386f8abaa      1
    121 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    122 0xd1547d1bdf27f87d31894899d45df1896ec3d537      1
    123 0xd394d4c661ebfaf82f2444350dfa7eb1f8965763      1
    124 0xd4bae95335c3cb22938b32f1a95a1d42cbf1e74e      1
    125 0xd7afbcb452066763184483b5227f8ac9a61203d3      1
    126 0xd8fc6e0dbbda3ade572af2326bc3368415fe23b6      1
    127 0xdb2538cb865bcf9fa4f830d8d8973037616591f3      1
    128 0xdb881c8fedd6b4c660905f9a41f6d577be51b668      1
    129 0xdb9d5b439493b6858a7b8d11cbf5885a39a07913      1
    130 0xdd4c061952f784f3f20b34cd55391126dc1e07e0      1
    131 0xe1a5b000a6dca024437bfaaf536c3756164b9e5c      1
    132 0xe274f53f1d8ac9cf25031e543cb0089b2c475689      1
    133 0xe397f52048e00c4b7377683a392edd80ce91ad02      1
    134 0xe40dcd3e8587c72cf741293c55d0ecb1fca5a852      1
    135 0xe5840064e6ce4923eac2ee381f5ab660617778e7      1
    136 0xe72040e2c1ad97f45390cb68dbe834deef070cde      1
    137 0xe773382a7574de1c82b1f67099e680c043048708      1
    138 0xe8a0cc457405c250a3142db639b3e058fd431510      1
    139 0xe985696a7e48496ebd07cec064b2a965c2d3ce1d      1
    140 0xe99a3ebb5f7ceead6de9feee8ff4771802d90a12      1
    141 0xea290fe57d2916e737fe6795479ddf24dca42075      1
    142 0xea41be885d925260ec1aaac5439ad0a39f74952d      1
    143 0xecd0d8de2e887f95d38e8ee2b22e41f3ae9df859      1
    144 0xeec963ad9a7f483a5eb7d845394a424f829b66ee      1
    145 0xf1aae566f12e1333a7d7e0a994ad2c1298d4b20c      1
    146 0xf244f986339c50e161f9fe90935580788e4f7aea      1
    147 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    148 0xf4f736875d51c8dd2de361373fccbff62ecc6312      1
    149 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    150 0xf8af408c09773b093e3cdd850c9b559840e5ad1a      1
    151 0xf93b6f2847c4b121453e0f08985a2b7301c53730      1
    152 0xfa2238eeb4e2e5612853bf29f925a36f971f9010      1
    153 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    154 0xfab9a3d37999e12252b47468d2ffd4be15936012      1
    155 0xfc3d6045934890e2f59283f8f373998121725291      1
    156 0xfcf42185fb42cfbad2fc3c1ac0d87ecef37f4185      1
    157 0xfcf77ac2cef5eb373d8eb9163f518126cce44f47      1

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
