
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:503         Length:503         Min.   :1.000   Length:503        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.072                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:503        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20847569 # https://etherscan.io/block/20847569
block_hash <- "0x8f2d1b8e2f9700f8d0249a7f6d33ed55fa04590291431b427f12bf481125dd1a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4498 

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

allow_artist1    <- pick(snapshot, contracts=c("BubaViedma","LucidDreams","Foundation","HorrorSans","ApocalypseFont","Rarible","MakersPlace","MakersPlace2"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("KarmatoBurnEditions","BubaViedmaEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("BubaViedma","LucidDreams","Foundation","HorrorSans","ApocalypseFont","Rarible","MakersPlace","MakersPlace2","KarmatoBurnEditions","BubaViedmaEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x07c68b6062a884bc08a63ec332eb07d024f24d60      1
     2 0x0b4fbdeadc2311f691b67a7bfc77ef7f3e2f8b61      1
     3 0x0bae2fd474615c52233c001a072f3e10553e26f7      1
     4 0x0d427a8f3e9b96a0d7770e634778bba0990754a7      1
     5 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     6 0x13d85ed3f62094dabcdf332e24fd4f118b0d9923      1
     7 0x1b78364999d5fa10086143c2ad04051177490bea      1
     8 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     9 0x1e454cfca1aca1cc9288270a46430b118ad1b4da      1
    10 0x1e857c3ea66e7dbc9f1d41992556accaa519b8a5      1
    11 0x20de677a611e81d4fe3cb866e63a32f08544bc18      1
    12 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
    13 0x291121da7faeedd25cefc0e289b359de52b8050c      1
    14 0x2add49799c873df7a9c158567af2770205cb997d      1
    15 0x2ae5f36c77a736f76d61f3eec06f12caf2963fd6      1
    16 0x3ac713d059b21c62c4c8d475d4a7991dfdffaeb3      1
    17 0x3d102e0d819095c7b9b278598cac5e7a12ba1310      1
    18 0x404437b4644fe2fc2cc5293f74fa6cf3daa61d77      1
    19 0x4a7a76e2c061ac51d7794b2a9003338ab1f13d68      1
    20 0x4da3f1255abf44fbd20b068223a7e4e539bf6ebe      1
    21 0x5a9981d3241910797d5f930b8dd80a0705a3929a      1
    22 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
    23 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
    24 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    25 0x6519e6117480d140cd7d33163ac30fd01812f34a      1
    26 0x685864d615cdbeb95fe1f286ffdc436fc22f0b74      1
    27 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    28 0x6b21fddfca1ce1985490cf630f9e5fac465ab55c      1
    29 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    30 0x70850885537eac231440d8391aabe8dddf094561      1
    31 0x77095648d600b4693852166d6a19500a382bfbdc      1
    32 0x787349b0c7d3bdf2583f2ba586b4990074593be1      1
    33 0x79dd8fab0661da2cd4131bb454bab060576ce2ee      1
    34 0x7a267ab259c4369c44112a67dae90afa858f2b35      1
    35 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    36 0x80fcfdd18c0b83a1827f5964d4d1b214db16adc5      1
    37 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    38 0x8a212825bbc75154931d0e1e098883cfbe2e51cf      1
    39 0x8d4120e117ce17727d0c0566aba161d94e11b8af      1
    40 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    41 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    42 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    43 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    44 0x9b5c8aabf848a89b1378a930d42fb094a7d94c74      1
    45 0x9bc20560301cdc15c3190f745b6d910167d4b467      1
    46 0xa76e4e20772f075b76472a1449e8461a1ccefbc0      1
    47 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    48 0xaea57cbf427757d9d51f0fb024cd7dfeb15c0275      1
    49 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    50 0xc1894e6a52c4c7ac5b2e0b25583ea48bf45da14a      1
    51 0xc49786d5546edef7659621ff1746ca6baf6c50a4      1
    52 0xc5b46513ea71294ba740e115f88b697c26d25c8b      1
    53 0xca38f02d7594dbd1c9eefae33bac5da04f79abf2      1
    54 0xcb59a5e878527e6a311ef5b4786e2845424a1a5d      1
    55 0xcebb48fa75c8d4a339c7bfd85a80748b59df6302      1
    56 0xd84f2a1e504f15b413ca9feea52135f01c5c775a      1
    57 0xdb2ba0eabbfe920d4e501dfd92d6667f512e5870      1
    58 0xe0f66ac65020ee81eb612ac865e9a35fb80ede8e      1
    59 0xe50cebc2f2888cfe2ded1b69ed5618ec79e243d2      1
    60 0xe57fa34b4000ffd409f13eefe46d892a3d31a8f9      1
    61 0xe64b416c651a02f68566c7c2e38c19fae820e105      1
    62 0xe7bd51dc30d4bdc9fddd42ea7c0a283590c9d416      1
    63 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    64 0xedcf5d6ddc5c8b64dd6927cab50a5b7fb3e50abd      1
    65 0xf1b867bbc9a36320866d51cd020cd356d2cfb767      1
    66 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    67 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    68 0xfeae8ec4afee29c939ca62749e2e6329c27d48ce      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 176 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      2 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      3 0x025fdd585f03ce846740fe5542469f1de425e439      1
      4 0x05a900e3728403fe758ebb3cae93e24e738bfa2f      1
      5 0x06395478361f2e7c5aa26a380d37876eb5cc1854      1
      6 0x07efd3c7a6dd4816c053d85cf6612b2ecae78eca      1
      7 0x0998fc77b101e4c538f57bc7616c5ecd77deab5a      1
      8 0x0bf8422f0a124e664f4ac2ec42cfe50afe8b8906      1
      9 0x0e4885b96aa3834e328d217dd2eec6012bb8d28c      1
     10 0x0f3430417f21637dfda49af9b0a310d799b3807d      1
     11 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     12 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     13 0x150b22ecc421a949286662e5c689d2cee8b481e5      1
     14 0x1576d7992762ae6c900ba9a3f15f02ed79d057d1      1
     15 0x18bd67edbcb6e1072c0ffdca2add8fec805246db      1
     16 0x1b194e94ee7cde8fc301f0c6faa8f161480a682e      1
     17 0x1ce1a58cd5705f386e07cc9d49b8812c5ee40b3f      1
     18 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     19 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     20 0x1ffe9c766d1b7cd775391b8b2c46e51aa6f0a038      1
     21 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     22 0x22514dce2d16ea7cd40b506592d69a93699f1e37      1
     23 0x2538fff9c6b0fe9d6985115bc014de0af3e260d3      1
     24 0x262a362221545ff279fef4748766bcccbcb9c24e      1
     25 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     26 0x281b5dce9cc3a3efab49b7e867beb76becbbf635      1
     27 0x297e3e1b23cdf212d93885cdea78fad626b31cb7      1
     28 0x2b6fb56283e6c68ef3571138b18064a6039d2708      1
     29 0x2d49b8b129c57926a0e655e0ba96f6e02b78d6ac      1
     30 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     31 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
     32 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     33 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
     34 0x3842d2313df449d11a7bac8652e0cc0edd9ae1a4      1
     35 0x3846c16117f7f119fe6495d28d410f66f275a589      1
     36 0x3989055ea5f7279bfca300a1dade00b64ba2afb7      1
     37 0x3c4fbefc1f215d7caf8c393efa1f9d54fdc62f19      1
     38 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
     39 0x3f0f7667e627473aea57e66839a421fee7bb2534      1
     40 0x3f5e01ee8439e8c0ba87025960bda715626b79a9      1
     41 0x41d870c141f7a6ae406ddc5f5de566499714b8c6      1
     42 0x4501aeb9672232c02fa717c8d69f29ec0db80a3b      1
     43 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
     44 0x47062d53fb6cf7686477c400a3c9eede9a874010      1
     45 0x476b535cf58dbdb440fcd4e8bc21c847b70a5314      1
     46 0x47842cce1bc77e05ca6c7d8e9cbe60bdc01a5e61      1
     47 0x478bb542f7658d635abba67edb987806dff5b83d      1
     48 0x4799b18582cbb51cc207ed9716034da309edf47e      1
     49 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     50 0x486752e4c8ab22f5abb1feb95abc7ce0a36473da      1
     51 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
     52 0x49fbb9f90e78dd1c0569816e1eeeaf2c28414ed7      1
     53 0x4c9a65dc425f512e4b4b57e47fd760a0a5123bd4      1
     54 0x4df00689952974edbe1cc05d2710ea1b2b6e185f      1
     55 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     56 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     57 0x50e4a02362373d9dae47f340c82fa146e1d06158      1
     58 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
     59 0x52e7f5797ff7eed52c85c78c8102b187db4b76c0      1
     60 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
     61 0x5381315a97df70fc1f47fe8489efb12ad48fc187      1
     62 0x53adfd2fd44b5222206091f8475cde1a53d7e3e0      1
     63 0x53d294c5eb6c44912aa8d11121901af095b70358      1
     64 0x546937734ce4d9553bf70b9c18ad9d4408aab94f      1
     65 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
     66 0x57b76626a420772ddfb5ec32379649c4318c3f96      1
     67 0x59867b4b17cb1d04e5f9c8f2699227427377bd9c      1
     68 0x5c835eb9ece4c2c5786251d787ca2ea8c5020b38      1
     69 0x5dca7bad26550b04a2d6911ba64bb7e7bdd67787      1
     70 0x5e0e0e105e3941b648d511a4bcc4a63b4088242a      1
     71 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
     72 0x5fe1ecfc22c2bce40b5645396011252c5f877036      1
     73 0x60559b4cc92292b589ded75295f5331adcc69e04      1
     74 0x60637813f341215718738caa006d0f1196c24421      1
     75 0x616a731c9d49fe681b5e1efac03b25b8ca23c156      1
     76 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
     77 0x66b73393b0d95f037215befbbb5223915cb3fa22      1
     78 0x66ec5ebf30c230976cfdf808a17fbd0add302537      1
     79 0x67691771270f0199bfe54a00f8343d15afc5b872      1
     80 0x6c0c160412f91a9adff0951b23186c87114f2aab      1
     81 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
     82 0x6ef97d5df6ac8f0a6a14c138f1aca7fc71b008dd      1
     83 0x727fb80e6ab480c1cd34d55a63fce6395df7c642      1
     84 0x73954a7081ae0be4a0b1d3bdea98666ea8aa0ba8      1
     85 0x75ef2eff7b12f5f900ef5c787036deab18423d1a      1
     86 0x762da606029d3120735aa1eec15464e265db7a3c      1
     87 0x76807f3d69b1d38f9d597e902f18dab0af926b8b      1
     88 0x7b42a219bb14d0719757a391d7cc6aa7f371e144      1
     89 0x7c846e2487a8b6ab7a4ceec6a2073c5ce388e2ff      1
     90 0x7e5baaa750e052187b9972b9f3ba8ef908b8dead      1
     91 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
     92 0x8300f444462e55d11dba5823a1584badfc293fd9      1
     93 0x83706775b6ff7844bde88df19499606d68f056d0      1
     94 0x846ee08276888189580c60941919f144390731e4      1
     95 0x86fc973fc5146132afd4421497d031a032e2de71      1
     96 0x8889ebb11295f456541901f50bcb5f382047caac      1
     97 0x8a6b368ecb5abc8451160a01029c5eb30f341112      1
     98 0x8ae212fa8255d5d5a0821ed605f1ea5fbeeab3a8      1
     99 0x8ce69f3f36465366de7dce5fc2137ebe37a24122      1
    100 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    101 0x91b6f978ddfea98ec9fef489e68b7f92b141aef3      1
    102 0x91c45d69722bbd601b5518fbae45ae8ad8b64457      1
    103 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    104 0x965f2225bc4657ad9e1a892e6299db312f2d5588      1
    105 0x966a2359de1bf205f083fe40a8ec2f21b911a433      1
    106 0x97d9aeade7a1e2e9be6fcd85450f9dce9d08fdcf      1
    107 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
    108 0x9a52b14ea7049850796c27062df5da8fbb99080d      1
    109 0x9b0726e95e72eb6f305b472828b88d2d2bdd41c7      1
    110 0x9b1df4dadfa3db4e2c95b682c29bc44af5c68db7      1
    111 0x9bf8fb41c325de460ae7e2c7e09bd9863c2d5452      1
    112 0x9d1b972e7cee2317e24719de943b2da0b9435454      1
    113 0x9f3dfd6966a4ce1a4c6053509a78fb4d53ad954e      1
    114 0x9fbed27f9643d0d0f302b3f041612f137012a8ac      1
    115 0xa0bdf16f3c91633838ad715a4bc7e8b406093340      1
    116 0xa0d4e6501529d25aa3fcd5d473ab859235da49a9      1
    117 0xa1a272bb3d92330553e8119e2994cc730b5d8ba1      1
    118 0xa1a8abdc2da3735ad37d77cdc4b0d0e2dacda274      1
    119 0xa9c28eb83763d33ebeb279109f1b3913774fb01c      1
    120 0xb2e1462847e6244f9931915ea2294005643b4861      1
    121 0xb8768094e62b2014e1a6895d3b85dbf0ed656290      1
    122 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    123 0xb8f203a52262359a57737ebba48723dfc56aeb23      1
    124 0xba9c57d2426e593526ec597e4016fa5ef0e97e38      1
    125 0xbac7fe1787906e7f8eb3302779cc78c2201ca5af      1
    126 0xbac9b3d875d2b217db635dfcb4cb36a3dbe3367d      1
    127 0xbb1f3d13e39f6c951a6b5daad48971f20bfd072f      1
    128 0xbb2844146f40689b1fe38460935e31830204c104      1
    129 0xbbc2b5a257949fa89c89272938f66d247f94f64d      1
    130 0xbe39ad6d10802b6ac0943eab3e6c6b1884a054c4      1
    131 0xbf017ede8844349848ff5cf987c50ee3b40d77c0      1
    132 0xbfe856e4968b0a91b61342c8fdce7e0376ba1894      1
    133 0xc1923cae3b5ff75c87a1cefa8e80e2985e1232a8      1
    134 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    135 0xc6411ca5a80deb7abc0827da82e2ddb9c906614a      1
    136 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    137 0xc8f2f8978798d4e8a8f89df6e68866cbf454d926      1
    138 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    139 0xcdd4379ab6e38301288fee3ab23e48138a0ddb16      1
    140 0xce180b7fec4d74ce6da29714ce5bde22fe3035b1      1
    141 0xcff26de23c8243f421e3a2829f447cf0902571cf      1
    142 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    143 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    144 0xd297bebecdecde66d49243934e243bf31c4f06f6      1
    145 0xd385605623db1be4974838829703b8e29124bf37      1
    146 0xd430fc849c60107201ea4f569108716a90ab65e2      1
    147 0xd595710aedaba1d2e18bfdbab46ab796664c6d89      1
    148 0xd61a6fb2c107a02459f069a6701debb45ef21fd4      1
    149 0xd84c9774ddf51a5d1fcfbe022f9d93e36eb4c23c      1
    150 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    151 0xdd6fd318bc85a98ba5f97680c34a946ffd8122f5      1
    152 0xde88c617aed023248fdacd36e3bf0171c848b852      1
    153 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    154 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    155 0xe2413913c5714dfb881e173e95bdb826fd40603d      1
    156 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    157 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    158 0xe510058152640c40266d910eea4c42bf58ced7f2      1
    159 0xe51ff9ef0cb02a8715729de3da80e0f438033b14      1
    160 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    161 0xe76a5119e2f920b439ac2cf021304c668bd2f82e      1
    162 0xe7d11c8601ecd7cfbf3bd5b0f74157773979ea84      1
    163 0xe8679c9dcd079459d1a23c455603f17c0ec007e4      1
    164 0xea06c7b29febe294c934eb8bfdec476399063a5f      1
    165 0xf19aed29a6cd4f5a1b3c8ed79d987e3fc1dca853      1
    166 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    167 0xf4e7d8a84884e308fb82ffd472d937af1c0aa464      1
    168 0xf5a03163d134f3accf6cfc5da8a93b96f6fdb69a      1
    169 0xf60aa01430f70d6aecd8b049bbd40d53554f908c      1
    170 0xf77b8f94301ca39ecfce417315f0dc1eb1fea02f      1
    171 0xf94ba062308ea92f7ab3cf55c4b410339717c74d      1
    172 0xf9c298545da80df7767c4e5a9296a071f13702b8      1
    173 0xfb42e046c2c46d007e626999514339818c8ae7b1      1
    174 0xfbc1c78ea33496aa0d4e33f430e3def42014195f      1
    175 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    176 0xfeed2eab7fc2c43d06f55ecd5ef5db5f2fe77935      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 244 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      2 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      3 0x025fdd585f03ce846740fe5542469f1de425e439      1
      4 0x05a900e3728403fe758ebb3cae93e24e738bfa2f      1
      5 0x06395478361f2e7c5aa26a380d37876eb5cc1854      1
      6 0x07c68b6062a884bc08a63ec332eb07d024f24d60      1
      7 0x07efd3c7a6dd4816c053d85cf6612b2ecae78eca      1
      8 0x0998fc77b101e4c538f57bc7616c5ecd77deab5a      1
      9 0x0b4fbdeadc2311f691b67a7bfc77ef7f3e2f8b61      1
     10 0x0bae2fd474615c52233c001a072f3e10553e26f7      1
     11 0x0bf8422f0a124e664f4ac2ec42cfe50afe8b8906      1
     12 0x0d427a8f3e9b96a0d7770e634778bba0990754a7      1
     13 0x0e4885b96aa3834e328d217dd2eec6012bb8d28c      1
     14 0x0f3430417f21637dfda49af9b0a310d799b3807d      1
     15 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     16 0x1340fc92d9f817d41f76a5a90c188d8633c307d3      1
     17 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     18 0x13d85ed3f62094dabcdf332e24fd4f118b0d9923      1
     19 0x150b22ecc421a949286662e5c689d2cee8b481e5      1
     20 0x1576d7992762ae6c900ba9a3f15f02ed79d057d1      1
     21 0x18bd67edbcb6e1072c0ffdca2add8fec805246db      1
     22 0x1b194e94ee7cde8fc301f0c6faa8f161480a682e      1
     23 0x1b78364999d5fa10086143c2ad04051177490bea      1
     24 0x1ce1a58cd5705f386e07cc9d49b8812c5ee40b3f      1
     25 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     26 0x1e454cfca1aca1cc9288270a46430b118ad1b4da      1
     27 0x1e857c3ea66e7dbc9f1d41992556accaa519b8a5      1
     28 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     29 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     30 0x1ffe9c766d1b7cd775391b8b2c46e51aa6f0a038      1
     31 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
     32 0x20de677a611e81d4fe3cb866e63a32f08544bc18      1
     33 0x22514dce2d16ea7cd40b506592d69a93699f1e37      1
     34 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     35 0x2538fff9c6b0fe9d6985115bc014de0af3e260d3      1
     36 0x262a362221545ff279fef4748766bcccbcb9c24e      1
     37 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     38 0x281b5dce9cc3a3efab49b7e867beb76becbbf635      1
     39 0x291121da7faeedd25cefc0e289b359de52b8050c      1
     40 0x297e3e1b23cdf212d93885cdea78fad626b31cb7      1
     41 0x2add49799c873df7a9c158567af2770205cb997d      1
     42 0x2ae5f36c77a736f76d61f3eec06f12caf2963fd6      1
     43 0x2b6fb56283e6c68ef3571138b18064a6039d2708      1
     44 0x2d49b8b129c57926a0e655e0ba96f6e02b78d6ac      1
     45 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     46 0x33d00dce2942a33e2b553b0b206b24f160799bb9      1
     47 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     48 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
     49 0x3842d2313df449d11a7bac8652e0cc0edd9ae1a4      1
     50 0x3846c16117f7f119fe6495d28d410f66f275a589      1
     51 0x3989055ea5f7279bfca300a1dade00b64ba2afb7      1
     52 0x3ac713d059b21c62c4c8d475d4a7991dfdffaeb3      1
     53 0x3c4fbefc1f215d7caf8c393efa1f9d54fdc62f19      1
     54 0x3ccaa6158ac53054fccb904dfc1407a18befe3c9      1
     55 0x3d102e0d819095c7b9b278598cac5e7a12ba1310      1
     56 0x3f0f7667e627473aea57e66839a421fee7bb2534      1
     57 0x3f5e01ee8439e8c0ba87025960bda715626b79a9      1
     58 0x404437b4644fe2fc2cc5293f74fa6cf3daa61d77      1
     59 0x41d870c141f7a6ae406ddc5f5de566499714b8c6      1
     60 0x4501aeb9672232c02fa717c8d69f29ec0db80a3b      1
     61 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
     62 0x47062d53fb6cf7686477c400a3c9eede9a874010      1
     63 0x476b535cf58dbdb440fcd4e8bc21c847b70a5314      1
     64 0x47842cce1bc77e05ca6c7d8e9cbe60bdc01a5e61      1
     65 0x478bb542f7658d635abba67edb987806dff5b83d      1
     66 0x4799b18582cbb51cc207ed9716034da309edf47e      1
     67 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     68 0x486752e4c8ab22f5abb1feb95abc7ce0a36473da      1
     69 0x4943cc7813da1697265fded90a2bfb749eba0d89      1
     70 0x49fbb9f90e78dd1c0569816e1eeeaf2c28414ed7      1
     71 0x4a7a76e2c061ac51d7794b2a9003338ab1f13d68      1
     72 0x4c9a65dc425f512e4b4b57e47fd760a0a5123bd4      1
     73 0x4da3f1255abf44fbd20b068223a7e4e539bf6ebe      1
     74 0x4df00689952974edbe1cc05d2710ea1b2b6e185f      1
     75 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
     76 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
     77 0x50e4a02362373d9dae47f340c82fa146e1d06158      1
     78 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
     79 0x52e7f5797ff7eed52c85c78c8102b187db4b76c0      1
     80 0x52f9b3aa0cb7cad168e4756f9d3306d0b59c526b      1
     81 0x5381315a97df70fc1f47fe8489efb12ad48fc187      1
     82 0x53adfd2fd44b5222206091f8475cde1a53d7e3e0      1
     83 0x53d294c5eb6c44912aa8d11121901af095b70358      1
     84 0x546937734ce4d9553bf70b9c18ad9d4408aab94f      1
     85 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
     86 0x57b76626a420772ddfb5ec32379649c4318c3f96      1
     87 0x59867b4b17cb1d04e5f9c8f2699227427377bd9c      1
     88 0x5a9981d3241910797d5f930b8dd80a0705a3929a      1
     89 0x5c835eb9ece4c2c5786251d787ca2ea8c5020b38      1
     90 0x5dca7bad26550b04a2d6911ba64bb7e7bdd67787      1
     91 0x5e0e0e105e3941b648d511a4bcc4a63b4088242a      1
     92 0x5efdb6d8c798c2c2bea5b1961982a5944f92a5c1      1
     93 0x5f2bdf26f6528ce05aac77d7fa52bac7a836ef66      1
     94 0x5f4ef1da0a7b2181d321f67de9efd7cbc4b32868      1
     95 0x5fe1ecfc22c2bce40b5645396011252c5f877036      1
     96 0x60559b4cc92292b589ded75295f5331adcc69e04      1
     97 0x60637813f341215718738caa006d0f1196c24421      1
     98 0x616a731c9d49fe681b5e1efac03b25b8ca23c156      1
     99 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
    100 0x645fd16f3508391e8385fc0a181076994079aa1b      1
    101 0x6519e6117480d140cd7d33163ac30fd01812f34a      1
    102 0x66b73393b0d95f037215befbbb5223915cb3fa22      1
    103 0x66ec5ebf30c230976cfdf808a17fbd0add302537      1
    104 0x67691771270f0199bfe54a00f8343d15afc5b872      1
    105 0x685864d615cdbeb95fe1f286ffdc436fc22f0b74      1
    106 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    107 0x6b21fddfca1ce1985490cf630f9e5fac465ab55c      1
    108 0x6c0c160412f91a9adff0951b23186c87114f2aab      1
    109 0x6d2ad2c79a8a8c4a6c77140dfc56bda96e0ddac3      1
    110 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    111 0x6ef97d5df6ac8f0a6a14c138f1aca7fc71b008dd      1
    112 0x70850885537eac231440d8391aabe8dddf094561      1
    113 0x727fb80e6ab480c1cd34d55a63fce6395df7c642      1
    114 0x73954a7081ae0be4a0b1d3bdea98666ea8aa0ba8      1
    115 0x75ef2eff7b12f5f900ef5c787036deab18423d1a      1
    116 0x762da606029d3120735aa1eec15464e265db7a3c      1
    117 0x76807f3d69b1d38f9d597e902f18dab0af926b8b      1
    118 0x77095648d600b4693852166d6a19500a382bfbdc      1
    119 0x787349b0c7d3bdf2583f2ba586b4990074593be1      1
    120 0x79dd8fab0661da2cd4131bb454bab060576ce2ee      1
    121 0x7a267ab259c4369c44112a67dae90afa858f2b35      1
    122 0x7b42a219bb14d0719757a391d7cc6aa7f371e144      1
    123 0x7c3018a930e2d0d060069ac1ec38dbf9619e20be      1
    124 0x7c846e2487a8b6ab7a4ceec6a2073c5ce388e2ff      1
    125 0x7e5baaa750e052187b9972b9f3ba8ef908b8dead      1
    126 0x80fcfdd18c0b83a1827f5964d4d1b214db16adc5      1
    127 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    128 0x8300f444462e55d11dba5823a1584badfc293fd9      1
    129 0x83706775b6ff7844bde88df19499606d68f056d0      1
    130 0x846ee08276888189580c60941919f144390731e4      1
    131 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    132 0x86fc973fc5146132afd4421497d031a032e2de71      1
    133 0x8889ebb11295f456541901f50bcb5f382047caac      1
    134 0x8a212825bbc75154931d0e1e098883cfbe2e51cf      1
    135 0x8a6b368ecb5abc8451160a01029c5eb30f341112      1
    136 0x8ae212fa8255d5d5a0821ed605f1ea5fbeeab3a8      1
    137 0x8ce69f3f36465366de7dce5fc2137ebe37a24122      1
    138 0x8d4120e117ce17727d0c0566aba161d94e11b8af      1
    139 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    140 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    141 0x91b6f978ddfea98ec9fef489e68b7f92b141aef3      1
    142 0x91c45d69722bbd601b5518fbae45ae8ad8b64457      1
    143 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    144 0x93ab3c4e6daa20b2b7ea65754a8c11311fbdba38      1
    145 0x965f2225bc4657ad9e1a892e6299db312f2d5588      1
    146 0x966a2359de1bf205f083fe40a8ec2f21b911a433      1
    147 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    148 0x97d9aeade7a1e2e9be6fcd85450f9dce9d08fdcf      1
    149 0x98be4c67bb47ce927b3ce8a3a3af1489b4493a49      1
    150 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    151 0x9a52b14ea7049850796c27062df5da8fbb99080d      1
    152 0x9b0726e95e72eb6f305b472828b88d2d2bdd41c7      1
    153 0x9b1df4dadfa3db4e2c95b682c29bc44af5c68db7      1
    154 0x9b5c8aabf848a89b1378a930d42fb094a7d94c74      1
    155 0x9bc20560301cdc15c3190f745b6d910167d4b467      1
    156 0x9bf8fb41c325de460ae7e2c7e09bd9863c2d5452      1
    157 0x9d1b972e7cee2317e24719de943b2da0b9435454      1
    158 0x9f3dfd6966a4ce1a4c6053509a78fb4d53ad954e      1
    159 0x9fbed27f9643d0d0f302b3f041612f137012a8ac      1
    160 0xa0bdf16f3c91633838ad715a4bc7e8b406093340      1
    161 0xa0d4e6501529d25aa3fcd5d473ab859235da49a9      1
    162 0xa1a272bb3d92330553e8119e2994cc730b5d8ba1      1
    163 0xa1a8abdc2da3735ad37d77cdc4b0d0e2dacda274      1
    164 0xa76e4e20772f075b76472a1449e8461a1ccefbc0      1
    165 0xa812ae608d18b528da5683cdc77f6334c706ef74      1
    166 0xa9c28eb83763d33ebeb279109f1b3913774fb01c      1
    167 0xaea57cbf427757d9d51f0fb024cd7dfeb15c0275      1
    168 0xb2e1462847e6244f9931915ea2294005643b4861      1
    169 0xb8768094e62b2014e1a6895d3b85dbf0ed656290      1
    170 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    171 0xb8dc9d4bbeb6705fdba1f89458a4bc2a2066a6c9      1
    172 0xb8f203a52262359a57737ebba48723dfc56aeb23      1
    173 0xba9c57d2426e593526ec597e4016fa5ef0e97e38      1
    174 0xbac7fe1787906e7f8eb3302779cc78c2201ca5af      1
    175 0xbac9b3d875d2b217db635dfcb4cb36a3dbe3367d      1
    176 0xbb1f3d13e39f6c951a6b5daad48971f20bfd072f      1
    177 0xbb2844146f40689b1fe38460935e31830204c104      1
    178 0xbbc2b5a257949fa89c89272938f66d247f94f64d      1
    179 0xbe39ad6d10802b6ac0943eab3e6c6b1884a054c4      1
    180 0xbf017ede8844349848ff5cf987c50ee3b40d77c0      1
    181 0xbfe856e4968b0a91b61342c8fdce7e0376ba1894      1
    182 0xc1894e6a52c4c7ac5b2e0b25583ea48bf45da14a      1
    183 0xc1923cae3b5ff75c87a1cefa8e80e2985e1232a8      1
    184 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    185 0xc49786d5546edef7659621ff1746ca6baf6c50a4      1
    186 0xc5b46513ea71294ba740e115f88b697c26d25c8b      1
    187 0xc6411ca5a80deb7abc0827da82e2ddb9c906614a      1
    188 0xc7ec028c706ec2fcfdcf66bba7e80ded634f6ca9      1
    189 0xc8f2f8978798d4e8a8f89df6e68866cbf454d926      1
    190 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    191 0xca38f02d7594dbd1c9eefae33bac5da04f79abf2      1
    192 0xcb59a5e878527e6a311ef5b4786e2845424a1a5d      1
    193 0xcdd4379ab6e38301288fee3ab23e48138a0ddb16      1
    194 0xce180b7fec4d74ce6da29714ce5bde22fe3035b1      1
    195 0xcebb48fa75c8d4a339c7bfd85a80748b59df6302      1
    196 0xcff26de23c8243f421e3a2829f447cf0902571cf      1
    197 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    198 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    199 0xd297bebecdecde66d49243934e243bf31c4f06f6      1
    200 0xd385605623db1be4974838829703b8e29124bf37      1
    201 0xd430fc849c60107201ea4f569108716a90ab65e2      1
    202 0xd595710aedaba1d2e18bfdbab46ab796664c6d89      1
    203 0xd61a6fb2c107a02459f069a6701debb45ef21fd4      1
    204 0xd84c9774ddf51a5d1fcfbe022f9d93e36eb4c23c      1
    205 0xd84f2a1e504f15b413ca9feea52135f01c5c775a      1
    206 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    207 0xdb2ba0eabbfe920d4e501dfd92d6667f512e5870      1
    208 0xdd6fd318bc85a98ba5f97680c34a946ffd8122f5      1
    209 0xde88c617aed023248fdacd36e3bf0171c848b852      1
    210 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    211 0xe0f66ac65020ee81eb612ac865e9a35fb80ede8e      1
    212 0xe1759f0cd1358c049e7f64c26e13de9492f9f888      1
    213 0xe2413913c5714dfb881e173e95bdb826fd40603d      1
    214 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    215 0xe3efd57b602dc396f7bcdcca34b03890bd53bd6d      1
    216 0xe50cebc2f2888cfe2ded1b69ed5618ec79e243d2      1
    217 0xe510058152640c40266d910eea4c42bf58ced7f2      1
    218 0xe51ff9ef0cb02a8715729de3da80e0f438033b14      1
    219 0xe57fa34b4000ffd409f13eefe46d892a3d31a8f9      1
    220 0xe64b416c651a02f68566c7c2e38c19fae820e105      1
    221 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
    222 0xe76a5119e2f920b439ac2cf021304c668bd2f82e      1
    223 0xe7bd51dc30d4bdc9fddd42ea7c0a283590c9d416      1
    224 0xe7d11c8601ecd7cfbf3bd5b0f74157773979ea84      1
    225 0xe8679c9dcd079459d1a23c455603f17c0ec007e4      1
    226 0xea06c7b29febe294c934eb8bfdec476399063a5f      1
    227 0xec9e512fe7e90134d8ca7295329ccb0a57c91ecb      1
    228 0xedcf5d6ddc5c8b64dd6927cab50a5b7fb3e50abd      1
    229 0xf19aed29a6cd4f5a1b3c8ed79d987e3fc1dca853      1
    230 0xf1b867bbc9a36320866d51cd020cd356d2cfb767      1
    231 0xf43b2be4aa887f426f05f78604b364af667c608d      1
    232 0xf4e7d8a84884e308fb82ffd472d937af1c0aa464      1
    233 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    234 0xf5a03163d134f3accf6cfc5da8a93b96f6fdb69a      1
    235 0xf60aa01430f70d6aecd8b049bbd40d53554f908c      1
    236 0xf62c22a4fb55c6ef0e2d298d56fb9fd781de0f2c      1
    237 0xf77b8f94301ca39ecfce417315f0dc1eb1fea02f      1
    238 0xf94ba062308ea92f7ab3cf55c4b410339717c74d      1
    239 0xf9c298545da80df7767c4e5a9296a071f13702b8      1
    240 0xfb42e046c2c46d007e626999514339818c8ae7b1      1
    241 0xfbc1c78ea33496aa0d4e33f430e3def42014195f      1
    242 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    243 0xfeae8ec4afee29c939ca62749e2e6329c27d48ce      1
    244 0xfeed2eab7fc2c43d06f55ecd5ef5db5f2fe77935      1

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
