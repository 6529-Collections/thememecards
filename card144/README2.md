
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:11997       Length:11997       Min.   :1   Length:11997      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:11997      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18118769 # https://etherscan.io/block/18118769
block_hash <- "0x451405b5498bfcf4e641cbd23fc4c43cd9e5f7785d6570bbe76e0d5161d156da"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4707 

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

allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=300,address_max=1)
```

## Allow Random Memes 2 Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random300Memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000dc53f3cdc01c055fa7bea4f5192c1cb43990d      1
      2 0x00dbe3800f1988e6a027b56b3c0bd3a30b6e7b69      1
      3 0x0223b033fe732601907a9ba50406082c39266221      1
      4 0x0303d8bbc845047b9af305f776f747ca1e25086c      1
      5 0x0341e935efa5150b84003869c9212ea68aeb3b5a      1
      6 0x03ecf76f0607ec754f3074c441bcc1857cceca19      1
      7 0x04aef0c8306c1ecb8cf17ff152c9b62e01282c1b      1
      8 0x053e6294400a9268e35df445624f58087c7f388f      1
      9 0x07eb4f3273cfdb805c6baff38357d43d6c28844b      1
     10 0x08bac1d3c16fe76edba4d41c600462069eee33aa      1
     11 0x0a8f4c21a980b0a76b4f7071cb4713d8a74753f1      1
     12 0x0ef024d299cb56805f2437cd00b8a361a7b06d54      1
     13 0x0f82f6b926d26621ac0acdaef7c0cf461f6430fd      1
     14 0x10eea07f522633557a4eb00bff16c2f466d49983      1
     15 0x11027c71a31f0eb416c6a1067c43a61f45574468      1
     16 0x114837ea03ed4e5bdbd6471cfa4dd80d68563dc5      1
     17 0x118638c389c7148017cc5b85d1f6d1fd25f76285      1
     18 0x1315cf6ab4ca1f0b1ff38cda0dff0282b26cc023      1
     19 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     20 0x1439b4d5a72343b68f12398c649df99d76b2af53      1
     21 0x15698a313fce62baed7a64dba44849977d98e8c0      1
     22 0x15e3bf5f0158afd884a85defea6ffa9056ef871f      1
     23 0x1602c6f0228761fd380b4e4cb6d7aa04c5683762      1
     24 0x1666b49d92d86e280d85e74eacf345f4830e0cb1      1
     25 0x166c9a16c8b9fda9d69950f2cd59a92b34b7954b      1
     26 0x16c3676077017b856dff4514170332abd7dc2729      1
     27 0x170fb8f8f37776dc48184686dec386d2d1c41cc9      1
     28 0x1951429db875b8954776a19980dc7aa816fe9402      1
     29 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     30 0x1b6695144282442fc85e85163930088df004e0e7      1
     31 0x1b8d65dcad712e1d0470fa8fd1f00956c2bc43c1      1
     32 0x1c38f0d96357168d8d13afdf8225f55f743bc4af      1
     33 0x1c3f8d46b9db827548078a2e7dd46cb487eed0e1      1
     34 0x1c9365a7bef98384401d513bbfa3416ebfe99578      1
     35 0x1cb3acfd25eb1d1db28ae60cbb78bda352297266      1
     36 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     37 0x1ed73cd65a1ebef57ef61022017f998705787fdc      1
     38 0x1f57b7cf79f16e1a4e54d2bb881851679370be44      1
     39 0x1f6278bd02a577f057f5e17d2af6cf008d508bbf      1
     40 0x1febadf986428f0888469b0a5e8f6d791496e3c0      1
     41 0x200b8319c8e962326771fc557d3c3e77063656eb      1
     42 0x20f1191e4051299d1a4e54f8ba95b636ab427898      1
     43 0x222fbcd2b08137638dcdcd1d8819cfd8cb06f0b8      1
     44 0x224b5e80309c565bd310f2984b0363054cba90f5      1
     45 0x22cbf92936f171e073350eb771d6f0c8b38330e0      1
     46 0x230d0096d6a60c6fdaa65b6d0370a6ba2b96fbea      1
     47 0x23830f2c4a3ca2363853964f2a449d13339da888      1
     48 0x24c238108f5a770663c4ab4b004ad5dc6f2d64d8      1
     49 0x258e0ae28f70ec728a9c0d84268ca3dd5c69fb3c      1
     50 0x26b7dbcf563bcb1505ab60c6948c2c73f20ead91      1
     51 0x26fd006b26b762faeb6a17bdf35e4644f4b3a259      1
     52 0x284b7200cc90cef0df0a9b14081fdd4d40bc9b7a      1
     53 0x2869b11ec4a74be99a8469b8742e653804a47cc9      1
     54 0x296c2d4daaef7b105ea4482a842e1cc882583547      1
     55 0x29bda07c9dd54f3e84dab4a4c3d7d024b9739d9d      1
     56 0x29cc37bb515745a75eb9ec13d2db1b7bc69447fb      1
     57 0x2a3438a9af1fba7b5e63a6f333fe296ef749067e      1
     58 0x2b53ba63ce6dd93f1bb2bb3245ac7631f1338366      1
     59 0x2b95620d41d5488d1018350c562b52622e68f0a5      1
     60 0x2d2dc1f45802f7a6b3bd87efd9176adca7b3c40e      1
     61 0x2d5313b5392f2709f4d1bed6f5f01ec0badc18c3      1
     62 0x2ea9b710e4038dfd78a7fba61b0ca1d3bba7b1d4      1
     63 0x2eb507b90cfdfde48a8e439ef441e2bf353cc7a2      1
     64 0x2ee8670d2b936985d5fb1ee968810c155d3bb9ca      1
     65 0x2f5926896bc7f9432392099bbc66526cea300077      1
     66 0x2fa5ac62ff5c3a2800e3eff0d1bdd14bdeb0e1a4      1
     67 0x308f833d14fc2803ebe9691de7e59d3844d7733a      1
     68 0x30eaf1e4db6e830e112988b87a300c03c75cc566      1
     69 0x311d07b222519bff34bd42c032478ea98b5e6eee      1
     70 0x324f6ead99aeda8b0083a4445e8df08a342e22e1      1
     71 0x336734a67020ee0f36fbdeb32517a5d8751bd0c2      1
     72 0x339833c45e82db98f0c98dc8c5e9e687ed11bd44      1
     73 0x33bf10b2b4a57bc20d955c00b2f735897124785c      1
     74 0x35bf94c72cd19da0f6875a6ebd6215f2d12ccd59      1
     75 0x361cef11ef82da9743e1f6ae888fe5547869c58d      1
     76 0x3667e767a12f9057ef12dc58764400b34cd88320      1
     77 0x376b682dcfc0e7eb383a5478c551325001784d90      1
     78 0x3950d3f8813301bef45b29c014ee5a74c47d4249      1
     79 0x3ad3030e9623e9e09ae21cd0632aaa62446a5c97      1
     80 0x3afa488a8cdbccc2163847c2a7054c778ece6a23      1
     81 0x3b63410fadbda0ec0ba932fad2997edd7a842679      1
     82 0x3d0400db7e6b017c05078cc07ed31ec4963fa994      1
     83 0x3d13af1580148b96a095d7ffa85c84d6ca975cc0      1
     84 0x3d8d4890db50d9140f81574d4a438d64be483451      1
     85 0x3e5cbba78dd36f651cefe21951de8f153ab1f8d2      1
     86 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
     87 0x3ee24138a7a6e9f5be87b446b9045fd69589f3ce      1
     88 0x403089b876128b97012aee31e3f91b4f6cfebede      1
     89 0x409c9f9dc413d7b026c677512aae81323b2c76a9      1
     90 0x40b35cf63c0d2ca561affb06e91d0db818dde14a      1
     91 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
     92 0x473f998299294733187a36ce31257826eea39495      1
     93 0x47f4f63b5760613a37acc9a83d20acf15d45e979      1
     94 0x48116616e768cb2ccf933bce172dbce9f6d9a4ed      1
     95 0x486df54de19dca684656d55b8202c68b8abb2ebc      1
     96 0x4ba7a675b583cd228fb6ff23232c8dd41cd715de      1
     97 0x4c327ee3ae154220513cddd0292d166bdb414426      1
     98 0x4dffede9b8cf0eb21f277cc21d10d5e7302f2399      1
     99 0x4eaf41fb572aaca9048e66efa95ec1e1e30b53b0      1
    100 0x504552190ee73a58ce0c9ad2690e82f67ef14479      1
    101 0x513b793ed1a277c639a6486f0be57ae866a8ca82      1
    102 0x52a1fdcbb75945b7a315976932adcd1d8042ce6f      1
    103 0x5306c064f74b2c45d3f1afae90cf0d74f7523fe4      1
    104 0x53dda0a69992b87e51f0f14b1337281f09a2c8da      1
    105 0x547a2e8d97dc99be21e509fa93c4fa5dd76b8ed0      1
    106 0x56befbe80008cec71897675b412c2ec746a5f63e      1
    107 0x59fa71f26e764aa95030b9e58d5301df83214c71      1
    108 0x5bc1fd44cc493280851d6f218f19965d4bcf5646      1
    109 0x5c396e679cc63df028e4c7eab88543cf18210fa1      1
    110 0x5d643f55b82e89f394c4a688ed6b91eb1d88e931      1
    111 0x60c280724bcfa9fb08dd55ff84c12fdd1d2fe02f      1
    112 0x622d4d42d78be6e5d081562d3c4cf12f011798dd      1
    113 0x6254f6b5a8836dba6e00b8529186de063c0f2c96      1
    114 0x6263b9edc7a9ee22789ec45995d17d9d21b8a609      1
    115 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    116 0x65806cfaa5fd763d379c92992d4eda2260c39d84      1
    117 0x669ecdde4a56480c48bc5a7f243cd94072bd5f94      1
    118 0x66e72223e8b1973b38d63fc1295ad47ba7a3e24d      1
    119 0x683a8c3353b67d863a34ef52270d436c1126cf66      1
    120 0x6850dd83a896ab7a1b191ee377cb46b708d4e515      1
    121 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    122 0x69a949d2330bfac2fdbee90ccfa9923343326b19      1
    123 0x6adba05ab489f2bfdd050616ddeb04598555c09d      1
    124 0x6c1aa1ebfc2d028b609c6c6b3153e8c94333a3aa      1
    125 0x6c2d65145b14978a95d14c8ddc5415d9fc447910      1
    126 0x6c3f8b24804f6d394cb0e5f407ecb661dc0f343b      1
    127 0x6d31129de814c26fbdca8f98d230c1f99015781e      1
    128 0x6d384f83c01340f0a3487177b162674f3164cb1d      1
    129 0x6dcb2a373398b17ef9b052d547a3785a9ac6985e      1
    130 0x6f15ddf0abd573986875ed740c1a94f5fef65205      1
    131 0x6f23f365330cb3a8cea066d00e672fdcb8357b9c      1
    132 0x6f6d59ba19821dc81193a926199278275fcf84cb      1
    133 0x7086f9b9ef5ab6653385f6751ee21bd82febe1a7      1
    134 0x70f4a2ce11a2524b6686963a5de86e336ee56ebb      1
    135 0x710852027bc25db82ba562d5b1499b6d9b02fa9c      1
    136 0x711c9768170cfefdf1873b398c23105720e6a015      1
    137 0x7165a04c41c9e5e67d4850eab1dc6ede84d117f0      1
    138 0x718d779affa85f2d4ccafc782042d0bdb41e67d4      1
    139 0x72a0726ae7a9054476a8c7e759962a4da667175f      1
    140 0x72baea3565cbda3ce3fbf7a305262964e918185d      1
    141 0x7500af1f7f829fa7a7fe4492e66b5bff5c05f3e1      1
    142 0x75f331fda7b8fa1fcba2bdd4197dfdfcb8632c7a      1
    143 0x75fe6f9af2e4b4fb9e86ebe26bb31162d292b315      1
    144 0x763f71beeac7b99585bdcafc7a25fec8c4a82594      1
    145 0x76f499ad599ab376471a62768f1f30a764f2e038      1
    146 0x77350e1152efd5f2d807a6124015c629a907155e      1
    147 0x777faa4f4f23f6fe4cda72fcce53fcf299f09726      1
    148 0x77f80700f974ca52cf52bdc87efcb3b0e0c4f78b      1
    149 0x783d71028044c5e8a6c997b705284865d02f751f      1
    150 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    151 0x78e1cb2d917f89bb998c3e068b9da9ec5e2b0f90      1
    152 0x78ff20aac0d76c3e01580f9181ad924f2b0e85e5      1
    153 0x790c83df4a0070bf72f571ce718a5d2d0bcbf982      1
    154 0x795128b6bf286c30c4334e776b03058cc70564b5      1
    155 0x7a54f66a497ea100433cc68fa739e9a16e3ec8bb      1
    156 0x7abdd18d37571e1ba884a8fa9e07a3612b584661      1
    157 0x7c2f06ab61a94fe9760cc8d9ad35a33c0472cb8e      1
    158 0x7d0ddc336fc0cfb5a13efe490de40decfe0900df      1
    159 0x7d39fa48a433aa853f7d14130d3dcd6a81b96a77      1
    160 0x7d3b2acb1a40f09eba725440e14a0e0e5eab4bf4      1
    161 0x7f873ec58d567e78a2c1dca322cbdb6d490ccb7d      1
    162 0x7fde0088a3b772ba42ae088e2d3b0b9026e26dd9      1
    163 0x806e37b374e4d867da45b24d8eb7584ae90fb2f5      1
    164 0x810e096dda9ae3ae2b55a9c45068f9fe8eeea6db      1
    165 0x8145fd1b46128f35609b0be92a88165ea9638ee3      1
    166 0x834cee2c58b212d37be016f303bc46e8184bd864      1
    167 0x8455cf296e1265b494605207e97884813de21950      1
    168 0x850daf4b914e46eede0cb09de538180bbea32a6d      1
    169 0x8753982d800bbbf1faf478ef84c3e96f2775d3b9      1
    170 0x88ac836c64d5e7b8b3abb8a0bfdf5f6703374954      1
    171 0x88bd9a49f6444b88a1e326a1929c91145141dc62      1
    172 0x897e842a5f50978f5cd337fe41e6e37099239be9      1
    173 0x8993801b6afc20cfeb9d2c0a354e154cd1d79d80      1
    174 0x8a01a85f1962938bbe6d19266581eae9ed33004f      1
    175 0x8a738c9224c6c4a35f6224a40d83dcf1aa1aa52c      1
    176 0x8abc6546a2895c27d2165776931c1276c258e903      1
    177 0x8acef818d1855363e8072f82c21793023b28537e      1
    178 0x8af6e15ed513b5b73573f58158b1b0bbd5085ec7      1
    179 0x8bace3a49a375027868cdd34e84521eed1f1b01d      1
    180 0x8bf048b0a714ef31398097037510ba449b2b99d8      1
    181 0x8c47286ffca3d75cc3b15ffe11093abe01913a3a      1
    182 0x8c87bd926981e151b4d99ed59c772b80629e5261      1
    183 0x8cb7f1a4f44593ec356b11c70c0a977c647c763c      1
    184 0x8d2a43ff7e015c55c2be316a52bec176a4328a9b      1
    185 0x8e059f6b103f7f92153c5430d998aa6829149e08      1
    186 0x8e5f71dc1d9b1dd976137d61fadd5a4318a8ea39      1
    187 0x8f5d478647f5b7ef9c18a9a0186e7b3af50d89d7      1
    188 0x92a7bd65c8b2a9c9d98be8eaa92de46d1fbdefaf      1
    189 0x95649f108393a38d182e148f0424c2604cda8cc9      1
    190 0x96545a59ce81bb6acaf95957ea09cf577889112f      1
    191 0x96fd61202a698ee3eac21e247a6b209ea5ffeb91      1
    192 0x973231cf471f768f0939042e0591da1b15f4799f      1
    193 0x9776c82c8a4e9a388829cf4bd78d3cc7b25e09f3      1
    194 0x97954789045bf067a33bbed388f8731e2c808468      1
    195 0x994b2b32530c95cd4bdac0ec0e0b41b187385b94      1
    196 0x9abcd812bffc1ef2e81a2dde75b5ed8809289d9d      1
    197 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    198 0xa110cc8dc6db20c75c60018eaadf7ec44dc024bc      1
    199 0xa36cbf2a4488e43a432a38807a7a4b1c21ea11b1      1
    200 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
    201 0xa42fcb74efa879f9f27c1cd521bec8c76b324cb4      1
    202 0xa6ddbccdbf36bf20e499afb82e6a8aade59e8af2      1
    203 0xa7abf57fd7a4313080f018e2914912b1de3b9085      1
    204 0xa7c6798540dd92856ffdf650b422e6d08c7a7b77      1
    205 0xab12bad50fa40b37371331c00fb5a035dc5624a1      1
    206 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    207 0xabb77d443c8f50c3f16cf8ae126941ba9153d9fd      1
    208 0xad887834294a92d5d6590608de07002be6fa3384      1
    209 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
    210 0xae91cb00c413a8d6089ba0bc8bf66fba47a912ea      1
    211 0xaf9a1e6a9b79fc77eafa40396e8a15d4d92867ca      1
    212 0xb175855d114f6560105e0d7c608785466879a27d      1
    213 0xb1e9f7d2c285ca1446355f267c021f5bf0155749      1
    214 0xb224bf6a5de0c2537209a1801c3a7456e1202a59      1
    215 0xb29479ef88dfff8dac466cf6c716e7985c4241c5      1
    216 0xb51906278fc209710dd755212fb7ea8d4d423df0      1
    217 0xb5e7594ec5c93498123571dbac1e7c1699afb768      1
    218 0xb6d4f6fce2f9dfc65015cb782d167077677a5d90      1
    219 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    220 0xb749a147ef387a90c1358a87c0e8ea9aa2ee3204      1
    221 0xb88f61e6fbda83fbfffabe364112137480398018      1
    222 0xb9654cd12524bd762976c7e3b3732ffb0db80ead      1
    223 0xba7b7292e528db8b075c6cfb874d4e8c44f7d0e5      1
    224 0xbb996e83b4071fedb06be1c7c949f3c946a8a80c      1
    225 0xbbd38f1c8cc4e8f1c4444db21f4bfe2f6393d9ce      1
    226 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    227 0xbd4fee42893e8bac47541dc2b5229d7139186666      1
    228 0xbf38423c1412a58f8ec0ec00abc25a1de0b5ef82      1
    229 0xc2d9eb979f4679adc0a4328ac5caa096a03f18c3      1
    230 0xc3424aa08c0e94e1ece3af84f5767acf8d76c39f      1
    231 0xc386a793af6cd288815a32128e5db7177a8441d9      1
    232 0xc3a538864600c21fd57eb45cdaca0f1665dccd8f      1
    233 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
    234 0xc4f370c6d3164a56971692362a9e488c0992a29d      1
    235 0xc51f6b8ee3c35a718d50aba5dd86683e1fe8217e      1
    236 0xc6893eeb690596e44f6c8668990a5cd7b8b1cedb      1
    237 0xc721b0b9e17b32dc9f981eedbe4f70be04b96415      1
    238 0xc893ff17028fe95157c9cc2730c407275d60469a      1
    239 0xc9b981461552989700b2d52eac6d2733192c79c2      1
    240 0xca85460d23bfb2e0079bab5aac84921d9d0762a4      1
    241 0xcb1c1fde09f811b294172696404e88e658659905      1
    242 0xcca3d4ece24a32ae1b037ea2fe14fb78163bcba6      1
    243 0xccb0f36ffe6b2a1e54c25870b8e1269290e75282      1
    244 0xcdf734d1ac2464399eb9ebec7f4b4f2f81d36fe4      1
    245 0xce08b02b4f7288c69131dab5b97364df42938cf7      1
    246 0xcf4f27a00e789b7919c4ad61c03c796794908962      1
    247 0xcf54b70bf3cec789dea3b7b2a01d3147cd6cd0ef      1
    248 0xd00911c7f9e5993ea0cd28cb615c6b21a0101666      1
    249 0xd09bdc084d61009e245a94e09cd52619f48ad216      1
    250 0xd09fbc94b3c830afc086733632ba0b63a9a44320      1
    251 0xd16804f9fa34ed95ae1a256e4a50635981f7a229      1
    252 0xd20ce27f650598c2d790714b4f6a7222b8ddce22      1
    253 0xd5755a4276a53ee7ca2703c6e1967af59cbc9feb      1
    254 0xd57729ba3b5a1a53dbddeeab17e26c9cf3af5e59      1
    255 0xd5a1f54aff17d90560cd3c5e78f8dea1837ea379      1
    256 0xd5b1bf6c5ba8416d699e016bb99646ca5dbab8d7      1
    257 0xd68615c82c613ca1f42fcdeaeddfa0e1d4c87e6d      1
    258 0xd73280d61210370d32c3dd05639753ec7f0ad4a9      1
    259 0xd7efae6e8e0556ebb5e77a499a34fce6a4d8c722      1
    260 0xd8cde9b0bba2ce08eaf8ca7198156c08a5451cb3      1
    261 0xdb368b61ac7c285c81bb557fb0c46ea7a764fac3      1
    262 0xdc2d5126c0147863e97e2a5c91553ac45ffa5374      1
    263 0xdd5f5c4aef9329aad44ac4dbad69d3afe1c8a5d7      1
    264 0xde1f3ca8d0666dd3d53a608a9da1c4d472d135bb      1
    265 0xded440a3b8d76c828db6737c4368ecbd374c2237      1
    266 0xdf51e97fbcf724f00218494864338e7aedb3dfb6      1
    267 0xe36b14f7a3fd29ac8befa667804d7dc0070d9177      1
    268 0xe375b00384ecbed3da6d2f8dec7b6784cf3693d9      1
    269 0xe38e8198a4e87c5f0f6bf96d9d3cd3053010a5f7      1
    270 0xe529c7121fb6aa33273c96bcf38d4ddc66c7ce66      1
    271 0xe8d8edbf4d3dda68ce89b2988e1c31b105e3150f      1
    272 0xea72fbceda4e851c9171a6b57a287ef986344efc      1
    273 0xea94dace59ede50c55d53cc90c26d8ec4ff0b685      1
    274 0xeb4c5c24468c3e666d01257b76a73326411b7928      1
    275 0xedd1f30d69898e4cb710cfb47c6114d31e6fed06      1
    276 0xee2b31fcf6c51ed01ae06cce05293c30afa5eb87      1
    277 0xf0c11838870ca444f5dc0ba0271a36bb9d9f81ec      1
    278 0xf17c382e51d9acd6eeee4df02076227c81256058      1
    279 0xf251b5d633b3ef46307653bcf04c8209cde7d8be      1
    280 0xf25d53830c733ce2d29741539ba6baf58d75df0c      1
    281 0xf326fc9373450d52d6ec77a71b8c79eab2cca819      1
    282 0xf3df5c1ba015845bade343252d6dbeab1bbf209b      1
    283 0xf3e8d5b704ccf09c02af5df9bfad1cf6e823703b      1
    284 0xf49ec27f278295b94d7c3badaaa954f0af278fe0      1
    285 0xf4ea2ddd1b59686a6e359976ed65674e97215df8      1
    286 0xf64932b6002fd2a465a72564d80422f762aaa566      1
    287 0xf6aafb44bc183d3083bfae12d743d947ca376562      1
    288 0xf6cb64ad324d2ff8ff26ff90360e2835086a9b3a      1
    289 0xf6eb2f6d790b7571751e5803d86de376b248e11d      1
    290 0xf8db01f59d0caa15067156ff7ed786eaf207753e      1
    291 0xfa19bb1257545e45e55798a9dcbdf147d1e1a69a      1
    292 0xfa8a62a68316475762966e3418c7343e2516adfe      1
    293 0xfacefcaf39da7e3ada119881fd2359cfb682d844      1
    294 0xfad7819967a6ef122a123f92416616a9a56eb5f0      1
    295 0xfade41403a1eee72d805c42f1eaf5f53a54d0e0d      1
    296 0xfbfd507f49a08b9b5f0a71231f69a38864d4b9ba      1
    297 0xfc0bcb2315629d03ebf10c291551355aee5cfcb6      1
    298 0xfcb33fbe33e1f0f066b41c24d91e994d8fd8e526      1
    299 0xfcdb35c1105ca8ea01df3b81a4570ff621817cb8      1
    300 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1

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
