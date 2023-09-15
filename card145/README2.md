
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12022       Length:12022       Min.   :1   Length:12022      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:12022      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18133069 # https://etherscan.io/block/18133069
block_hash <- "0xff290214157f3ca24ddeaffe3801d6314ee4adb1819a530c764f42a28cb2d595"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4685 

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
      1 0x0022252b11a1c7a688c3e6281873e0111d63c270      1
      2 0x0050ab609416ae54b937231fbff04b23f69496dc      1
      3 0x0282018582f1925347f445b7a986d336965b1847      1
      4 0x02ac3a098a3f8197a6d4a5e6fcd4114df6b3e490      1
      5 0x02cfdaff6bc6e896f364f93a50559e3fd0a6ff1d      1
      6 0x02f3f238f5a6521b4715bdc73283d0fb06d276a3      1
      7 0x0360b9acda197703cb3040715c75659c38f13cf7      1
      8 0x039053b4674d12dcdfe69fadc25afdb0c17360e3      1
      9 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
     10 0x03bd5e165e78d2ea367e316bb4db8da59b843bd2      1
     11 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
     12 0x053e6294400a9268e35df445624f58087c7f388f      1
     13 0x05c250120ce07ba6fe361b39ac344148435c25ca      1
     14 0x05e07f4bf676bacfd8d211895cb080e5711c0a24      1
     15 0x06074279257fde7d5ab41c9673d134295ba1a28d      1
     16 0x0780a20cdde4cebc291b2bd53b897204f360f4a0      1
     17 0x08bac1d3c16fe76edba4d41c600462069eee33aa      1
     18 0x096dd05d5e0abc910d41e080562e7f22b30b1864      1
     19 0x09fb6e5f70b58daeddafd331e88804e8f1331967      1
     20 0x0d4178564cbf36e7eb7166bd9292080d2f7484c2      1
     21 0x0f3c600960b143d46c70b0ec1d9da818a7208f9b      1
     22 0x0ff14dad100343a01cb7599aa7485c4892378e74      1
     23 0x118638c389c7148017cc5b85d1f6d1fd25f76285      1
     24 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     25 0x125932718c2c15d593fbfe9860b179e13371d9bc      1
     26 0x140271fd98e2c9d4338978bd983b3c2e4b20f6f3      1
     27 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     28 0x14ec3c75b6b68f59af5bc328a4aab494b5b7b735      1
     29 0x1527fa3a0778d574adfe714b7e60c7b2a1bf7c29      1
     30 0x1684c382352fa8992e2d12264b08cbfd8e06b7b1      1
     31 0x170fb8f8f37776dc48184686dec386d2d1c41cc9      1
     32 0x17154fb9a5ae2b24cf81f3d813c27d8f741d25bf      1
     33 0x17f4d9e092d23d81860d8950c18fdf1dcce75232      1
     34 0x18799f860f9adb26e076d581603037f79342e0c0      1
     35 0x19694868cb9fc8435ef794545c7332915ec43330      1
     36 0x19fbccbbb66e21c7f6d9a5f8fa33ff61927c3214      1
     37 0x1a4280869e14004dbfe81d1e8483c8055c9ac4ad      1
     38 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     39 0x1e2f87542ba49c77d71cd28d9d8e70f0b642288a      1
     40 0x2112134feff010321a1692f3bda6c9373552306f      1
     41 0x25f1a3ce7d9dea98d51c18b95758d417a9a641af      1
     42 0x26773472e4440415a5fe4252b671cdc59fcadbe9      1
     43 0x26ee1ea70a38fee396fc5db7d9d8c75b93dda0da      1
     44 0x29b42b0c9c7ad2f487a7fade8999c749f872d025      1
     45 0x2a3e07fc005c26efeffc84c29f38bb14c3dbbb84      1
     46 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
     47 0x2ebe0096626bcd819dc45fa717ce2b4a473e62cb      1
     48 0x2f68a3bab7172b3d1b5b23e71d44e220665953d7      1
     49 0x2f77ca1f5339bcbdd99d466bea714d3d87f3a422      1
     50 0x30f0f915a4752445cfeadfac0beb539fa1192b38      1
     51 0x315562f0dfbdfc2040242fc45b4ad1cae8c254e2      1
     52 0x31e99699bccde902afc7c4b6b23bb322b8459d22      1
     53 0x32cc3a7ae3d1a1f7f0f41f564175dd4d4951c877      1
     54 0x3334c740aee8c9e6a0c6f16ac057b0e9c6a73479      1
     55 0x33389d09b2e267918e645f56e8ea41a1a2ad97c3      1
     56 0x3686a4b272c646ef6fbe34377337d95db7356e63      1
     57 0x375ab791287811a5527a54df9d7de8ad3ef4ad7e      1
     58 0x3b1311363bff7c9d43263a2f1c325753210d3327      1
     59 0x3be58fe702ea3239ef6979b48087aa0e3f9d7ecf      1
     60 0x3c098770c32634140f77401c7086cc2e48ca9ee5      1
     61 0x3c2550ddb9294c21d209999bdaa15f8249d0c161      1
     62 0x3ce53518159c99c7f47dcbc0426fe375f824b945      1
     63 0x3d13af1580148b96a095d7ffa85c84d6ca975cc0      1
     64 0x3d2e7f3e4253f57e5ae7638fd14cc8e16715ec76      1
     65 0x3db28e19b63cb8b3c257d37f56bf0455faf11e0b      1
     66 0x3e2b3d1307e349a96fe25ecbf83c367f58f6d958      1
     67 0x3ec7fc75cb0abf11d65a50f389061c3ff3463655      1
     68 0x3fda15273e4e7211377352e0bb84c02703303723      1
     69 0x3ffbed9340a2ab0c157a61771d7ccfbd7790c9cd      1
     70 0x403089b876128b97012aee31e3f91b4f6cfebede      1
     71 0x40962b448aa899196906646340cb4eb61a8d49b4      1
     72 0x40a193b9e3d9aec65bfcc78ced05d7d30d93bc13      1
     73 0x439e4e4624a6ce7be5cd0ae90709c43ea0ef082c      1
     74 0x44b1c428744baab6056bd9049bce34ff32144bd9      1
     75 0x45db9d3457c2cb05c4bfc7334a33cee6e19d508f      1
     76 0x46bc4aae45feba36eb29ee91527dd9615911eed6      1
     77 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
     78 0x49dfe490c49b15625f98c543fd55e8a2fe5fa5c2      1
     79 0x4b03e0fc0cc982adc25e04bbf2d549fb53991fc5      1
     80 0x4b670acc43436b4da2d029e38f4e506773caebd7      1
     81 0x4c0646b7b71b901c5db3c469c68dda3e2d19de3e      1
     82 0x4c9b3b18a54e59b23a75250425670e6a7467d225      1
     83 0x4d2cd3e1d994ab457197efef5b8057783c2f7d32      1
     84 0x4dc2e00c88b8c9e3c2bf090d7854770640daf27f      1
     85 0x4dfd1688ec88704e0d43976c58c34dcdb12ab55c      1
     86 0x4dff50cfe5a2d72971f944132de19d2a64e978a8      1
     87 0x4e21d3cec38509cf7a3ff9ec53c32be893e9f2c8      1
     88 0x4ee538b3a9c12644a9d3ae20a67437e8d18b91c2      1
     89 0x4f65f97c0e40e8635253cf27ce5c412c450b4710      1
     90 0x4faeb85b41468ac56640d0f2baf2241a242ee5cf      1
     91 0x518b870520476e08ee523a285e9255b443883885      1
     92 0x51fb095c8d7ff783cf884d011834192c46bd9398      1
     93 0x5200fdc6d083895d7722794d8d002a3f04943a90      1
     94 0x529096c3065d8a82355bc358e0f2af764c337a20      1
     95 0x533813aa3c513a0079aa268ec3141f67e4b11a2e      1
     96 0x540e16d0e898e45850ffa5bbb4d117d5aaef8f0b      1
     97 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
     98 0x550cfb8eb4d96d519de53459c2b9ed590560aa1f      1
     99 0x56577f982e2066fec566108f0a63f3d09555c6ca      1
    100 0x578dedd91786c6b7989c87539729b6f4d17d910e      1
    101 0x58d55449e4cc7719970a27bd1be569cd6834483f      1
    102 0x5a0dbfaa579d16e23fb76d1b04c94be48d051b55      1
    103 0x5a0e828703aa063b375933dd9ab1c4a147f14abf      1
    104 0x5a5617ba16beba3d04ae3c6b1e9d522c1856763c      1
    105 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    106 0x5c396e679cc63df028e4c7eab88543cf18210fa1      1
    107 0x5c5bb26835287b1d7b0fdb5e0c7ea890c509f851      1
    108 0x5d3dd154c5e582b1b1ac90ebb0c05744038e32e0      1
    109 0x5eff35d620168bd496b39243fee0afdc593e972c      1
    110 0x5f747cf7aa9e03dc2bfed25fa8cce89bf06488b8      1
    111 0x61e8c16a67be3c0d16e8113a688ecfc963e4503e      1
    112 0x620910b020fd12e7b1ff8c0d52eda66bf31213d4      1
    113 0x6324a44c1ec7cc6588f730c2ba3ac4ddf52d83aa      1
    114 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    115 0x6493ed5656590b9acab800cf108298a8d97f073e      1
    116 0x668df49e79e6a828b27f455297291a8bd2fe0531      1
    117 0x68d503ffd7659694f36ca8fbcce21fbb47490472      1
    118 0x6a32300ecfc84accfa9a4e1200457ee4a22c7d3c      1
    119 0x6b8a7304a3c7e6cf3c06f10aa7261348a22cd800      1
    120 0x6be89fa94cadad5ce328db60e0fdf063dbd469bf      1
    121 0x6c2ef68148f96f0ff50499743b8acff699ddaa72      1
    122 0x6daafa799c4d527f718aa83a81bf6e0abdaa3d61      1
    123 0x6ed7f81208839e31e11840049201201c469a7a56      1
    124 0x7106e9b712fa396941a023186011ebbd7d0c201e      1
    125 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    126 0x757394197bf7f1d3fbe8ea4c7fb012b821ea37e4      1
    127 0x75e60fd5f88faa26ecccc1212a3c8d6be4d41ec2      1
    128 0x762494a4d00d28d8a95f0935b0934fd9bcd69528      1
    129 0x778fa0a3134a4b9a1955ecee703a586925c9cbee      1
    130 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    131 0x78eca41c3085bf0eca6470ab202f51510e5f86df      1
    132 0x793a68e64214a1c4a0406a214874b60781320596      1
    133 0x7a827fa01ad4985ca7dff695131b28a1600b9130      1
    134 0x7b1c6f103c0851173f1c343a7e34d2fd1d5bae7b      1
    135 0x7bb5cc37a81623d6556d64c5d89333d3aee2a39b      1
    136 0x7d639b54904816df6ef24f10ad421a874621b512      1
    137 0x7d8b981b45879b054114d12912438dd17fca417b      1
    138 0x7dd3005f938feecf091151c0e4f5f53327b035a2      1
    139 0x7fb1f91574e2d39c41a85395afc7ac2e4c0e6b9d      1
    140 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
    141 0x807ea4c5d7945dfea05d358473fee6042e92cf37      1
    142 0x8125174cb26ff90b2a3b7a47a3614ab4261bd346      1
    143 0x81a80a99c8ab3c57823d6235134979e8c13b2239      1
    144 0x82671d5ba4c050ad8583ffd578b5dc66c613d54e      1
    145 0x82f724dd0547f1962bf062539c33693154cae713      1
    146 0x8386262bc928b5c7a0d19496c4921adffada05f2      1
    147 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    148 0x8471d92cd0d98bee82258c106adf147bde76ed9f      1
    149 0x8507ef739bd4b7cc05d31dcedc67adb94c3c38e3      1
    150 0x854fb5e2e490f22c7e0b8ea0ad4cc8758ea34bc9      1
    151 0x85532659390f6c2e7500f9a28ba9ca26ccf9519d      1
    152 0x879f36dcec0780ed7c0f863c9951cfe881f10144      1
    153 0x885cb1217070bc7e05661a6b2a906ce3d9cea705      1
    154 0x888801e8331d0d6098d53a5571e9444ec351d888      1
    155 0x895f4d750369f5b28072521eb2352b7b808043a7      1
    156 0x89e8493a292aa54734f7c2ff4765719138f18189      1
    157 0x8a0ba098c376e331d6fe74875ce242c47d99a226      1
    158 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
    159 0x8a3db9109f2ef0d234b6ca0b7a08f9bc63efd9a2      1
    160 0x8c1321c782c169aa93c7c211ed92956e84d2f3e8      1
    161 0x8df937afdf1d08c2ba565d636ca1365a42144385      1
    162 0x8e50b222b2c027259392f9f4d6e39e59c24edfc8      1
    163 0x8ec2f31fcb64c018909f10a8d6c5756c5a3c3ab7      1
    164 0x8efe0c9face897246a7b7c5274a32f1721b30fe8      1
    165 0x8f5c1e85e70590cf8500b878da27cd1051b783a7      1
    166 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
    167 0x90a3c5613c0f6e49cddfba0f0cbe5c208a1f1438      1
    168 0x911e63cc9d7fbf4ce3f793bae9c3d75702b0fecc      1
    169 0x91a3ebce4e0d326539c6e2720f25449774cb615e      1
    170 0x92bdacf2a74f437feb842e75d4117f5f829e53bf      1
    171 0x9388f86ca5c16a420e65f6580e997e21bd2665ad      1
    172 0x93a635440f2d8e303177f033b56408ca03e746c5      1
    173 0x953cc221d2d43ba9b7eb9bdf17617aea39cea774      1
    174 0x96b615975a4017c2230e93727db65c7bf96cc2bb      1
    175 0x980e22587fc29d25cf2c82baa6933dc0f73c09c4      1
    176 0x989cd4a3a88c86300726edb5d5516a517ed47599      1
    177 0x98efd2126b1b76a5f426715ffbdd286c3f7e740d      1
    178 0x98f9626154f44e3f30c112e9bb48b0678568b916      1
    179 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    180 0x9b220307d270fa9cbac7d6a41758274704aec977      1
    181 0x9ca69c07e04315a8b0dc95156bb038609564a0aa      1
    182 0x9db32b92a3026544c8d5ec2222700bb0a4d5a901      1
    183 0x9e9c0431eafe5addf548bacfea974ccb550dad45      1
    184 0x9ec73d0003b49cc7a889f4ebbd38e192ee3a4620      1
    185 0x9f885643734848acb417ea3afce0b7d1160d3561      1
    186 0xa0202fbe4ee4ebcc96fbef5c3fdd46ff29cb8351      1
    187 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    188 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    189 0xa266b9edf05b39a6d2bd9efbe1d5445192bda55c      1
    190 0xa342e537bea996a2c96864c31dd9b3d9e40eb2fb      1
    191 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
    192 0xa3cc58eaa3800dc163726e031a2da49a9bb64eb3      1
    193 0xa3fbabd1ee57af8e779531c75444cb4c259214f6      1
    194 0xa454daff2609603b169e6290fe69667705ed11fb      1
    195 0xa511b78b844de12ba860ffc2f3033052c6c260b7      1
    196 0xa52f056c6b8dd39759fead233103a14ddee4be55      1
    197 0xa698cade532bf7491f0f887284f8e10e2de97198      1
    198 0xa6e50c3d38b21a606f919d3eca0b6e9271704fce      1
    199 0xa883b60c5ead0efd604b41a7c0509c3c6b81739e      1
    200 0xa8c181e1ce31c5ac4e5737b3465eebac286f415e      1
    201 0xab12bad50fa40b37371331c00fb5a035dc5624a1      1
    202 0xab90ddde2cf6a4753106a058acb4cc7412a58ae7      1
    203 0xac718fb27ec43925bbf905f74463236e7605cf70      1
    204 0xac79d2ca0d415dad95090b5b9f5cb5704cfac071      1
    205 0xae1788699d4c34808d1d56479c618f2c024a5b52      1
    206 0xaf263ec04290b2bfbc75748fc22c465fa8df76c3      1
    207 0xaf469c4a0914938e6149cf621c54fb4b1ec0c202      1
    208 0xb0115095f99f794f15753e1eed762631578b5268      1
    209 0xb066a5b94c4d1c7c06610d1628375e5e4b265de5      1
    210 0xb1db41aa2484e3f5f5a510e07003c29fd1b0f115      1
    211 0xb1f7d890b786e73a01ad4cce1693f1db3d3c9a99      1
    212 0xb24ce9443d701c811256f14aa50dc76aebe7f0d5      1
    213 0xb3b0dc3f6c731f2d2d96b284661757aee2c5bbc5      1
    214 0xb5ce9f0158b02bbad38eb706bc3956d5b8b55288      1
    215 0xb6848f941dc6c5baae2d1ef18b22b5c02f5d83ad      1
    216 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
    217 0xba3e6e7918f2b767930294545988b8143257518b      1
    218 0xbb674c3251c4d1fcbc4860dc707daf94c286a461      1
    219 0xbbd8be55502d819351a1eeafd85df26d71407e40      1
    220 0xbc149c9eca07013ecd81958a5b0746d190682af4      1
    221 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    222 0xbc94e04a219ebd4d12b881fb0d6ff019d15a8885      1
    223 0xbe1ad586f9461ff4e1757f939b8cc635a43fa63c      1
    224 0xbe81d93f812b833684704fd2af8e28d27f51e541      1
    225 0xc113eaca52c51372106f9fa57d6ef19638d61fc9      1
    226 0xc22d43928cdbd57b6725e669c1606b18a2bce82a      1
    227 0xc2bf53639861a84c186c5440c0579d2bad435d81      1
    228 0xc3af7fe243d42347b88a5a519630ccf26da2ae43      1
    229 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    230 0xc5189dd826a5091dfdcc8890b83f9190320d5341      1
    231 0xc5e4bba6b2a977adca47b6e39b4ff10f6ebbda58      1
    232 0xc68bb9728103765ace6d43d98bce3281af3850cb      1
    233 0xc7b72bb1acdce3eb45f9b38f43cd31e4927c725b      1
    234 0xc87e9dff30a7472329237c4ad222b35b529f0fb7      1
    235 0xc915d266472685f1ef2f974c5f01b4915f0df46e      1
    236 0xc917ee63ea36765d63737d8baac564dc57c33b69      1
    237 0xc91915d01bb96ec08b9b5fd767034de9cd390f17      1
    238 0xc928cebf08833cfa95a64efed522ce683e26de48      1
    239 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    240 0xceed9585854f12f81a0103861b83b995a64ad915      1
    241 0xcf01b135efce6bced0e2b8ec55eef6380dfef07d      1
    242 0xd0157b9ebe373975cc9bae67e4f4b92e178f072e      1
    243 0xd0ec43248df125fbb02da77934ff8b3e675f198f      1
    244 0xd0f8293536d112b2f1b7fc7adbbf5e54e83dc3bf      1
    245 0xd58518b059895d0628f065f42d2d90d37f347f1a      1
    246 0xd6cc8bf1a2bdf94be558a40b2a665a46c94211b6      1
    247 0xd88fdf946aefb208ada91269c605d86114883581      1
    248 0xd8a87ef6a7179c6bcc617eb32c3462ed7eb9e1cd      1
    249 0xda5da40b45d9b6fbc8d99ca3eeb78477eef75888      1
    250 0xdb9986bd0596b8a4873b09b4a10b81b13f2c9ddd      1
    251 0xdd855f892196a685c9f4d80cb1d556f1f20f56d4      1
    252 0xde40d2cb9db9b1cc295e18fb2b12612a66b5c5d6      1
    253 0xdeab3f7094a77199ae670316976654c17fea35d6      1
    254 0xdf5b0e3887ec6cb55c261d4bc051de8dbf7d8650      1
    255 0xdf617fc072215c638137b3038628b420064c06b2      1
    256 0xdf7cc19cb1066c80760dc1b5464c4798dacfe407      1
    257 0xe0b685bf93818ad568d67692dbb1967bdfcb9447      1
    258 0xe171aa86734557919f81ac8e9cfe69be644526c0      1
    259 0xe1f0da1b238db9cb3e51bca447f1210c7134277d      1
    260 0xe22c2f9460e76daf26411d46911b66232f4f1253      1
    261 0xe2b34bbf669096a794397376fb0587e98eb81016      1
    262 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    263 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    264 0xe5f33c3d6f158dc9a29dd90926b8f69d3dc4f10b      1
    265 0xe941c022f3a890aa2ea84081d6b5aa3b4f244850      1
    266 0xea1cd1545e5bc3a1183baab0089b29fcbb45f448      1
    267 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    268 0xeb44e719bde774f08fd35a54315f08a6006cc9b7      1
    269 0xeb586487116eebdef804647f8823a6ad1913f954      1
    270 0xeba5c4630c35e5c85f9b6d63c50227b5ed93be1f      1
    271 0xec58c92ab1b87111ab2bcfea24ff9404c25b37c8      1
    272 0xeccd4cc37ede429cb77d409782f5b7bfdbe172b5      1
    273 0xed6618cda6a25482b99fe8bc70f039034575ee7c      1
    274 0xee34bbb4882c0ca569f70901217e6aeb3b347b62      1
    275 0xee48b89fa6815a6e046971552a5b08e1ca659974      1
    276 0xeea3d1d0bffc94c1bf9664426882de8c54bf7c78      1
    277 0xeef0c3ca47ee2b81f921ff30a381552f773a5bd2      1
    278 0xef220de4c66c1ed36836d266e8b6c169693492b5      1
    279 0xef6914f95fc782a5397cff1686873da77fdeca8f      1
    280 0xf10439002216758e926e0e6ef022c693ef7cf269      1
    281 0xf170fac80d66f4a3cd92c030d6a63dc1cf637d4e      1
    282 0xf1b96faf44d0a04f7a39ed90b4b3a2942403b109      1
    283 0xf1f174ab7f3e7c2c97a220650e0136e178fdcbd1      1
    284 0xf2285af9aac7d12574784476b9bc43f8adac034f      1
    285 0xf4296d0591541f6d25e241a0dac3d7021d8b821a      1
    286 0xf45cb4d901541000a9513f422532bfcdf1612d2c      1
    287 0xf51fd1e7bb2064fd4dc313397283f98a860bcdfb      1
    288 0xf5559d32946212a8f718c8d6c79c19c6200c9d6e      1
    289 0xf57c198a7f337867b7c2d5f591e8965622ad1491      1
    290 0xf7b6409b12a540947688f054dbf5b629fb3fc7d8      1
    291 0xf92dcbdf576973c02c8c0543f253657877ee1ba1      1
    292 0xf944030287430a84583e565e8feb3792e9db0708      1
    293 0xf9f40128e49d2941fc7c906a9eca8bb65b54d60d      1
    294 0xfad7819967a6ef122a123f92416616a9a56eb5f0      1
    295 0xfad7f76d73e79a9c65c85f23f16631180c150ec8      1
    296 0xfb47b2ca7df248d36cf0e4f63bfd0503c25debd4      1
    297 0xfc5c301fc19f13300b37c36ab1839d3462c21a4f      1
    298 0xfc7af206e1a69f7886fb47ff8e1287fff4406d03      1
    299 0xfdd8e1e1f0bc23cd0cd684483a2d09e105ba86d4      1
    300 0xff677564b3bb5f9cd7e3913f096b6d68a161dddf      1

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
