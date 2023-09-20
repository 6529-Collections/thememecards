
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:11699       Length:11699       Min.   :1   Length:11699      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:11699      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18168669 # https://etherscan.io/block/18168669
block_hash <- "0xa0a9c164522a84feca3492e7aa49f76a0c24e6ca634cca377368e47630956067"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4511 

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
      1 0x000cd27f10dffac73201258eaa3925c0452051a0      1
      2 0x009f284bb658c55d292904ee733c50827dbb8e5a      1
      3 0x00f1706e8d023cbca6697dc22fdecbe52b2476de      1
      4 0x0155e31d9c01c28395277bd4210bf5c51965e8d6      1
      5 0x015681019ba287158d530bd841951a9b7bc0f57c      1
      6 0x0341e935efa5150b84003869c9212ea68aeb3b5a      1
      7 0x03593a431c7c3b01cea6836c2199c43614e17e71      1
      8 0x040902d25655b8ea4de830a67bd874207e783555      1
      9 0x0427a43fa3c5490ce7e9a7914b366f84f5c4f588      1
     10 0x04434c2cf7d7d90255348079e840b4d50a62588e      1
     11 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
     12 0x053b8ebe5c8aee5ca41fdb86de4849d5be6e3c77      1
     13 0x05503136f1063e9669764e10c1700ef6a910070d      1
     14 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
     15 0x0794167f4a3c992790b09c7b020f3435ee94d155      1
     16 0x08255e706f23ab1f5703eb717d8cfb5d097ae0f6      1
     17 0x0826cb102de7616823cb19b0038e1af91b262a0f      1
     18 0x08ba7fbc0aba77c3f8664cc71baee71ef602df85      1
     19 0x09e4560469666b2019ab38f4173fa160c2c5bee6      1
     20 0x0a123325a2eb92dda47587d697e9a333aad9dd97      1
     21 0x0a605f26a5b6c37b27fddb155ccb495fd4f128c0      1
     22 0x0a8138c495cd47367e635b94feb7612a230221a4      1
     23 0x0a8f4c21a980b0a76b4f7071cb4713d8a74753f1      1
     24 0x0ace76b681084e0f7fd9fe4eab763275d43bec8f      1
     25 0x0c81f4885b20fdcc97ce679ecfea9b94879f12b1      1
     26 0x0cf7e52aafec849a4df31054168bc2b1a39bc316      1
     27 0x0d648f5ef02403282329baedacb670e13ffdb41f      1
     28 0x0dbfd05fd48d62056ae630bb714e5d9f2255b5cb      1
     29 0x0e01a5cbb516331a0cd86ae901ec8aa5b0ade261      1
     30 0x10fa97b2214bd074c349787511c71a3c1a5122b7      1
     31 0x118fe7cec5f26a7c50f6c5bf732f0e6951222ef9      1
     32 0x13e073537f9d820eb7b8bcd41edbf7f9df85024d      1
     33 0x140de2c2585f66e922b43ebca1387dad90de1adb      1
     34 0x159ebf9863e44f9d910c0e8d8948cc91b803c168      1
     35 0x15fc6728ec8d16b66e7151a4df7261b331ff8b7f      1
     36 0x167e05781fc728a22155019293fce4df335e782a      1
     37 0x16ae62a3c2b3ff7c849a9dab8fd9dd6908b9b791      1
     38 0x16bf2b9490348b21bea767ff4a3d3c82e3059f12      1
     39 0x17526dd2955c6d7b4450bf066d196d7001e70804      1
     40 0x1756e785c364e30031e90f34e5d3b5aa4c83c1a6      1
     41 0x18528b590e730f70ba39b7b72f54de1d12e3519a      1
     42 0x18ba19ef5c0e060c7c0bb1e622c78d6fa493e2f7      1
     43 0x1b97faa31777ae0f250be8307673e8875559e44e      1
     44 0x1d4bfeeae08be4069145460f8f765fdb6332bc90      1
     45 0x1e31c297fd3d88710c35e9e1d7eb5ce27494fc72      1
     46 0x1e6d8d1a6076916d38b120740308d07c01cdb2c7      1
     47 0x1efc22a134a410fed4439c1b11b010dbeb8bd5d3      1
     48 0x1f8a8ac54244e4016a376152b2f92d467552fa7b      1
     49 0x205b07af095793ccef2b1b24fe5d54f4aaacb6d4      1
     50 0x20a1e6ac98f4406aedec1623e7446f357d78e7c3      1
     51 0x2290f81330c51d712416ec074af282ba3a10ce3f      1
     52 0x238edab30142a77ff1be2ce3ec42d5608d283434      1
     53 0x24141a358980b41084a487bb39c5e0a95b6e6559      1
     54 0x2428470da5c7a8559f11100c91019a81bc89bd99      1
     55 0x247fa06d93ada5dd3a9fa7f692855c05783afc0d      1
     56 0x24a5ffc3e20f8b88baf5a9b1d90ee4da3915643b      1
     57 0x250accb8b81f758039f054188132fe0a8474cd9e      1
     58 0x265765de876f24fbdb15532f601232f876cfaff0      1
     59 0x2759c9fd93e494c366e90eab777c7838de6da822      1
     60 0x279a84d4d6b1d4b1895086c775db1e91d5849cdf      1
     61 0x28f3903fc90ebf184d2763af2d7db321f2fecab3      1
     62 0x2a3e5cf41a9a3ec33ae00cca569076f55a52837d      1
     63 0x2c3244f7761540e41859d9a446b489b08a85a058      1
     64 0x2cbdb4c6d4b42a584a217aacbfc9d585827b9cd3      1
     65 0x2cf84928261f655a47d04ec714d3bedf9375de46      1
     66 0x2e10fec445f90ffb0be093b133a6f68e55d95764      1
     67 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
     68 0x2fca9a598678d0d09309b06eeff493dbf6c7d1a9      1
     69 0x32f72648a8fb1f97c6688e32cd5d5be8a8e25ae3      1
     70 0x3360f2bc513563cbed5a02dbb1d3b98e98f7ba1a      1
     71 0x3659f47efc320cf7b5f7a4ab216447c0dad30e90      1
     72 0x3804107dbabaf63d3b8e2b11fe43fa3caa811fe9      1
     73 0x380886e656ce40bd145a9fb85ac7e3dc51d5ee8b      1
     74 0x391a3699f430a4563097aec735b3c242108350ca      1
     75 0x3a2ea5595098565e7362e04da30c5ec29435731f      1
     76 0x3a4c7ac6873316e2c62a168150c49362ea4a77f5      1
     77 0x3d2e7f3e4253f57e5ae7638fd14cc8e16715ec76      1
     78 0x401f8b8348b8ccfdf04bbfb4e3e6df105c9e71e1      1
     79 0x409c9f9dc413d7b026c677512aae81323b2c76a9      1
     80 0x417f6c2afe33161f2166daf0bd6d27f1ea0aec4e      1
     81 0x4184f399b416f5d2ae4fcd9a842253a9896e73cc      1
     82 0x421ae97f38789280fd114478a748a0f943e6bdd9      1
     83 0x425052c13e1785b736e69324be5685e4d396a3a0      1
     84 0x4303f832a6a32567d8ec01a070b58d8a4f10e025      1
     85 0x44148af0432145858b6aab657f64c6314e52a570      1
     86 0x46cfba60b62b1b2feea4c45b1961214c05083582      1
     87 0x478bb542f7658d635abba67edb987806dff5b83d      1
     88 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
     89 0x4839ab5e2e84cc9765e38c818c8345c5517d5d19      1
     90 0x494d36925644a66737c3cfe8f6d1ffb8768c9941      1
     91 0x4bada215008b595f9d35379c06bdba0283fffcff      1
     92 0x4c420110bdc4dbf16e8d5e0b559168dace5f58b5      1
     93 0x4cbba098a091a53d7402bc84aa8abc5591ed9fe0      1
     94 0x4cfec81930d87c0ec2d38a4819d05a756756d85b      1
     95 0x4d51a39b4b74502cc5016e15e9106327936e3c5c      1
     96 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
     97 0x4f5faba416f21e9302d469dde4e3519204e17a03      1
     98 0x5147c1a9e594041ccdba5d044b6274231dc6ebf5      1
     99 0x51525196970866f38fae1853e363d0a355d41320      1
    100 0x52029325ab640c933e86fea95002c1b9a774a6b5      1
    101 0x520b33b4b4015fa88a87feb4a252620a5b79a1a2      1
    102 0x52dcc21f51dafec21a428bab7707af69c37c2878      1
    103 0x53343fd9e770dc2bb3f42fb562164d78cb9ae935      1
    104 0x5391ff3a579bbcb8372638f62defc7773a8f55e5      1
    105 0x548418b6265658d5f1f07d37ceed4cfedb4e3202      1
    106 0x54be3b98564f0a89237ff10d5c7a053edf2af10c      1
    107 0x54f6a34678d5b78205b024dc5db6222c3bee9e6d      1
    108 0x563b4e3be5452bd01852dc5e698ca4b2392d1200      1
    109 0x56b380f3bf7a902c8bece6e20fb843270c70bd63      1
    110 0x56f1760d582a25c1807a3d1c9f83ee5553f0f290      1
    111 0x5758af9e9764c6c6813ef516c6cd93c9726fc4f9      1
    112 0x5774daf116c1a7734df96d831cfb9fece129cab9      1
    113 0x57a15518b0ab5309d6459dc32a8acd1e89e4f333      1
    114 0x58b3492976b3273ee7d256ab8bf7f9338f3b37fa      1
    115 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    116 0x5a0dbfaa579d16e23fb76d1b04c94be48d051b55      1
    117 0x5a6b923189d47dd05772cf8599fed7baa4066a29      1
    118 0x5b4bb3c2bb6e2ec707cc39a86dd398c0a9f69add      1
    119 0x5b93ff82faaf241c15997ea3975419dddd8362c5      1
    120 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    121 0x5e65a0ae97928b30f44c6fb852c27cbb47acdeda      1
    122 0x5f98718e4e0efcb7b5551e2b2584e6781cead867      1
    123 0x6194bda4590bbdc6a128851aaaf34802dfa8e4a1      1
    124 0x61c9740e6ca15c78959f50d5aa579b9d27e71d7f      1
    125 0x61d7aabfd210e7a5d5d60083ae0a221fa37eeba6      1
    126 0x61e8c16a67be3c0d16e8113a688ecfc963e4503e      1
    127 0x620910b020fd12e7b1ff8c0d52eda66bf31213d4      1
    128 0x6254f6b5a8836dba6e00b8529186de063c0f2c96      1
    129 0x64932f86d69f2717307f41b4c6b8198000583c63      1
    130 0x658e6bb274382615bb0686727fbd33a0ea4d2f13      1
    131 0x65f443b15177bba952a419aee1cc219df93564ad      1
    132 0x6885c0a837c320f47a66df243c49a4e605ceb1ab      1
    133 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    134 0x6a9b72846ab3cfaa3394460ba314c9d3da02110d      1
    135 0x6b0e4ea76f522cc337e4683e01d5b5779ab67f7b      1
    136 0x6ce9faa7d77291eb6495541be1e8adf28f9f04c1      1
    137 0x6f3a349df0dc8e88263a1ebdeb59a31be094fb88      1
    138 0x70210b9928a21102900dede6ec65cbeb2992dd50      1
    139 0x702782ec4d986dd1d821d71452cd6d10ea5e0ea0      1
    140 0x709c13074ed89a5d8efe996b4f1e2315d833f431      1
    141 0x71ab2fcddc6dcc57d1212496f90a7066fdcd298c      1
    142 0x721540d874053b14f3c5e421abbe7bcce008456b      1
    143 0x72a0726ae7a9054476a8c7e759962a4da667175f      1
    144 0x739c6cd894603ddb5e974f597d49bf5ae72727fb      1
    145 0x75c84c7b446cebbb3830e3d64a042599b884c83a      1
    146 0x778fa0a3134a4b9a1955ecee703a586925c9cbee      1
    147 0x78c74a90e80b4d93b911f979839cf764be00b4d7      1
    148 0x78f5761b5a541c2e0f7d1921eaa14a0546f41396      1
    149 0x78fb3d569650ea743fb7876312cb5ff7505dd602      1
    150 0x7c4f9133852935cbc555d907cf981408cd2d4834      1
    151 0x7d87e23d7777d4829845f03f31721e10775799fd      1
    152 0x8047672c2df5a47c98c139e8fb7b403a13802956      1
    153 0x813e63c2376c5b396892da8bfe69aae3093b9e92      1
    154 0x8145fd1b46128f35609b0be92a88165ea9638ee3      1
    155 0x826a37fd4390a3369ee4b51ae23f03a4d6551e29      1
    156 0x8279b7167c17d11fe60238248eef4aa9e750b2b6      1
    157 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    158 0x82faab046e03236a09cec632fea6f172f69e44e2      1
    159 0x830d7a1cec156f2a8627ba8ac4af83a44dedc787      1
    160 0x839126d8b0ff719d5394fd288a55ab24b2a7baac      1
    161 0x839869f27ddf564995726a32c15a56aae85a354a      1
    162 0x848889067b71a8958849862fa7be9632c524b129      1
    163 0x8507a82600e3f2c85459c27721184698f18359fb      1
    164 0x8510a4ab79cd9b8c32e5a96d2f1124566f17a80f      1
    165 0x8555e51f5af836096b165327302f27304ec7aaa9      1
    166 0x8acef818d1855363e8072f82c21793023b28537e      1
    167 0x8b2ce5097f4a1619ba02f443ebab5f306342b867      1
    168 0x8c77fc46bac2649b55788c2f1657cabc32ce7aaf      1
    169 0x8d02f1d992704539fa0914d5860d3de1f223a80d      1
    170 0x8d12daa8d907ad9c86c1c317e83cc430e9685771      1
    171 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    172 0x8df937afdf1d08c2ba565d636ca1365a42144385      1
    173 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    174 0x902552dee5e3cf54dc7a1eb97017b85788120055      1
    175 0x90e96bb8955a2e19d5659c68cbe6fa43c8c4ce2d      1
    176 0x91215d71a00b0907ca67b25401172ab18d4b18f9      1
    177 0x9212d88943b58975afbf0af31510c0c00414efc6      1
    178 0x938a0af4b86057489bc651dd02c080890d8ed5e5      1
    179 0x94c7c5f905fc888ddc48c51a90b68ddec44f8d8c      1
    180 0x9517b043184f9b1a91b9ca142f8a7c6ed421c249      1
    181 0x953cc221d2d43ba9b7eb9bdf17617aea39cea774      1
    182 0x95f1422062f660fbdab6da626799389d86b8448c      1
    183 0x96236adb640ec620a85898378375cedf03ca21ff      1
    184 0x9972e72d43f8427c5821cfda342f598907b7c2e6      1
    185 0x9b0c89b9e698aa887ee51af06ef63dbfbde2badc      1
    186 0x9e027bc1bae517189285a207c69615d805670f7a      1
    187 0x9e414d7ca84ec395070967223ac64c8264b90ecf      1
    188 0x9eab4b2feed9483838d2e9b7c1ad407a18624df4      1
    189 0x9ed249b910842e9913e3eb1187f784f1db6944d1      1
    190 0x9f18598d7efd3bc6ac7cf1bb12e8c779e6f196ee      1
    191 0x9f432a2964351a7830bd9ea10f65107677d73e04      1
    192 0x9f761d100c27d8c20eb32ed5c929c3332f01dcea      1
    193 0x9f83a2a2b6e0b11a24dfa39817dd248475174a83      1
    194 0xa00bbf3f9272329710a36fe3f47cb053e377cdd3      1
    195 0xa033962385ca19c7a7109fafee356daa7a9291d3      1
    196 0xa16ab54e2a9d2772a476e966e2904e4af3067c7f      1
    197 0xa230d3d35600e66ef9c087bfd7c588e0f4e1d545      1
    198 0xa2f8d521c8cf328132e187378d545b2b37be2f31      1
    199 0xa40c49ade6c72324821dc073a968bdc114bcb0d9      1
    200 0xa454daff2609603b169e6290fe69667705ed11fb      1
    201 0xa5bd06a4722acd345b0bf872a0780f1e1db33a6d      1
    202 0xa66e796b33016963bf6eaa062fa81d4254f33519      1
    203 0xa68ca082adfe17fbb4348e88e7dfa59a49680ce7      1
    204 0xa7da565acbde5c8d2a0e3b7e8b158e056fe58586      1
    205 0xaa43b3ee536455939ac6155993351a9b34f72ced      1
    206 0xaa6be4e0e099e230ab5a809da8b07c9773b5407d      1
    207 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    208 0xab8d0a6784ce6b16da622c9a3a63f611c5f24cf7      1
    209 0xac2318d4fbfb276b279f03d46942254b941bb345      1
    210 0xadffc7ff3e937e3d7a34e989fac05fe06cd0fc99      1
    211 0xafa955f6b0f0b929382781cfe397de8ed033a555      1
    212 0xb2033e5bedfc8272e43ac181bd9c28e3812f2da5      1
    213 0xb22db1dde941a13fba352d56d857a26a5eda1cca      1
    214 0xb24ce9443d701c811256f14aa50dc76aebe7f0d5      1
    215 0xb2cfe0c83cc4395889e779c44c1e33341ea0155c      1
    216 0xb4fb5f99e1fcafe096ae5574c7005f602960489f      1
    217 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    218 0xb5daeb78c105f2b2ea81e327b3dba5d012e4f3e5      1
    219 0xb7802764002a0b15ff89300bbdf0de80a50c5d9b      1
    220 0xb792aa01d8526fb630f7d763bcb34a239a0229d4      1
    221 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
    222 0xb9f7774544cb360cfc934e2f8fd8f0a1ae30c4fb      1
    223 0xba567d6ce93c021e46baa959ffc241fe35a10297      1
    224 0xba64444d6d16d3661644464140d2dec3708ad332      1
    225 0xbaed105af8c00f5a512ddbe79d66e834707857ab      1
    226 0xbb315ba16c951cdccbab7be11b935387d82d98cc      1
    227 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    228 0xbbc30ddda94ac46496736215129b6b6f8c02bbf4      1
    229 0xbc149c9eca07013ecd81958a5b0746d190682af4      1
    230 0xbda24920e435105f9c65637eb79a6408dfb4583b      1
    231 0xbe0e8bed30fc3a89944648c4e9d988288a973769      1
    232 0xbfbaa97b4f95758df42f6ea6b2a2efba0460efb3      1
    233 0xc00b1fd8984cb568b451404074703c6a29bd91d0      1
    234 0xc1cce7179a1463837f30a49ab393ec32c32fbdd2      1
    235 0xc213c7e9f8e499e0d50afb58478d6079254c0209      1
    236 0xc217bd12bf5fb14a5e10450eebf34890092d3cb5      1
    237 0xc2bf53639861a84c186c5440c0579d2bad435d81      1
    238 0xc3cf5565433873113e48e0147656c30b827d814b      1
    239 0xc47e595b445844916c4648870efa1736e4844e1b      1
    240 0xc4cf93e6a744effd072d14c7516780b93542f4f6      1
    241 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    242 0xc7380be6ec377a6e66f34fb89d04a21b3cce2e4f      1
    243 0xc947a80884c307bad745c82c078099725bc005a0      1
    244 0xc968fffb7c3dd8c870d7149f454c4730844a60e5      1
    245 0xc9a693b5e1c8b3a46528be8e1ac912932bf2788c      1
    246 0xca2db534a407cb75ac2bb333083b8e78b4f6f8fe      1
    247 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    248 0xcb63958caff1b3b94cb0d6875cd23e945b06a9dd      1
    249 0xcb954087ecc62651b29ee5e5bc9d06e85a557740      1
    250 0xcba202f6c6684c497c3fb7740a6f6ba14041cde8      1
    251 0xcbb10ea89a9616174981a1b5a95aa9092ae4cf23      1
    252 0xcc0dc0a1d51bb8a8a593fd9a4a2eea1e7b688c39      1
    253 0xcd0cdbbea19a7932487feee640279a23db9d7de0      1
    254 0xce5052edaa831843ed8689a55cbf30147aba90f2      1
    255 0xcf4f27a00e789b7919c4ad61c03c796794908962      1
    256 0xd0157b9ebe373975cc9bae67e4f4b92e178f072e      1
    257 0xd0ed45ae054edc7bdd8def06fbe8bbd6ab5f1ff3      1
    258 0xd0f8293536d112b2f1b7fc7adbbf5e54e83dc3bf      1
    259 0xd16164c0982fdf729fd9f9652ab80d57e365333c      1
    260 0xd2ea8c9440b63fa16e0e82ae96b80ced580bd8ab      1
    261 0xd37697f3c50d7108fef25d16cafe229753224a05      1
    262 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    263 0xd500c8228c794461a659bd0a53d9e8922b51a9b1      1
    264 0xd54aa07a5e77bcc0269e2f1a830139fb72d818a9      1
    265 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    266 0xd6701a92ee4e8df99a7ff1a453abba8da84e0c98      1
    267 0xd8c39a8e2779efa2f3e8c57d9bcd18ae0bbad76c      1
    268 0xda7ba2645e540d39c0e4b3c51a17126fff2b7e45      1
    269 0xdac8fc039f633969116d412522b3338e3f1eba44      1
    270 0xdb407f575a4685959de5490d2316752ff5ef9c57      1
    271 0xdbb3f1bdc0353eba1c57dce84fc6a5c3975b321f      1
    272 0xdf2e06c29cfcdb4f1231504d2116da7bad9395ee      1
    273 0xe0c43cf26b0bacb408c6334468700179c61aeebf      1
    274 0xe171aa86734557919f81ac8e9cfe69be644526c0      1
    275 0xe1b8f185a6d65c2e6e46ee8ddbac4afb69b85ce9      1
    276 0xe1da9e3ea9efc074ebffd4d2bed209b370705188      1
    277 0xe33fd3abc86b43588089b0c1c3f44732f8fe44e1      1
    278 0xe4ac516dd8f9d84d8e882dc30130caa205e48ce5      1
    279 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    280 0xe55f32e874b540f6ad8584a56a484b76cda4a08f      1
    281 0xea764c590589a76a54e76d5060ccc52a0af1431b      1
    282 0xed0dd923857f3a038f065ee174e73916062c3762      1
    283 0xeebd455c141750d4ebf27d66be57dd3c7aa3e554      1
    284 0xeeefaa6890ac8bd6d05b48160fa663dc3a6fe72c      1
    285 0xf3359e5b89f7804c8c9283781a23133bbd979c9d      1
    286 0xf51fd1e7bb2064fd4dc313397283f98a860bcdfb      1
    287 0xf5819cc26f0481c9b86294b4c24027518a04bd5b      1
    288 0xf59baccbacfa9c87970a6ccd52af8dfdf87dff1c      1
    289 0xf60b4342cf2a83451cbc17b40c0f9527908f6c0d      1
    290 0xfaa0b3e499e752c409439d7c02fb60547ab241eb      1
    291 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1
    292 0xfb87834945db9862f2cbfd3fd3b3493cd2de2abc      1
    293 0xfbce265272c92c3ce57b62ae0aeeee332c55b136      1
    294 0xfc7355bf612fb59043b0ac3d728942f41a3e3905      1
    295 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1
    296 0xfec19978338ce77ab61f2502648f862765bf01a9      1
    297 0xfef1697cfa1a6c1211020681405013fee40fe2f2      1
    298 0xff1697cd18f03242ea521a80334a9060f0c25c7a      1
    299 0xff546e41d928472bd99fafd30913e41a7f6106d3      1
    300 0xffa97863a506f560f2c0f947b493a456bcd7c99a      1

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
