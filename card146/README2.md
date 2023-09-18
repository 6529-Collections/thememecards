
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:11668       Length:11668       Min.   :1   Length:11668      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:11668      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18154369 # https://etherscan.io/block/18154369
block_hash <- "0x2ad59acdbe6d967c2fa57aa8a28965ac5499e5757cf4176f3aaa6f794f2da3bc"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4917 

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
      1 0x00809faa992806d58e5e858f22d8c896793b77cd      1
      2 0x0297aad5017b07fd7983d729f90e687cde98f8d7      1
      3 0x053b8ebe5c8aee5ca41fdb86de4849d5be6e3c77      1
      4 0x05503136f1063e9669764e10c1700ef6a910070d      1
      5 0x0623a3b40d4a7a95e410d203fae7d8f9c92ed480      1
      6 0x072a2718dffb7861b44b9b23fc35a0ab1b2445f8      1
      7 0x077777f53544c2b2ef753c19e8fc048049c5ff95      1
      8 0x07c73af8440b6a35a271f146b137521db6e19df9      1
      9 0x080011fc1bc948505daa9d76e5a3ff24e65d6d9e      1
     10 0x087886a2aa1e263bd72eb67a365d268a3e13c85b      1
     11 0x09e4560469666b2019ab38f4173fa160c2c5bee6      1
     12 0x0a9ce4ae04d04a54af3949d356b6d6d7a6438f02      1
     13 0x0b18cd32055d75b168cb1e96c11154342066db9a      1
     14 0x0b6eae3681cc726f6d8ad39c38d7fb89018e1c26      1
     15 0x0c0d8f387030c631de001d99d40b2e519cf4d10f      1
     16 0x0c55e7d45f53b6999732a75917df6eb711e58674      1
     17 0x0e10593dd9b07484feb02229c22c819fa6be31b6      1
     18 0x0ea770824779b7bbc4f530705e64b291f5c43af1      1
     19 0x0f615319d7ceed5801faf6b13c9034de9223a3ec      1
     20 0x108c0c18df932d6d4e4cb743c95486e383ad2b65      1
     21 0x110886b13e4a932396c0e9d53bf887770ca16784      1
     22 0x11267972b5ec7554fe1b315390c8315f91c4826e      1
     23 0x114837ea03ed4e5bdbd6471cfa4dd80d68563dc5      1
     24 0x11fa4f109a1601bd1afdb509b3bdb1e23fd6a675      1
     25 0x149d93dfc34bc8092ad9562a6672394d5edb68a4      1
     26 0x1688ca553e48049f192dc727ff14414bf1524243      1
     27 0x17106b7ee3c4d1ad690b9caf2b2a2a6e1bde49d0      1
     28 0x17af4bfcb2e4d9611cd65a4a5c662f95c7167026      1
     29 0x184c4f086ed057b95d994e4b0b7ff02519e9306a      1
     30 0x18ba19ef5c0e060c7c0bb1e622c78d6fa493e2f7      1
     31 0x197b380ac62fa4785067cf7257c05f052776039a      1
     32 0x19f258959630311a21f4d279b6c41371d6389437      1
     33 0x1d14bfa2fedcd3f4da56d42e97582f614519d075      1
     34 0x1fc444e3e4c60e864bfbaa25953b42fa73695cf8      1
     35 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     36 0x209db432f06b65317b44d18cb665146b37a9b5d1      1
     37 0x21aa9286d8c7091c57659dfd7106cf5ff6c51f78      1
     38 0x2295fcb7e16f6f2f2793f1b2846caa4c94a52c90      1
     39 0x234c9dc1ff5caf605cc702efa51a4a6be5a66649      1
     40 0x24a5ffc3e20f8b88baf5a9b1d90ee4da3915643b      1
     41 0x24ee3cfc053d13f1c2a395471cc305b3ba831d8c      1
     42 0x25120c6122ee525b87e7b58cacf8a3f74dffc922      1
     43 0x25b78843e447f1187dc31c4990e2dfa6bb0033a0      1
     44 0x279c14599b8c2abd6d59686846958dec848a43cd      1
     45 0x289d545e52b97524278f94672d2be8c44eb3f369      1
     46 0x2ac217236e93bd3fa7eb2536c1b69072a54fa394      1
     47 0x2b3f8bec92fa0d34637f42ed865917aded146fef      1
     48 0x2dc820ae955aa5a34cd073f72b76e127cd7bed1d      1
     49 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     50 0x2f80b028a6e5c0d1fa92f62c3ed000af5bae143c      1
     51 0x30a933add6fd601d152aa4aaf7620b5e27e88888      1
     52 0x311399b3d6f728973bfe8ee06b11ea3aa67b0859      1
     53 0x315562f0dfbdfc2040242fc45b4ad1cae8c254e2      1
     54 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
     55 0x324e9e6e602fdf1f7f50decea6cb83fff575020f      1
     56 0x33914463964f994f093bfbaef4db18110dad60d7      1
     57 0x340b69daa84271b53a4b31d801ba72d19e6c934a      1
     58 0x3600aa29edb4f1c864014ca5d5f233e8616fa9f0      1
     59 0x3607eda07116b277c1f791ee224cb2ad9291a009      1
     60 0x3804107dbabaf63d3b8e2b11fe43fa3caa811fe9      1
     61 0x3854bf7264b0c4c871be8507fbf1de141bdc3b5a      1
     62 0x3a4c619284748869eb3ab16494b461256b72f875      1
     63 0x3bd8f8600a20aeed0c9bb7f5f0f55e1ab429ff5b      1
     64 0x3c2ea9fc12e137e55165a53d3f7c9fb7a95b6b0b      1
     65 0x3e34060586e19a0bf0be74267888fbdb0a8f7e5a      1
     66 0x3ec7fc75cb0abf11d65a50f389061c3ff3463655      1
     67 0x3ee24138a7a6e9f5be87b446b9045fd69589f3ce      1
     68 0x3f3dac9bb1458b644be33718c447128e9b6a87e4      1
     69 0x3fc145ea0fa4290ff0304c689f78dc8fc69788f7      1
     70 0x4209859ae2992f581d2678392b4ac80c13d5eed4      1
     71 0x42548a143764550be44273dffb098103625fd965      1
     72 0x42cef12222672e260d275b80a0ed0bc40896af67      1
     73 0x4303f832a6a32567d8ec01a070b58d8a4f10e025      1
     74 0x43176751abfbd7a39549748aab57641c26df7129      1
     75 0x432bbc3810692e6baab69cbea162bed134dcb87c      1
     76 0x43caf87bb1f32a558a4c89a04a27b66e943c6999      1
     77 0x443037c611e0236c0c3d1e811c7b785d1d360ce2      1
     78 0x4521b5e551a682ce13e302d11a8b6e66ac895657      1
     79 0x45f5fdfa40b5e2befebe78e3f85e197ede734981      1
     80 0x47be83cd4037b98366ccaaba4e10b1a5d5486f14      1
     81 0x48e7d74810b0950ef03232468ec294a1a14e2e23      1
     82 0x495a10d99b304c613bdbef9529aa655731a7a49c      1
     83 0x4a10db49d36c39934bac4e882e94d268a67dbce7      1
     84 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
     85 0x4b038c047f79e0cfd3762b9e8b4cb017709315fa      1
     86 0x4b670acc43436b4da2d029e38f4e506773caebd7      1
     87 0x4b85b8e5d4d2e3220218f69500c4804721ee4cfc      1
     88 0x4babf2f14934fcbeeeac5a71df0aaa20f0eabcc6      1
     89 0x4bde9f2fab6d26810d3058f0275604e9312846df      1
     90 0x4c80bc92532225bfc71ebfb0bd75f7f71ab99fc0      1
     91 0x4c9ab1064da9c0d384530131498e3c34617a1508      1
     92 0x4cbba098a091a53d7402bc84aa8abc5591ed9fe0      1
     93 0x4d6227fcba4c25feac2b6ea347ebd7851b781edf      1
     94 0x4e0358255ad25c4306fa0ee6657ce0c52ce22f53      1
     95 0x4e0bd3bef962d9fa4a561b288d6680faf8fcd200      1
     96 0x4eba712062cb51e1aafbb7858485b5a86ec91cb3      1
     97 0x50d12e66195a67c80531f251ceac41e7e5e67340      1
     98 0x50ed078be051280de17e724dd06f375a8cb3f020      1
     99 0x51525196970866f38fae1853e363d0a355d41320      1
    100 0x51fad37dad97121bc30d166577189a0d771a0361      1
    101 0x52524f21eb713231df10792091b7e783c6a30ac4      1
    102 0x527ad5b6d8ad7cdf62f353a55bac5492fa2e73b1      1
    103 0x533813aa3c513a0079aa268ec3141f67e4b11a2e      1
    104 0x533dee7f5e471b34ce9ec908f6c010dc2a160413      1
    105 0x54a222feba0c5f0be95bc7979e38fc3847827d28      1
    106 0x54f6a34678d5b78205b024dc5db6222c3bee9e6d      1
    107 0x5508ee93c62505a983bcd927854d1ca329e15857      1
    108 0x552f294781d4313fe2f5f6b8df3ba90743a6e17f      1
    109 0x55a652dfe51ec04264eefd87d04a42c36b42bec4      1
    110 0x5622cc24ddecf5888d31df4b8b12648d105472bc      1
    111 0x57175429d316461f3b0cf06d0d428eccbfe23d51      1
    112 0x5a7bcfb9390b37fa0055e434b041f20a719e1aa1      1
    113 0x5af9fdc81af83b0c2fae687fb916bcc6cbf13e7a      1
    114 0x5b77982fc8dcf080d14c6a2b5d39460d157790fd      1
    115 0x5ca12f79e4d33b0bd153b40df59f6db9ee03482e      1
    116 0x5d999a7dcf597169a7f8dfb03c7935da39c163a7      1
    117 0x5e055693d6fee356abe3ff5960584b839184101e      1
    118 0x5ed0f666e6c20f5eeb2214514b56df2adc47a0b2      1
    119 0x5f3461b965b97e687db9001efac5dd250f30c0e0      1
    120 0x5fae9d4b591f213b3ba75287f2cfac0883d17f7a      1
    121 0x5fd403fc29ddb91842d3c3f42e4dd1be2a1f5e37      1
    122 0x60f1bbd5d1300d8db8abcc75236eadf2adf5104e      1
    123 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    124 0x61c112e7435b025a624e393c92d751229b93834e      1
    125 0x63360384656e316a50317fbd36f3eba8b1bc30cd      1
    126 0x646aedb1e877fa189b5aaa6c36009a8970be93ce      1
    127 0x660c3bf8d0f8b84a9e1e7a076cfe4685128f5f7c      1
    128 0x67052b837563a6d19134f06c5ac2fee9f7eebd26      1
    129 0x6928bfb114d228dffa0604f990d4a964bf1b6e61      1
    130 0x69c38c760634c23f3a3d9be441eccbd2e50e5f73      1
    131 0x69cee4cb90eede1feb66c7953adf172e3f937dae      1
    132 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    133 0x6ab0bfded6af31cb0a5987f08564ecfbee691757      1
    134 0x6b40bde07fa0e98289809bcedc286b4eb3e79389      1
    135 0x6b9eb48af8d9d4ef58e3e37390f5cfdf6525f5dd      1
    136 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
    137 0x6daafa799c4d527f718aa83a81bf6e0abdaa3d61      1
    138 0x6de4d35786c9bf77d48d8bcb2e8df2f93bd83aed      1
    139 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
    140 0x7024ee7932e4d879409f9618b560cc82cf093a7a      1
    141 0x71a4e46e5d78de036f7e6c916be092566e1565b9      1
    142 0x720325f6e2a7d1ded0ba8de8c77a60fca4d292cd      1
    143 0x753dcd2e10a9b3a7abd62cc34b85ed6497b462d2      1
    144 0x75c84c7b446cebbb3830e3d64a042599b884c83a      1
    145 0x763f71beeac7b99585bdcafc7a25fec8c4a82594      1
    146 0x77d24d4d990d439d0c086d93b932c175238e812f      1
    147 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    148 0x782de3f99f9c73c125a5e6b494373a3c68a2a914      1
    149 0x78887448976b93443125cd9ec40a9485684c759b      1
    150 0x78ff20aac0d76c3e01580f9181ad924f2b0e85e5      1
    151 0x7a2ea9bc6389d1fd50d2850e9b295da706c6c911      1
    152 0x7bb5cc37a81623d6556d64c5d89333d3aee2a39b      1
    153 0x7bcec44c7486143ee875103de66006047cae8df7      1
    154 0x7c263f8a628537eede427587e28353f2c25d6107      1
    155 0x7caa4ff0239ad4fef3dc7c628d7dd749d2e4d453      1
    156 0x7d656ce2a065b331abd295d07f466f6249ac7231      1
    157 0x7dc35b473b6ff652c1c437996244a8d14d026252      1
    158 0x7e1ee6fbd56b93f09cd5028fedc2e7b574585088      1
    159 0x8028bfbbb68ac004e0704161cd080409a8cf2950      1
    160 0x818859ff6240c09b5ef9cb1f60f6f8555f6bb49c      1
    161 0x827f541d27beb06eb0441a4ac7d4844cbbe95c0e      1
    162 0x82e133653e5069a61062b28795ef19a99a3d2c75      1
    163 0x8400b081f5dc9c5bffe1b57276afdfab2dfbf29c      1
    164 0x8436e9a3aed9f8d595684edb1fa7ebd6ea6a2cfa      1
    165 0x8457e614bcd617d5c81409a7e1d17db7ef9a3fa2      1
    166 0x851fe70498e1792739e429b466e3a12cf6e50de6      1
    167 0x85bc03678220009e82206ddccbf713228750f759      1
    168 0x8727ddb932095fa3ea0fbb6e77f65b1581b9123e      1
    169 0x87cb21babb72d71bf3219a848704061dc701c835      1
    170 0x8924ac1196e2ae89603925b6e001dd14e3f7fdff      1
    171 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    172 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    173 0x8aa14a902725d9be58138d60fb804801e6935a6d      1
    174 0x8b2ce5097f4a1619ba02f443ebab5f306342b867      1
    175 0x8b338b8380755d416d892a0d7793bef75f6105ea      1
    176 0x8c847c9b48be308ab7002011acfcc728483d8b21      1
    177 0x8eac39817aa7a1072c6e81859aff383f534a4105      1
    178 0x8ebd84269464b6ad7353d192f3b487e69d607e05      1
    179 0x90a38dd336e281462b7e01bb6b63d85be22f6a6b      1
    180 0x90b12473b278c6d00c06e4678e1246a306141f35      1
    181 0x91006ced6152bcad20decc9f4c1158f47c512331      1
    182 0x910b48c25afc5b2a998458b62d467d482eca74af      1
    183 0x91a3ebce4e0d326539c6e2720f25449774cb615e      1
    184 0x9337a9c93a43b4aea78ee9dfafe93e5118c79451      1
    185 0x93d8d7cdff64d4a31ea2554a3735e58fd69f052f      1
    186 0x957b0cd4e9851537aa7cc84e9c7b22bb68d949d1      1
    187 0x959cb1897c18d4be61f365eb25893e0cdbb21401      1
    188 0x95e8a4792d7e5bf6ce3973184400cc9c37b6ae67      1
    189 0x97954789045bf067a33bbed388f8731e2c808468      1
    190 0x97963380a1ed099905a7f3d8207e650caf2fdc05      1
    191 0x97cadc16ccae6c31727327029a4fc1d153d8fb24      1
    192 0x9e1fcd7cc0fa12e74212488fc5cad56850e20542      1
    193 0x9eea866c15ad425aef0e1a0179843a8f98341436      1
    194 0x9f455b1fb95d998ab1f25a0e957352fe11f508c0      1
    195 0xa0ea96c0201b3d567bb29cded320cff8f1e25fe6      1
    196 0xa17b595edcb5c66c532c830e34e823a3e0033c8e      1
    197 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    198 0xa244482e30822cad556f0338fb1e5bab72890604      1
    199 0xa29f7d70c623e8cef9989d7b063212fdeb3047f8      1
    200 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
    201 0xa3c523365e5604ad806c1c599c90aa9ebf7a3277      1
    202 0xa3cc58eaa3800dc163726e031a2da49a9bb64eb3      1
    203 0xa4df50f02a778bf281ea0db761900d354449eb17      1
    204 0xa58714c5884f5e217df9583d1f8151826e938b02      1
    205 0xa68ca082adfe17fbb4348e88e7dfa59a49680ce7      1
    206 0xa6c0402650f7b49d829bef339dfd3b6a6c602730      1
    207 0xa6e50c3d38b21a606f919d3eca0b6e9271704fce      1
    208 0xa7c6798540dd92856ffdf650b422e6d08c7a7b77      1
    209 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    210 0xa84ff8c3feaf71a7090e90f676ef1c9c94f7dc92      1
    211 0xa95ee864f3aafb3da6546914ced476cf0a49ab2b      1
    212 0xab7473da0af4658ae2879cb641d82e7652721486      1
    213 0xab8d0a6784ce6b16da622c9a3a63f611c5f24cf7      1
    214 0xabf107de3e01c7c257e64e0a18d60a733aad395d      1
    215 0xac4dcebd7a69144a9593f3459bfcbc2db4ebf09d      1
    216 0xb05dec2370a17478e726452bbfcd093f64f5b900      1
    217 0xb21cc6e889df4ca5d8331b864a4ed346145ba44e      1
    218 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
    219 0xb783dc0696f93ec74fd9214b527278670cb5b09e      1
    220 0xb7e04ce435f55bbd6711d181e5df6c446e3e16bc      1
    221 0xb7edc97386c4889e70a28e469e9be078ee08cf43      1
    222 0xb876e9e9d89998a483dc9fbf83a12ecab172ac9f      1
    223 0xb8a4624d5eb9dbf05fde82a3af920100c7a19086      1
    224 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
    225 0xb9fa078a43da2664c8499c5d2073f2526f96f106      1
    226 0xbc8cc8b1fee0ed52dea5acdfd0002c2a679ff13a      1
    227 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    228 0xbe2127ebbd7c2a3e6473e213749f212b4f97c69b      1
    229 0xbf02a32ee6e8570d76d510bfa9bcab2a4f651e80      1
    230 0xbf879dfa3ae386ff7c071f8c3e3e29eedcb94a36      1
    231 0xc18120b4b22c78ea55f8a7acbd7d7082d3c73a2b      1
    232 0xc213c7e9f8e499e0d50afb58478d6079254c0209      1
    233 0xc23485ae4315c409f0175a782841e0f10f12b0ac      1
    234 0xc29b6d6a91f5cb8efecb4722d4150ecca925434f      1
    235 0xc385caee082bb0e900bccbbec8bb2fe650369ecb      1
    236 0xc4f370c6d3164a56971692362a9e488c0992a29d      1
    237 0xc5af7c22ce278cc5ffc008a59612d4db8bd14c7c      1
    238 0xc63ca1263d8c79fae328154da0b7be5d08e9a8c9      1
    239 0xc69b09762e70d773d0f601304517628f6dd3326d      1
    240 0xc87e9dff30a7472329237c4ad222b35b529f0fb7      1
    241 0xc8ba28507aa7f1825db28a7882402d616ad7f562      1
    242 0xc8e871fe50cc7408a15793f4848891456091224f      1
    243 0xc915d266472685f1ef2f974c5f01b4915f0df46e      1
    244 0xca6528e0089d0b3090e1705b7276872f39921398      1
    245 0xca7cf06fc4053f1b74c332750e4e360f6c1f5f4e      1
    246 0xcb25b216e4241535fe0e36d5f83f36a4a30b233e      1
    247 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    248 0xccf6d942a95574a76d703a8403ad3fce8759c5dc      1
    249 0xce40daf73399225724324ac04686f731766bcee9      1
    250 0xd038d146dfab6b92df8ce2c92369f09375fc5b32      1
    251 0xd38919a385cf9bbade3249dad91b8d880b42bee0      1
    252 0xd4d3c85017cbcf26ca6c89ab5767227d7777196a      1
    253 0xd511f7d8738e519820f4e4194bddb253c1ce517b      1
    254 0xd526ebc929877963ee14e984fdb1d63b0fc2a096      1
    255 0xd6705c53d643cf4540547f28d67bdc3e60038fe6      1
    256 0xd6a234a57a3705a67dc67c6fff9f750df690ce22      1
    257 0xd73e55a3f739fbd783f7a2a307831afc31c6510b      1
    258 0xd8032e5bf3bb016c7195d1d494c9222832ae5d06      1
    259 0xd86eab09bb728c6df320dfd03f5a5390e0990bb6      1
    260 0xd8c39a8e2779efa2f3e8c57d9bcd18ae0bbad76c      1
    261 0xd8d417853b2127b8b8ea45ec406dedb244e2ccf6      1
    262 0xd9c0e1af68d08c8c00d418431e8c036662a82e37      1
    263 0xda258e794c0465904e4973bb96ed462ffaa8879e      1
    264 0xdc2d5126c0147863e97e2a5c91553ac45ffa5374      1
    265 0xde178a961723eb49654d20cb8482e54d46c58901      1
    266 0xde1f3ca8d0666dd3d53a608a9da1c4d472d135bb      1
    267 0xde3c597480051d659a3611569a3affa367000d0d      1
    268 0xded440a3b8d76c828db6737c4368ecbd374c2237      1
    269 0xdf947f1e3031fe63bd432f4e7b684dece92c0bda      1
    270 0xe003256f4cc1ae9d7545326efe8f05f12f066c81      1
    271 0xe0e31366067277dde4a391cd5d77f43cdb9ffa6d      1
    272 0xe239488f1efc96a27408ba60c102103d4d1909d6      1
    273 0xe77a08d1513974f9cc89fd2eb043eb68355eed20      1
    274 0xe9d5a03e6b2d7644ae73a07c866966e4fd92a4b3      1
    275 0xe9ff88b514c9cad6c060fdca09b05e7080bb262e      1
    276 0xea72fbceda4e851c9171a6b57a287ef986344efc      1
    277 0xeb697a943f4edc306de51ac5fce1366a30496e2f      1
    278 0xecad8abbd90ee7aedaea07f24849f895cf2467e4      1
    279 0xedfaed62db466ceb580a7804affcc65184a4bc46      1
    280 0xee620a0991d57f464aad452789a4564ba51245e8      1
    281 0xef646a7688ec76c3368b8df136e54394f6fdde9d      1
    282 0xefbfc695b171ec729ac92e7e2b0dbdde3fe201c6      1
    283 0xf01bcb0090cd0f734688ce77ae067da58e9d8005      1
    284 0xf0280db7c831526f80dae97d2d14807ef889aff0      1
    285 0xf0c3a1f1d9b79d5a8decb8e92973cdb56b7e81da      1
    286 0xf1d48b593f11111db5ce35d87fe9d6a6deeae53b      1
    287 0xf2172ba910a5213ca2f91b7bd7782bd6e4e69cb6      1
    288 0xf3a80df4e39486545b2c3e79a79c8500dbd71ae3      1
    289 0xf45cb4d901541000a9513f422532bfcdf1612d2c      1
    290 0xf5559d32946212a8f718c8d6c79c19c6200c9d6e      1
    291 0xf59a58fdc3f820941fa16f53e56cb11e7cb2841d      1
    292 0xf64932b6002fd2a465a72564d80422f762aaa566      1
    293 0xf6aafb44bc183d3083bfae12d743d947ca376562      1
    294 0xfa5e8ad3b7439634099bcd630c5a4653d6a6e62d      1
    295 0xfaaf6e583546295ce96316ec26424f37885668b9      1
    296 0xfb81c53b90942f80a637d59207dad341d22c6df0      1
    297 0xfcb85d4139123b1ec6746d1c487a2201045b9f72      1
    298 0xfea710fb904464feeb6a12bdb668e11646aea9b9      1
    299 0xfea95c4419c395601084b010b7b67aeffadad03b      1
    300 0xfece3a78855b52db21bf868e0141927fc7e0d719      1

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
