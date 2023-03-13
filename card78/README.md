
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:30541       Length:30541       Min.   : 1.000   Length:30541      
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.002                     
                                           3rd Qu.: 1.000                     
                                           Max.   :17.000                     
         name          
     Length:30541      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16810669 # https://etherscan.io/block/16810669
block_hash <- "0x77ba41a680718f6f1dd3d133542ea03060b46a19e75a85bb1c0acb857f44cd93"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4585 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)


airdrop_leafswan    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","MakersPlace","KnownOrigin","Stateofmind","MetaLeafs","Leafswanv2","Leafswan","AIexplorations"), address_remove=address_remove,address_pick=12,address_max=1)


allow_leafswan_singles  <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","Foundation","MakersPlace","KnownOrigin","Stateofmind","MetaLeafs","Leafswanv2","Leafswan","AIexplorations","LeafSwanXMLNDR","Marine","RealLifeAIHeroes","RealLifeAIHeroesSeason2","YourDigitalVersion","YourDigitalCharm","YourDigitalCharm3D",""), address_remove=address_remove,address_subtract=airdrop_leafswan,address_max=1)
allow_gradient_phase1   <- pick(snapshot, contracts=c("gradient"), address_pick=55,address_max=1)
allow_memes_1_phase1    <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=69,address_max=1)


allow_leafswan_editions <- pick(snapshot, contracts=c("KnownOriginEditions","LeafswanEditions","PersonDoesNotExistEditions","LeafswanEditions2"), address_remove=address_remove, address_subtract=c(airdrop_leafswan, allow_leafswan_singles),address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_leafswan) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 12 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0279273242ea0a8be97f682221c0c7f9187416db      1
     2 0x1cc848568daabc2541f58c6d2f3344972ac1f2cd      1
     3 0x3f7cf1e0d51c141884d7ef41378747a505f30441      1
     4 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     5 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     6 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     7 0x88888888eef9d7648d15ad26c68d1653464856b8      1
     8 0x9b25f796dcf939b73d30ab1006d62ddde1a0b1d8      1
     9 0xa7625e30eb5356a0b777e9617b55a92dddff2975      1
    10 0xbfb75e1d59fd27f0aaa5a1a9a301fbfdf9d415af      1
    11 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    12 0xf916b594fa568fb4cf5ca7f0ebeb11ffd9e296c5      1

## Allow Artist Singles Phase 1

``` r
c(allow_leafswan_singles) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 325 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00200fd8210fed9e4131cc25f347da849cb1dfa2      1
      2 0x006661b64fefed1576ac7d41a17c0150e9702583      1
      3 0x00e9baa01e8345d1bbb1025c3138caaa7699eb37      1
      4 0x02fc303a3675de2ab5494cc0efd62ad02d73102e      1
      5 0x056b0a3d013fe5f997f1ba5f5fecb7cc335a8b81      1
      6 0x059a3eb0e9bbeb8fa4ceb5ec67e05a01e182a9e2      1
      7 0x060950d041bf0402b973c8e62793b2b561f8c64a      1
      8 0x061c594850b9fae25cb8f832cf24e69fb0cfe61c      1
      9 0x06be7158a23ca5b7ca1bc7723b6024df1aecb8d8      1
     10 0x074fdc302f8d3c0e8b11c80f2a07bf2a3b8ca855      1
     11 0x0795c1b182cedab5fe3ee2ae91d61abcc5d38a54      1
     12 0x07caab549a0edb644aac592fdef871aa00908e97      1
     13 0x07d7ad96fd130c88ffb995078fc82c5701c55ce3      1
     14 0x09a31e9ea6490991995d4ecec3c5748b993064fd      1
     15 0x09c5577fe587a772dcb44a15e3a4ddf43130e15b      1
     16 0x0a2542a170aa02b96b588aa3af8b09ab22a9d7ac      1
     17 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     18 0x0b20b9bd6ea6f9d75c3eb676e703056e1b7538e3      1
     19 0x0b4ae84e396aee628c562449bc6d49968c1e1aef      1
     20 0x0b59e3e73dbe453e973a876d34397c0cb759daef      1
     21 0x0b8f4c4e7626a91460dac057eb43e0de59d5b44f      1
     22 0x0d1d74535bcabda2da2cff5a53c2b899901d423b      1
     23 0x0d99e9a9cb81ef149ae2a1270a6c7a9593edba9b      1
     24 0x0ed7643fb4eda6a98da2a942da73132ad03581ea      1
     25 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     26 0x114cbd439a59d06d73fa431ac9ef5e2d4f4805b1      1
     27 0x13c942e3f8be4faf966ef03e6038b33d000db22f      1
     28 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     29 0x15624b0cd84e566ac94c0a3cd524bd6d59619dbd      1
     30 0x15b9df1c21f1ea332a289fa43545b0a58f7b7046      1
     31 0x1610634c1fcda56b9934d67f885b30dc73f938cb      1
     32 0x177661440b721736cb79cc3982f6b43058132348      1
     33 0x18bcbab94d57a3b8e0935289c8f98b2c23bcd5c6      1
     34 0x19fedef56411bb49afaf65f7e4be50629be17632      1
     35 0x1ace22a56af1ca4ca3edda6b22b1ef0007513684      1
     36 0x1b5f4b3cbff53245ae516b5475be336622dd5dc4      1
     37 0x1c06aca8d76f5d32858eebfc2fbc1873ff7324a8      1
     38 0x1cf0bbc0252e8b443012f7ed098f292da566eda2      1
     39 0x1cf7ca3e42eb5f2a9d4091c2d5b1bd3ed3bced88      1
     40 0x1d5db8ba6cecd04f2501b5535b884547b61f2d6f      1
     41 0x1e265209eb69c8c16d7aee66a818a6455482b41c      1
     42 0x1e3a64ce9b4a573674284ab590b4fc538746fa21      1
     43 0x1f1511e6f4e9ddde7b3b33c9a64b86e8c1ed9004      1
     44 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     45 0x2093dbbeaf3c3a52f42f95b6af1cb6bc99c3e368      1
     46 0x209af07a4b6b2d15923ffa8c3d41cdac49bc6c4f      1
     47 0x20e7bfeef1d38899ec88124829dd34e711c1e26c      1
     48 0x21339e2ce5ce1d7a19a28e774513619c0c6259da      1
     49 0x223716c5aa5fd892c0daff1f24d4531272e8b12d      1
     50 0x243add5ce61bd821d19c8ea27522462299b87d1a      1
     51 0x245145accb812ebf3c7ac307f326eb7df73bc223      1
     52 0x24859f1cc0cc7e265205a5a1b8e2c2b69af0127a      1
     53 0x2541f511078bc8c1dff033e7bbfe51ab4e7c2a55      1
     54 0x25fc4d1aa94883fa2c10ba1583cd154ba0a73007      1
     55 0x2610cdc16f4f88a723009497dfb8ab7b6473ce01      1
     56 0x2719150aea3b6becfc85229f8a89b7f94aecce1b      1
     57 0x28b39cc34568514a067d9a691668e30356748471      1
     58 0x28bebbbf890da864c0db39e278b868493eb7c8e6      1
     59 0x2a07272af1327699f36f06835a2410f4367e7a5c      1
     60 0x2a5620208693ad312de212d359dd2fe364744e6b      1
     61 0x2a8990902b4810d78ce9f4c76cf80c31ad57972b      1
     62 0x2a8e6277c7dc46572ab32d9ae1aed159c24fdf4d      1
     63 0x2a9948d632bd060744bdbb82b5bcd9d03bd3016c      1
     64 0x2ad489368b79dab42e019ed95d879f12ed40bc59      1
     65 0x2cb3338f518b2d61470b4d1d503bdc84c1a04ecd      1
     66 0x2ce48f39c5440bf2216ecd04663c563cae49ccac      1
     67 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
     68 0x2e6803e20fc55714e7140f516823d2ae27a12c79      1
     69 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     70 0x317fdbf289a41435df30efcb00015f95b1eaddfe      1
     71 0x34805e6a3796fb04e82183667a78c2f7bff29170      1
     72 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     73 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     74 0x35217485853a6d756b2505527fa431423e0d4de1      1
     75 0x36b454b1542d0ebdef201630498c37cdc9015f80      1
     76 0x3707f031d5bdb85f457a2804088f0462eedf6bd6      1
     77 0x370ce44dfc5f1439a707173159da7c9218bdc0ba      1
     78 0x379f75d14f5822e3491d4618f9517a99f881da0a      1
     79 0x3a7243fd7a1925ed2edf75cb0df827c65bdded7d      1
     80 0x3b22621a2b0e1f8dd3056975efc2150d5fbee980      1
     81 0x3b63410fadbda0ec0ba932fad2997edd7a842679      1
     82 0x3bfb16301eb4478cea40f900a04fafe69d3aaa61      1
     83 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     84 0x3d81a716c76c30b6e2f272461bf9c844aee7469f      1
     85 0x3d889b6391506032f9e9380a8f5cd3199661fb72      1
     86 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     87 0x406f4533b2f6209812d7ef9c7d69b8c54217c208      1
     88 0x42f34449209059717e6c48ed0110783a7df82abf      1
     89 0x431573685058e5d480fe383dc1fd3b644913c239      1
     90 0x433efcd1875d1a8c9bdd6f5a94a65485ffc0441c      1
     91 0x437ff6ab3f6445cc2c29699157b9fcb26900b1ed      1
     92 0x43b02f7e4779670f3e12d5fefb2119ddc4b35436      1
     93 0x44022a7eea59beff273e0d033d76884fa6da0822      1
     94 0x4426863132ca102a9306c0e6702d57e7ef3d02bc      1
     95 0x443d234e9e6e0c0ceadc199bfc9409958c4736b0      1
     96 0x45369628387a6952aa6b8ace18b2bff5e2c04322      1
     97 0x478bb542f7658d635abba67edb987806dff5b83d      1
     98 0x48645cd1770e65bc68bd0dc42874289155537bdf      1
     99 0x491888d80ef743e814bf70e37bd919874f2671c0      1
    100 0x498cf91288ee77aa0ae3425d3a7046c860c06520      1
    101 0x4c8bc5dd54bd78a2792b578bd2fc919196e64d77      1
    102 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    103 0x4cf150b0116d503bf1aa79d0ea64e2f8b21136cd      1
    104 0x4d096b18c8e469ce0bb7173464f8a64ca9a9b042      1
    105 0x4d255d9b31aa437a0df8d02565c99d0faa6ed49c      1
    106 0x4f51836e363342c52e9fcf2ea892e9ecf702812f      1
    107 0x4fc74abcb0e319f4ddebb1b7a46ad60743516d10      1
    108 0x5012d68f846bef12d8c8ce6ad3de311afc0706a5      1
    109 0x521ee6589c870aea565671a759f9b95cbf7f44e0      1
    110 0x52767b52d90311790623ecc66292505ac7774471      1
    111 0x56ac3d05d81a3e264918dade66e5ec1ab16c6082      1
    112 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    113 0x579d7b6d6aefb3ad424516ee3089a632e0ac42f8      1
    114 0x57fb56ee20f4215dd631c79a63b42ba9c27efe24      1
    115 0x58aea6498d4b9851f1bcce9ded13691267468649      1
    116 0x59828178d98df883e6c96a11498d53bb7e953b6f      1
    117 0x59b6688e18d7dc4a8046d2ff1f0f520c361e1c3b      1
    118 0x5a2a4a9d322f8ead0279546a6cfc394e3a330d09      1
    119 0x5a418d8bc0c074a4a8fa88d1322dc51cc1cb9d29      1
    120 0x5a813b82e4520f5534ec617747c1887b9f03f051      1
    121 0x5d78452298d002a3e1b14eab4b57f8513022654c      1
    122 0x5dacf1ce88a5acd042a3c22c9b15982927428191      1
    123 0x5df4ba87a6b5d1aa4c4393a6cb4b90ed1c7e09c7      1
    124 0x5ed35a54d2cfeb531c9070ed4dfc27c305344ada      1
    125 0x5ed432ccb38f8f954cac617b1d6153f58bfaf0c9      1
    126 0x616356306a7a89bc470508cef96b62f7fadaf91e      1
    127 0x61dc23f0c8245c547e9256c9d92865b5e3b44c78      1
    128 0x63b7d0006102e672e6936fcfe6197e4c782f51d4      1
    129 0x6431ca52424fb70b482729316bfb38776bd2891e      1
    130 0x643d46872742226f1666660d4b4100151297d959      1
    131 0x6486d5dbf01f1edc446c87220003f49edb43972f      1
    132 0x667566c500a0d40b1fdce38af3a756a7f45d81e0      1
    133 0x669d8770f4a24edaf223273ba8d04e654fa7a84a      1
    134 0x6772e726dff4ad2541c859b0799e8c18c6e2c489      1
    135 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
    136 0x68d503ffd7659694f36ca8fbcce21fbb47490472      1
    137 0x698e9a51378ed9b292e1ea27cd2ba419aee0e4a4      1
    138 0x69a0841248dff9b5bf7c58b673ae837f9f2eb6cd      1
    139 0x6a7cc5e205bce058454e1576419f3dab1ce21637      1
    140 0x6b1050c1c6b288c79ac1db299dc481048abbbbcd      1
    141 0x6b995e13b1448df63ac5b44d032b85f6e6ca31d5      1
    142 0x6bb8acab4849aedc8dab242f8ae3e4604d151081      1
    143 0x6c386cc632915a95af78923983e22d0c529ac9c9      1
    144 0x6cfe66a3a1686005a2d1d218ffa6418b049709a4      1
    145 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
    146 0x6e388502b891ca05eb52525338172f261c31b7d3      1
    147 0x6e5b0de0add71e1052b11abbfe6c433dd0466fb4      1
    148 0x6e5e3a5dfb5d218bd81d8083f11aeac7656ea668      1
    149 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
    150 0x6e977c8155d914e7aa99331169939978a4d1b102      1
    151 0x6f398e7872ac5f75121186678c4e6e015e83c49f      1
    152 0x70482d3bd44fbef402a0cee6d9bea516d12be128      1
    153 0x711ea5d3073bacfae358438f711e71d6c15f3a88      1
    154 0x727f25672f4f2815831ed496c87b33faeb639238      1
    155 0x72a0726ae7a9054476a8c7e759962a4da667175f      1
    156 0x72d229bea4aec5d1b2fe5f0c576966c43f9bfcf5      1
    157 0x733eca8fb32d47a27931726c0a7e966e3edb9c4a      1
    158 0x73b77ec753862706b986995ca69bb2e023116823      1
    159 0x755d209c13b28e2da074d1a3ecf87a5d3f7573b0      1
    160 0x7596e3dedec292a675a6ca081163c1185a243a84      1
    161 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    162 0x79122374ecbad9cba0ddf0e0a5f1b676462677b4      1
    163 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
    164 0x79fccf0b331cad4f00132901d77e9f8988943060      1
    165 0x7abc25c069c8194e6b501782c57904a8ed940a57      1
    166 0x7b022d4c96ae8408280a8bebd99707755b4abb4e      1
    167 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    168 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    169 0x7ed3f5b5ac938e17303b5c9557901091fb483abb      1
    170 0x800e962f595f44430648642a2c152ee61156f7a8      1
    171 0x8083def24331022579d60a2f6b3654457cefbf80      1
    172 0x80aa186f646fa560495c4e965aa983b958a47369      1
    173 0x813d6e94cf379d9da155cceb1c3fba68d18709ea      1
    174 0x81c77f8ac639955a7bfd5f25f9ab533b0d388f34      1
    175 0x8205ecc26495ff55e1d6d84f59bd82c2dddd646d      1
    176 0x839c1d8b81cd8ee8eb54fd6b0552e1404c17a79d      1
    177 0x841412043d4e6b727bae0ea5ddb5cc73dcb699f2      1
    178 0x841e54526835b674392e2b8f0ec19be0577add87      1
    179 0x8426ccae3fd861e6c89ff81e9a1a641a2eb32845      1
    180 0x84693096d7c628a3942c7c8bd5920d35a85ac234      1
    181 0x851242d6bdc176dd25f8534f054b65701ab15121      1
    182 0x86b0db23f26dced67e7b50c5d39a04a84bf249b5      1
    183 0x8889ebb11295f456541901f50bcb5f382047caac      1
    184 0x8aa04ed102fcc87f6c3851803048321a3a211448      1
    185 0x8aab946e4cb2d27adde693aae8f8ac5003b0f0b4      1
    186 0x8abc6546a2895c27d2165776931c1276c258e903      1
    187 0x8b5a1a96fa5a485a202b0797c26635b5ad687c60      1
    188 0x8c053c40e688557a9a12f4c5896b8285ad4af5e9      1
    189 0x8c40ceaa3372abf813df010de33afd26209b2be8      1
    190 0x8d1bab837081efcfe3469c8f99a334fb0fe69cc9      1
    191 0x8d3f70206c4547f0844b9d2a6f3639634ee0bed3      1
    192 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    193 0x8f8d26958922a390f54057c4db3d181cd8b68662      1
    194 0x905d13367f3bb940072a133c81563f1b1a6779ad      1
    195 0x91940d536b99c0aefb3923b911e7e4798a6d87eb      1
    196 0x9334955ce4770f6991db8cf469b1a1b2184f0e05      1
    197 0x957a85e0f23a5c6e1ca7c8e869fefd01cc406e0d      1
    198 0x96951beae8a3c191800877cdde67dc12a5881584      1
    199 0x96dc89dbe84970ee42a2f0b35fb50599e6745ff8      1
    200 0x97a99da15fc89d96b3fbe673ffd8f69f6ce7c9b1      1
    201 0x982e09ebd5bf6f4f9cce5d0c84514fb96d91c5f9      1
    202 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    203 0x99e0a9e19775b61b50c690e8f713a588ea3f28bf      1
    204 0x9a192d7afe4450f723c3a7be88f66b1b2b3b74dc      1
    205 0x9c10aeedafdf62c190a0da647097c6ad47dc7698      1
    206 0x9cd9aefbd6d423f69f5c9caf2fa9972381012b09      1
    207 0x9dea4aba5aad3cbf01dfd5ef02bb14e00063625d      1
    208 0x9e283900715b62f8923b14fc2cb64088e2d5bd5d      1
    209 0x9e4a9b4334f3167bc7dd35f48f2238c73f532baf      1
    210 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    211 0xa091881bd8ec3fde46d3bce1ac05de783ae431f8      1
    212 0xa1a2e01301088bc187221dc909003e631eb159db      1
    213 0xa33ee8da1f8e8af1f189a252364bb11df5b13ac6      1
    214 0xa633f5b4578b00e8598a16095a0a0df02ad95aa1      1
    215 0xa7f2c87faa3ac558ebfdc7f4c01840b6d1cef2b9      1
    216 0xa8893706c4659161b13678cd83a6bcc5836c48f4      1
    217 0xa974c36f6c8968b1ef7d8e07017abcbd710dbc4d      1
    218 0xaa08108752a4e2efc98a09ca674f0990b3ffd1d7      1
    219 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    220 0xab50c998f2d25d62505e6325cf9ff6ec755a821b      1
    221 0xab7473da0af4658ae2879cb641d82e7652721486      1
    222 0xac10ca20fe0977ea9f448e73f6b670d9ffd42ebc      1
    223 0xad25e0585f3ce763a9a95ab3f91e58f345dc2106      1
    224 0xae6839a38bc75a27d64419778ce39dbcec54a2d9      1
    225 0xb232b78f8e6ee95caea3f1364d69a8da23844e75      1
    226 0xb23d2ca9b0cbddac6db8a3accd13ecb6726d4ee7      1
    227 0xb2d56e9bc10640afef23d65b3d4956f50a8f382f      1
    228 0xb32b4350c25141e779d392c1dbe857b62b60b4c9      1
    229 0xb3589bbd2781aad79cc2727fd5abb762b522a7a2      1
    230 0xb3ab08e50adaf5d17b4ed045e660a5094a83bc01      1
    231 0xb4b81b693f322f6dadac99c9de4366970e99f6d8      1
    232 0xb5a4e4cb6aaa9f6ea1e00444251682e0f20f86ea      1
    233 0xb5bb31bd1a14af9de9cee1e8f878834624a68b02      1
    234 0xb696ed9dff1955fcb10a251001c46c6b4129ffb6      1
    235 0xb83e94132e56fac6d21c5e67ffec0111bc31a61a      1
    236 0xb84580a14e42c74efa1213ab2fb9f98da33452ee      1
    237 0xbaa6166b5edfa65b7693efd4be1220946a2bceaa      1
    238 0xbb34d08c67373e54f066e6e7e6c77846ff7d2714      1
    239 0xbd7a1c16d7b3af4af6d1a0fd7437466d765656ce      1
    240 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
    241 0xbdd95abe8a7694ccd77143376b0fbea183e6a740      1
    242 0xbdf53fe485928d2f269cb344864d539c5862aeab      1
    243 0xbe253a7bbacc4d76ec48f66dab6293234267c8e0      1
    244 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    245 0xbf7c5f30057288fc2d7d406b6f6c57e1d3235a27      1
    246 0xbf8090eab036022b7e0c77d9e34793f1b756d94b      1
    247 0xc080324f33d5a591bfb7e7d504e62d20c7ebac48      1
    248 0xc23b78f47cbf3f4ccf3bfe5899dd6a20c308ceb5      1
    249 0xc2587a1685b194831c0e4a8927df0c33437fca3f      1
    250 0xc2ac64b67398081fd86b060b61def2835c3e9d24      1
    251 0xc452fdb85e809d15b972ec588f24e27bcaa5ff93      1
    252 0xc47d8fc56e7037ac8d943058a219a03442f0c7e0      1
    253 0xc496c3d5393dc9216ff54227e2238d45144ff359      1
    254 0xc4c6c27b2259794a1dd35d438e703281c0e4a004      1
    255 0xc595b41608621f4b312bdf4b500a446efa0103d3      1
    256 0xc730ff9e249d257c689a858204bd5d6c9a47973a      1
    257 0xca49cc398fd82904986487bb11ddd451c2f17b48      1
    258 0xca5572053d89c8602c29e23d51646f9aa43537f7      1
    259 0xcb73b46d4826fb305408ed7b2fbb30b9c9978325      1
    260 0xcd5ede6d15bb82d479f993c41d14ba422cd47983      1
    261 0xcdcfba84cb4f9f5e9837361e90edc9a22bb559ff      1
    262 0xcdd83cb19fd9fe5dcacbe27f6a0410bc374f43ca      1
    263 0xceebeb0cbfa8b6fb319dc2c3323ecafafdbd179a      1
    264 0xcefa22191e49d3d501c57c9a831d01a09f7c1112      1
    265 0xcf354c795a3cd1dc000ed1cdb8b26e3b8eae2820      1
    266 0xd07d220d7e43eca35973760f8951c79deebe0dcc      1
    267 0xd25e36f54f6abb2529100034426bb3be74147837      1
    268 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    269 0xd42d07891df502613b570d55c2c228e42968afeb      1
    270 0xd4e5d6defc141ab74ee65461677087b43f9460a5      1
    271 0xd57f5d1c30de1d45149bc2d9cf528374d17f10ab      1
    272 0xd5ff53f48f14e9409b581e41a4cddfe0e97dc724      1
    273 0xd739429bfbdef8f25280bee993c6f5fb2e884a3b      1
    274 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    275 0xd869decd794d2a6bcfb760d3113ef9cfb7681f98      1
    276 0xd8f469c76bd1d6ff4208a9cbff9749cc2eb1c9f3      1
    277 0xd9319ba104516ae3227da9e9552e2d5b83d68d72      1
    278 0xd97aa65000f6e132849d00cbb550bdabc6fc4440      1
    279 0xdaa1131400bb96fad912bdf5d329d43f55c029bb      1
    280 0xdaaba0e37d08fd1e8101034349f062182092540d      1
    281 0xdae354431c3bfea69e8140f3ea079133a73196f8      1
    282 0xdaff7ccb1b7b9d008b977be2611a1149c797f754      1
    283 0xdc184b91eba750a4fb62c4d8e642e9ead9b6cb55      1
    284 0xdc7f3c5fc1ce22e8d8f2c35029055bcc06ea1dfc      1
    285 0xdcdb5db0c724cd25b6b7c19346b435a5e87024c5      1
    286 0xdfc3554d72a3a1c2340b9ce54a9f6310e1fe4235      1
    287 0xe4f44f453fbafdf39e7af6720b505fb64b041666      1
    288 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    289 0xe6468262b5a34057676586dc1976de5f664b7083      1
    290 0xe66036990f24b3dff4c192fcebdddad83d4ebc20      1
    291 0xe82481f42d3ca7f1ee8c7103fec59f0944756976      1
    292 0xe8a48d600fc6b5a8ca5817ba5625f139a9793460      1
    293 0xe97dbedea928f579486a8be028c58787566230db      1
    294 0xe9a0dfa349bd7af1bfd68dcb6f39c93cbfe541c0      1
    295 0xe9ca5757aa58e3f852d8d6c20581fc4c39769549      1
    296 0xeaab59269bd1ba8522e8e5e0fe510f7aa4d47a09      1
    297 0xeb0e194d60f307391bbeffdc6d4bec8f56aac045      1
    298 0xec7100abdbcf922f975148c6516bc95696ca0ef6      1
    299 0xed9cbb45e7d51b4a7db0a4923b65fd20408acabf      1
    300 0xedadfda063374ca9f7f7ddc0873e75c437dd6e4a      1
    301 0xee980ff87d65fca2d73d9ef8b9e36c1b93aa9a2b      1
    302 0xeea5e23c7a42bfbf2c5bf0228e82f2f2dddc9fd3      1
    303 0xeea89c8843e8beb56e411bb4cac6dbc2d937ee1d      1
    304 0xef35a2067064de83a239413f52775640f5f2151e      1
    305 0xef3f063136fe5002065bf7c4a2d85ff34cfb0ac0      1
    306 0xefafa2c48a9f3bb64fadc1447d786c9e21963c8d      1
    307 0xf013e5f697e84c6831220a21a96556242ee9ad86      1
    308 0xf0508dc132fdfd8a10e2944ff2be49c526e3b727      1
    309 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    310 0xf25ad24b791e37e83f4dadfe212e0e9bb45a1f8b      1
    311 0xf3ade0af18fa586bfd59b8ab17f5849067a162a0      1
    312 0xf3c8643253cfec419435e410bb27cad677ac7411      1
    313 0xf3d7f04be645adbdde46b54f3f760cdf8f7416a5      1
    314 0xf50131d7d2b5239fe1e934658fe3f6131532a437      1
    315 0xf6a5ac61988aa2f329a1c90becac111e9e3a694a      1
    316 0xf85d8406acbaca3c3e69b2fefaeeb050c427337b      1
    317 0xf8bf46a9a2de6477a4c3586fb3c115ed1f234d53      1
    318 0xf8fbf33868c374d8ad5277189bf1c86daa2913fe      1
    319 0xf916719c7251e109cf3d1a977b6cad198b630c32      1
    320 0xfc9861c74ec615f4bb6c68de02b9d9930f0322ac      1
    321 0xfd02c03137288a87c453814ddab5fa3553e34dc0      1
    322 0xfde4eb36beee2484480712a8a790b3eb3c240d4b      1
    323 0xfe3a0b3f89c1f0a8562d6db445f68d988a3bd2bd      1
    324 0xfee7d8afb8379780ead0499dec1ec988f4e75aad      1
    325 0xff5be09b283845774f29cb114a42640f253e61a7      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 55 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    10 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    13 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    14 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    15 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    16 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    17 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    18 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    19 0x59068075a799594db03c0255eed68e8e121155c8      1
    20 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    21 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    22 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    23 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    24 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    25 0x69e68074f1aada957edd39c5eae0069973343f30      1
    26 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    27 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    28 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    29 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    30 0x82139687faae8a29851902783e02e699de0e0846      1
    31 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    32 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    33 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    34 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    35 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    36 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    37 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    38 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    39 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    40 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    41 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    42 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    43 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    44 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    45 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    46 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    47 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    48 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    49 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    50 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    51 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    52 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    53 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    54 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    55 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

## Allow Memes 1 Phase 1 (Random69)

``` r
c(allow_memes_1_phase1) %>%
tally() %T>%
readr::write_csv(file="memes_random69_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06d643b9ee73de07ade8b5c9fd520fa2aa28262a      1
     2 0x0af5cfecd1b1be7cd808bf466470db20cb65c51d      1
     3 0x0b7576a64a0f4b4924d55ed328ede4979446521b      1
     4 0x0f0ef9408ca5b0dc364ff5921bf544c811e9c94e      1
     5 0x10156766f659fa455b89410579185bb29a90eb9e      1
     6 0x15bd2cbaa3e06b4fe9c1f7938521c63a42fe5328      1
     7 0x1bf83e708a446190c37d81621613a2b37eaab180      1
     8 0x231e0412148ac123c29c99ee2ef32c5e1b727977      1
     9 0x23be5fba1ba9c07ee676250040db99474da72997      1
    10 0x263da73598db8ecfedeb2ff17b5f12173c929929      1
    11 0x27629bf330aeb8ae9ef2ebe7b148e97b5dc6c035      1
    12 0x28b8906d06784d25557b6b6d9705455b265809b3      1
    13 0x28b91b119f139a18b5688f6327c4c4eb839ed644      1
    14 0x3a4c7ac6873316e2c62a168150c49362ea4a77f5      1
    15 0x3ad3030e9623e9e09ae21cd0632aaa62446a5c97      1
    16 0x3d5296326f1d61afb52f01eeb931a0d329e24b36      1
    17 0x3d5cd234a502a18cd6d65432259ce0772c1238f2      1
    18 0x48d26fbd89dda10c72cd8dc7b82b9eee0af6b48d      1
    19 0x4c29a17253d5f5f0acbabbe2117478ec6204a2d9      1
    20 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    21 0x53817bb6513f1f43dc932480ffa928b85a588001      1
    22 0x546aa1022b9840d3e1b2c89b3c679093a476fc0f      1
    23 0x54dcb363205d953d620809cd315640cc36b679e4      1
    24 0x5881f3b480f5625a65847f42b5f4df9b530c6453      1
    25 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    26 0x5d15989394195207534a9ecbf582d712a2d2ebe8      1
    27 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    28 0x66567f4ec7743e58713f44f083da3de78a52556a      1
    29 0x6facad44e126f2cacde44a0b2b85ce1b643495d1      1
    30 0x7ca83da48606cbfbbf12af0294dfb7433c0393ea      1
    31 0x80b389d7f56915f496a1b88e38f19a65516cf49d      1
    32 0x83bda4c6d6238ff9d6bb955344e6849ac1ff21a4      1
    33 0x8488f78d943cf6b5e1231c5370fed186ba7a3044      1
    34 0x853c69418605529a68907aaf7789270e3cf69d97      1
    35 0x87d8dcef48c9632e87de450e55941d68db0b1463      1
    36 0x8b5ac0ed13a1cd4aefa2a22284ebf92d067a078d      1
    37 0x8cb439e29790550d7774fa076d7940b1d7842412      1
    38 0x8f3a658a9736530e89c242ef572d7198cfd540ea      1
    39 0x944756efc704ca4e60960d4b8d741e85df216995      1
    40 0x97da0a74230cae791eeea9a768a39947ec152ea8      1
    41 0x9ff3d60cef60e835f142ce5e13a180c70ac89549      1
    42 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    43 0xa12f331e927de32c3516982a21407e676dbd5e92      1
    44 0xa4766aa40e3f8c0e5f4d95be05a222a574f98f10      1
    45 0xa73bd72b4bf7875d4b4d53581bef3e0eadcd051b      1
    46 0xab0e2f3c3c29deaa7c3ca0dacec97a33d5c26708      1
    47 0xaf12a9af19331fa9b89915537ce9d41bce5e4c5d      1
    48 0xb23be5baef9983a1c72d186774b537d95c5d218f      1
    49 0xb2a31c6e088235ee6e279f69dcf8b7233d3543dd      1
    50 0xb6461bf5223dfe357745895ef4473024e9dc2e20      1
    51 0xb8257f4f9e8bac4206ea02d3dbb3a09422e42d75      1
    52 0xbe527aa383c9979d0133d27b8ea1c43b694d6f9a      1
    53 0xbe58a1b85d11e2083bb7c5766284ed7848448d1d      1
    54 0xc2d9eb979f4679adc0a4328ac5caa096a03f18c3      1
    55 0xc59495774c3af7adb580381aa7ca5b3cc3f3888d      1
    56 0xc7382780c13d05fa6ed857fda3fef3d1b9e0e00d      1
    57 0xca04e939a0ac0626c4a4299735e353e8dc5ef3ec      1
    58 0xcd76da10cacec513822cbdbaf9044682dc8d4fa4      1
    59 0xd63dc86e7fdbe5923481b96beb2a5a79119e51e7      1
    60 0xd640c898b0902bd02f69de0fe8d0bd560956db76      1
    61 0xda39876a118f1690e584351dd01b785a8f1297ed      1
    62 0xdfefbc7c5f2ec0313734eca58bc1fb17210bd34b      1
    63 0xe2880a450f9c565e01a91e83cbdce2b695b7bc18      1
    64 0xe63586f0ebe319d338b60487243021be9bc596d0      1
    65 0xedd1f30d69898e4cb710cfb47c6114d31e6fed06      1
    66 0xf837203e4fa9c139e41ff3241c434411930c1081      1
    67 0xf9e3f1e067e36d8766b53a2a2fd0fa0ebd08e210      1
    68 0xfa766f029e703d341cc5386b9a4cf4d52be3b6a3      1
    69 0xfd0da64406a3e52b0eafff28bbc46baf86205e13      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Artist Editions Phase 2

``` r
c(allow_leafswan_editions) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 326 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0007b9c4d506c6be7b4a3d454d078db5899172da      1
      2 0x01805fa1d279434178e4dd73e848eaccd93d9778      1
      3 0x01909d7ded4d972c0171f2d5a85424cd0250f9bd      1
      4 0x02a24225ab381d5442f05b247f0558288b89c2be      1
      5 0x03777bc796f13863644621acacfc18a052fc6a20      1
      6 0x03f58f0cc44be4abc68b2df93c58514bb1196dc3      1
      7 0x0412b93f98dcddd486859ef3163d1edb45fea3e5      1
      8 0x04347c18218c11bc5a43c05a1bb024c831c43433      1
      9 0x046cd19a339761c8f62ffa5c42023bc5e07e7c5c      1
     10 0x04c54c28c7ea539a66166646d1a895d760f32fc9      1
     11 0x04ecb5a1565aab40632698244a92119ca56367a4      1
     12 0x06186e1bfb2309c3cbc9dfed039a45d7187bd6d1      1
     13 0x07650e5c7bfcc3818de03bfe1f05179974d7e34d      1
     14 0x0811eda9769c41a1a9510d1990b2738b27dc54c5      1
     15 0x0a8825c03100e6cd1a82266729bf90f7fe71dc6d      1
     16 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     17 0x0d47d2f2d53324d85cf4ab381f807a1f1f0653da      1
     18 0x0da6883d1beb1f0d8e1e1a3eb5d369d64d102f68      1
     19 0x0e7a408a775af29310970de51b59501e21eee87a      1
     20 0x0f5ad36cf3bb8e98cc185e7385ba063fe4cb2018      1
     21 0x0fc1e682f966e272599bcc53cf4f8622303aef0e      1
     22 0x105d8f9b6b8586b89e73a595a1470aa55814d951      1
     23 0x10ac96adabc52de2909da2b1b3cb6821830449ba      1
     24 0x115f064602187027843e58016dd1e9af5789dfef      1
     25 0x11ae370040f234d35fd205360a56e9af516178ce      1
     26 0x11d3502407d152147ba2ca2cfe78fa8dc8e5ef70      1
     27 0x11d7c4af8960cab1324893dd06be55b28580299c      1
     28 0x13a7d2fb3f73b57ef539c35c4ec59ed49b1833ca      1
     29 0x13cb497399225e9cab59f578b2cf378546cba3b3      1
     30 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     31 0x15c7ade742ce5b722248292756aa3bfb078a3a81      1
     32 0x166982a9d81641f2136b96f6088c1c9648056fbd      1
     33 0x16ad0fe1a3b740316706506dee1f42b365364486      1
     34 0x16c93ec97512832ba4244cc69527530d358db0e5      1
     35 0x17124e15738b30d0abd345900f0a98049c6d55df      1
     36 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     37 0x19a68e5a24ead474cdbd1e74864317cc7ac3afa1      1
     38 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
     39 0x1b5b34d20b994631fce946281bdee1d302b0b924      1
     40 0x1cb65b78c71606090917e425dada70e976f28fc7      1
     41 0x1f9724f3054d8f9fd28349067f796e1491d7d1c9      1
     42 0x1ff2aa2beb5b6c33a56e97022f5760cb3195ff2e      1
     43 0x20d40d87c0eab110330f8e95240f3af78acf3083      1
     44 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     45 0x2383eb9a26e7d5aa70eb6c31e9c8c6fc5664061b      1
     46 0x24a0435dd391a6962d8d99ce19f51f2b540412b5      1
     47 0x24af52e99df6446831be2ab94e6db7add6a3b2f5      1
     48 0x2618f923dd7797e1442ef0100992a21015b7c788      1
     49 0x261b89e16244fa9a1288e75d06544992a7d63768      1
     50 0x270e8014a75dfc51c54349e7ebed8363a5898425      1
     51 0x271f2d33c88133878a9eaec8091de094c617013b      1
     52 0x298b8bd664349c6f86d2d4c36c81336eeeaf41a0      1
     53 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     54 0x29ffebd73dd219b601ba99888bb5d16d58eef222      1
     55 0x2a005cc6630049a058cb47f255046ac65ff5504a      1
     56 0x2a193f2d2095341861b991cd6fd22ec1e9b9507c      1
     57 0x2a40e8eb5ce03cc5327f8326040781cdb8e36b16      1
     58 0x2a9e00633cb57a567eb72ba09e9adb2334c97487      1
     59 0x2b85b841b85fe6bf05e1a41330fe13882ed1f286      1
     60 0x2d3e2776404cee093599d429257b6b42b14912d6      1
     61 0x2df11633fcd026e6d57a29ac401750befef26562      1
     62 0x2fb150e4da26c7b34c50660207d09367480a77a1      1
     63 0x303095f338b59b9bd275064bdac974206f01be9d      1
     64 0x30559da92dcaf2786afcc71541b46dd292d274b5      1
     65 0x30b399b1900e1e2c33bdc08f0544efb15ae253d0      1
     66 0x31aa460581818874cfb238df4800e91498ac28c0      1
     67 0x323e6bc721bf81a5292a9c91accdb4315c3f1ec6      1
     68 0x3263eccdfdb1ccccf51f02806fc013f3b725bf89      1
     69 0x329debe3c5ac91dee9d58ee637a915471ed02679      1
     70 0x32aaeee01a1bd496c57fc448947ad8136c6f4565      1
     71 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     72 0x33fa380088a9bcafd0b2dbfc40cdefd60515329f      1
     73 0x34ef24d450c5a4a339f2c4c89c8a3161228584a7      1
     74 0x36baef820b43bed7011c6bedc609c0a6a0d499a2      1
     75 0x38dff25a32d7281d2dc463a2a0f41af22ad034d4      1
     76 0x38f1dfdcaf2f0d70c29d4af6a4aa9e920efe8b18      1
     77 0x390ee2f50ce16972e3ffc8ef2a0b47e500f5ca46      1
     78 0x3929eff8e0feb3166992942d8b9030d95646c3ce      1
     79 0x3a0b2bff978d3ce69caff9fdb9095bb513ea4d24      1
     80 0x3a8a9caf13b05159569f829377ce151a2edc8fb8      1
     81 0x3aaa53cf7ce5ec1d6ee2bef8dc9d473a4364614b      1
     82 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     83 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     84 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     85 0x3d4568c59adfe6a045ac68f00df27e19a61c2a5f      1
     86 0x3dcff084bd2d9c14e45fa28b61775420dc712080      1
     87 0x408388219cba8fd7eec20865fdbf7786f4cfd145      1
     88 0x40bdff7ef927be9ab215e3667dcff3494dcf56df      1
     89 0x419090a6ebfa48e2b0be7118c5b8ab643808e710      1
     90 0x41ef42bb9cb016deeb25d7ca99cfa2f464e3863f      1
     91 0x425c9d1396c60e3235b48b68a642fa9ee4e07d77      1
     92 0x42814cfb08b3a1f9fedd618d35e4e2a156b4488b      1
     93 0x42d6977c0e0596dfc681afc637cfa53e48301488      1
     94 0x43022fb8cd87f7fe5b9cf7d646a5680afdd108bb      1
     95 0x439bde89bf67de2c3beca46e9683e4d738bc6221      1
     96 0x4403c07105ac47e02490792ebae235ffcfe14b31      1
     97 0x447646610380ce8e11e4edb0e1eba238cdb2e18c      1
     98 0x44c2079fe6c6e331c276e1a60a40a8c4e48ed0bd      1
     99 0x45742d640011084bccd9cbc55dfe15671f694aca      1
    100 0x465d46e45ab8868035829ac53cc2c25a578aad13      1
    101 0x4661e47f96883c3e3bddd8fdd8dd56a14648738f      1
    102 0x46ddfb43370dc8611e834dbdbe43e8164fbd6001      1
    103 0x4738c97a2752b2fc171659e46103c36110466673      1
    104 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
    105 0x47ded080a4ef933e96e357d468363a8a0cac03f0      1
    106 0x49bd3fdd5fdf30124dd77c137b178de74fe8649b      1
    107 0x49d0e33e576d637d5b0148f241ee3cea1b8fb531      1
    108 0x4a1a329b1642676a09930f20d1d3a164a18387bb      1
    109 0x4bb87fd811c5c5719222c2c88153266b3001482f      1
    110 0x4c394af636861334af33b9f76ca15f2686c383b8      1
    111 0x4dd7f16dea28f8e68201549dbf0df2e504a36494      1
    112 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
    113 0x4ea033bd2b7caa2ea03cb5b87e98bc5a339f302a      1
    114 0x4eb8a34d631cbf724a48d5701b57bad24c1e4779      1
    115 0x4fc3b50ab3c30d294ab2ab8b06bd178c54157a6e      1
    116 0x534e920f195732d77266e9b903829e13b3993164      1
    117 0x53be4c1a0c36de8601073962fa53c2255cf29450      1
    118 0x554de79a001002a2d71ee48aa7c17faf298f2ddb      1
    119 0x55dece5aae34a50840259d2d7a2da363bf54f512      1
    120 0x573bc03f18f41b200b949a973d67b453a7ef11a4      1
    121 0x5794f7de490ea3f193ca8b60373db91a86fda811      1
    122 0x57c4c97004a2a65fdb030fc39459dd29c7ed5675      1
    123 0x5816d8339eca3d45ac4e7a9a157cb1bd16862fd7      1
    124 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    125 0x5972a0b205881659aa8cbf287fca3f3bdfd33e36      1
    126 0x5a04ad067b3a022bbc03dd49f105d462b6bbc1ff      1
    127 0x5a8116f07937394e6ac2a51995ab6a054c08bf9e      1
    128 0x5a813532385cf4519de1592f3cda28a3a885fb63      1
    129 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    130 0x5c087d45f975cb190a33a40f130c9d4bbf0a3ae8      1
    131 0x5c721340b4f9d98fdd6fc57ac6fbb4b468cf0b6d      1
    132 0x5ce5a4f93dcbe313b12f521b5cd46b26dc253c6b      1
    133 0x5e7b14fa1bea3c0b4b485beb3a3fb837b0adbd6b      1
    134 0x5f23bf4d66ff4ea4e4c888ea9ef13404fbeb7b49      1
    135 0x5fc33d1ad1a0ecef11eb5cbcd7c30cdf3badbc64      1
    136 0x60eaaa669c70166dc7504a860353161cf525da0b      1
    137 0x61cd60bff6f2fcbfff26f12d71be2b83e0049ef3      1
    138 0x61f9dc3a073f06a5c70cd48c7a85ea4203087c9d      1
    139 0x63bd415fc10ce71af0fdd807d575fb7a42052117      1
    140 0x64d18532f07855920ab624de8d2a8a365f365e5b      1
    141 0x656f6595a0c8a5c4e5d6b193228aacfd7cf55c47      1
    142 0x658884c6be7a46cc14229c7646a57c9e049f4a6a      1
    143 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    144 0x6948108b463d5ee9c1cf6626502accea8fd47b7d      1
    145 0x69b851bd6e7c164ad61020f14fa9914e380f4aa7      1
    146 0x6b88ccacf59e2bd4df9481c0c90d89936c9ad021      1
    147 0x6bb32af5c18d4505c29bb390828c072b7cf47f10      1
    148 0x6c0426ccb743145755049edb54ebeabd1f31670a      1
    149 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
    150 0x6cfc02facaa3db1ee637ba5a30221775f1fce065      1
    151 0x6d24feb9195bb9407ca31002fea7e88d6ad2b6f6      1
    152 0x703daf8e0b424420004190c3082441e52b7ecd4c      1
    153 0x70ff537a22312615a16e1ffef022c6b49b86d0ea      1
    154 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    155 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
    156 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
    157 0x7304689aac83c3b236332b0c233878f1819ca89d      1
    158 0x734b5ce55f0e08ba02d1956eef689d20fbdcd4df      1
    159 0x738eb7a178286ece763f125537f6916dd3f43709      1
    160 0x751c2d719a2ac1161611d8e4afaa9dba50aeecff      1
    161 0x762da606029d3120735aa1eec15464e265db7a3c      1
    162 0x778e3b5ab41f6fa20bb5812fc1ef5929bcbc422a      1
    163 0x77dad28f302ebd245f15480be38037197cc4135d      1
    164 0x78b2f3245050e8cc808a4e8682509583e8280d30      1
    165 0x79137d62f126836f96fed0fe044c7c0023f6cd15      1
    166 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
    167 0x798fb342208c8e094413d7412d10336ba474c905      1
    168 0x79c0aff8061a34798bf79fe85538efbfe0603ce5      1
    169 0x7a832d1984a7b1d53418bee78a50a876285b1b32      1
    170 0x7c5de7cec42945990c0054a1ceca4fbca6ec3dd6      1
    171 0x7d055494701e93173a766beb3ba6fd2f9b923d89      1
    172 0x7e3cdbbc82edd873f1cb9aa0ed45062fc6a5dbf6      1
    173 0x7e76134ae5d8c226a22c9bd889933ed6944ee2e8      1
    174 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    175 0x7edec938f79748539413d638e6cff66dbed0bbe7      1
    176 0x7f4679ed286cb37e5925d6ea4e5f75a1bf39df87      1
    177 0x7f7a01103cdd4be0e594c1e78d2dbd2cf13362d0      1
    178 0x801ac1049467293c3d8564a039c63eb133757e82      1
    179 0x8083b7d599d5a30dc0afa2b7b60515cfd5ca8110      1
    180 0x809e815596abeb3764abf81be2dc39fbbacc9949      1
    181 0x814757b1515bb6e07f66eeb4feb0e922abaddd97      1
    182 0x834c771e04770b1f54fe13d258e5a640d78fba29      1
    183 0x83bde1523a8311482e4f5e59e3170ddaa0445ec2      1
    184 0x840f5d0fa8ac1f000287dfe54c97d4e76b058ef1      1
    185 0x8715dcbf7394a4e6ad98ac5d112e7fb6c3ea9f70      1
    186 0x882530826b5e11a6954bced842f196fb6de59443      1
    187 0x89018aa86f8f535951802126f30496eeecd5b358      1
    188 0x890be1fb49f86938100b687c0bedf6322e530894      1
    189 0x8999020b433a0ac8efdbfe49ef03855aef262eba      1
    190 0x8a8035f056af830b7205c58c1dc037f826fc2b92      1
    191 0x8e244e89b3589f798424d5ed6c6b7397a9aa0688      1
    192 0x8e8d647440fb97f214be7b610ef916be0948375a      1
    193 0x900b24b5651f53820cbd09bf1ac231038f6254ef      1
    194 0x909d7745f61afa75540cc26cdd7032582a1d85b5      1
    195 0x919c8e97a5fc9b5a8d61551c51513bd64545ed07      1
    196 0x91cb1ff4322559a5c5c67dba075ab081c255ba1f      1
    197 0x942dfb0c7e87fb5f07e25ec7ff805e7f973cf929      1
    198 0x951360e606587ab33ebcad952593b2fc99d68c3a      1
    199 0x964fcbe3896a42db2045479724d1994380feeb56      1
    200 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    201 0x978354196b348efbe5c2b89a46ddf13030ff121b      1
    202 0x98a11c376e6e9c1aa5cd28d51227ce6283976596      1
    203 0x98e35ed59bc63fcf01d48bd7620e8dce34e27e7e      1
    204 0x994d0c5e289c9750849b916cd4961e1e7ddb451e      1
    205 0x99581599e8823ee9130380281478e729e926353f      1
    206 0x9980db65004727f5ca68e57c616a3cd0db39b5b4      1
    207 0x99d7be449d280379802588554734fc521f891f39      1
    208 0x99d8d9a3c90b94b8a09c9292615e086b22ef4fd5      1
    209 0x9acd7f6ea20e666ca24406d910de71f83b7f5438      1
    210 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    211 0x9b08042e20dc4e883c41e89813be918d6729099a      1
    212 0x9b0da6499d6ea156c2fbca0f2104d42b7f8f2f88      1
    213 0x9c0031fe523e9f080b51bb18920c7be46d173046      1
    214 0x9c3bd9dc1569379c926a11c4153063fcd152fc07      1
    215 0x9da8b8fe7d972f1c4b4d3824021f9e6abcb51f6d      1
    216 0x9e8c9180c52d4779bdf2b9f728d37bf81e9454e4      1
    217 0x9f0b6b9fcc29037e83c0465c1308f0d0eda2e8f8      1
    218 0xa0ba93e067cbd590e308461ec70993bda100315c      1
    219 0xa1ce3d8e2c7607cc04fbcfd282ab2bf046ce8e98      1
    220 0xa222204acf1be4077d34102fab38a759060b77c2      1
    221 0xa28fd178c686beebd6818d6772e56f6e7a6ff5cc      1
    222 0xa31b444f2cfd111d16d61335247e66bc491c9039      1
    223 0xa33e5e1ccf57c0caf78ae399061f270dd24ffcdf      1
    224 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    225 0xa46664ba7222221475146c6710c812741a6c8bf5      1
    226 0xa530f7739413e787c205233658185edc1e68c25e      1
    227 0xa693fe70a4180c29d0ea522dbd4833dced5fb94a      1
    228 0xa6be6842b0f0ae6e8b3c773f5893b91a01871615      1
    229 0xa7071299be9de85a7ac0c170e7f129856b7adc9f      1
    230 0xa73b66af2ee0c0e2c7c6392ed6352ad0d3b05e33      1
    231 0xa7aa4b444a75f6df5fcae6e563755b14cf573728      1
    232 0xa82b6ad343205d37a5271f55411f02d2d5208b9e      1
    233 0xaad08fea58a5822a076bba2f562f4be5e7db6f41      1
    234 0xaadf5e8b700a15880a2d7f0eb0129955cd3118bc      1
    235 0xabb89dd8d2ecb0b872811a80b9ea8fb9b45562b3      1
    236 0xabd9b21ab5a69e5ba7fb166567211bfc9e602315      1
    237 0xacaf5248d299279b2dbe1b68aa5eb2d74624abc3      1
    238 0xad119a5520aab0664859a847b8117da7aea55948      1
    239 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    240 0xadb1ece7e393132c4a8cfa86a291b039109c4142      1
    241 0xadee5ee18e77ccee0717b707d28d589292b65c9c      1
    242 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    243 0xae78f07e15cfd1fb9fed5b7156708271188ad620      1
    244 0xae88f6a642bcdc56169ef1cc3d2a1b8215935348      1
    245 0xae92ac902fb7937f652dcec459424b122747a86c      1
    246 0xb3201b7aa5818259513e3b0911a643d8e87be2c8      1
    247 0xb81e6bf02ecf2dee200b46b3b25dac34fc7ed54d      1
    248 0xb8b86f77a34d5679daa6df4ae830294c6e108b88      1
    249 0xb955193b05aa57b2f568d110c24ba80dbec7da65      1
    250 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
    251 0xbdc3e5b342b78bae722b86a6d442b92701dc7ddd      1
    252 0xbdddfb2ff21efcc00a2ca5a7d1fdc5798c98f07b      1
    253 0xc00d315682a2173a6e13469027b83f02d86632f1      1
    254 0xc1003fd0f7126333ace9772f6dee41f0e9d21205      1
    255 0xc1a3cc1dbb944139ae91b349fa75e14101b10eef      1
    256 0xc1cf83f7c006cc8f50e62eec470c704f05cd7e29      1
    257 0xc371b423480d0d0bcc7773a68e7a47f50298c7d4      1
    258 0xc41dec3cf3a042a3684be9279d5067ab9d813033      1
    259 0xc7a8ba1b471ef355c897d1c3555c4e940e317133      1
    260 0xc7b57729663ddd90a05af66b42e9d4f71448f099      1
    261 0xc974d92be2ac93fcb434dfe019be0121a5668ef8      1
    262 0xca50cc37abaa58d19e3a23ccb086f17f8384cb3c      1
    263 0xcb3a94b74cc564f97022230a0c42817931653b08      1
    264 0xcbe9be4f72c2992e6fbb7c50639aa944f2ea2cb9      1
    265 0xcbf6a2f2b068dcd6e686b0f90dc3c24fd9739e36      1
    266 0xcc5943ae36253dcef05815eab1bebc6a6b3253cb      1
    267 0xcdec862959d58d31958c15c171c4437ad9e6c711      1
    268 0xce013eb11d874c8fa60ec9007626e154f33ef2c1      1
    269 0xce3d3fbb42d4e960f9cb068c3a65242a269feea5      1
    270 0xced183f3efe63c054f36bc2d701cfe4631e61f1a      1
    271 0xcf0d5e13fb1f015d66b5878857e0274e27932153      1
    272 0xcf8fd84b9074fc741e8b797009284f1361016f6c      1
    273 0xcfc2729173d1ef8d25050c76b84ec5af1fe7bd44      1
    274 0xd03d0b1bebe7ec88b16297f229f7362b7420585c      1
    275 0xd05d20eb7ced9b62ad242c987a3a2bc7e3011b1f      1
    276 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    277 0xd458f62925b3e135680626a0f520098972f93fe9      1
    278 0xd55ca002c8ac7d73b8423696ed55f8f40652914a      1
    279 0xd614f79b1b01d42bed92eb05b58f973be9a3dd3d      1
    280 0xd6eb449e39a260dd2c17d08147482e43ed770e53      1
    281 0xd7539be6aaf5fd4f462fb19296ca3015e6c9cd63      1
    282 0xd764f692dae9747e67f463636120f743a564e346      1
    283 0xd8216dc438796240973e4d8c821bbb58a613205e      1
    284 0xd826aa5f8880f0ace6bb0fc70d4e1220c71f3dee      1
    285 0xd8c2cea4b41479fae6b64519e5b0efaf5fea4ff5      1
    286 0xd9c6c5d2d625f6370ce0da78da7847d3f436fb03      1
    287 0xd9e3a4199b6c1d5ed60150a36ebec51def695ed7      1
    288 0xda47a6b9ec624f9d8cfe1db6423a26cbebc1246d      1
    289 0xdcd50efb4ccdc9c409b4a4d8eb919ed301c86abe      1
    290 0xdf75365217cc3e75e7be6a234153e98efc75e723      1
    291 0xdfe9def041c0a05d3de86d24adaa8f9a31cd0118      1
    292 0xdff6b88d0372d71288103f3ac0a91a211a413794      1
    293 0xe07516eb36d52528eb466b41d5166de05c773dfb      1
    294 0xe14b40c23f0974550e810841ab9cf5b1b378748d      1
    295 0xe1c35e6cbbb0175af3ff0c823a1c6a8ea54b4fda      1
    296 0xe33fc4b9359dd24d18085621767da4ef8e6fd138      1
    297 0xe3cbab0b6e9567c4685b26e3996b2a485aa30695      1
    298 0xe4016ed970cf5e8841b56405556378c628868a3b      1
    299 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    300 0xe895eb79ff949f4ee735c0a98aa131fdd4820ecc      1
    301 0xe911dddbf0db120809ea67e290ecb9fd49135b01      1
    302 0xe95e4c20b70aeeff43469933d46acc2aa3e6acf4      1
    303 0xe985696a7e48496ebd07cec064b2a965c2d3ce1d      1
    304 0xe9f595099f1260eb723a5447bbc53ac9fd11cf4f      1
    305 0xea310c966d3ff5e09c65487f1763b21361eb71ef      1
    306 0xeccebb164c0e70f8ad52d77e992c31871cfdcf77      1
    307 0xef8b82c923ccaea5bc555ac6a5b4d1cddd46d686      1
    308 0xf06bed3f0dad7932d8d00fe48c36751f5c10be23      1
    309 0xf1380d5c5735c2c1195fe43e7f7bf696dc440051      1
    310 0xf159dc86d3989f76e3ee343bb30e672bc081bb88      1
    311 0xf3c1712b56be85c334830e1c68016aeca26dd43a      1
    312 0xf511d47077e4090402426c90d0de5274f0f96b1a      1
    313 0xf59e1cfe7780c2a512be24d49834943318753e92      1
    314 0xf69ae10193aeb88cdeb5f6e4cc92cc350c990b5e      1
    315 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    316 0xf814a8e1607aa1d1a33f42a4c9b3b67eadce45bd      1
    317 0xf84fb8403f2a321f89d8c18d3a9629320263514c      1
    318 0xfa756a92973b1bbbee9df4b2ea3dae2e5891d424      1
    319 0xfa7aa0a1c778a7933a632245445ed11e352355f9      1
    320 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    321 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    322 0xfd067a330af3e78468c36cced4ea8877b3fec7d3      1
    323 0xfdcd7184142909f30713693fe6f7e014b91f7815      1
    324 0xfea49124b05b61337dc0aa7e1af316ef0da24d3a      1
    325 0xfedea86ebec8dde40a2ddd1d156350c62c6697e4      1
    326 0xff13f9b23a16a82c3ccdd29ece6af667eeb15834      1

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
