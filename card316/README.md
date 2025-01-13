
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:428         Length:428         Min.   :1.000   Length:428        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.026                     
                                           3rd Qu.:1.000                     
                                           Max.   :2.000                     
         name          
     Length:428        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21598869 # https://etherscan.io/block/21598869
block_hash <- "0xc0a8998cdc88b4c7d760f7b98300ad6521315a587a13f3a647efb331b038ef0c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4604 

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

allow_artist1    <- pick(snapshot, contracts=c("Foundation","initium","essensesabstractdreams","LostInSpace","WayOutofTheVoid","Essence","SelfDestruction","LostUniverse","TheExplorersSeries"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("revelationeEditions","EquilibriumEditions","DEEDAirdropEditions","OnceSkyFlowsEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("Foundation","initium","essensesabstractdreams","LostInSpace","WayOutofTheVoid","Essence","SelfDestruction","LostUniverse","TheExplorersSeries","revelationeEditions","EquilibriumEditions","DEEDAirdropEditions","OnceSkyFlowsEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 201 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x004c1e5be68337c33deff01e23de067c234bf874      1
      2 0x0057b22d4385adfbc48a6581b141e04acdaf7768      1
      3 0x0078978d493a5a0db7ef37f5cd0b4e89adebfb42      1
      4 0x008275a6ecfcac9321e7930ccc62f3083ec309b0      1
      5 0x02ba0a458fdc070f0c82c4615edee1682c858343      1
      6 0x053d722d9ed18c5c413824b968e9838284f2f343      1
      7 0x06e11a8ff360c5522f3f4a06c6fb28a20a7a0a3f      1
      8 0x089660619d26b6ca62eacd74e2ddd6089024fccc      1
      9 0x0925d347f811d264879271d2905f54309eaccb93      1
     10 0x0a84cdad0447bc13e1acdaf9a3f4265c88f8ad61      1
     11 0x0aaf1a0316606aa46dfa694a6a0028d60b1f2a25      1
     12 0x0cc58b2e44f42fc00fc12d4a9b01fb95fe14891a      1
     13 0x0e7a408a775af29310970de51b59501e21eee87a      1
     14 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     15 0x0eeacd4c475040463389d15ead034d1291b008b1      1
     16 0x0f16a0c8f580cf2f057b4fd23985fd4d0b7787e8      1
     17 0x0f2667933203c932a82f52634cd7f2550072d8db      1
     18 0x10b54d8e8e7ea708e5c71915401261f92e03b376      1
     19 0x1125d53e6b3b8f79f7a40d5ffc67266e707f6175      1
     20 0x1321d53babdd597edeb22e606d51ab4515045c0e      1
     21 0x149b25b5878e26594c183469da57e90d833dd892      1
     22 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     23 0x184e2d53a04bc87a6b597703edcd62d768da1f27      1
     24 0x1a5eddd4cd1c74cf244d4aa330493620a7f2a422      1
     25 0x1b64d74bb19c9d657d63e04e01d040b6abbf2108      1
     26 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
     27 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     28 0x1e4d44f00d42052326fba19ecad73846875b0557      1
     29 0x1ef031c7e5e62753f1035af047469ceafce050ab      1
     30 0x20af8ede68fccdbc1b421bc4abdf1f35d9afe69a      1
     31 0x218f2fd0b94d83056563f5bb658a4cf3e814cb70      1
     32 0x21e10167f7e56419c763c32a070b32705f036acb      1
     33 0x23a1a86f5921a383f9e3172cf4047b41db5d7749      1
     34 0x23a35eb32b713bb1370069a5d440ae6d3ab378d9      1
     35 0x2525f1a08a2d5f037dda969f2fa1b56e4b4b47f3      1
     36 0x258c0f4d65a9e3fba7c33df068c452e276208f38      1
     37 0x25b746daac96c3a096867dbdf9342266620a51b2      1
     38 0x295ff892a2b5941ed26ff8a10fecf90554092719      1
     39 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     40 0x2a41282434f89f5bbf272b1091a7a0cefd22ccd8      1
     41 0x2b14c6978f131c3ec4dd1a4f86c954194069a834      1
     42 0x2bcea5de9c2411fe02b0dc1cdcec71149a682439      1
     43 0x2c474fed08b074fdad8a9977a5fdce69d5a97bf3      1
     44 0x2e9a9dac81b25c127344c13c6e28f0efed9348f4      1
     45 0x2f365e12ac5b2a4ad26fa7990a2f079ad6476b91      1
     46 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     47 0x342dbf118bc1fc72ddcd04bee2862430d3e85410      1
     48 0x36f3523a9d397731b6c8d33282b3f216a2e51d6c      1
     49 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     50 0x39ac46ac2048550a2188d2e5047522c6367ba6d4      1
     51 0x3b0a5f022d0e0b43d0b74f961dc7bd236b919205      1
     52 0x3eb18413d536d7ab03320300598de457fa078ab2      1
     53 0x3fb38cee8d0ba7dcf59403a8c397626dc9c7a13b      1
     54 0x3fb65feeab83bf60b0d1ffbc4217d2d97a35c8d4      1
     55 0x3fca50044fbcafd02022d831e1e16f339ed56000      1
     56 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     57 0x44bf2c5b519190a7be406b7c7233fe6bfcf61a79      1
     58 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     59 0x46873897f487119f34ffde1e6819dbda90cf8acd      1
     60 0x470e23d4f17f19d59f519d4dedff56c7237a4131      1
     61 0x478bb542f7658d635abba67edb987806dff5b83d      1
     62 0x479ff7140dbeba08063abea4fb63fbc7aa27a6d8      1
     63 0x4904a2f99abafc72cd6f270f2a37d3f5c5e92c41      1
     64 0x4ae09616687b6ab324f7db29d3b58840b3ff6334      1
     65 0x4afa06edf41513ba33b3cacf6546741e44b1f548      1
     66 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     67 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     68 0x4eba95c34aec5cfc7306cf2442e6aed09e030fa4      1
     69 0x4f809effb315a91093169e17e6e1bf8f27889240      1
     70 0x50566166c042eaea48d85e99126205235f9c63c1      1
     71 0x51e5d474606db3a8ac712fbc38f5b89dda949e34      1
     72 0x53f34fd3a0105fd0a3e5c37494e0ad1dc10e438b      1
     73 0x570cf51ac81d5871d7fa72fe1f02eb71670dacfa      1
     74 0x57b45f7056f586b9a1d4e0169ff8f6929cb3c303      1
     75 0x5908c672dbbee99b1c3c55e28c4a2c40f361be2a      1
     76 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     77 0x60a363530ec7db4fe74f5ebe62c337fdca8efe0f      1
     78 0x60af14cfe4ca1e36b3aa27e524d911cd57355414      1
     79 0x60de2e12448ca6d6e23810120e8be0639d425703      1
     80 0x6207405ef8e92dbe7ae67a256211d85bcdb63799      1
     81 0x6224fd05d32b5e9ea20d61bfd10a5dcc2db6e76c      1
     82 0x638052d973ce64454ff21e8edf7b2b955788f79a      1
     83 0x65db65fdbc22eb99949c4c6ea4eba19d7d819a51      1
     84 0x667f3f63d522a4cc6e24cf357fca7d7905f3762c      1
     85 0x6860d457124613765a81a57a5853f467abf2d8fd      1
     86 0x69142d6d30fa4131af591620be4b335470d077ca      1
     87 0x694f8a82319873a88f7a9fce5d49664b8d7ec579      1
     88 0x6ab27d9a127c47f3f6c64c6472fe309332453d3a      1
     89 0x6b6fc01183ca5abdb963b7f35ee3838e4a1a213b      1
     90 0x6bf33cda127a81568b41196c38ced5f26f48f143      1
     91 0x6cfe0f1cfc110be97c4d5cc838f26c8f5ca08f17      1
     92 0x6e6a9a9a910edc91fcead64dd73faa4a83a860ab      1
     93 0x6e94662f608a083bdf8a61c26006d10f7471507f      1
     94 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     95 0x73cddf71b29954f393afab3e577370e7a4c5fb4b      1
     96 0x747df17bf5c6241bdab22fbb9514168bf2279fac      1
     97 0x759c51e04dd9062e8d2071febe9d47caea199de5      1
     98 0x766977e1e61a75914cfebabc7554d558185b22ea      1
     99 0x767d7b4ca4e7434b1e821b1aea95e2820a1230b2      1
    100 0x784a8bc7d62bef2169f91b3354cbc41e43cfaf7d      1
    101 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
    102 0x79a94077ac9ff31a9e728a25bf9a549affea1632      1
    103 0x7b83f4aef8377f3f6a54ae6728a8a3eccb3f6b3c      1
    104 0x7c1ccd1f558896ba7265dd0a80884ccf2a6aa5c3      1
    105 0x7cee1eba6121c1f828ef299cd1c3d8dfde12607b      1
    106 0x7d985cedf2a503768049b2017ebbd480755d0174      1
    107 0x7f2e63ff56c5cc7d922e85031504ab7b4a244160      1
    108 0x851d4366904a4eb06f818085a8a9cf2ba867f3a2      1
    109 0x88f9baf87cdcfa1768ea127869894c21adb977bf      1
    110 0x890715b510a4b251cf1777707db0af7e049320c1      1
    111 0x8a5078f5cb70f6246d143287f8044c62a816088f      1
    112 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    113 0x8ac236f975c5b44948c45c662834e28f017c7097      1
    114 0x8f445aa67c45805dd9a99f3a7224cbd3ba5ef569      1
    115 0x8fdbf14ce9d37b2b8cb3b4f7a0e883e71464b377      1
    116 0x9178799399c8f32dbe1ce98d09802953c8a53976      1
    117 0x930252373a6c96c6b61f66deb88d5ec9cac6af16      1
    118 0x9444b8d9cee7807d1e6062990745f2a9eb95e4dc      1
    119 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    120 0x9b67bc898a9892838253373fd43c81aaae836b7f      1
    121 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    122 0x9f5259fe40f34941acd627388dc24d4dcf9b117c      1
    123 0xa097b835cbe0531d005b95930f76f8552ccf9924      1
    124 0xa4ce47b03e577b4407056457aba54fda6dad208b      1
    125 0xa535fbc480d20fc03cdf33fd04c12422f4e63b54      1
    126 0xa91b9d36acb333c4b0fc175bb353498857a94b9d      1
    127 0xa926f594a3c97fc92a095cb38e1cb9aba270babe      1
    128 0xa93755a6a7cb1ccdde8dcea4b81bb34ab1c83a5e      1
    129 0xac707ee01a605bfc712536d703e5e790357c48e5      1
    130 0xac72c7a53882890a231d03a306778e6ff3298499      1
    131 0xac95e9dec056835d88817a8a59a06b1667c515d7      1
    132 0xac9d7336ec3df01596bd3bf20ff53acc66bb4761      1
    133 0xad9039b7ac9bf08b9b2c3afdd6cdec3f26fc9a45      1
    134 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    135 0xb0207ab99be9a94143ceca8834e40ca23c8cd133      1
    136 0xb02313bb524047fd5332d81e1bf301cffea324b3      1
    137 0xb02c3a77a6631bb1598080d77ca596f1f69b3f60      1
    138 0xb0357e4dc38312c88f4af7ea6503672acf6718fd      1
    139 0xb7b44a80b486973775bd401a1fcd1b767954ab92      1
    140 0xb88f79372c05aaccb745e3091c41831e803a6fa5      1
    141 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    142 0xbb1f635ac13a3a6e5b6474e31f7594f165e2a4d1      1
    143 0xbc2ec35d6edf5bde2f2720bc770f8a9599d559e3      1
    144 0xbc401630c1523a27c725ad6582cef2b28ad44525      1
    145 0xbc68c859fc8ea2de63c60f236688dee448c8b70e      1
    146 0xbc867727babd596dbb4e59da26f7575608294ed9      1
    147 0xbe3f5a715409de484845ab0635c1951df0065cb0      1
    148 0xbf1cb5218f89279a93069dc83fb2dfc2cd0f4c6d      1
    149 0xc165346e6d4d5de14a8b19a91dd65daa08d5d74b      1
    150 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    151 0xc26fc403db1d6f1960d2469b7d91713b1e7b3317      1
    152 0xc40d0646bc9f0c4a8972edd785686abae886c91a      1
    153 0xc49786d5546edef7659621ff1746ca6baf6c50a4      1
    154 0xc4c8cf8e9b28a75c0efa77003ac3d6a8e27bcae5      1
    155 0xc55175f8be83d0477713a5b9f64ac4b82438ca5a      1
    156 0xc580950681be90e085734509b8cdcb6b16ad41b3      1
    157 0xc5b89f65b708ba68011f8ef177126760d4322ba6      1
    158 0xc6c2f699aa5e0cee972186d2f1fc5da72983b801      1
    159 0xc86b96f9f895a3d35196af36ed5e6f389988a100      1
    160 0xcb63de5613de3add32a48c6736f34e1c8de5281b      1
    161 0xcba202f6c6684c497c3fb7740a6f6ba14041cde8      1
    162 0xccf9a7325855454ca38815841a8951d9675d501d      1
    163 0xcd486efddbca4d4ac9504f4272a7148376b36e94      1
    164 0xcdc8fbc1cd62335902664e31295c87e47f7453da      1
    165 0xcf10cd8b5dc2323b1eb6de6164647756bad4de4d      1
    166 0xcfcdcb46d037ebdd5ca9c2e4285e5db403544e25      1
    167 0xd1d10682d0fb865372c9197df73f485ee53a74e7      1
    168 0xd383c147e8226d91372601b2cd277052ebe9cc03      1
    169 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    170 0xd532962fd7976880fdff92db9cbe48a7369b1fc0      1
    171 0xd572c1b3ca511dfe88c3c759b5d189831b972b2f      1
    172 0xd6d3346b882993fc4c6a1d65b10b6ee357f79743      1
    173 0xd7a3d41a0af242c89811f4f725497f34897a38e8      1
    174 0xd91dc7c83bd01b91cb25019dfc4e35bc6faab814      1
    175 0xda15b2fd4eb7967039c7e46e69de77fee2e235df      1
    176 0xddf1992063449a46d8661f6177a97366fb9fb128      1
    177 0xdf157b7bc919d559f58756e395079cb4f8bbf826      1
    178 0xdfc8a3e6fe8251e05c484f302858d36b1bed3e38      1
    179 0xe06fb71ee85029d641cd30c6291e9e7be20d5842      1
    180 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    181 0xe21236307f8a1c4c97e05e62373791f6d2911fe7      1
    182 0xe4b77842e0d992f6358f0742f8c2cbe955b8839e      1
    183 0xe730885fa59a15accf02c9440cbe6e020cac5ddf      1
    184 0xe8170bc2483571d06aadf73ed5f90e0719c6afa4      1
    185 0xe8dc123f0acd763d2f28e97ce6d5a0994a5aea61      1
    186 0xe94f5002581b2e841f35ee810d6f2b8bbc986210      1
    187 0xe9581d138c9d625b851164682352fdb17691ebd7      1
    188 0xe9f3ad11d9901d82f4d05ec70ca59a0e21f56d37      1
    189 0xea6d01fa5c7c5de50c4887f121354d15fc414d27      1
    190 0xecd7a7c7b3ca6037f431693ebdf22e0c074c76a8      1
    191 0xee664d1b64927199bd0eed43293463a41f2883ca      1
    192 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    193 0xf2b506a799159ac4add1ad7182d1635b6cf5dc24      1
    194 0xf45fb9b9ce41ceba30c8223c8a8604c5e9fe9f54      1
    195 0xf4835d15ec30bd62ceb8dd2c5307265bdefada16      1
    196 0xf817a1e2b11fb30abad06a828940a49b5bd052d4      1
    197 0xf8764e85e8cd731466dfa21572711c908cc5f617      1
    198 0xf9add84e8476754273d8850f047497538ae07941      1
    199 0xfbce445f05f3d94a7e9fa6df66b57e08e19d407c      1
    200 0xfe15dc236461743b2557daaf252fcb783b12ec74      1
    201 0xfe4ca509568e82af9463775466124b52c64d7988      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 56 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x046cd19a339761c8f62ffa5c42023bc5e07e7c5c      1
     2 0x09c4eea8ad5e12e1979a9edda897c4cac13baed9      1
     3 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     4 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     5 0x17cbd516166720b8c75fe75235ed0218d9cfdfbe      1
     6 0x1b63e884871aff9a6a55fdd30dbcb82d647d5f99      1
     7 0x1e69075cb9e0239a1b916566e38bd5b854840746      1
     8 0x236446b98087bc8ac9cfae6c176189b4a3139acd      1
     9 0x2471c70645cf0a8f38e6c07a98355b7779fcd13e      1
    10 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
    11 0x2b84bd501b5487e78ac82307d7677ceed119216a      1
    12 0x2efd420c2a39e0eea94e8a680cd761618fabafd2      1
    13 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
    14 0x34433cb54d406d231cd511567f33ce1914fc888e      1
    15 0x369712ace28dd9d7ac6276cd8bf208a7c51e2937      1
    16 0x3ecc3b8ef2b8427cd84720455068a502ba2fb750      1
    17 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
    18 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
    19 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
    20 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
    21 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
    22 0x547e9acab1262b9ddd802b5faf3397a87f2cdeb3      1
    23 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
    24 0x59d04f01411b879f9416073edb3382854edd4e08      1
    25 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    26 0x5dbe2b1f8e3946dc425e472e15b891348cb93a8b      1
    27 0x5e99e182c0fa14408169a20368c185b704a8f605      1
    28 0x5f0bdfda59ebfc57521c07ca90e14936c35947c9      1
    29 0x6c167ae3f9247ccfbe9b9bf3c1b014612ca680a5      1
    30 0x780701eda98d4437967881553efe18da3be04249      1
    31 0x7f11f726cc5c0b6d6230b1d4f0db626214199503      1
    32 0x82cda29197a03f5024848c3759433f88aeffd876      1
    33 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    34 0x94d7fa78a5477f91c8bb1f321846a2b7902c7c64      1
    35 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    36 0xa73769aed346319287410811639ac3bec8464d55      1
    37 0xa87a32d6472412505f347801eded900b1f896cd3      1
    38 0xadd922c4966514fb6c10405441fe690ece5f804f      1
    39 0xb0bcfde547e15c48fefeed2cc021f030de4f317e      1
    40 0xb4acab4cc728b35b0af212b7371704445957eab1      1
    41 0xb4c397c9c012b55c3aec534be1b1c08440467301      1
    42 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    43 0xc1f74ddd2570d8970082489f35b7a114b12cdfd2      1
    44 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    45 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    46 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    47 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
    48 0xd924e899fb21a87379eda515f75e85ce6899c242      1
    49 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
    50 0xe05ab36d8e6e5969968df13508d35aabedf75b77      1
    51 0xee193f18cc3549e1158e4b940f065d8f460c143e      1
    52 0xeff7643ffc84afbb2f6851b70eb884908d264cea      1
    53 0xf0b1853debc24de72b4ac72adf1fd5d9327fb737      1
    54 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    55 0xf2439241881964006369c0e2377d45f3740f48a0      1
    56 0xf56d09a54c27e8aefe9e1b89759ffbbe465fdcf5      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 257 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x004c1e5be68337c33deff01e23de067c234bf874      1
      2 0x0057b22d4385adfbc48a6581b141e04acdaf7768      1
      3 0x0078978d493a5a0db7ef37f5cd0b4e89adebfb42      1
      4 0x008275a6ecfcac9321e7930ccc62f3083ec309b0      1
      5 0x02ba0a458fdc070f0c82c4615edee1682c858343      1
      6 0x046cd19a339761c8f62ffa5c42023bc5e07e7c5c      1
      7 0x053d722d9ed18c5c413824b968e9838284f2f343      1
      8 0x06e11a8ff360c5522f3f4a06c6fb28a20a7a0a3f      1
      9 0x089660619d26b6ca62eacd74e2ddd6089024fccc      1
     10 0x0925d347f811d264879271d2905f54309eaccb93      1
     11 0x09c4eea8ad5e12e1979a9edda897c4cac13baed9      1
     12 0x0a84cdad0447bc13e1acdaf9a3f4265c88f8ad61      1
     13 0x0aaf1a0316606aa46dfa694a6a0028d60b1f2a25      1
     14 0x0b75630a69e27fd44b06748cd6b5f59e52994994      1
     15 0x0cc58b2e44f42fc00fc12d4a9b01fb95fe14891a      1
     16 0x0e7a408a775af29310970de51b59501e21eee87a      1
     17 0x0e8d35bb16acc48144253d703eb19358f739aaae      1
     18 0x0eeacd4c475040463389d15ead034d1291b008b1      1
     19 0x0f16a0c8f580cf2f057b4fd23985fd4d0b7787e8      1
     20 0x0f2667933203c932a82f52634cd7f2550072d8db      1
     21 0x10b54d8e8e7ea708e5c71915401261f92e03b376      1
     22 0x1125d53e6b3b8f79f7a40d5ffc67266e707f6175      1
     23 0x1321d53babdd597edeb22e606d51ab4515045c0e      1
     24 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     25 0x149b25b5878e26594c183469da57e90d833dd892      1
     26 0x17cbd516166720b8c75fe75235ed0218d9cfdfbe      1
     27 0x183abe67478eb7e87c96ca28e2f63dec53f22e3a      1
     28 0x184e2d53a04bc87a6b597703edcd62d768da1f27      1
     29 0x1a5eddd4cd1c74cf244d4aa330493620a7f2a422      1
     30 0x1b63e884871aff9a6a55fdd30dbcb82d647d5f99      1
     31 0x1b64d74bb19c9d657d63e04e01d040b6abbf2108      1
     32 0x1b7f00ca9d8fea5a9853f18ad247f806a8dce57a      1
     33 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     34 0x1e4d44f00d42052326fba19ecad73846875b0557      1
     35 0x1e69075cb9e0239a1b916566e38bd5b854840746      1
     36 0x1ef031c7e5e62753f1035af047469ceafce050ab      1
     37 0x20af8ede68fccdbc1b421bc4abdf1f35d9afe69a      1
     38 0x218f2fd0b94d83056563f5bb658a4cf3e814cb70      1
     39 0x21e10167f7e56419c763c32a070b32705f036acb      1
     40 0x236446b98087bc8ac9cfae6c176189b4a3139acd      1
     41 0x23a1a86f5921a383f9e3172cf4047b41db5d7749      1
     42 0x23a35eb32b713bb1370069a5d440ae6d3ab378d9      1
     43 0x2471c70645cf0a8f38e6c07a98355b7779fcd13e      1
     44 0x2525f1a08a2d5f037dda969f2fa1b56e4b4b47f3      1
     45 0x258c0f4d65a9e3fba7c33df068c452e276208f38      1
     46 0x25b746daac96c3a096867dbdf9342266620a51b2      1
     47 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     48 0x295ff892a2b5941ed26ff8a10fecf90554092719      1
     49 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     50 0x2a41282434f89f5bbf272b1091a7a0cefd22ccd8      1
     51 0x2b14c6978f131c3ec4dd1a4f86c954194069a834      1
     52 0x2b84bd501b5487e78ac82307d7677ceed119216a      1
     53 0x2bcea5de9c2411fe02b0dc1cdcec71149a682439      1
     54 0x2c474fed08b074fdad8a9977a5fdce69d5a97bf3      1
     55 0x2e9a9dac81b25c127344c13c6e28f0efed9348f4      1
     56 0x2efd420c2a39e0eea94e8a680cd761618fabafd2      1
     57 0x2f365e12ac5b2a4ad26fa7990a2f079ad6476b91      1
     58 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     59 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
     60 0x342dbf118bc1fc72ddcd04bee2862430d3e85410      1
     61 0x34433cb54d406d231cd511567f33ce1914fc888e      1
     62 0x369712ace28dd9d7ac6276cd8bf208a7c51e2937      1
     63 0x36f3523a9d397731b6c8d33282b3f216a2e51d6c      1
     64 0x385fcbd4fdaa1ae35263141ba39b90f305016af4      1
     65 0x39ac46ac2048550a2188d2e5047522c6367ba6d4      1
     66 0x3b0a5f022d0e0b43d0b74f961dc7bd236b919205      1
     67 0x3eb18413d536d7ab03320300598de457fa078ab2      1
     68 0x3ecc3b8ef2b8427cd84720455068a502ba2fb750      1
     69 0x3fb38cee8d0ba7dcf59403a8c397626dc9c7a13b      1
     70 0x3fb65feeab83bf60b0d1ffbc4217d2d97a35c8d4      1
     71 0x3fca50044fbcafd02022d831e1e16f339ed56000      1
     72 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     73 0x44bf2c5b519190a7be406b7c7233fe6bfcf61a79      1
     74 0x44f829ccbdc9c15ca84d4fd3f00c4682c35f228e      1
     75 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
     76 0x46873897f487119f34ffde1e6819dbda90cf8acd      1
     77 0x470e23d4f17f19d59f519d4dedff56c7237a4131      1
     78 0x478bb542f7658d635abba67edb987806dff5b83d      1
     79 0x479ff7140dbeba08063abea4fb63fbc7aa27a6d8      1
     80 0x4904a2f99abafc72cd6f270f2a37d3f5c5e92c41      1
     81 0x4a5cf3741aec26e8b70dd82049140ecb1a02d859      1
     82 0x4ae09616687b6ab324f7db29d3b58840b3ff6334      1
     83 0x4afa06edf41513ba33b3cacf6546741e44b1f548      1
     84 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     85 0x4c2f526cdaaeea289fb02fce54ac5cf9ffb5229b      1
     86 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
     87 0x4df9c44e40d1bea726002fd6d45962908beebd28      1
     88 0x4eba95c34aec5cfc7306cf2442e6aed09e030fa4      1
     89 0x4f4538e2553c61bce747f73c5fe8133d4b383dda      1
     90 0x4f809effb315a91093169e17e6e1bf8f27889240      1
     91 0x50566166c042eaea48d85e99126205235f9c63c1      1
     92 0x51e5d474606db3a8ac712fbc38f5b89dda949e34      1
     93 0x53f34fd3a0105fd0a3e5c37494e0ad1dc10e438b      1
     94 0x547e9acab1262b9ddd802b5faf3397a87f2cdeb3      1
     95 0x570cf51ac81d5871d7fa72fe1f02eb71670dacfa      1
     96 0x57b45f7056f586b9a1d4e0169ff8f6929cb3c303      1
     97 0x5908c672dbbee99b1c3c55e28c4a2c40f361be2a      1
     98 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     99 0x59d04f01411b879f9416073edb3382854edd4e08      1
    100 0x5bf8930a345dea4a1bea0ce8b876f7d29cd24787      1
    101 0x5dbe2b1f8e3946dc425e472e15b891348cb93a8b      1
    102 0x5e99e182c0fa14408169a20368c185b704a8f605      1
    103 0x5f0bdfda59ebfc57521c07ca90e14936c35947c9      1
    104 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    105 0x60a363530ec7db4fe74f5ebe62c337fdca8efe0f      1
    106 0x60af14cfe4ca1e36b3aa27e524d911cd57355414      1
    107 0x60de2e12448ca6d6e23810120e8be0639d425703      1
    108 0x6207405ef8e92dbe7ae67a256211d85bcdb63799      1
    109 0x6224fd05d32b5e9ea20d61bfd10a5dcc2db6e76c      1
    110 0x638052d973ce64454ff21e8edf7b2b955788f79a      1
    111 0x65db65fdbc22eb99949c4c6ea4eba19d7d819a51      1
    112 0x667f3f63d522a4cc6e24cf357fca7d7905f3762c      1
    113 0x6860d457124613765a81a57a5853f467abf2d8fd      1
    114 0x69142d6d30fa4131af591620be4b335470d077ca      1
    115 0x694f8a82319873a88f7a9fce5d49664b8d7ec579      1
    116 0x6ab27d9a127c47f3f6c64c6472fe309332453d3a      1
    117 0x6b6fc01183ca5abdb963b7f35ee3838e4a1a213b      1
    118 0x6bf33cda127a81568b41196c38ced5f26f48f143      1
    119 0x6c167ae3f9247ccfbe9b9bf3c1b014612ca680a5      1
    120 0x6cfe0f1cfc110be97c4d5cc838f26c8f5ca08f17      1
    121 0x6e6a9a9a910edc91fcead64dd73faa4a83a860ab      1
    122 0x6e94662f608a083bdf8a61c26006d10f7471507f      1
    123 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
    124 0x73cddf71b29954f393afab3e577370e7a4c5fb4b      1
    125 0x747df17bf5c6241bdab22fbb9514168bf2279fac      1
    126 0x759c51e04dd9062e8d2071febe9d47caea199de5      1
    127 0x766977e1e61a75914cfebabc7554d558185b22ea      1
    128 0x767d7b4ca4e7434b1e821b1aea95e2820a1230b2      1
    129 0x780701eda98d4437967881553efe18da3be04249      1
    130 0x784a8bc7d62bef2169f91b3354cbc41e43cfaf7d      1
    131 0x791fad8329a0a56e2ce98380d6d6fda3302337da      1
    132 0x79a94077ac9ff31a9e728a25bf9a549affea1632      1
    133 0x7b83f4aef8377f3f6a54ae6728a8a3eccb3f6b3c      1
    134 0x7c1ccd1f558896ba7265dd0a80884ccf2a6aa5c3      1
    135 0x7cee1eba6121c1f828ef299cd1c3d8dfde12607b      1
    136 0x7d985cedf2a503768049b2017ebbd480755d0174      1
    137 0x7f11f726cc5c0b6d6230b1d4f0db626214199503      1
    138 0x7f2e63ff56c5cc7d922e85031504ab7b4a244160      1
    139 0x82cda29197a03f5024848c3759433f88aeffd876      1
    140 0x832408210f74c5cd8ac6fd8ebc92bb6b8a7834a0      1
    141 0x851d4366904a4eb06f818085a8a9cf2ba867f3a2      1
    142 0x88f9baf87cdcfa1768ea127869894c21adb977bf      1
    143 0x890715b510a4b251cf1777707db0af7e049320c1      1
    144 0x8a5078f5cb70f6246d143287f8044c62a816088f      1
    145 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    146 0x8ac236f975c5b44948c45c662834e28f017c7097      1
    147 0x8f445aa67c45805dd9a99f3a7224cbd3ba5ef569      1
    148 0x8fdbf14ce9d37b2b8cb3b4f7a0e883e71464b377      1
    149 0x9178799399c8f32dbe1ce98d09802953c8a53976      1
    150 0x930252373a6c96c6b61f66deb88d5ec9cac6af16      1
    151 0x9444b8d9cee7807d1e6062990745f2a9eb95e4dc      1
    152 0x94d7fa78a5477f91c8bb1f321846a2b7902c7c64      1
    153 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    154 0x9b67bc898a9892838253373fd43c81aaae836b7f      1
    155 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    156 0x9f5259fe40f34941acd627388dc24d4dcf9b117c      1
    157 0xa097b835cbe0531d005b95930f76f8552ccf9924      1
    158 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    159 0xa4ce47b03e577b4407056457aba54fda6dad208b      1
    160 0xa535fbc480d20fc03cdf33fd04c12422f4e63b54      1
    161 0xa73769aed346319287410811639ac3bec8464d55      1
    162 0xa87a32d6472412505f347801eded900b1f896cd3      1
    163 0xa91b9d36acb333c4b0fc175bb353498857a94b9d      1
    164 0xa926f594a3c97fc92a095cb38e1cb9aba270babe      1
    165 0xa93755a6a7cb1ccdde8dcea4b81bb34ab1c83a5e      1
    166 0xac707ee01a605bfc712536d703e5e790357c48e5      1
    167 0xac72c7a53882890a231d03a306778e6ff3298499      1
    168 0xac95e9dec056835d88817a8a59a06b1667c515d7      1
    169 0xac9d7336ec3df01596bd3bf20ff53acc66bb4761      1
    170 0xad9039b7ac9bf08b9b2c3afdd6cdec3f26fc9a45      1
    171 0xadd922c4966514fb6c10405441fe690ece5f804f      1
    172 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    173 0xb0207ab99be9a94143ceca8834e40ca23c8cd133      1
    174 0xb02313bb524047fd5332d81e1bf301cffea324b3      1
    175 0xb02c3a77a6631bb1598080d77ca596f1f69b3f60      1
    176 0xb0357e4dc38312c88f4af7ea6503672acf6718fd      1
    177 0xb0bcfde547e15c48fefeed2cc021f030de4f317e      1
    178 0xb4acab4cc728b35b0af212b7371704445957eab1      1
    179 0xb4c397c9c012b55c3aec534be1b1c08440467301      1
    180 0xb7b44a80b486973775bd401a1fcd1b767954ab92      1
    181 0xb88f79372c05aaccb745e3091c41831e803a6fa5      1
    182 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    183 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    184 0xbb1f635ac13a3a6e5b6474e31f7594f165e2a4d1      1
    185 0xbc2ec35d6edf5bde2f2720bc770f8a9599d559e3      1
    186 0xbc401630c1523a27c725ad6582cef2b28ad44525      1
    187 0xbc68c859fc8ea2de63c60f236688dee448c8b70e      1
    188 0xbc867727babd596dbb4e59da26f7575608294ed9      1
    189 0xbe3f5a715409de484845ab0635c1951df0065cb0      1
    190 0xbf1cb5218f89279a93069dc83fb2dfc2cd0f4c6d      1
    191 0xc165346e6d4d5de14a8b19a91dd65daa08d5d74b      1
    192 0xc1f74ddd2570d8970082489f35b7a114b12cdfd2      1
    193 0xc2065e8c845448b6fd7a3ad168918f2ba6e9d45f      1
    194 0xc26fc403db1d6f1960d2469b7d91713b1e7b3317      1
    195 0xc40d0646bc9f0c4a8972edd785686abae886c91a      1
    196 0xc49786d5546edef7659621ff1746ca6baf6c50a4      1
    197 0xc4c8cf8e9b28a75c0efa77003ac3d6a8e27bcae5      1
    198 0xc55175f8be83d0477713a5b9f64ac4b82438ca5a      1
    199 0xc580950681be90e085734509b8cdcb6b16ad41b3      1
    200 0xc5b89f65b708ba68011f8ef177126760d4322ba6      1
    201 0xc6c2f699aa5e0cee972186d2f1fc5da72983b801      1
    202 0xc86b96f9f895a3d35196af36ed5e6f389988a100      1
    203 0xcb37f1fe07988e6ff6c21b28b986fed13ebfa549      1
    204 0xcb63de5613de3add32a48c6736f34e1c8de5281b      1
    205 0xcba202f6c6684c497c3fb7740a6f6ba14041cde8      1
    206 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    207 0xccf9a7325855454ca38815841a8951d9675d501d      1
    208 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    209 0xcd3811f1900a0ff9c0c1dbc846ef65e9861b260d      1
    210 0xcd486efddbca4d4ac9504f4272a7148376b36e94      1
    211 0xcdc8fbc1cd62335902664e31295c87e47f7453da      1
    212 0xcf10cd8b5dc2323b1eb6de6164647756bad4de4d      1
    213 0xcfcdcb46d037ebdd5ca9c2e4285e5db403544e25      1
    214 0xd1d10682d0fb865372c9197df73f485ee53a74e7      1
    215 0xd383c147e8226d91372601b2cd277052ebe9cc03      1
    216 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    217 0xd532962fd7976880fdff92db9cbe48a7369b1fc0      1
    218 0xd572c1b3ca511dfe88c3c759b5d189831b972b2f      1
    219 0xd6d3346b882993fc4c6a1d65b10b6ee357f79743      1
    220 0xd7a3d41a0af242c89811f4f725497f34897a38e8      1
    221 0xd91dc7c83bd01b91cb25019dfc4e35bc6faab814      1
    222 0xd924e899fb21a87379eda515f75e85ce6899c242      1
    223 0xda15b2fd4eb7967039c7e46e69de77fee2e235df      1
    224 0xdc7617d4224fcd303d1c9c1504dcdca67cf06be2      1
    225 0xddf1992063449a46d8661f6177a97366fb9fb128      1
    226 0xdf157b7bc919d559f58756e395079cb4f8bbf826      1
    227 0xdfc8a3e6fe8251e05c484f302858d36b1bed3e38      1
    228 0xe05ab36d8e6e5969968df13508d35aabedf75b77      1
    229 0xe06fb71ee85029d641cd30c6291e9e7be20d5842      1
    230 0xe14397613d3c700d3abb63d15ff481d09841ad8f      1
    231 0xe21236307f8a1c4c97e05e62373791f6d2911fe7      1
    232 0xe4b77842e0d992f6358f0742f8c2cbe955b8839e      1
    233 0xe730885fa59a15accf02c9440cbe6e020cac5ddf      1
    234 0xe8170bc2483571d06aadf73ed5f90e0719c6afa4      1
    235 0xe8dc123f0acd763d2f28e97ce6d5a0994a5aea61      1
    236 0xe94f5002581b2e841f35ee810d6f2b8bbc986210      1
    237 0xe9581d138c9d625b851164682352fdb17691ebd7      1
    238 0xe9f3ad11d9901d82f4d05ec70ca59a0e21f56d37      1
    239 0xea6d01fa5c7c5de50c4887f121354d15fc414d27      1
    240 0xecd7a7c7b3ca6037f431693ebdf22e0c074c76a8      1
    241 0xee193f18cc3549e1158e4b940f065d8f460c143e      1
    242 0xee664d1b64927199bd0eed43293463a41f2883ca      1
    243 0xeff7643ffc84afbb2f6851b70eb884908d264cea      1
    244 0xf0b1853debc24de72b4ac72adf1fd5d9327fb737      1
    245 0xf18d8f1cfa7a24ed602128a46dcd9927dae6c05b      1
    246 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    247 0xf2439241881964006369c0e2377d45f3740f48a0      1
    248 0xf2b506a799159ac4add1ad7182d1635b6cf5dc24      1
    249 0xf45fb9b9ce41ceba30c8223c8a8604c5e9fe9f54      1
    250 0xf4835d15ec30bd62ceb8dd2c5307265bdefada16      1
    251 0xf56d09a54c27e8aefe9e1b89759ffbbe465fdcf5      1
    252 0xf817a1e2b11fb30abad06a828940a49b5bd052d4      1
    253 0xf8764e85e8cd731466dfa21572711c908cc5f617      1
    254 0xf9add84e8476754273d8850f047497538ae07941      1
    255 0xfbce445f05f3d94a7e9fa6df66b57e08e19d407c      1
    256 0xfe15dc236461743b2557daaf252fcb783b12ec74      1
    257 0xfe4ca509568e82af9463775466124b52c64d7988      1

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
