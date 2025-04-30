
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:491         Length:491         Min.   :1.000   Length:491        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.122                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:491        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 22346969 # https://etherscan.io/block/22346969
block_hash <- "0x6a16fd9f401632a2474131644cb16c6a0f246edc8bd591c4c32cfba804964abb"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4628 

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

allow_artist1    <- pick(snapshot, contracts=c("DeGenReMemesby9256"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("TheMemesby9256Editions","BTC"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("DeGenReMemesby9256","TheMemesby9256Editions","BTC"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 5 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x230512379ced76d35500b03b6892a31b17de1248      1
    2 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    3 0x538eb86b52e480ec0ff1fa87c77c1f36e3f04a0a      1
    4 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    5 0xb8ed7d09aeede2e7e9abd156c2b93defd32473cb      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 231 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0206b6213a7926c406968999e9bd97495c13fcb9      1
      2 0x02cfd5fd2898eba09af848950ff7a542c75de3d9      1
      3 0x0311c9df09c28978bbea44a606644cde11055fd4      1
      4 0x0314cc74e461d881ba3b5c93bba34cc81e4f7004      1
      5 0x032261cb5868414d70251a4d609e196838551457      1
      6 0x03e81230edb43a393d33b4e6724f31b955327f0c      1
      7 0x0519b923ee8159bee24fffd82bfaaf7e0024cc7b      1
      8 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
      9 0x07be036f5baeda1115b1132e2412900d85985510      1
     10 0x0836321e4b7c75f85472a451b3e04e5adb0e6177      1
     11 0x09c52c99e701304332b5998227f07d2648e8a72c      1
     12 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
     13 0x0a33896f09d43a5c9af1a14670c2869b945fe39f      1
     14 0x0a96d5dc037ea14ca76355460adf41c279877b9a      1
     15 0x0ce98350b9dea81fe0b47dbc4787a1e9f8f1d7ff      1
     16 0x0f10e80711e5b6b87b4c00d6acb6856be091e98f      1
     17 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     18 0x128a4adf4f86a4e32447ca1f1828acf9707c6cb9      1
     19 0x13267f77e7241db36f9ee3418dd3069bbc91f2b2      1
     20 0x13d735a4d5e966b8f7b19fc2f476bfc25c0fc7dc      1
     21 0x1508622685a8d18f514612e6be4f44b691826c4a      1
     22 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     23 0x16a55d6c1d1991dfe63af7a0b8f19749791c9f52      1
     24 0x170fb8f8f37776dc48184686dec386d2d1c41cc9      1
     25 0x18d4abe1c93ff40f957124eafc1f632a9318bd8b      1
     26 0x19d4ccaedd75ef32ba76430af03e27cee010606a      1
     27 0x1a275b0bc79469dbb438aa33694f1b7348010d56      1
     28 0x1b21a4287cbfa8f81753de2a17028bf2fa231034      1
     29 0x1f8a8ac54244e4016a376152b2f92d467552fa7b      1
     30 0x20e2cb51f9a1788b3840d94aee6ff3299d3fde20      1
     31 0x21a06e63ee6dca27b49b87555b4e39a553ffa7ce      1
     32 0x22757fe151ee28e8d1492971ff8d70e3c1d3df9a      1
     33 0x2286f534a7821a952592fd1d4c78ffc453f64870      1
     34 0x22b74259bfa6c1ec630c99e108fe7d532df04751      1
     35 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     36 0x2360f0f7e8f29231d844faed5de568e491011504      1
     37 0x24fc4565ee0be682139289ea669cf97d86b75845      1
     38 0x2638c0c7bb8b6cc7af4f90a7ab01df9398e46893      1
     39 0x264ca3862bdd88505839033bf684e3a217432d4b      1
     40 0x269cf21103a3d9a0a2a0849a056f7e291addffa4      1
     41 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     42 0x282d656a9d95c64522f6bc1a42ee759de81e8dc0      1
     43 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     44 0x2a42cf4e1cfc33850fd8d8da24f86cd89b82ad95      1
     45 0x2afbc4356bf863a029ef58875ad00cf6b8975a52      1
     46 0x2c9b7c9b5e9ea9a7d16485f454f5ff0662d76e25      1
     47 0x2cdf6562aecd383aa45270f22b35920913f83ea1      1
     48 0x2ff08c037996d75d8680064241186768cf4e1fab      1
     49 0x33d8ae1cd83595ffb4104a6571290235ba33c8ed      1
     50 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
     51 0x36e18ab9da425d5b59de9176f19d867f8eb30b25      1
     52 0x38d0055e0de80b103263d8cfcb37bc1a2593f9e6      1
     53 0x3a3c2918deab486f311a82fc67798a2767b58209      1
     54 0x3b99969c74b3b991686ac41dd4acd2a283f1c066      1
     55 0x3ceae39c964eace130886325e78ded552f415124      1
     56 0x3ded646035e0ac9a4eeab15496b8fc008bcf4a49      1
     57 0x3f63c576a04570dcbb05be4534dee0a7c3df5188      1
     58 0x4076eca4db9684fa1d9bfac231cb516889a33e5a      1
     59 0x40fbe29010ca2e25357202a8ae3474afd498166d      1
     60 0x415238cd0891f38591aa7cb48c8ed451acf54296      1
     61 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     62 0x4501aeb9672232c02fa717c8d69f29ec0db80a3b      1
     63 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     64 0x4638eed954ce851515cf8a1462dbcc69ca332ae5      1
     65 0x4652b1ae2a9d0050dfa73d2b4c87d9b3ea175eaa      1
     66 0x4735117ac47cd53338f742174bf2d7c5a6667ea1      1
     67 0x4dc208f337fd51bb6a35925cb4a6b0ed168d3460      1
     68 0x4e51a06e4643197e1be2f050b6af60f9ad3cef06      1
     69 0x52f051280e850f32cbfafd8d78c31edec4c3248c      1
     70 0x557c60995797fa7b47be105227a2e46148d85750      1
     71 0x57cbb6d42975eafbab88b269cb7249e05f425449      1
     72 0x58f8f8a6c81cec8b2fd3a3999b4ed0a0b2862a6d      1
     73 0x5949d14a911c196cc24bff3c63ba65a9e5170ddd      1
     74 0x5c7f3e12a7c86d54e115affc0162124589e4497a      1
     75 0x5e439b06536ce2a9f17b226b94a13b438ab971fb      1
     76 0x6264e10f53bfa97aa0b966f01e63cf60636a9c2d      1
     77 0x636958c636886b728ab9e190a8a77397b97fefc3      1
     78 0x648174333697082875c3187c6c2571d562b422c5      1
     79 0x64dd2d8ca8016d0af6095115f5cf677d950e7935      1
     80 0x66af9e0005166fd3631a5beabb6c4eddf3a65312      1
     81 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     82 0x67243d6c3c3bdc2f59d2f74ba1949a02973a529d      1
     83 0x6a29cafc9dae22b131d619716ffa1ef6fd4bec8f      1
     84 0x6abce6d8f90cd034e6a1efa853c61ced6941aa7d      1
     85 0x6e3e9b841f2c0e8a85596cb3d413779479cc724c      1
     86 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     87 0x71eece7c9a6e68bed5ca071046cdccec20ff9808      1
     88 0x7284a6feb4ccbe66f8e52137b506085865ac8dfe      1
     89 0x736fbf35964693f9325417ff281d1a3c9044b213      1
     90 0x7499003740393bec0ece8745e099c2eb3f59dc9d      1
     91 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
     92 0x75ccf763b78ea6174604fca946465bea177f5e23      1
     93 0x776e9767a68ab12ce43bf1c7cb7fcd72f7ef7d30      1
     94 0x78297a87dba724a4cad36cf102f8b770bc032a35      1
     95 0x7842cf4a8668d4fa74eb033878a3b5b8ae52f86c      1
     96 0x79b6a2060427b0d34da346fb722766e7cc6f1371      1
     97 0x7a3a6fe2bde4ae601804bbf951912e51c82060c4      1
     98 0x7a71d126254d6ef6942e0b3d513dd5eb09d35c28      1
     99 0x7abdd18d37571e1ba884a8fa9e07a3612b584661      1
    100 0x7b5af6790381f932abae790e8b0d0ff50e287f8e      1
    101 0x7c862eac48b66d1222cc0be6bdae63f0b24f0fff      1
    102 0x7c9b8e872c0c67e33265cacec5bf4dc2c58ef501      1
    103 0x7d92a98884a726b005234ffa3dffe018b8ce804c      1
    104 0x7f02d0944832ae644faf30cd17f5e1340cabaf3a      1
    105 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
    106 0x7fe329a60f5b86f8bc9fa2ecfebbd7596e5964ed      1
    107 0x813aff0d9f488fdc1aa48f03fbd9967aadb4410f      1
    108 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    109 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    110 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    111 0x833306375573df6e02ba859581dc474e58bda43e      1
    112 0x847f35ab961629fb2c8fe98c6ed5b8c34a2e5ae6      1
    113 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
    114 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
    115 0x89b21aef1716c5000471006fd31ebc8c426f1322      1
    116 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    117 0x8a4ae95f8c670f8bafcea5a05d9808c4af43f5f9      1
    118 0x8a507a95451dc47eb6c46ff21b58e94c95e357ce      1
    119 0x8b1de9760c745098af64a4327b4c9d113ed424e4      1
    120 0x8be5a2df6488b0937299d27f092716f4a9ab2fd8      1
    121 0x8ca88bcd7cd3e43c2f39592428565a2fa0545e72      1
    122 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    123 0x8e22df65a6cebf6709e1ca7f44c544a1c2fb5bad      1
    124 0x8f72a6934ce350b19432a3dcb1c698d6f7b49fb6      1
    125 0x946ecb45a0a6681c87abc6dc8f6471a7ab75337b      1
    126 0x966133652465b15365a91a8b1f6c95276838300d      1
    127 0x970b52bf8964934e721f655325cc946e4901be6b      1
    128 0x9752669abd5b7c5a3224bf9a3049d7961d25031a      1
    129 0x98d87b33661c9a10f8c5d51e65146f02c68f59f2      1
    130 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    131 0x9dfd31a8740f81fcf82cfbe35f9a1c0161613f18      1
    132 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    133 0xa0041264f924f00c6714d52ad90e58868bf930e6      1
    134 0xa1a89972db6326ab64417e1eae57a736b83f510a      1
    135 0xa3982c586a7d0b267e08118021b4b4b10e227d3d      1
    136 0xa6547607e18190ef82af3e832b098ebddeedcbf4      1
    137 0xa6e59b844891e619801b298f4f0af52054513a3c      1
    138 0xa86d8add58edf1e77a20c6edea06de34465ec98e      1
    139 0xa8b8d99d35cdbea702fdddd2085790700b1857ba      1
    140 0xa9316cd909559a8b272ca6f9179db6bbcde9e305      1
    141 0xa96cbc4c3651b25aebf9ec28d478781bc366bac7      1
    142 0xaa1e92ddd28c835fe66689771d35f38947950fd4      1
    143 0xaa525d002e0240a9c6461e3400e37962e4f0c5bc      1
    144 0xaa9c52276069b58227697805f24707e175313351      1
    145 0xaad08fea58a5822a076bba2f562f4be5e7db6f41      1
    146 0xabfb3a0a0e4c1bc2ad036f4531217e2becd215ee      1
    147 0xadf0dd8e27a8643347bfad2b48fb487710fe36ef      1
    148 0xaf5d970c3f340d0a1baa984ec07f8b90799bd011      1
    149 0xb02416c56bba0842a0c8c261cad38df0ea9bd305      1
    150 0xb03fb18eb14dbabdb66eb8c380cfaa97f2ac3d21      1
    151 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    152 0xb310d4bca61fa50e683088e5d78294bef96fd2fa      1
    153 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    154 0xb3707a1d3f5f92dadf897f4bf5ce506c0faefbe0      1
    155 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    156 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    157 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    158 0xb6ef6a5bbc8d77f83f09dab2b6f311c3356afc4d      1
    159 0xb824e5c5e0959887b0deed5454fd91aa0f405d2a      1
    160 0xb8eaee4a13dbd506bd8925cf7b9c950e7f5f8539      1
    161 0xb99f2d79df6784b2008602623ca122901c39898c      1
    162 0xba07d7ee1932821f3ed557b98230fd6ac9f2a106      1
    163 0xbc7b2461bfaa2fb47bd8f632d0c797c3bfd93b93      1
    164 0xbd070636eea199ecb5fa302ba48fbd086b4be5c1      1
    165 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    166 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    167 0xbeff97d345baa117f245a5cede3530830556fa1d      1
    168 0xbf79e6fbcb276ee022d3710d26ba01eac4dcad36      1
    169 0xc07a67f50eb5bf18fd0c140c702a3032faa3681c      1
    170 0xc0f6632a51f576043d0ba1f99c76cf434e940f77      1
    171 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    172 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    173 0xc314ae14a46e7fc679dbc80b32cacc3a6af3699b      1
    174 0xc3ab2c2eb604f159c842d9cadabba2d6254c43d5      1
    175 0xc3be75af49a71bd36b08c513cc4520b5e5290928      1
    176 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    177 0xc5f893eb8a3b771860340ab59cac5de4bd6be7f0      1
    178 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    179 0xc699e3a61b108c77e9872e49a51698b7ad62da88      1
    180 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    181 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    182 0xc881ba24fea61d9c629bb7eb79b3b20392584115      1
    183 0xc94d58e2af06a529dec8ee9a7b81c7030e753330      1
    184 0xc986ab5f1e6e4555f263727b3d013af095b6c1ea      1
    185 0xcc58c190f7b55eee00f09437d766f05913194f7e      1
    186 0xccb4e229fc4ebe2c8034960ca9a4b3ae5ef3a7bb      1
    187 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    188 0xcdbab8ff4de3c9ab024c4f17cb10b82e25ae16a3      1
    189 0xcf2f67229aeade145c9beb8508798458798e7b82      1
    190 0xd0b7cc2113ed934d9293c4bef02cd06ac5735bed      1
    191 0xd1bd8f0864b0cd72f116d17217d9ac7edf151453      1
    192 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    193 0xd46ec6c0c3a0059b40264bb100999776bdade75b      1
    194 0xd7533467e05c0c104ee16f1bc887d28612aad734      1
    195 0xd79ac194aa7da730052cc14b41a173bc4b4f1b68      1
    196 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    197 0xdad9f6cccb4a76d2bec6c01655c299b94c1355ed      1
    198 0xdd1df994af2fb033b4d5325f6a8b7cf8eff54b4c      1
    199 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    200 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    201 0xe37bd75a04a6311b86ad47e3e6ecbfd639889237      1
    202 0xe3a96bd21069fbe45355171203971f29bc22c121      1
    203 0xe40a2786405b4ed534633c7c63420d8514ea097a      1
    204 0xe6f35996ac684c85fd299b8ed256e2fd65e12914      1
    205 0xe760bc1d8b3a8cda9ef23708bf10aa1c530119f6      1
    206 0xe7811da743bb6cce0c49e1d362b2e9120360a373      1
    207 0xe81ccab76718ac222c8c9f2d19197c9d39e9fca5      1
    208 0xe8a0cc457405c250a3142db639b3e058fd431510      1
    209 0xe8b52916a11e3f4ef5f87811fc5edc112ce4b63c      1
    210 0xe96c8a3fa8d8249247f52359335dc571ed1eba05      1
    211 0xe9b53ee1ba333bbbad111a4ebcb08178f32013da      1
    212 0xeeefaa6890ac8bd6d05b48160fa663dc3a6fe72c      1
    213 0xef2befd1c0861eae1fba4703968d885430106539      1
    214 0xef7eacc3049a20bb8d1093dfeaa61e40c2f4349d      1
    215 0xf03ef3e139a037363a9a8e623999a69276449039      1
    216 0xf07a68610ad27bfedabae3d2f16927f8e5be66b6      1
    217 0xf190a6926ee7e9572f606187f14072e95832cbfe      1
    218 0xf3231261b5c5ed2beea362a02669eee8b9c45497      1
    219 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
    220 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    221 0xf57b432a80f32649ed4a67b5c7c5dd96d599eb56      1
    222 0xf5819cc26f0481c9b86294b4c24027518a04bd5b      1
    223 0xf5be0b66e1522125f84ec7973b020b267b4401b9      1
    224 0xf73c935815908ad9a056fc50269f95d8ba032e9d      1
    225 0xf908c3b9b8f1e1dfb6152c47aceb5ab605b72246      1
    226 0xfbae0c1fa41086ff83c4832256372c29aed8b57f      1
    227 0xfd22004806a6846ea67ad883356be810f0428793      1
    228 0xfd41dc8df72d91312ec0f982172c637ba4faf1a7      1
    229 0xff0bfea882373e98572a280d852edfeb675a57d2      1
    230 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1
    231 0xff2341680047fa367b1a091cc954eb06a0af167e      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 236 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0206b6213a7926c406968999e9bd97495c13fcb9      1
      2 0x02cfd5fd2898eba09af848950ff7a542c75de3d9      1
      3 0x0311c9df09c28978bbea44a606644cde11055fd4      1
      4 0x0314cc74e461d881ba3b5c93bba34cc81e4f7004      1
      5 0x032261cb5868414d70251a4d609e196838551457      1
      6 0x03e81230edb43a393d33b4e6724f31b955327f0c      1
      7 0x0519b923ee8159bee24fffd82bfaaf7e0024cc7b      1
      8 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
      9 0x07be036f5baeda1115b1132e2412900d85985510      1
     10 0x0836321e4b7c75f85472a451b3e04e5adb0e6177      1
     11 0x09c52c99e701304332b5998227f07d2648e8a72c      1
     12 0x09eaa08f8e288d34d416b92c53cadafb5cf1209b      1
     13 0x0a33896f09d43a5c9af1a14670c2869b945fe39f      1
     14 0x0a96d5dc037ea14ca76355460adf41c279877b9a      1
     15 0x0ce98350b9dea81fe0b47dbc4787a1e9f8f1d7ff      1
     16 0x0f10e80711e5b6b87b4c00d6acb6856be091e98f      1
     17 0x0f5d5751d4f300c3d0adbcadc05f0af4f8baf788      1
     18 0x128a4adf4f86a4e32447ca1f1828acf9707c6cb9      1
     19 0x13267f77e7241db36f9ee3418dd3069bbc91f2b2      1
     20 0x13d735a4d5e966b8f7b19fc2f476bfc25c0fc7dc      1
     21 0x1508622685a8d18f514612e6be4f44b691826c4a      1
     22 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     23 0x16a55d6c1d1991dfe63af7a0b8f19749791c9f52      1
     24 0x170fb8f8f37776dc48184686dec386d2d1c41cc9      1
     25 0x18d4abe1c93ff40f957124eafc1f632a9318bd8b      1
     26 0x19d4ccaedd75ef32ba76430af03e27cee010606a      1
     27 0x1a275b0bc79469dbb438aa33694f1b7348010d56      1
     28 0x1b21a4287cbfa8f81753de2a17028bf2fa231034      1
     29 0x1f8a8ac54244e4016a376152b2f92d467552fa7b      1
     30 0x20e2cb51f9a1788b3840d94aee6ff3299d3fde20      1
     31 0x21a06e63ee6dca27b49b87555b4e39a553ffa7ce      1
     32 0x22757fe151ee28e8d1492971ff8d70e3c1d3df9a      1
     33 0x2286f534a7821a952592fd1d4c78ffc453f64870      1
     34 0x22b74259bfa6c1ec630c99e108fe7d532df04751      1
     35 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     36 0x230512379ced76d35500b03b6892a31b17de1248      1
     37 0x2360f0f7e8f29231d844faed5de568e491011504      1
     38 0x24fc4565ee0be682139289ea669cf97d86b75845      1
     39 0x2638c0c7bb8b6cc7af4f90a7ab01df9398e46893      1
     40 0x264ca3862bdd88505839033bf684e3a217432d4b      1
     41 0x269cf21103a3d9a0a2a0849a056f7e291addffa4      1
     42 0x26c46e6f7b37e3eea85b6edf0e95583d0bb292ad      1
     43 0x282d656a9d95c64522f6bc1a42ee759de81e8dc0      1
     44 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     45 0x2a42cf4e1cfc33850fd8d8da24f86cd89b82ad95      1
     46 0x2afbc4356bf863a029ef58875ad00cf6b8975a52      1
     47 0x2c9b7c9b5e9ea9a7d16485f454f5ff0662d76e25      1
     48 0x2cdf6562aecd383aa45270f22b35920913f83ea1      1
     49 0x2ff08c037996d75d8680064241186768cf4e1fab      1
     50 0x33d8ae1cd83595ffb4104a6571290235ba33c8ed      1
     51 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
     52 0x36e18ab9da425d5b59de9176f19d867f8eb30b25      1
     53 0x38d0055e0de80b103263d8cfcb37bc1a2593f9e6      1
     54 0x3a3c2918deab486f311a82fc67798a2767b58209      1
     55 0x3b99969c74b3b991686ac41dd4acd2a283f1c066      1
     56 0x3ceae39c964eace130886325e78ded552f415124      1
     57 0x3ded646035e0ac9a4eeab15496b8fc008bcf4a49      1
     58 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
     59 0x3f63c576a04570dcbb05be4534dee0a7c3df5188      1
     60 0x4076eca4db9684fa1d9bfac231cb516889a33e5a      1
     61 0x40fbe29010ca2e25357202a8ae3474afd498166d      1
     62 0x415238cd0891f38591aa7cb48c8ed451acf54296      1
     63 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     64 0x4501aeb9672232c02fa717c8d69f29ec0db80a3b      1
     65 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     66 0x4638eed954ce851515cf8a1462dbcc69ca332ae5      1
     67 0x4652b1ae2a9d0050dfa73d2b4c87d9b3ea175eaa      1
     68 0x4735117ac47cd53338f742174bf2d7c5a6667ea1      1
     69 0x4dc208f337fd51bb6a35925cb4a6b0ed168d3460      1
     70 0x4e51a06e4643197e1be2f050b6af60f9ad3cef06      1
     71 0x52f051280e850f32cbfafd8d78c31edec4c3248c      1
     72 0x538eb86b52e480ec0ff1fa87c77c1f36e3f04a0a      1
     73 0x557c60995797fa7b47be105227a2e46148d85750      1
     74 0x57cbb6d42975eafbab88b269cb7249e05f425449      1
     75 0x58f8f8a6c81cec8b2fd3a3999b4ed0a0b2862a6d      1
     76 0x5949d14a911c196cc24bff3c63ba65a9e5170ddd      1
     77 0x5c7f3e12a7c86d54e115affc0162124589e4497a      1
     78 0x5e439b06536ce2a9f17b226b94a13b438ab971fb      1
     79 0x6264e10f53bfa97aa0b966f01e63cf60636a9c2d      1
     80 0x636958c636886b728ab9e190a8a77397b97fefc3      1
     81 0x648174333697082875c3187c6c2571d562b422c5      1
     82 0x64dd2d8ca8016d0af6095115f5cf677d950e7935      1
     83 0x66af9e0005166fd3631a5beabb6c4eddf3a65312      1
     84 0x66b280b5778c35c719209614428caddf00aaa3ce      1
     85 0x67243d6c3c3bdc2f59d2f74ba1949a02973a529d      1
     86 0x6a29cafc9dae22b131d619716ffa1ef6fd4bec8f      1
     87 0x6abce6d8f90cd034e6a1efa853c61ced6941aa7d      1
     88 0x6e3e9b841f2c0e8a85596cb3d413779479cc724c      1
     89 0x6e977c8155d914e7aa99331169939978a4d1b102      1
     90 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
     91 0x71eece7c9a6e68bed5ca071046cdccec20ff9808      1
     92 0x7284a6feb4ccbe66f8e52137b506085865ac8dfe      1
     93 0x736fbf35964693f9325417ff281d1a3c9044b213      1
     94 0x7499003740393bec0ece8745e099c2eb3f59dc9d      1
     95 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
     96 0x75ccf763b78ea6174604fca946465bea177f5e23      1
     97 0x776e9767a68ab12ce43bf1c7cb7fcd72f7ef7d30      1
     98 0x78297a87dba724a4cad36cf102f8b770bc032a35      1
     99 0x7842cf4a8668d4fa74eb033878a3b5b8ae52f86c      1
    100 0x79b6a2060427b0d34da346fb722766e7cc6f1371      1
    101 0x7a3a6fe2bde4ae601804bbf951912e51c82060c4      1
    102 0x7a71d126254d6ef6942e0b3d513dd5eb09d35c28      1
    103 0x7abdd18d37571e1ba884a8fa9e07a3612b584661      1
    104 0x7b5af6790381f932abae790e8b0d0ff50e287f8e      1
    105 0x7c862eac48b66d1222cc0be6bdae63f0b24f0fff      1
    106 0x7c9b8e872c0c67e33265cacec5bf4dc2c58ef501      1
    107 0x7d92a98884a726b005234ffa3dffe018b8ce804c      1
    108 0x7f02d0944832ae644faf30cd17f5e1340cabaf3a      1
    109 0x7f94e30381aa6657c45833ec7fce2e493c1888ef      1
    110 0x7fe329a60f5b86f8bc9fa2ecfebbd7596e5964ed      1
    111 0x813aff0d9f488fdc1aa48f03fbd9967aadb4410f      1
    112 0x8179e48d959a4c41d5e0432ac05b30707149d649      1
    113 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
    114 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    115 0x833306375573df6e02ba859581dc474e58bda43e      1
    116 0x847f35ab961629fb2c8fe98c6ed5b8c34a2e5ae6      1
    117 0x883ce32e186c69559e94c8ba276bfc4322fc194a      1
    118 0x89900edbf6b88e99ad849ea34d6e3df969631380      1
    119 0x89b21aef1716c5000471006fd31ebc8c426f1322      1
    120 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    121 0x8a4ae95f8c670f8bafcea5a05d9808c4af43f5f9      1
    122 0x8a507a95451dc47eb6c46ff21b58e94c95e357ce      1
    123 0x8b1de9760c745098af64a4327b4c9d113ed424e4      1
    124 0x8be5a2df6488b0937299d27f092716f4a9ab2fd8      1
    125 0x8ca88bcd7cd3e43c2f39592428565a2fa0545e72      1
    126 0x8e0d9d97efc9d2a32f18ca12e2f177eec382d951      1
    127 0x8e22df65a6cebf6709e1ca7f44c544a1c2fb5bad      1
    128 0x8f72a6934ce350b19432a3dcb1c698d6f7b49fb6      1
    129 0x946ecb45a0a6681c87abc6dc8f6471a7ab75337b      1
    130 0x966133652465b15365a91a8b1f6c95276838300d      1
    131 0x970b52bf8964934e721f655325cc946e4901be6b      1
    132 0x9752669abd5b7c5a3224bf9a3049d7961d25031a      1
    133 0x98d87b33661c9a10f8c5d51e65146f02c68f59f2      1
    134 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    135 0x9dfd31a8740f81fcf82cfbe35f9a1c0161613f18      1
    136 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    137 0xa0041264f924f00c6714d52ad90e58868bf930e6      1
    138 0xa1a89972db6326ab64417e1eae57a736b83f510a      1
    139 0xa3982c586a7d0b267e08118021b4b4b10e227d3d      1
    140 0xa6547607e18190ef82af3e832b098ebddeedcbf4      1
    141 0xa6e59b844891e619801b298f4f0af52054513a3c      1
    142 0xa86d8add58edf1e77a20c6edea06de34465ec98e      1
    143 0xa8b8d99d35cdbea702fdddd2085790700b1857ba      1
    144 0xa9316cd909559a8b272ca6f9179db6bbcde9e305      1
    145 0xa96cbc4c3651b25aebf9ec28d478781bc366bac7      1
    146 0xaa1e92ddd28c835fe66689771d35f38947950fd4      1
    147 0xaa525d002e0240a9c6461e3400e37962e4f0c5bc      1
    148 0xaa9c52276069b58227697805f24707e175313351      1
    149 0xaad08fea58a5822a076bba2f562f4be5e7db6f41      1
    150 0xabfb3a0a0e4c1bc2ad036f4531217e2becd215ee      1
    151 0xadf0dd8e27a8643347bfad2b48fb487710fe36ef      1
    152 0xaf5d970c3f340d0a1baa984ec07f8b90799bd011      1
    153 0xb02416c56bba0842a0c8c261cad38df0ea9bd305      1
    154 0xb03fb18eb14dbabdb66eb8c380cfaa97f2ac3d21      1
    155 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    156 0xb310d4bca61fa50e683088e5d78294bef96fd2fa      1
    157 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    158 0xb3707a1d3f5f92dadf897f4bf5ce506c0faefbe0      1
    159 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    160 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    161 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    162 0xb6ef6a5bbc8d77f83f09dab2b6f311c3356afc4d      1
    163 0xb824e5c5e0959887b0deed5454fd91aa0f405d2a      1
    164 0xb8eaee4a13dbd506bd8925cf7b9c950e7f5f8539      1
    165 0xb8ed7d09aeede2e7e9abd156c2b93defd32473cb      1
    166 0xb99f2d79df6784b2008602623ca122901c39898c      1
    167 0xba07d7ee1932821f3ed557b98230fd6ac9f2a106      1
    168 0xbc7b2461bfaa2fb47bd8f632d0c797c3bfd93b93      1
    169 0xbd070636eea199ecb5fa302ba48fbd086b4be5c1      1
    170 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    171 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    172 0xbeff97d345baa117f245a5cede3530830556fa1d      1
    173 0xbf79e6fbcb276ee022d3710d26ba01eac4dcad36      1
    174 0xc07a67f50eb5bf18fd0c140c702a3032faa3681c      1
    175 0xc0f6632a51f576043d0ba1f99c76cf434e940f77      1
    176 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    177 0xc28d30dd716a40b0c19215b732ce7be0e80a5098      1
    178 0xc314ae14a46e7fc679dbc80b32cacc3a6af3699b      1
    179 0xc3ab2c2eb604f159c842d9cadabba2d6254c43d5      1
    180 0xc3be75af49a71bd36b08c513cc4520b5e5290928      1
    181 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    182 0xc5f893eb8a3b771860340ab59cac5de4bd6be7f0      1
    183 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    184 0xc699e3a61b108c77e9872e49a51698b7ad62da88      1
    185 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    186 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    187 0xc881ba24fea61d9c629bb7eb79b3b20392584115      1
    188 0xc94d58e2af06a529dec8ee9a7b81c7030e753330      1
    189 0xc986ab5f1e6e4555f263727b3d013af095b6c1ea      1
    190 0xcc58c190f7b55eee00f09437d766f05913194f7e      1
    191 0xccb4e229fc4ebe2c8034960ca9a4b3ae5ef3a7bb      1
    192 0xcd241563f7288b5eb2b9b715f736232889b62d8b      1
    193 0xcdbab8ff4de3c9ab024c4f17cb10b82e25ae16a3      1
    194 0xcf2f67229aeade145c9beb8508798458798e7b82      1
    195 0xd0b7cc2113ed934d9293c4bef02cd06ac5735bed      1
    196 0xd1bd8f0864b0cd72f116d17217d9ac7edf151453      1
    197 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    198 0xd46ec6c0c3a0059b40264bb100999776bdade75b      1
    199 0xd7533467e05c0c104ee16f1bc887d28612aad734      1
    200 0xd79ac194aa7da730052cc14b41a173bc4b4f1b68      1
    201 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    202 0xdad9f6cccb4a76d2bec6c01655c299b94c1355ed      1
    203 0xdd1df994af2fb033b4d5325f6a8b7cf8eff54b4c      1
    204 0xe07caa87dfc96fbc74f4c113270cd385574d5bde      1
    205 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    206 0xe37bd75a04a6311b86ad47e3e6ecbfd639889237      1
    207 0xe3a96bd21069fbe45355171203971f29bc22c121      1
    208 0xe40a2786405b4ed534633c7c63420d8514ea097a      1
    209 0xe6f35996ac684c85fd299b8ed256e2fd65e12914      1
    210 0xe760bc1d8b3a8cda9ef23708bf10aa1c530119f6      1
    211 0xe7811da743bb6cce0c49e1d362b2e9120360a373      1
    212 0xe81ccab76718ac222c8c9f2d19197c9d39e9fca5      1
    213 0xe8a0cc457405c250a3142db639b3e058fd431510      1
    214 0xe8b52916a11e3f4ef5f87811fc5edc112ce4b63c      1
    215 0xe96c8a3fa8d8249247f52359335dc571ed1eba05      1
    216 0xe9b53ee1ba333bbbad111a4ebcb08178f32013da      1
    217 0xeeefaa6890ac8bd6d05b48160fa663dc3a6fe72c      1
    218 0xef2befd1c0861eae1fba4703968d885430106539      1
    219 0xef7eacc3049a20bb8d1093dfeaa61e40c2f4349d      1
    220 0xf03ef3e139a037363a9a8e623999a69276449039      1
    221 0xf07a68610ad27bfedabae3d2f16927f8e5be66b6      1
    222 0xf190a6926ee7e9572f606187f14072e95832cbfe      1
    223 0xf3231261b5c5ed2beea362a02669eee8b9c45497      1
    224 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
    225 0xf3e1f9230ef6bb5646dc04879e67a3005cb69b37      1
    226 0xf57b432a80f32649ed4a67b5c7c5dd96d599eb56      1
    227 0xf5819cc26f0481c9b86294b4c24027518a04bd5b      1
    228 0xf5be0b66e1522125f84ec7973b020b267b4401b9      1
    229 0xf73c935815908ad9a056fc50269f95d8ba032e9d      1
    230 0xf908c3b9b8f1e1dfb6152c47aceb5ab605b72246      1
    231 0xfbae0c1fa41086ff83c4832256372c29aed8b57f      1
    232 0xfd22004806a6846ea67ad883356be810f0428793      1
    233 0xfd41dc8df72d91312ec0f982172c637ba4faf1a7      1
    234 0xff0bfea882373e98572a280d852edfeb675a57d2      1
    235 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1
    236 0xff2341680047fa367b1a091cc954eb06a0af167e      1

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
