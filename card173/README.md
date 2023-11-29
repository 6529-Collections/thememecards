
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:462         Length:462         Min.   :  1.000   Length:462        
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  2.000   Mode  :character  
                                           Mean   :  4.329                     
                                           3rd Qu.:  4.000                     
                                           Max.   :125.000                     
         name          
     Length:462        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18646969 # https://etherscan.io/block/18646969
block_hash <- "0x693fbf053a3284264631e37a1f3998cd8be8506fc2433f9fdf45c30f64a99330"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4481 

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

allow_artist1    <- pick(snapshot, contracts=c("BinaryDimensions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - Editions

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 233 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000f4432a40560bbff1b581a8b7aded8dab80026      1
      2 0x0125cc84a1ff57c86ae6063b15049edca75a503c      1
      3 0x022fd450a42a12cbc5e5155133b5839f92f45aa4      1
      4 0x02f26a1ff2d800406cbf4e3d959ef3c047c7ed15      1
      5 0x04a764cf89ed0c4cf64d2190e2e7bef04462de9b      1
      6 0x04ccafdb49fa5961ec55c234a005e7663cc2eaaf      1
      7 0x08267cb203b4cf1066a81b0f218dddd8f0e33e1e      1
      8 0x0c306f1993d4ecf7fffbfd0a6149976f34a8931c      1
      9 0x0c6b85d0721905f7ac7bed47cebe58c0b977677e      1
     10 0x0cabac6f30b66c721435109f017419a2a48ef3f2      1
     11 0x0ccdf93f055438afeaea50b33077409c9538f1d4      1
     12 0x0d3c1f3c822dbdead3f27a62c88a1a57ee27463b      1
     13 0x121548714331f556f0213db956c8184d210362c8      1
     14 0x1228a857fd7ee845f4999f33540f6b9d0988e80d      1
     15 0x128a7ee5e9eb664172966f34afac82234c897e68      1
     16 0x13257e056112d578414e5d291926c9d684d5d9ef      1
     17 0x15001a844836bfccdef9f0fe326481c29052f8ec      1
     18 0x16b1dc26446a69d9307ae4ab2ee67c082165a4cd      1
     19 0x184f479a0f4285449756a94f2b063ce4b00066a9      1
     20 0x1b0f7946e89b700562062f45da044d0035127ddc      1
     21 0x1d4bc31726b55d7acdeea53fd51aa1d2986841f3      1
     22 0x1e74e3b9aff3180b8ba336c86e45d591974ae11c      1
     23 0x1f455832e75555cd848958efa5e85f10899463d2      1
     24 0x1f86210f2450c9d043c0ae415bc0f38417f13cb7      1
     25 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     26 0x2026f0a2f9d72f52bc2d1e9881c55d2bb1cc7c1d      1
     27 0x203528358cb41a5b472bfc9ee48ca330859d9ef2      1
     28 0x20ce68b0a875023d1ce516a928a082ac5627fa7f      1
     29 0x214eb2d31d8188759b25a18fe3305fea7977e8ac      1
     30 0x2351ca60e09a078ddfdc3ad4d00ae825ee81c113      1
     31 0x258d10438cae128f43802209040fa12e1a46eab8      1
     32 0x26526f9096ca882a2f346683f2b74474f8b52512      1
     33 0x2840f42648f019e97eb45aebe10e0d99267e2573      1
     34 0x28c173b8f20488eef1b0f48df8453a2f59c38337      1
     35 0x29a8fcdbe61302b42b05da77c7db2ef21ff630ca      1
     36 0x29c886d321f69ea8c35da1179872740e1d89a35d      1
     37 0x2a15f1fed563c9c815eb510dcb4a3475a087894d      1
     38 0x2a59d2927541d17c0fa19140703ecb4b697b765d      1
     39 0x2aa501b0fba3f2eb1abe2a7116685ef4fc60f82e      1
     40 0x2b27a40e78aae02f13ea730f2e49f28db97c01ad      1
     41 0x2bcfb6355c57646cd196ce4f14a8caf571a10e58      1
     42 0x2ec7b0b01d7c72a31f3834fe4f208c4d04d1cdac      1
     43 0x2fa1ddc720cac9c5c2e03e0a03c6e2330c94ec05      1
     44 0x30b108fd1d025f2edeb08b40d354be797c615cab      1
     45 0x31b9aa7881a0225a3d189a4f52a8a53f09853d7c      1
     46 0x321b0ccd207390fcb22b81e7ab139724511dbe8b      1
     47 0x324a26fc926c6a0511f47f329fcb5887440da303      1
     48 0x3341680f41a2e2405a7a61cfb020a92e94af4ead      1
     49 0x358b152585242861d024557cf685bf1df7adb289      1
     50 0x35a4c2bc98230d07c94de4961a646b8dafb6eb80      1
     51 0x35cde78e3434d033bcdbce70f7ee120fa7523854      1
     52 0x36ebd79fd8f5cec6b4914fa0d37e6aa2b2ed4815      1
     53 0x376ae03055a7a0a6f9a79476b2971eaf0f6e6f3e      1
     54 0x381da126d7100f7079b811cb78a346a9834e5c18      1
     55 0x38802a1c483c03bc5f7f8bfe7d789c8f8cde71c0      1
     56 0x3af99a4ba2943de1b56ce887eac9425c333a1472      1
     57 0x3d0a9aba6604663369ce736cb4a5f21eaf7faa31      1
     58 0x3d0dcf1d458e67a781de243850d21b5b11ac7957      1
     59 0x3d559d0a7e6bda753008c9a0c138d9a1d6115d6f      1
     60 0x3e483f5516564e8078b7108a8fba84762f6eb547      1
     61 0x3ee1c068fc9653489a5d03ab925047efeeb4bc26      1
     62 0x3f4f2609bad184b1412a382800b8a5cd5c0648dd      1
     63 0x3f563ef5f9bee511b129a3e632213443ea651caf      1
     64 0x4090b8843d4665a8ec789f0e96594d9c899a1196      1
     65 0x40ebf8a762801d1706d9d16a8abfec4c452d15e5      1
     66 0x4161cb28b929e90c284cd280daddfc5cfc76188e      1
     67 0x429d280f4f29e8f0fd4106bdca90828311b717fb      1
     68 0x43a4e886f371ee955e65cd9d88485c8edffdd3ff      1
     69 0x44900d2dcfa104bbd139544bb451b26bf7866031      1
     70 0x45647bd8fa8b0e6a98fbc6091266d5d519632084      1
     71 0x45753b8b1f4a93c72a91d23667f846ea31783f42      1
     72 0x45b71868fa9797fe3c6901dd0c7b66fbdfa13a23      1
     73 0x45f810c9051ba878cb40547c347384c530c7c246      1
     74 0x478e7d8309e4d7eafc810ae8050d7af8790a964a      1
     75 0x48ea53788931f1daa696d63f7c1044a427cae640      1
     76 0x4963434045c6b5d776963c6987755712dcd6575d      1
     77 0x4a8f6075a6faee7cea5233930a3fb5dff4f39887      1
     78 0x4a935a9baaaa46d6ebabd731cd43665ef5a98134      1
     79 0x4ba7ddfe032f6126a24ee48083ae4aab9633d689      1
     80 0x4c7512f396536340f598735be36522ec7c61c82a      1
     81 0x4e749a6aadc2b86112676ff53567170268fdfcf1      1
     82 0x4fb9316f2ee8fb48addcc3e996e5115a7e1f237f      1
     83 0x4fc1252351e19e9a2c0204d818b2371c23492c12      1
     84 0x508dd44ff3404e618d430d4562c3773b073a5ccc      1
     85 0x50f8c08b0124092e1001b355f4b8ae2df85f715c      1
     86 0x54021e58af1756dc70ce7034d7636de2d2f1fa74      1
     87 0x55b97e94b29cd9bf491fe63011cc23dae6392628      1
     88 0x5603d121834371c219a12bca6c1f8afdcde40e97      1
     89 0x563056745f6fcf971b6ff9ab24a1ce9d4118e5c4      1
     90 0x563153823d702516f92fc24edd9358d6973f60f9      1
     91 0x577d28133471b8183acee876f9928e4bf4e4e723      1
     92 0x590ef336b0617900bd61201cc34c3cceecd20381      1
     93 0x5b92d2b38ab372bff63f55113b98253adff62a74      1
     94 0x5bc1bd2723727cede0d6a77bcf3a5bd6a5c8ad5e      1
     95 0x5bfc1c281bd0ebb99d62209e15573b96cddd7a5c      1
     96 0x5c6d1ebc79e908b574e806ac3681df28d22393b6      1
     97 0x5d403ac9eb70ae73e28de3221953e5563afb2438      1
     98 0x5e7ddc75ddba78301f35e4c98b6c1f1684a6fb8e      1
     99 0x5e9f96cec38b684bee7634244a4679ca77df4fae      1
    100 0x606eaf6afae3cad042fbe7d8d32aac88935f3430      1
    101 0x609399fc46eb745a0e28ae37c786089a5d8dc501      1
    102 0x6201539cdaab71be9aa41e0cb7cc08302bac71b1      1
    103 0x62bb6d5e99a73fea24390a9825f89282ec4c908e      1
    104 0x6312f0d37ce4456f8d745df4f5072391eb8fee8d      1
    105 0x65f7e3ea4c1507f50467b7334e6d8f7547bb41d3      1
    106 0x67628461109e58cee6e9eb14c7c6936efbee34b2      1
    107 0x690fe7c3d7f721ab764281911d6cbe358db1bf2b      1
    108 0x6957ed92c4b7b389ca92d97d173271a642360701      1
    109 0x6a4bf9949dc169e79762cc116e309d08a6ea4a5e      1
    110 0x6d0b3db2d2e848631a2e12aea38f6c55099437f2      1
    111 0x6df31405b841750e9a680bc85ac567193104fec1      1
    112 0x6f4e3617db82be3aa2f470de50051f7b3f95be9b      1
    113 0x6f5b5e073d646af1885c78e9bbf0079767ea3f3b      1
    114 0x71211a75c7995aa0a3f3fbf666ccb9446ce051b3      1
    115 0x71c8614296a0b69bb209f9912c3fc6c066ebc5ff      1
    116 0x72ecf5fb686b2acd3a57cf8f9961ed22c219c9c4      1
    117 0x731a54014b5f74e1458b9c17dd69cb58acebe531      1
    118 0x732bb12525961f5853154deb9d0a4aacd2eb240f      1
    119 0x7465ce02a9b3eac18b580ebfe87c4ac9fbb2e628      1
    120 0x75950c499084ffd3130288dd8a929136b988c9a4      1
    121 0x76441f45f00bd90823c936c233e8f11aeaa6b501      1
    122 0x78195c870b277b9f654d718650727198ed8a43a8      1
    123 0x7b5f7c0b36576f55c58e2f661d40fdb3bafce328      1
    124 0x7b85f6ad47c397099557c09c11e087b5f0828f4b      1
    125 0x81dcab1513d88478a3536348d6d7560f2d8762a4      1
    126 0x823109c99824d5c10c88f5003d388a9d9df15ae0      1
    127 0x834a9484537c2760ef6d6e6736d1ea800d67966c      1
    128 0x85285cf2156af7f3cbc23d8c2c764d1199805a46      1
    129 0x852ea856fb44e0d01973440bbc26c8854eeb669c      1
    130 0x858013142255cad3fd5137bdf4a7a40348cb4d4a      1
    131 0x86e578946d012b73c4b62070af5c8c9e62d5a22a      1
    132 0x87631b45877794f9cdd50a70c827403e3c36d072      1
    133 0x8767f9face6a268256d346c972c786f160a59394      1
    134 0x89fbd89e67509e7acc2110a24b72b74f6eb81be1      1
    135 0x8b7f927dd8368616ae9a40207085ad7d14d50432      1
    136 0x8d3925584d117dc94c83e19507f73f0f4a68ffe0      1
    137 0x8d831f50b30abeb94253229d5ffbef72fc5cbfd5      1
    138 0x8ecdda228a2d19b905aa96d63a956e657d17ad98      1
    139 0x8ed3d17cd74348330e26f6efd03f44fccd219109      1
    140 0x8ef417cd1d063d4fdd0e9fa365ac9786baae3a86      1
    141 0x8eff6a1864894db816fd46b6c3cff7a16126e943      1
    142 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
    143 0x9113f0d1d6541c4fe16b55873aa79d4a90b0bcc0      1
    144 0x9385b8303c136033e05a40e7913cdcedef211886      1
    145 0x9417ab659e81411dc3d0385b2dd27fa0a4f38392      1
    146 0x95891562df04ddc3214858130dde22e2fafd5f03      1
    147 0x95c6d0f794b9c629c94b1cbd9bbf4c290d326f99      1
    148 0x972071cf406d3ac59f64596476b8dd35a3f67303      1
    149 0x989be987c03e4edb18064c5ccd357ad7c5efdbe9      1
    150 0x9aa824a302597e3477d0435cfc5b1e1b9eb23449      1
    151 0x9aa85697322f2353c183e3b46f68b9c13bed5054      1
    152 0x9ab53482709d76346123bf4881dcdbe9cfac14ef      1
    153 0x9b7ee85506ef305f625f895be4739c6c633a6293      1
    154 0xa16543c88fe017f37620d3b92a5b97fa0a72a6dc      1
    155 0xa1a1ea2f4ef2c85d24de68057939c1d2daf4a131      1
    156 0xa24c929ba69a7abc09d0d822e416c172b0eb66d3      1
    157 0xa261634e88daf2630946ee07c643bbd4de640eca      1
    158 0xa394e5a73117a50edcb7c6440d44638cf854b92c      1
    159 0xa517a89cf035af6d1d9c3a139258578c175dc282      1
    160 0xa5363d48e1e770b24cdf4d803ab83ebda22ff0f5      1
    161 0xa56c6b57127e8881fbe51046058d0ddc1bb9e24f      1
    162 0xa5f64fd823ccf6b5729a8c913c67b67d744966e1      1
    163 0xa6037450f99144298e3a9238b411668e04266811      1
    164 0xa6e797e5e2fe1cb84927340843c81e3bd8d013cc      1
    165 0xa7ad4296d9f41434d1982a4c4cd98df88121858f      1
    166 0xa81725fae8bb3930a95debb0adeaa184b5e9cc10      1
    167 0xa835e600183889569aaaff96d5a4d8c9c8b38c35      1
    168 0xa93599002e0c6ac38f745ce420768be6787f850f      1
    169 0xab16b971a3f666c713297a09bbb1b833fc2feedb      1
    170 0xab1b9521de0f0a30c43817c66c54c06a95548058      1
    171 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    172 0xadba5ea1525c5ae27a0f98408c8e5d67e28c754c      1
    173 0xaed3d37b761bf61767208da1dbb3a971b9f13fb5      1
    174 0xb06c7b4041264fe221422e9cb3a468cda19f4b75      1
    175 0xb18c26983e6e52c36695e7545d09546192d8fdc9      1
    176 0xb235ba58e93ba482b19e81d66eb001cd6ffd601b      1
    177 0xb27843028441a1ee753057980cf5098a21e71567      1
    178 0xb327c369750d45fae30b836d36d1692787a255e0      1
    179 0xb32e0b5f762445398463758c827b15f38739df63      1
    180 0xb4dbc39c3ea0dcb67db6c4ce35c6cf0f9f1cd47c      1
    181 0xb6762c98c9d86a5f9585afb6a777e5269a5d9263      1
    182 0xb735b24baf131d46640dd9af3e378ddb99867ea0      1
    183 0xb7a4c15ab3ec5470c8a1422ebd92b4d971b85bee      1
    184 0xb8385857f57ae4d82b3510a7e2e6086320971f0e      1
    185 0xb87ebf06f8c99f43ecad940e4f1ace84eece776b      1
    186 0xb8a5137c0c6e8ea2e2ace151e637d6c015b1c0f2      1
    187 0xba81086f77432d171d8747ed63916dce55ff272d      1
    188 0xbac894e709927bab4f80505243d51b01c318bc93      1
    189 0xbd767a5f341f4083cc619e4741f719370c619fe2      1
    190 0xbf7ca30788d24031f1ffc6e3124adbf5197685ec      1
    191 0xc2443081f96da64a51f9e32aef05971d6d3ac645      1
    192 0xc352b4935f02eee34343bad9883f6b555bef8910      1
    193 0xc580950681be90e085734509b8cdcb6b16ad41b3      1
    194 0xc787008ab01b4f37a34df6453104198f416c7eef      1
    195 0xc7878c2d5dd500f787370309aaed195d51b14710      1
    196 0xc99545b3258d4a5fa476b017a4d4158f35de0a67      1
    197 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    198 0xca9ba74ee20917211ef646ac51accc287f27538b      1
    199 0xcb19fb33913cfb0c0864ace5f4941a3e9e845092      1
    200 0xcd6d22fbd8eedec089fc0fdd9572deb49b83a7c4      1
    201 0xcef397cff890e226e1f672f6d4aa2268030a3ee7      1
    202 0xcf6ffd58c3c3e72b07bff2cd578573f5ecaf0280      1
    203 0xcfe35b407a3380bed5ab5c6aa0a253aeb7c011a7      1
    204 0xd16150765d5150374035328adfea9782107a848e      1
    205 0xd1d8f0732d1ea3af666c98fd3b8815c5bc33e281      1
    206 0xd26c46d1c1d5413a3e14d24c1b8c4157e318e600      1
    207 0xd2d669e927189998b2fc08124f4ffe91a2486cb0      1
    208 0xd322135fb340c2cb162c838e4006f474986cfcf2      1
    209 0xd4b25a7dedbecf6d35c84b57fa89f6a732582b87      1
    210 0xd5b0e02e4cecb59ead4b7b29ee1148f7de774194      1
    211 0xd8b428f3b8057ce0aad0b1e2c3028677ee5bea8d      1
    212 0xda925fa3823bbc85875e1530636b94641a264f15      1
    213 0xdc0548af1b43ede7c431ce23d828970c67e91c64      1
    214 0xdf3c392e418f751f055b4245884ff21cced14d91      1
    215 0xe1c47a494fa4ae2e9ce13f400b4c1e164d1b74a3      1
    216 0xe5375eb9c8f155f827831903f039a1e328741976      1
    217 0xe605daa4160e869524842b50ff7826cb8dc9815a      1
    218 0xe9f0db9a703420c05e2acad0f6ff9cfb69357ee0      1
    219 0xea43d6caad8ba7ccd63efe9a85b544771c082cf9      1
    220 0xea4d8482132d15666a58bdbadb6c856eecb1b5bc      1
    221 0xeefa6898191c92690bd8ef1bb972096d2971bcf0      1
    222 0xefb28a1d2ec4334214dc3a2a53e5c70647d89747      1
    223 0xf17f2e5751b2ef41b888451089e3f84f9274daad      1
    224 0xf31faa095adb816fe30277de1b3aaa3846a3dc4d      1
    225 0xf4401fa25244ff799505972df091e4e6ed6bbcc5      1
    226 0xf4cce7303cc70546657950169e90f75b39c920ed      1
    227 0xf5d5d86717ad1eb192a9686da87267e25be9da43      1
    228 0xf7f3680d0cb3035da30187ea0e9967b8fe9d4ca8      1
    229 0xf84a5fd946a7714c6d48508fa8292c8c5037b5a8      1
    230 0xf9bb36f0919c6de88c646857ece9837054c816d5      1
    231 0xfaaf6324666971cab93023654405188258ea898c      1
    232 0xfeaa535d0c6f25c120709238f665eceae0772527      1
    233 0xff66832e0674ef4cded709ea0481e31a4976a5f1      1

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
