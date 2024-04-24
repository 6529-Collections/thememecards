
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:527         Length:527         Min.   : 1.000   Length:527        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.302                     
                                           3rd Qu.: 1.000                     
                                           Max.   :19.000                     
         name          
     Length:527        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19694269 # https://etherscan.io/block/19694269
block_hash <- "0x412e0d999c386b929d00bb111776be3c4435a4c25a1ba294ae8cece3622183b6"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4539 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","Foundation1","Legend2","StillLife","Body","Balance"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("DreamersEditions","LCPASSEditions","OpenEditions","PrestigeEditions","ColoredLCPASSEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","Foundation1","Legend2","StillLife","Body","Balance","DreamersEditions","LCPASSEditions","OpenEditions","PrestigeEditions","ColoredLCPASSEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 28 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0166ab96b38a8f346225d316ea09156188ccf020      1
     2 0x0522f4bc456647377e01c2c87173c087a55c533a      1
     3 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
     4 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     5 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     6 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     7 0x22221063e238d31add04e561f4d9896795313412      1
     8 0x2748dd506916598ae2b06629bc0d83d72e9ed295      1
     9 0x2c59a8c6ddc24fa405bf0af881e436a296a52cce      1
    10 0x32e90406cd2bfe7275a7ab72a13e7037ee8e909b      1
    11 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
    12 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
    13 0x46105de145ceff841edc1382f3e9d6836ae4a609      1
    14 0x596558fd287175afa7ff695f565f65e38547b0e6      1
    15 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
    16 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
    17 0x7e2f9c4193f0bad1fc36abccc713137d0954014b      1
    18 0x9045003fecbc67280240c07b0df300d64d09f7da      1
    19 0xc876b346d50c4199458ee0e3754883f62fe3a5b0      1
    20 0xe49fd99292983927398cab7e932cf8903eb12ad5      1
    21 0xe5678b3a07c101fba71ea3d44848799eff0d6cf8      1
    22 0xe72c4bea4b5f5caabeaca6cd38894f8ede1f2e16      1
    23 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    24 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    25 0xededf4df6bee95786ca56a04f2cc7ec7c8fe35a8      1
    26 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    27 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    28 0xfe91bf57cffcba664167082e73342e1666fafdba      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 212 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x03025b2e8acf9322230c7a154cc9b192bd245100      1
      2 0x048a26e7ccafe4e965c1d82e21855b1b770833b4      1
      3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      4 0x07785597fd4a7d0ce4e0fddccb85e01f09dc08cb      1
      5 0x0b8edc5d3b491aa703f446e28d20f407f093c13b      1
      6 0x0ba9d9bf30a86945e3218bfcbca346c5d7f81838      1
      7 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
      8 0x0c60d8b3d638d9a9bbda663fcf9ad9a8d010323d      1
      9 0x0dae94f2b836b4e2d87918e5b5056c7605c3076c      1
     10 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     11 0x13257e056112d578414e5d291926c9d684d5d9ef      1
     12 0x15242eaf7884b3f655d8ac6727e13757db0b854b      1
     13 0x180669310d089fafcbee40479ee719752d680d4e      1
     14 0x1885d64948580a54a58f23ef3146b2ae225f3f56      1
     15 0x191e3291dae72f59a670ce8b9475081e5d07de40      1
     16 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     17 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     18 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     19 0x240916c131e67f2c620cd7939654af73fb3c14d2      1
     20 0x264ca3862bdd88505839033bf684e3a217432d4b      1
     21 0x26d9c909c4761bee65d57a301b10deccfcc38d06      1
     22 0x27b304669b1ad32240c8df45fa95de85c28f2d2e      1
     23 0x2835241b4288f8752837e91b1ffd945b08cd8878      1
     24 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     25 0x2d0bf022e326eecaf231eead1a360f6472b715d6      1
     26 0x2d0ddb67b7d551afa7c8fa4d31f86da9cc947450      1
     27 0x2d745c6197acad49827c9ae01d7fcda29a6d0b31      1
     28 0x2e407a65dc925cb9884cff588e0ef1814e221466      1
     29 0x2eb313694cef9605faf6b9cc3846ffd7dd6c6a60      1
     30 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     31 0x32c60bf76818ff92453025f30dd21a7e44b709fa      1
     32 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
     33 0x336bd505b648d378a251f06ec3f6693620913f92      1
     34 0x344cdb2de46696c988eaf05efbcc33f33b0e3cb6      1
     35 0x350aa47305555e8c6e80ff1b670b3db228573520      1
     36 0x35a4351ebfab671416c6b7075c4c64faebe625b0      1
     37 0x35c692c7dafbfe2534de69d8114a393940b3b366      1
     38 0x35c92a2447de0576399aa34b74d2cb26af950a69      1
     39 0x365733fc86e2a7d4ab70dfe106497ede37b0d729      1
     40 0x367ff64da0668d86e7ac5d43dd9459fd4ce381d6      1
     41 0x373d419979862c6845e74cee5ae1e075801b6abc      1
     42 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     43 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     44 0x39b787e1c6c0716d1f41cd77769889513131776d      1
     45 0x3a9f45ac308ccc0a1a48b0f9e2f8ce859a0039ea      1
     46 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     47 0x3b1431d0b99c393216a1b065479a34ed37edce5e      1
     48 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     49 0x3d446c9528f5dec1848dbe4cc6d4fd2c432b336c      1
     50 0x3f5f37a55e65cd1d42a3307f030fbe983f39e4a7      1
     51 0x3f8550c6ef10ed9c95ca850dee528d25fe862d74      1
     52 0x3f8fa3b3466efa02470bc418b2d307d06dfc9c2d      1
     53 0x415172957acd7002feadc91bce0bcb7deaaed868      1
     54 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
     55 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
     56 0x478bb542f7658d635abba67edb987806dff5b83d      1
     57 0x48b56fbf752c5d4758e01694c19bf1b4dbe03a55      1
     58 0x49140e8e60913110f2e37ffaf4f5231a03611e3d      1
     59 0x491befac53aac201e97cd6f4c750e4a958d22d46      1
     60 0x4cd5dde5a1b632735fb19667a7c871743c00ba59      1
     61 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     62 0x564a544b7fbb31c976b353dff3ec4015e0871619      1
     63 0x57db311f8b2076bdde5e0fca74973204ae20d59b      1
     64 0x58b3492976b3273ee7d256ab8bf7f9338f3b37fa      1
     65 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     66 0x60136487f2526531d8ca1a0f9d6fdf7f5220a02b      1
     67 0x60322b225e06bf2f0c36c45d0b06090e891ca91c      1
     68 0x608a48fe1a4990315af6b0c016d1f0d2f582f7a8      1
     69 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     70 0x61f5c63f6d56c0c7a83291aef8385f5de22efc58      1
     71 0x62d3c2a80d3cce916b9b645cbcda14d35418275c      1
     72 0x62d4377141e0a4a6eaab3167e0ffa4bc037db3a5      1
     73 0x633fb030794b8508cb27fb1ea6b45df95bddbbd1      1
     74 0x64084d818a908d809d0754dc09e32834c048342b      1
     75 0x656f6595a0c8a5c4e5d6b193228aacfd7cf55c47      1
     76 0x65c6f38e1f6687c64dfbb18c5a37bbd6f6ad0692      1
     77 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     78 0x67353d0d86237b69eb521906f7e05b15b7454c63      1
     79 0x69cb9f5c9065e8fd858497e801d4b085638aa7ba      1
     80 0x69e67aad5495a48c4ef3f770d93a99935e1dc90d      1
     81 0x6a7928165f5ae63df374b2e6f0e8fe0a0a5ea042      1
     82 0x6cc5fffd0ff76b485f99dad6d0ca9413b9667114      1
     83 0x6d5c8f445936aefa1021c6d53e86c4ad5545e48d      1
     84 0x6d8ea9bdb65412df77ac68d747d4c580107ab376      1
     85 0x6d98c5b40356a3189298df88fe2d7108fa6265b7      1
     86 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
     87 0x6e8a8f55a81b3f1dab9831fb44b2cf2bbc447977      1
     88 0x6e91e00fad12e30878a12cb9bf121805a994cee5      1
     89 0x6ebe7f9cda8619ab2c508ba6c8f6675836185e0c      1
     90 0x6ecbc32217572222b476a172677f8aeb920639de      1
     91 0x6f278ef400d31730310e321a39ce9acc8de00624      1
     92 0x703b244ee82ef3ba949160197c343b48e5d04908      1
     93 0x70ddda43d4f616263b1db0ec672cbd8795db13b1      1
     94 0x7343b9d4a3a00f92e28ad553809f5f23f4976c33      1
     95 0x7485e327bf1d82e1cece52a5f821f1632ad23abb      1
     96 0x75256a46e98eb0f9c4eefc197eb3dd88d559a771      1
     97 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
     98 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     99 0x774cd866ceadf1871efd610ad30603ffb8034ae5      1
    100 0x78641a82415044490aea56f3a2c07d263d1941f4      1
    101 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    102 0x7e45d8d82713396f3853031a957b18034dab48a2      1
    103 0x7ebc6689843ca9212fb945830fdc248c436944e7      1
    104 0x7f54ad2ff488cd59530c8421e87d789676fb3c3c      1
    105 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    106 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    107 0x822f0ff30ad18d4ff379c39dcba3e72a75e108a2      1
    108 0x82f23de5a474fc904041c357576afe53c30dd250      1
    109 0x830f459f98514e306cdab5380799a69fe2db2a7b      1
    110 0x86f0d6712cd1b59ae7d2bd7c7777dcdf3de252f2      1
    111 0x873427efac598321cd2019a0c4ca6f66bf957c15      1
    112 0x88d7d41e29612b643c445983c988d447d8ca37bc      1
    113 0x8aea4d2a686a82ae455c09e372920a628848f667      1
    114 0x8b5f371edff19761629a1ff2bb3ba0e1b0684f87      1
    115 0x8c99623cee8fa0bf6e7632c8f170c6467aa54daf      1
    116 0x8ef19d27aa64609419563d2745b8f0a0c6cdbf0d      1
    117 0x8f641c6e5a3aab5596f692881814965b892d3f84      1
    118 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    119 0x9192683edea0264fda574ddf101a362207169e2f      1
    120 0x92058906c65bc9cf1b77f2f76de0687854cae050      1
    121 0x924449ea24fc318e4867e1773625ea62ef53513e      1
    122 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
    123 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    124 0x95d50b3edd982dd43fc51dbf68d091904c1aa066      1
    125 0x9649055a504f627b009c45611c0c1435c49f2d5c      1
    126 0x976828b23af10f75e94133b15f31ffb054572c7e      1
    127 0x980e22587fc29d25cf2c82baa6933dc0f73c09c4      1
    128 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    129 0x98cf0978e7144ce0ce8e911449d02b9c56231667      1
    130 0x9960937e6e9f93f9b94398ad75e9bfa2acbfa68c      1
    131 0x99a2efeb580b32c8de7b9aa56406bc05a9333557      1
    132 0x99c46ad6e52d196d04a795ba970f72ada7f777d7      1
    133 0x9df2844244ff3c0be263d2e09b32f3311eb77b1f      1
    134 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
    135 0x9e976c76f7441c916ee8f40e3965b41a2229f785      1
    136 0xa29f7d70c623e8cef9989d7b063212fdeb3047f8      1
    137 0xa3599cf1048930c9e897fe9245dd95511cd16f1c      1
    138 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    139 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    140 0xab0ac99d34b23dbfd106f539bd8a265275f28c87      1
    141 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    142 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    143 0xac896d4906584fe138a3b4edfbc55570d0f0470e      1
    144 0xae93133a5c9e86336c4005a04d6150a52da4854c      1
    145 0xaf4c0da9c2d9018dcc55b8cbfb96fb9348b61765      1
    146 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    147 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    148 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    149 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    150 0xb407763f587f5f8726dcc9eeb7a72c402117016a      1
    151 0xb45763c75556484af461e00f0f3e0954ed60b25b      1
    152 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    153 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    154 0xb70d0906ab07ae1fb9283fefcf3df7d499c0fffc      1
    155 0xb77b1fd000e21d7e831a17e5c146e0ffe5ece440      1
    156 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    157 0xbb261e9c522d826886cf9d03b200e268a7ca36cb      1
    158 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    159 0xc01c99b2543e28eff240e90384a1fd757a484927      1
    160 0xc02d1eda13d073bd2c245f0a6b5a2aed9ddf5c35      1
    161 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    162 0xc4dad120712a92117cc65d46514be8b49ed846a1      1
    163 0xc50c43b274f264c765b645214a266bd809b25e2d      1
    164 0xc5611f5225f835f066a7990feeedfc2bdd07d554      1
    165 0xc6aa79a22b08c794b25dc58cb36f2250779cdf4f      1
    166 0xc6c5ee2c54c79695ebef26f3171e5b96ed74578d      1
    167 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    168 0xc785461e7bd4726e920c3103683eef2ec51b4efd      1
    169 0xc828df18d5a55009ab502137a64e9d77808a5bf6      1
    170 0xca9bb916074c6a51281a45cb9b4123a41f79cbac      1
    171 0xcb90dd8a79be00ca9ab62284978cf881c2899346      1
    172 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    173 0xcdb9c642d4e7385d52c5eb6cb9d32d536e12750f      1
    174 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    175 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    176 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    177 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    178 0xd53f450865fc6ea2d9bf38231aac358eeb137c84      1
    179 0xd54066fc71914e6f6890cd49dda28a8755b3cff5      1
    180 0xd5f6812971692117f581796147d2d3a2a579737c      1
    181 0xd6d9977522c6f4126ec407de2af999c29e672268      1
    182 0xd7b83c30609db3f1f4ad68d9c046703a7d06d722      1
    183 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    184 0xd8d5b0a8255bdb366827bd729db67b12432ac6ee      1
    185 0xd8e769a87ff31beffaecdcebfcdeebd8f2a5ad98      1
    186 0xd9854b1bc87d37d295cc402e6cba23f9a4cd7dda      1
    187 0xda834e5743899eea822f877d3dbf1dc18489583f      1
    188 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    189 0xdbeccbbbc202a66ffa97f0bd3299a37485d7e7b0      1
    190 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    191 0xdd787d1da924e95281aba822a8baa1c6c081e2b3      1
    192 0xdf98322a314921fc6863806dae62a157081b0a5a      1
    193 0xdfc88dcdcaaab27d50ed9502c74a5d6b3b2a0816      1
    194 0xe06538abfc043aad711ae7894010b74a44be92e4      1
    195 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    196 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    197 0xe626cddfb9773177079a47dc46c603c107e7c2a0      1
    198 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    199 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    200 0xeb6d2870e0b66dde3373996ab5abf34c8395cb95      1
    201 0xed076561823c6797680e1167ea0f33c71a9098a5      1
    202 0xf035839daaf652f03156b297a27eda25a35e6316      1
    203 0xf19aed29a6cd4f5a1b3c8ed79d987e3fc1dca853      1
    204 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    205 0xf3d834c5c5189bb0b4119dc7b366910fdd6bc81c      1
    206 0xf4dbede0aa5d580b7191acff0a40a33c885f69c9      1
    207 0xf5a93410e7e32bbf28a8eaafbd7f241cf0b290fb      1
    208 0xf5b05e9e1ca1d622f84c3cdeac27a94e3cb6d978      1
    209 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    210 0xfc167e6d3aaf26fd8e13a8c1d719fc112af23e89      1
    211 0xfc1eeb7402d9a1179d43d6f97a2a907a7262f551      1
    212 0xfc9e36803438104929d54637c23b6dca391dafc8      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 240 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0166ab96b38a8f346225d316ea09156188ccf020      1
      2 0x03025b2e8acf9322230c7a154cc9b192bd245100      1
      3 0x048a26e7ccafe4e965c1d82e21855b1b770833b4      1
      4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      5 0x0522f4bc456647377e01c2c87173c087a55c533a      1
      6 0x07785597fd4a7d0ce4e0fddccb85e01f09dc08cb      1
      7 0x0b8edc5d3b491aa703f446e28d20f407f093c13b      1
      8 0x0ba9d9bf30a86945e3218bfcbca346c5d7f81838      1
      9 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     10 0x0c60d8b3d638d9a9bbda663fcf9ad9a8d010323d      1
     11 0x0dae94f2b836b4e2d87918e5b5056c7605c3076c      1
     12 0x1134528d239cf419f9081ceebc3430ac191c318f      1
     13 0x13257e056112d578414e5d291926c9d684d5d9ef      1
     14 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
     15 0x15242eaf7884b3f655d8ac6727e13757db0b854b      1
     16 0x180669310d089fafcbee40479ee719752d680d4e      1
     17 0x1885d64948580a54a58f23ef3146b2ae225f3f56      1
     18 0x191e3291dae72f59a670ce8b9475081e5d07de40      1
     19 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     20 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     21 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     22 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     23 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     24 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     25 0x22221063e238d31add04e561f4d9896795313412      1
     26 0x240916c131e67f2c620cd7939654af73fb3c14d2      1
     27 0x264ca3862bdd88505839033bf684e3a217432d4b      1
     28 0x26d9c909c4761bee65d57a301b10deccfcc38d06      1
     29 0x2748dd506916598ae2b06629bc0d83d72e9ed295      1
     30 0x27b304669b1ad32240c8df45fa95de85c28f2d2e      1
     31 0x2835241b4288f8752837e91b1ffd945b08cd8878      1
     32 0x287bdf8c332d44bb015f8b4deb6513010c951f39      1
     33 0x2c59a8c6ddc24fa405bf0af881e436a296a52cce      1
     34 0x2d0bf022e326eecaf231eead1a360f6472b715d6      1
     35 0x2d0ddb67b7d551afa7c8fa4d31f86da9cc947450      1
     36 0x2d745c6197acad49827c9ae01d7fcda29a6d0b31      1
     37 0x2e407a65dc925cb9884cff588e0ef1814e221466      1
     38 0x2eb313694cef9605faf6b9cc3846ffd7dd6c6a60      1
     39 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     40 0x32c60bf76818ff92453025f30dd21a7e44b709fa      1
     41 0x32e90406cd2bfe7275a7ab72a13e7037ee8e909b      1
     42 0x333fccc5046da80e10078363f7e0faefe08aeab3      1
     43 0x336bd505b648d378a251f06ec3f6693620913f92      1
     44 0x344cdb2de46696c988eaf05efbcc33f33b0e3cb6      1
     45 0x350aa47305555e8c6e80ff1b670b3db228573520      1
     46 0x35a4351ebfab671416c6b7075c4c64faebe625b0      1
     47 0x35c692c7dafbfe2534de69d8114a393940b3b366      1
     48 0x35c92a2447de0576399aa34b74d2cb26af950a69      1
     49 0x365733fc86e2a7d4ab70dfe106497ede37b0d729      1
     50 0x367ff64da0668d86e7ac5d43dd9459fd4ce381d6      1
     51 0x373d419979862c6845e74cee5ae1e075801b6abc      1
     52 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     53 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     54 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     55 0x39b787e1c6c0716d1f41cd77769889513131776d      1
     56 0x3a9f45ac308ccc0a1a48b0f9e2f8ce859a0039ea      1
     57 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     58 0x3b1431d0b99c393216a1b065479a34ed37edce5e      1
     59 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     60 0x3d446c9528f5dec1848dbe4cc6d4fd2c432b336c      1
     61 0x3f5f37a55e65cd1d42a3307f030fbe983f39e4a7      1
     62 0x3f8550c6ef10ed9c95ca850dee528d25fe862d74      1
     63 0x3f8fa3b3466efa02470bc418b2d307d06dfc9c2d      1
     64 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     65 0x415172957acd7002feadc91bce0bcb7deaaed868      1
     66 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
     67 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
     68 0x46105de145ceff841edc1382f3e9d6836ae4a609      1
     69 0x478bb542f7658d635abba67edb987806dff5b83d      1
     70 0x48b56fbf752c5d4758e01694c19bf1b4dbe03a55      1
     71 0x49140e8e60913110f2e37ffaf4f5231a03611e3d      1
     72 0x491befac53aac201e97cd6f4c750e4a958d22d46      1
     73 0x4cd5dde5a1b632735fb19667a7c871743c00ba59      1
     74 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     75 0x564a544b7fbb31c976b353dff3ec4015e0871619      1
     76 0x57db311f8b2076bdde5e0fca74973204ae20d59b      1
     77 0x58b3492976b3273ee7d256ab8bf7f9338f3b37fa      1
     78 0x596558fd287175afa7ff695f565f65e38547b0e6      1
     79 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     80 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
     81 0x60136487f2526531d8ca1a0f9d6fdf7f5220a02b      1
     82 0x60322b225e06bf2f0c36c45d0b06090e891ca91c      1
     83 0x608a48fe1a4990315af6b0c016d1f0d2f582f7a8      1
     84 0x6112083d4bb12dbf64e4850842685aaefbeacb10      1
     85 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     86 0x61f5c63f6d56c0c7a83291aef8385f5de22efc58      1
     87 0x62d3c2a80d3cce916b9b645cbcda14d35418275c      1
     88 0x62d4377141e0a4a6eaab3167e0ffa4bc037db3a5      1
     89 0x633fb030794b8508cb27fb1ea6b45df95bddbbd1      1
     90 0x64084d818a908d809d0754dc09e32834c048342b      1
     91 0x656f6595a0c8a5c4e5d6b193228aacfd7cf55c47      1
     92 0x65c6f38e1f6687c64dfbb18c5a37bbd6f6ad0692      1
     93 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
     94 0x67353d0d86237b69eb521906f7e05b15b7454c63      1
     95 0x69cb9f5c9065e8fd858497e801d4b085638aa7ba      1
     96 0x69e67aad5495a48c4ef3f770d93a99935e1dc90d      1
     97 0x6a7928165f5ae63df374b2e6f0e8fe0a0a5ea042      1
     98 0x6cc5fffd0ff76b485f99dad6d0ca9413b9667114      1
     99 0x6d5c8f445936aefa1021c6d53e86c4ad5545e48d      1
    100 0x6d8ea9bdb65412df77ac68d747d4c580107ab376      1
    101 0x6d98c5b40356a3189298df88fe2d7108fa6265b7      1
    102 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    103 0x6e8a8f55a81b3f1dab9831fb44b2cf2bbc447977      1
    104 0x6e91e00fad12e30878a12cb9bf121805a994cee5      1
    105 0x6ebe7f9cda8619ab2c508ba6c8f6675836185e0c      1
    106 0x6ecbc32217572222b476a172677f8aeb920639de      1
    107 0x6f278ef400d31730310e321a39ce9acc8de00624      1
    108 0x703b244ee82ef3ba949160197c343b48e5d04908      1
    109 0x70ddda43d4f616263b1db0ec672cbd8795db13b1      1
    110 0x7343b9d4a3a00f92e28ad553809f5f23f4976c33      1
    111 0x7485e327bf1d82e1cece52a5f821f1632ad23abb      1
    112 0x75256a46e98eb0f9c4eefc197eb3dd88d559a771      1
    113 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    114 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    115 0x774cd866ceadf1871efd610ad30603ffb8034ae5      1
    116 0x78641a82415044490aea56f3a2c07d263d1941f4      1
    117 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    118 0x7e2f9c4193f0bad1fc36abccc713137d0954014b      1
    119 0x7e45d8d82713396f3853031a957b18034dab48a2      1
    120 0x7ebc6689843ca9212fb945830fdc248c436944e7      1
    121 0x7f54ad2ff488cd59530c8421e87d789676fb3c3c      1
    122 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    123 0x819dfc1b757c0ed67fea97e33e70cc4cc640f99d      1
    124 0x822f0ff30ad18d4ff379c39dcba3e72a75e108a2      1
    125 0x82f23de5a474fc904041c357576afe53c30dd250      1
    126 0x830f459f98514e306cdab5380799a69fe2db2a7b      1
    127 0x86f0d6712cd1b59ae7d2bd7c7777dcdf3de252f2      1
    128 0x873427efac598321cd2019a0c4ca6f66bf957c15      1
    129 0x88d7d41e29612b643c445983c988d447d8ca37bc      1
    130 0x8aea4d2a686a82ae455c09e372920a628848f667      1
    131 0x8b5f371edff19761629a1ff2bb3ba0e1b0684f87      1
    132 0x8c99623cee8fa0bf6e7632c8f170c6467aa54daf      1
    133 0x8ef19d27aa64609419563d2745b8f0a0c6cdbf0d      1
    134 0x8f641c6e5a3aab5596f692881814965b892d3f84      1
    135 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    136 0x9045003fecbc67280240c07b0df300d64d09f7da      1
    137 0x9192683edea0264fda574ddf101a362207169e2f      1
    138 0x92058906c65bc9cf1b77f2f76de0687854cae050      1
    139 0x924449ea24fc318e4867e1773625ea62ef53513e      1
    140 0x93ecd5b996c4a3991b5662e43072997bc46a235a      1
    141 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
    142 0x95d50b3edd982dd43fc51dbf68d091904c1aa066      1
    143 0x9649055a504f627b009c45611c0c1435c49f2d5c      1
    144 0x976828b23af10f75e94133b15f31ffb054572c7e      1
    145 0x980e22587fc29d25cf2c82baa6933dc0f73c09c4      1
    146 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    147 0x98cf0978e7144ce0ce8e911449d02b9c56231667      1
    148 0x9960937e6e9f93f9b94398ad75e9bfa2acbfa68c      1
    149 0x99a2efeb580b32c8de7b9aa56406bc05a9333557      1
    150 0x99c46ad6e52d196d04a795ba970f72ada7f777d7      1
    151 0x9df2844244ff3c0be263d2e09b32f3311eb77b1f      1
    152 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
    153 0x9e976c76f7441c916ee8f40e3965b41a2229f785      1
    154 0xa29f7d70c623e8cef9989d7b063212fdeb3047f8      1
    155 0xa3599cf1048930c9e897fe9245dd95511cd16f1c      1
    156 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    157 0xaac709a19274b53a9ce099e66c672422c019df7d      1
    158 0xab0ac99d34b23dbfd106f539bd8a265275f28c87      1
    159 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    160 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    161 0xac896d4906584fe138a3b4edfbc55570d0f0470e      1
    162 0xae93133a5c9e86336c4005a04d6150a52da4854c      1
    163 0xaf4c0da9c2d9018dcc55b8cbfb96fb9348b61765      1
    164 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    165 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    166 0xb3b16685ca2d2a764882e2bb2a8d3d769cd74145      1
    167 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    168 0xb407763f587f5f8726dcc9eeb7a72c402117016a      1
    169 0xb45763c75556484af461e00f0f3e0954ed60b25b      1
    170 0xb5292bb264b19eb7fafd054236ee2f2ee4509d8e      1
    171 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    172 0xb70d0906ab07ae1fb9283fefcf3df7d499c0fffc      1
    173 0xb77b1fd000e21d7e831a17e5c146e0ffe5ece440      1
    174 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    175 0xbb261e9c522d826886cf9d03b200e268a7ca36cb      1
    176 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    177 0xc01c99b2543e28eff240e90384a1fd757a484927      1
    178 0xc02d1eda13d073bd2c245f0a6b5a2aed9ddf5c35      1
    179 0xc33da4e7e5216130d90af5a4292db6c46ada41cc      1
    180 0xc4dad120712a92117cc65d46514be8b49ed846a1      1
    181 0xc50c43b274f264c765b645214a266bd809b25e2d      1
    182 0xc5611f5225f835f066a7990feeedfc2bdd07d554      1
    183 0xc6aa79a22b08c794b25dc58cb36f2250779cdf4f      1
    184 0xc6c5ee2c54c79695ebef26f3171e5b96ed74578d      1
    185 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    186 0xc785461e7bd4726e920c3103683eef2ec51b4efd      1
    187 0xc828df18d5a55009ab502137a64e9d77808a5bf6      1
    188 0xc876b346d50c4199458ee0e3754883f62fe3a5b0      1
    189 0xca9bb916074c6a51281a45cb9b4123a41f79cbac      1
    190 0xcb90dd8a79be00ca9ab62284978cf881c2899346      1
    191 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    192 0xcdb9c642d4e7385d52c5eb6cb9d32d536e12750f      1
    193 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    194 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    195 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    196 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    197 0xd53f450865fc6ea2d9bf38231aac358eeb137c84      1
    198 0xd54066fc71914e6f6890cd49dda28a8755b3cff5      1
    199 0xd5f6812971692117f581796147d2d3a2a579737c      1
    200 0xd6d9977522c6f4126ec407de2af999c29e672268      1
    201 0xd7b83c30609db3f1f4ad68d9c046703a7d06d722      1
    202 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    203 0xd8d5b0a8255bdb366827bd729db67b12432ac6ee      1
    204 0xd8e769a87ff31beffaecdcebfcdeebd8f2a5ad98      1
    205 0xd9854b1bc87d37d295cc402e6cba23f9a4cd7dda      1
    206 0xda834e5743899eea822f877d3dbf1dc18489583f      1
    207 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    208 0xdbeccbbbc202a66ffa97f0bd3299a37485d7e7b0      1
    209 0xdd5f934edbcfbdb25ef5dd9b8a3868d3633de6e4      1
    210 0xdd787d1da924e95281aba822a8baa1c6c081e2b3      1
    211 0xdf98322a314921fc6863806dae62a157081b0a5a      1
    212 0xdfc88dcdcaaab27d50ed9502c74a5d6b3b2a0816      1
    213 0xe06538abfc043aad711ae7894010b74a44be92e4      1
    214 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    215 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    216 0xe49fd99292983927398cab7e932cf8903eb12ad5      1
    217 0xe5678b3a07c101fba71ea3d44848799eff0d6cf8      1
    218 0xe626cddfb9773177079a47dc46c603c107e7c2a0      1
    219 0xe7014919c80cf68b64050290ca6d4c685648d14b      1
    220 0xe72c4bea4b5f5caabeaca6cd38894f8ede1f2e16      1
    221 0xea1126e70185400a2cf0de2a6b35428fb8affd29      1
    222 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    223 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    224 0xeb6d2870e0b66dde3373996ab5abf34c8395cb95      1
    225 0xed076561823c6797680e1167ea0f33c71a9098a5      1
    226 0xededf4df6bee95786ca56a04f2cc7ec7c8fe35a8      1
    227 0xf035839daaf652f03156b297a27eda25a35e6316      1
    228 0xf19aed29a6cd4f5a1b3c8ed79d987e3fc1dca853      1
    229 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    230 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    231 0xf3d834c5c5189bb0b4119dc7b366910fdd6bc81c      1
    232 0xf4dbede0aa5d580b7191acff0a40a33c885f69c9      1
    233 0xf5a93410e7e32bbf28a8eaafbd7f241cf0b290fb      1
    234 0xf5b05e9e1ca1d622f84c3cdeac27a94e3cb6d978      1
    235 0xf987a65115b267bc9a16f0eb52b7f7e94e554cbb      1
    236 0xfa4e98a3973ac6d51918e69d49f749219518c5a5      1
    237 0xfc167e6d3aaf26fd8e13a8c1d719fc112af23e89      1
    238 0xfc1eeb7402d9a1179d43d6f97a2a907a7262f551      1
    239 0xfc9e36803438104929d54637c23b6dca391dafc8      1
    240 0xfe91bf57cffcba664167082e73342e1666fafdba      1

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
