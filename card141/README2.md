
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot2.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:12106       Length:12106       Min.   :1   Length:12106      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:12106      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18068769 # https://etherscan.io/block/18068769
block_hash <- "0x654d54ee927d212fd2b383841362184dd22764e11e6c829e7e04fe2e322ae13b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4457 

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
      1 0x022d46788c21bc8d933c745387e107c9fd9aef95      1
      2 0x02d2731dba8769765bedff6272f326b0d00506ce      1
      3 0x02fbd51319bee0c0b135e99e0babed20df8414d2      1
      4 0x03b367733f46743534c9bb13bd4fb5ae2780babc      1
      5 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
      6 0x05503136f1063e9669764e10c1700ef6a910070d      1
      7 0x0578aa0595e0c83530dcde1d7a339479bfe6b0b7      1
      8 0x057e2a10b1e72eaebbc7edf7f54fbc3762f975c1      1
      9 0x0594ad5995af0c6107ae1c031eefebe3eae42c1e      1
     10 0x064a350ef26abd06630814a0e22498195ee85bdf      1
     11 0x066de4fe6306c3b1fee7654d869e5393953df4b5      1
     12 0x0720bcd1240479659ff933e620a89aee5816bdb8      1
     13 0x072c3ee4e4c5e4eabb7f484738797fbf260f056b      1
     14 0x076d40d4453a72a3dec44970b1529baee379dd0b      1
     15 0x08a9ead5bd9af49a1f777a9f15929871abd684c3      1
     16 0x096dd05d5e0abc910d41e080562e7f22b30b1864      1
     17 0x0a689d7687f20ad37918291fcb0160452fcd4af0      1
     18 0x0abd672a6f2a3e2907ee3d8ac9744670c04df8e2      1
     19 0x0cf7e52aafec849a4df31054168bc2b1a39bc316      1
     20 0x0d49acf3910e9592aab4b2d06ad16613c97e0c80      1
     21 0x0d6ce4f0e1fa53af3f624e17da5f47791dcaf70c      1
     22 0x0dcee254468df83cfe9dfb2236bf253459cdb079      1
     23 0x0f3c600960b143d46c70b0ec1d9da818a7208f9b      1
     24 0x0f4b8d14968ef8e888ae1462cae7f0dda2828954      1
     25 0x108289ecb4830e7de08dc8f9e58526ddeccd2d32      1
     26 0x10e01e2c2ba427d6ae509c085ac06df79b85237e      1
     27 0x1236b147e44366e6b954b344cd9afc72bf71b34e      1
     28 0x14baaa573906c24ec9dbdbd3d092fdb869b2d0f4      1
     29 0x160a588038a7284760ad05cea4466d2774d06c7a      1
     30 0x16363a953016b9b941da29a69f361f2b7a28dd40      1
     31 0x1756e785c364e30031e90f34e5d3b5aa4c83c1a6      1
     32 0x1844247cb5320efa99b6804e73b5cae8c1181768      1
     33 0x1884ef1ad5254c3fe32fa0c0395eb4fa7316c8a5      1
     34 0x18876acfaf008b85429a4ca223b8d8f33ebe7867      1
     35 0x1a3bfc4ef279975059221e5285db047905165a5b      1
     36 0x1b5dc7f6a12471bd4f6f584e648edd8fcde50512      1
     37 0x1c38f0d96357168d8d13afdf8225f55f743bc4af      1
     38 0x1ce4d8f0362efa9d16eabd90a2e6eb683e7d24e0      1
     39 0x1d3a9a01d1476a1eea9e230cb5ae4ed3ccf0c041      1
     40 0x1e3a1348881f6d0a5e1cdc6bf19b88d98399b15a      1
     41 0x1f3fd666b44d0f68f50012470595ff36200908c3      1
     42 0x20091f925b2dfe7e9da692c4dba8b6f39190c6d7      1
     43 0x20251a0505ead51fb2c6ce5c1f399924ea068322      1
     44 0x2355a3007f75e9f5f96070f6ba1345a22a8ca1d3      1
     45 0x23830f2c4a3ca2363853964f2a449d13339da888      1
     46 0x24ed7409cb9f1adbe5b83c86a237049da6eca675      1
     47 0x260c776f721e6281e51f21aada74f6407ccda236      1
     48 0x266b2d881291c5bb36cf386d16e5f8ebb8e19a71      1
     49 0x2706c71bf130a8bc3ec03002dd1068f7248052cb      1
     50 0x2759c9fd93e494c366e90eab777c7838de6da822      1
     51 0x27d4fee2fbe69bc02be96d1e146abe0a29797424      1
     52 0x27e37590cc23bbaf90a53cf2e1afa70c285c1fbf      1
     53 0x29b42b0c9c7ad2f487a7fade8999c749f872d025      1
     54 0x29bca487c390fa4773fe10839af969e13f2e5844      1
     55 0x2a00f63af45627ff351549106ea735bd973aa86e      1
     56 0x2ab173ada010a29de3c312edc0d6d39608c69697      1
     57 0x2c6f446d354199c635be737508423b4686c8c751      1
     58 0x2cae0ac9a7a7048516868aad672c49ab632b38c8      1
     59 0x2d671d4b065af46c342a4cfdd097b7fd1f5b9327      1
     60 0x2e89f8b69e755b0e6b36fde2d06d98252e10036c      1
     61 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
     62 0x2f957f77ed4314fa588a11bfb0f1e77060a9c71f      1
     63 0x306d822df1990e74d4841e349057d69a31ff0f24      1
     64 0x31d173ea66fa8fba38b3ddb798c1c1098f3ae8c5      1
     65 0x32a348ead7ed30d35b96b9f87320fef862033574      1
     66 0x34fd55a860f1df45bf006a81c2c3136f48254622      1
     67 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
     68 0x36925330dda72070aa7670d1f1505bf77e25643d      1
     69 0x3753d9da9bef05c0f51e5bb4dfa390cac639073f      1
     70 0x375ab791287811a5527a54df9d7de8ad3ef4ad7e      1
     71 0x38140b7f622e5c3c2f11d4531718b6ed892bd235      1
     72 0x386db8a4a2659cd4e1f76d3162c45e212b63a3de      1
     73 0x387a6107fa226e296710707bac98b718f805ede9      1
     74 0x38c3cabb114f9eb5393ebf28d8473733121801b7      1
     75 0x3932fb6453fe830eef57b77f483837ddd3fe3f3a      1
     76 0x395f2502a415bca4a2f84d73bb8929182a5dd252      1
     77 0x39dfeed68d0c848f3927cf8d01663ea639ec5b3f      1
     78 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     79 0x3b846d4d8823fb12cc3694a2584c5c32057808c8      1
     80 0x3d0400db7e6b017c05078cc07ed31ec4963fa994      1
     81 0x3d5ee44e4fafd6af43ee28269af157763d6d33d0      1
     82 0x3db28e19b63cb8b3c257d37f56bf0455faf11e0b      1
     83 0x3e0326556146dd34f4b0442989a48fcd87f0ffd2      1
     84 0x3f7dd0b106b10d06bb4813715b4e24d9e626dd64      1
     85 0x4209859ae2992f581d2678392b4ac80c13d5eed4      1
     86 0x422f3eac277a5f3adcafc5d4054c04ae35a18ec4      1
     87 0x424060738dc814b16c203650ba7e1ecffc7e504e      1
     88 0x4303f832a6a32567d8ec01a070b58d8a4f10e025      1
     89 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
     90 0x4382bd0d6800d192826bb040e5c5057375d5aaa4      1
     91 0x455867c5b24bf1a29ee02e0195b9ff36bea17ca8      1
     92 0x46cfba60b62b1b2feea4c45b1961214c05083582      1
     93 0x479b537c591877b7b6f7a084e9b07d90c4bff688      1
     94 0x47bceed9670a928ffa1540d048a4386ae11ec4eb      1
     95 0x47d9e2c5aaa56bb47b4ddee929840b11be55da77      1
     96 0x47f4f63b5760613a37acc9a83d20acf15d45e979      1
     97 0x48ae825591a926da5f49aca43608f28fdf37210b      1
     98 0x4a8169774f741170642e9d6877f47655b0fa0525      1
     99 0x4b75b25402eeb4e0c4c87581f9f8201339a951e5      1
    100 0x4bf57c3b1396470882fe7f448a7c9c16f25960ba      1
    101 0x4c85ece5bbf191d3e1b76934c8372d4ae3bf6927      1
    102 0x4ccc0aa065a37a3589e2db6e165d2f8f522e9fa2      1
    103 0x4d42d679cc113ff293f96e1b820209c661ee32e6      1
    104 0x4d544c95f86ab807e25c7c5ecd0ccfccdea9e51f      1
    105 0x4dfd1688ec88704e0d43976c58c34dcdb12ab55c      1
    106 0x4effe8c926e295467b9fc7b23048b72174638fc3      1
    107 0x4fd217f4d9c70e7ef41dfd549c7ffa9c14358a52      1
    108 0x50b3aee4aadaf115c4274bda7b788572d859a9b2      1
    109 0x50ed078be051280de17e724dd06f375a8cb3f020      1
    110 0x51787a2c56d710c68140bdadefd3a98bff96feb4      1
    111 0x5200fdc6d083895d7722794d8d002a3f04943a90      1
    112 0x52524f21eb713231df10792091b7e783c6a30ac4      1
    113 0x5286bc17220d51a36e55f5664d63a61bf9b127a6      1
    114 0x5322da29ca29ced6854f4d222b2a4164baa64baf      1
    115 0x540e16d0e898e45850ffa5bbb4d117d5aaef8f0b      1
    116 0x546aa1022b9840d3e1b2c89b3c679093a476fc0f      1
    117 0x547a2e8d97dc99be21e509fa93c4fa5dd76b8ed0      1
    118 0x5515de03f84c930cdae230de1e98b658d56975c7      1
    119 0x573369473b02d0b4fc9f3556184b67705512d26d      1
    120 0x5b975e28fe5f763dea46246029147e83d69c8e4b      1
    121 0x5bc928bf0dab1e4a2ddd9e347b0f22e88026d76c      1
    122 0x5bf6681a73a33a076c5569a4e729f41ae20bd901      1
    123 0x5c65c8adf2ef1cbbbc391bf39f4dcb72f07a539d      1
    124 0x5d9431fcd504d116ba2e636a1e0c364ce874b0c0      1
    125 0x5ee559349f01e5032324d6804d9ae4fd89041795      1
    126 0x60f55d4b354878205f15eb2804653c696f4dc8bf      1
    127 0x6194bda4590bbdc6a128851aaaf34802dfa8e4a1      1
    128 0x61bec61b20631a53d14b176c83b8d47fe711a6cf      1
    129 0x61c9740e6ca15c78959f50d5aa579b9d27e71d7f      1
    130 0x62247532f72dce05d7bca9f1a6d778c91a00054c      1
    131 0x6269a98ae8cd1099eaab82ceaace4d84f8116347      1
    132 0x64951c5e4ed6ab0ed319720fa3a6cc32949cc649      1
    133 0x656e08efe4fb1433781ffdca8411458ffd8575c1      1
    134 0x6582a0fae4931833aa1f663785da13cbfcb90ab6      1
    135 0x66477d6f62dbd802de7196d052c650731c5bdd10      1
    136 0x66fa6d3aa74a9c2b0bd1676747dfc2f6951c936f      1
    137 0x67744f4b07a3708da6a2362739cc0872e81a6555      1
    138 0x69a316825819a86c9479c4b18fcec8a59ade5d87      1
    139 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    140 0x6a6d335d1ca6c4539b7cee6476b706685f97bc78      1
    141 0x6d211a3b22997b7b0107364e120a03372267eb52      1
    142 0x6dbd64d9c1aca3a5041732406eee7f75637211c2      1
    143 0x6e3258ce4ecbb70e3cb18692d24e82b675d2e2cf      1
    144 0x7024ee7932e4d879409f9618b560cc82cf093a7a      1
    145 0x721540d874053b14f3c5e421abbe7bcce008456b      1
    146 0x72791f129f58ab697e3298ae044235a2e3f4bc85      1
    147 0x72a80114df86c9e16eb6e4f64f27a4c5c06e8759      1
    148 0x72baea3565cbda3ce3fbf7a305262964e918185d      1
    149 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    150 0x76adc7dccfff0c41e02a1b93a22cf269bb40cf46      1
    151 0x76c9d075f88f7915eb1ab5efb3846838b59e2db6      1
    152 0x77e8001a83e5175e36b0d80c11fe8b5a65ab5ca3      1
    153 0x78941100d90c5b66b05b52e720549e8d33e89cdf      1
    154 0x78c74a90e80b4d93b911f979839cf764be00b4d7      1
    155 0x7a6968204542f1a45af3400e76c32dc21894f334      1
    156 0x7aafb5a543c1171405f1ab67451b08b8ba33b2db      1
    157 0x7c6b066cd8538687f8719397ece8a038573ef54f      1
    158 0x7d656ce2a065b331abd295d07f466f6249ac7231      1
    159 0x7d8b981b45879b054114d12912438dd17fca417b      1
    160 0x7ddae9afcdf981729b18ad3ac0fcef50d9662c24      1
    161 0x7ed747d46323a4055fb0b89694f34baa945e9fae      1
    162 0x7fcbe04eec78622da899bd141d7e496e7624e227      1
    163 0x805c36f06e1f1233aa9617932431c425d5385abc      1
    164 0x806a69dafa134654ad8ec2a6da619c94695f5ba2      1
    165 0x82c09bfb5f206920539112194567973b25db4821      1
    166 0x82c3ae7fd3f45f553ae130d5d13f88bb46fcc2bf      1
    167 0x860e3777df77395f9e22c5d36a37f063dfdc07bc      1
    168 0x873440b72f96d02cf2b80890f111546416b8aeac      1
    169 0x880f450841e67c786ad3b0b3644b26d2680b7c8c      1
    170 0x884befa765a966508cb0bd8ca36e300a4529f380      1
    171 0x886c4d1d4bc99399a42ef9cdcbf0f0dfa700a860      1
    172 0x8880b69c95c0183bd3d4f30bc4272e457552c3d2      1
    173 0x895f4d750369f5b28072521eb2352b7b808043a7      1
    174 0x897996600a1177dac84899e3d8d3f04d1b7db82e      1
    175 0x8a4b89d76a1a745a4a1adebd3793253fba0adadc      1
    176 0x8a52affafa6b15cf1496babad7d6d84129a4764f      1
    177 0x8a7c07154908a1701f61ce2ee1b81fe4eee4216a      1
    178 0x8c1321c782c169aa93c7c211ed92956e84d2f3e8      1
    179 0x924a1fe275c0ea466c519830719c8d216c415e19      1
    180 0x94345607093b249b5100e8d1e499928dc163dfdc      1
    181 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
    182 0x95649f108393a38d182e148f0424c2604cda8cc9      1
    183 0x9572001a64ad0a42fb908429dfa0214a3c18079d      1
    184 0x959cb1897c18d4be61f365eb25893e0cdbb21401      1
    185 0x95b5e520e93e92954c6100272696b0e6cf8bc013      1
    186 0x96dc89dbe84970ee42a2f0b35fb50599e6745ff8      1
    187 0x970324b1ea88c20a2f7e6c557409cd91222409b6      1
    188 0x9825fea6d8f03447e58844643024281846dfb2e6      1
    189 0x988070d1060fd2c826ccbc9b1446296a96fa75ad      1
    190 0x988ce7e0aea652381dd51f48279a02110aab9d59      1
    191 0x9bafb60dcc082e472b6a45eea5a6c4775d53fc86      1
    192 0x9bc2879384ec9543718c93bdc769050286f91281      1
    193 0x9cd27b1142ca7dcf60167355791a4089463347ae      1
    194 0x9d9103b2951450fad4884c0f19a18e5f9cd4171c      1
    195 0x9e175101522a47aa9f79669994419f52f5d629f1      1
    196 0x9e7a5b836da4d55d681eed4495370e96295c785f      1
    197 0x9f49288c4658660c82dd98019f600a3d35969fd0      1
    198 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    199 0xa2daccbf8cf2243c724260cb72972d206f2fcba2      1
    200 0xa30cf7f52afa4492b1454f8f8c38e7f10618f1bf      1
    201 0xa352bc969fef8b6938d74b388062f2220ae437c9      1
    202 0xa4f0670468dfb825e6c2b5571e2c97d0844190a3      1
    203 0xa52f056c6b8dd39759fead233103a14ddee4be55      1
    204 0xa5e6b86b278ae2511518e0837047ae9595777434      1
    205 0xa60f18b7653d363f12a263de60c4d03e12159d15      1
    206 0xa66e796b33016963bf6eaa062fa81d4254f33519      1
    207 0xaa1de591879f7061676f4831c2e9d89651f7f6c1      1
    208 0xab6ca2017548a170699890214bfd66583a0c1754      1
    209 0xab8d0a6784ce6b16da622c9a3a63f611c5f24cf7      1
    210 0xaba2407532c89b522f9075c7676e1fb94d664103      1
    211 0xac0de81404e464c2f6ce1e981bc640030df61bae      1
    212 0xacf5b55087ba2767e96c487b9a007753f7ebb66c      1
    213 0xadffc7ff3e937e3d7a34e989fac05fe06cd0fc99      1
    214 0xaeb23fa327411ea08495d7d695aa147a27ebafb1      1
    215 0xaf37d6b9bfe0f5e4954ba1f2d0734aac32b2ae34      1
    216 0xb0115095f99f794f15753e1eed762631578b5268      1
    217 0xb0d26b3f358a0d559a714095a3a7778f74acbec5      1
    218 0xb13e2b1fcd163bc3705b1867487e9617a970a062      1
    219 0xb22db1dde941a13fba352d56d857a26a5eda1cca      1
    220 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
    221 0xb320826ecc718506c5d86f14494da398900ad360      1
    222 0xb4f3157f77fa1903886b3207140f6150f35ea7f2      1
    223 0xb66f89259545c7a323832833e8ab467297329b75      1
    224 0xb783dc0696f93ec74fd9214b527278670cb5b09e      1
    225 0xb7d032cc77919e17078a172ba98af27925a4a03d      1
    226 0xb8ada1b0661d6a177d3af6cf318f7ecc4ea24e70      1
    227 0xb92338347d4ce5b0b3d20d28985e458908f43d52      1
    228 0xba23c2c327b1a67cd812e8ba1ddde24d5bb6065c      1
    229 0xba7ddd1aea2b2af6eea96a1ed68da42fbe4b6c37      1
    230 0xba83d8ef99b935b55deae87e9623b4c5d5baef88      1
    231 0xbb6b62e63ab3fbd3e78a4c9cf61488482623e9a4      1
    232 0xbe9d7f6dd755875cc1e8203dba92c92e871bd9c2      1
    233 0xbf1be8861bb200002f1fab3f7482bc85e4110e24      1
    234 0xbff456923e74ed77d9e8acc2a7d9ff52d8c67bf6      1
    235 0xbffec59f5e85f37af43a872dbeb641cd18921f04      1
    236 0xc19ca6cc85de33ec664fef9595905b8e57dae13d      1
    237 0xc2079e5b077eae969dbeca3747bb855ae9df8f3f      1
    238 0xc25424d003da3d80024e1ec27640e8fa82fc02cc      1
    239 0xc33afb2af5e7c44f30cf9b121a01baee2965bfd9      1
    240 0xc47e595b445844916c4648870efa1736e4844e1b      1
    241 0xc51f6b8ee3c35a718d50aba5dd86683e1fe8217e      1
    242 0xc55b7614fdc6f11d649d03420262f91db1181be5      1
    243 0xc7c6d5da121a293f148df347454f27e82d6cad7e      1
    244 0xc8e871fe50cc7408a15793f4848891456091224f      1
    245 0xcaaf72105cb8dd7a1234988a635e243e1621ad3b      1
    246 0xcadf672af45192a94c97ae2c0e5dfd2a74a8ab1d      1
    247 0xcc3e5f175b24664291afa6e9d551117f05fe8f20      1
    248 0xccffaf47449abebfd00a55aa34eaad5752f6f32d      1
    249 0xcd38ebb19044c07662c0569e3d4a5e5dfda1b9fc      1
    250 0xce08b02b4f7288c69131dab5b97364df42938cf7      1
    251 0xce27b1362f6a109d063b6c7e81ac9e30cc79d547      1
    252 0xcf01b135efce6bced0e2b8ec55eef6380dfef07d      1
    253 0xcf4f27a00e789b7919c4ad61c03c796794908962      1
    254 0xcf934852f4a8048b60df5596ed2baa430a99159f      1
    255 0xcfa1664c628f75c4f58f6f4a1eba1bcc0f110c4b      1
    256 0xd0157b9ebe373975cc9bae67e4f4b92e178f072e      1
    257 0xd070ada7f4a088444e61bbfc1fe6a31d87ea0e92      1
    258 0xd12f477ad6cfac95a12e08f96f06cdacf23c6643      1
    259 0xd1f3a51bde35942789eb3b3ba205fcdc340c8552      1
    260 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    261 0xd44e9b676e74ae45c0a39150be771eb189bb2337      1
    262 0xd53de8084e9316323409277afdeeffa8a96d188f      1
    263 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    264 0xd64b4f50c8d118f6dd33f16846345616ab80e16a      1
    265 0xd6871d2ce0977460ccabb6483bf2b2e23e68ceb5      1
    266 0xd6ce702632f94069e138d30836194b121f047e79      1
    267 0xd72a01ad4b5005c3e47559861b0964f5c28912bb      1
    268 0xd732748071e38a48b4e9a3ff1b2e7e87c0a39c2e      1
    269 0xd78c7aa0455e78e5305ffc4a672b419b448ec816      1
    270 0xdb897d2dba51155735cfc39b79f3c47380efee6c      1
    271 0xdc8d581cc9ad83d61e5f5aa9d998bd325a4b8b06      1
    272 0xde64640c32cc89ee35e1657e02c4b0100e838fcf      1
    273 0xdf5b0e3887ec6cb55c261d4bc051de8dbf7d8650      1
    274 0xe1e71e8b336814014efc951e9cbd9432b2e7bb8d      1
    275 0xe254f96cfd8b768de31013f8bd6c3476ee2f1469      1
    276 0xe36b14f7a3fd29ac8befa667804d7dc0070d9177      1
    277 0xe4347d34d8c0c941926e84b56cb9dfcbcf4154fa      1
    278 0xe552c715b14c5235d8c2d2277fc05c3c4f3217b0      1
    279 0xe66e423f39379ee3618d34af5736e07e278c79e6      1
    280 0xe7eb7bc80fb29a04be374599f342d0c381e4de2c      1
    281 0xe7ee5a3a7e0c9114a0d39f53619bad9ec7466068      1
    282 0xe81d1697e807a2a9d42b9cdd5e3986ebc09d485e      1
    283 0xe8df91ff1ce483ee7827ae58b02975d88611be0c      1
    284 0xeaaeeb152d21122a8782bbb046edf48d0d9a389d      1
    285 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    286 0xeaffb626014495a3331ed7539fd229893f57cb15      1
    287 0xed4a96ec302c7588bfe84c13888c79ac8a321dca      1
    288 0xf3d7187fec14d1cc2b10ca84916b7d86541af846      1
    289 0xf569e5fea87cb45da33ab8246080c73c50ccd950      1
    290 0xf5eb53e83a9fbe8491575b1174cee2f2989a43c1      1
    291 0xf694d1bd527b09e030851062839d57a44a5b565e      1
    292 0xf6ad2baa24b7be97921dd359eae00ed0d8a1a5cc      1
    293 0xfb81c53b90942f80a637d59207dad341d22c6df0      1
    294 0xfbbc953b46ae6ae37392999707f996a868d40f20      1
    295 0xfbf862d62b28c425e29c38091308cdd298424cb4      1
    296 0xfc9dd877930e9799beb663523bd31eefe3c99597      1
    297 0xff37108f227d9b1159fbc42d2aeab138310656e1      1
    298 0xff6ccb4c64e197f053272cf85f64cda42bf550b3      1
    299 0xff7a752057d8b3444c94347b7195d29384df4e00      1
    300 0xff93e7aa23280d83115aa513b16de7493edd0196      1

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
