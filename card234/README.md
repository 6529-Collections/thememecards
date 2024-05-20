
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:439         Length:439         Min.   : 1.000   Length:439        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.196                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:439        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19894669 # https://etherscan.io/block/19894669
block_hash <- "0x780b514de68019150f7bfbf6eda04d03922625649544e4a20db8e2b53739f7be"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4510 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","noiseeverywhere","MyWrong","StillLife","StrangeBehaviors"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("MisbehavedEditions","PSiloveyouEditions","imadethisart4uEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","noiseeverywhere","MyWrong","StillLife","StrangeBehaviors","MisbehavedEditions","PSiloveyouEditions","imadethisart4uEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 13 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1c397c2a361d3baadebf573b198109a368729f2c      1
     2 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
     3 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     4 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
     5 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     6 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     7 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
     8 0x8efceb29863fb0b62c73c6c9c23f10f24243ebfa      1
     9 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    10 0xafb905265fb0c005b4755660e7f69f5829f3c754      1
    11 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    12 0xc449f005667bef849261b35accf931a4bace48fb      1
    13 0xea7d3818a3b736fba40852cd90471df33c6d6821      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 164 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00e484da1156202e9dd341ad7ea9c908bb919e96      1
      2 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
      3 0x029acd49d3aabc08402c84367c050dfd52b6c566      1
      4 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      5 0x0361c7133371486373803ecbdee1be0073945df2      1
      6 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      7 0x077be47506aba13f54b20850fd47d1cea69d84a5      1
      8 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      9 0x0a5aded334cc0ddf815ba4fdb3b3747899d45b8b      1
     10 0x0a68c758f0bde9f2daf586d68412a0825aa24ea3      1
     11 0x0aaa763dfde0f7de2fbb69f71d6f8f15de9e22f4      1
     12 0x0d99e9a9cb81ef149ae2a1270a6c7a9593edba9b      1
     13 0x10a8f6fcec6ad125b3acab3345a6aedd003ab666      1
     14 0x1142184c427c843172c14025df52501536cafe26      1
     15 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     16 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     17 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     18 0x13904554a77b2af0b1eec0369834da0b7102a267      1
     19 0x184e1642e3afcd1f4fdcc584cc70f969fae3e3e1      1
     20 0x1968e029cb827943abdbe3b50204115b2ded7686      1
     21 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     22 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     23 0x1bfab2dcaa9aec8d03632cfd9318922b693814cf      1
     24 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     25 0x1e364eb88cbbabec80bcd7688f7059274f54a39a      1
     26 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     27 0x25fc4d1aa94883fa2c10ba1583cd154ba0a73007      1
     28 0x26613696bd07c7a6e43c94ea457584f1a5d9f979      1
     29 0x26d7b4fe67f4601643304b5023b3caf3a72e8504      1
     30 0x28ac6a2ab0ee19f8c452d830e5d34242ba302b52      1
     31 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     32 0x29cc37bb515745a75eb9ec13d2db1b7bc69447fb      1
     33 0x2aa3419db6758119e0e8bd4cbb867ff7f3ad189e      1
     34 0x2cec0c032323686737feffefbfa2eb9b3d075a0a      1
     35 0x2d06ec78d357b2cf84b7c00416051b933dab472a      1
     36 0x2dce07502002ecc96d17f948ed52f978edd362d2      1
     37 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     38 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     39 0x30da7cf3c16b85f694a6535b582c49e1d7bbde6d      1
     40 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     41 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
     42 0x3299b96d78aada4191e4aad22e585fb613f3b420      1
     43 0x336bd505b648d378a251f06ec3f6693620913f92      1
     44 0x33ba48ec6863152420e0ba6f0badb286f03e9db5      1
     45 0x3adde9c976371a06c95b70030b31fc37117241b4      1
     46 0x3bcb727562144f5576c2e574386b6a842d303c8c      1
     47 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
     48 0x3ebafda0f5379b0be49af9e0f8111280907dcc1f      1
     49 0x3f4ac5eacd9cb45a4ec9564b7a49f4950a9689db      1
     50 0x408a2b89232738f7a254d1d91df392a0316eb79b      1
     51 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     52 0x45b6ea1c32cf72466689c80c3e1f85056befd716      1
     53 0x45e86cd6d4bc558fc604eced6693d2854922f5ec      1
     54 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
     55 0x478454599df852e11524083605457875e50ceea8      1
     56 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
     57 0x48f1b764b8cd851025b88fa28f92cb855b9079c0      1
     58 0x497056b9e7832eeadb26c80724eb79d7ad0ca77d      1
     59 0x4b0fb3b2b31f80625cf68f81908996345c88d0f5      1
     60 0x4c6e348056b8d35075af1d92d521f9ffdea531e2      1
     61 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
     62 0x4fde21bc28732fea402ecd822692e6ccd53cda1e      1
     63 0x5290b98f59d82335b472139e296f546fbd3b8716      1
     64 0x54b525f1839ae10085de6affd0c549df1f1deef1      1
     65 0x5541987c482af91d0086984a0b56da45dbb2e1df      1
     66 0x58415cc885717052276dbaa4567584d73d6d3bac      1
     67 0x58954c42e77393662721830d0fc9086e7e6b6673      1
     68 0x63b63ba2fbaf4bb1a101db9e7de8a9bd719995f2      1
     69 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     70 0x668f1e8ad3d43c50d0d95bf621b36dad4412f202      1
     71 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     72 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     73 0x6e0dfe55fbf0c7a9902aaca535635bf86a03f1be      1
     74 0x6fb78d9c19d3b0ef9cc4e134c82b77a5beeae401      1
     75 0x756c87abe1784f9c4e568b794f182181873aeccd      1
     76 0x78ab8a82dd562d4498614ca81ae5fcf1b48d7629      1
     77 0x794316614b210acd02f7c88085f2872a8d657d8f      1
     78 0x7a1c5e9dc063a6915855247e96efab3cfc20ad9d      1
     79 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     80 0x7da10c719381f5eb07e2a1f875c3633f0e7c8c78      1
     81 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     82 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     83 0x7f6f76d22d2c5130f2da38d0691748d1b61c19d4      1
     84 0x7fdba82ec2360d4e566175943cdefd8443a77143      1
     85 0x802fa804b97ccef82331243feb6ba56c9b4281e0      1
     86 0x804604f55fe79ad0237696465c57d3de997c14c1      1
     87 0x8160f95edd83e0e420b81e32937b3424fee2b926      1
     88 0x852e45ed1a6502406af4c409df4e0eef3cc0a0e3      1
     89 0x85a2e94155e7165de6e105e5056c58c83dc657b7      1
     90 0x85bcb8a0809c5dd5b03939b62f692d5ca5e3c0fe      1
     91 0x89e9223e2ff8307ba2b693b7fc49aa559deaed86      1
     92 0x8dc436f8ccc74f01ad58516d97504e9c47281fc7      1
     93 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
     94 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
     95 0x967881c7aa824750a2764694366b250951b95980      1
     96 0x977034d4819b17b4592033f7adfb94a45cc70467      1
     97 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
     98 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
     99 0x9edeeebae83123deb3e4c837f476ab815a34b572      1
    100 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
    101 0xa3c88e140367547d11cff2912d00caa3fc442d67      1
    102 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
    103 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    104 0xa7db2cb984db795ae132a14523d9928472360228      1
    105 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    106 0xab5f32d395b515d132d5a1f1e3337f4f679d13be      1
    107 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    108 0xac2e80f93d507ab5ef326b38b8c7363e02bd2cfd      1
    109 0xaef0372934166586f9a59c111b14afe6302baec5      1
    110 0xaf0d424a053bd78b7e479e30de6850c5e0ae1b4f      1
    111 0xaf614b479583c53221e323ee6264a5efda63601f      1
    112 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
    113 0xb11efa4a9dcbdd5f6125494d90f9a8f998bd4a86      1
    114 0xb18fd1c3505f1305df7881ae353429796e6c5c7f      1
    115 0xb35ef8ccf63393f5eebe90ea161d395776739bb0      1
    116 0xb3e054cc7c9bc6ad762648a764a747460318d20c      1
    117 0xb3ea523a421604f27e91c12249de2512b3bc4c68      1
    118 0xb5f48c2201231e6398aa983c40d0592251ce7c58      1
    119 0xb7ffa8403e08e84db4305c169818917cf8791d8d      1
    120 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    121 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
    122 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    123 0xbbbc24fe3538ed5e6bb9b9c4fa0442c4c446720f      1
    124 0xbfc530fe1ec08d4cdd40efca5a419ba9094d68f7      1
    125 0xc117c92319376a0ab225b936f2eeeec8d387c951      1
    126 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    127 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    128 0xc64a2cc5bcca4b28889594f0715fead1574da926      1
    129 0xc6afa4dae41589a5da7b0b8307695025b88be339      1
    130 0xc7f354b172e53c0dffa8db17b028f634cd2efcbf      1
    131 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    132 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    133 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    134 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    135 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    136 0xcdf46a2591f48c25013483f0bb89119ac103f851      1
    137 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    138 0xd0720db051fbc63adda92464c408fa6c3e3db017      1
    139 0xd2b09dc390aa62da598cedd4afb3f0bee5ce69d5      1
    140 0xd49a5e5af2ca34543e1c8a11e12360cd951b71ee      1
    141 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    142 0xdbc30d47297b5360f5fb386e5791b6ca75ba54a9      1
    143 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    144 0xddd6578aaa0010808bb13b95c9838c440f8b682e      1
    145 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    146 0xdefc79316c9e12f2c5d82ab3bf10861ce38ce3cf      1
    147 0xe036a80f628e531982189fb6c4f277deca521e36      1
    148 0xe10772c3c2e8879b13d5d2886ef8e9f9b95b83aa      1
    149 0xe1678bdf494bf00464b6fee30591b8b4fc3e480a      1
    150 0xe322875fffb56f840456c7ba4742c125dd9cfd14      1
    151 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    152 0xe6468262b5a34057676586dc1976de5f664b7083      1
    153 0xe71775afbe475bd88d44e27f4bf1f94118e33e6d      1
    154 0xea13fc9b05806a76e478b82f6527d4f7eb21321d      1
    155 0xea4d8482132d15666a58bdbadb6c856eecb1b5bc      1
    156 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    157 0xeb1830e98bbd7f526c68bf4bc7916cbca0ec797f      1
    158 0xef136cd8d9bbd4746ed6172e93c7be36b53b6741      1
    159 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    160 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    161 0xf4405d30fc81e8bea085d2d1a90404bde2359453      1
    162 0xf9b55e4046c36a182457f87c95385d2437335bc1      1
    163 0xfce3ffd04de1ffdc60c1cc79a5127c5e4904c759      1
    164 0xfdb6dae2791921aeb1aaadfd0ce766225029e3e5      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 177 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00e484da1156202e9dd341ad7ea9c908bb919e96      1
      2 0x01b22bb2d473a53fae501c273e6d5592a58a131a      1
      3 0x029acd49d3aabc08402c84367c050dfd52b6c566      1
      4 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      5 0x0361c7133371486373803ecbdee1be0073945df2      1
      6 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      7 0x077be47506aba13f54b20850fd47d1cea69d84a5      1
      8 0x08493a92a95d4e93bdbc49f66e7647f1445a9210      1
      9 0x0a5aded334cc0ddf815ba4fdb3b3747899d45b8b      1
     10 0x0a68c758f0bde9f2daf586d68412a0825aa24ea3      1
     11 0x0aaa763dfde0f7de2fbb69f71d6f8f15de9e22f4      1
     12 0x0d99e9a9cb81ef149ae2a1270a6c7a9593edba9b      1
     13 0x10a8f6fcec6ad125b3acab3345a6aedd003ab666      1
     14 0x1142184c427c843172c14025df52501536cafe26      1
     15 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     16 0x11c03b0e147af0e527a7f8bdfec94987fc5c579d      1
     17 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     18 0x13904554a77b2af0b1eec0369834da0b7102a267      1
     19 0x184e1642e3afcd1f4fdcc584cc70f969fae3e3e1      1
     20 0x1968e029cb827943abdbe3b50204115b2ded7686      1
     21 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     22 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     23 0x1bfab2dcaa9aec8d03632cfd9318922b693814cf      1
     24 0x1c397c2a361d3baadebf573b198109a368729f2c      1
     25 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     26 0x1e364eb88cbbabec80bcd7688f7059274f54a39a      1
     27 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     28 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
     29 0x25fc4d1aa94883fa2c10ba1583cd154ba0a73007      1
     30 0x26613696bd07c7a6e43c94ea457584f1a5d9f979      1
     31 0x26d7b4fe67f4601643304b5023b3caf3a72e8504      1
     32 0x28ac6a2ab0ee19f8c452d830e5d34242ba302b52      1
     33 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     34 0x29cc37bb515745a75eb9ec13d2db1b7bc69447fb      1
     35 0x2aa3419db6758119e0e8bd4cbb867ff7f3ad189e      1
     36 0x2cec0c032323686737feffefbfa2eb9b3d075a0a      1
     37 0x2d06ec78d357b2cf84b7c00416051b933dab472a      1
     38 0x2dce07502002ecc96d17f948ed52f978edd362d2      1
     39 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     40 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     41 0x30da7cf3c16b85f694a6535b582c49e1d7bbde6d      1
     42 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
     43 0x31ad7d2bfc184007aab6737991b55db404dce28a      1
     44 0x3299b96d78aada4191e4aad22e585fb613f3b420      1
     45 0x336bd505b648d378a251f06ec3f6693620913f92      1
     46 0x33ba48ec6863152420e0ba6f0badb286f03e9db5      1
     47 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     48 0x3adde9c976371a06c95b70030b31fc37117241b4      1
     49 0x3bcb727562144f5576c2e574386b6a842d303c8c      1
     50 0x3dc94530601d9ba00a014ae7e0210dcf9960eb67      1
     51 0x3ebafda0f5379b0be49af9e0f8111280907dcc1f      1
     52 0x3f4ac5eacd9cb45a4ec9564b7a49f4950a9689db      1
     53 0x408a2b89232738f7a254d1d91df392a0316eb79b      1
     54 0x4212d149f77308a87ce9928f1095eddb894f4d68      1
     55 0x45b6ea1c32cf72466689c80c3e1f85056befd716      1
     56 0x45e86cd6d4bc558fc604eced6693d2854922f5ec      1
     57 0x461123fb3a23b5503c01afdb83b0e4387c98ed4b      1
     58 0x4730497622bdfd6eafe1f09fa22b3a0aca94a646      1
     59 0x478454599df852e11524083605457875e50ceea8      1
     60 0x47d8404fcd1c52e6642c7ed374fab052b381ccc1      1
     61 0x48f1b764b8cd851025b88fa28f92cb855b9079c0      1
     62 0x497056b9e7832eeadb26c80724eb79d7ad0ca77d      1
     63 0x4b0fb3b2b31f80625cf68f81908996345c88d0f5      1
     64 0x4c6e348056b8d35075af1d92d521f9ffdea531e2      1
     65 0x4c7f1354ae3d30f7a88c9d972ad7f96b44ce0dee      1
     66 0x4fde21bc28732fea402ecd822692e6ccd53cda1e      1
     67 0x5290b98f59d82335b472139e296f546fbd3b8716      1
     68 0x54b525f1839ae10085de6affd0c549df1f1deef1      1
     69 0x5541987c482af91d0086984a0b56da45dbb2e1df      1
     70 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
     71 0x58415cc885717052276dbaa4567584d73d6d3bac      1
     72 0x58954c42e77393662721830d0fc9086e7e6b6673      1
     73 0x63b63ba2fbaf4bb1a101db9e7de8a9bd719995f2      1
     74 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     75 0x668f1e8ad3d43c50d0d95bf621b36dad4412f202      1
     76 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
     77 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     78 0x6e0dfe55fbf0c7a9902aaca535635bf86a03f1be      1
     79 0x6fb78d9c19d3b0ef9cc4e134c82b77a5beeae401      1
     80 0x756c87abe1784f9c4e568b794f182181873aeccd      1
     81 0x78ab8a82dd562d4498614ca81ae5fcf1b48d7629      1
     82 0x794316614b210acd02f7c88085f2872a8d657d8f      1
     83 0x7a1c5e9dc063a6915855247e96efab3cfc20ad9d      1
     84 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     85 0x7da10c719381f5eb07e2a1f875c3633f0e7c8c78      1
     86 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     87 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
     88 0x7f6f76d22d2c5130f2da38d0691748d1b61c19d4      1
     89 0x7fdba82ec2360d4e566175943cdefd8443a77143      1
     90 0x802fa804b97ccef82331243feb6ba56c9b4281e0      1
     91 0x804604f55fe79ad0237696465c57d3de997c14c1      1
     92 0x8160f95edd83e0e420b81e32937b3424fee2b926      1
     93 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     94 0x852e45ed1a6502406af4c409df4e0eef3cc0a0e3      1
     95 0x85a2e94155e7165de6e105e5056c58c83dc657b7      1
     96 0x85bcb8a0809c5dd5b03939b62f692d5ca5e3c0fe      1
     97 0x89e9223e2ff8307ba2b693b7fc49aa559deaed86      1
     98 0x8dc436f8ccc74f01ad58516d97504e9c47281fc7      1
     99 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
    100 0x8efceb29863fb0b62c73c6c9c23f10f24243ebfa      1
    101 0x94aa50fe3c1ad32b0419004eee4f278ca3908876      1
    102 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
    103 0x967881c7aa824750a2764694366b250951b95980      1
    104 0x977034d4819b17b4592033f7adfb94a45cc70467      1
    105 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    106 0x9b848370417c9f525d9168f2aa91a2cd819240e8      1
    107 0x9edeeebae83123deb3e4c837f476ab815a34b572      1
    108 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    109 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
    110 0xa3c88e140367547d11cff2912d00caa3fc442d67      1
    111 0xa456e09a747488bb1c33b194cf4cb180d2dfe720      1
    112 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    113 0xa7db2cb984db795ae132a14523d9928472360228      1
    114 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    115 0xab5f32d395b515d132d5a1f1e3337f4f679d13be      1
    116 0xabe9acce140a2d338a5df5f08b6dc87366e34d31      1
    117 0xac2e80f93d507ab5ef326b38b8c7363e02bd2cfd      1
    118 0xaef0372934166586f9a59c111b14afe6302baec5      1
    119 0xaf0d424a053bd78b7e479e30de6850c5e0ae1b4f      1
    120 0xaf614b479583c53221e323ee6264a5efda63601f      1
    121 0xafb905265fb0c005b4755660e7f69f5829f3c754      1
    122 0xb072ca86404a0ca5780b01a9f3ac60205b3d8018      1
    123 0xb11efa4a9dcbdd5f6125494d90f9a8f998bd4a86      1
    124 0xb18fd1c3505f1305df7881ae353429796e6c5c7f      1
    125 0xb35ef8ccf63393f5eebe90ea161d395776739bb0      1
    126 0xb3e054cc7c9bc6ad762648a764a747460318d20c      1
    127 0xb3ea523a421604f27e91c12249de2512b3bc4c68      1
    128 0xb5f48c2201231e6398aa983c40d0592251ce7c58      1
    129 0xb7ffa8403e08e84db4305c169818917cf8791d8d      1
    130 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    131 0xb9076ac54d4059661a24ff1831e8719a98eb8fc7      1
    132 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    133 0xbbbc24fe3538ed5e6bb9b9c4fa0442c4c446720f      1
    134 0xbddb878b393bf91f8d34b2edf7087ca529e3ccaf      1
    135 0xbfc530fe1ec08d4cdd40efca5a419ba9094d68f7      1
    136 0xc117c92319376a0ab225b936f2eeeec8d387c951      1
    137 0xc15add7eb1ba708bc7189cef6a4c47200b77a52b      1
    138 0xc449f005667bef849261b35accf931a4bace48fb      1
    139 0xc4a843ddf533540474fddb7a9a03342348f6742b      1
    140 0xc64a2cc5bcca4b28889594f0715fead1574da926      1
    141 0xc6afa4dae41589a5da7b0b8307695025b88be339      1
    142 0xc7f354b172e53c0dffa8db17b028f634cd2efcbf      1
    143 0xc83bcd5fc46f61fe331bd63398f776adcb711f12      1
    144 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    145 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    146 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    147 0xcc7cc2e94273e694998ba0b3c80306cf031aedb7      1
    148 0xcdf46a2591f48c25013483f0bb89119ac103f851      1
    149 0xd01fa4427a05fd7944557ae94b67d90109fad402      1
    150 0xd0720db051fbc63adda92464c408fa6c3e3db017      1
    151 0xd2b09dc390aa62da598cedd4afb3f0bee5ce69d5      1
    152 0xd49a5e5af2ca34543e1c8a11e12360cd951b71ee      1
    153 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    154 0xdbc30d47297b5360f5fb386e5791b6ca75ba54a9      1
    155 0xdcfd6d6e63f15a391d96d1b76575ae39ad6965d9      1
    156 0xddd6578aaa0010808bb13b95c9838c440f8b682e      1
    157 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    158 0xdefc79316c9e12f2c5d82ab3bf10861ce38ce3cf      1
    159 0xe036a80f628e531982189fb6c4f277deca521e36      1
    160 0xe10772c3c2e8879b13d5d2886ef8e9f9b95b83aa      1
    161 0xe1678bdf494bf00464b6fee30591b8b4fc3e480a      1
    162 0xe322875fffb56f840456c7ba4742c125dd9cfd14      1
    163 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    164 0xe6468262b5a34057676586dc1976de5f664b7083      1
    165 0xe71775afbe475bd88d44e27f4bf1f94118e33e6d      1
    166 0xea13fc9b05806a76e478b82f6527d4f7eb21321d      1
    167 0xea4d8482132d15666a58bdbadb6c856eecb1b5bc      1
    168 0xea7d3818a3b736fba40852cd90471df33c6d6821      1
    169 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    170 0xeb1830e98bbd7f526c68bf4bc7916cbca0ec797f      1
    171 0xef136cd8d9bbd4746ed6172e93c7be36b53b6741      1
    172 0xf0b606b9d4d8809eb41a744eb19be23b0dea74df      1
    173 0xf3fbb2c0a711529ef9b81fe59a5ef5b8f1e0eb27      1
    174 0xf4405d30fc81e8bea085d2d1a90404bde2359453      1
    175 0xf9b55e4046c36a182457f87c95385d2437335bc1      1
    176 0xfce3ffd04de1ffdc60c1cc79a5127c5e4904c759      1
    177 0xfdb6dae2791921aeb1aaadfd0ce766225029e3e5      1

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
