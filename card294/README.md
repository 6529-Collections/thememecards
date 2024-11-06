
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:234         Length:234         Min.   :1   Length:234        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:234        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21096969 # https://etherscan.io/block/21096969
block_hash <- "0xbc3393d0d1f4a9505c3e83f66711cb7e123de6dd624b1495653f82fd001d985a"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4554 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","StephanDuquesnoy","MakersPlace"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("StephanDuquesnoyEditions","StephanDuquesnoyMedallions","NGEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","StephanDuquesnoy","MakersPlace","StephanDuquesnoyEditions","StephanDuquesnoyMedallions","NGEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 23 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
     2 0x068161ba6b0373cefc55491c465b9939817c93ad      1
     3 0x131ba2a1fddfaecf8a55f73bb1ff9b314626b94f      1
     4 0x1de1adc8f4e360cd71ec056d20cb19fd2fa0b927      1
     5 0x2b0eeea18c8cdf09ec828cfa99a80deffe47328a      1
     6 0x300da191248a500b2174aed992d6697bf97f9139      1
     7 0x33be64b5921955387ddb08a31c3193461612b565      1
     8 0x341c7de61071dfbd0c7275aa2464e2256c0a3172      1
     9 0x5287a9940ca1e2400f4004bc57f22f4b35f66e82      1
    10 0x5973b426c616af28773d7a1457bd645a220ea79a      1
    11 0x5b1e9c155de1474629aeb9291d6cd0ca3aa54dd2      1
    12 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
    13 0x61f4abbc0378459336de4fd4a8c3b09628a993b5      1
    14 0x78059873335c542747f02ac93d83eb2fe1c1c510      1
    15 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    16 0x8d09aeac57f577e78e2c1501d8873eeea3b1c0e4      1
    17 0xab7473da0af4658ae2879cb641d82e7652721486      1
    18 0xb7a2401d9f546c62ed813eb14c92a830c56f9fc7      1
    19 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    20 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    21 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    22 0xca35f3dfc3fd9ffff8241b93db8f71d46d2122b8      1
    23 0xdc4471ee9dfca619ac5465fde7cf2634253a9dc6      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 160 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x0360b9acda197703cb3040715c75659c38f13cf7      1
      3 0x04ce45c82e68fd4b0b11d5aa592a171068e1a6c7      1
      4 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      5 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
      6 0x10ae05efb32db90ed98d1d530f2ef19a77e7ebe8      1
      7 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
      8 0x11732f4b391e6c511cb402ade3ba3dc49914039d      1
      9 0x1275a3dcd8e065ee0c249f332554af8e02440db0      1
     10 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     11 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     12 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     13 0x1a2f598028c1877c39b9e978aa0ffbe5d22cf43c      1
     14 0x1e7f81e84661c0ea9b8470ec2d15c4aec7071633      1
     15 0x22fb48e316f78030c06560e48d6b2b7e9dd10a40      1
     16 0x234890bb60245ee6ebaf801e0812e37767005f54      1
     17 0x24e485b78916ca63b7bc948eede06ffcbbd01d71      1
     18 0x256f72aa045bd7b4008178c03232348dff98890b      1
     19 0x28130f3b9abaea65bb960e7ee34a22989fcfca7f      1
     20 0x292d634189452e37b73ab087fc945309885ff5f4      1
     21 0x2c0d351e5b58722676da689a4c2b7cfe68a6052e      1
     22 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
     23 0x2d43afff705b2358cf18f41ed80dc3047c7c332c      1
     24 0x2f12283d3357e14a9abf56c96458ef2869c22bf2      1
     25 0x30a2de17979ec42dc7c8052690474948e7005039      1
     26 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     27 0x326abacedf7d6c0300768259c997e796d99c9e2b      1
     28 0x32aaeee01a1bd496c57fc448947ad8136c6f4565      1
     29 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     30 0x34ee160d57c7c14d018b6f751fafd1060b560db6      1
     31 0x3a80d39ea6d316edef9ba0c847305b76ce2b5e58      1
     32 0x3b5ca475ac4d82433ae91f281951fa1825ded3e2      1
     33 0x3bfe743a71636d1c0ba12339de335fa349ce832f      1
     34 0x3ca985eca9baa6412605cf1ca91db1aee41cac33      1
     35 0x3d4b9f9967cff180921bf507725712b6a4f90ce7      1
     36 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     37 0x3eb5b7f40c90b2d9ed8ad3bf3b99ff450e4da81c      1
     38 0x43aa7abf84d5812817dd8784f6f1c6d809513706      1
     39 0x4451e7021a3c5508dc60a86798e889d7429b8933      1
     40 0x4478edf93707c0116b84232f7e539d2f1c1a3c97      1
     41 0x45e8bb296136f08cc7fca0281efcb18b971f0314      1
     42 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     43 0x46d43db499b97e4933d43989d619fc4e193b67af      1
     44 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     45 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     46 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     47 0x48a470cc479a6277c4a077ce204ad749e9269c1b      1
     48 0x498cf91288ee77aa0ae3425d3a7046c860c06520      1
     49 0x4bb23a688e57b588c8b5bfcf832825207cdd4da1      1
     50 0x4c8bc5dd54bd78a2792b578bd2fc919196e64d77      1
     51 0x4f547d3de88cdba1a25ca7af6324d2e6e9025e1d      1
     52 0x516656beacd4c41f4e1a2273263ff4995d0fd1d3      1
     53 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
     54 0x578b076f33c021ca8ec8873be00c734559a99057      1
     55 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     56 0x5dab695311e4e7d5be4743187c3e40589f45c9b0      1
     57 0x5e051c9106071baf1e4c087e3e06fdd17396a433      1
     58 0x608a070ee28659b8549db85364632b93aac63c82      1
     59 0x6154570ff4695ed37e883afccaa492c0886bcd0b      1
     60 0x64c53dd100884542cf8a020105dd264ee2ae9921      1
     61 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     62 0x675754365c09c0ef56d6b68295424131d5542472      1
     63 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     64 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     65 0x6d5f1fc2799d285f26951ad0d2bffd6a85cb2b6f      1
     66 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     67 0x7006507dabd3222a7c828ad9af0f87707b19748e      1
     68 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     69 0x7101a39c053954e3e7fd010fdf3f6ef6bdbcbde0      1
     70 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     71 0x7320dfa95fadd9bbc20bf8eca42587c44176e9a7      1
     72 0x76e904f6236e7a2c7a91bb64fb9fe8546c72136c      1
     73 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     74 0x7e604316bf70f92987b0602d3ea6371c01fc5d0b      1
     75 0x7ee2a944c53ff74d7d686ee80e994dbd40b9dd81      1
     76 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
     77 0x814e21d296f494d879d2126d8b78326a3370396d      1
     78 0x8414f956d158bd1cd6e52742423d51ef3a65fedb      1
     79 0x841e54526835b674392e2b8f0ec19be0577add87      1
     80 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
     81 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
     82 0x8dafe69056b0cafbc805da2501956ff9a60dd140      1
     83 0x8debc0beaeb233df0d7ceef26d27280146efd525      1
     84 0x8ea1b83d63851346dacd88d72b30219af00ccade      1
     85 0x90d8ca60cfd39b1ed0759a6daae4a66285534346      1
     86 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
     87 0x9727938aa47822147a4acd534b088634cb23a4a8      1
     88 0x994d0c5e289c9750849b916cd4961e1e7ddb451e      1
     89 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
     90 0x9b08042e20dc4e883c41e89813be918d6729099a      1
     91 0x9c273f1e31df16ed0f6cc4901e7315972c040a71      1
     92 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
     93 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
     94 0x9eb1cca71ffd5a9a915b50b8ebdc1f3e3f88ac2b      1
     95 0x9ed7e2f21f9f716994684c0309d35fae5f95427a      1
     96 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
     97 0xa05664abfe5f91db188f50987863c972d95d92f5      1
     98 0xa222204acf1be4077d34102fab38a759060b77c2      1
     99 0xa3ded378678d735eb14ad78db64887386932c62b      1
    100 0xa65b73c58956cdb130e60cb6ebc54032a6773aa9      1
    101 0xa73f470755f734c714dd07ba1918f43329032920      1
    102 0xa8d519273d7b02ad7b8713b3b23c7bae6ccd7fe9      1
    103 0xa92e794e2b196e612a59b9b5c5051e1888fd7b24      1
    104 0xab47906227c253142d5296fa975be3febc06e330      1
    105 0xae045f639cd6a64a26c743dfd3ddd7e8f75f76cc      1
    106 0xae0adfb699a774664c79f2ca139aa292129059fb      1
    107 0xb1bc3ef864b01758b33b17daf695d180b1743d1d      1
    108 0xb22898a1e815046bc77c3d2afdc449a5f514d638      1
    109 0xb63ecac8f0cbac1ad0eaa9f4e8797644c744ea3e      1
    110 0xb77b0b4ffa6ed52e729daf3a3edaeaa585b7413f      1
    111 0xb78e99d2301ee621f4332a0d1ef759423976d8b6      1
    112 0xb9e8861c3486efca7cf2b98482d286c9f6f44df7      1
    113 0xba5e753d635811f676de4797c9ea2dade93c8f39      1
    114 0xba77e0b03b41a980bb4e7bb32ec7a2dfe1b66f8f      1
    115 0xbbbbf1e31b416da698d220ddbd4be517497cc3ae      1
    116 0xbdc3e5b342b78bae722b86a6d442b92701dc7ddd      1
    117 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    118 0xbfb9915884de2ec1f96d999ec81c29e129035edc      1
    119 0xc024203eccb9719fca0d52744fbd489f0a6e4298      1
    120 0xc02761d9ab63070fbe4ad2e6107420ac4b2d688f      1
    121 0xc1003fd0f7126333ace9772f6dee41f0e9d21205      1
    122 0xc342c6281b60310436d99dbc821e99bf77091f4f      1
    123 0xca300722464cabdb4290c4c7f8426c91a7f93955      1
    124 0xca30a18dac40f38decdd2d311129cd111b66c10b      1
    125 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    126 0xcae7e26d7d18ae770b17ab1d69f8b3c2569899b7      1
    127 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    128 0xcc3b2af633074ae159a16a0eda2753e7578262ec      1
    129 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    130 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    131 0xd64444b2c80afc2f425a886b1603fc4c0616d128      1
    132 0xd67da3d36085b966b1fd7f72dd8bd7e5dc630f7d      1
    133 0xd6d8e393824aac8ecaf4e1f689717bc001064abc      1
    134 0xd732748071e38a48b4e9a3ff1b2e7e87c0a39c2e      1
    135 0xd8313486900439021386614e9c160a570ef37890      1
    136 0xd98d67bba62ec2259b5ce59214b0104e1ace939b      1
    137 0xdbba4211647bb7b154d8f981afd87524443fd567      1
    138 0xdd25963251fceb0a7bef9bb713eed260829f5656      1
    139 0xdf3c32db47975c42806dbd5a402381f49eb2cb98      1
    140 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    141 0xe0a0ebe6621e346afbda9bd2c91ba20d39254ab6      1
    142 0xe2a9829ed811d0253de26216a36f301f57bf9ffb      1
    143 0xe2ee9d78cac02826f4eda8259203c715071504e1      1
    144 0xe357f44ded7e09699ed03db15c51be199fc1e23a      1
    145 0xe365aba924c6105da509fca843c783132bd6c113      1
    146 0xe4da1b57eaffb177ffd6b0c6c7fd4dfc51a98581      1
    147 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    148 0xe9dc8606e049a0d0f97d887f1fd4d06e23d9de95      1
    149 0xea4aa740a98e7a0b5ab8f3b292aa64e559116f62      1
    150 0xedf94c6c26a4ca77429f0e1c7eaaded19acb4437      1
    151 0xefb409f69f833536362d6077f14b0a51efd9d468      1
    152 0xf4516664d159973fd7b254801a10a51ab2db0c3c      1
    153 0xf58be576c924c9cf0bd17023b810ae46b1d42be2      1
    154 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    155 0xf8027e0f03c95782ef9be1826828bee931c1ab83      1
    156 0xf813e0de2293b487f75caca607290f3161944f3c      1
    157 0xf883f509b8af55669c0dcd00d64ee8d2ffdb76e4      1
    158 0xfbda58bcaafe5242f3f4aedde42dc2966d4d5b48      1
    159 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1
    160 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 183 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0040122ed305e0b16dfc9fd8e0e9a74a3cdd5509      1
      2 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      3 0x0360b9acda197703cb3040715c75659c38f13cf7      1
      4 0x04ce45c82e68fd4b0b11d5aa592a171068e1a6c7      1
      5 0x068161ba6b0373cefc55491c465b9939817c93ad      1
      6 0x06dac6a2dcfa0f0dfb3e7e766515eef5b2c6399a      1
      7 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
      8 0x10ae05efb32db90ed98d1d530f2ef19a77e7ebe8      1
      9 0x11674c2ebbb09e3c007492502027db7d92b3e1c7      1
     10 0x11732f4b391e6c511cb402ade3ba3dc49914039d      1
     11 0x1275a3dcd8e065ee0c249f332554af8e02440db0      1
     12 0x131ba2a1fddfaecf8a55f73bb1ff9b314626b94f      1
     13 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     14 0x1388ae7b87bdf98369d28c747642049d0078055d      1
     15 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     16 0x1a2f598028c1877c39b9e978aa0ffbe5d22cf43c      1
     17 0x1de1adc8f4e360cd71ec056d20cb19fd2fa0b927      1
     18 0x1e7f81e84661c0ea9b8470ec2d15c4aec7071633      1
     19 0x22fb48e316f78030c06560e48d6b2b7e9dd10a40      1
     20 0x234890bb60245ee6ebaf801e0812e37767005f54      1
     21 0x24e485b78916ca63b7bc948eede06ffcbbd01d71      1
     22 0x256f72aa045bd7b4008178c03232348dff98890b      1
     23 0x28130f3b9abaea65bb960e7ee34a22989fcfca7f      1
     24 0x292d634189452e37b73ab087fc945309885ff5f4      1
     25 0x2b0eeea18c8cdf09ec828cfa99a80deffe47328a      1
     26 0x2c0d351e5b58722676da689a4c2b7cfe68a6052e      1
     27 0x2cf0bb6d1cc1a7296ad0da0adfdf74074844a588      1
     28 0x2d43afff705b2358cf18f41ed80dc3047c7c332c      1
     29 0x2f12283d3357e14a9abf56c96458ef2869c22bf2      1
     30 0x300da191248a500b2174aed992d6697bf97f9139      1
     31 0x30a2de17979ec42dc7c8052690474948e7005039      1
     32 0x313fe2f013b5ee2e2aac09b59462464db56f7680      1
     33 0x326abacedf7d6c0300768259c997e796d99c9e2b      1
     34 0x32aaeee01a1bd496c57fc448947ad8136c6f4565      1
     35 0x33a34e27a81436ba9d79276406a285e89a8bd8a8      1
     36 0x33be64b5921955387ddb08a31c3193461612b565      1
     37 0x341c7de61071dfbd0c7275aa2464e2256c0a3172      1
     38 0x34ee160d57c7c14d018b6f751fafd1060b560db6      1
     39 0x3a80d39ea6d316edef9ba0c847305b76ce2b5e58      1
     40 0x3b5ca475ac4d82433ae91f281951fa1825ded3e2      1
     41 0x3bfe743a71636d1c0ba12339de335fa349ce832f      1
     42 0x3ca985eca9baa6412605cf1ca91db1aee41cac33      1
     43 0x3d4b9f9967cff180921bf507725712b6a4f90ce7      1
     44 0x3e0914fc2b3b67a476142020b7d6990d01ad234e      1
     45 0x3eb5b7f40c90b2d9ed8ad3bf3b99ff450e4da81c      1
     46 0x43aa7abf84d5812817dd8784f6f1c6d809513706      1
     47 0x4451e7021a3c5508dc60a86798e889d7429b8933      1
     48 0x4478edf93707c0116b84232f7e539d2f1c1a3c97      1
     49 0x45e8bb296136f08cc7fca0281efcb18b971f0314      1
     50 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     51 0x46d43db499b97e4933d43989d619fc4e193b67af      1
     52 0x46dea66b6fc9b1d6002041dba6bc86fb34346472      1
     53 0x47a366519eaf385804a404697eb8d5cb1fd55d8b      1
     54 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     55 0x48a470cc479a6277c4a077ce204ad749e9269c1b      1
     56 0x498cf91288ee77aa0ae3425d3a7046c860c06520      1
     57 0x4bb23a688e57b588c8b5bfcf832825207cdd4da1      1
     58 0x4c8bc5dd54bd78a2792b578bd2fc919196e64d77      1
     59 0x4f547d3de88cdba1a25ca7af6324d2e6e9025e1d      1
     60 0x516656beacd4c41f4e1a2273263ff4995d0fd1d3      1
     61 0x5287a9940ca1e2400f4004bc57f22f4b35f66e82      1
     62 0x53e9c9405762538bde0b5422429c8e87757ab8af      1
     63 0x578b076f33c021ca8ec8873be00c734559a99057      1
     64 0x5973b426c616af28773d7a1457bd645a220ea79a      1
     65 0x59968c2c25faf934664448f5ce2c022fce0b9398      1
     66 0x5b1e9c155de1474629aeb9291d6cd0ca3aa54dd2      1
     67 0x5dab695311e4e7d5be4743187c3e40589f45c9b0      1
     68 0x5e051c9106071baf1e4c087e3e06fdd17396a433      1
     69 0x5e1127be6dab460330e97e0dbb4912accf6ea178      1
     70 0x608a070ee28659b8549db85364632b93aac63c82      1
     71 0x6154570ff4695ed37e883afccaa492c0886bcd0b      1
     72 0x61f4abbc0378459336de4fd4a8c3b09628a993b5      1
     73 0x64c53dd100884542cf8a020105dd264ee2ae9921      1
     74 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     75 0x675754365c09c0ef56d6b68295424131d5542472      1
     76 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     77 0x6c6e93874216112ef12a0d04e2679ecc6c3625cc      1
     78 0x6d5f1fc2799d285f26951ad0d2bffd6a85cb2b6f      1
     79 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     80 0x7006507dabd3222a7c828ad9af0f87707b19748e      1
     81 0x7091d6cb3e3497664e29b0022d516eaeb6d9ffe6      1
     82 0x7101a39c053954e3e7fd010fdf3f6ef6bdbcbde0      1
     83 0x72eb30d3ca53f5e839325e2eacf535e70a9e6987      1
     84 0x7320dfa95fadd9bbc20bf8eca42587c44176e9a7      1
     85 0x76e904f6236e7a2c7a91bb64fb9fe8546c72136c      1
     86 0x78059873335c542747f02ac93d83eb2fe1c1c510      1
     87 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     88 0x7e604316bf70f92987b0602d3ea6371c01fc5d0b      1
     89 0x7ee2a944c53ff74d7d686ee80e994dbd40b9dd81      1
     90 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
     91 0x814e21d296f494d879d2126d8b78326a3370396d      1
     92 0x8414f956d158bd1cd6e52742423d51ef3a65fedb      1
     93 0x841e54526835b674392e2b8f0ec19be0577add87      1
     94 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
     95 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     96 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
     97 0x8d09aeac57f577e78e2c1501d8873eeea3b1c0e4      1
     98 0x8dafe69056b0cafbc805da2501956ff9a60dd140      1
     99 0x8debc0beaeb233df0d7ceef26d27280146efd525      1
    100 0x8ea1b83d63851346dacd88d72b30219af00ccade      1
    101 0x90d8ca60cfd39b1ed0759a6daae4a66285534346      1
    102 0x9318f724edfcf60066a924ddb9133a312becf0f7      1
    103 0x9727938aa47822147a4acd534b088634cb23a4a8      1
    104 0x994d0c5e289c9750849b916cd4961e1e7ddb451e      1
    105 0x9af7edc48bc8db5e4afd3af95df143ea29e2e2d6      1
    106 0x9b08042e20dc4e883c41e89813be918d6729099a      1
    107 0x9c273f1e31df16ed0f6cc4901e7315972c040a71      1
    108 0x9cad606ef8153b3b06d58a6ecf9e20050c92d8a1      1
    109 0x9e1ccb3c9c55ab371d427745bf213df80cf23141      1
    110 0x9eb1cca71ffd5a9a915b50b8ebdc1f3e3f88ac2b      1
    111 0x9ed7e2f21f9f716994684c0309d35fae5f95427a      1
    112 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    113 0xa05664abfe5f91db188f50987863c972d95d92f5      1
    114 0xa222204acf1be4077d34102fab38a759060b77c2      1
    115 0xa3ded378678d735eb14ad78db64887386932c62b      1
    116 0xa65b73c58956cdb130e60cb6ebc54032a6773aa9      1
    117 0xa73f470755f734c714dd07ba1918f43329032920      1
    118 0xa8d519273d7b02ad7b8713b3b23c7bae6ccd7fe9      1
    119 0xa92e794e2b196e612a59b9b5c5051e1888fd7b24      1
    120 0xab47906227c253142d5296fa975be3febc06e330      1
    121 0xab7473da0af4658ae2879cb641d82e7652721486      1
    122 0xae045f639cd6a64a26c743dfd3ddd7e8f75f76cc      1
    123 0xae0adfb699a774664c79f2ca139aa292129059fb      1
    124 0xb1bc3ef864b01758b33b17daf695d180b1743d1d      1
    125 0xb22898a1e815046bc77c3d2afdc449a5f514d638      1
    126 0xb63ecac8f0cbac1ad0eaa9f4e8797644c744ea3e      1
    127 0xb77b0b4ffa6ed52e729daf3a3edaeaa585b7413f      1
    128 0xb78e99d2301ee621f4332a0d1ef759423976d8b6      1
    129 0xb7a2401d9f546c62ed813eb14c92a830c56f9fc7      1
    130 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    131 0xb9e8861c3486efca7cf2b98482d286c9f6f44df7      1
    132 0xba1624dcebbf017badcb7f3cd75ede06cd96feab      1
    133 0xba5e753d635811f676de4797c9ea2dade93c8f39      1
    134 0xba77e0b03b41a980bb4e7bb32ec7a2dfe1b66f8f      1
    135 0xbbbbf1e31b416da698d220ddbd4be517497cc3ae      1
    136 0xbdc3e5b342b78bae722b86a6d442b92701dc7ddd      1
    137 0xbec5d88ef9491b661ef33a596eed6a4e5cdb00cb      1
    138 0xbfb9915884de2ec1f96d999ec81c29e129035edc      1
    139 0xc024203eccb9719fca0d52744fbd489f0a6e4298      1
    140 0xc02761d9ab63070fbe4ad2e6107420ac4b2d688f      1
    141 0xc02a3f4424c8834fcb1f31832501f9e3e090262a      1
    142 0xc1003fd0f7126333ace9772f6dee41f0e9d21205      1
    143 0xc342c6281b60310436d99dbc821e99bf77091f4f      1
    144 0xca300722464cabdb4290c4c7f8426c91a7f93955      1
    145 0xca30a18dac40f38decdd2d311129cd111b66c10b      1
    146 0xca35f3dfc3fd9ffff8241b93db8f71d46d2122b8      1
    147 0xca4cd1152aabcb80859add1406ab7ec55e963e28      1
    148 0xcae7e26d7d18ae770b17ab1d69f8b3c2569899b7      1
    149 0xcb8dbd4010d2431f8bf1cada88f5d0b33ec07f2e      1
    150 0xcc3b2af633074ae159a16a0eda2753e7578262ec      1
    151 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    152 0xd2a3f9c6fbe4c13d979898e603d64561264a6b35      1
    153 0xd64444b2c80afc2f425a886b1603fc4c0616d128      1
    154 0xd67da3d36085b966b1fd7f72dd8bd7e5dc630f7d      1
    155 0xd6d8e393824aac8ecaf4e1f689717bc001064abc      1
    156 0xd732748071e38a48b4e9a3ff1b2e7e87c0a39c2e      1
    157 0xd8313486900439021386614e9c160a570ef37890      1
    158 0xd98d67bba62ec2259b5ce59214b0104e1ace939b      1
    159 0xdbba4211647bb7b154d8f981afd87524443fd567      1
    160 0xdc4471ee9dfca619ac5465fde7cf2634253a9dc6      1
    161 0xdd25963251fceb0a7bef9bb713eed260829f5656      1
    162 0xdf3c32db47975c42806dbd5a402381f49eb2cb98      1
    163 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    164 0xe0a0ebe6621e346afbda9bd2c91ba20d39254ab6      1
    165 0xe2a9829ed811d0253de26216a36f301f57bf9ffb      1
    166 0xe2ee9d78cac02826f4eda8259203c715071504e1      1
    167 0xe357f44ded7e09699ed03db15c51be199fc1e23a      1
    168 0xe365aba924c6105da509fca843c783132bd6c113      1
    169 0xe4da1b57eaffb177ffd6b0c6c7fd4dfc51a98581      1
    170 0xe53c5b4f841570d193e05a27bfafe1c554f14875      1
    171 0xe9dc8606e049a0d0f97d887f1fd4d06e23d9de95      1
    172 0xea4aa740a98e7a0b5ab8f3b292aa64e559116f62      1
    173 0xedf94c6c26a4ca77429f0e1c7eaaded19acb4437      1
    174 0xefb409f69f833536362d6077f14b0a51efd9d468      1
    175 0xf4516664d159973fd7b254801a10a51ab2db0c3c      1
    176 0xf58be576c924c9cf0bd17023b810ae46b1d42be2      1
    177 0xf70ebfd828b993e2863670530c2ec62c049f37ad      1
    178 0xf8027e0f03c95782ef9be1826828bee931c1ab83      1
    179 0xf813e0de2293b487f75caca607290f3161944f3c      1
    180 0xf883f509b8af55669c0dcd00d64ee8d2ffdb76e4      1
    181 0xfbda58bcaafe5242f3f4aedde42dc2966d4d5b48      1
    182 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1
    183 0xff748e6b10082a435b9b0ecccf358c42c7f34dc0      1

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
