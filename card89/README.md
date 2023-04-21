
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:22363       Length:22363       Min.   :1   Length:22363      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :2                     
         name          
     Length:22363      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17087469 # https://etherscan.io/block/17087469
block_hash <- "0x8e322177cd1f8ee25b01a92f88fed9c310855714b34dcbf0cf710f5b80dd2060"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4642 

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

airdrop_megan    <- pick(snapshot, contracts=c("CryptoClimates","GHOSTCLUB","SkinnedAlive","JohnnytheJerk"), address_remove=address_remove,address_pick=9,address_max=1)


allow_megan_phase1     <- pick(snapshot, contracts=c("CryptoClimates","GHOSTCLUB","SkinnedAlive","JohnnytheJerk","CryptoClimatesEditions","Degeneracy"),address_remove=address_remove,address_subtract=airdrop_megan,address_max=1)
allow_memes150_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=75,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_megan) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x426f6d19d35d7044488fba52eb2aa0fdbabc7c4b      1
    2 0x6dc43be93a8b5fd37dc16f24872babc6da5e5e3e      1
    3 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    4 0x8036bb84031e60a1bac93078c0e59bbd5e485db9      1
    5 0x926d2562cd25e2a8988449f61271c19ff65d2c04      1
    6 0x968a9e8b72205c76c6b2a0701e58bc7165f7bba6      1
    7 0xb8c2c29ee19d8307cb7255e1cd9cbde883a267d5      1
    8 0xc142995f05436c91bba172b7d17522a301323236      1
    9 0xeb90141895a0ff1839e4846362ae780b9796644f      1

## Allow Artist Phase 1

``` r
c(allow_megan_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 137 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00f5f4b914345cf7bcb665c8b1de35b22e7acd6e      1
      2 0x01d71a1e369cd675a1215b9f1bd63b582f6f257a      1
      3 0x08195cac6f2e283bdfc4bf64123beb5622c9c13a      1
      4 0x083d1316a2a86f24831f702b13d950e75327619c      1
      5 0x087ee66cc7914c20b49ab2a2de3cfe3b08fb18f2      1
      6 0x09d390e78faa3eca8c6e3c0bb3c6cbcaf679a44c      1
      7 0x0b5dd26f44d36141d97a3bf6841e7ba63c092af9      1
      8 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
      9 0x137effbb0b405cd6a3c549e53dd6244a439c514d      1
     10 0x15d51e51caf5585a40cb965080098bfb68af3336      1
     11 0x195fce941ae0c972b8dc814d2059537b67f14038      1
     12 0x1a1f337faa595a3603b4a3e67776150e7883954d      1
     13 0x1baec9d58603f6eaeddbd66acc56db76215fc8d6      1
     14 0x1da5331994e781ab0e2af9f85bfce2037a514170      1
     15 0x1e2467b33c6049e2392f221c8335278a9b45b61d      1
     16 0x1f0fcb7f6723b8fe3144bcf9abae6a6e612dd8cd      1
     17 0x20515afcdbc4b9943213377f81481f65884ba63f      1
     18 0x207b3c1083a62e282d375a11f52730879cbfbbbb      1
     19 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     20 0x23787626349cc170bfc512e5b5e3d3f44e00d02d      1
     21 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     22 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
     23 0x26dbcf7bb0e7826e087c691228b5f337143fbe16      1
     24 0x270dec6b9657a605f9a6c34ccc4678719d5c108c      1
     25 0x29f8da36b8b92fac92b68e8b428358903b7e9cb2      1
     26 0x2c81cee7050c9d03295560851ffde123bdf9696a      1
     27 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     28 0x342bd953d03c0baa824124a40e33010a215e51f5      1
     29 0x350764082c23b755126bb98933e9e1e7db487787      1
     30 0x357f5a836f86dc4ccf82a14691d929852c5222d7      1
     31 0x37cbc09501e6965c97c0e4984a0a371b5d591dd9      1
     32 0x3916729ed99ac3bccebb6a5f9382a19b463dec49      1
     33 0x3bc3a258fc6dd9f6d3021a47b44706c656faa6da      1
     34 0x3d5f1ad033c4e7c6a6d433d50d694c488dcaebcf      1
     35 0x3e81ea3a8a2d0b71a85468b02be83d09bfffe476      1
     36 0x405ac08452aed32c1d63a67ed017a6dbf0003dc0      1
     37 0x42cef12222672e260d275b80a0ed0bc40896af67      1
     38 0x43b3a12cdc49003c9537d0ab92800a97c0a8959e      1
     39 0x4400e3990d534edcb5ea476eefc8e45330f28215      1
     40 0x446a6560f8073919d8402c98db55db342a20300b      1
     41 0x44b51387773ce3581156d9accb27849a204f31dc      1
     42 0x4d70e61c6209fcf64f759b572962c43b69ebd0e1      1
     43 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
     44 0x5349ac882d15f17951e72071cec31c6f43787803      1
     45 0x5622cc24ddecf5888d31df4b8b12648d105472bc      1
     46 0x592a51330f1e824dd71135d3adc18d56a7bbf5df      1
     47 0x5a700c1d63b7717b0f1b5f4723f3d0201b39bcf6      1
     48 0x5b38d9ae52e2e3009110a7d8a26032bd1001cd3a      1
     49 0x5b53cbf3a7ae63d71694f7e1e6d1f4f5dda8f635      1
     50 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
     51 0x62e7ebf1b901e8a1e18057274ee24976ee9a7e9d      1
     52 0x6301add4fb128de9778b8651a2a9278b86761423      1
     53 0x66ce2e20013c47e62cf5c12398eaf1495cc28745      1
     54 0x66f832bd2c82fc78a3843e3bb82260e59a1fbfd5      1
     55 0x68da640489b88108b042e9915b826804862deee8      1
     56 0x6a2ebd6b63dc3f8ccd1f91292cefb07255e01c86      1
     57 0x6a46c72f7897cc788586ae813e3036545daedcab      1
     58 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
     59 0x6b7f5b10899ad60d5f93cd0c25d8ea062d557714      1
     60 0x707bfeed11b63d3360de40313f01092aa9b365c1      1
     61 0x7143f19d9df476227013260f015b06359918ef3f      1
     62 0x7239eebf5f37aad21ca7cb20c882290328c7a8c6      1
     63 0x752198ad579f9a5ac455e586598108a1215f0dd0      1
     64 0x7a204ae5b7e4458f87c9ec5056953caf3f9aece9      1
     65 0x7d87e23d7777d4829845f03f31721e10775799fd      1
     66 0x7ed28d8d8857d5566d581cc2342e7f7e8234ae1b      1
     67 0x827c622c85c9970b4902918bc5b843e0aa291489      1
     68 0x82e36468d8fbcd1655a32f9c79b8e5cdef0f2494      1
     69 0x8358fc2dd62c00c1a9cd0fcb577309a4cead2471      1
     70 0x85f455a02f07d0fdea27f8bd28e0ce0c86a54294      1
     71 0x89902603d272446bdbbdc4b8da5fb1c4de7c759e      1
     72 0x8ad272ac86c6c88683d9a60eb8ed57e6c304bb0c      1
     73 0x8ae6422631292c31aeeb2efe154d6326f703f46b      1
     74 0x8ba1b5bf769ce974d059187dccbf9db91e628bd0      1
     75 0x8bace3a49a375027868cdd34e84521eed1f1b01d      1
     76 0x8c57e23cbf902ed01a7600fef1b8d083efb24909      1
     77 0x8cad34fdddbd7cf8fa8a9ee242c2b573964dfecb      1
     78 0x8dbbca57ea56290efa14d835bbfd34faf1d89753      1
     79 0x917a453b20dda213e3ab9c04bce0c4a4ae197411      1
     80 0x94de7e2c73529ebf3206aa3459e699fbcdfcd49b      1
     81 0x971eeb30f075907208dc688806193129370dd1b3      1
     82 0x977089ca5b701e09637768cde651b94855d13eab      1
     83 0x97b86c6c27d42f52793c5aa111ebc6f19e30f35c      1
     84 0x9ad85d7c0f7eff3d0718c3a3a032557558f7048b      1
     85 0x9e189c74307412dc4ccc98d7a18a4c606962a509      1
     86 0x9e1e3857fb2484379858b9daf230379015a7a100      1
     87 0x9ec4bdf1fa30738faa83f950be25bb1b87f853dd      1
     88 0xa421d48e2f17402c8165fc4c46ac78cadbd858a9      1
     89 0xa46f24d23bb97d2fd127b81a6d849b66d976b8f0      1
     90 0xa75d179e0a9d3cb14a1ab856cd4cb94aa290a3bd      1
     91 0xa88235065d97a56719ea7d4fe72f8f953c984c0b      1
     92 0xace1c6f4dab142925a3d628c0fa5440c4dedd815      1
     93 0xafd37c11f9dfee561db31d9bab7cec4812bbd241      1
     94 0xb47a70df538b9a3b591bc5d75da66a04c879b291      1
     95 0xb88f61e6fbda83fbfffabe364112137480398018      1
     96 0xba06d8a2ce9a254a1dafd0a5390cde80c629ddb6      1
     97 0xbca2cf5e09748e38abd5091559ac95968d26b949      1
     98 0xbd9d1d31a6a451f7a8ec61fc5fd491fd59d5f489      1
     99 0xbdca0a84c5c9f67cdcc615e60221c088971620e4      1
    100 0xbe112243a36f44a86eefd542a074d31fbbc8c12d      1
    101 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    102 0xc229d7d3dd662a1b107e29aa84bb0c8ff609cf3a      1
    103 0xc4c3f78e1b605d2984d02c30b5898dc7ade4374f      1
    104 0xc550716af5cfe7e6315e6d8b94d2154448b2444a      1
    105 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    106 0xcb0a0107419f9aca86e793f0808afc2e4ff490ef      1
    107 0xcb42ac441fcade3935243ea118701f39aa004486      1
    108 0xccfa9099f4f97c883e98dfc808448171401e001f      1
    109 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    110 0xd2a744047bb754040911ae14cb5bfe289812e9a9      1
    111 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    112 0xd6dd6961d3224958fcd306b76a991ab974ec1ebc      1
    113 0xdadddee3daaa02f854634c56067aeb658ff42b4f      1
    114 0xdbdca117d007ca7b02da0403bbda93ec53f0730e      1
    115 0xdcae87821fa6caea05dbc2811126f4bc7ff73bd1      1
    116 0xdde7841d35c90a9e246b8294d60acd6889237382      1
    117 0xde1b6dd59a147a5cbd584f5ec2e0954cfecd2816      1
    118 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1
    119 0xe1ff19610020d72930aee1b9c047e35b7fd0080e      1
    120 0xe20adb03a470b5573bf572dd918e052d0d17866e      1
    121 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    122 0xe55a329415a518e3f743cbb850f2dbc694fe4bc0      1
    123 0xe83c750b2708320bb134796c555b80df39a3d97b      1
    124 0xe86c9cb39b281f9315b0b98cf34aa388daae0c65      1
    125 0xe8dbd6474fb0128863f5d3204af5ef363d90adb0      1
    126 0xeab3739d4e716bb07901d72dc94b4139b6968623      1
    127 0xeafd279c599a41f909e2800a613ab965cb57f236      1
    128 0xec3281124d4c2fca8a88e3076c1e7749cfecb7f2      1
    129 0xeda7c3afebbd81f34353e4e438870865653249e0      1
    130 0xef80395b4943e1741c92a6f2119eba58cfed889a      1
    131 0xf22742f06e4f6d68a8d0b49b9f270bb56affab38      1
    132 0xf2a9b2f80d3e5ce23f78020d72d7cd9532cc2e0d      1
    133 0xf6f61b87ca0208c9d4bb12ba4f7ed8366d9d26a0      1
    134 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    135 0xf997a1a60cf8a65185a938647bd22c6158463195      1
    136 0xfb38cc51133ef30286229be866a261c36c1e8f8c      1
    137 0xffb6d97bd1e7b7bd08595096d15037401a1f416b      1

## Allow Memes Phase 1

``` r
c(allow_memes150_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0022252b11a1c7a688c3e6281873e0111d63c270      1
     2 0x017c4dc18a2d67e82a49c4359016e50fd8e63233      1
     3 0x0279273242ea0a8be97f682221c0c7f9187416db      1
     4 0x094ce175d880a7fbff2cd01382f8fa1a172b734c      1
     5 0x0f1025f754b3eb32ab3105127b563084bfa03a6f      1
     6 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     7 0x12e08b091a8b6419502449bbb369709c1e4a60e0      1
     8 0x15e3bf5f0158afd884a85defea6ffa9056ef871f      1
     9 0x162a64e9988479a960d943418479152c58e0c884      1
    10 0x1688ca553e48049f192dc727ff14414bf1524243      1
    11 0x179504149c675376952500c65bd2f83052952541      1
    12 0x18b518f0b00f45e53c650c1a8a560667e81ae4fb      1
    13 0x1adef8b7253c43caaf50500913d422ae688367bd      1
    14 0x1af42e861037f45e1afde49917d545cda5171a72      1
    15 0x1cde3c510e0178afc5a0717b5ba323eec275cb0a      1
    16 0x244474fbcc4a68b4dc8ba70c6533f0afaedcdf61      1
    17 0x251a0be0e10f5dce6908ed3ec3777fc71cc5b420      1
    18 0x25b12eea057708adcb521ef7da0d4112523372fa      1
    19 0x266b2d881291c5bb36cf386d16e5f8ebb8e19a71      1
    20 0x2879707ec9b6c9c84d9c2512b64cd6ab36882cbc      1
    21 0x28b91b119f139a18b5688f6327c4c4eb839ed644      1
    22 0x2a3c05f993d34bab6fa50c72f2465777d1c5d118      1
    23 0x2de20bf6ff647c1a47d4192d1b6d1114024bb475      1
    24 0x2f054c2f2c3497b96c66436fc006ebe096439be6      1
    25 0x309ba35771337651a771c502f7b9cf8aac866cca      1
    26 0x31004f28babda1f632cb445944892b04a411e7f7      1
    27 0x36885851f71f09a087039260452e4756af87d9ef      1
    28 0x39079407a3be76fa77603858d0d8c753c9bb3925      1
    29 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
    30 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
    31 0x5170ccf3f96c73aaa1f560912e91049ca77aade6      1
    32 0x5560a6e16411a4eba2e6e2b8573e21538f1965d3      1
    33 0x5591f6258fd9eb2ad0d3a4e88245d06e1e8661bb      1
    34 0x59c1591ecd3689e911a49ebd406f9fb157171cef      1
    35 0x5c055c05126b8bd0a16596e79f76308098ef46f3      1
    36 0x5c21e2ca71209821b95a8182663a64569aa8f822      1
    37 0x6191c173e855202083455859e670ac16a669788d      1
    38 0x689a19f57077f682a1d7cc19d9066f1a834721a2      1
    39 0x6b92686c40747c85809a6772d0eda8e22a77c60c      1
    40 0x70ea6798510298b2eb333ebe3fd63f5959149444      1
    41 0x7106e9b712fa396941a023186011ebbd7d0c201e      1
    42 0x729fa41c7413def56234fa29a72991b49b9a542e      1
    43 0x7d639b54904816df6ef24f10ad421a874621b512      1
    44 0x822ec855cc9f41b592622108e6f3565e976430c6      1
    45 0x86f0a82dfd9745ec23bc8a72c819193e74962eb3      1
    46 0x87d8dcef48c9632e87de450e55941d68db0b1463      1
    47 0x88c5677336e252c7a53c29dd20375edc802a5919      1
    48 0x88d95780c75db73bd6d527be913dd6b3cb1a201b      1
    49 0x8dbb75c576b71b43eea54398f8606aec530181dc      1
    50 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
    51 0x96545a59ce81bb6acaf95957ea09cf577889112f      1
    52 0x973231cf471f768f0939042e0591da1b15f4799f      1
    53 0x981a7420150d815147a62a93905e0064e8ad7342      1
    54 0x9bd4b05b6f3cd3778012f72c16c42fd0490cfb3e      1
    55 0xa061fbfa7dc7ee9f838a717e8b55fbc34641bf6e      1
    56 0xa1ef5ac40d7ea6de39faef3cda7bdd82e09240d6      1
    57 0xa40d281de9c15d501ef21ac32f777a5a551fddd6      1
    58 0xa563df990d0a84a23cb4d08b508fe156e97a5593      1
    59 0xa8207370cd7c6b87aa427459ec439486f3dfc8f0      1
    60 0xb62ea019c3ecf647c21b3d394f373c4089e3e4d6      1
    61 0xb6ea381cac9da2de47c6a760445bbeab2b1db480      1
    62 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    63 0xc60a2326a50606c84e00f2e4c1035c8d39fe467a      1
    64 0xc776cb46892baa6d87a3817019367e05652eef2a      1
    65 0xd0157b9ebe373975cc9bae67e4f4b92e178f072e      1
    66 0xd3b605dd135358ca50cdf59af570ea3ab717bd82      1
    67 0xd8e93ccf41f079627f40573152bea5178041e1be      1
    68 0xdbe2258624f94ab8ee30ceb67b2a078b24bb6d6d      1
    69 0xe093f019e45218949bf210696869665eee186fc3      1
    70 0xe1c1c3867181cdb15f5c011120b4b7abcce91c5c      1
    71 0xead90127d60f78f3e97781938418004be1794eae      1
    72 0xee2b31fcf6c51ed01ae06cce05293c30afa5eb87      1
    73 0xee56745127f9ad1f49b68db7df35536e9d9885ef      1
    74 0xf837203e4fa9c139e41ff3241c434411930c1081      1
    75 0xf8ae436403100fa10603aea3c7b1ef097d61d89e      1

## Allow Gradient Phase 1

``` r
c(allow_gradient_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     9 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
    10 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    11 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    12 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    13 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    14 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    15 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    16 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    17 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    18 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    19 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    20 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    23 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    24 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    25 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    29 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    30 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    31 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    32 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    33 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    34 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    35 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    36 0x69e68074f1aada957edd39c5eae0069973343f30      1
    37 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    38 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    39 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    40 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    41 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    42 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    43 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    44 0x82139687faae8a29851902783e02e699de0e0846      1
    45 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    46 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    47 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    50 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    51 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    52 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    53 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    54 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    55 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    56 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    57 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    58 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    59 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    60 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    61 0xbf814810b44978de273191fd612aa47f7b69d564      1
    62 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    63 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    64 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    65 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    66 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    67 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    68 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    69 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    70 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    71 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    72 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    73 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    74 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    75 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    76 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    77 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    78 0xfd22004806a6846ea67ad883356be810f0428793      1
    79 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1

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
