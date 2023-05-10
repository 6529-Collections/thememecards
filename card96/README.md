
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:31421       Length:31421       Min.   :1   Length:31421      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:31421      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17221469 # https://etherscan.io/block/17221469
block_hash <- "0x3fb8a27d09e6a0afabd8a26fa5aa6b34ee6ba64022dd8f56af6570db20a93289"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4877 

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


allow_billy_phase1     <- pick(snapshot, contracts=c("SuperRare","SuperRare2","SuperRare3","SuperRare4","Foundation","Moments","AFWIFLEditions","14thStEditions"), address_remove=address_remove,address_max=1)
allow_memes69_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=69,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_billy_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 183 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0092477b8acb3ac744bee3408b0c91c0ab5f9b70      1
      2 0x01e3884fc9c200c59f602e5c7259aab0910f2d3c      1
      3 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
      4 0x0544fbed9b72aa036517b21d1db50201a17d09ce      1
      5 0x08942872046fa44bc6456e491e8de11de8bae73e      1
      6 0x08ba7fbc0aba77c3f8664cc71baee71ef602df85      1
      7 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
      8 0x0f2005e5b1edf748a045d721e472886fee1815e4      1
      9 0x118fe9d1dc8efa98f3bf41618d9a3d7f049a61b1      1
     10 0x1273b62a0e80ed80ffd46c119555eca9787fa37f      1
     11 0x13fc125a65ce63803082071e385b34f1fb254b41      1
     12 0x153002a1aa045a92317cb58893a5e2907ee3f421      1
     13 0x1660b9e29f0db4116cfb1dfd4467ed765414eeab      1
     14 0x193959c6cd6a6b36f150b173383260cfeefc59f6      1
     15 0x1b16fa5d05f1e29a4ab8af4942c377937f022a51      1
     16 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     17 0x1d1bd1ae7df548c61d1ec3442c399591205f1fb9      1
     18 0x1d320444d6afcca57b8b2420c4eb15d723de32e3      1
     19 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     20 0x2168b2295e912a27c0015bad9f09f090ebce1a99      1
     21 0x221320d34800760e06b206acd01e626e463eb03e      1
     22 0x251be313e35d620a36b148e1e301e7082e0d11c9      1
     23 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
     24 0x29b63a037d8bd0341a45585b8b8fe1bc9e1aa4f5      1
     25 0x33e3c007d1d48e2b645c9ce22570267b0c82f578      1
     26 0x354ddeba429d9e43ec1438af5319b2158ad36afd      1
     27 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     28 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     29 0x3a943f529de8461697702fd9bb8fcda5d9a8ce61      1
     30 0x3a98f478825d9114eb118779a75fc9b0998cb9ec      1
     31 0x3ac2cc26fbb230e704393dc79e97cca47e709247      1
     32 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     33 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
     34 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     35 0x3fb160a681806acc91aed14c284c09a96ebc9dfc      1
     36 0x4038f1a494f8ec245cf85ea385e53fa111958b01      1
     37 0x403afdf9ea925d3b48e719a44610da1679a57651      1
     38 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     39 0x41db2348f54dbab35908594a3a47e9c3e2c71bfe      1
     40 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
     41 0x42b46951ae492fde820016ad6432c1360a227dd1      1
     42 0x4322aa46ffed67809862da613725728e2fb8eae3      1
     43 0x452fcd9f73a5f09f6b6be34b708e4c2b7d8fc46b      1
     44 0x456aa68594d1f8604becc3ec11b6bd0cb6e3e7ad      1
     45 0x4685b377e1c85620c6e2ba79a587822c3b6597e5      1
     46 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
     47 0x483a36e454e711ef5ef080539dfc1218ff09606e      1
     48 0x48b3466b30fcebea37341a14edc40d033ba9f366      1
     49 0x49a606975f17ec67da1438e6be5309c112d4f4b4      1
     50 0x4c9f829f7e317ff75c5ab562b502c45995ee1740      1
     51 0x4dc2e63f17c9a3175ec2d0f07b3f171c3298070b      1
     52 0x4ee589b12f56aaa3c4eaa4dc7ebe0b62b7c02cd4      1
     53 0x51fee9bf45c5dab188b57048658696edab9d72cf      1
     54 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
     55 0x5492e0112e49e5fed0a7b1278c76df655e509a99      1
     56 0x556ca7014504a0e133eb66a8a36dc5501d682a82      1
     57 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
     58 0x5739cee517442cfcf017eb0ba06ed5bd136155df      1
     59 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
     60 0x5c9baabb6f31012dbe573a42b8ef6deddeb8b2e6      1
     61 0x5e5ad4d2da374e58da24baec8e00642ec741ba58      1
     62 0x5eff35d620168bd496b39243fee0afdc593e972c      1
     63 0x60bdf6c5ca680a68e8e1668dd8fd886a74bea424      1
     64 0x6191c173e855202083455859e670ac16a669788d      1
     65 0x6255a47bf039e8232aead49b80a963ddd2671ac1      1
     66 0x62f0dd4e7d33b3c11ffbfdd31fa82d4aaed38ca8      1
     67 0x631cd42eb1063d1fe2b49a78744f3094702edbab      1
     68 0x631ebefd35fa59ac01e9c0514cdbcf9cb70b18bf      1
     69 0x6456f20453a383004f17b921cd770d187fcd30ef      1
     70 0x64a56d840e40aa8cd107b11ea1b49716df7dcb8f      1
     71 0x65b5ea1e9df2f92f4fe3bdb6f2cc8550c608a534      1
     72 0x65d7c1346ac84956b17cb3fabd0ad1ca9a59b678      1
     73 0x68b3778679e6c88a19f612dbec9bacc3a4e52d05      1
     74 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
     75 0x6c37148710e10d07522b4754d336e49dab5b1167      1
     76 0x6de91f0606bcdc90b442f52ded8a35bbe8c828d0      1
     77 0x6e22fcedff1e77c33986e9cc32889315c2799b5b      1
     78 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
     79 0x6f54c650190b44ac9efe531380417ccf12d2d06a      1
     80 0x70ecfb2be7b3f3d411901c2d974b0de6c9e6359f      1
     81 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
     82 0x74f77004a8ee8dd023c94f42f290e959a973defd      1
     83 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     84 0x768057610a07396e05046af01e7c9b912e4a81f3      1
     85 0x7a534e82fe3ae606127e9b5ce58bb9b028f36f0b      1
     86 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
     87 0x7af2e406bcc85580f2994f6cee959b280c6e0c32      1
     88 0x7c344832ec4142108f6b6bc49ce884281e6a19a3      1
     89 0x7c3777256b0b03cb332a06eefff1e961b2598694      1
     90 0x7cdc15eba13609a244e96b6b9e4c4a95f81ec953      1
     91 0x7ce80ec49bc4facbba5fc88e99ffbce900b44993      1
     92 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
     93 0x7dbd06824226b73a4e162bc04d2a58f3ba6bab51      1
     94 0x7fd1d8e388b29ef0b8a00ca07e2266686a5559ee      1
     95 0x7fdba82ec2360d4e566175943cdefd8443a77143      1
     96 0x803e96717d25ed306160d35ab0368dd595d0efb6      1
     97 0x8105e062d6b94c951e93739aee33018bd6a6d1c7      1
     98 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
     99 0x82166dcc47202c5fed2b4a8a5153589d19f37981      1
    100 0x82b6f03fac5ee84bf175e82458144aaca469ddc6      1
    101 0x82f23de5a474fc904041c357576afe53c30dd250      1
    102 0x8379ac05ef71f982d497f392dc1a14ee859efd95      1
    103 0x83a195e5de78a360407875b82b52782d331eb298      1
    104 0x8497277c9339170a0420e86da8352e0c084624cd      1
    105 0x85e37cd123d2889410d9fd1f434c9936e882e5c6      1
    106 0x88923378021bea85f9b09ce571a309e12c7d2262      1
    107 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    108 0x8f295c042933205df1d4060c2d9d185a62400ce9      1
    109 0x906d3c28dc17e47c71cc7ca712c8df068df396fd      1
    110 0x922631898b7956fd3c601f5b1f069ed16efc8256      1
    111 0x922d1874a9984cca520a6c078f9856a158442f57      1
    112 0x923b2d973b407c317869124362515db392148342      1
    113 0x93958d6e3b79b6d15a11ba00876ce8edcb02ba95      1
    114 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    115 0x98a692316057e74b9297d53a61b4916d253c9ea6      1
    116 0x99f4f03f53dc5cb4a1bb01ebbe29a5a4104c1ead      1
    117 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    118 0x9eedbfcffa62587fa5e17d1f556d17308c4a049a      1
    119 0x9ff77d193c091ae350b8ce26d50d46e392631292      1
    120 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    121 0xa2d1ebf3b7d49cbc5b171f4466b7a90f75bd597d      1
    122 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    123 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    124 0xad74b6eaefae55e9e73e5189e3873ffaa17d45c2      1
    125 0xad79626c73b520a441fab760ceec7ad65e4b6681      1
    126 0xb6cf777e3696a502107417265c92d1b075636a10      1
    127 0xb727b626e2eda55b804bc4a658e6ff0a4b0f7369      1
    128 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    129 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    130 0xbaa02edb7cb6dc2865bc2440de9caf6a9e31f23e      1
    131 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    132 0xbaff6c3f45fe23711abc53bcbe1acf93a6359cef      1
    133 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    134 0xbdf5ff3524e9062e10c0e6f29234ad67d4adbd01      1
    135 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
    136 0xbfae66905244be67431f84af82fcb04ae3899e80      1
    137 0xc0fb55eb408a4e12773139ae152da413d288f3c2      1
    138 0xc100c2d3531c647822991dda31f3ad2eaaa60ec3      1
    139 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    140 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
    141 0xc704389a3f999f325d568df1ab676d4b750f7c37      1
    142 0xc7c717f8dde3e55f7ca7c045a3dad0ac2fed7c14      1
    143 0xc7db4c95e3050c73db2da4041a03c24df14fd024      1
    144 0xc8a803eabd096e93a4d3e9b97dec4fa06fe7783e      1
    145 0xc976f3841c1e39ef84b5766691ee1ad1951ca65f      1
    146 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    147 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
    148 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    149 0xcdd99ee657a33b1da6802f4117d7e5cb2ffa5d79      1
    150 0xcddcbd9c2665a71cd4cf5d1fac0740bf7643f260      1
    151 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    152 0xd0d521cf3c2125b35c9a74fdb3b839214b1491a7      1
    153 0xd387a6e4e84a6c86bd90c158c6028a58cc8ac459      1
    154 0xd6756f7032cf147d9672968f1e27416071341a42      1
    155 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    156 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    157 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    158 0xda4ff7b6a1eb3176c347a3ffc56a26ee82fb6893      1
    159 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    160 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    161 0xdfe279c336e767beb9fb1c19c1695e2000a2c720      1
    162 0xe4ae3533810f3ce97dd9b89031ddcc97ba91756c      1
    163 0xe4e848c9e08133d85acb1cd09f631b3dee54235a      1
    164 0xe54a00d5418b4b36037edc9302c4ac2418d714af      1
    165 0xe5798a530bb7105e148d38ac884f05c28ed8e804      1
    166 0xe628d7135485ffae3c1dd9b07e9cbe3815fecb74      1
    167 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    168 0xe8cc7c453eca25450ca16d551db93d5f5740e054      1
    169 0xe9cb56a31a8aaaa28dfd30a037fbcf470bce1e2e      1
    170 0xedad1046c0876d4f92b4f2b20a6068543c36de5a      1
    171 0xefe3f8fa7fa99d77f8e50c8484184906cfe02d12      1
    172 0xf041c4f026547f7fbea6904f77bea16997024751      1
    173 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    174 0xf2439241881964006369c0e2377d45f3740f48a0      1
    175 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    176 0xf31f591b3dcd383047d7045de96a200a09e4dde1      1
    177 0xf36424a56aa6cbbabef69534e64c2f9c93153024      1
    178 0xf39e356b26630115dd171f1d41640c45cec9ab21      1
    179 0xf3df5c1ba015845bade343252d6dbeab1bbf209b      1
    180 0xf50bdeafecca3a679d26ae4c2dec953e032a6fda      1
    181 0xf68c5e1770e5aa9f6f7d52250ac91412766c2ef9      1
    182 0xfd29534c497a8ab8c85d7f560eb35cffc0bf77b5      1
    183 0xfe3107bbba13af26d6a38c19c5a24790d8c6eabe      1

## Allow Random Memes Phase 1

``` r
c(allow_memes69_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random69memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x087e14b18e87367a7cc58f04989d749b8a0e71a1      1
     2 0x0aaf7ea77d6df1761da21f269f65dc5c0db22fea      1
     3 0x0ca3a3b144148642f68b88bded521ce637db8f3e      1
     4 0x0f22657ddcf96c4d8e7cd22f0ab18fbfb579e551      1
     5 0x123cd3fbcae960bbb0e7f68593b453186065c6d6      1
     6 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     7 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
     8 0x18799f860f9adb26e076d581603037f79342e0c0      1
     9 0x1ce4d8f0362efa9d16eabd90a2e6eb683e7d24e0      1
    10 0x1e3a1348881f6d0a5e1cdc6bf19b88d98399b15a      1
    11 0x2135589bdf50c0226393c48514612145e2d3bedf      1
    12 0x2141f82d131900acc49eb42730ec8935168444b8      1
    13 0x2c90d7a5e09dbfe0c017ed2106b428fca7686c62      1
    14 0x2e22e216c57a69d937ab0215f8791cc1357cc4fe      1
    15 0x352216a5b428fbee3dc882a2c268a1669fb8b2a5      1
    16 0x3addc2ce68d7f94af141a3bbcb86aaca1ec4494f      1
    17 0x3b9818a507afb5b6865152a29d5f762e2f8d458d      1
    18 0x3c176a32895eedcd93db8d97682cd08fc1305c52      1
    19 0x3c71982c02b15418070ce1a53a1d7efb9500aa48      1
    20 0x3e02aa6b648394769d9094838cdc100962d33e72      1
    21 0x3e89a0f26657013bf251ced30d5d198c3b0d2466      1
    22 0x418bee1f71a547a370eca0c8a98b622870c5682c      1
    23 0x435a4ccc3712b0b40ce16a1c7bfa263ed6e9bb33      1
    24 0x4428dd9585e610521cba37c59b2a415154d875fc      1
    25 0x4762b5ac03fc097a500b48cfefffc027e8b37227      1
    26 0x4bb0788383b3f558396369e9936712e8a6527240      1
    27 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    28 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
    29 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    30 0x55d647550ac4dee50e00edf13debee5404f4c452      1
    31 0x5c7ef5ae70072f7bdc3a546ce86b9e316af9f57b      1
    32 0x5d2bd71575f064e1f103c276f5f59697faf17d53      1
    33 0x634ae4b57e6246f0318257743e5255648f9473a6      1
    34 0x6a32300ecfc84accfa9a4e1200457ee4a22c7d3c      1
    35 0x6c359ab465ec8f893fb6c4243810f4d8933d71b5      1
    36 0x7026f77c6940f1280fdaf606a8a87a9c8ced1eea      1
    37 0x70af781f6851b057adcd963da042aaca1915050b      1
    38 0x7b768c81353cd5ff73b74e72dc7409cf789436a3      1
    39 0x85d8e62e0e58e361aee49364e67ac6db5e8e38d1      1
    40 0x87f7f3dd57afef06b7ab16c9b0bcf9eed9fac9b4      1
    41 0x9c31f5112faa5f8996562e2a2139988e3db63063      1
    42 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    43 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
    44 0xa4bc9e71d8fa39a76dc8f69794b7d8bc383e6d58      1
    45 0xa954b7233974944a681957de8385b46b855ffd41      1
    46 0xac01ad9a3e3c3448a5a0e6fc798def034a01a67c      1
    47 0xad30b79c749c458601ddf4817dd768c0b4fd932a      1
    48 0xad5bd06f5fb4633888ac7506ec0c87eb99998653      1
    49 0xaeff8f5eee811039bd087bcf84742ba9c4ecced9      1
    50 0xb3866e0292d10ee4bf69534479b5113697d1c681      1
    51 0xb44c873fedf6606757a3b76f56604806ed620d49      1
    52 0xb9552a89286e7b3feaf7a5048873aa770f7906d0      1
    53 0xbeb15cb4e910fe9e730c48f55a4de49212203d10      1
    54 0xc8d3e10a72d2d846ebd134dfbd23e14cc26a4ed5      1
    55 0xcae2874732acb94ff38c98b5812395f0f342fd69      1
    56 0xcfaedb4d9a764fefca8126d2bcb2a7f233aadf9d      1
    57 0xd070ada7f4a088444e61bbfc1fe6a31d87ea0e92      1
    58 0xd5d290d2638f4fefbda64895bbfeaef52aa5fac6      1
    59 0xd86aed968fb30957a2a55151bb9e226bdd90bf64      1
    60 0xdab22cab9d6bd85872c7507b9470ce9727aa22e2      1
    61 0xdbe2258624f94ab8ee30ceb67b2a078b24bb6d6d      1
    62 0xddf14fa3e1bf3881cd6ec491f0ccf3d1389a78d4      1
    63 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    64 0xe6d5ec576a21924dfbca631d8128f5b4272a7fe6      1
    65 0xeccd4cc37ede429cb77d409782f5b7bfdbe172b5      1
    66 0xed3f868fa921974f4addbfc982b1298c82dc6cd5      1
    67 0xedc1040e47f75ff85867ff4358c6229801af31f8      1
    68 0xeed61878614473ce2f6ec680421a06c7ec6753dc      1
    69 0xfc382669ecf6a4f19efe1e32c64d75a1daffb1e1      1

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
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
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
    45 0x8252df1d8b29057d1afe3062bf5a64d503152bc8      1
    46 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    47 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    48 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    49 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
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
