
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:2066        Length:2066        Min.   : 1.00   Length:2066       
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.07                     
                                           3rd Qu.: 1.00                     
                                           Max.   :25.00                     
         name          
     Length:2066       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 21296969 # https://etherscan.io/block/21296969
block_hash <- "0xd585e5d015d0adb3c3d63e58733c0216620d23aa7211bcac8757e6dc7a8f3444"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4583 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","GRXXD","PIRANESIELEMENTS","Fieldsofchain","MABLABROCK","MABLAB","Frames","MarsMeteorite","EventHorizonLandscape","KnownOrigin","KnownOrigin2"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("MABLABeditions","MABLABeditions2","MABLABeditions3"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","GRXXD","PIRANESIELEMENTS","Fieldsofchain","MABLABROCK","MABLAB","Frames","MarsMeteorite","EventHorizonLandscape","KnownOrigin","KnownOrigin2","MABLABeditions","MABLABeditions2","MABLABeditions3"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000f59bfbcdfff89ae5bb910e984ffaebcf652cc      1
      2 0x087f6883133b0070e5d60d74b0820c7be4f69391      1
      3 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
      4 0x0c57daae59367c1b26cd337fdc0b69356f31ffa9      1
      5 0x11d7c4af8960cab1324893dd06be55b28580299c      1
      6 0x13fd513c2104941bc399589b5391957b27392e8b      1
      7 0x1445cbc4333df7a8cd1c234026d717e8e9163bfa      1
      8 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
      9 0x163c928f7a5829415d6467d8bf4c130afc1082db      1
     10 0x190f49c0f92a2c10aca57108b8ccd49416c22d25      1
     11 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     12 0x1ff2aa2beb5b6c33a56e97022f5760cb3195ff2e      1
     13 0x213f6b2680dce3e4d0924acff3e4e34520ef9ba1      1
     14 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     15 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     16 0x321fdfcf79eeaed39fc67daaca07d92457129d4f      1
     17 0x33195e40a838209d27810b36de6ff90a383ce8cd      1
     18 0x34b93462e65303f3857460971584fd0d908f2f45      1
     19 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     20 0x389d43178ad6076521c7f2ca19beec806ef00d2a      1
     21 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     22 0x3e3588fc0dcee9cf9d2241d6353db5ccfd3a8e19      1
     23 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     24 0x3f84bc2c6f60f87db9187df42b4784f76fd30d3a      1
     25 0x3fd1de859c27d0ec4699f28466b77d8e71bd7ad0      1
     26 0x441c155c569ca5c3cecb37b5ab759c1fd6c0aa67      1
     27 0x4718d398dd4b0ec472700bd1f6091ed82c49c256      1
     28 0x49ff66d90c67ca99feaa5e8e473396d5c25aa71a      1
     29 0x4b5bf7fcccbd98450b215b619b7dbdb036a3dd46      1
     30 0x4d1c149e6728314d0d6c4f7d48dbf83bd196444e      1
     31 0x4dae7e6c0ca196643012cdc526bbc6b445a2ca59      1
     32 0x525022ecd0de305f714e108d3b4ce68928c2d81f      1
     33 0x54c375c481f95ba43e2cecd6ef30631f55518f57      1
     34 0x576a655161b5502dcf40602be1f3519a89b71658      1
     35 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     36 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
     37 0x5e4bea43eb527650686f219ab682865493850edb      1
     38 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     39 0x616ed675fde921be584dc6376ae67d0a2fc27adc      1
     40 0x61ac983b4b8bd94f64ed44778096572205e27e87      1
     41 0x6511d993b38b5afbf29f66023ca850b01abe80e0      1
     42 0x6632b1b4b13669e22b4ac7247c853f35acbe48fa      1
     43 0x6b44862103b8a45f5ec701b69ec28a5d6d304950      1
     44 0x70169f427e3352c720f1749c79b6d2245f7c5c02      1
     45 0x70cfb1fdd807dee18764c805da7517ad30f47009      1
     46 0x734bb23e9eafe199d808b4d3cc4fadd66799da2d      1
     47 0x76807f3d69b1d38f9d597e902f18dab0af926b8b      1
     48 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     49 0x7c632d97febebafa33f12d750bfc378906341263      1
     50 0x7d6d89dbe3d4f23a13c1c7c9c4f7f656a04250ea      1
     51 0x83cfbf713e60b0dbb093eff8659284c23c20ff27      1
     52 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     53 0x86d89966f507bbffddf9a5dff22ef5609b0c80dc      1
     54 0x8751ac343552314ee263ef16d66351c04c380891      1
     55 0x88be96dc047cde01286e991f7333a2e9a4865856      1
     56 0x88dbaf595af3efcf3f8ecdab834b9299afc67d42      1
     57 0x8eb514633f8fc1aee4b405f8165f3eeb42826f6d      1
     58 0x8f12219298c446b114c82420591e4c38fefcd323      1
     59 0x93c2d261f937ed79572f2474a96d5635c2d9f0fd      1
     60 0x952dd5887bd9a061768cec90e2ac8f3c1ed21480      1
     61 0x97203b7c4699230dedce5841966930b13ba3ec92      1
     62 0x975e0e9456aca3b4933f5d6b9f0c98225cfa94cf      1
     63 0x99ffa0a514e1c43b5baa7f64a74f394ea277b3dd      1
     64 0x9a659894e5d115846767db0e1685744c452e7a6e      1
     65 0x9e2b7e0ed9911e83fcdadf591cfe8fac320bcad3      1
     66 0x9e9081492c8c8bb906bb66da9621241d4b14f9a5      1
     67 0xa14fe103bc14c6a88f2784f2e018fd71a25e46b7      1
     68 0xa22414e4af5767448624017c7e0151d22490412b      1
     69 0xa2bbf371015ff4f6d8fd2671126ca8f630a0eabc      1
     70 0xa5e5d3afe7daf372972559c7fa3b75cb7a4ae0d7      1
     71 0xaabfabb07fa60375a2f7731c341601c499eadcf8      1
     72 0xaf252c2990915006ef9601bfb97e234d99475e50      1
     73 0xb1af684799c96f5c740786be59dc834ff8ef5add      1
     74 0xb7d3a787a39f25457ca511dc3f0591b546f5e02f      1
     75 0xb7f73311a823fa70059cf6e22a842c3bd64c53c3      1
     76 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
     77 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     78 0xc2587a1685b194831c0e4a8927df0c33437fca3f      1
     79 0xc723b407673e10c3614906b39e72c03a2b3fac9d      1
     80 0xc875c65601a98761c5dd38ae8ca34f718277bf3f      1
     81 0xce9c62ff24e5653de248ced49c218e6e23caedbc      1
     82 0xd4f802ec4f588952a3a92fce0d5d3629e78d4f57      1
     83 0xd583ed342689782e3f865740b706e2f9dd17d56a      1
     84 0xd8be18a8b1d7d469f3482b4ae8343cce0b533a16      1
     85 0xdac8fc039f633969116d412522b3338e3f1eba44      1
     86 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
     87 0xdeb0804d5e54d49c1923da459730e66b60eead6b      1
     88 0xdf85b9146d73a852d7b900d1abd974f2a8119f57      1
     89 0xe07f46f58f5a1aee8b9e20a16f8cb4ee868e6c25      1
     90 0xe113e7b1e8ecd07973ea40978aed520241d17f27      1
     91 0xe48ba53386cf2d32c557ca116ab710a615ffbc83      1
     92 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
     93 0xe96c3e1d842beccb10bf57c453b84bf75725241d      1
     94 0xf08889f3b4f5a56482fc295db2ffc7301f888ca0      1
     95 0xf1db8a2623193757317639d0532daa5e3c8ea20c      1
     96 0xf49562560f99900a16bcba5577b42646a599944b      1
     97 0xf52393e120f918ffba50410b90a29b1f8250c879      1
     98 0xfaa9dccf86f5cd4db872377c18368b5d58bc7b38      1
     99 0xfd2de1e6008d29286f780c3ee561b297625a8538      1
    100 0xfee2a5405a2f372f9de7fabf7b2a3ec675217fc0      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 99 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x07f1bfd75f05c3fa6fd9d483b856811a9c40bf32      1
     2 0x07f6329cdd8c29a241a0ad8503436c6652f6b509      1
     3 0x189895af3d4fa80972bafff92a0d39c35e48d0cf      1
     4 0x19443b60f0047551b16491a65e0a01486a2cd9a0      1
     5 0x1bfe8143b7343bc6f31ae5b53c6a3e0c825fa3de      1
     6 0x1cf9e3022e521e2a273049731fc9aff7a3682044      1
     7 0x212c7d24fab3ceaeae2951ebf942542104698fad      1
     8 0x239f0e87c1a1196d2b95a233d809a743a22cf185      1
     9 0x2547111297038dd45bb1b7bd21450197d4af90ad      1
    10 0x2638f91804b32190622ef82c6a265d8e9dae57a6      1
    11 0x26a1e46832bd5ffd7d21481d185fc258a7080316      1
    12 0x26fca9a99e95be90447a25a1da852d870417aac9      1
    13 0x270c943a37a8a6bc2845a846e33e24ea0c96abd0      1
    14 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
    15 0x2b97b1ac7fdc2ec487f0f4a8d15dd3de8e82968b      1
    16 0x2c4a6fd81e52d256ec34db1e03a9af627b2d62c5      1
    17 0x2f01879ddc136f2afe2eaee7dad3dc029d8c0417      1
    18 0x300da191248a500b2174aed992d6697bf97f9139      1
    19 0x336bd505b648d378a251f06ec3f6693620913f92      1
    20 0x33d72423b6a0fa80832ddfcf123762c27c032c41      1
    21 0x36885851f71f09a087039260452e4756af87d9ef      1
    22 0x369615bc57975b06c418f42d3fda3754a385b97b      1
    23 0x38f1dfdcaf2f0d70c29d4af6a4aa9e920efe8b18      1
    24 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    25 0x3d55da88ebc54bcb91e07973c8f920835bcb1b1c      1
    26 0x3ec4483cdb2bccc637ef2c94e8f6eeeb8247823b      1
    27 0x3f4804330a0d720463cf9932656ae14e7116fc12      1
    28 0x416e92f37fb344ee99b7af6340025434b59f13bb      1
    29 0x425f54838b350fa757e8748d6ff9344a402beaa8      1
    30 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
    31 0x4629bff25144473be9a67fb807d4c8df62455065      1
    32 0x48c910f86993e5b090e8ef9f62daf099147cfd43      1
    33 0x492ff138d76aa034607a7f14f72bad34322f981a      1
    34 0x49bb5e19f3ffb6e166c6ebb988d3a7e0aadbfe04      1
    35 0x4babfcbdb9cf95eac6024cc911d6c1163a144a76      1
    36 0x4d5e59ff1e193946a149d3f42b7b1f973ccbb716      1
    37 0x4d6667739da3bdec58c0ad3f41bb617edc256239      1
    38 0x5164cf3b0c8c0fdfe4bcc9cf4f1e8f7e39461a59      1
    39 0x519f95380bed75a307dd16d53842d83f342efa3d      1
    40 0x56879cc88fa3895c082c22035db1386dcac53bba      1
    41 0x575f53510b1a6a05c3582390c410d8e050718251      1
    42 0x5e6de9dfc0cfa60abf42b17cd36fcd01b36708c7      1
    43 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
    44 0x6519f68ca26c7f59deeabce7194da4d14e1f2847      1
    45 0x651e0f50bf70ff2524ee1fc2aeb85dc2ab46a020      1
    46 0x65c1ccacf103fbb1bdc26bd996e76276a7811b45      1
    47 0x673d1d29b4f4466083045c8042cafa0cec081271      1
    48 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
    49 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
    50 0x6d079216c1bebbae518d30ca6e2f1485a3cc0f75      1
    51 0x765718a8e4a5562be34b122af259ce5a71372e12      1
    52 0x765ae31119d7f56b0179ae28917f8f2b49ebae13      1
    53 0x7862e990963405616afd1e08cd369433a87adb3a      1
    54 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    55 0x7ee10fc321f98aae526e3c9f2b7682517e3d3d64      1
    56 0x815e60f2ac840dcaa0b0bd6f9d1d9c3779ef103d      1
    57 0x85898fab8f22f29d4c34db01caddab1d0831a8d9      1
    58 0x869e92be652a6541a8b1d6e5a34799b89a96e64e      1
    59 0x875fa181a5bfcaae0f37c41dc88219eef192aeae      1
    60 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    61 0x898765e04fc0b18f809e02622615b238e9af5ced      1
    62 0x8b32b2dc502ce6d81b1f2c44e946b1a47327ace0      1
    63 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    64 0x8e8cf8791d52e253402bbc3aaf6ed4ff4d7559fe      1
    65 0x91f7bbbd59b1e9525711b9e84e15971e6fbebdfb      1
    66 0x94542231fed4a37f7372ef6fb158e32deabe1c11      1
    67 0xa2b68750c6eaed70380b90b1bae1e3470fcf06be      1
    68 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
    69 0xa559cf3e51c7715548179ecfe0e3c2fe6613be49      1
    70 0xab55d3fe744a9b0074243e3be63d4636554ed5b1      1
    71 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    72 0xb523170d7d9010a5046e7753ee8c814ad9ec0c05      1
    73 0xb7d829c11a4a45830d995f5b8c66d92ff276ce9e      1
    74 0xc1921b07527832e7453b6fd465ea664723cfee32      1
    75 0xc3edcbe0f93a6258c3933e86ffaa3bcf12f8d695      1
    76 0xc458e1a4ec03c5039fbf38221c54be4e63731e2a      1
    77 0xc704c58cfa7601a6645048a6e577732319e889b2      1
    78 0xc7fb22b28cb51753ad33616bcdcafc46bdc3c458      1
    79 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    80 0xcbe80c1faf915cde4c685b7116a68b47a25da326      1
    81 0xcc9b3f50d1befda707ff0b1a808e4e1f1aa15eb4      1
    82 0xcda1111a8c7631891d58e76fd4e5ec42a60ebdf1      1
    83 0xd3aefe3c531e3e2eb0689206e7d495843c943550      1
    84 0xd3fac37e04a928f581459ea81d7b1e2dc64c2fbb      1
    85 0xd69e257ae6088b717ae6d2ddec9297703b4fb725      1
    86 0xd8404763bce69672b42d9409f3ac9b4b2e684cfb      1
    87 0xdb1345a4a059280fd1511339e372efd026f2f6db      1
    88 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    89 0xe02cfbd6d5cdf3035bad9a37b6141c1bf76a1b6c      1
    90 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    91 0xe3fcb080f46ad0581b8ce75ba1873d61a26f6c1f      1
    92 0xe44999fc4ae55e4ab165d1b36d4b31df5649a921      1
    93 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    94 0xeaf7ff61960232696f7d16a2395cf030b26282a7      1
    95 0xed65a6ea9c2b4f3ffa084ee3a2bb0ccc97bf0bf0      1
    96 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    97 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    98 0xfa7aa0a1c778a7933a632245445ed11e352355f9      1
    99 0xfc9f1a82d3a18b317ca8d719e9fbfa6960417d06      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 199 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x000f59bfbcdfff89ae5bb910e984ffaebcf652cc      1
      2 0x07f1bfd75f05c3fa6fd9d483b856811a9c40bf32      1
      3 0x07f6329cdd8c29a241a0ad8503436c6652f6b509      1
      4 0x087f6883133b0070e5d60d74b0820c7be4f69391      1
      5 0x0c48427ec3f8eb65430c6812d3eed00669965742      1
      6 0x0c57daae59367c1b26cd337fdc0b69356f31ffa9      1
      7 0x11d7c4af8960cab1324893dd06be55b28580299c      1
      8 0x13fd513c2104941bc399589b5391957b27392e8b      1
      9 0x1445cbc4333df7a8cd1c234026d717e8e9163bfa      1
     10 0x146d745139d417b8b5a1190cc73b34d7d37a9bba      1
     11 0x163c928f7a5829415d6467d8bf4c130afc1082db      1
     12 0x189895af3d4fa80972bafff92a0d39c35e48d0cf      1
     13 0x190f49c0f92a2c10aca57108b8ccd49416c22d25      1
     14 0x19443b60f0047551b16491a65e0a01486a2cd9a0      1
     15 0x1bfe8143b7343bc6f31ae5b53c6a3e0c825fa3de      1
     16 0x1cf9e3022e521e2a273049731fc9aff7a3682044      1
     17 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
     18 0x1ff2aa2beb5b6c33a56e97022f5760cb3195ff2e      1
     19 0x212c7d24fab3ceaeae2951ebf942542104698fad      1
     20 0x213f6b2680dce3e4d0924acff3e4e34520ef9ba1      1
     21 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     22 0x22cd049c57f070fd6bde748e493aa2627621ebf7      1
     23 0x239f0e87c1a1196d2b95a233d809a743a22cf185      1
     24 0x2547111297038dd45bb1b7bd21450197d4af90ad      1
     25 0x2638f91804b32190622ef82c6a265d8e9dae57a6      1
     26 0x26a1e46832bd5ffd7d21481d185fc258a7080316      1
     27 0x26fca9a99e95be90447a25a1da852d870417aac9      1
     28 0x270c943a37a8a6bc2845a846e33e24ea0c96abd0      1
     29 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     30 0x2b97b1ac7fdc2ec487f0f4a8d15dd3de8e82968b      1
     31 0x2c4a6fd81e52d256ec34db1e03a9af627b2d62c5      1
     32 0x2f01879ddc136f2afe2eaee7dad3dc029d8c0417      1
     33 0x300da191248a500b2174aed992d6697bf97f9139      1
     34 0x321fdfcf79eeaed39fc67daaca07d92457129d4f      1
     35 0x33195e40a838209d27810b36de6ff90a383ce8cd      1
     36 0x336bd505b648d378a251f06ec3f6693620913f92      1
     37 0x33d72423b6a0fa80832ddfcf123762c27c032c41      1
     38 0x34b93462e65303f3857460971584fd0d908f2f45      1
     39 0x36885851f71f09a087039260452e4756af87d9ef      1
     40 0x369615bc57975b06c418f42d3fda3754a385b97b      1
     41 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     42 0x389d43178ad6076521c7f2ca19beec806ef00d2a      1
     43 0x38f1dfdcaf2f0d70c29d4af6a4aa9e920efe8b18      1
     44 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     45 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     46 0x3d55da88ebc54bcb91e07973c8f920835bcb1b1c      1
     47 0x3e3588fc0dcee9cf9d2241d6353db5ccfd3a8e19      1
     48 0x3ec4483cdb2bccc637ef2c94e8f6eeeb8247823b      1
     49 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     50 0x3f4804330a0d720463cf9932656ae14e7116fc12      1
     51 0x3f84bc2c6f60f87db9187df42b4784f76fd30d3a      1
     52 0x3fd1de859c27d0ec4699f28466b77d8e71bd7ad0      1
     53 0x416e92f37fb344ee99b7af6340025434b59f13bb      1
     54 0x425f54838b350fa757e8748d6ff9344a402beaa8      1
     55 0x441c155c569ca5c3cecb37b5ab759c1fd6c0aa67      1
     56 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     57 0x4629bff25144473be9a67fb807d4c8df62455065      1
     58 0x4718d398dd4b0ec472700bd1f6091ed82c49c256      1
     59 0x48c910f86993e5b090e8ef9f62daf099147cfd43      1
     60 0x492ff138d76aa034607a7f14f72bad34322f981a      1
     61 0x49bb5e19f3ffb6e166c6ebb988d3a7e0aadbfe04      1
     62 0x49ff66d90c67ca99feaa5e8e473396d5c25aa71a      1
     63 0x4b5bf7fcccbd98450b215b619b7dbdb036a3dd46      1
     64 0x4babfcbdb9cf95eac6024cc911d6c1163a144a76      1
     65 0x4d1c149e6728314d0d6c4f7d48dbf83bd196444e      1
     66 0x4d5e59ff1e193946a149d3f42b7b1f973ccbb716      1
     67 0x4d6667739da3bdec58c0ad3f41bb617edc256239      1
     68 0x4dae7e6c0ca196643012cdc526bbc6b445a2ca59      1
     69 0x5164cf3b0c8c0fdfe4bcc9cf4f1e8f7e39461a59      1
     70 0x519f95380bed75a307dd16d53842d83f342efa3d      1
     71 0x525022ecd0de305f714e108d3b4ce68928c2d81f      1
     72 0x54c375c481f95ba43e2cecd6ef30631f55518f57      1
     73 0x56879cc88fa3895c082c22035db1386dcac53bba      1
     74 0x575f53510b1a6a05c3582390c410d8e050718251      1
     75 0x576a655161b5502dcf40602be1f3519a89b71658      1
     76 0x5849734d2e3ada23b64e12311adfa6bcd6fe687c      1
     77 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
     78 0x5e4bea43eb527650686f219ab682865493850edb      1
     79 0x5e6de9dfc0cfa60abf42b17cd36fcd01b36708c7      1
     80 0x5f2abc70bb1567c7807da87e944ddf965a0846bd      1
     81 0x5fc13c5a541041f8fa12fab492d064435eb8d2f5      1
     82 0x616ed675fde921be584dc6376ae67d0a2fc27adc      1
     83 0x61ac983b4b8bd94f64ed44778096572205e27e87      1
     84 0x6511d993b38b5afbf29f66023ca850b01abe80e0      1
     85 0x6519f68ca26c7f59deeabce7194da4d14e1f2847      1
     86 0x651e0f50bf70ff2524ee1fc2aeb85dc2ab46a020      1
     87 0x65c1ccacf103fbb1bdc26bd996e76276a7811b45      1
     88 0x6632b1b4b13669e22b4ac7247c853f35acbe48fa      1
     89 0x673d1d29b4f4466083045c8042cafa0cec081271      1
     90 0x68bca5a8bdebe05fb8a6648c7316b4eb7e19a064      1
     91 0x6b44862103b8a45f5ec701b69ec28a5d6d304950      1
     92 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
     93 0x6d079216c1bebbae518d30ca6e2f1485a3cc0f75      1
     94 0x70169f427e3352c720f1749c79b6d2245f7c5c02      1
     95 0x70cfb1fdd807dee18764c805da7517ad30f47009      1
     96 0x734bb23e9eafe199d808b4d3cc4fadd66799da2d      1
     97 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     98 0x765ae31119d7f56b0179ae28917f8f2b49ebae13      1
     99 0x76807f3d69b1d38f9d597e902f18dab0af926b8b      1
    100 0x7862e990963405616afd1e08cd369433a87adb3a      1
    101 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
    102 0x7c632d97febebafa33f12d750bfc378906341263      1
    103 0x7d6d89dbe3d4f23a13c1c7c9c4f7f656a04250ea      1
    104 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    105 0x7ee10fc321f98aae526e3c9f2b7682517e3d3d64      1
    106 0x815e60f2ac840dcaa0b0bd6f9d1d9c3779ef103d      1
    107 0x83cfbf713e60b0dbb093eff8659284c23c20ff27      1
    108 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
    109 0x85898fab8f22f29d4c34db01caddab1d0831a8d9      1
    110 0x869e92be652a6541a8b1d6e5a34799b89a96e64e      1
    111 0x86d89966f507bbffddf9a5dff22ef5609b0c80dc      1
    112 0x8751ac343552314ee263ef16d66351c04c380891      1
    113 0x875fa181a5bfcaae0f37c41dc88219eef192aeae      1
    114 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    115 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    116 0x88dbaf595af3efcf3f8ecdab834b9299afc67d42      1
    117 0x898765e04fc0b18f809e02622615b238e9af5ced      1
    118 0x8b32b2dc502ce6d81b1f2c44e946b1a47327ace0      1
    119 0x8c1f393c989c667ad20e3d9df72a4f778c5c64b2      1
    120 0x8e8cf8791d52e253402bbc3aaf6ed4ff4d7559fe      1
    121 0x8eb514633f8fc1aee4b405f8165f3eeb42826f6d      1
    122 0x8f12219298c446b114c82420591e4c38fefcd323      1
    123 0x91f7bbbd59b1e9525711b9e84e15971e6fbebdfb      1
    124 0x93c2d261f937ed79572f2474a96d5635c2d9f0fd      1
    125 0x94542231fed4a37f7372ef6fb158e32deabe1c11      1
    126 0x952dd5887bd9a061768cec90e2ac8f3c1ed21480      1
    127 0x97203b7c4699230dedce5841966930b13ba3ec92      1
    128 0x975e0e9456aca3b4933f5d6b9f0c98225cfa94cf      1
    129 0x99ffa0a514e1c43b5baa7f64a74f394ea277b3dd      1
    130 0x9a659894e5d115846767db0e1685744c452e7a6e      1
    131 0x9e2b7e0ed9911e83fcdadf591cfe8fac320bcad3      1
    132 0x9e9081492c8c8bb906bb66da9621241d4b14f9a5      1
    133 0xa14fe103bc14c6a88f2784f2e018fd71a25e46b7      1
    134 0xa22414e4af5767448624017c7e0151d22490412b      1
    135 0xa2b68750c6eaed70380b90b1bae1e3470fcf06be      1
    136 0xa2bbf371015ff4f6d8fd2671126ca8f630a0eabc      1
    137 0xa3b11d1f06d71eaa9cd3d0142f08e7ace9b474ec      1
    138 0xa559cf3e51c7715548179ecfe0e3c2fe6613be49      1
    139 0xa5e5d3afe7daf372972559c7fa3b75cb7a4ae0d7      1
    140 0xaabfabb07fa60375a2f7731c341601c499eadcf8      1
    141 0xab55d3fe744a9b0074243e3be63d4636554ed5b1      1
    142 0xaf252c2990915006ef9601bfb97e234d99475e50      1
    143 0xb1af684799c96f5c740786be59dc834ff8ef5add      1
    144 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
    145 0xb523170d7d9010a5046e7753ee8c814ad9ec0c05      1
    146 0xb7d3a787a39f25457ca511dc3f0591b546f5e02f      1
    147 0xb7d829c11a4a45830d995f5b8c66d92ff276ce9e      1
    148 0xb7f73311a823fa70059cf6e22a842c3bd64c53c3      1
    149 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    150 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    151 0xc1921b07527832e7453b6fd465ea664723cfee32      1
    152 0xc2587a1685b194831c0e4a8927df0c33437fca3f      1
    153 0xc3edcbe0f93a6258c3933e86ffaa3bcf12f8d695      1
    154 0xc458e1a4ec03c5039fbf38221c54be4e63731e2a      1
    155 0xc704c58cfa7601a6645048a6e577732319e889b2      1
    156 0xc723b407673e10c3614906b39e72c03a2b3fac9d      1
    157 0xc7fb22b28cb51753ad33616bcdcafc46bdc3c458      1
    158 0xc84d2bc55399de1f181c5adc01ee9b4e9a1f5efd      1
    159 0xc875c65601a98761c5dd38ae8ca34f718277bf3f      1
    160 0xcbe80c1faf915cde4c685b7116a68b47a25da326      1
    161 0xcc9b3f50d1befda707ff0b1a808e4e1f1aa15eb4      1
    162 0xcda1111a8c7631891d58e76fd4e5ec42a60ebdf1      1
    163 0xce9c62ff24e5653de248ced49c218e6e23caedbc      1
    164 0xd3aefe3c531e3e2eb0689206e7d495843c943550      1
    165 0xd3fac37e04a928f581459ea81d7b1e2dc64c2fbb      1
    166 0xd4f802ec4f588952a3a92fce0d5d3629e78d4f57      1
    167 0xd583ed342689782e3f865740b706e2f9dd17d56a      1
    168 0xd69e257ae6088b717ae6d2ddec9297703b4fb725      1
    169 0xd8404763bce69672b42d9409f3ac9b4b2e684cfb      1
    170 0xd8be18a8b1d7d469f3482b4ae8343cce0b533a16      1
    171 0xdac8fc039f633969116d412522b3338e3f1eba44      1
    172 0xdb1345a4a059280fd1511339e372efd026f2f6db      1
    173 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    174 0xdc7d0d897e1b7998c780d16b3c08482a74e71f33      1
    175 0xdeb0804d5e54d49c1923da459730e66b60eead6b      1
    176 0xdf85b9146d73a852d7b900d1abd974f2a8119f57      1
    177 0xe02cfbd6d5cdf3035bad9a37b6141c1bf76a1b6c      1
    178 0xe07f46f58f5a1aee8b9e20a16f8cb4ee868e6c25      1
    179 0xe113e7b1e8ecd07973ea40978aed520241d17f27      1
    180 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    181 0xe3fcb080f46ad0581b8ce75ba1873d61a26f6c1f      1
    182 0xe44999fc4ae55e4ab165d1b36d4b31df5649a921      1
    183 0xe48ba53386cf2d32c557ca116ab710a615ffbc83      1
    184 0xe85f832d9f551cf445a6e8ceaaee73ef88a340d6      1
    185 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    186 0xe96c3e1d842beccb10bf57c453b84bf75725241d      1
    187 0xeaf7ff61960232696f7d16a2395cf030b26282a7      1
    188 0xed65a6ea9c2b4f3ffa084ee3a2bb0ccc97bf0bf0      1
    189 0xf08889f3b4f5a56482fc295db2ffc7301f888ca0      1
    190 0xf1db8a2623193757317639d0532daa5e3c8ea20c      1
    191 0xf49562560f99900a16bcba5577b42646a599944b      1
    192 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    193 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    194 0xf6ec5b0d097178a0acf493afdcda2a54545ab0f3      1
    195 0xfa7aa0a1c778a7933a632245445ed11e352355f9      1
    196 0xfaa9dccf86f5cd4db872377c18368b5d58bc7b38      1
    197 0xfc9f1a82d3a18b317ca8d719e9fbfa6960417d06      1
    198 0xfd2de1e6008d29286f780c3ee561b297625a8538      1
    199 0xfee2a5405a2f372f9de7fabf7b2a3ec675217fc0      1

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
