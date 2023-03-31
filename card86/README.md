
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:1745        Length:1745        Min.   :1.000   Length:1745       
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.034                     
                                           3rd Qu.:1.000                     
                                           Max.   :6.000                     
         name          
     Length:1745       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16917269 # https://etherscan.io/block/16917269
block_hash <- "0xb39402875c86d60c3c984e73c2097fe79f115a437aa3c56a838fd289ceefaf08"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4634 

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

airdrop_popwonder   <- pick(snapshot, contracts=c("SuperRare","PopWonderWorld","OnDisplay","OnDisplay2","PopWonderPerks","PopWonderPerks2"), address_remove=address_remove,address_pick=10,address_max=1)


allow_singles_popwonder_phase2    <- pick(snapshot, contracts=c("SuperRare","PopWonderWorld","OnDisplay","OnDisplay2","PopWonderPerks","PopWonderPerks2"), address_remove=address_remove,address_subtract=airdrop_popwonder,address_max=1)
allow_editions_popwonder_phase2   <- pick(snapshot, contracts=c("LandAlternateDimension","LandFullMap","PopWonderLand","BassExtravaganza","Ascension","PopWonderEditions","PopWonderEditions2"),address_remove=address_remove,address_subtract=c(airdrop_popwonder, allow_singles_popwonder_phase2),address_pick=200,address_max=1)
allow_gradient_phase2             <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_editions_popwonder_phase3   <- pick(snapshot, contracts=c("LandAlternateDimension","LandFullMap","PopWonderLand","BassExtravaganza","Ascension","PopWonderEditions","PopWonderEditions2"),address_remove=address_remove,address_subtract=c(airdrop_popwonder,allow_singles_popwonder_phase2, allow_editions_popwonder_phase2),address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_popwonder) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x290d59a28b8cac6d358514c30b0e44d101d688ae      1
     2 0x2a3bccbbea5bd5f788cdadf2963f8e29c653cad7      1
     3 0x4897d38b0974051d8fa34364e37a5993f4a966a5      1
     4 0x668097bc23b4fb257626e884ecd8dc80b257c4f2      1
     5 0x6abc0b05b554212a6bcdc10f6bff26017d5adbb0      1
     6 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
     7 0xab4ead4014c3e4429fe53a56fdc256d1ea583ae9      1
     8 0xbbc30ddda94ac46496736215129b6b6f8c02bbf4      1
     9 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    10 0xdfd3b58cfc180168faebb6366b98aff25fe1f8f1      1

## Allow Artist Singles Phase 2

``` r
c(allow_singles_popwonder_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_singles_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 63 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x09ba0d8c6e10147660714388d4857260d9bd7958      1
     2 0x0c2de53b7f4520afd3f92ac793943cf90fbcfe13      1
     3 0x104c78d9b89684ab370ae68b1e0377e684b83b56      1
     4 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     5 0x174cc60fccb27c0571c9dfc2f2554be908e1051f      1
     6 0x1c2b1310c986cf71752a5a4b805444913722e1c9      1
     7 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     8 0x1e32a859d69dde58d03820f8f138c99b688d132f      1
     9 0x1fa8e196af3887d12dbc1b7b14202fed6f7c3352      1
    10 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
    11 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
    12 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
    13 0x307f93ac1b62e59c43d0d9e1a39950a5ec9a1357      1
    14 0x3a6ed06bd3737fe85863d3865d957ab0ed0b0644      1
    15 0x3ff23183a9c70665c14831e10cc6297b6ffd157c      1
    16 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    17 0x42f3f76ba5202d7a4c48fd428b5613c657689bcc      1
    18 0x440631dbdc0753e45241569c6d63552eac8e3130      1
    19 0x446a6560f8073919d8402c98db55db342a20300b      1
    20 0x47faf11173b705d077af8d3c0567c028b0ac33e2      1
    21 0x4d1572ea399cfcb0a4b25b364df2c5ba68697e18      1
    22 0x553a96c13b67d500182bb6ab46d53a2edfb22706      1
    23 0x6130b7313833f99956a364156b3329e50695bd65      1
    24 0x6ace1c1c9779b77640b539b12f618f777cb1dfcc      1
    25 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    26 0x78b5942e39add93e7aee98ea4440057acb527b49      1
    27 0x7d551b4aa5938d18be5c5e9fde7fece9566611ba      1
    28 0x7ff17fa91fb9272c8a293d9668dea218844bf316      1
    29 0x819884634b84b3f9b71abde27ad1c599d8327176      1
    30 0x824b25b219b5de8dc30895d48a71534504a7f95c      1
    31 0x830cc132dd66f6491ceaa20206f398247143d9cf      1
    32 0x8c57e23cbf902ed01a7600fef1b8d083efb24909      1
    33 0x8dbbca57ea56290efa14d835bbfd34faf1d89753      1
    34 0x9769334fc882775f4951865aa473481880669d47      1
    35 0x9bb524bccce50c6a3606cecd80cfaa38d7d4fc26      1
    36 0x9d2cefe8b9b6f53445aee03c6a63dd706c8b121d      1
    37 0xa482facaf77d9181156059bf43b95982ee3a3910      1
    38 0xa9995648fcac5b970cd25e141da08c6e0a063fc8      1
    39 0xb072eaa86fac49876e15dcb11c7b6e6e9652a4fa      1
    40 0xb1f3c577b81bed198c144b05b63ae185b313254f      1
    41 0xb3a4d18a160e1e96e704785e655c245ee649fb90      1
    42 0xb53349160e38739b37e4bbfcf950ed26e26fcb41      1
    43 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    44 0xb91347c4b453a14d916297528c06651ea4db85f8      1
    45 0xbec69dfce4c1fa8b7843fee1ca85788d84a86b06      1
    46 0xbf23b32f95694920c663c40416899e284aea1284      1
    47 0xbf4c53721e3677a6c3d10e1ae08918d0477297ad      1
    48 0xc117c06994b3df5ff9e373753b08deba3018864f      1
    49 0xc15753ae3b6099b8a3366b836433d6542645b876      1
    50 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    51 0xc449f005667bef849261b35accf931a4bace48fb      1
    52 0xc797a3672c0cc4cca963ecb282a0285218811d9a      1
    53 0xd64379c6380e08faab950c5e62a7f6295c7f9c79      1
    54 0xdbb857aa54c7a8dd29e73eef7edcbe6b757f9160      1
    55 0xe77a08d1513974f9cc89fd2eb043eb68355eed20      1
    56 0xecbabc7cd9900db8be4ab55831a066997eef1d2e      1
    57 0xecfeb34a2020154a9cd8a854f377588197744658      1
    58 0xed66ce7eee03790056ca5ba5ee61bc4f77ba2ded      1
    59 0xf26f2f6f86cf3e8832a07da6f053a66a7a45697d      1
    60 0xf3478bf2d1120590fbf4cf4544c70d20237037d8      1
    61 0xf3696a84edcc19cc9db92af3977b027969070912      1
    62 0xfad487ded5b60fee99f51df3b087382f24aeee13      1
    63 0xfb36a825ffa74200132aa44148eb4a9bf0bd919a      1

## Allow Artist Editions Phase 2

``` r
c(allow_editions_popwonder_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0035c775d807829cf9672fd2f37723c3d4e9ad5b      1
      2 0x01713a2fee332cec0339831e57583244012b7680      1
      3 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      4 0x02f5d9c2b5376f8b9150cf148aa88a5fdd5dcc50      1
      5 0x03818e6c14e7123ff9068a8999717b9befd7ca47      1
      6 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      7 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      8 0x0535c5a30b2a4699bead00215c21016608801575      1
      9 0x05cda24eeefd1f24f18dac398f5a612674d3ca5e      1
     10 0x07ad9d1373116b4e4602b2425f081729a6edab44      1
     11 0x08d5bd85e9cd8ab2d970995eb1357065ff12ae48      1
     12 0x0af5dc10fb8783a853e72bc91aed481c57c84cbc      1
     13 0x0bdd4d69ec9b16839eb1461cb5c1b946a3db6769      1
     14 0x0cfcd5950999f3f39a307fd67e6442f9199eae1a      1
     15 0x0d6fa7f770959105bce8236f52a055b59abf9e36      1
     16 0x0fb90b14e4bf3a2e5182b9b3cbd03e8d33b5b863      1
     17 0x1593c9ab842c36a926ae39cb2173731f9362e8fd      1
     18 0x15ab7dd2261026db55623d0da3946a3e022be19f      1
     19 0x177a8340bddc2041cc362aabf543dee4e394e3e3      1
     20 0x19c1350ff158e057cdd254ec9e6c459e08891a54      1
     21 0x1a3fcac25b69015f4110fddc689d11a8b3133975      1
     22 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     23 0x1d130d29b3906555030452f0f29cdb0b9750fd21      1
     24 0x1e815a8188f1b84564577c1c998f7e6b4706b752      1
     25 0x2133d9f050074f6c00fc9519117a04630916ba38      1
     26 0x2177da04f9479496c2292d6344d306aa49beb34a      1
     27 0x21eb81d8a6b4055e5d6b8a4a477567383bd0956a      1
     28 0x22b55a5b9977111bd66290a98666b6db7a4322bd      1
     29 0x24883e5f51e8c7da708183fd5388c22fcfef5ab4      1
     30 0x26dbcf7bb0e7826e087c691228b5f337143fbe16      1
     31 0x26e8633256144c30624a1c1e2cd4661aeb799025      1
     32 0x27493616036728733146e8b9886029683bcb9d24      1
     33 0x279775d31fbbfa8e589631ef49c6b3d0913803e1      1
     34 0x2a4d27abf7ebbce29131c9e8fefd3ab335aa4cce      1
     35 0x2aa48f410007b7380d2846d03142febbbedeb3d3      1
     36 0x2acc5eee90d77d94d61dfc9192eb4848ef403d3e      1
     37 0x2bc2ee8264464e8bec03b890c30fc222120f74e9      1
     38 0x2d89cc4e013db2908b877c51d39ff63982761c96      1
     39 0x2dfcc264b08a181bc7c38b9901059bab369a5a4e      1
     40 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     41 0x2f8b15c072254afad6ea69ef03cf29a3e232f7b4      1
     42 0x31a166d4d9fe2ef4f88ef1be15884ec42212375d      1
     43 0x321e64d977a8653a6a406fbcfd70a7e2d6db6925      1
     44 0x3278e49a89fc080df2b5cc379704c02508fd6b5c      1
     45 0x32d495f3ee6fffa388530ca0e3be5b2de2012f87      1
     46 0x35aa3f733958b7416669303945093c98819f77a6      1
     47 0x35be6874b36e5e6207f6d715e603fbc317d172df      1
     48 0x36ed861a319278e5b6c39028884b0ca93df105d0      1
     49 0x371c4cf0efeda222fd6461cd5e4f9d62ac61d78e      1
     50 0x3830e626e03a4cdfe923e15d35ec6eaa2a41917f      1
     51 0x384789422fac6347e9b4350de4117ae10ddedbd7      1
     52 0x39ac007e4bfc2a590dca8f89b46332e1130da246      1
     53 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     54 0x3eec521f2f3203683efb7f4259ebc0f52372dc50      1
     55 0x4129b4337d5a2f7278a02ff15bb1e4e548d8ee60      1
     56 0x42cca422354d95bf26c130bb31dca57995156ce5      1
     57 0x433a3ee1de0ff146a20b7c246e0d1d09e19d0b1a      1
     58 0x4426a261ac0080ca10ee024c2588c9dfcf6737fc      1
     59 0x446de57a85d7b8cec2ba273293b55605be27eeea      1
     60 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     61 0x4832606179a3506f3e64c1b04dfbe3a13a55d0f1      1
     62 0x4b1a52faba3e36f959ec18aeabbd222dd8e124eb      1
     63 0x4bedafef9de6c959d376310f45e5efc1cf7e30ff      1
     64 0x4cba33eb9f1bf2a33c7cd0fdbde82f00a6a83976      1
     65 0x4d19695f6560fd5034860b689d6e6d913cbf1d04      1
     66 0x4e4f5d368be4ca4b9f8809ecb8832fd8772cbdcd      1
     67 0x516c54c98a0f530ae1e3bb62e192683f386bd385      1
     68 0x5242f9383ddefd5d842b6e762ba5e73c07e270b8      1
     69 0x526525fe1222613defca291bcf176d9849d91c6d      1
     70 0x5397378174c121543bb3e21fe3ccbfc06dc5961f      1
     71 0x563b4e3be5452bd01852dc5e698ca4b2392d1200      1
     72 0x567e08ec909bc19553dfcf4efc7dfc77d93653ea      1
     73 0x57d6e0138f8e3943716994f4488e9caeef877f68      1
     74 0x58541d9bf7d750615421fc37d77721d8f9147771      1
     75 0x5a6da46839fe767c8a481a735af978baa5576944      1
     76 0x5ac4c2140ae997adb41f5999ca72f245271eb14e      1
     77 0x5d432907437922f8d07ce935595307b09985fd4b      1
     78 0x5e9539bb342d293856d10c2f10756a3f84f21aa8      1
     79 0x5f2175a85aa9e7300c83be7958bf395dc7ec7795      1
     80 0x5f80fa8a11fcab25bc4837f0009737d5e9ca9f3e      1
     81 0x5fd2c02689d138547b7b1b9e7d9a309d5a03edcd      1
     82 0x6079696f3d9fc53ba8350d8db44e048e6a3814b1      1
     83 0x62d36767473db61a39d7e5e71fa378cfa3317362      1
     84 0x631ec40dbae6ee11590549b5e270628840093a08      1
     85 0x633fb030794b8508cb27fb1ea6b45df95bddbbd1      1
     86 0x65cd64f936b25ba20b006bbd8c89ba420f3e6d3d      1
     87 0x67525bac2c2be47d9da94638e333c4315b293fbf      1
     88 0x69ec014c15baf1c96620b6ba02a391ababb9c96b      1
     89 0x6a0208dcbf1bb2cfb6b36ee0e7a5d53aa8826f91      1
     90 0x6a790cf85792db7ac24fdf45cc1783f76b4c8faa      1
     91 0x6ec79c81d230c29debed6f328cc27d108c96768b      1
     92 0x77e62fb482027eca49b160e6f7b699fd3621f68f      1
     93 0x780ba6c53069f5fed88a59ae3e7a78710e889594      1
     94 0x7ca6b4645b71f874a35bec18f0e997e51d8d815c      1
     95 0x7d87e23d7777d4829845f03f31721e10775799fd      1
     96 0x7ecf502926462c145609a8da5ef70c9db9abfabc      1
     97 0x7f26d2edbe7ed3bc6e86f6981ef35fb421376dbd      1
     98 0x800ea3663431c72ece331546ab522e9b8a2afbfb      1
     99 0x8061fea6516bc87f3219dcba464a1b50378ec3dc      1
    100 0x810657d67c68cf7299f7ecbc321b293ee064c5df      1
    101 0x8411a8ba27d7f582c2860758bf2f901b851c30d3      1
    102 0x878e906a8d749870fdd8fb76a08b0233444c3aae      1
    103 0x8874174a2366668d54fea6343f71709389563c8a      1
    104 0x8ba1b5bf769ce974d059187dccbf9db91e628bd0      1
    105 0x8bd831dfdaef68252d4a2c2ae1354db306e17051      1
    106 0x8d57e5917847b7bca5b1a525b3ca519afac46b6d      1
    107 0x8f899f39dc2799661aeece95293067b610c9de88      1
    108 0x929e85730bc34ec9d5569f105829df0b300a0059      1
    109 0x95465fde2e76da172ebe047167bca1569d47ba5b      1
    110 0x963b0a8101d151a92f49dd3cafcc5f43f33a5400      1
    111 0x9a1172976c47a86f0381134ff91d200111431f49      1
    112 0x9af496c94c91780405b29e4789651959d09448b3      1
    113 0x9b3580a5a1e53a03a65f323da26b15c75284f148      1
    114 0x9d1b972e7cee2317e24719de943b2da0b9435454      1
    115 0x9e2c90b5c4b8363505ac4c162fc7299952dadfb1      1
    116 0x9ea493786d8be968a9652976543999456d0bb5fc      1
    117 0x9f5cb6553cfc25de2bbb6d5ead0e92fc7dbe9825      1
    118 0x9fe35ff858feb8a1f906f3029b3bb1d60383ac42      1
    119 0xa1fd627d5ca88ce2c3fac250d86ee9af2795a131      1
    120 0xa2eae178f372220ff6c6d8cacd63efe4e4b44525      1
    121 0xa38b7a980df40439f1968abf67673fafdd2ac3b1      1
    122 0xa3a7032e9c986fddf4cf83a9b27829aafed487d5      1
    123 0xa4a5578d65371dd74c25a45ea6379c136917ea44      1
    124 0xa5008218a367a1afd99642ecca257e7848efb5a3      1
    125 0xa5bac94a97b9eb9efdd80904851c8c5d45418719      1
    126 0xa6857a067c9529de72d88b5933585f2b89404bc2      1
    127 0xa7b10e4b966a5885ee3408e7bc4da8b72bdee3ca      1
    128 0xa8676b7590bcb670c71ec8c4267f475352264d0a      1
    129 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    130 0xac32ea503c5e3e03d7e079d7fba99a168fd69b6b      1
    131 0xad5d0f44255d67ab9941b5380bcaa2ee54b42d02      1
    132 0xadbb802949cca822e558f0c8ea3b4dcee15fa2d0      1
    133 0xaf081fb7562a138afb14e5813e832a72ac2465e7      1
    134 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
    135 0xafcfa38af7aa41b4ee87d88715bdaf70d1381ce0      1
    136 0xb02f4682fe41cef73eb915cf1d5c75c5ffa81c1e      1
    137 0xb11efa4a9dcbdd5f6125494d90f9a8f998bd4a86      1
    138 0xb18c88aea2933bd7196112e41cc226228efdf856      1
    139 0xb3eb9bc116fcf22c3d6d5f920855d4bf34a9b0ba      1
    140 0xb3f3658bf332ba6c9c0cc5bc1201caba7ada819b      1
    141 0xbaa208f9acf53d3a1353420641ec31cf1550526c      1
    142 0xbaa738690c4e617f64c00fcbdb2ee69281d5da80      1
    143 0xbafa0ecd146c524bf4033b2ec04cb5774b08f75d      1
    144 0xbc3ced9089e13c29ed15e47ffe3e0caa477cb069      1
    145 0xbd426885d919696e08da9fb4772e477eaef04e7d      1
    146 0xbdc0e583175e2a8853f9282af3f2011e67e2809f      1
    147 0xbf40f7d4de3ddbe74fac58f14d5504e6c1e64343      1
    148 0xc0f4f2eb2e01ced954fdbebd1feff12e584fc26d      1
    149 0xc0f8c5747c27071f9aa675e660e0113c578e4bc1      1
    150 0xc142995f05436c91bba172b7d17522a301323236      1
    151 0xc17c673d9afba81606f6394808d75309abd40ef9      1
    152 0xc2079e5b077eae969dbeca3747bb855ae9df8f3f      1
    153 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
    154 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    155 0xc53a1682403bf4e00b8c6d8995953209f3c8fe6d      1
    156 0xc622b9e04f9ffed69d63aab4f2369afca6310ab7      1
    157 0xc87216e598c860e02dd8ce98e2d157085e0e9d4e      1
    158 0xc88df486141309ba42903e17279c0c8dba897190      1
    159 0xc8fe8f3a13339aa7e8e64d301627a5117c308d2e      1
    160 0xc952865f0d3f5b152c3bd7772b23d655e272641c      1
    161 0xc9a39e6321e1e63633008664caeee256e7e950c7      1
    162 0xcade1e68a994c5b1459ccd19150128ffef09ea3c      1
    163 0xcb34abe9530450f7606a3abf03564fd1179f3586      1
    164 0xcc0960243d099bcae96c0d1aeacdda01434d2ebc      1
    165 0xcd89d190353b1d0d4c103102c38b76f7696d511d      1
    166 0xcd981d4402a3d88fff43afdc5926514bdb5824bd      1
    167 0xceabd044b9a3d52eb9e1d65ed2a65b7a11a6eba4      1
    168 0xd1a2bffa66a23b657ccb4e35425d9db6f667b29f      1
    169 0xd1ca63e92fc083ab3016989fc7ea3e2fab928db9      1
    170 0xd2498b4dc8402789736f7c94caf969ea65badfa2      1
    171 0xd29d0bac7e2931a5b169ea5e6f3c38cf15443d6a      1
    172 0xd310abf0438170fc8cdc525e6b9cc0b498363624      1
    173 0xd435aa3217020db4b9fa3a6ddd7edaadee9cadca      1
    174 0xd511f7d8738e519820f4e4194bddb253c1ce517b      1
    175 0xd530282d853169a23822762af0dcab045d6295d3      1
    176 0xd5cf0b4851bf23ae9cf3d8725b72b7c1614af833      1
    177 0xd6ebe143acbae8f4e56636412d5a10ab312fcc2c      1
    178 0xda327740d72a7e92aaf9128b7ee8d1940d054e77      1
    179 0xdc3ed2238350aacb484aa6b0d17c4fc3353f0046      1
    180 0xdf75f242112432a60ed0995eb35e35d58080b4a7      1
    181 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    182 0xdfd92cbe0ebd35a3cf89494037c24c14978ad2d8      1
    183 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    184 0xe031112902b44c10c742d965d04f64720e7af983      1
    185 0xe58767c9130266a050ccc5b408e40113bef06a47      1
    186 0xe7043ccf95d3db094777c67ef225ce759b94550a      1
    187 0xebb1bf432de7629712a3706389ecfe110a3aa6f8      1
    188 0xecc424c1ffb22ba13d84669f15107f53809b0ee3      1
    189 0xecc953efbd82d7dea4aa0f7bc3329ea615e0cff2      1
    190 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    191 0xedcf5d6ddc5c8b64dd6927cab50a5b7fb3e50abd      1
    192 0xf1348e4feaf91568497152480abde7f3603109bf      1
    193 0xf53bcd08714938e53cc04b937434ba7063a3e839      1
    194 0xf85e56ff0c5c485aa5634f6cccd1dc3c031adb09      1
    195 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    196 0xf95d5d15ea2dbef4476bbf85663ecdd8b9b5a34c      1
    197 0xfa1b867a9a2ad93f186eef6e364dfa6548ca8f43      1
    198 0xfb3a33de99d27b944b10863233fd6e98eca97f20      1
    199 0xfe59f409d7a05f8e24aa90626186cc820c8e3005      1
    200 0xffe296bbbc86dfac056b42c6180d9f8b4cb412aa      1

## Allow Gradient Phase 2

``` r
c(allow_gradient_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_gradient_phase2.csv", col_names=FALSE) %>%
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
    26 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    27 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    28 0x59068075a799594db03c0255eed68e8e121155c8      1
    29 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    30 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    31 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    32 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    33 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    34 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    35 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    36 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    37 0x69e68074f1aada957edd39c5eae0069973343f30      1
    38 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    39 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    40 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    41 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    42 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    43 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    44 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    45 0x82139687faae8a29851902783e02e699de0e0846      1
    46 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    47 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    48 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    49 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    50 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    51 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    61 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    62 0xbf814810b44978de273191fd612aa47f7b69d564      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    66 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    67 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    68 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
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

## Allow Artist Phase 3

``` r
c(allow_editions_popwonder_phase3) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase3.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 660 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00653d03b147a663390dd5c5b1a55dc9cc0bd2fd      1
      2 0x00bf6f3d10e3625ff79db5671abb9e1e7fd84ea6      1
      3 0x013cf7f1eb254fa14f7ee524a3c514f21b2b3446      1
      4 0x0162f0b0deaf7147d5603d83c2c149737a6e80df      1
      5 0x01a45f7fbf7453092e49b6db137568f9cfab8ead      1
      6 0x01d71a1e369cd675a1215b9f1bd63b582f6f257a      1
      7 0x02252111113e291606466deca177b707e1f46363      1
      8 0x024878eaff0e05e559cc4fe553f64c18c0469182      1
      9 0x027cae2ed1a23350a751452e907b4120330f9762      1
     10 0x0328c8b74fc29943b1c6be6cba3f94119366e9b7      1
     11 0x03f0e71ac43276fcf0b327b1abe8cdf5974aecc1      1
     12 0x04173063c0c491470b735954790f94ed307aae9d      1
     13 0x04bfb0034f24e424489f566f32d1f57647469f9e      1
     14 0x05ce5d83371872ca8d6c4319170b452898fda3f4      1
     15 0x05fafb4c0bed99f43774a60860d0a45c6a8818f4      1
     16 0x0634bc77f6c56d899c80dff1a1f4fa813a194c5e      1
     17 0x0636770acd48071eb24e8592d796e3a03e88195e      1
     18 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     19 0x0728f8f9239757a9bd891408b4ffeca59293c263      1
     20 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     21 0x07ab86707f7ac092028a8a1b747f23a2adddeded      1
     22 0x083d1316a2a86f24831f702b13d950e75327619c      1
     23 0x08db288778050b1f29ca2181421bc89253e3fd28      1
     24 0x094823d9d58aab278569e6ffbf202443069205b3      1
     25 0x0a2b10149ab619dbef207be22e6d29c51400893f      1
     26 0x0a2de356a990f3efacb1582b323b8f5dc156d5fa      1
     27 0x0a46d80bc52f22b4a2c26dca307fd439a0c834d5      1
     28 0x0a4a4fd3f3e65f2f5030dba04fd2bcc055823384      1
     29 0x0a6d9de7753c807c26210907aa9fb6a5b671c453      1
     30 0x0ae154d26172444efdf82df4ecb7a114b984c8a0      1
     31 0x0b5f5c5ee41f49dfa674f501b2a45a3449458d51      1
     32 0x0c93929360ff8b46a46c2de1c8edea9541b78eb3      1
     33 0x0cbdf085991907c1183b0438e31b9c8e47c716cd      1
     34 0x0d266f9a1300eb05e8947c7a39f88c623118bc25      1
     35 0x0d76461aa04e1ca42730aac61df11e4a47164e48      1
     36 0x0e014bcb0cd02dedd51f32258255abeffc28d42d      1
     37 0x0e63d7e489363028e23a6da417d5767f9e399246      1
     38 0x0eefa9732dc7d2eb781dd7dd58041a24dfbf4019      1
     39 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     40 0x0f692f4a862679008bbf48cc0a7abdff12908f79      1
     41 0x101864c31fabacb4138400d121ac07253e482da8      1
     42 0x1064aa646a7aedbd40816fc0c35e044d0244a764      1
     43 0x1174309662f8c5f29f9e360ae79e071e4bad45cf      1
     44 0x120e4809a78cfc042bcb090b8daf6b40963626a4      1
     45 0x1243e7ad51eda47e580c20e751acaa0b8863c17c      1
     46 0x1262c73929d0d6759203fd3090f3a8a171ddf66a      1
     47 0x1268a3e20268e6c873558d323166b7633db26c03      1
     48 0x127fe84657032e777bcff49fa577f2fecd2bf09d      1
     49 0x12e140a67a661e2f73e699d5864ba5cb20db4bf3      1
     50 0x13670c98dda2ae5158b941fc6abbd24d1735f938      1
     51 0x13e032c402d982794439be4aac7741a573f6f0b1      1
     52 0x141f240d775f0ffb24a244f61dbed577a3e5b003      1
     53 0x1428af74177150299168ab67e3c773bfcbf5e757      1
     54 0x145cee509f657a8a42f28577b3cdb85b74fc7da7      1
     55 0x154f4a87f0fde6b52469af74e66f3c6a788916b2      1
     56 0x1643635228eda7c2c82f05b520fe755eabaed547      1
     57 0x16701552e6a60f58f54b5647be33da8250b86ba9      1
     58 0x16a7cf1b739fc45d7ceac90ad6a7582126db4b00      1
     59 0x173c5a802976be2470dc2f99f524cc1940d96e9b      1
     60 0x17410ac8e1e7c0296d3d1ee82c2bce9cf5250a5c      1
     61 0x17ed592f2cb40dc5f5a7d2948ba051f4c71aceee      1
     62 0x18a6393168738702810daf55ffd179f88e3d50a2      1
     63 0x18a6a993674262e166c475edfae0858f6d0456e6      1
     64 0x1926db00f4e096fd363a6504098d6633d483157d      1
     65 0x194774149cdb23c03ef126632c1ce330315bcdfb      1
     66 0x1993ae6e30f627c003e179138961088967693a5f      1
     67 0x19cd2a253b3e559b2b5ad57170b4d5b97b64bfb3      1
     68 0x19d4ccaedd75ef32ba76430af03e27cee010606a      1
     69 0x19e94b5c55c0e7bacc158de47dd1cf2b9a80577e      1
     70 0x1a11eb6be5dc646c96328e75fcd654cd9a530096      1
     71 0x1a92c3d98a234bcb7e8b704ed510978d8cd925b6      1
     72 0x1a9d4939a82f13fb1ebc483a0c6cbb229fdc96c6      1
     73 0x1b06e09ce479169ac4e1ee9fde71eb2542f51283      1
     74 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     75 0x1bf555e99b9056e75f5752ebf6593c4929bf5d50      1
     76 0x1c3a537d1f4e464696679c3d9fbc9becbbab5247      1
     77 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     78 0x1d28a703ab06bcef8666e4692fe63188b6fcbca2      1
     79 0x1d4752ec4aaaf60d0d6817fed1d2784fe2080218      1
     80 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     81 0x1e761acec1cd06f3f75dd58637bf788f159594d7      1
     82 0x1eacb095b6a8c2487aa212ecdd2b9a111dbb1a7f      1
     83 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     84 0x1f648b364a8c8cdc679d0c77e60fd263fd1d9da8      1
     85 0x1f7b1afeb239552dc1b0dd13a9ebc8d9ec6e079e      1
     86 0x1fad9f65b39d235469bcb59bc664872b93eecac5      1
     87 0x1ff1b9051dc8ff22ea61d8b2d3cb14e8fefbdb70      1
     88 0x1ff84234f7802c3f02ff9d8836e2d570866c0396      1
     89 0x201a925f7fb2a67150956d9c7c13c61977c19c53      1
     90 0x20ec02894d748c59c01b6bf08fe283d7bb75a5d2      1
     91 0x2115e23a90bbc25c2500f4b158f213356ad292e0      1
     92 0x22b0cee7dda7ebe3378c280d3e79bf6092937079      1
     93 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     94 0x22e3ecc80c5d3678e41ac1593d1610b07953e42f      1
     95 0x231fdcac73f9fed1ac2d12af5f7cea39456019d6      1
     96 0x232e02988970e8ab920c83964cc7922d9c282dca      1
     97 0x233bba656a23bd4b782d4ee7caf679a7034f0313      1
     98 0x246e210187433dd7fade45492b12b08500f458b7      1
     99 0x24bae85e45756b1a7f4e0e171f7812a6864c5696      1
    100 0x24ea141a8b400c5f7e8fa11c6e031464bd637233      1
    101 0x258bcab7abc959d033e588828916ac45c0adda86      1
    102 0x25b45ee1f5efb473c6b83d37f3c17ad439b95352      1
    103 0x26153ca3a54b0c54aeeeff9f2de50262af620118      1
    104 0x274a434f95cfe49702df41022f2fdb629267f3c8      1
    105 0x277884f6f7c8a96aa25cd5d4ff0a6354c57017a4      1
    106 0x28395ad01ca521ec9f51b3a493b94e5104a276ea      1
    107 0x2840838c93d7fcbbb444219258982cd52b8b47aa      1
    108 0x298e30553c179969c8f9ea80e5918e82ef8d1a38      1
    109 0x2a9f8609e0e8bbb17128e01d83ed9f96e0fe69de      1
    110 0x2ae664f70540468c55e0e7a157944beda89d0a98      1
    111 0x2c07681e81b5f98c615cd2dd807a1b141982914d      1
    112 0x2c63f7b3735b58d71a9c47c80f21428e46ca13fb      1
    113 0x2d093e44a98f218f8dd3bfd59b80b461f1e36d80      1
    114 0x2d4a7c6be7eb43196ffc703132536981c5fd0d37      1
    115 0x2dcc5c7a9a212baa30f900b412ddfa99ec6777a4      1
    116 0x2dcd42bdb418edb7e651f486deda821c625aaae4      1
    117 0x2e7173503081160aba4d68204e9cee72e7a9531e      1
    118 0x2f0c0743d0604c2bee80a575d3313dd01a105134      1
    119 0x2f34dfb91116c5f56aeb444fd18e7ef0d8158f7b      1
    120 0x2fa000c0e99e3b6454bed942d8eccd26cff6d5cc      1
    121 0x30484de5c7954252581c69718bd08717c229b070      1
    122 0x3055d970653a2d1785a989d2699ea50c0c96f159      1
    123 0x31c29f8f1486abbec72169501697e1b657eb0f32      1
    124 0x31ea0d4e4492dc6b59aac609214e8fce6fd08cbd      1
    125 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
    126 0x32f8ad16fa8e1ba6c0fc482fc6de66b50a89ae5f      1
    127 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
    128 0x34f49d19462bca4169d66921312e2562f1502cef      1
    129 0x35849f8eb66900488fb85e2e4c7eade1c5a588f1      1
    130 0x35b7dfe73e81e3815212f3da0bd6508b473f9733      1
    131 0x35ffc49e7fefc188dff81db3c717db098294bc23      1
    132 0x369615bc57975b06c418f42d3fda3754a385b97b      1
    133 0x37032e904c8725e93faf4305c9e07ad881f1a238      1
    134 0x38039da6bc26c101316af426cbf7f43834957c46      1
    135 0x38ab17c5b796f15305ec81ff7ef2a0b00d796d1d      1
    136 0x390b89aca6e64f9b1844f61aacdb5883bf456f10      1
    137 0x39a167e13c3fa53025069ef7e62c7cc8e8b46fe5      1
    138 0x39c3c6d6b0dfe0081ef26c7b132e2b508d3644b9      1
    139 0x3bc3a258fc6dd9f6d3021a47b44706c656faa6da      1
    140 0x3c6deaf5c95717ed5442c9836dfc62c1e5e83164      1
    141 0x3c8b433f9bbb57954895cb391abdc5f316969013      1
    142 0x3d35d9013727b8d611dd1cfd8976e4b9b1904551      1
    143 0x3d41fcabfe9541df6f33da58fd890f5f0674b5a5      1
    144 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
    145 0x3dee682ad8fc1bfc3657c59e841d135d51f9662a      1
    146 0x3e3588fc0dcee9cf9d2241d6353db5ccfd3a8e19      1
    147 0x3e415cbd89d9c5f0e7476e0f3e7dfe984d0f9fef      1
    148 0x3e6ca480f9f474c5f495baf8263d5ff284d3bbc1      1
    149 0x3edb3cc6af46e571b0048b9f80eb28880046db59      1
    150 0x3edca1c467a44d10f6e63748c52552a04dcedc67      1
    151 0x3fa5b646b19271033f059ec83de38738f3e3163d      1
    152 0x3fb38cee8d0ba7dcf59403a8c397626dc9c7a13b      1
    153 0x400abc3dae98ec5aeef2681b40d4ced0a5aff934      1
    154 0x401aa8968004aeab2274b0ab1a1ea7bbee1aa638      1
    155 0x40ae064d920dec0f1ee1cfa2fffe65ea10aaa5f7      1
    156 0x41548b16b60ed834226473d67ced48fff5e2059d      1
    157 0x4187134b79b007cff97e24fcb04c47115e7b63f1      1
    158 0x41f5b3d6d67d66deba1993bbd13d12484676dbc0      1
    159 0x41fec248777efa4bdba27108cef74da24b74a4bb      1
    160 0x423af79ca18d95009bee04fe80ff19be10fa2ad6      1
    161 0x42757298759773e1af7199b18f6ad2307dfdcd88      1
    162 0x42a63047a1cd8449e3e70adeb7363c4ca01db528      1
    163 0x42c3714e42115ef94fd1e9ca26e66f8b0df6a91f      1
    164 0x431b251725cbfcc7453a403eb824cbdf11aeeb09      1
    165 0x43b3a12cdc49003c9537d0ab92800a97c0a8959e      1
    166 0x43e8cc474a61fda748bfde05edfb75900a70f27a      1
    167 0x442a653a06482079199a106079240ee54ceaf890      1
    168 0x45399f730fd3e1c3379acce2cf9b0c60abb53c9f      1
    169 0x45c4a18130e88d8593e0c7877b24aa4fb25b2051      1
    170 0x45e023881182cc2937374f61bc601846be4ed6fd      1
    171 0x461c65bc41c8c822177900dfd8217ecebc3a479e      1
    172 0x4799b18582cbb51cc207ed9716034da309edf47e      1
    173 0x483ab488a7875b9834f09ec4e21acf5da215db99      1
    174 0x483c069180324a165ee53c6fe327125d559e9478      1
    175 0x48ddb5e74817105a6911cd9a747fe53411e8d601      1
    176 0x4969cf3d82afaebb1604da6ad393c51bb797ee2c      1
    177 0x4a980243b57dd0652c74fe38839c0fa09458367b      1
    178 0x4ae48b1b34fb85510c3eee6689c7c6569d6c358c      1
    179 0x4bdc0df1875fff7c18fe4cc7c3b228e443fd3295      1
    180 0x4d0cc9347f41ac1e4018534a2bf0e47659188e8e      1
    181 0x4d116a07a5d3011eb75ee9b7605d27c3f9218742      1
    182 0x4d994ea6a89ac37e43e9ecab11ec5acfc1632018      1
    183 0x4e2e67836d10b02b1dce78591af6dfd1e2d7bcba      1
    184 0x4e626e64b44a9f299da9cb006320297f870a8397      1
    185 0x4ec741b83ec1f0b491152904b1b8383c2975031a      1
    186 0x4eed9308c3a10da02f92e516e5ba969f75367ba4      1
    187 0x4f234ae48179a51e02b0566e885fcc8a1487db02      1
    188 0x4ff05543d3a53ce52da701fba2af11255f0a066c      1
    189 0x500f2e800003f7f33e1edc76e3ae89b591f432a6      1
    190 0x50a9f0f42ecde1df805b1ba6e885881accef049e      1
    191 0x50af68c8fe5c0c5a9cb11bc0edad3cd678b71293      1
    192 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    193 0x510d0115b26b20d45c4a6bea3ebcf25db625888e      1
    194 0x5140285b5a3504f90ad86d9fbe0d96a095baf2ca      1
    195 0x5289156f8b0876e7576fe960e39a3e84c90d821f      1
    196 0x52981e081ea651a41e08a44f90c7895a70c3e198      1
    197 0x530cf036ed4fa58f7301a9c788c9806624cefd19      1
    198 0x53f778c4c00cc5b1776ad20ffaad1bcf2146698c      1
    199 0x53fadc97caf5e09085047d10fab476025915fa27      1
    200 0x5408ad45fd5db107766ff88f5faa4b8d1aab6121      1
    201 0x54cb5138bfe9e89445ace9b6cdff739f9e3dc2d0      1
    202 0x550e970e31a45b06df01a00b1c89a478d4d5e00a      1
    203 0x55b8a8f7b1bf4edbcff9e2dac766fe5d50940977      1
    204 0x55da273b463a3179bbf5f22872b7f33785631691      1
    205 0x55e54e3af4e3df2352d8005a7f883e2c125ca0b3      1
    206 0x562257821731283b842cf689df9941ddbec3a2a5      1
    207 0x5670c685e1dde13e6a9f7293ea6db0ac9090a7b4      1
    208 0x56e507da02d59190a59824308d1f63d40e0278af      1
    209 0x576ecb41cb752ebdd0969137c36ffd2eb0b14aa7      1
    210 0x57c88c333344c2632823b727b90838a950f2348d      1
    211 0x5808717db53c6bc96a7042762889946bab59b84b      1
    212 0x5858eafe41ca64eeaf404c4cf511c24526bea1a2      1
    213 0x58e0dfc4848dcea5cf67100b2d616604a9f88681      1
    214 0x58ff5ce924518816871d1334dd2f38f021940b3e      1
    215 0x591f8a2decc1c86cce0c7bea22fa921c2c72fb95      1
    216 0x5959002cb524181d5526714c2804c3775212d823      1
    217 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
    218 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
    219 0x5af5d005f956e448dab7f00430e6f6d6ea45209c      1
    220 0x5b58c9853a2b471992423577cd4993cb8e462b7b      1
    221 0x5b64053c3f9bfda9effbc3c6f57dc427fe674bf2      1
    222 0x5b8149f0c8c3942724f4e404390c14d7277b5214      1
    223 0x5bafd5fae38826a875711c84adf7b5cccb73bad8      1
    224 0x5bd906bc2c38ddf3abd65b6e81e5f2d2a4ca98ae      1
    225 0x5c736d849527bb15fbc0111f4ca13b4cfc1a5c8d      1
    226 0x5dca7bad26550b04a2d6911ba64bb7e7bdd67787      1
    227 0x5dfb8e65cf9ce76fb96ca2a840860680d379d735      1
    228 0x5f5d7022d09207680d63ec679ebf370cb940e98a      1
    229 0x5faa31ea9a79a8e4fa391de7cae5adc9b4b02833      1
    230 0x5fe785b2f589c79c89dbbafa217bd7dedd8c918b      1
    231 0x60282772ab3e7f9e71b6eabd698ce4de82a18776      1
    232 0x6191c173e855202083455859e670ac16a669788d      1
    233 0x619f45abcbb4b2ce12be75704351038b49860097      1
    234 0x61b04d2b5cb1f1f8505e7b4e1cb3d9e8a0b15bae      1
    235 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    236 0x61e0841d103d77325e7743d1ff7117efe7c2c9f6      1
    237 0x61e29379b06491ea5a0be90fae954c56c91fd9c4      1
    238 0x6269a98ae8cd1099eaab82ceaace4d84f8116347      1
    239 0x627f489e01fb281263f84e2226b98dba082155ae      1
    240 0x632584b80889833be60205c932289632d79e262c      1
    241 0x6359a11dd611a76fc0fa19722a6d4b458ce8d505      1
    242 0x638b2619bfabf3218bd2fb9bd909f03eb35c0bc6      1
    243 0x63aefba4e4241669a206cae0fca726434900a50c      1
    244 0x63c89c7212df4e3ffc8c6d9ed8014dd3a2d9fa5c      1
    245 0x645eb399f09a347f040ab48017512556ee13f9b4      1
    246 0x64aa471f59653b13f08559ff7ce39453bcd415dd      1
    247 0x6647b8cc5a0749adc92b8956d34c03a3e3343360      1
    248 0x66af9e0005166fd3631a5beabb6c4eddf3a65312      1
    249 0x66d5527411d821cb513f2d214bbbc0ad73586b35      1
    250 0x66f18019715410c94d8882cb57ed0f2ba432fb39      1
    251 0x677420671845f3bb7a2f59a0cc530198e1f596e9      1
    252 0x67904cd46a648219f860ae1ae9c8e20e60105300      1
    253 0x67a3a43a55bd9cbfa5d696a7ef2d57e9e0869fdd      1
    254 0x681a64617e1083c36cbebbd8ac9e64938d3c2591      1
    255 0x68858e8270ab4858eadbe94d0adf609693c59c82      1
    256 0x68e7589134a235c00c11bd3a49dfa5ee9625b87d      1
    257 0x68f516cd11a1f8e49ec826fd5ab9d64114d2d31e      1
    258 0x69592d3be7f6545fb3a6eab2392643b89795f106      1
    259 0x6a08986aad2967bd659457472fefd9bcbd417316      1
    260 0x6a33051a4c514dddcd8ea78242192199ca30c00b      1
    261 0x6a91b82a9abd58963b164d01cbf793cf60c089fd      1
    262 0x6abdd01d5bef7a6ae0e20b872b7a004e8eb0d2f3      1
    263 0x6b02bb2d29a8d60a0dfda3500ed6d43b485d4f24      1
    264 0x6bda032b6f8b00c940df7e406411f45459fde0da      1
    265 0x6c0c160412f91a9adff0951b23186c87114f2aab      1
    266 0x6cb5c0d93178514d4d943afe1e5a13ee27845b23      1
    267 0x6d075df8e96c8b9aa84a69ca1a16249b7c20459f      1
    268 0x6da3d343e73480e51434372a8550f3de8a8f9d61      1
    269 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    270 0x6e25b73303586bd563f088ec17cf113783cf842a      1
    271 0x704c978d50590b21e4c2f43eb4da24fe61fcd707      1
    272 0x70cc40ff234f0bb22a1497e6e6d812a91c26d2a3      1
    273 0x70d3d73eb2de714ec307889f277728ddde1798c0      1
    274 0x721cf6cee5e0b6a39619e26040fde151a75c51c9      1
    275 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
    276 0x727ddd50497df9e84a956918fd636df802ea0eb6      1
    277 0x72d5704ba3850e131b9047d5f26876929fd2a2b4      1
    278 0x730c89e9305762bf43036ee1d0bb8771c1fbcc17      1
    279 0x736b195c02551de080e457f0ddf1fdff4b9c6847      1
    280 0x73ac93a7950af4e92be6ed06f1c9f0fc8d166838      1
    281 0x73b12915ad8141b35233ff6787a99a75328fcdf0      1
    282 0x73c20e18134b093df54418b5b63afac19ae21c84      1
    283 0x73eebbaa32164474d35ec65183cf7cb2ad7fb285      1
    284 0x741a97426ef683ff35c6dfd7f1fe1c20303a4c39      1
    285 0x741e65cee534965addc3b765c6ed8d5683fd81ec      1
    286 0x74daf8b664a5769c6bb490965e97b22b5b216fd3      1
    287 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    288 0x76807f3d69b1d38f9d597e902f18dab0af926b8b      1
    289 0x76a2473e0dd655b7100e8c639af52a6f5baf0a03      1
    290 0x76de145e697c1ec1e76c5db285a6b54ae6292a77      1
    291 0x76f510d7f95dc546fea0851b96a5e4db3511f532      1
    292 0x7841c102d8d13c1f78c0d942e73117cdc6119697      1
    293 0x787349b0c7d3bdf2583f2ba586b4990074593be1      1
    294 0x7877c464f6601486053d3046d85ec20167363820      1
    295 0x79ded079b6f289dc888b09b0fe5d35fcfe764c4b      1
    296 0x7a18bdf86df7e869d741301df06099a6306772e2      1
    297 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
    298 0x7a827fa01ad4985ca7dff695131b28a1600b9130      1
    299 0x7a8cfa3713b3934aa9541d30b71df4bf88637215      1
    300 0x7ae352b0c23c63a288b8f40f249b5f72d15b8f8a      1
    301 0x7b206794f3f14d42b7311baa727f9fc20c31201b      1
    302 0x7bf4209cf7c38bad15c3531a9291a6345f1a7b3b      1
    303 0x7c05c434ac7cf219aed5764191b5ab43c0f048ce      1
    304 0x7c306565d407c7a0f57f29379de61aeb2cee06e1      1
    305 0x7ce5de4c75866b9d23d893808890755603762854      1
    306 0x7da10c719381f5eb07e2a1f875c3633f0e7c8c78      1
    307 0x7dccc79361b0227a85c2ff788ca240d31f2a78d0      1
    308 0x7e22dd944050eb6988e1437cafd06c6a2e549d30      1
    309 0x7e2a67ecfce3ac2f55ebdd181b937350c9e1c78c      1
    310 0x7e3b9e461d1ccd4d0ec4cf09fc1de9d6d4e335ac      1
    311 0x7e54fa1f85eddad0622841b0eaadeea2f19d3240      1
    312 0x7e6d4810ea233d7588e3675d704571e29c4bcbba      1
    313 0x7ef4eabd29436688dc4a58419a0575a100c1c134      1
    314 0x7efc61570e1c1091f253480356b94218a3cad19a      1
    315 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    316 0x801216a17cc09d1699f7e956e27a77ab783d01e3      1
    317 0x8025bb40e101aa2cab46fbf14b3346310422bc24      1
    318 0x81341f8a07f61f8b6e5c4a56ca3e5789774f26bc      1
    319 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    320 0x816fc2addfa4f24b30b7b8668639ab7a64f67a7d      1
    321 0x81e388d5139f109e859f38230101e4f8b036d8e8      1
    322 0x82e8e9e21275861fb0733d654dba0f1680da99ec      1
    323 0x82edd2493531293922ea3c3e1d8e1f1ec25d2340      1
    324 0x82f0ea949a795f17b7a3b52b0ab20516dff194d4      1
    325 0x83719d20bc1dfa830510120d9e09dd62f3ff4db5      1
    326 0x83c7e9fd51b680832e3d2624c4a79ccbbaa352ce      1
    327 0x842858c0093866abd09a363150fb540d97e78223      1
    328 0x8441725c5b9860e230cb353df6cdef198ca37af8      1
    329 0x8469a0c1872355ef6e418c04189d6d1da012a0df      1
    330 0x8469dbdcad91274baf81094d429b424aab1c7ddf      1
    331 0x84e2aeabb4f48551930b79371ebcd13f55a3ea88      1
    332 0x855ce7c2a55bc1c0e206d992bc68f90892994387      1
    333 0x85a30ede5e46029457e20d55427ee8673fad7762      1
    334 0x85bb680ebaef5524acb04ac8d60154f5a2c26121      1
    335 0x8684e8c7d4ae0256372e5fd727ca2bd3f4ac95b2      1
    336 0x869d4ce910b206b3e0a31662dd65ce07cf3548a7      1
    337 0x8756d1a0b5699d3ee8197c59f7b969a0bb188ef3      1
    338 0x87bb1f1723ac6c6b341a5df432f5a852d49ad12d      1
    339 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    340 0x87ea2366282169dbb24ab26dc787f1f53f993913      1
    341 0x8825a06cedb0f81d374c65742f90034cab7997d7      1
    342 0x884081746855260512c880809a63339fc1e7d237      1
    343 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
    344 0x886478d3cf9581b624cb35b5446693fc8a58b787      1
    345 0x8888888888e9997e64793849389a8faf5e8e547c      1
    346 0x88889778402dcf6cf363bb9ac47f7a2d8e77a2af      1
    347 0x88a7560dcefeb94f0e2e2b23e271b5161d133de7      1
    348 0x88b07f430375d6784e5cf062b596043f76b38d7a      1
    349 0x88baff51e5125659a7a694603ef237d2974b72d5      1
    350 0x88be96dc047cde01286e991f7333a2e9a4865856      1
    351 0x88da36152d4691e8d4efc998ddb7b22fe1b16898      1
    352 0x88ed47db4c7c45fd257f1dc6338bd835b1befe4f      1
    353 0x894ccde01b887c7dba5224fd447daaf1d9826bec      1
    354 0x895d023d6b9815886658a65dc87dc437ec138d42      1
    355 0x89f42ccb1b90103a4b886cc8ee01979492808079      1
    356 0x8af2bd409ea0290029c14aac5d1be77d5c9a114f      1
    357 0x8af3e5d0b93332a972f02c50767befa71408ce86      1
    358 0x8b3477ca7445ec524dd534fa4a596c993e93675a      1
    359 0x8b611a5198f49c985823be094726da9d39387d80      1
    360 0x8b6316060f0d2ff3d38bb237ebd068cc7d3cb47e      1
    361 0x8b7ce94dd00483317c705ccb27d814cd1c06b002      1
    362 0x8b8d9c0963a78b7f43a20ed37c266bd1ffefdf4f      1
    363 0x8baf972eef3effd5959b9d8e27e2289dcabde3c3      1
    364 0x8c0d32e856e30b9fd0d11a60e6a691c1d94517d5      1
    365 0x8c6af0a77b37de0b56fe211156823f896330f789      1
    366 0x8d3b3827f3d942cec668e5c3e95bf86aa8d5235e      1
    367 0x8d87b6150253045bf48fabc081ed25446dc89d9c      1
    368 0x8d99e8c0c14f12babafb833982282f29ddbf5ac0      1
    369 0x8e101059bd832496fc443d47ca2b6d0767b288df      1
    370 0x8e60653778dfd76af0f89efc39f793c6f8d69669      1
    371 0x8ed4d3c67ee1a73f5df8ca3e10698f1cd4c5b14c      1
    372 0x8efd9addd8de6a4e64664d1893dec51f8c3339e9      1
    373 0x8f18d6a49bb392a84a4a4c03b69d29179e333946      1
    374 0x8f96ee66348f79290c5e9f59046f6a2105aa88bb      1
    375 0x8fb5f5526abb70cc3aa622f4ab76c049412f9a5e      1
    376 0x8fc024fbe13a66a0aebe8afcfec1f635c286fef0      1
    377 0x8fd55da05907806ef94ec41c2aa8558b79897f15      1
    378 0x908db31ce01dc42c8b712f9156e969bc65023119      1
    379 0x90c0e8c6e4791b9e4f99272fd70016a682056847      1
    380 0x90d9c3bf402b3ed87169623dfd91e77d23c3b224      1
    381 0x91b6f978ddfea98ec9fef489e68b7f92b141aef3      1
    382 0x91c264a926387114b1a1d60c5457b54c38052ffb      1
    383 0x928ee99d063ba6839c30e663822c8cf6c6dcda02      1
    384 0x94de27f524228b71fe58348b6c0895bd411d71e5      1
    385 0x95eb24f50de40058b460a6ba195428f729abca32      1
    386 0x96288323c79e990e87e99cbe0bdbb55f565dc44d      1
    387 0x96ceed8be02c2f346c8fef7d830a24b5ee2a435a      1
    388 0x96f8d8f51bf82f48fb9ff0685847873f35646b8f      1
    389 0x972a474bb3747da867ca9b7233d7201e2d7413ca      1
    390 0x972d521729c0d07b360ae470f03878fe405f0650      1
    391 0x973231cf471f768f0939042e0591da1b15f4799f      1
    392 0x976f0f0cbb3fb38ee247463f5e574b9b1a353f8a      1
    393 0x97825f9bc2019271e7a8294071df87c764e367cc      1
    394 0x9830d8becc05b21830ade6893059c97a38f2dbb4      1
    395 0x9857a977d23447ad63c6b37c85f561dfde5c3d98      1
    396 0x997cec591fcf61e9d52070a6bf8567fd718f13ec      1
    397 0x9a568bfeb8cb19e4bafcb57ee69498d57d9591ca      1
    398 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    399 0x9ace182c0d1f30e6a8b90650659de6f29df6af23      1
    400 0x9b059f4d2199e73aa86f8938167a1a6067474cf4      1
    401 0x9bc0fd28965072c62ef8828314eaddda4865b71f      1
    402 0x9beb634e24472f6b1f202cbdcd2ebdc2372ad813      1
    403 0x9c2355b55f2c6a4da56731aad78bb4b5a69e271a      1
    404 0x9e063b533d031eef54a0de7babe37a086bc964a3      1
    405 0x9e1e3857fb2484379858b9daf230379015a7a100      1
    406 0x9e4c4c818aaf38a6a8e099cb26093aba6c521f76      1
    407 0x9e7f11f34632bf6362fbcbe11cb638b00b68e755      1
    408 0x9ee19c090db326185822ca8bd51f3d37aa627826      1
    409 0x9f5a95e48d010beed3b578dc378b27d8a49fe491      1
    410 0x9ff17099662514cfdfd932c9abd10b4995cb2f42      1
    411 0xa00c78fc1edb8f9c91637722041dba67af32bb22      1
    412 0xa0ab186f4815f1fd190ae7a9f0702d5b6ec15966      1
    413 0xa159a996ed582d183d98fed1989b2f0a95a0cf63      1
    414 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
    415 0xa1e8c429c49b3c628fcef76b8d59495cf2c8bbe6      1
    416 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    417 0xa3a4548b39da96eb065ff91811ca30da40431c0d      1
    418 0xa3d4e52b7ddd59373e046343c1ac6ea23490f28f      1
    419 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    420 0xa499ec786d69c0eb4c67767a0c590420faec7f98      1
    421 0xa671d8c5a0c0b423186a380b08e9507a5cf4f1d7      1
    422 0xa67d16546534746f39d310cf23ff4f0c00f58dc6      1
    423 0xa6bc074b5b61cd92700465e9c8706cfe8a4f47bf      1
    424 0xa6c12d417553f4c9a12c6d4376bc2b56e43eb2dc      1
    425 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    426 0xa76573a000289f6341c48cf37ae1389c3a88e7b3      1
    427 0xa7ebf19d015a225a4bd2660fb9eafb8c8bf10059      1
    428 0xa7f3f205fb211fbd8c94273d0b6cbff725fe6807      1
    429 0xa80a3fc9f979eb3efd1a3ba7d630074b57359a1f      1
    430 0xa941be630a39802f90c77b8d5f26add5a034e8ab      1
    431 0xa975ebaf140d206080dee102552dbf88d057af18      1
    432 0xa98b45a88ecd3fa138be3f55cca63690ec2fa7ef      1
    433 0xaa00e53f96f2e7938e5d9b0e8411d4776dbc2fb3      1
    434 0xaa584127b91100dde6b52228c28848a7b1d059c9      1
    435 0xaa6fd0c3dd82e34b1a0ce7a526235336442fd678      1
    436 0xab0d9d9c57d42a79333e475925def2bb35ba6925      1
    437 0xab6ddfff861c7eba5ac9166dd31566975e8e3104      1
    438 0xab766e079444e4de0f11b44e63bcec95eabef97d      1
    439 0xabd5c00dfa3ee341f2f7a94a5bfd3f1a16efafb2      1
    440 0xac06d32f100d50ca67f290307ec590443d639c8e      1
    441 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    442 0xacb818130ac9cd4484848848a0ca3e7dda4bb265      1
    443 0xace1c6f4dab142925a3d628c0fa5440c4dedd815      1
    444 0xaf263ec04290b2bfbc75748fc22c465fa8df76c3      1
    445 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    446 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    447 0xb055af702180e0ad4ac8eb2250bf6736224cc2e7      1
    448 0xb06000e99c4db5ed010513b984aa901a49a061d0      1
    449 0xb1b96ebbf9f74998f96acf42b403a420ab9917c3      1
    450 0xb204cfe5e3114f8054989e98b07f59a84eb9224d      1
    451 0xb24c430a177c268a8ade3d0c6ac15854f7461c2e      1
    452 0xb2a18003067dd5f96ce5a51f276c01873c136e5c      1
    453 0xb2ae757de10e4f96d811af809619d94c64a5573c      1
    454 0xb2f13661eb385286ab330ba0e7661a1636793186      1
    455 0xb35e71b6a5aebb07dfc3aa705968411ebdbc003f      1
    456 0xb5fc8914a5d453b340cb16c9dda4f8b0c1f32fa0      1
    457 0xb697b9455eeee9574ec5c5350cb10c8a5b752a39      1
    458 0xb6b7b48166647fe132d34253978d5f5cd499c905      1
    459 0xb6e5c9fad3bca83c80ca3325f7b01dd5ecae9629      1
    460 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    461 0xb74adeda4b37fc8d9548ba89f87ef0078c1c324b      1
    462 0xb776fd4b00939037d27a0ce20ffcb2813c38fd1d      1
    463 0xb806e21fbbb67a06462a013294c2b502559eb92d      1
    464 0xb817ad503569e1961f35bd751720e7fb189fbac2      1
    465 0xb8b05181ce694ba588af51bf7e8eeb82e52b03a8      1
    466 0xb90ec4b2da6ffce7d7a758bf2121f6a67d526357      1
    467 0xb9c71dffd0315a61b5df3ef77f2caa5149fba4e1      1
    468 0xba07d7ee1932821f3ed557b98230fd6ac9f2a106      1
    469 0xba5b22de516ff79da6528f8afe3593183f301866      1
    470 0xba890ce12f72cc4b8e03823fab611b3163855cfb      1
    471 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    472 0xbb12fcf546eb68336476566ee891996c1c1e86d5      1
    473 0xbb1af94f2849216ee56657b15dec32be43a7d607      1
    474 0xbb39b074b16c761bcd6fd3cf41eb59ccc14133df      1
    475 0xbbb6ce562b8ff6a9da51813094dc81578c266f41      1
    476 0xbc7b2461bfaa2fb47bd8f632d0c797c3bfd93b93      1
    477 0xbca2cf5e09748e38abd5091559ac95968d26b949      1
    478 0xbcc94f4c922736925e0e9c15391657888e85f435      1
    479 0xbcfb8c2dc58b0c12c894a7b70b03b58d020e2c31      1
    480 0xbd1b38d479591f5304389cadc7f9304043c31f36      1
    481 0xbdd8c23c8e8a39e2cad6159c3a9483f4c106c616      1
    482 0xbe6a5992d259990ddf43dbd681568db292ba47d7      1
    483 0xbf5b2ad0c6da52c670fb084612c2a7116bdf780a      1
    484 0xbf9f3a128566567bb7f84ce449d9dd913319897b      1
    485 0xc06dbddf5b059d4de36ed72fde4bc3793c63ce6e      1
    486 0xc128d62a47ea49d0f7edabd7ef0629bf5478b6a4      1
    487 0xc14098618d75ad042cdb1d89d16d951d09a3043e      1
    488 0xc1c0e9750fab87ac871ea40d005063f3750fe143      1
    489 0xc24f574d6853f6f6a31c19d468a8c1b3f31c0e54      1
    490 0xc2d742fdbb31442b843a098dcec3c20054c28986      1
    491 0xc30481d4824bd9ce97255901e43f78be9e234b6f      1
    492 0xc31da5c844177b716ecc3f78fb8b08098c222452      1
    493 0xc322e8ec33e9b0a34c7cd185c616087d9842ad50      1
    494 0xc3d63127a75161a4a26725c23dfea7a474ac0da2      1
    495 0xc3fed190bf671ad5d92cd94932b1a2e17d047f2f      1
    496 0xc4a2721e9ea9cff032464bca27f035eb4ad7f17c      1
    497 0xc4ff57adf4c6d26882bdd44c45908084eb66933c      1
    498 0xc5b46513ea71294ba740e115f88b697c26d25c8b      1
    499 0xc5d9096fe92b846269e842f3f894615c3b7f0d5d      1
    500 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    501 0xc659284cd530f1df076b38a469c4207d731a2710      1
    502 0xc704389a3f999f325d568df1ab676d4b750f7c37      1
    503 0xc7e108db4bebe412e35a7bbc9dd8bac2ba1a27ea      1
    504 0xc81d405b17e3787f67bcf9fae18fd5920c1f2ac0      1
    505 0xc857d06f76651ca5125e067bec173416a3c79f81      1
    506 0xc87e9dff30a7472329237c4ad222b35b529f0fb7      1
    507 0xc8d65470bf182100998036d221dfe484f5277087      1
    508 0xc8dea65f89af64daf9092f7040827f4b534df0ef      1
    509 0xc8f42217b6b1768012b4431f922be0759fed7397      1
    510 0xc92931bdadbffa8d5a319c299c619d74f4a631e7      1
    511 0xc948454951290bbb83e6d56e6fee5e9653750e7f      1
    512 0xc986005978d0188c5b726d96f293568405ee6613      1
    513 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    514 0xcb69c5838afd73317abe43ea23574ddf7a6e51b7      1
    515 0xcba3dc4b6d3cc2905a38fad9e766eb6c1932329e      1
    516 0xcbe8340bbe38de99cc86508d75ca611eff7a3426      1
    517 0xcbee390e62853b80aaae7181b205f6cf368cff7d      1
    518 0xcc0bcb3ea8c819d3ce70dd92a4a1ef6d7b295512      1
    519 0xcc44329a34109d973326edae0f04cf0f71f406d4      1
    520 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    521 0xcd4046ae93690aee64025543ebd31121d9c53501      1
    522 0xcd6034eb0f8f5c8b0ea954ea64f5b1f10f4d9bde      1
    523 0xcda897a9cb33fd692fea211f27786bc598cc2ec4      1
    524 0xceb72dd636147544f72784fe8c9f469c606a213f      1
    525 0xcec1930935e6148c6d74a9a8d18e99c216a75b9a      1
    526 0xcf3661d294d40cbcbab7819bdd6e95ea98e0f2eb      1
    527 0xcfc65ebdcd6b90b9036913971f963e8d9d83f37b      1
    528 0xcffe08bdf20918007f8ab268c32f8756494fc8d8      1
    529 0xd0bd3712e4c7b57213d34126a22a42da4ef73b17      1
    530 0xd1344833f3ccb6359583657be3d8959a18ab83b2      1
    531 0xd19cac0a3d46d82f740d15e1ec46f45641960d41      1
    532 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    533 0xd1ba123d1fce1b43669305d95310bdb5a9053d2a      1
    534 0xd23e85525fd675f1bf41856f07b5aa0accdeb07f      1
    535 0xd24a2f1feae9d9af52fd7e0f85f20190e85a1fc1      1
    536 0xd36954df517cfd9d533d4494b0e62b61c02fc29a      1
    537 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    538 0xd41a4df6d9973ce29791571d767defb16a40fa76      1
    539 0xd430fc849c60107201ea4f569108716a90ab65e2      1
    540 0xd4a1451124438ca93a92d43d922e33cbbb7ab1b3      1
    541 0xd51ce9be4a1cb6185b76ba825c59236a6cf5ca2a      1
    542 0xd5927446a85b2dc8b28144f608de76c98fbc4e2a      1
    543 0xd5f9405bbc27934b615a64c43407a03f5492da56      1
    544 0xd60a65bf3a24e02bad84a87eba6820f9c933a809      1
    545 0xd6208d782261c3df0015d15e4156ce80a32b6c22      1
    546 0xd620ce4e4c38abbd268cc1580de2ebd154763adc      1
    547 0xd68cd858184ef4c75e72a50710b7efcd3c5f060d      1
    548 0xd7272f37e384b594e885237aa29013cb49295e14      1
    549 0xd84c9774ddf51a5d1fcfbe022f9d93e36eb4c23c      1
    550 0xd8b902c3817f11725fd93b88e5e862e612ff3988      1
    551 0xd905de91f928b20ea0ccf455e5ba07eb6373955c      1
    552 0xd9736c47ef4e0364b29c9da102c8d09810ec32c3      1
    553 0xd9c09e1ddacce4ee6548b66773bf4e6fe9dd0882      1
    554 0xda6bb6703b2802e44ff4087962742b839b7a6e1c      1
    555 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1
    556 0xdadddee3daaa02f854634c56067aeb658ff42b4f      1
    557 0xdae2d80e803e7e7bc279309ede4e039788b4936d      1
    558 0xdb056a6e0c8831f53637b61e060abd0b50204074      1
    559 0xdb90f65394a4579e33de03dacf1d4157fd027f38      1
    560 0xdbbb4e0ff76d32869b098106b83c4043e9d2e50c      1
    561 0xdbd8c21abbaa205c0e87465f9d4695137727741c      1
    562 0xdc41f9a65a822886ba17537c6bd99c9a462c22d2      1
    563 0xdc714109faf8170a3a70304bdfc2c88e5a055045      1
    564 0xdcf04ebdb1a597ca1ea7b7ff28ff016b0588390f      1
    565 0xddea95566f1e9c7b53f7f2e7449e2aa9c2be0cc9      1
    566 0xde40d2cb9db9b1cc295e18fb2b12612a66b5c5d6      1
    567 0xde70bdf7dfe13614d281716813274380a59e3e5d      1
    568 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    569 0xdf4b127566fe6a268a49c0cc36b3bb13d2c2d567      1
    570 0xdf58068495af8eab3375ef2117959f07d536dae6      1
    571 0xe0ca5b132965d3668973f44c589800102cf25e4b      1
    572 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    573 0xe113e7b1e8ecd07973ea40978aed520241d17f27      1
    574 0xe11dc16c1880cec9d430b40596ae994d940e0a85      1
    575 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
    576 0xe1e6c935dec5356214346afe629e0b085785c5c6      1
    577 0xe288a00df4b697606078876788e4d64633cd2e01      1
    578 0xe310b6d726c1a3c3537a7bfe63bbd73f005fa429      1
    579 0xe3c1b623c2f51b243e174d4498b07b3c97fda871      1
    580 0xe3f27deff96fe178e87559f36cbf868b9e75967d      1
    581 0xe3fbe8b5bd672db6fc66889e9dca8cd73f9be164      1
    582 0xe4cd5b52b309fd29a30ad24a4ec55af76d1c92a1      1
    583 0xe4d493e868e19baffebf2f12ef05a4149b84e221      1
    584 0xe5362637b8ee58411c7ff94ce7e49701af5f00d5      1
    585 0xe59570a88cce155cfef63b30f0c00fe6c30b5297      1
    586 0xe5c618d20d24346002c1b0d65f4f0150d5056935      1
    587 0xe65ba2667f6cdab2e3f9f202972be47c3737811d      1
    588 0xe684aedcb17d70923dd50ac757ecedc43d86cc49      1
    589 0xe695618c6e1d2cfe8abca79bdeecb79d72cc2ea8      1
    590 0xe71758660904d74618c0f70d6606bc270096c899      1
    591 0xe744544f50fc6bde59cd8ed3e656487d4791357f      1
    592 0xe7dae42dee2bb6c1ef3c65e68d3e605fabca875d      1
    593 0xe95b6b567bd6938883dc92ac223c7d63c61bfc02      1
    594 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    595 0xe9ec96160d092b283cbae7d62f6a54d4a8253d81      1
    596 0xeb197b76f320c854bc18dbf62bd33a28663abeee      1
    597 0xeb4c5c24468c3e666d01257b76a73326411b7928      1
    598 0xeb90141895a0ff1839e4846362ae780b9796644f      1
    599 0xeb946bfac0d78e7f0d12d53e5f5b890bff25c7db      1
    600 0xec51874c842305ac63f7500f6e14e5791dad07c5      1
    601 0xed9e12f73fb7c8fe7abe92c3cab65b90fda52bd2      1
    602 0xedfae7d120bfa17eb4ec7a689a5bc20e72bb4ef4      1
    603 0xee0c857703bc21f982a0db25929730f9b5d4791d      1
    604 0xee1f85c00207c3354231ff75b661211c725ab5d1      1
    605 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    606 0xee64277377d728578ee3b4b8e3d6f48d965acff9      1
    607 0xee7a6b828169ced383e3dd6b54cd1b33cc1edf94      1
    608 0xee9158199b2a11696f048a1151156bcdbe431e32      1
    609 0xeec0efa48234b92388989b3c7f74dd2401594b7b      1
    610 0xeed4242f735fa70ed1cf30deae41efb793ea01f0      1
    611 0xef728cdeb37a080ab86312e860a956122d3a6252      1
    612 0xf0bda1946d4b2cef6943a07f8814ebb05aff0cf0      1
    613 0xf0bf1c59ee9b78a2ce5763165e1b6b24cb35fd8a      1
    614 0xf1a9f5aeb0f975489ac2628a22040cf42e9fe8dd      1
    615 0xf2439241881964006369c0e2377d45f3740f48a0      1
    616 0xf2565fd865648012da5b8c4c8a6b444e244ed250      1
    617 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    618 0xf381240af0f2f2fcc3c014e42a40032af9791962      1
    619 0xf39df112ea070c93f2841650805fde216218a572      1
    620 0xf3db4aa3dafca94e6d43bb24776414569b75883e      1
    621 0xf417315f03db70d1ca6f40c50958ac4d8cae2f55      1
    622 0xf47588a5a54a0a2a1de1863a88a120bbc0b4b777      1
    623 0xf4b8b9232fdccda76ec9285a91eca9892ed9bb15      1
    624 0xf506a20c1fc521340740fd6507dc819140c6ced6      1
    625 0xf520aaf89eea21fc9366b3cf3cd9206f0a24bdbb      1
    626 0xf6c47f1679c550b480139fbc2c7eaec75d409659      1
    627 0xf7cd2cac5f58f2d92d6e50da38c147788fe6aad4      1
    628 0xf800d8407b1488bb6dc3789c2d45147c25c38af5      1
    629 0xf839413480bdd4d426558f0ef06c72751d0e769e      1
    630 0xf8ab1f75e08a0d4f58922c8b3ec3351fcc1eeab0      1
    631 0xf8ba44c82a608df0678df28ae6deb31882947aa0      1
    632 0xf8c99cb131c793bf563794738e9f16b25fcb6781      1
    633 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    634 0xf96bcd2784e052a367a0e5e6ec43ef30372690e2      1
    635 0xf97e1da1b688dd6a6f1cac2746ddf242de6ca172      1
    636 0xf99027e4295441256b0cee102bfd64fdb41070a7      1
    637 0xf9caa0e790a9a89fbd84e0cf1b455eeb1dc50d1d      1
    638 0xfa0611efe7a1f4ce14983aea59edcceca73e2e7a      1
    639 0xfa18da7e2db0802312bf47ed927ccffeeba4036a      1
    640 0xfae8b043881608706a8d79e92fd4086972445c7c      1
    641 0xfaff9a8e78ba6ceaee89269b6d7a00e97d097ed0      1
    642 0xfb9d2eb56e88607ee2a77797e35b4ebed934b791      1
    643 0xfc199e896bb4a153046bacb8b3c3170079190705      1
    644 0xfc3ba73f6ebc680ce855557fabf9581f7b783d08      1
    645 0xfc70950390918d1b1d5716b0aeb34a3061879305      1
    646 0xfca3891ed59d9ac1a5572493f9de470ac680f500      1
    647 0xfcc2d631c320035bf59e48a49c5f729ebb452ebc      1
    648 0xfd4a44535431e43e43deacad1948e7b552b5684a      1
    649 0xfd4b763bccd7594a2e596016daeec65ea86148e7      1
    650 0xfde36cbdd65160bd83541f060f44dbafcf0a0ab8      1
    651 0xfe0e5b8179419d241ce20cc094150ac4e912ea59      1
    652 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    653 0xfe36d8ad2d4fbc2f27c4982dbf81b881bf8b2b07      1
    654 0xfebaeca1976afa7a12298ec2016a0497253f2e1a      1
    655 0xff0bfea882373e98572a280d852edfeb675a57d2      1
    656 0xff190a4cc92b154635140335791d3779f60dc311      1
    657 0xff5269e8ae6a9f2ff2a93c770fd35f8ce71712f8      1
    658 0xff9b0054cca48713c2aaf1eb3c7e79fb9abf7d35      1
    659 0xffe5722ece6404d5811061bb6643c8e659df0416      1
    660 0xfff0ebdd920dc90e8313cc875b9a9940362ea20a      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529_phase3.csv", col_names=FALSE) %>%
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
