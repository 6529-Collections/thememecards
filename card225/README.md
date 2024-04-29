
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:388         Length:388         Min.   : 1.000   Length:388        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.456                     
                                           3rd Qu.: 1.000                     
                                           Max.   :25.000                     
         name          
     Length:388        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19744269 # https://etherscan.io/block/19744269
block_hash <- "0x88f1424201eaecaf3dec7860ef4322725466e7469739f0b818d86bcb89b3af9e"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4632 

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

allow_artist1    <- pick(snapshot, contracts=c("RecoinsduCoeur","ColourWheel","OrabelMainCollection","egomania","Foundation","PLURALFACES","LesFilles"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("diamondsEditions","KnownOriginEditions","KnownOriginEditions2","FeelingsEditions","thankiuEditions","CroquisEditions","MathandCryptoEditions","PrismaticaEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("RecoinsduCoeur","ColourWheel","OrabelMainCollection","egomania","Foundation","PLURALFACES","LesFilles","diamondsEditions","KnownOriginEditions","KnownOriginEditions2","FeelingsEditions","thankiuEditions","CroquisEditions","MathandCryptoEditions","PrismaticaEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 58 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0183d9be08db4024c2dc9aec10b9526fc6adeef7      1
     2 0x01a4582fe0a70e0b7edcef5f6096ae87499fc1ae      1
     3 0x05c232ce5bcc9bfdb838b41a7870aea5e4fa0fa8      1
     4 0x0a0686a3624ab67cbd2ef6cb569cd0b53d7c4033      1
     5 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     6 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
     7 0x172059839d80773ec8617c4cb33835175d364cee      1
     8 0x193dd0842ea6a0e3b41d9669f088ed3d642f3f10      1
     9 0x2901a2b2e8069d9e9bc378957645279ee0b8828f      1
    10 0x2a00f63af45627ff351549106ea735bd973aa86e      1
    11 0x2ae4f87745c3431005babffc94d5b0c5090b0365      1
    12 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
    13 0x2bba7d62012faaba01c45aec96c742fbfee8ce0e      1
    14 0x2bcea5de9c2411fe02b0dc1cdcec71149a682439      1
    15 0x2c90f88be812349fdd5655c9bad8288c03e7fb23      1
    16 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
    17 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
    18 0x45fec0b9d95155f695e1078e177abb234c70472d      1
    19 0x4744cda32be7b3e75b9334001da9ed21789d4c0d      1
    20 0x512c6c58f2256ea86777b349d84e88bfbc155bc7      1
    21 0x53f778c4c00cc5b1776ad20ffaad1bcf2146698c      1
    22 0x5e470e0c9865a6207642161d55403b64c44844a3      1
    23 0x68acff57bdb5a9169d209e6539d6492fafd822e8      1
    24 0x6bf749e0745ac7222e4f8697ee648060f719d421      1
    25 0x6f31779b86eb507f110ce4b506ef022e500828a3      1
    26 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
    27 0x7442ae0096b7065d6de11f204f418b7ba1859317      1
    28 0x8335932a9bc745f3abce2d0092b33cd375ac2b51      1
    29 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    30 0x912a3fb96154bda9d52b067abfd4f940152fd413      1
    31 0x94c878b7dd00654e42a27fc6b747a97125e7bf8b      1
    32 0x95bb5c078baa7380d9c6bfcb9237c6c8c32d5184      1
    33 0x98817e88a18159dde7716d2a1b3edf01fae4d399      1
    34 0xa5a37c94c0127679052938bf80795c8452e9379f      1
    35 0xaa1a54bfa93be8ac8d01182827433f227ba59e17      1
    36 0xad3372cd209550e03aeeba8a756688d6255f94eb      1
    37 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    38 0xb896c515f140efb49bb4eca5e24fe2e8090c21e1      1
    39 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    40 0xbb06b7363d416fae8ddef474776e0c1ce2657c4a      1
    41 0xc112ab488eb78b8ff702aef53f5d766272fe4087      1
    42 0xc734fce96d3dd328fd8bed5922cdee54ba3beeb6      1
    43 0xc86fc577e85c0a4bfe66c5c9ec3f6295eb63528c      1
    44 0xcad645a7f6ce207b2b4a1286372acb9cb86331e8      1
    45 0xcaedd5e74bb3ee95ce8f564c9dda431c146c5836      1
    46 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    47 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    48 0xd6f6385f9650015def77f5437f2f7c5ee3d2f372      1
    49 0xd8bcbc5e79e42bee67465c340bc8ee2aae16514d      1
    50 0xdb57a237bbd016f6da776b892fe7e75d8b495e62      1
    51 0xe2d128323cf7560a6e7a82726d7b425aedc7a556      1
    52 0xe41532a746f18b9db7ae523e75092d06f5d4cc55      1
    53 0xecd5e98bd002708868f36bb6daa29e16f11b98c0      1
    54 0xecdd7902903077f4d185d5fae4895318a050ae85      1
    55 0xee4243eea50d8840f459da4fada53679aec1c702      1
    56 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    57 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    58 0xff33f5653e547a0b54b86b35a45e8b1c9abd1c46      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x054c3c10ca7811f94e699760fef4d5d602b4434b      1
      2 0x05de930defd17a1d14ce8b5db5e0903a89ed250b      1
      3 0x086d21b3125e01338ed4757b9ce8b7b44130e07d      1
      4 0x0925d347f811d264879271d2905f54309eaccb93      1
      5 0x0a2542a170aa02b96b588aa3af8b09ab22a9d7ac      1
      6 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
      7 0x0aaa92f5aca880179e6c53f13c95d42537b2a1b6      1
      8 0x112be8010ac3ae3aac99490002555297438ac394      1
      9 0x116d55e17d9a1fd341f9938ef5755a6f5f3a96ad      1
     10 0x14229c6e20f4b5c30b88f93a59711cf435dd2020      1
     11 0x1a75950c1b1f63c09a4eeb8d27f5ed2cb9707607      1
     12 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     13 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     14 0x2164b7c16e2c384f9518d53b004d9f2a63a08135      1
     15 0x2254cb1413b0a7d5dc4bf0c71e52c32f991468f5      1
     16 0x25225734320010e49b64ab91eeca184f51fc4191      1
     17 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     18 0x2b1683a4340de00c9301a29cccfd182f6843849f      1
     19 0x2c123fc5c27888571cd525e8ae9b0c5ff848386d      1
     20 0x2c6eadd3a66d3aa0cb000e5bfbc714f0c477a17e      1
     21 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
     22 0x2d36fcb71196beff330a07197a78d87bd1447b58      1
     23 0x2d3ffd5f6624e1bc063c1fb1722011cda927f5de      1
     24 0x2f9eb05f0c01f62e8b5ff5e72ab4f821b0f956ae      1
     25 0x2fe50c88f228dacfc24100de0c5167aa7a539dc6      1
     26 0x30c79e9ae1b939f122db705111c8f721559e1b23      1
     27 0x35aeb06054140e1b9d8974d97e5b422ca7449157      1
     28 0x38835098a8accd046efa8123f9fcd5aaa2cb3a7e      1
     29 0x420afea8152c4e038b18ac54898ca9b4e2e1f4b1      1
     30 0x484ed89966cb13780801f3b621601e5a8961238e      1
     31 0x53ed40c04d7bf1a908e0c57e0a889b85e534ea4e      1
     32 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
     33 0x5e2889eb037aeac4f64fbb9856514f6dfebada63      1
     34 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     35 0x628d28bf9c0f2f8328d89c2a24870c119a12a8e0      1
     36 0x6368ce2750325f5002587c70c8bc298b4df61eba      1
     37 0x651350a0ddab993dd69eb18fe63914a90bdca006      1
     38 0x668373aa60f3bdc7d254df5a1cde22c510c492c4      1
     39 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     40 0x6b711e387693cded325c5a0c334594060034a1a8      1
     41 0x6b913441148b57e717c95e8b035199624303c401      1
     42 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     43 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
     44 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     45 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     46 0x6e7256e46b5343f3e8d0bbd3b4f63b90c531c263      1
     47 0x70e49c1e07ed456f03f971b844564f9196131799      1
     48 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
     49 0x762da606029d3120735aa1eec15464e265db7a3c      1
     50 0x76409ee20d9a4a3b89ca3bd6c7585c014fff9363      1
     51 0x7812fc0b855f14d4221bba01fd656b5328ec4ae3      1
     52 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     53 0x78da99cd35cc3939d4bb94707b33469b544b6eaf      1
     54 0x794af4513f50aa4f99cb99dffbdc71c2d6967f6c      1
     55 0x7e6c1222d574d58ae437f746d2ef349958bfc5a0      1
     56 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     57 0x8887236456b4394e5bdd0a1814123507c5032cf8      1
     58 0x88f6cd7d33ce0b4028e2eaee2cfa6586348dda8d      1
     59 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
     60 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
     61 0x908b770a12816b29796b1635bd2b5cc50e30982f      1
     62 0x91b7f6b94554cf9e1dc041d9a15f5bec30b71166      1
     63 0x92baffdd6cfb11a4e57a58ffec4833b4d1abd25d      1
     64 0x92febb94ab6b159a758316c7640e846e14603921      1
     65 0x93f7cb21d6904492b33e0df24008c8b13ce64380      1
     66 0x98b210fb3db750546ecb7357ae9c54bfcec54b0f      1
     67 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
     68 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
     69 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
     70 0x9ba9160e463f974444397e8f20a2470052f58823      1
     71 0x9cf0dd406ee9e8a5fd1af7f7473d45fd7fa3543e      1
     72 0xa0d55039834d2373014b66a322b49e32f4869f49      1
     73 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
     74 0xa25803ab86a327786bb59395fc0164d826b98298      1
     75 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
     76 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
     77 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
     78 0xa5d8dec9f9f8e20be782b2932b3c4c6629d6e2b2      1
     79 0xa6d672b63bf2cf328cd954c51c5a1e335ce2afa1      1
     80 0xa750329b5821d02e0a3977d36ffc5f9de51f0fb6      1
     81 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
     82 0xab48edd90bdf367d326d827758bacd2460c59d17      1
     83 0xab85fc70c4663bfb702af12efe2f83cc304e6d01      1
     84 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
     85 0xac69ec334f07d67db1c9bd79454acb77c3342dcd      1
     86 0xb44af26ff1402477821f8f7a993251a3bd6a8b06      1
     87 0xbaca8a4528ce21edd8d3ff46220151427868653a      1
     88 0xbd9b7373aac15d9a93c810df3999343f4fe1ed88      1
     89 0xbf3aae135034452987490d6587b42704e617cf79      1
     90 0xc1deb3e48461dddc4a4791b11f89d22079d036fe      1
     91 0xc2b170c31caad754a38a8bee9570d89591911c81      1
     92 0xc44505d1111ec42279aede122a77cba17359438d      1
     93 0xc723f9bb6c7e8077e4f34942377280ebf098f9b5      1
     94 0xc991fd20ffd2ee620a73ea7dadd051fe86d39b15      1
     95 0xc9d7549222b94eb3d5ec1a9571e9dd53bf14e47e      1
     96 0xcc3e42c55a9874cec685fb88c107dc77a50eed9b      1
     97 0xcc61aaafaac195706ccb5e59e48fcbde7afec199      1
     98 0xcebc355929db17dcda82738320d3d12a48c21a3f      1
     99 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
    100 0xd3f714357a3a5ab971941ca475a006814edd0d2b      1
    101 0xd514f2065fde42a02c73c913735e8e5a2fcc085e      1
    102 0xd7d19581d391fdde782e3f84f5436367dd6add98      1
    103 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    104 0xe57a90d4fc87084a39acd908a010e649bb5f6b72      1
    105 0xe7677698617c6a3d4676eb787c1bc10081d5b8d3      1
    106 0xeabdf7d6adab860a0fc332352c83ecabd304d176      1
    107 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    108 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    109 0xefcd6473f8cece284415aeae5c4de971709cf121      1
    110 0xf248a71dc0b1bbd4ffb6093207c78a9eecd1c2ca      1
    111 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    112 0xf861cfd2b510f921b20fe840784c1690177253a9      1
    113 0xfd2596c60f44f39fea7dfc51c34bd9dca20b7464      1
    114 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 172 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0183d9be08db4024c2dc9aec10b9526fc6adeef7      1
      2 0x01a4582fe0a70e0b7edcef5f6096ae87499fc1ae      1
      3 0x054c3c10ca7811f94e699760fef4d5d602b4434b      1
      4 0x05c232ce5bcc9bfdb838b41a7870aea5e4fa0fa8      1
      5 0x05de930defd17a1d14ce8b5db5e0903a89ed250b      1
      6 0x086d21b3125e01338ed4757b9ce8b7b44130e07d      1
      7 0x0925d347f811d264879271d2905f54309eaccb93      1
      8 0x0a0686a3624ab67cbd2ef6cb569cd0b53d7c4033      1
      9 0x0a2542a170aa02b96b588aa3af8b09ab22a9d7ac      1
     10 0x0a690b298f84d12414f5c8db7de1ece5a4605877      1
     11 0x0aaa92f5aca880179e6c53f13c95d42537b2a1b6      1
     12 0x0eb47beabd9ce2cab7cbed57aa6b040975bdc1b7      1
     13 0x0ed0d0f3a5f831d1567ad6f8e58280977a1b205c      1
     14 0x112be8010ac3ae3aac99490002555297438ac394      1
     15 0x116d55e17d9a1fd341f9938ef5755a6f5f3a96ad      1
     16 0x14229c6e20f4b5c30b88f93a59711cf435dd2020      1
     17 0x172059839d80773ec8617c4cb33835175d364cee      1
     18 0x193dd0842ea6a0e3b41d9669f088ed3d642f3f10      1
     19 0x1a75950c1b1f63c09a4eeb8d27f5ed2cb9707607      1
     20 0x1e2fa044a25bca1576ed94562db50f51dcc24ca8      1
     21 0x1eea95f2d2ed24cd3451da93a69efdd08767cc5b      1
     22 0x2164b7c16e2c384f9518d53b004d9f2a63a08135      1
     23 0x2254cb1413b0a7d5dc4bf0c71e52c32f991468f5      1
     24 0x25225734320010e49b64ab91eeca184f51fc4191      1
     25 0x2901a2b2e8069d9e9bc378957645279ee0b8828f      1
     26 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     27 0x2a00f63af45627ff351549106ea735bd973aa86e      1
     28 0x2ae4f87745c3431005babffc94d5b0c5090b0365      1
     29 0x2afc032aaf6d7ead33fb05f9d42d874874dfb9d1      1
     30 0x2b1683a4340de00c9301a29cccfd182f6843849f      1
     31 0x2bba7d62012faaba01c45aec96c742fbfee8ce0e      1
     32 0x2bcea5de9c2411fe02b0dc1cdcec71149a682439      1
     33 0x2c123fc5c27888571cd525e8ae9b0c5ff848386d      1
     34 0x2c6eadd3a66d3aa0cb000e5bfbc714f0c477a17e      1
     35 0x2c90f88be812349fdd5655c9bad8288c03e7fb23      1
     36 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
     37 0x2d36fcb71196beff330a07197a78d87bd1447b58      1
     38 0x2d3ffd5f6624e1bc063c1fb1722011cda927f5de      1
     39 0x2f9eb05f0c01f62e8b5ff5e72ab4f821b0f956ae      1
     40 0x2fe50c88f228dacfc24100de0c5167aa7a539dc6      1
     41 0x30c79e9ae1b939f122db705111c8f721559e1b23      1
     42 0x35aeb06054140e1b9d8974d97e5b422ca7449157      1
     43 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     44 0x38835098a8accd046efa8123f9fcd5aaa2cb3a7e      1
     45 0x3fc6c595a3445d56bf5d8322a392f3e027197c6a      1
     46 0x420afea8152c4e038b18ac54898ca9b4e2e1f4b1      1
     47 0x45fec0b9d95155f695e1078e177abb234c70472d      1
     48 0x4744cda32be7b3e75b9334001da9ed21789d4c0d      1
     49 0x484ed89966cb13780801f3b621601e5a8961238e      1
     50 0x512c6c58f2256ea86777b349d84e88bfbc155bc7      1
     51 0x53ed40c04d7bf1a908e0c57e0a889b85e534ea4e      1
     52 0x53f778c4c00cc5b1776ad20ffaad1bcf2146698c      1
     53 0x5d4e840aee5c438934a4dbf8687d9624bca637d8      1
     54 0x5e2889eb037aeac4f64fbb9856514f6dfebada63      1
     55 0x5e470e0c9865a6207642161d55403b64c44844a3      1
     56 0x614a61a3b7f2fd8750acaad63b2a0cfe8b8524f1      1
     57 0x628d28bf9c0f2f8328d89c2a24870c119a12a8e0      1
     58 0x6368ce2750325f5002587c70c8bc298b4df61eba      1
     59 0x651350a0ddab993dd69eb18fe63914a90bdca006      1
     60 0x668373aa60f3bdc7d254df5a1cde22c510c492c4      1
     61 0x66f5ddc253852f407d334f8e90e3bc46fdaaecaa      1
     62 0x68acff57bdb5a9169d209e6539d6492fafd822e8      1
     63 0x6b711e387693cded325c5a0c334594060034a1a8      1
     64 0x6b913441148b57e717c95e8b035199624303c401      1
     65 0x6bf749e0745ac7222e4f8697ee648060f719d421      1
     66 0x6bfd30747ba72ac7bb2f20d6138e1312020fafe8      1
     67 0x6c40798acaec7502c3d276cfb4e79423571a5b0a      1
     68 0x6d4fdc55136dee5fb4f27a278c9cc35ad01d2d3c      1
     69 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     70 0x6e7256e46b5343f3e8d0bbd3b4f63b90c531c263      1
     71 0x6f31779b86eb507f110ce4b506ef022e500828a3      1
     72 0x70e49c1e07ed456f03f971b844564f9196131799      1
     73 0x7405fe24003a50e4f4117d35e9b5a9f5e512fede      1
     74 0x7442ae0096b7065d6de11f204f418b7ba1859317      1
     75 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
     76 0x762da606029d3120735aa1eec15464e265db7a3c      1
     77 0x76409ee20d9a4a3b89ca3bd6c7585c014fff9363      1
     78 0x7812fc0b855f14d4221bba01fd656b5328ec4ae3      1
     79 0x7895e668bd43c9623af538f274c06cb22cbc74f2      1
     80 0x78da99cd35cc3939d4bb94707b33469b544b6eaf      1
     81 0x794af4513f50aa4f99cb99dffbdc71c2d6967f6c      1
     82 0x7e6c1222d574d58ae437f746d2ef349958bfc5a0      1
     83 0x8335932a9bc745f3abce2d0092b33cd375ac2b51      1
     84 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
     85 0x8887236456b4394e5bdd0a1814123507c5032cf8      1
     86 0x88f6cd7d33ce0b4028e2eaee2cfa6586348dda8d      1
     87 0x8aef89129806b23c7930dfdf2b46e22ae1849c49      1
     88 0x8d4228752c94cc70c72595f45f8b0d8d869f7fdd      1
     89 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
     90 0x908b770a12816b29796b1635bd2b5cc50e30982f      1
     91 0x912a3fb96154bda9d52b067abfd4f940152fd413      1
     92 0x91b7f6b94554cf9e1dc041d9a15f5bec30b71166      1
     93 0x92baffdd6cfb11a4e57a58ffec4833b4d1abd25d      1
     94 0x92febb94ab6b159a758316c7640e846e14603921      1
     95 0x93f7cb21d6904492b33e0df24008c8b13ce64380      1
     96 0x94c878b7dd00654e42a27fc6b747a97125e7bf8b      1
     97 0x95bb5c078baa7380d9c6bfcb9237c6c8c32d5184      1
     98 0x98817e88a18159dde7716d2a1b3edf01fae4d399      1
     99 0x98b210fb3db750546ecb7357ae9c54bfcec54b0f      1
    100 0x9997205cd290fb2d7bf537c919a572d16e5020a1      1
    101 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
    102 0x9a569681630473506a77d8abfb1636cdf8744d9a      1
    103 0x9ba9160e463f974444397e8f20a2470052f58823      1
    104 0x9cf0dd406ee9e8a5fd1af7f7473d45fd7fa3543e      1
    105 0xa0d55039834d2373014b66a322b49e32f4869f49      1
    106 0xa12a9cb850ddf6abe695949a50465dc1d6b3a34e      1
    107 0xa25803ab86a327786bb59395fc0164d826b98298      1
    108 0xa3ec0ea98a1a12c56a75e214a8972809e7594362      1
    109 0xa4e9a636d9ec9472c3ebb1430266b634c4ac114f      1
    110 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    111 0xa5a37c94c0127679052938bf80795c8452e9379f      1
    112 0xa5d8dec9f9f8e20be782b2932b3c4c6629d6e2b2      1
    113 0xa6d672b63bf2cf328cd954c51c5a1e335ce2afa1      1
    114 0xa750329b5821d02e0a3977d36ffc5f9de51f0fb6      1
    115 0xa837c3d5fdfe4e878a3f8370df89cfd972b116e1      1
    116 0xaa1a54bfa93be8ac8d01182827433f227ba59e17      1
    117 0xab48edd90bdf367d326d827758bacd2460c59d17      1
    118 0xab85fc70c4663bfb702af12efe2f83cc304e6d01      1
    119 0xabd9643f92e771831a6dcd588844a75b03b0ac12      1
    120 0xac69ec334f07d67db1c9bd79454acb77c3342dcd      1
    121 0xad3372cd209550e03aeeba8a756688d6255f94eb      1
    122 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    123 0xb44af26ff1402477821f8f7a993251a3bd6a8b06      1
    124 0xb896c515f140efb49bb4eca5e24fe2e8090c21e1      1
    125 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    126 0xbaca8a4528ce21edd8d3ff46220151427868653a      1
    127 0xbb06b7363d416fae8ddef474776e0c1ce2657c4a      1
    128 0xbd9b7373aac15d9a93c810df3999343f4fe1ed88      1
    129 0xbf3aae135034452987490d6587b42704e617cf79      1
    130 0xc112ab488eb78b8ff702aef53f5d766272fe4087      1
    131 0xc1deb3e48461dddc4a4791b11f89d22079d036fe      1
    132 0xc2b170c31caad754a38a8bee9570d89591911c81      1
    133 0xc44505d1111ec42279aede122a77cba17359438d      1
    134 0xc723f9bb6c7e8077e4f34942377280ebf098f9b5      1
    135 0xc734fce96d3dd328fd8bed5922cdee54ba3beeb6      1
    136 0xc86fc577e85c0a4bfe66c5c9ec3f6295eb63528c      1
    137 0xc991fd20ffd2ee620a73ea7dadd051fe86d39b15      1
    138 0xc9d7549222b94eb3d5ec1a9571e9dd53bf14e47e      1
    139 0xcad645a7f6ce207b2b4a1286372acb9cb86331e8      1
    140 0xcaedd5e74bb3ee95ce8f564c9dda431c146c5836      1
    141 0xcc3e42c55a9874cec685fb88c107dc77a50eed9b      1
    142 0xcc61aaafaac195706ccb5e59e48fcbde7afec199      1
    143 0xcd432c8f076b740cb7be693a18983218588d96ca      1
    144 0xcebc355929db17dcda82738320d3d12a48c21a3f      1
    145 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
    146 0xd3f714357a3a5ab971941ca475a006814edd0d2b      1
    147 0xd514f2065fde42a02c73c913735e8e5a2fcc085e      1
    148 0xd6ee002752c995a118e42611baf52aba7cc1fef1      1
    149 0xd6f6385f9650015def77f5437f2f7c5ee3d2f372      1
    150 0xd7d19581d391fdde782e3f84f5436367dd6add98      1
    151 0xd8bcbc5e79e42bee67465c340bc8ee2aae16514d      1
    152 0xdb57a237bbd016f6da776b892fe7e75d8b495e62      1
    153 0xe13d4abee4b304b67c52a56871141cad1b833aa7      1
    154 0xe2d128323cf7560a6e7a82726d7b425aedc7a556      1
    155 0xe41532a746f18b9db7ae523e75092d06f5d4cc55      1
    156 0xe57a90d4fc87084a39acd908a010e649bb5f6b72      1
    157 0xe7677698617c6a3d4676eb787c1bc10081d5b8d3      1
    158 0xeabdf7d6adab860a0fc332352c83ecabd304d176      1
    159 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    160 0xecd5e98bd002708868f36bb6daa29e16f11b98c0      1
    161 0xecdd7902903077f4d185d5fae4895318a050ae85      1
    162 0xedba5d56d0147aee8a227d284bcaac03b4a87ed4      1
    163 0xee4243eea50d8840f459da4fada53679aec1c702      1
    164 0xefcd6473f8cece284415aeae5c4de971709cf121      1
    165 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    166 0xf248a71dc0b1bbd4ffb6093207c78a9eecd1c2ca      1
    167 0xf52393e120f918ffba50410b90a29b1f8250c879      1
    168 0xf6b1a88d33ef451cfb23f2172ffb8860f7844fb6      1
    169 0xf861cfd2b510f921b20fe840784c1690177253a9      1
    170 0xfd2596c60f44f39fea7dfc51c34bd9dca20b7464      1
    171 0xfe588c79166bf423ddce78d7f266d5651c8405dc      1
    172 0xff33f5653e547a0b54b86b35a45e8b1c9abd1c46      1

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
