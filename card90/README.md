
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:29889       Length:29889       Min.   :1   Length:29889      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:29889      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17107669 # https://etherscan.io/block/17107669
block_hash <- "0x5457452b7b46f122c5d60c365476661733457c9d1c771ee1fac6184b9833443b"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4279 

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

airdrop_meg    <- pick(snapshot, contracts=c("Foundation","Genesis"), address_remove=address_remove,address_pick=8,address_max=1)


allow_meg_phase1     <- pick(snapshot, contracts=c("Foundation","Genesis"),address_remove=address_remove,address_subtract=airdrop_meg,address_max=1)
allow_memes150_phase1  <- pick(snapshot, contracts=c("memes"), address_remove=address_remove,address_pick=50,address_max=1)
allow_gradient_phase1  <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Artist

``` r
c(airdrop_meg) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 8 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x1a962d6a95cea8060b1e9f55c5bc6622315c0108      1
    2 0x2e26fa722c032ed0b6068bbd7aa9c017e4d52759      1
    3 0x53dda0a69992b87e51f0f14b1337281f09a2c8da      1
    4 0x540e16d0e898e45850ffa5bbb4d117d5aaef8f0b      1
    5 0x6250ad95533b1f6d2ff1ffcb3df7d31cec0b1271      1
    6 0x7015853f4be641296f6c92690ff69fd36704dc2d      1
    7 0x73010bac31886ce742d2a8283321ac73d193bce0      1
    8 0xe35e2e4624987575dfd263b124a3999e180b8b89      1

## Allow Artist Phase 1

``` r
c(allow_meg_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03285482fa5114dde09084dbef4e5c4f8034cb8f      1
     2 0x04c1c15a1554891fb2dd7c4939e7d95d06ee253f      1
     3 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
     4 0x085843dbde124ab8babd558fea60534962628338      1
     5 0x0a2de356a990f3efacb1582b323b8f5dc156d5fa      1
     6 0x0b5dd26f44d36141d97a3bf6841e7ba63c092af9      1
     7 0x0e63d7e489363028e23a6da417d5767f9e399246      1
     8 0x183b6267cb9f985a8aa7fc912672a21d644a4102      1
     9 0x1a08ba6d9dea10e5b502208c78513c4753ad67a6      1
    10 0x1cadb2b82e5c3276242c6894e4ea22e23ca68daf      1
    11 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
    12 0x223db826013eaf2c46aab1cb2ae6e8b652f9dad6      1
    13 0x22bddf61d288dac8c2da1584f5d78ceeae4d21ee      1
    14 0x24024f20853aa10267f71f03c7f29b4567b56955      1
    15 0x24141a358980b41084a487bb39c5e0a95b6e6559      1
    16 0x2649c4628da55e874c85fa0bcb189777cd41b8ad      1
    17 0x281dc2b4b1319b7799fb0d3adfa04f52b170b1c2      1
    18 0x291121da7faeedd25cefc0e289b359de52b8050c      1
    19 0x29d1784af2d11a3d1073ccc9870388375a8c21b9      1
    20 0x30eeed8b7aa55a48728cfd7a1f7440c393faf582      1
    21 0x3265a39fa1bdd28b9029568208e3cb4639adbf3e      1
    22 0x34800c626ac4a37216ac84594c4c9767015a5636      1
    23 0x372f5344045ecce94f596103d7c7ee6f6cf50d46      1
    24 0x37b689a445e096c3f52b334511bf412a610359b9      1
    25 0x39aacbae8a4718e81b0cc22ec61bd27bf18265f1      1
    26 0x3ba53331f1084ad49b5b13da7882165b52879ba9      1
    27 0x3c07136cd054ca5640c8ea0d4c16c3d600b4e879      1
    28 0x3d6c71e2f3bf4accd0578e9f0d8d4d14a6c3b9c8      1
    29 0x3e1104cb75e81767c8a40eed02505267838718fb      1
    30 0x3e35232f43041e4f8a95239f191b71ba975b168c      1
    31 0x4b2a7123ab809670c7989a58b2ef8338028c791f      1
    32 0x51ed46a3c9165e65cf79f5db390017e05da0038a      1
    33 0x521ee6589c870aea565671a759f9b95cbf7f44e0      1
    34 0x55cf34223929a9d893c1ea402735a3a6fc6e5f74      1
    35 0x573555dc420c87082143e0b9af96b3413c7514a0      1
    36 0x594c6b641626f4ad29aa2bdd046d7ba678f1b7ce      1
    37 0x59982b331530a6aacf200f1b9cd40cb451609cbf      1
    38 0x59dbce846aa40f138d805a7cebe2afb0c33066a3      1
    39 0x5e1ff82e8f3db1bacc4466a3d601e03ce17739f1      1
    40 0x68858e8270ab4858eadbe94d0adf609693c59c82      1
    41 0x6a2d60031529a90c2c550af5df820481e70a5a8e      1
    42 0x70be33fcd9428cbb81a2425a613712bb8e085795      1
    43 0x73d05c2ea70dfc3b220444c94567dbc84bb0d24c      1
    44 0x74a2a4705de72f8b5e49adc1c6f4a338193890a2      1
    45 0x7d7ea735b7287d844c3a9312bc1114c6131781d0      1
    46 0x7e8fcd1046ae351af66c12b75b051fe550afe71e      1
    47 0x84ef482bcfce278b3632ddf1fc606bff27b72502      1
    48 0x8924ac1196e2ae89603925b6e001dd14e3f7fdff      1
    49 0x8b3a26ed875c61a1db68a4e52d046a3c7929ca66      1
    50 0x8bbff4607aa5ba5094f568dd444effb9a9315414      1
    51 0x8bdd94ae16ba2bcc172dd5b66666c58abd1dc568      1
    52 0x8e018350d31c897ce2f1070fd40855d31c849465      1
    53 0x8f3de026508faa671101791764906f07fc9143d2      1
    54 0x932e1a187aab69ae2f786f2877be32d73d6cf31a      1
    55 0x99986b5e808fd039b992fad30075f517979d490c      1
    56 0x9e064df29fb6bce6134b4b1b06600a6240138461      1
    57 0xa1114eaa6d983390c49e9e29235c75f516674643      1
    58 0xad1e365152589d3501340af5c004bfa1ef0d724a      1
    59 0xaed030182cb9d62e0cd6d8cce9cc0d7583d0543a      1
    60 0xb0a2ad259c2d875632f5028aca169760e7580db9      1
    61 0xb32955db11e05040f694b33f6482510b6b0e830a      1
    62 0xb3f288309ec2be759c1c8b85f7bf93bf0b8870e9      1
    63 0xb3f3658bf332ba6c9c0cc5bc1201caba7ada819b      1
    64 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
    65 0xb917ae9907d09ca8b11dbc57b9a9a82f0a232daa      1
    66 0xba9ae18995f5cfc98196850d40b58445c08cb485      1
    67 0xbec69dfce4c1fa8b7843fee1ca85788d84a86b06      1
    68 0xbf63a30d911ef4a3d73c9e3bf3d8012e93d6ded3      1
    69 0xc292d5bb3cdb73c32879a83b3e9590c4aae838b1      1
    70 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    71 0xc567ee384df678b217a28897e0d1e4b944624e12      1
    72 0xc57b2900f3ab9b9bd4ffcdb8174cd7665d0da8bc      1
    73 0xc8f1a199eeb0eccedfb0f401b828ee6fb894aaa7      1
    74 0xd56fa041f6cb82a8cbbcecfb53da41d4648d57e8      1
    75 0xe036a80f628e531982189fb6c4f277deca521e36      1
    76 0xe092f8840b20eb54ccd61ddab5647dc0a72dc2ca      1
    77 0xe35c3d08163da9bd4efa00879a78504d69820b5c      1
    78 0xe38df4fc94172294164d51dd5736ba8a8ce6116a      1
    79 0xea213ed8d4d10aacc533bb7ab93e88c81275bf57      1
    80 0xeb3648865b1471996e5f65ae845f9eb9226b9a02      1
    81 0xeb90141895a0ff1839e4846362ae780b9796644f      1
    82 0xef8a28b8a39fb292afc471e21106eeef1985ce4c      1
    83 0xf0c43ee266723f424d28bb9134463a6d37d5c4c3      1
    84 0xf36fc687c16f3021635b848a7d7450f5217240da      1
    85 0xf5ad607e8c16c41e3b15b2b32d1ed115d914b1ab      1
    86 0xf637e64875e767c00dc2267f7a5fa2ff33531911      1
    87 0xf88b90affd941829f2b55f4a5cf02d2c2b2c96b2      1
    88 0xfdabf3b545aae3d49eb79d185beee3ee49859219      1
    89 0xfe5573c66273313034f7ff6050c54b5402553716      1
    90 0xfebded7df0b739564dcb218b4e673f0918528b8d      1

## Allow Memes Phase 1

``` r
c(allow_memes150_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x166c9a16c8b9fda9d69950f2cd59a92b34b7954b      1
     2 0x1c2ea5a58d54914e06bee9932f5acd0a622930e8      1
     3 0x2141f82d131900acc49eb42730ec8935168444b8      1
     4 0x2e89f8b69e755b0e6b36fde2d06d98252e10036c      1
     5 0x352216a5b428fbee3dc882a2c268a1669fb8b2a5      1
     6 0x37dfaa62f0fe5e6ebe990ef1a0722f5279962e37      1
     7 0x384973ce79c352ad766f56672babec85e897f234      1
     8 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     9 0x3e65bc8e53618b68d983a701e58d89423a94170d      1
    10 0x406bbd4b112e77d7091e36c23b92cca918b5419a      1
    11 0x41a7519f4e3d8f0c93ac9e8cbb413763a9e213b5      1
    12 0x42f2d682d9cee89a98347768337938ed8d18c266      1
    13 0x4c4b8d30813a59ce6a3e590f745ab9815a206858      1
    14 0x4ea7cc46443cbfd0ae19aec35f08dad83477a52d      1
    15 0x4f4bd77781ceee4914b6bc16633d5437eabca5d3      1
    16 0x56d90629aa29830e361e113e53a979f95afe93c1      1
    17 0x5d054491bf455701d266fc2088c7afe3db267b97      1
    18 0x5d7a182082c433090596e2ed57d4f88cf6468d81      1
    19 0x634cabdbd8b78b69e4633fdbd4b0c7def83858ef      1
    20 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    21 0x6666083ba28027fd6db24a3c1bf288c0f3f95807      1
    22 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    23 0x75d4fcaffb56055bf9df3574d345473e31a16076      1
    24 0x7bf8bf27e3f300fece90eb785e4f09aba827edde      1
    25 0x7e0051a0b48f3c5848e66fd02bea3400f979a89a      1
    26 0x7e254fdb54ea865ea27d3bd201a5384b2abdecfd      1
    27 0x81bab6ea2b3b6f2cb249767f7ca253fc5cf0318f      1
    28 0x828fe9cfa3c1d7ec49b7bc70ffe8898970b25088      1
    29 0x82dcba7a8bf3aa462040038ecb3d5d90901676e8      1
    30 0x8e82a5b8f8f0b86ead15a4e56dfdcf2418ffe647      1
    31 0x952dd5887bd9a061768cec90e2ac8f3c1ed21480      1
    32 0xa10d31b628bf5f9fc8e02e91a7564bb2a1e5fea0      1
    33 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
    34 0xa3d7793b8771a072da0fab34f0cd8d526487b4da      1
    35 0xa7abf57fd7a4313080f018e2914912b1de3b9085      1
    36 0xb662ffa9f1f873c12d60e105117ac9c081e14d87      1
    37 0xbc9aa6262e2b34d5d124bd754d74a8a61f75580c      1
    38 0xc5e2a6d4029a93e5ede4ca7c93d1aa3f7c4cc3a0      1
    39 0xc9a1a9caaa608dd3a7bb90b5f16b4a0991095aef      1
    40 0xd33744da3013927fad387d24f57cfa241735ded9      1
    41 0xda5da40b45d9b6fbc8d99ca3eeb78477eef75888      1
    42 0xdb56979106450cbbb6427b3b90b96af18a1f4546      1
    43 0xe0e31366067277dde4a391cd5d77f43cdb9ffa6d      1
    44 0xe560646ef7a69400974d85e818bc0e054bde65c1      1
    45 0xea77389a4a030403521d365022f9c5dc14a5fe8a      1
    46 0xeaaf2b6c6671946b6774e3f353ad0959f7c20e88      1
    47 0xeff3554ceabeafa42d92f539bf71c713f5da5e06      1
    48 0xf480935955d38a332cf40c65add722d46b922462      1
    49 0xfae772e7498b0cdfc4ea929da72333dcc0dd2517      1
    50 0xfb1bb9d4a4c50457d339a686212a0716ea0adc80      1

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
