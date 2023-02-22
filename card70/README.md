
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16675069.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:47558       Length:47558       Min.   :  1.00   Length:47558      
     Class :character   Class :character   1st Qu.:  1.00   Class :character  
     Mode  :character   Mode  :character   Median :  1.00   Mode  :character  
                                           Mean   :  1.26                     
                                           3rd Qu.:  1.00                     
                                           Max.   :652.00                     
         name          
     Length:47558      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16675469 # https://etherscan.io/block/16675469
block_hash <- "0xcd584c6a8ae9ecbfdc98e5a4b9139f849c2da2da205f65819463121e094322f1"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4663 

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
  "0xc6400a5584db71e41b0e5dfbdc769b54b91256cd",
  "0x4b76837f8d8ad0a28590d06e53dcd44b6b7d4554",
  "0x0887773b5f43c58f0da7bd0402fc2d49482eb845",
  "0xcda72070e455bb31c7690a170224ce43623d0b6f",
  "0x41a322b28d0ff354040e2cbc676f0320d8c8850d",
  "0x000000000000000000000000000000000000dead"
)

hodlers_remove <- c(
  ""
)

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=4,address_max=1)
airdrop_neurocolor    <- pick(snapshot, contracts=c("SuperRare","KnownOrigin1","KnownOrigin2","AsyncArt","Foundation","MakersPlace","MakersPlace2","abstractpaintings","Magnum","F4T4L3RR0R","RadiantGlowingBits","sweetvoid","Deathexe"), address_remove=address_remove, address_pick=42, address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_neurocolor1of1  <- pick(snapshot, contracts=c("SuperRare","KnownOrigin1","KnownOrigin2","AsyncArt","Foundation","MakersPlace","MakersPlace2","abstractpaintings","Magnum","F4T4L3RR0R","RadiantGlowingBits","sweetvoid","Deathexe"), address_remove=address_remove, address_subtract=airdrop_neurocolor,address_max=1)

allow_neurocolor_edition_phase1 <- pick(snapshot, contracts=c("neurocolorOpenEditions","KnownOriginEditions","ShinkeiShokuEditions","NiftyGateway"), address_remove=address_remove,address_subtract=allow_neurocolor1of1,address_pick=200,address_max=1)


allow_neurocolor_edition_phase2 <- pick(snapshot, contracts=c("neurocolorOpenEditions","KnownOriginEditions","ShinkeiShokuEditions","NiftyGateway"), address_remove=address_remove,address_subtract=allow_neurocolor_edition_phase1,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x1566ae673ae80725bcce901b486c336e6acef465      1
    2 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    3 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    4 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1

## Airdrop Artist

``` r
c(airdrop_neurocolor) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0566295faf52797d4a29e5b78bf28e287e4ca2da      1
     2 0x07ff22c4a01e788b83f42ce0d9b9dd79df6f1409      1
     3 0x08942872046fa44bc6456e491e8de11de8bae73e      1
     4 0x0b99547e49d5daff6ccb57c791b9310c4b4f842e      1
     5 0x0cb2d792aabb3fe0fb309df27233d19de5aa4916      1
     6 0x0fb586f584dca4f2ea2d8bfb19ab72bae35d6900      1
     7 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     8 0x234bab2648ca50fe14b8495bf77a923585105b57      1
     9 0x2755c1c680ef0a3ddc916e930d41108f3c9eb14e      1
    10 0x279c14599b8c2abd6d59686846958dec848a43cd      1
    11 0x33f99cd0e3c56a6852d4627455296f30f2851772      1
    12 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
    13 0x3bce001be02921f23eef4a512e6a36015ad63562      1
    14 0x4a7a76e2c061ac51d7794b2a9003338ab1f13d68      1
    15 0x4cfd427ac7217ab1768f410efc33a37132b8f3c9      1
    16 0x55b1eeeb65316129993e84ed2ced1472351ba1de      1
    17 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
    18 0x70b26a35f8f308ef8286798c33b4f7a1811c7630      1
    19 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    20 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    21 0x80b3153f39aeec1ef68adc038913698e103e6e1d      1
    22 0x8721cf071c5e68ad361fc7f3f988483f80818505      1
    23 0x87b1702a5a31f6b63820c99f2234a3323ac728c4      1
    24 0x89119dac068cfc98bf2ffb7d15948e0901d997dc      1
    25 0x8fe8cd04ad2cffe0d09cc45386659ccf4dcfee48      1
    26 0x93a781d6e6a8e8916ea766a40c46ed2064b55141      1
    27 0x98d633d2df5e70b6b93936f225fcc16106383aa2      1
    28 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
    29 0x9ad247cfead80a440c42555a0b412c44727f012e      1
    30 0xa208a0e5a1a74e6b9c3f053be2d1ea884067b925      1
    31 0xa22414e4af5767448624017c7e0151d22490412b      1
    32 0xb271b694e686215f37e15fafe12b1e2e015739b7      1
    33 0xb53349160e38739b37e4bbfcf950ed26e26fcb41      1
    34 0xbb2aa379177a52f93089af262b98e9f298cde638      1
    35 0xc3556a0ef4ccfb13ae57489f1e685cd1dd16b372      1
    36 0xc842ce7214a14fa98186a010bcb43c7e99e4caf3      1
    37 0xcaedd5e74bb3ee95ce8f564c9dda431c146c5836      1
    38 0xcb6a1ff7aabbe2afceb4f7c5a6ec7ded94506c50      1
    39 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    40 0xefea7c883b74ef2865d953d8fa46d6e654b8ffdf      1
    41 0xf83a6fc4394ce4127b5e5aed9d5100e151e4e4d0      1
    42 0xfa1be1708f0f6c3b23e0e3339d30aa6a6ddc5e0e      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    14 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    15 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    16 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    17 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    18 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    24 0x59068075a799594db03c0255eed68e8e121155c8      1
    25 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    26 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    27 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    28 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    29 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    30 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    31 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    32 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    33 0x69e68074f1aada957edd39c5eae0069973343f30      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    36 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    37 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    38 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    39 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    40 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    44 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    45 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    46 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    53 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    54 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    55 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    56 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    57 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    58 0xbf814810b44978de273191fd612aa47f7b69d564      1
    59 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    60 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    61 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    62 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    63 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    64 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    65 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    66 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    67 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    68 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    69 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    70 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    71 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    72 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    73 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    74 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    75 0xfd22004806a6846ea67ad883356be810f0428793      1

## Allow 6529

``` r
c(allow_raw, allow_singles) %>%
tally() %T>%
readr::write_csv(file="allow_6529.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 4 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x13eac939d3c7ff1a985d164d1f14411505b4c822      1
    2 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    3 0x9274f2f89fbe5dc00c21c628e46a18c7187c14d7      1
    4 0x9dc5b2cee788e3e9c67489b5d7ce634dbf48a32e      1

## Allow Artist 1 of 1s

``` r
c(allow_neurocolor1of1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1of1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 107 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x061600d3515b387d562504eed5a1a485f9ae0ee4      1
      2 0x076e7ec4724fe43262accf765dfc8002fb83dd2b      1
      3 0x09382e9bbdf4a47d48d83d56857704cf6fab1f82      1
      4 0x0c5a2c72c009252f0e7312f5a1ab87de02be6fbe      1
      5 0x0e7986203c2fb670d701353a13dba5bb186d5f80      1
      6 0x0ecd8a50ce988862dc0e69a669159036e88ea649      1
      7 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
      8 0x106cbcfd8bc05b056ef0c6ab6a60262e29637fca      1
      9 0x107c9a79c6b9b199ffaa620a47800fe2291c0d92      1
     10 0x10882b1196d9d3ef28e77c3219af99b9dd8068ef      1
     11 0x1689b564814db5f1c7abd74129771d282e2d652b      1
     12 0x184d23f3e045f53502a0513119f3254f40a0af0d      1
     13 0x1c8393d35e8ea4228df398d3667cf914b60ee5c7      1
     14 0x1cb617af52dca15fc73887fdf05f6b79baa23f9c      1
     15 0x1dde27109f4ce2623c90aec001ccc47745d4a0b4      1
     16 0x2089035369b33403ddcaba6258c34e0b3ffbbbd9      1
     17 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     18 0x2ac81c02fbfa8fc432d572f2894ea61554d11dd0      1
     19 0x30f0f915a4752445cfeadfac0beb539fa1192b38      1
     20 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
     21 0x32613943243860edd1a054283ca8a48c2a82ec41      1
     22 0x3659f47efc320cf7b5f7a4ab216447c0dad30e90      1
     23 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     24 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     25 0x452d5973628d7eab041e8db2417af5c550a8c2f4      1
     26 0x46da519aa182dce7fb0486bb12272cd971e3a2c2      1
     27 0x49e3cf47606a5da7b11b270a790e2112a467485f      1
     28 0x4f9997213124846ed58215c13835d4a22e10e14a      1
     29 0x54706845bc5794a2b859dd864e300ccc20da8394      1
     30 0x573555dc420c87082143e0b9af96b3413c7514a0      1
     31 0x5ab5844dd55ab73212d1527e4cf72fea884e39dd      1
     32 0x5b5fc02d41eaafa7ecde3c02c3e5c59110a77d99      1
     33 0x5ceeb5bd2b847c6e0f65af734ce6aba1c269f0db      1
     34 0x6018e57d73bd0bfbe773ef5b0bc099c37b16c988      1
     35 0x610ff408a190ad7564883223b8b6e509ceae5c7a      1
     36 0x618d17fa59c67aea20a88dc75d4e28e728a6ff28      1
     37 0x6530bbae1272ed6c16f54e562b20385a910d4a24      1
     38 0x6756db92db2e138ee09c1264bfacc7ef645a7bb0      1
     39 0x6d5f1fc2799d285f26951ad0d2bffd6a85cb2b6f      1
     40 0x6e991fa1107712c90929366d8885d0c1ad11f5b2      1
     41 0x73d05c2ea70dfc3b220444c94567dbc84bb0d24c      1
     42 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
     43 0x762da606029d3120735aa1eec15464e265db7a3c      1
     44 0x76afd4849dbb63c04f3c2629d7a216d7d3e0e04b      1
     45 0x773d715200ab5c07f39db9772e3c83c48534a585      1
     46 0x78089705ed807984cf3edf9149c9369e8fabb9be      1
     47 0x7853010ab93f48ce78137703f7f0d21ace194d11      1
     48 0x78cdb41120f53f6361ca94a807283a929350beef      1
     49 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
     50 0x812b4e6e9f2899c4805ac1a1ecd8f72e2e96baab      1
     51 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
     52 0x86fff6af1614bc18c72840ff8e6c033dd43bed8a      1
     53 0x87085d8d22d688e7ac9da3384127169f83e059e6      1
     54 0x87b2dc356091c794490cbbf661384c7e6343ea61      1
     55 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
     56 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
     57 0x8c778a46752a46d09b50aeab6425dcc39360e248      1
     58 0x8db709930b23c01885f49661b14853b82347fb49      1
     59 0x8dc287825d71f758bab052608ba8a4f156f84176      1
     60 0x8ebe8b747cab6d473483aa3889870cf31c05d1c6      1
     61 0x92158120bad37251c6c845c1a9e399bece66cf44      1
     62 0x9219bfb4c17ac5d29366fc2718ec416650f74beb      1
     63 0x971995d5961a3fda21d5cce39a9c6615e138cb02      1
     64 0x9788e8a2de43664da334c3cf8f69041fe5858470      1
     65 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
     66 0x9c6a76bde90c6250ac8196683f4b637ac9b357bb      1
     67 0x9e00b0b219d5c2612a61fc66b9d864b375f6d90d      1
     68 0x9ee509b47aefba69e968f166ae9f37a55e6b2d82      1
     69 0x9f8b1dff7f2b77d3532786deb1213bbe31c0676f      1
     70 0x9fc57c82655dff05aa137da61289e728ecfd510a      1
     71 0xa22e31a92d1970e62a1605403f1e42510e2d05ab      1
     72 0xa337d966e8a1fb59846e620a35d54f071f6fb3b4      1
     73 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
     74 0xa7ca4f6e5cb2b1bf960f543a191fe744cf0cb6be      1
     75 0xabf566ff26f3679624441e24ca8db84bd46c2cb5      1
     76 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
     77 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
     78 0xb39fec80f69cfcbecfd766a551f7220ff1db875e      1
     79 0xb43461f2b60f63de07d80eb1a667a9b84802e8cc      1
     80 0xb62ea019c3ecf647c21b3d394f373c4089e3e4d6      1
     81 0xb64199cd690aa4e78f1a6625660347ce63031d72      1
     82 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     83 0xb9a3520eb5b9369e408ab33e581a4bd4950e4f35      1
     84 0xb9dcd2f68174e1ed42714b1c928a7b574b119afa      1
     85 0xbed38cd3752dea7902fd906bdf35ef10eb58183b      1
     86 0xc2ac36ae74784f290e7b912e5cbf35c346fe4d93      1
     87 0xc2dd18c444c29d57e8b7b62ba87daea2042a1ee4      1
     88 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
     89 0xcf7b0637b0ceb93cf36298ab0dfb48b47edd74e2      1
     90 0xd4d350e0e7730660e6cdf532d613cdfe775d6513      1
     91 0xd9ff1fbd68e910392dc404f8df0fcd23a64921c3      1
     92 0xe1d29d0a39962a9a8d2a297ebe82e166f8b8ec18      1
     93 0xe47231e1c690df1b3de04492bd06011e6f75a599      1
     94 0xe5cadeb54eb3616f8d3d6e41ee9ff2087f07394c      1
     95 0xe5dfedcaadf4df48f2fcc56663b89ed0ad73da3e      1
     96 0xe703f231ab056ecb99c92a1232cc1020acfc72f8      1
     97 0xe7079eec020ddfc3f1c0abe1d946c55e6ed30eb3      1
     98 0xe96f7dfc0cea7e229a18176e85f2ac0d16e96594      1
     99 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    100 0xeb816ddefc8413bf1aa74f1cd07a9db5291167cb      1
    101 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    102 0xf4a12bc4596e1c3e19d512f76325b52d72d375cf      1
    103 0xf6330de94b60a063f547f7b9dfaa5a624cbda702      1
    104 0xf97664376416e9379f2354db444bfe3f00b6936b      1
    105 0xfaafdc07907ff5120a76b34b731b278c38d6043c      1
    106 0xfc4672341c78f1ebc0c23fdbcbb03b61c6f2d50f      1
    107 0xfe5573c66273313034f7ff6050c54b5402553716      1

## Allow Artist Editions Phase 1

``` r
c(allow_neurocolor_edition_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 200 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x01d6f0b38e2814dd8ef0163bfc05492eb7ea9b23      1
      2 0x027cae2ed1a23350a751452e907b4120330f9762      1
      3 0x02ac3a098a3f8197a6d4a5e6fcd4114df6b3e490      1
      4 0x04173063c0c491470b735954790f94ed307aae9d      1
      5 0x056fc9f87ae27ba058288a4e8264effc7082ba1a      1
      6 0x06adc801778965ab3d829418b4110df7936957ac      1
      7 0x09564716e12eb2ca51f64d07d76aa88b7cdc0283      1
      8 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
      9 0x0a85920198806af7a0d0804d5770714441179004      1
     10 0x0b0ef7d9d87fdb2deba35fccbbf6fc444ff1c449      1
     11 0x0b67c46339201da8fb5d267dbb8987e20cc7d4ab      1
     12 0x1228a857fd7ee845f4999f33540f6b9d0988e80d      1
     13 0x1412e5521143f53031a174fe80cbc5564e76e27d      1
     14 0x1732fdc4b72083815f181aae616944c8edd82e11      1
     15 0x188c30e9a6527f5f0c3f7fe59b72ac7253c62f28      1
     16 0x1ce3f5a97c846682ce2b1967efb241274ec976a9      1
     17 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     18 0x22d983adfd23c03926848030fceae04acbee0d52      1
     19 0x234bab2648ca50fe14b8495bf77a923585105b57      1
     20 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
     21 0x24908dfd0cc30008370594b85b8c43b2611c0302      1
     22 0x249b61fd7a7502c1887e4dcb5b04f585a5f90673      1
     23 0x25a8d18190aa1c155bd35449dc0d1e58f5f3edf1      1
     24 0x264928da5e53a86ae08f3e74765399eb6a707567      1
     25 0x26613696bd07c7a6e43c94ea457584f1a5d9f979      1
     26 0x26740e7977dd4de54a674af28ce555563d694be0      1
     27 0x26caaa5943437bb9dc896b358a5614709e4db062      1
     28 0x27c94e50d0936a4b17d1541b9520a21bfb2ebfcd      1
     29 0x27ec37f465e1755036cc854aa96dd304f82213e6      1
     30 0x28057cc3e351da6a928953a3ccdfad3ec138f27c      1
     31 0x2ab03b6e2b261f542569b8f016eb48d302c1e4aa      1
     32 0x2d1f2ee209111b8f449786aead39f225d6147952      1
     33 0x2d6836b5d89e319f38ac3c058f4c835f0c69b398      1
     34 0x301cc218009608e59f9647f4b7dd8a49be8493af      1
     35 0x31f2792889c0bb79f7c9e8ded4f8c53c6c8f4765      1
     36 0x32cb6603e32c6a887786cde499e804de8c01c7b5      1
     37 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
     38 0x3355a7ba8344331f40e2e6576868035023c89076      1
     39 0x3376d8278a123e02aee2846ac91dcfd987cde86f      1
     40 0x33d7a3ad5b4168f30c051ee66afd3c2a865ed919      1
     41 0x33e913e15c2c6a0f35126ddaf6bb34c99850f79c      1
     42 0x3433c2753da24df566ea14c40584179e97396cf3      1
     43 0x3595a1508cb1180e8e7f50008db1109f5293efc5      1
     44 0x3863fd96fa4f422f8366835c09b5bf790473231c      1
     45 0x3b1b3edb62b527a134e5a7541f8231a3840486e1      1
     46 0x3b23503185058c72a3f778f0a3fe2bd8044738b0      1
     47 0x3c1de5094f68bc23da7a016c3ec52182ca8a2dda      1
     48 0x3d0889c3cd2f9f9b53b7132b5e9a0c90d371b9ba      1
     49 0x3d2f9441fde0db524a77bf9fdda610538fcccd16      1
     50 0x3d5f1ad033c4e7c6a6d433d50d694c488dcaebcf      1
     51 0x3d9801215e80142934b94ae285867dc5986ffdbd      1
     52 0x3dfd4bcd7d5da4a08ed1db62b15ac9dda3b39e64      1
     53 0x3f8a4401dfc3e07bad8e2fd13f7357096c909148      1
     54 0x40596edc2e449ef48cdefc5a787f653f768369bd      1
     55 0x40c8cef5dac8e6d0e4e11d1856c155fc4ee31046      1
     56 0x41a808574e5bfc517a025cf29237755de3a1eacd      1
     57 0x439e99d2c1f031f2a79f3883a4360ba7d13c3509      1
     58 0x4423225b1ec18f4156355425fcf29b1f59caffeb      1
     59 0x45288feed8687238b947ba115b6119ae1c6cfbde      1
     60 0x46bd851210fa4318e693ce89ee7b6673c4acf2a5      1
     61 0x4744cda32be7b3e75b9334001da9ed21789d4c0d      1
     62 0x488889b0fb47c06a7c1e43e8cf0daef920a6ccf6      1
     63 0x50e6ddf908915997249085a6a19e3ca281329fda      1
     64 0x52029325ab640c933e86fea95002c1b9a774a6b5      1
     65 0x5290b98f59d82335b472139e296f546fbd3b8716      1
     66 0x551b807e76f4bd7be9b288e43e64f038d1d6d3b3      1
     67 0x557c60995797fa7b47be105227a2e46148d85750      1
     68 0x57f1890853175059bec4ae1e7cb696f03765ca3c      1
     69 0x592e480e0066a51eb981b532e275d6576e5730fd      1
     70 0x5a17fb43794212f5fac2298747c8757b8dd94a17      1
     71 0x5a6abe2b4bd1e178d984afa8630cd6ac212b070b      1
     72 0x5b049c3bef543a181a720dcc6febc9afdab5d377      1
     73 0x5b15b440f1e013417d3251fb23d8d7e4c8ecca1b      1
     74 0x5d18dffde59e681b76a9a712e14b328b4e8cf513      1
     75 0x5e5fb3cc1942d204af34242798c5024b850de099      1
     76 0x5e61a6c4d2e7bc93fd15fcf3fa18cc305185a4c8      1
     77 0x5f81e417ad6c0c4e8c93c67fc30e5a1f4d47267b      1
     78 0x61d7aabfd210e7a5d5d60083ae0a221fa37eeba6      1
     79 0x61d85490f053297bf23f90c6359de2d9a65a5796      1
     80 0x63748140c409b490952c37dae5a60715bf915129      1
     81 0x6527cd5ca5afd243aad787ec2a3cb68709570a70      1
     82 0x6853a596d6d7264d3622546da3b891b6fe17eb82      1
     83 0x6a2cb68ff38a86652ef60562de607761dd1b6bd6      1
     84 0x6a4bf9949dc169e79762cc116e309d08a6ea4a5e      1
     85 0x6c6c25bf7419d89ad7649f4632cccfe02b09127e      1
     86 0x6fddcd2a10999993ad4aaeca40a8a72cc36985e2      1
     87 0x703a32ca5a82512e8c95060456b5995d0c5141a2      1
     88 0x71784687d4c74338bf284bea22956c74fbe6d631      1
     89 0x71786f56576503afe7286c9cfa32dbde584e8c34      1
     90 0x72eac0495f5aa6f7471b8193a57747eaec8dfec1      1
     91 0x770d7eda6e52cc64737b553abc1f6fbf6dc07699      1
     92 0x78b23ba27451f96f835f9a3f2ee716bcafa02479      1
     93 0x7a3fae39975cfe63f6cc0f728835a576a7512535      1
     94 0x7ab5bd27e41f319d61aa7b643a6eaab9df7f11b5      1
     95 0x7b92849d753be78c3c0c013a6185e2f575c1714d      1
     96 0x7ba4c7b3fe940f28ce7fa01d126685eece4992fa      1
     97 0x7c9663d4563106fc14623c0b7ea5480f7acd0f2d      1
     98 0x7cabb73f5b840b245ec2528751445da1f6dd7eee      1
     99 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    100 0x7d1d114a82a5b1452d81d597f28e6007f5593ad3      1
    101 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    102 0x7ee8f8f465896f56edbdb5209015b64249c96ddc      1
    103 0x7f15e65c0d50492e843aeeed918669b36167dd6b      1
    104 0x8180257115d3440340b2db02cdd32c5c30952b84      1
    105 0x81dcab1513d88478a3536348d6d7560f2d8762a4      1
    106 0x83b6dffc7bbe17c58f0ccc49126a6f435832e31d      1
    107 0x84b623ed234c5263ebb1e59d51fac3f4cb0897b5      1
    108 0x85132d932c4fabcc311aafe568212b1927715331      1
    109 0x8572e09642c8c4941a774ee5a3534b9e3379851c      1
    110 0x85bc03678220009e82206ddccbf713228750f759      1
    111 0x86031609ec59a93e7faf75b06937c12671bea580      1
    112 0x86a41524cb61edd8b115a72ad9735f8068996688      1
    113 0x8868e984009e484a37ee63ddf1bea2531a099aaf      1
    114 0x8cb1e98e9787e6da4ee801a0b66ceac180071856      1
    115 0x8f54249ae4c8a73e92d44459e026c9197670f3fd      1
    116 0x91364516d3cad16e1666261dbdbb39c881dbe9ee      1
    117 0x917176e92cac77a9f503897bae9ca43d8a2ec0fb      1
    118 0x9192683edea0264fda574ddf101a362207169e2f      1
    119 0x91b26fffffb325e13f1ef592b0933696098044af      1
    120 0x9260ae742f44b7a2e9472f5c299aa0432b3502fa      1
    121 0x93b00caac54906ee4ac7c62d4571eda5bc1ed245      1
    122 0x9499054d02a725316d61fa896c29d58550ee4a5b      1
    123 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
    124 0x9679a83eed712b859faf2bfcfda36658e3c5ff6b      1
    125 0x970b52bf8964934e721f655325cc946e4901be6b      1
    126 0x99b97fb739dde3c7ef2a58b285ba65a4874a3bbb      1
    127 0x9bbd708653f4f105d5eadd19607b7f360fa787af      1
    128 0x9bfe4320207a44563bde522a7e48da2acb83a857      1
    129 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    130 0x9c7f37a2e0496236374085b9d7aec2c0206b5500      1
    131 0x9cee1b1d787bc70d8e4c00d14e25088ef66deb97      1
    132 0x9fca5cb296333aa780d3367951f766ab3d7d4098      1
    133 0xa16998a5c0c65d11fd2d9fbef57e1b2f041c5e9b      1
    134 0xa22414e4af5767448624017c7e0151d22490412b      1
    135 0xa3c277b8f35881cbdb017e52bcc376b3ce8f21da      1
    136 0xa505b105060eafbf4521e337256ba0433b6fc6bd      1
    137 0xa8674b0f20d44fe47a3f733036038977cc31f8fd      1
    138 0xa881e7264cfa774545ca0d805ec83e2725a94b22      1
    139 0xa8d2027a756d60f596b63bc802563c4c861f94c4      1
    140 0xaf2e919b59734b8d99f6bc1f62dc63d6519d14bc      1
    141 0xaf54b99c8980753a106442125c00bd1137c8a57a      1
    142 0xb0703d86ffb8586751fd46a82b9050b48bbed23e      1
    143 0xb0b134d249748c89c3ac52a0f4bc2aba4ead8498      1
    144 0xb1b51c2686456194e0432be407e06714efdecc79      1
    145 0xb39a6b4cb933aab3c30da765acf26c4c00a4bf11      1
    146 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    147 0xbbf902716dc9f3a55218278f5016a0f82ad2bcff      1
    148 0xbc6e31e4fea1c00e5271952d374d35b718cc75be      1
    149 0xbdf3c95908135c9e54928bfa440c2c808a244fa9      1
    150 0xbe0034c0155458202ccb53d02ed15964530f7c58      1
    151 0xbf2a403e23c4877dd966102eb221378eae3a9fef      1
    152 0xbff1205bfcbaa467cf39b213e1fa5ae5c256e571      1
    153 0xc43526c3f4df6e90a17070e7f089724222e5995d      1
    154 0xc461beb0db4839937cabeeaa8772e3dd4a67fab1      1
    155 0xc471d0f5ca629b3387231f1586b6b87e1dbb49ad      1
    156 0xc9d20b57f1ef4cdcb7d3ff4dadba4a1a584cdee9      1
    157 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    158 0xcc72d9e9525b55297c03c2d09b9677617a02d156      1
    159 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    160 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    161 0xcf0e739956f2aea51fa09c25402ee3eb80369dd1      1
    162 0xcfcf666004b9b2cce18ee582068d41edc59caf43      1
    163 0xd385605623db1be4974838829703b8e29124bf37      1
    164 0xd4b6595ff5f3c21e0b00edb8947a31110a9c4b8f      1
    165 0xd6527cab3fe73e96f23b7f6bb041af0ae124d917      1
    166 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    167 0xd889eba3db46702d2e4cfe6d94229db2ba188494      1
    168 0xd91babefdb88393cdfdde2b45df67c985bf3c82c      1
    169 0xda01b6fe8e18c93a83ad42a75334a2debd8167af      1
    170 0xdae6ca75bb2afd213e5887513d8b1789122eaaea      1
    171 0xdb882c5bf8bd9b32532ed6cee18e9056f3b4a574      1
    172 0xde54227dc7cb1de999979f21548096d92b64827f      1
    173 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    174 0xdf75a95c4075b8171971f3514c34cdc4f12b4403      1
    175 0xe1b0b385c5e855f131d300c9b8f33b2e96691d3d      1
    176 0xe1d3c8d69e5177073856f6b553bef5e64b0d8402      1
    177 0xe3fcb080f46ad0581b8ce75ba1873d61a26f6c1f      1
    178 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    179 0xe5d7176f23afa7ae597d6eabdee6da7e87bff073      1
    180 0xe747820f098e11f1de8f7a3a8fc671704c19a5a7      1
    181 0xe8436a2337751f769794c616697c032cdb213b85      1
    182 0xe893b92fe428916931cea98fd9fa734e4688dd0a      1
    183 0xe8c9333d6ba367ebd7ed1afd83088b5b5ea15d84      1
    184 0xea1c7edf57c7302679807bd7de1d21501bd61c03      1
    185 0xea310c966d3ff5e09c65487f1763b21361eb71ef      1
    186 0xea512c5e43b234cbfa01b1b816af17ae721bfaa2      1
    187 0xeaf7814cdf7236bc5992d19cc455f9c92b00aa9e      1
    188 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    189 0xeb83e695adcac2e83f290d2d2815fc58e6491d7a      1
    190 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    191 0xef59c086549e7a965a9a6a1ece1c6b5526020ef3      1
    192 0xf2fc60b88137a9f22bbb5ed0b9310401ade39479      1
    193 0xf46bea4c3ba0455bf9537282238eaba193851e30      1
    194 0xf54611a627afa7c00a58569932554372dd3f4b3b      1
    195 0xf7d385bc2d5173f2f013d694e8d9e8c3440638d9      1
    196 0xf813e0de2293b487f75caca607290f3161944f3c      1
    197 0xf930a731f1d22fcbea9d18f82a2154547d85a83b      1
    198 0xfade41403a1eee72d805c42f1eaf5f53a54d0e0d      1
    199 0xfcbe1f6ec1c26c8f48bd835d650a3383ea1797c2      1
    200 0xfdc87a87306092cdc2f47880119068cad83a0f87      1

## Allow Artist Editions Phase 2

``` r
c(allow_neurocolor_edition_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_editions_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 367 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0084b157fea3f2637a7c3ee211cd2cdef882f5b2      1
      2 0x00a28244ccba541b353f33726d790e4238baece5      1
      3 0x01df3717ea028db51c6080db588cd04940c493f6      1
      4 0x02b0390f0d2c3302984ecbe83b7c9425b8fcb83f      1
      5 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      6 0x03018c0dad483a344841675321d9676054dc9c9c      1
      7 0x03f5d4e8ef9494519d4977965dd24e5fcffb000a      1
      8 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      9 0x04547314afabd1060fe6513518584b1ca602b51c      1
     10 0x052d5c3a06ad80940447ca2692220ceff5df9952      1
     11 0x0578aa0595e0c83530dcde1d7a339479bfe6b0b7      1
     12 0x07ff22c4a01e788b83f42ce0d9b9dd79df6f1409      1
     13 0x090c984cc693b991d7cbaeddb43cb75f13fe5f2c      1
     14 0x098da3eadd07ec543c6e63fa16cc6427e693d6a6      1
     15 0x0a9bc3b0d19bca983cd9a6d251dc5920e87b223e      1
     16 0x0aabbdeb30fa5a53ae7f061b38d6b4168147d99d      1
     17 0x0b99547e49d5daff6ccb57c791b9310c4b4f842e      1
     18 0x0bcaa741ab13298e717286b27911dcbacade5728      1
     19 0x0bfa25410217c62a45babc7e020c42547b11a1fa      1
     20 0x0c2a1ec9b02754ea2326d7edd2f01b01053792b8      1
     21 0x0d6e832e8188c308904de51ea62b43920fe1da46      1
     22 0x0d799223232b8eaaaaa5671ecd4280af85360cde      1
     23 0x0db5e7924d87d2ec27876882e7af911a58050dd1      1
     24 0x0e552d0b3562806d1546b2f6b25cd973ec65e4b9      1
     25 0x106be3b6818c7c774df5ce0a801ff9e05d65b2e8      1
     26 0x10882b1196d9d3ef28e77c3219af99b9dd8068ef      1
     27 0x1222c55609df8b1697e235fd33edec065ffb4714      1
     28 0x14441ac732c0d8cf15f10043d355da11c831d828      1
     29 0x175fc6de1db2ff5368726f7bbc9934b9f2333917      1
     30 0x178f6dc99291aefaf00ca7065d1450cff33191c7      1
     31 0x1796d2685c4929319d66b1db8a5f625b693f686f      1
     32 0x19ab7cdc807b34d135061df410936eccb876265e      1
     33 0x19bb567ac0116876fc74f2f5512eaeec07ccf968      1
     34 0x19d74f3ab452b027c0ff27612f611477f0a3b1ad      1
     35 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     36 0x1adef8b7253c43caaf50500913d422ae688367bd      1
     37 0x1ce9d62ac3dd1b897d582cda4075793ee8d0bfd3      1
     38 0x1d25029d92a7051f46a13be9a512443c966d3542      1
     39 0x1e05dcea408fa504707a17ddb4b28145e762b8e7      1
     40 0x202b01344af9b0349817c0df98ec8d78491d2478      1
     41 0x206c037ce4df3754873ed7015bf19236b0d04e8b      1
     42 0x213f6b2680dce3e4d0924acff3e4e34520ef9ba1      1
     43 0x22720ccde7db8141576f844beafcc9c7b7d602aa      1
     44 0x255eefd8307b3878be1e620fbd6a0ffa193b1cc5      1
     45 0x27316d50598b15823974ef1af7272c32d0fb7433      1
     46 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
     47 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     48 0x29f871db578bf56c97ecdbcd2c79a44a442ac17c      1
     49 0x2a00f63af45627ff351549106ea735bd973aa86e      1
     50 0x2a3f7e5170ea8ca967f85f091ef84591f639e031      1
     51 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     52 0x2ccf61071babdb229346f8c5ceaab0d343404596      1
     53 0x2d13271c668d31e0ddd4563727e871f82dceb41c      1
     54 0x2d9319de02b8a02d40ed7abb5d69c6bd7a7f9cb2      1
     55 0x2de158b5c5f9510d4fb20fcd2af43c7cd5deb27f      1
     56 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     57 0x2e2926572404114b8a4cba716d87205aa7fe7229      1
     58 0x2fcb45c294731ea7ea1f615ffc280cece30c16cc      1
     59 0x2fd499305b13c207adf59e4595b1072014ea23fb      1
     60 0x302a226721914743e370f230b62e7b97bab535e5      1
     61 0x31231036ed51376a70b3824d7da1a4e036def5a4      1
     62 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     63 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
     64 0x32c9c98abd1215f01b1e6056d6bfc84fd975508d      1
     65 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     66 0x364df0367019629a54c12635f236afb020aaab54      1
     67 0x376eb113e9210ee82019bb830816bba600294785      1
     68 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     69 0x39117cedc1769fd480ff696a61e8534893805865      1
     70 0x39a25b5281b1433f6bf1e1dad3f6f1c21fdc480d      1
     71 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
     72 0x3a71c05b9666e49b5dc15df245b6d16d8f66f710      1
     73 0x3a9f45ac308ccc0a1a48b0f9e2f8ce859a0039ea      1
     74 0x3b93f5680a6f81aa1935c83c02beb08927e9d49d      1
     75 0x3bc1b52d90fcd503031ca39f718474f3b42b2200      1
     76 0x3cc435694547cc536106fd54fd11d88d2a85b611      1
     77 0x3d15f04d93d412f7ff55a667809d3219faf8374b      1
     78 0x3d79e08f56ed3efdf5fdfdc82938524b502a4161      1
     79 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
     80 0x40df5ee2722b43628e7e4f4b59909627f0c018f8      1
     81 0x41bd9ede67476deff711c1ddbd702b741dcc23d8      1
     82 0x41e467415de60e1fd222648e2bd66d6665b6dc62      1
     83 0x4319063dd74bdf2abf65106f7da558c29fe34c27      1
     84 0x43f3d84aeece98167a6fd8d8361d38351cbe68d0      1
     85 0x445816ca9bb0146940b802d49ce146787c73b5ca      1
     86 0x44c2a80f8b924858db469a5b35128908e55c531f      1
     87 0x4571acd6f3b4cc5f422fa2464a087d8b6171dd47      1
     88 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     89 0x4614865abd4748096b9aa924c4f8d2ed636b2df7      1
     90 0x4639e37f75a4599fe22371d618c9336da47cad1a      1
     91 0x464278144ff19c4939f63418b79f3e6a239158c6      1
     92 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     93 0x47f5dcbb04e00951c228dbfa9934df1febd77680      1
     94 0x482293e4c21130f069169e5d7e9c99f239c5ee8e      1
     95 0x49e3cf47606a5da7b11b270a790e2112a467485f      1
     96 0x4a7a76e2c061ac51d7794b2a9003338ab1f13d68      1
     97 0x4a98517c29c4ae451b77429575d645b44852368f      1
     98 0x4b187db714710b066855b4f9b8f7b6614c51f0e7      1
     99 0x4d6aa3da789ea162a5978193bd584d3067227835      1
    100 0x4d91838268f6d6d4e590e8fd2a001cd91c32e7a4      1
    101 0x4dd2a681d52f783ca9e317bfb9c6b1f52acdc619      1
    102 0x4eb9b17b0038c1797025158014d9a7b26e52cd80      1
    103 0x509291eef289159b01bef7748f2e1c5d59a35666      1
    104 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    105 0x512c6c58f2256ea86777b349d84e88bfbc155bc7      1
    106 0x51e13ff041d86dcc4b8126ed58050b7c2ba2c5b0      1
    107 0x54dcfdc60dbbe9891f0a003b903304ca14e4b611      1
    108 0x5684dfb90d3839c8fe6b852d19c653bca145274d      1
    109 0x573555dc420c87082143e0b9af96b3413c7514a0      1
    110 0x57cfe91a289f37a91e465db4f16d1f752cf818d7      1
    111 0x58847353d941840f261366561dab73bcce277b6c      1
    112 0x5887df7f972926067e00dcf88141bc17067d3669      1
    113 0x59033057946073a01b2cde98df007d7e64ebcf7b      1
    114 0x5a3e28c2bf04989e6a7506a9ef845ae2dbc6d90a      1
    115 0x5aa71418ac6238c78ea9a032a768dbbbf86bfeca      1
    116 0x5b5fc02d41eaafa7ecde3c02c3e5c59110a77d99      1
    117 0x5c5bb26835287b1d7b0fdb5e0c7ea890c509f851      1
    118 0x5c8081fb5dc727d45e1f104dd2df5d52aec8e26f      1
    119 0x5dcd16d1a606fdc186c2f09385799eb7bea61fc5      1
    120 0x5dfb8e65cf9ce76fb96ca2a840860680d379d735      1
    121 0x5e3ace83a12c4e46c675dabe2935d379fb40b139      1
    122 0x5e777730a1b0218eca21ab87dfc2eb673ca88a61      1
    123 0x5ebd000e2075526273b180787696ae0b17f0877a      1
    124 0x5fe785b2f589c79c89dbbafa217bd7dedd8c918b      1
    125 0x60637813f341215718738caa006d0f1196c24421      1
    126 0x60e4bf9876f184d75638feda66123c0ad06fe112      1
    127 0x60e897098fa6615c2e828800fb1cf51e001cb1e0      1
    128 0x613ead0ea5af374af0ccfc117ef116a8e8d133fe      1
    129 0x618d17fa59c67aea20a88dc75d4e28e728a6ff28      1
    130 0x623a48b90ae3e093e85de6d7f82cc7dff67e6294      1
    131 0x6396e0da264d4bd157cf00cd0baf2b406a22d8dd      1
    132 0x63c9a867d704df159bbbb88eeee1609196b1995e      1
    133 0x642cf538eb490080578bbacd597dec6d859232bc      1
    134 0x66a1e1772b53895c8c4edcacf2ebbc10f8ea0f57      1
    135 0x6756db92db2e138ee09c1264bfacc7ef645a7bb0      1
    136 0x680d896c6c0b637a438fcfa3d8386861737ce6a9      1
    137 0x68858e8270ab4858eadbe94d0adf609693c59c82      1
    138 0x6971dbf0c6923c8ad9f6da9e0b1cbcda97c339b8      1
    139 0x699c60cb08f73bdfd86453a3dc057e807417b143      1
    140 0x6ac8ecebc610e2add9aab5afb16d9e27711e603d      1
    141 0x6b67185780b8522d67cd62489eee73b1a39eb17b      1
    142 0x6ccebb1e653d349778f908d466037df2f2a1419b      1
    143 0x6d1adf0b8da563aacb2a49c60eca3c873a041b8c      1
    144 0x6d5f1fc2799d285f26951ad0d2bffd6a85cb2b6f      1
    145 0x6ec2997cf35457daa9de4cdc989c697d921b5285      1
    146 0x71356112b4ebe1cd2d0caf59d437bb70574e22da      1
    147 0x71b262311d38a0beb4204d981787d098fbcfeac0      1
    148 0x722e9468d7f8c2710b6dbfdf70f01b045aca1851      1
    149 0x7265d372820dd9d6f0bbd9ee410373f97e13d34a      1
    150 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    151 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    152 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    153 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
    154 0x762da606029d3120735aa1eec15464e265db7a3c      1
    155 0x76881c1d4e06eb51303fc7a10b7cd47f171c5fca      1
    156 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    157 0x77f083db15472ed3a38cdae0b6e7ef229b52399d      1
    158 0x78cdb41120f53f6361ca94a807283a929350beef      1
    159 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    160 0x7aba6e8e3055f6c3217237bdea1936e32ff4251b      1
    161 0x7b1c6f103c0851173f1c343a7e34d2fd1d5bae7b      1
    162 0x7c4a24c5b1c0eba323ac796b841708ca4f79feb1      1
    163 0x7f2cf626a94c1a15cf1f208a66f08d8aa5660fcf      1
    164 0x7f484af42b87536adbeff4a12c36053b40b2a7a1      1
    165 0x7fdba87da6ed499f19fccd1b8072b4dcf6991ffe      1
    166 0x803028dd9d3b5fc1b12e48b4f3f4218cc6470146      1
    167 0x810e096dda9ae3ae2b55a9c45068f9fe8eeea6db      1
    168 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    169 0x828d97f928264bfd94208949a836402d91d61168      1
    170 0x82a0a012bad2c38d4afcf6c971b20728ec5d537d      1
    171 0x82abb5df200d2998db4e80639b145d052ca40062      1
    172 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
    173 0x84300dcc7ca9cf447e886fa17c11fa22557d1af0      1
    174 0x846978259adc90d5be03c7f2921af8b58857a1d6      1
    175 0x86fe9b4a3055994fb38175ef7bf3ecf88d0608d2      1
    176 0x86fff6af1614bc18c72840ff8e6c033dd43bed8a      1
    177 0x87085d8d22d688e7ac9da3384127169f83e059e6      1
    178 0x87916d0aa39f520662e09b960d8172624147ebd6      1
    179 0x87a5ee283e602718957fdc1a4a60f1cd290500ea      1
    180 0x89338593b2586a08b22483c94c27e10fa306a881      1
    181 0x89795eeb2117b5a65784ebc979c0767934a7dd93      1
    182 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    183 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    184 0x8c4148cd664e6c03c7f089d58f367779cc7a436c      1
    185 0x8d9197335361537fb0cf95d256a9a0aebe6030c7      1
    186 0x8dc287825d71f758bab052608ba8a4f156f84176      1
    187 0x8ed55d7ae8a86032bfb78eedfd318086f9bc43bc      1
    188 0x8f5d69d024ffd5211e0efc4ad7f7846bb436f56c      1
    189 0x8f903cfc0af3c2ec0d872c57538af5e071544a57      1
    190 0x90c3266eacf2ee7f10942b95118f80d139e72860      1
    191 0x91507969d129eb5fcc9a70192302fdb1e80604c6      1
    192 0x92158120bad37251c6c845c1a9e399bece66cf44      1
    193 0x930f7ad052bf3ed8edbe52cd7df73e399e0fccd8      1
    194 0x9392a85b901522e7f58ce9ebe82bbd8bb352f328      1
    195 0x93ba3db76f3da0da35bb431e91afee7bae402428      1
    196 0x93e6832b6d190d1d7373408833231b1dc3bff79f      1
    197 0x9424116b9d61d04b678c5e5eddf8499f88ed9ade      1
    198 0x9441295f5a5f77c090ae106f6724510f07fc4bca      1
    199 0x94db5f225a1f6968cd33c84580c0adae52a04edf      1
    200 0x953bb3aa4671b859298d98f70890b510176add63      1
    201 0x9570fbd500d3591a19c8e3b07e5656249fbde200      1
    202 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
    203 0x974771475d4f8ffe6a09a223818b19073f73a8ff      1
    204 0x9788e8a2de43664da334c3cf8f69041fe5858470      1
    205 0x982b1d44ac3ff17e4e0b6e2091fe0ca62c9442a0      1
    206 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
    207 0x9a521c898cf8f9924c407ab8817d2330ed71c908      1
    208 0x9af481276b075e036bc23e887a8bd275e69ef74c      1
    209 0x9affee43567355874140bf139183addf3278c0de      1
    210 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    211 0x9d713029e28895c11cada12a3c72848e76a4e04d      1
    212 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    213 0x9fe686d6facbc5ae4433308b26e7c810ac43f3d4      1
    214 0xa0305d4959737131dd2d050b44e2737307925bbf      1
    215 0xa0df267afe0438460754496ce0f32ca006cc9360      1
    216 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    217 0xa14fe103bc14c6a88f2784f2e018fd71a25e46b7      1
    218 0xa21fed132341f41c7f96cd9a5cdb3965bc0b74b0      1
    219 0xa337d966e8a1fb59846e620a35d54f071f6fb3b4      1
    220 0xa3deb012dca06b12a7e9ae06738fccb4d39a6324      1
    221 0xa3ff78160428f990bfa32b13682cb17609c157cc      1
    222 0xa431d9ae84cce1c1c6d28f8258b5b95bab930210      1
    223 0xa4e12ce3955fc4289dec95c5e2a696ba24a845cf      1
    224 0xa52c1aa50fe9f138982696da79f2287c7c62a836      1
    225 0xa54e9bf1087418723c310f6749f6d7a7b732afe8      1
    226 0xa5855f4f40a409ffde3a171eb616926aec5d7b9c      1
    227 0xa8e376248fb85dd9680fdbeecc3ec72e20c37cac      1
    228 0xa9ae78623f0262b94e1479999f9165b4ab970177      1
    229 0xaa7c59b989276575df1281db16d56db69186c590      1
    230 0xab55d3fe744a9b0074243e3be63d4636554ed5b1      1
    231 0xabb77d443c8f50c3f16cf8ae126941ba9153d9fd      1
    232 0xabf566ff26f3679624441e24ca8db84bd46c2cb5      1
    233 0xacd37800e67ce9a35bf6cef176be1303d73f8f29      1
    234 0xad31ad089f44bf44792decddf37c14650eb0d7b5      1
    235 0xad9f4655afbb676cf93b324c6d0852498a4f48f0      1
    236 0xadfa1f34834037c203bf8be4e948e56460d0945e      1
    237 0xaf63db4e76a7cf1920232a748b65d5c9c8689fbb      1
    238 0xaf9e18cd797548220317fb459095c6afac5c33a7      1
    239 0xb1c784a6aacfd3e170eb4ddb67f48d619034baca      1
    240 0xb39734396b95660707ef7f3d3a76e8566ee34377      1
    241 0xb4ed04164fb9a2b530dc3cde21c726dd7db94636      1
    242 0xb53349160e38739b37e4bbfcf950ed26e26fcb41      1
    243 0xb944d8c673142aa64548c8660e9b24c2948ccb89      1
    244 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    245 0xb99eae4b1fb07880896ab975534869bcddea35e0      1
    246 0xba0eaa19f49ca94bf28e27968f54d4abf8936b9f      1
    247 0xba189a4f264f5a434b5dcd49df5ab150dd0e54ae      1
    248 0xba3693094dd9d67ce95114579116eb4bf459f103      1
    249 0xbb0af8fb13473b81c2b6fe1503a0f70207231717      1
    250 0xbdc0c04d7324d8c56593f570b7f88ca72714b349      1
    251 0xbec69dfce4c1fa8b7843fee1ca85788d84a86b06      1
    252 0xc01f6c8fc8852840b80c189a71dc60e50c9223dc      1
    253 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    254 0xc11f42d7b2ff88bc32da5d06bc4b7467ac2b80aa      1
    255 0xc13a07216193443f2310d15ccd7f4029fc359491      1
    256 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    257 0xc1d4aa6de06e9d7f49d9ed18c633104b5b4bf275      1
    258 0xc277a9007f07ecfc959f8ece7a46626f65b9a3d9      1
    259 0xc291ed2d87d779c4678002c7d3f23e2ffb2d7a6d      1
    260 0xc2ac36ae74784f290e7b912e5cbf35c346fe4d93      1
    261 0xc2c2d3f485dd68348f52fb8b135f6b3f1130189f      1
    262 0xc34aa5ddd90a6af4d89ebe4d0026f439bf11368d      1
    263 0xc3556a0ef4ccfb13ae57489f1e685cd1dd16b372      1
    264 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    265 0xc4e8d1717c3921454792241ab7ba481f1c5304f2      1
    266 0xc5b46513ea71294ba740e115f88b697c26d25c8b      1
    267 0xc636a5c78860ca9dd12457ca7af2f5d6dcb279f4      1
    268 0xc842ce7214a14fa98186a010bcb43c7e99e4caf3      1
    269 0xc8749019278dac36e1636c2753d1514ea1cc9c74      1
    270 0xc888c19a101979e0c29274895a86f0c3bab6cf7b      1
    271 0xc9627355805f254a6b4f7f3105f6c3ea23f7c5fd      1
    272 0xc9ae113b8451d0afc8a021d796322d545a7e7dbe      1
    273 0xca3872b1f2203c4b488955f6d8fc28c35315b507      1
    274 0xca456d71f0cc61dc27bd28a8e7f7b15bce91fa05      1
    275 0xcaa95ef5bc0f724a76bd4ba66ddeb6d50da4d116      1
    276 0xcb9e6fe9f84f4cd14b7505729045f917f269a3fc      1
    277 0xcbcb8a49716228e93f31759500a5d378f3582954      1
    278 0xcc61aaafaac195706ccb5e59e48fcbde7afec199      1
    279 0xccf25b841cc6b9adf95ab9eca54867f75ac321aa      1
    280 0xcd76da10cacec513822cbdbaf9044682dc8d4fa4      1
    281 0xce1dd76ce2536286510dbae5d1209e932b2429de      1
    282 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    283 0xcf0c19baf291b578383ba758c7d3cff0b2fce371      1
    284 0xcf9741bbce8ba8ec2b0dc8f23399a0bcf5c019d5      1
    285 0xcff37c836bd08dcbe82eee842d6d420ca0349492      1
    286 0xd09be3296fba19f337a67487ccd6f0b7b3faffe1      1
    287 0xd237d3b488dcddbd85408d2e879988f97ac81280      1
    288 0xd2b7e133d4e7044f7ef36f697f4ddfe07411b717      1
    289 0xd323e4b040111b3feba91091290685cf0be3080a      1
    290 0xd3aefe3c531e3e2eb0689206e7d495843c943550      1
    291 0xd3fd5c2f754591a3c9148bc72cc4d28b9e88283e      1
    292 0xd4cf19f76addb489d079d0f60f41d6e91e7c79e1      1
    293 0xd4d350e0e7730660e6cdf532d613cdfe775d6513      1
    294 0xd530282d853169a23822762af0dcab045d6295d3      1
    295 0xd64d7b3bd6f744ab507a4cb081af05bbdf93cd4e      1
    296 0xd706f4bc6be1e63c5866ac4198857f092bfadb67      1
    297 0xd7ad468821806e16bf28a19435e4077880bb93d7      1
    298 0xd7dea37adbf6888ef7a9a4035b0edf36932cf0ed      1
    299 0xd8261516087b76e1027019da8756d85d8ace7b1e      1
    300 0xd8dd75fbc61476e3cf6c93935501b018ad480657      1
    301 0xd9a4c56ae084ec95d5b34fb184f34db0d97ce92b      1
    302 0xdb5008e73c7cefe15a147450352be3cbd258da97      1
    303 0xdbc30d47297b5360f5fb386e5791b6ca75ba54a9      1
    304 0xdc7bfc3058654da90692457f68ee3a34e2dd3908      1
    305 0xdcb73eb559c13a666bad999e7f219ff3ced1866f      1
    306 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    307 0xde41df3d6734e920a420646a0151ede1be81a6ba      1
    308 0xdefc217196c439e879f393a6327f611e5c489011      1
    309 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    310 0xdfa413375306e2169adcbbe8551f69739328e6dd      1
    311 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    312 0xe08af621f22ea4f2f5de0d051be48f3336497547      1
    313 0xe1678bdf494bf00464b6fee30591b8b4fc3e480a      1
    314 0xe1d890dfe988c56e9a62b83aaef8bc0f32063851      1
    315 0xe27feb6e6ca8b733079642fcd90bcba734c3ce8b      1
    316 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    317 0xe390fc4cf341c408d342821579fd5172a163077e      1
    318 0xe48bb840cf698a6d0b60c69b96752e0d9f84e28f      1
    319 0xe549f03f51fff04a6abbd9af122d41fb9e94675a      1
    320 0xe59d87e5298ac2fb24c2fc55d256cb8fcb696238      1
    321 0xe5b545c8f1c6a9ecbc53d78b3a2916fcf065bb43      1
    322 0xe5e2390581164076cf7a8a6162fea037b9a3d344      1
    323 0xe60b2ce34bc4c118f373197050167489083679d5      1
    324 0xe6a242a47e25699bac6139caac40feaab2b57449      1
    325 0xe6a5f0bb03c43196d43badd08899396c224b8e31      1
    326 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    327 0xe828abd66d651cae1c1ba353995241fc3e5a336c      1
    328 0xe872bfaae7c02a2f1e2c624544c68269458cf886      1
    329 0xe9143073c1d65cd8e9979dbe53aae5de5cc5679c      1
    330 0xe92689183961135f51bd5edc601a0aa123c069f3      1
    331 0xe9e1a4dc4b18f3ec20cae001c5480af731c64a34      1
    332 0xea4aa740a98e7a0b5ab8f3b292aa64e559116f62      1
    333 0xea9fb10809015cfc7712590c71cf1d8f8cc45890      1
    334 0xeaa561c4e6110589ff2af64c78182780e5725b27      1
    335 0xeb816ddefc8413bf1aa74f1cd07a9db5291167cb      1
    336 0xeb8813ea3b69c928fe836e765057d0e19b5a3adb      1
    337 0xecaebf8f5a28a6ef322bb5428c11270ed057c497      1
    338 0xecffed885667f154a28db2553657af50eac19f99      1
    339 0xed10fc70a816b779c44dad64b3a1d4830642bbbc      1
    340 0xed4f0630af068494e2d7fbad715a7a1892c9aed9      1
    341 0xee0688e09ce4c20526464411ad44adba31c2eda3      1
    342 0xee27b8e5760f979d67a6e98b2b403a09d4c04fd7      1
    343 0xee2a4c655ea792d92f12a3caea21188be6dea808      1
    344 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    345 0xef1de2fa338fb55e834bd6cb8bad382d410cb882      1
    346 0xf035839daaf652f03156b297a27eda25a35e6316      1
    347 0xf08889f3b4f5a56482fc295db2ffc7301f888ca0      1
    348 0xf0ac3990cbf147bb94801cc12bf062b0d252c4e8      1
    349 0xf28fc8d0eb98a34f2b03d384f7db6f1e7b877a33      1
    350 0xf2960b925e77e1093fe6ab1b23eeaeba849fac09      1
    351 0xf2c452547004ede3c28384917924032c4904b3d5      1
    352 0xf32c6d7e0ab3f5f52383608e5a7b28d41bc3f13d      1
    353 0xf3c8b978107690418d26f0f049dbe5d3eef7704c      1
    354 0xf4c96f563a1463737057dd9c9d7f0a9c5978ea58      1
    355 0xf566c12516d171b451dacb93c114753c07f980e0      1
    356 0xf57a9b1f574b1f80c8cebe252706bb8b4d783d21      1
    357 0xf668cc6cde8ae8670d90400c32efd78c8d6e243a      1
    358 0xf6784dbcd92b2ec36b496a303de36ed427ce6cdb      1
    359 0xf7c82e990dc53baeaba0a14c7a1c383333b811a4      1
    360 0xf83abc519e046c5391d219fabf1a3c87dd5924d3      1
    361 0xf91cfdc088368b23b5d03eeca53e5bd28cb8ec6d      1
    362 0xfa9774b092a5ca896e605ec3ed52dfda9ddfb991      1
    363 0xfb1369548e5b8768ca2c3ada0556606bd7d8b615      1
    364 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1
    365 0xffbfbd51e0a340d9b1fb8e56fbc02c981e25d363      1
    366 0xffd3f6ca1e4baba0993c88bbda9403c8a92d326b      1
    367 0xffdc43e03609c792bc62f4fd137ad80e4b9c294b      1

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
