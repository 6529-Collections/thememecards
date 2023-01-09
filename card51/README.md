
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16360269.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:31416       Length:31416       Min.   :1   Length:31416      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:31416      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16361269 # https://etherscan.io/block/16361269
block_hash <- "0x73999659a3c33745ab412a92852d0d37db1152afe6f612901ce889c40a77be4c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4468 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=9,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=44,address_max=1)
airdrop_xose      <- pick(snapshot, contracts=c("Sloika1","Sloika2","Foundation","AlmostNothing","Waterworld","XoseCasalCollection"), address_remove=address_remove,address_pick=33,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_xose        <- pick(snapshot, contracts=c("Sloika1","Sloika2","Foundation","AlmostNothing","Waterworld","XoseCasalCollection","46rotationsEditions","XoseCasalEditions"), address_remove=address_remove, address_subtract=airdrop_xose,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 9 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    2 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    3 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    4 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    5 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    6 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    7 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    8 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    9 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 44 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x112e372bb5ec17c1ea6b1d679ea72afbd2a94404      1
     2 0x12dbcaf089d921da60fe5dfaef912f11633b7dc7      1
     3 0x17f61a7fb9beb42cc00e767300b96441d25042ae      1
     4 0x18bc4293dd7f72d9dccac45e207093df3c23cd17      1
     5 0x205819b6f631c73c36f899b2f58ef996d713a1d3      1
     6 0x2206445d241ccb7dae93b2d2acfc67f75b90fd76      1
     7 0x26a1e46832bd5ffd7d21481d185fc258a7080316      1
     8 0x3cd76a7a90e0f6a7dfe6791a54ec459acaebc5ca      1
     9 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
    10 0x40b9c74ecabb64c797b284f7d4a4d945b745ec3e      1
    11 0x40d500be11e713a66f89b4cebc6d8d11e3a0f2a7      1
    12 0x45c3d26097f723658860a5c8cec8e06ee23a9014      1
    13 0x49063ec5e112aeaab92f50b5c4e81a5630dcf604      1
    14 0x4e2c1c305d33c79614082ac0bb8ca7c68d8779e2      1
    15 0x5054a665b7ac3c30939b02acb34827af25aba35d      1
    16 0x54d401e7ce5f3262795524b121a584e68b5a41c0      1
    17 0x625ceba7f2fa59a6b10cfaed5c54ebf3cd0501f6      1
    18 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    19 0x70da927bc27dff979cff22493e1ba26f3a61d941      1
    20 0x72be121ecebe178fd3dd0afb0bb91f75739a681b      1
    21 0x78eca41c3085bf0eca6470ab202f51510e5f86df      1
    22 0x8865de06e60964c74deacd501fe9092d2b2c1215      1
    23 0x8b4567fa8c4715c27a682215a031033a8b544206      1
    24 0x8da03feb7174b4b2a6b323f3dc40d97f3421cd07      1
    25 0x93fbd02200d8684ec0c0a48eaf07ba9546762889      1
    26 0x9aa91eeed8e34d7ed2145d651f76fae3e15371d3      1
    27 0xa8c4e3ce1743d0f2a6c227548c982a7c40569940      1
    28 0xaa15a96fba6c84737e632a93140d9f549f55338f      1
    29 0xaf469c4a0914938e6149cf621c54fb4b1ec0c202      1
    30 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    31 0xb30a154d44c97262cecc51e4c429b2d138a161cf      1
    32 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    33 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
    34 0xc06a00899ebd1481f243ac4ec92a276c1ff26e8a      1
    35 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    36 0xcebc5f213fcdadd3a4c88e8a186e105aa5b33a4f      1
    37 0xd69e63c3223f881a411a9264f8ab6adbf6b3b48e      1
    38 0xe4a718104474da81790a305bcec5cf98c0359a10      1
    39 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    40 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    41 0xeac0ef67aa5239d40c08984efdd61eacd6b8d140      1
    42 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    43 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    44 0xfc681305295ae89991d3aac5c444fb6d96b3ffae      1

## Airdrop Artist

``` r
c(airdrop_xose) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 33 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x030571c35b84e01613383d631c10cc8afaf83977      1
     2 0x0734c876c0923eb8356baaf51e5f71229b7d2bb5      1
     3 0x1795cd0a6761c19a1b019d6478e07c7a45fe77a7      1
     4 0x193d6bc3375f4821cc2c3d69f79261576cc3432b      1
     5 0x1c3f8d46b9db827548078a2e7dd46cb487eed0e1      1
     6 0x30f0f915a4752445cfeadfac0beb539fa1192b38      1
     7 0x317fdbf289a41435df30efcb00015f95b1eaddfe      1
     8 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     9 0x442465cbc0cd84f93c12cb1134dcdad6aa54f1aa      1
    10 0x48e7d74810b0950ef03232468ec294a1a14e2e23      1
    11 0x51eea86e142bf26847f160ae4392b3de8a3ca03a      1
    12 0x61e1e9a8bde81106b881512c502c38fe17bcbbb5      1
    13 0x697e9c6783e348ac67d2c129b6b00ad616bf74a5      1
    14 0x6b1d9489208f6693ac85fed38cddd1fcf278b094      1
    15 0x6e81af1bcd504a51da5bd6a0d7df70d7674ce90e      1
    16 0x78a42a84bfe3e173c3a9246b3f5f1c5aa8bbae72      1
    17 0x8ee376de530fb9a734df676e7e4342b48355f483      1
    18 0x9be353a221c4f3762092f44896054bd071b1481d      1
    19 0xb509189877c46f92cb784d59e1fb6dfc03cd7ede      1
    20 0xb6cf777e3696a502107417265c92d1b075636a10      1
    21 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    22 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
    23 0xc3e2fa491f1f2a36407f5ef3561b4bbc7851ef80      1
    24 0xc77ec27e6ae82ead54894dffc7d3f3fe3a09246c      1
    25 0xc7d9753e4a1b95351af5fed8e0997e02f1611d31      1
    26 0xc8780bf4dc27a310ec708794b9f40553cc545da0      1
    27 0xcfcfdcc985aa078e8b4e086c9355a3ed888197df      1
    28 0xd2350c37a02b40f8a3bcf94da9466fc418a5f409      1
    29 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    30 0xd92df32ec6cfa9e5d58d68cfbefb3fb699771487      1
    31 0xe530507083ecb8d2b474b96544216e0dc92b1883      1
    32 0xe9c2d3bf3a898f700cade5f5f4a89a5e5756f4e4      1
    33 0xf52526c0db0c0877ae726b3c33622bb65c10a1e7      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 79 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x28b8d4f7516a112e2e2fd462293a1c27cde327a7      1
    12 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    13 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    14 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    15 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    16 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    17 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    18 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    29 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    30 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    31 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    32 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    33 0x69e68074f1aada957edd39c5eae0069973343f30      1
    34 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    40 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    41 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    42 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    43 0x82139687faae8a29851902783e02e699de0e0846      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    46 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    47 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    48 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    49 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    50 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    51 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    52 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    53 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    54 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    55 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    56 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    57 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    58 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    59 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    60 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    61 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    62 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    63 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    64 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    65 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    66 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    67 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    68 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    69 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    70 0xdb561a899557404581e6180fe6d4178577dc117b      1
    71 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    72 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    73 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    74 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    75 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    76 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    77 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    78 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    79 0xfd22004806a6846ea67ad883356be810f0428793      1

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

## Allow Artist

``` r
c(allow_xose) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 84 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x056f154c822cb374508cd318038c3d1e1230c377      1
     2 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     3 0x14c9078dacf2011f6dafd54d4cb6312d573289fb      1
     4 0x180c7f25a4dba3310cc3746619256d1eda5a4f5a      1
     5 0x19e428e313b390412abc914a4576fa5b90b68c7b      1
     6 0x1b591e98dc0ed3f967359242064c76161fb4c0dc      1
     7 0x1b8532be318e881a6d073b6b24aa584d76d017fe      1
     8 0x1b9fbead09468a48eb10856ba0eeb1649c6ec4c9      1
     9 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
    10 0x1e6f942e17e7f2d2261bb97f1fb8ae2171c6e430      1
    11 0x2153a766c5c25a5a7d9d06b1fa165f8c80517d23      1
    12 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
    13 0x2fb0e475f6d495dfdfd9176af9113f48f7687565      1
    14 0x30dec07af807a47c4c5a2ba69a22738831f002b8      1
    15 0x31d649312cafdd4e77f5c15f7fb1a10aa6d603d5      1
    16 0x32f12843e7dba0e9452f5223713bb9a332313d2e      1
    17 0x3437c527d6c5cdb8cf0ca5029eef864085576574      1
    18 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
    19 0x36af69fa35c61841349f5bd3f09efe94e5cebbb9      1
    20 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
    21 0x4d58baa3c3e25af70f7080cbf26dbff0fe316be3      1
    22 0x5147c1a9e594041ccdba5d044b6274231dc6ebf5      1
    23 0x52abe6a75eaaed6545b615ec4e0d08e689e84cc5      1
    24 0x547322faf3b4d0e16c8dda880da2bbd40ebd6109      1
    25 0x551d12d97ff1d56fa8009c88b65d986d60c5667b      1
    26 0x584faca9910d23f2af6de60ee4f7778e8fefec1e      1
    27 0x5a449b37a73ead94a23927a26b81adb50ff51874      1
    28 0x5bc926e531431b5a1a0f26e2dd4a7aa8f322b1ce      1
    29 0x6088a918b024d2f4a2dc9f734f3656c9c27e978d      1
    30 0x6147bb122124d41cd6aef242f6c016cf633ff08c      1
    31 0x64c84501fd4aa7f597bd855a41ec92f493bd2b1f      1
    32 0x6ecae358e99dfdd1abe900bebe5f775431c12324      1
    33 0x71a7702ff3d858a0853a918fb44c537740a8bde9      1
    34 0x720d2aafacf8c934984efb7d4fde200be05385f5      1
    35 0x727f25672f4f2815831ed496c87b33faeb639238      1
    36 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
    37 0x8497277c9339170a0420e86da8352e0c084624cd      1
    38 0x893203e9cf0d4a5dfb446e723711dd5d46df8604      1
    39 0x895bef95023f591ebd1a9e8f53bed2b73702e4d1      1
    40 0x8a3b8dfe66bc420c1da15af38bf1ad3640594538      1
    41 0x8bf048b0a714ef31398097037510ba449b2b99d8      1
    42 0x8f70b3ac45a6896532fb90b992d5b7827ba88d3c      1
    43 0x8f9be4310f9abb0e5843cc6363908c9b01dfeb3f      1
    44 0x90c0e8c6e4791b9e4f99272fd70016a682056847      1
    45 0x914f7592f9c7969ff0a098c2c1d64f10c3407255      1
    46 0x92b3cd6a5e8dfe8df7ea7bd40ed75e670107b782      1
    47 0x93699c54757c807cf39e77892a4f2e572e98c604      1
    48 0x94777385a21559d7b9d161803d12ef7d70905b9f      1
    49 0x996e1d4ce3e1e558889832832004b2466153adbe      1
    50 0x998cd437f187f924ab2713a2bba037001a812bd7      1
    51 0x9dcf96b9e91e5250f0106197d6b396d736ddc680      1
    52 0x9e4f2bf1d32adc25d650e0b7f7cae04d861721e5      1
    53 0x9f12665668e0db6d95f216612a0d9689bf23688b      1
    54 0xa5fc9436df21125e88a2f73089bd17dbafe46e74      1
    55 0xa75e52dc9616c9b99f28dbf9d4a2db16dbcdb5d7      1
    56 0xa7efbb4a06a680f3b65c5861ec387408ceafbec8      1
    57 0xa81155b70662d44e07fb432d5e85abf7d9e1732a      1
    58 0xaf4a749f3e218a67e40fbc295ba58f93c890a27d      1
    59 0xb1fcc14c34e77105d6b320d79bc720fb529ff5eb      1
    60 0xb560264a2ec39a8b125fd6f2955d1ffc54dc6c88      1
    61 0xb6fef67b00d644fbb5e4368aed9f85ec2d134fd5      1
    62 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    63 0xbaa738690c4e617f64c00fcbdb2ee69281d5da80      1
    64 0xbd46f6fa9045e203dfc0cd31d19294dc2011cce2      1
    65 0xcbfb9a917c2b45fbd8e820e66602ef26dff9cd40      1
    66 0xce5de409ae461a16d17837a55ca245b7805b19cb      1
    67 0xcf8cf5df28db4f4e8376c90d8cebd5f7a4f73620      1
    68 0xd273467a0dc1e8332c396c62ae2a6b8429d3777c      1
    69 0xd385605623db1be4974838829703b8e29124bf37      1
    70 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    71 0xd78f0e92c56c45ff017b7116189eb5712518a7e9      1
    72 0xd9a64ab214cc849ae97c67a40caac2a71e38acf3      1
    73 0xd9e04cadccc5ef61851653591b384e597eca17ec      1
    74 0xde29f91a18a03b2de15bf1d50db272dd311aca56      1
    75 0xdeb9fdd1711a826577347f46a2edee59dc143589      1
    76 0xe01cf9c2f08e0bb3cbfad7d05c20ded26245fe63      1
    77 0xe328318c037754ef86ffdde28d4e2acbd8da9b6a      1
    78 0xeb9ae650f8e7eae4f6dca2a2af35596add420347      1
    79 0xf2439241881964006369c0e2377d45f3740f48a0      1
    80 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    81 0xf5b0d29df7b5f55f06ca78ab9e1c0c53eb552608      1
    82 0xf78e40101da8a9ced4948cc600f7f6ef6d1f9107      1
    83 0xf7ce48a33cf8c9e6874f8f68ebbfca40bca748bc      1
    84 0xf8b44217a26c5046cdf53d478adb8e22e817445f      1

## Versioning

``` r
R.version$version.string
```

    [1] "R version 4.2.1 (2022-06-23)"

``` r
packageVersion("tidyverse")
```

    [1] '1.3.2'

``` r
packageVersion("magrittr")
```

    [1] '2.0.3'
