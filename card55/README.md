
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16424969.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:32232       Length:32232       Min.   :  1.000   Length:32232      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.295                     
                                           3rd Qu.:  1.000                     
                                           Max.   :652.000                     
         name          
     Length:32232      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16425469 # https://etherscan.io/block/16425469
block_hash <- "0x053665f827f0f0bab911d45bc2017a3705d1807e928ad1eeaad326371629d818"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4446 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=10,address_max=1)
airdrop_oveck     <- pick(snapshot, contracts=c("Oveck1","EggHatchers","Cuba"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_oveck       <- pick(snapshot, contracts=c("EggHatchersEditions","Balance"), address_remove=address_remove, address_subtract=airdrop_oveck,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 10 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     2 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     3 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     4 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
     5 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
     6 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
     7 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
     8 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
     9 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    10 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1

## Airdrop Artist

``` r
c(airdrop_oveck) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 49 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x109632e956ca6baf528fe5a9724c43b0c4a63fbe      1
     2 0x1656fa54d6ba064a8fa0deb49c140baa8fcd701a      1
     3 0x22aa3f5d1daffe1a9df298e79a0cf2f98c1b92ff      1
     4 0x237a86eaed0df656d8bc4ae619760ade5b4705f7      1
     5 0x249a49d3201c1b92a1029aab1bc76a6ca8f5fff0      1
     6 0x2604fb7b2c561d6a277a0ec8c2308a26cee18272      1
     7 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
     8 0x3432b45b9ee95bd5c31a726b936cf2ec719a2153      1
     9 0x357fd6924cde3df99a9e83f1ace7a9d85cb18aa9      1
    10 0x403afdf9ea925d3b48e719a44610da1679a57651      1
    11 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
    12 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
    13 0x495f947276749ce646f68ac8c248420045cb7b5e      1
    14 0x577ec302c94f57d09b7fa1006e6165554d576f04      1
    15 0x59560854986b354d2dbc4368a09526dae0b244db      1
    16 0x5c7ef5ae70072f7bdc3a546ce86b9e316af9f57b      1
    17 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    18 0x73d97c30603b73cf4ccde4934c6027a9599d861d      1
    19 0x834cee2c58b212d37be016f303bc46e8184bd864      1
    20 0x8379ac05ef71f982d497f392dc1a14ee859efd95      1
    21 0x8497277c9339170a0420e86da8352e0c084624cd      1
    22 0x877167231d569c9fcf5e7a5e2b073c7cd41f2cff      1
    23 0x888712f0397e830c9463f443f63eb8992841020d      1
    24 0x8ae2c787c8839f9e3e52e7d6757caca2a76dc140      1
    25 0x8c8024bf5f90a06cced7d32babccb934942c82f6      1
    26 0x8dbb75c576b71b43eea54398f8606aec530181dc      1
    27 0x92cfc1c067ec6259b9019ce897d9035a01b1b750      1
    28 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    29 0x9a8842a31bf85d7cb54a868c6ad29264faa78379      1
    30 0xa32560268dfff7e62cd0cda1dcc699406321aed6      1
    31 0xa8ba075e79241d0771fc4237174c1a8d67edf7d2      1
    32 0xab6ca2017548a170699890214bfd66583a0c1754      1
    33 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    34 0xb8adcc8e382a7c43bdfa2e1c47059a1afec10e06      1
    35 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    36 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    37 0xccf43dcc4e52e0216e461955bd98b08da53213ea      1
    38 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    39 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    40 0xd6dd6961d3224958fcd306b76a991ab974ec1ebc      1
    41 0xda6ad74619e62503c4cbefbe02ae05c8f4314591      1
    42 0xe8c4156a19951deff3203f116fc63274da746baa      1
    43 0xeb7921cda81164f7e711ac0bec1a9243fd10a815      1
    44 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    45 0xf2b15320db6992ba49a7563e50e888c20edcbe3c      1
    46 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    47 0xf636b6b5913ee5c3bf4d52d13ce6c40179c794bf      1
    48 0xf6c5a843ab50e77bbeed76dd40ad7b2e932a2b79      1
    49 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1

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
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    17 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    25 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    26 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    27 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    28 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    32 0x69e68074f1aada957edd39c5eae0069973343f30      1
    33 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    34 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    35 0x71677455ef1479a96596cb7fb894d16dbe6e792a      1
    36 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    37 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    38 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    39 0x76db02500f7631d57bc2dcdca9d4cf782b99e119      1
    40 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    41 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    42 0x82139687faae8a29851902783e02e699de0e0846      1
    43 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    44 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    45 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    46 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    47 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    48 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    49 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    50 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    51 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    52 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    53 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    54 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    55 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    56 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    57 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    58 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    59 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    60 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    61 0xbba3ced54477c12fdf16d7009771affc7a8c9ba1      1
    62 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    63 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    64 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    65 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    66 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    67 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    68 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    69 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    70 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
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
    4 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

## Allow Artist

``` r
c(allow_oveck) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 122 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00a28244ccba541b353f33726d790e4238baece5      1
      2 0x0396e41347f6a8ca45b4a3efa82c436786dd44b5      1
      3 0x039d7c84bc43f8bc311ec21aeef57dcb45a8091d      1
      4 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      5 0x088814aa47878c126c527cf61e40955f38305de3      1
      6 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
      7 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
      8 0x1052dd3882746ed4388b00b0353a69b38f612471      1
      9 0x119f8903be1fc789cdc7827317143f1c071f22f5      1
     10 0x11e7dfd1ceb3571e00888e96c77ed92fa32c3692      1
     11 0x13b5de1a572e2f2fc5c3d9aaae88583e510f6af6      1
     12 0x13bbb01f3fa22a830ac7c9de9b52c0bfed020a83      1
     13 0x155a0e449e8c9266640d80a685ad7809a1c467c9      1
     14 0x1a1a0fa219da646ac7c2bdc7ca00a677f78d280a      1
     15 0x1e8e749b2b578e181ca01962e9448006772b24a2      1
     16 0x1ecf471fd7609caca60dbebc382ab25006e602e7      1
     17 0x2310da9a053ee821086eb150025f67db0313ada3      1
     18 0x250c3cbc55750a3850ff7846697d225b44ae824e      1
     19 0x2595aaffb353e812bb5cbb63f7f62e36fe528f02      1
     20 0x2684a5daa74f60f6e7511bb317e4ada4754b1427      1
     21 0x2afd8ba65eee777c0f73bb65ef07e1024b627b55      1
     22 0x2b2c3544cbd93aeed917e0a017f3387709b4c679      1
     23 0x2d6836b5d89e319f38ac3c058f4c835f0c69b398      1
     24 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     25 0x2e0d63ffcb08ea20ff3acdbb72dfec97343885d2      1
     26 0x3016d104c55f1756020cb6c11a090648a34a7930      1
     27 0x305d4e760299a0941c1ee984d5ea3da83f84399a      1
     28 0x320545619413f669bcb0f622ad522038ddd3e6e0      1
     29 0x321ed5739440bdbd731d54a19aa20b18398d374f      1
     30 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     31 0x3330777cc5270db65e7df1d738e989dc16dd49f2      1
     32 0x3433c2753da24df566ea14c40584179e97396cf3      1
     33 0x38c3cabb114f9eb5393ebf28d8473733121801b7      1
     34 0x3a943f529de8461697702fd9bb8fcda5d9a8ce61      1
     35 0x3ccb86a565daa86d1613a634dd16663779bbd63f      1
     36 0x3d5ee44e4fafd6af43ee28269af157763d6d33d0      1
     37 0x3ec3c31694137b9cdfaa18f62d7ca20300155afc      1
     38 0x49f9c2be1aa71afac6f2f2ce5b884d8cda10e26d      1
     39 0x5470c5a6fce7447afd2c9be3a0f25e362c093661      1
     40 0x562257821731283b842cf689df9941ddbec3a2a5      1
     41 0x58d55449e4cc7719970a27bd1be569cd6834483f      1
     42 0x5a8da3f3d058d302b2e9327f32dc28ac175a4f31      1
     43 0x5b049c3bef543a181a720dcc6febc9afdab5d377      1
     44 0x5b33b6f41e87111b7d22685b297f086b5535034d      1
     45 0x5c3b3f880597349e7a1ae7034ff11ec6d7e909be      1
     46 0x60282772ab3e7f9e71b6eabd698ce4de82a18776      1
     47 0x6278e4fe0e4670eac88014d6326f079b4d02d73c      1
     48 0x631ebefd35fa59ac01e9c0514cdbcf9cb70b18bf      1
     49 0x63784c971e102bce755b140680de86404843aedb      1
     50 0x652d7e1b706b10de0bfee179c1cc41df73d3ed34      1
     51 0x6608bca71bee95ea18daba0ffe4fae6d50056e6f      1
     52 0x67ed35ed9d3bbb0b82596f4e0e07499a87aac518      1
     53 0x68b3778679e6c88a19f612dbec9bacc3a4e52d05      1
     54 0x6e0dfe55fbf0c7a9902aaca535635bf86a03f1be      1
     55 0x6fe936422846dcc13f3a8a9aad4be6ac3c15230c      1
     56 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     57 0x75b772f2bb4f47fbb31b14d6e034b81cb0a03730      1
     58 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
     59 0x7a926e039b30b9ca5a0499e6a8b1f7fe2c30aef8      1
     60 0x7b8cfa270d6ec606a1211dad628a3778c21fa9cd      1
     61 0x7c344832ec4142108f6b6bc49ce884281e6a19a3      1
     62 0x7dcb39fe010a205f16ee3249f04b24d74c4f44f1      1
     63 0x7fc200b56b7d0120193f034aa20a51eebe15eea2      1
     64 0x83bbd8eee409a756ed2fbc3dded6bdcd02e65b22      1
     65 0x8454dee79effe16d9fb369594eb0321230ccdd73      1
     66 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
     67 0x89d42f152a826e28c69413ec5c98e6bccb7c7abf      1
     68 0x8b975f76989a92a29a9d4a588d9f24c80cec29b7      1
     69 0x90198c48129ac7564933f8bab85669e303ff6b9f      1
     70 0x922d1874a9984cca520a6c078f9856a158442f57      1
     71 0x9a78c0913396f8607eac7c4ae211a9138c2a1fac      1
     72 0x9b9125633b27c358c74b15a6064d42196d5a8f52      1
     73 0x9c44bf6f8a7291b7a04bebcac7c5cd7fa7771ab1      1
     74 0xa08b56016571f17dcbe77b0fe769f85511a4a657      1
     75 0xa1e8c429c49b3c628fcef76b8d59495cf2c8bbe6      1
     76 0xa21e8556a8d69a8168818f16d9495f0c0efdd987      1
     77 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
     78 0xa3599cf1048930c9e897fe9245dd95511cd16f1c      1
     79 0xa706bbcc244d91a56ba9368c033cec1e75688af4      1
     80 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
     81 0xa7e458a1b32070387e7548063e1f5e7f3982e6d1      1
     82 0xa813e2bc05d8447d244ac40219aeda13b0db1873      1
     83 0xae220d647426e368ac321efec83a9d696bf84e7a      1
     84 0xb1bfd5873a33bb2d052e724808a87534fd9d4153      1
     85 0xb2d866297dbc7c7d8ad045c05afb48a95f28fb3c      1
     86 0xb41d6f4bdad2adfb160a2e22c1a00cd08ef7f794      1
     87 0xb8779ec12392f02b1f6dfbce7c8c28088da43622      1
     88 0xbbe40fa2759e7dda7c393aec4b04fc6ccfdfc14f      1
     89 0xc10fc4f8566f2e89cbfb682e50552c193aa1244d      1
     90 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
     91 0xc19b11e2540dc903ea59f9b5ead590e548a0f06a      1
     92 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
     93 0xc424e4642e0bcc09e545127200ed686d3869ad88      1
     94 0xc464465e1391a2f81ceabbf9afe211c32b43a067      1
     95 0xc5e615b39de5e528458094d476fd6708c2793e4a      1
     96 0xca63a0f3ac9bdaf6dd83bf2646bc2c0e9cf974bd      1
     97 0xcdd99ee657a33b1da6802f4117d7e5cb2ffa5d79      1
     98 0xce266203cc90f26b346a9f359fea7ced2f4e62dd      1
     99 0xd0ed643099041b805adec2e2af3329c06a20ffe6      1
    100 0xd839464c711294358b74badf801d935cf6b22e01      1
    101 0xdc6babb7e90b27e3f6be77d35eb03aa612b92efd      1
    102 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
    103 0xe181725de73c2a58325e5b08af5524a7d81fdae2      1
    104 0xe2b2793d51c652106807caf9b56db81f71e7e280      1
    105 0xe2c945b5093f6314e889e0c5b260d86457c47301      1
    106 0xe3369c94757681f339d032ec66aa2ac59e9a7c26      1
    107 0xe3482e02333f817d94e0b1f7cbc03a3a46c141fc      1
    108 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    109 0xe7189567e7c584fa61b786f8f60b83c66f777d81      1
    110 0xe828abd66d651cae1c1ba353995241fc3e5a336c      1
    111 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    112 0xe9cb56a31a8aaaa28dfd30a037fbcf470bce1e2e      1
    113 0xeaab59269bd1ba8522e8e5e0fe510f7aa4d47a09      1
    114 0xeba40500926f604c9e4e7ae56a253a0949ca7461      1
    115 0xecffed885667f154a28db2553657af50eac19f99      1
    116 0xef4c96571934fa142c2089bbfec88bbdbe05cc95      1
    117 0xf57a9b1f574b1f80c8cebe252706bb8b4d783d21      1
    118 0xf8a09bf413576a2e8b406dae09732aae29a1c8d4      1
    119 0xfb2ec6543d97808d8a00fca199c39022b0b3368f      1
    120 0xfe9a6e87b3910686b9e88ec9418dd957e0028ee7      1
    121 0xff93e7aa23280d83115aa513b16de7493edd0196      1
    122 0xffd3f6ca1e4baba0993c88bbda9403c8a92d326b      1

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
