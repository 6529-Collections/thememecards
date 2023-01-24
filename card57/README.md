
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memes_airdrop.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:503         Length:503         Min.   :1   Length:503        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:503        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16468869 # https://etherscan.io/block/16468869
block_hash <- "0x517fba5240bd98f8be1000b3356f725afb30cdc4ba5d03329a6c3cac90739a79"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4677 

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

allow_memes1       <- pick(snapshot, contracts=c("memes1"), address_pick=199)
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 199 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00b8fdee9fb53a4783fe174196e724c385608636      1
      2 0x052564eb0fd8b340803df55def89c25c432f43f4      1
      3 0x0593cfb5105e6158835b6958f0b5cd7d78f1d4c2      1
      4 0x05f6e36090aeccb6b296bac1e0aa7ff185459218      1
      5 0x0743882fa09a0c257b18d53dc1b101d3f32a04e5      1
      6 0x080011fc1bc948505daa9d76e5a3ff24e65d6d9e      1
      7 0x0a81816f351dac9deea2977df128d3d25dc202d8      1
      8 0x0b2efb0a89f270e31241c4e9ffc20a98de37945a      1
      9 0x0df149648233704de9dcd6f51ef51c6e8d37072d      1
     10 0x0e0886a3fdbfbc8fa8ec7fef815d3e3b4ed0888b      1
     11 0x0e2565a4998912ba4b512a6862e6a872626cda7e      1
     12 0x0e2bfc70c51774c21ad70b0926bbe854a34cbd37      1
     13 0x0f82f6b926d26621ac0acdaef7c0cf461f6430fd      1
     14 0x10f2ab2886d2ffda72abb390545d1bce8b2d20e6      1
     15 0x11fa4f109a1601bd1afdb509b3bdb1e23fd6a675      1
     16 0x13b6a9aa1adaca0137e6041edf1b2f3de30883f5      1
     17 0x143dbc3740da457382230fcbf8b396956b4002f7      1
     18 0x147704ec27b1b1a30e2e320c486c0cfd8f710564      1
     19 0x15326a892b1d1b03bb499777649025ccebb380b0      1
     20 0x1932033c6d7e424cd2a6929e22a5963caf256791      1
     21 0x1a7e6c5a59f070681640d59222757781fe090b89      1
     22 0x1bc14f2908955dbdf748b094a2032194807f1ccc      1
     23 0x1bf54fb23138c9f11d8c6b0821663ac591294e4f      1
     24 0x1c91f3034c235f24d39157e9f052d152f4c3857e      1
     25 0x1c9365a7bef98384401d513bbfa3416ebfe99578      1
     26 0x1dbdf2281b5e817498e8ecffcb4e25b64586e1d7      1
     27 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     28 0x1fae7b5831c0b57dd172da74fd8beb983ab6b20a      1
     29 0x247d9e1fdf72941053b633e5a5428ca8f954e034      1
     30 0x249a49d3201c1b92a1029aab1bc76a6ca8f5fff0      1
     31 0x263da73598db8ecfedeb2ff17b5f12173c929929      1
     32 0x29cd9553e268ca520d73437a8045ab9604a149e5      1
     33 0x2ac3b47e7bc9d42822c1db3e6948c1a47051e805      1
     34 0x2c1969425c01d58e103f33546ff799f0648fe634      1
     35 0x2db19ee7f7e0eae5730e7a393fcc899d70df9f25      1
     36 0x2e7bd05f67e7daede69cddd040daf36ac8e72680      1
     37 0x2f242df0d81cf7008f8701f0fd7080d1eb44fdaa      1
     38 0x303041eaee7a348b18142ae7e1870e0b1b9f3598      1
     39 0x30f0f915a4752445cfeadfac0beb539fa1192b38      1
     40 0x331017600f8d211462e60e664a5c3332b0f77353      1
     41 0x342d23b3dbe624fb87f11ef799cec13efb8586a0      1
     42 0x35707e11098eab56161d3cbd37dfc8e232d36f03      1
     43 0x3592283e6b611f323db089b82946f6df2d1948ed      1
     44 0x3600aa29edb4f1c864014ca5d5f233e8616fa9f0      1
     45 0x36c246d67dbd8c5c2988091bd6626440db5424d2      1
     46 0x3d586423db784c6a395b48c2a06a89259e31df7c      1
     47 0x3e02aa6b648394769d9094838cdc100962d33e72      1
     48 0x3e89a0f26657013bf251ced30d5d198c3b0d2466      1
     49 0x3edad7dff01d015de2181b5e07d3370a4d6304ce      1
     50 0x3f1b2862d959961c8e0021991320bef9a7d506ac      1
     51 0x416d7f3823bc7d8dbab00056ede0325e52f0a5c2      1
     52 0x4193366cba7cd552d671bcbb8d8a21c647ea8159      1
     53 0x425052c13e1785b736e69324be5685e4d396a3a0      1
     54 0x4299b157abde3ef09e3e2db77ea7cc2a24b30e5a      1
     55 0x450a6049e00c72c8205424a1f3d752ad06382fb9      1
     56 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
     57 0x47196dacaf732ef3d6502ce01fdf9b1dce8c4a81      1
     58 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
     59 0x484a12a27e8c95718e0b60d14369bfeaa426aff3      1
     60 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
     61 0x4b3b0a6c0bacb088b6b0b0ba31bab77282068117      1
     62 0x4e383f2a50826a0767f445712cb1bffc82bd363f      1
     63 0x4e8d2f44b563a3ff93ac07dfe76c9d603be9533a      1
     64 0x4f65f97c0e40e8635253cf27ce5c412c450b4710      1
     65 0x51097fdf7b9ba09e578c4421b99b427097b055a1      1
     66 0x51baa32ba08ffaf3fa54b0cc0a0f7550fb3f6171      1
     67 0x55c8ddb3f486d9f434da6bc24bc756d8d81b2a7b      1
     68 0x577ec302c94f57d09b7fa1006e6165554d576f04      1
     69 0x59514026c0dc3e157317f3ad4779b01d0bedefde      1
     70 0x5a5617ba16beba3d04ae3c6b1e9d522c1856763c      1
     71 0x5b049c3bef543a181a720dcc6febc9afdab5d377      1
     72 0x5c1761e297f8bbbe8800c9cbeeb9d6f46639c8cc      1
     73 0x5c396e679cc63df028e4c7eab88543cf18210fa1      1
     74 0x5c43c2d884fc7f08f69ff6168ac7c3134d3bae8c      1
     75 0x5c7ef5ae70072f7bdc3a546ce86b9e316af9f57b      1
     76 0x5d054491bf455701d266fc2088c7afe3db267b97      1
     77 0x5d2035ebe68f4ce03d6822a68472d82c74af18cf      1
     78 0x5d53197394f44d8a9ba7a6f747ea1d2155049b6b      1
     79 0x5d999a7dcf597169a7f8dfb03c7935da39c163a7      1
     80 0x5e65a0ae97928b30f44c6fb852c27cbb47acdeda      1
     81 0x5eff35d620168bd496b39243fee0afdc593e972c      1
     82 0x63b39fec34e761f30a025989409344efdd7dd4f1      1
     83 0x6454d7a04ab11045acaff1881bea152a62e530d6      1
     84 0x64713256f61ed47789d1da3cc65157fe957910b0      1
     85 0x65dd62569b3dc812966a020aa27a317afd42a9ed      1
     86 0x66482c9ffa6eb98659a1f8b5535d146471c0f743      1
     87 0x68b3778679e6c88a19f612dbec9bacc3a4e52d05      1
     88 0x6b1d9489208f6693ac85fed38cddd1fcf278b094      1
     89 0x6c0e639dcfc73e810bf66c90a9005d4ebd8a7e5d      1
     90 0x6d0ae96edea9de068bc0f20250a8b5526f56a077      1
     91 0x6f1d374e14c4e4dc3bdc12f5e06f57cc9328c8a1      1
     92 0x702782ec4d986dd1d821d71452cd6d10ea5e0ea0      1
     93 0x74c0aee5d5ed76d9db8cee696978968fdfa5b1b0      1
     94 0x7581a5a10ab7ab75d4cec577e4669893818fbbb6      1
     95 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     96 0x75d3bd82c08c7933f3d2b7b79c4e4208e229a0d6      1
     97 0x77777777174870c5067e39dff228d2db44f52b6a      1
     98 0x78d9f7747bfb3b06e10e2fa7a99c0209955eda1b      1
     99 0x7dab31a034ef37229a53d270dd973e4f559665b3      1
    100 0x7dc166391edba45b124e236cd099fdb266e541ff      1
    101 0x7f8b215572f16f982c626f044cde114152dd0e6a      1
    102 0x807ea4c5d7945dfea05d358473fee6042e92cf37      1
    103 0x81b9a5f21efdb5df0210471b9a94e0d4ad9951ed      1
    104 0x8244fb57a34d01963c609cdf3f07ec7350310ef3      1
    105 0x826b60ed96fa061db7f048619ced00319b503f54      1
    106 0x8880b69c95c0183bd3d4f30bc4272e457552c3d2      1
    107 0x88f09bdc8e99272588242a808052eb32702f88d0      1
    108 0x89b88287e2905003551f06ed77c3491be360de8d      1
    109 0x8ac10968eb62178a0cb5d71df80a8699b79b8087      1
    110 0x8c8024bf5f90a06cced7d32babccb934942c82f6      1
    111 0x8d03354b729ebbe5ac44dc35f067d01bfe7eb684      1
    112 0x90dc1cd8da9858a8bee4436cc9761c3c24ae77df      1
    113 0x91b26fffffb325e13f1ef592b0933696098044af      1
    114 0x93c79c3ee2964efb7235b9f445f50e6ff9003ac1      1
    115 0x93f02bbac7fa759e58b261b286e30ff0c6c922bb      1
    116 0x952e2d065e60b0fbc9f130917f227621204ed48f      1
    117 0x958d0d86233a27222f6ef4d3e555191393f6d477      1
    118 0x96667e68a477ac7220e7e2d54d552607abd7f758      1
    119 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    120 0x97d670721d09f42f56aa6c3cf501f81d28cb4876      1
    121 0x98025f7776884d38093508e31f4e3119e2979cad      1
    122 0x9a8842a31bf85d7cb54a868c6ad29264faa78379      1
    123 0x9aa2b4782b7cf35b7dfc699604a4de16d80adfd6      1
    124 0x9be353a221c4f3762092f44896054bd071b1481d      1
    125 0x9e9c0431eafe5addf548bacfea974ccb550dad45      1
    126 0xa081ae42802e0be86f5a6dd9274adf50c9986a1d      1
    127 0xa3ed68420d25232db51e74836ab09e61fe04352e      1
    128 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    129 0xa69b52e1506fe0274f181e543ac1dec962776c46      1
    130 0xa84b6da113c0279363dd1dcc7465677a4b0455a9      1
    131 0xa8ba075e79241d0771fc4237174c1a8d67edf7d2      1
    132 0xa9e3eddf2616b3d2b142f439575a7811e54223e9      1
    133 0xab6ca2017548a170699890214bfd66583a0c1754      1
    134 0xabfd4364c7ddcae4e03bf5758eecb35d062a2c9e      1
    135 0xacb30829880b7006e54158ec0b877d988c02ea83      1
    136 0xad91770c1e6b8d2937317ce589a7d31d856b4ae9      1
    137 0xad9b8a1bdd5dcbf2cd5f69d3f13eefe37b51245d      1
    138 0xb07c70eccb3373e9108a436cc1028d2ec6312ebf      1
    139 0xb0b45ab7a7790ba43d8470d527d05f75559393db      1
    140 0xb0d27d60e77abb8284c8f8543812bd165f4e6d24      1
    141 0xb1bfd5873a33bb2d052e724808a87534fd9d4153      1
    142 0xb509189877c46f92cb784d59e1fb6dfc03cd7ede      1
    143 0xb57281325a55b83e373ae3be11716956e952c976      1
    144 0xb9c416eb0d03efdce046e4f4bba26c1c3544f4c1      1
    145 0xba567d6ce93c021e46baa959ffc241fe35a10297      1
    146 0xbb5e3399daf10fbc5094e3f4d3b9aedb7547856c      1
    147 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    148 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    149 0xc0986d68e483376291922a5aa3a5a8cd8928e523      1
    150 0xc113eaca52c51372106f9fa57d6ef19638d61fc9      1
    151 0xc46db2d89327d4c41eb81c43ed5e3dff111f9a8f      1
    152 0xc5f8dacff69b602f2559bcee06eb355b5c4fe238      1
    153 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    154 0xc709c2e280f246123d0bb72394bc79f10fbdab4c      1
    155 0xc8780bf4dc27a310ec708794b9f40553cc545da0      1
    156 0xc9aefca21844fdb3fe8f58d883f908991d757692      1
    157 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    158 0xcec2baca25afaf9bbecfd9a800ca2c71576cbf9f      1
    159 0xcfcfdcc985aa078e8b4e086c9355a3ed888197df      1
    160 0xd038d146dfab6b92df8ce2c92369f09375fc5b32      1
    161 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    162 0xd2839e6d35efae479bcc932fe2779f97a24177af      1
    163 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    164 0xd3641bd03a67af07550b049065a19f84333b4b5b      1
    165 0xd394cf7e0c9f7c89f81a2870c576c1910974047b      1
    166 0xd7b03000928f4a603b4e8a58a9f0c3f6587e3057      1
    167 0xd803f85b52e359ca7151336be08c7d22683d0a7c      1
    168 0xd93ceb6bc9127a553726caabf0afb95907e88e32      1
    169 0xd94d142a9e84dcd088bf458b06da0d20448b8611      1
    170 0xd9be72a803caa9b2ea0fb20f95eca03bb2bc16df      1
    171 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    172 0xda8b32bdd0c67ed440f54b0853631dea72f2d2b7      1
    173 0xdaf288eea1a696ecb1dd37a1e6b2a4058a604e7c      1
    174 0xdafd126712aacea92eb9fda9f148d949a3453faa      1
    175 0xdb5cf3013a437ce5437b241b9f39b04f5d0bbb71      1
    176 0xdd0c444fa6c64614393c381a7763cae204199f7c      1
    177 0xdd115ef817fab7b54bcbf3d4152d2e7c9d7c4e37      1
    178 0xde08b96895c99f9983c631f7a9b7d504bebeeb94      1
    179 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
    180 0xe3c2eb2e71724aa14b1d13f81448873e983fc25e      1
    181 0xe41fef6d3b5f174ae306b17e80e023915d9ac093      1
    182 0xe4c8324534c0c6bca174cd0f02fac9889c36ba59      1
    183 0xe5f648ea98f17aa12acd59468643c62fbbf813eb      1
    184 0xe828abd66d651cae1c1ba353995241fc3e5a336c      1
    185 0xeb4e17f4b6a9490ee3579c0f1edf144868f2753c      1
    186 0xeb7921cda81164f7e711ac0bec1a9243fd10a815      1
    187 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    188 0xee58519040de3504932f4c79e1d056ef1b42a026      1
    189 0xef8a4a0ea54d053ed9175d078e69db5f6450329c      1
    190 0xf06fc22f0f9d15a271e18aa216fe183e20a7a092      1
    191 0xf2bef831670df52ae5492dcaf6ae62aac86f6cc7      1
    192 0xf2cce62603ac08d3fe92bc97bd68f137b7f1b765      1
    193 0xf42dee9230dade4c244b8d72a5f1eb2cb91f242a      1
    194 0xf54d3e434a74865540224ee6219f60ba35de228f      1
    195 0xf65b1bc72ffe0c8bcbc91da87abc53ac8cf884fd      1
    196 0xf8ae436403100fa10603aea3c7b1ef097d61d89e      1
    197 0xfaf1345b95a606f44e392536d864a5960cd4df23      1
    198 0xfb9aba4aa3f8d1c97e9f0eff356df2ee9087af02      1
    199 0xfc3dbb65401d54e5567f5bef94793ec1999450e0      1

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
