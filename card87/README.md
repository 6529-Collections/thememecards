
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:23502       Length:23502       Min.   :1   Length:23502      
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:23502      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17059069 # https://etherscan.io/block/17059069
block_hash <- "0x597ddeaebda1b54cfa71ab7bc335b6eeb03175aba4741c2ee915847acaded25d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 5061 

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

airdrop_memes   <- pick(snapshot, contracts=c("memes1"),address_remove=address_remove, address_pick=11,address_max=1)

allow_memes_full_set_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_subtract=airdrop_memes,address_max=1)
allow_memes69_phase1        <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_pick=69,address_max=1)
allow_gradient_phase1       <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_deekay_phase2     <- pick(snapshot, contracts=c("deekay"), address_remove=address_remove,address_pick=300,address_max=1)
allow_raw               <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles           <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes11.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 11 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     2 0x23602ca06e977c86339ffddad74966e824ab691e      1
     3 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     4 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
     5 0x3876be5be4998adecbfbbad26604a762467e7f42      1
     6 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
     7 0x54913cc8ea17731d62589039dd0152f306473843      1
     8 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
     9 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    10 0xad102dacb54816ee8b128c63b83c7b5fc587f4d7      1
    11 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1

## Allow Memes Full Set Phase 1

``` r
c(allow_memes_full_set_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes_full_set_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 68 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
     2 0x0220e0b5a23bf2419e643de74649a01bf77960ee      1
     3 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     4 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     5 0x12cd871105b29760a61f56753d54a59381f5bcc6      1
     6 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     7 0x144bf0aaf16debadcd2638f572ebe42a92f5bcc0      1
     8 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     9 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
    10 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
    11 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
    12 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    13 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    14 0x2c9646bf2477a747897047b69ca1a1913ae5f611      1
    15 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    16 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    17 0x388160a99390392278afdba240046b8b5e73f77b      1
    18 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    19 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    20 0x42d38ed60a64d0b8c36f190e185d17db3617a091      1
    21 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    22 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    25 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    26 0x5df5342342701b8ae5bce28f74ebb73b5fc13a54      1
    27 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    28 0x69e68074f1aada957edd39c5eae0069973343f30      1
    29 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    30 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    31 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    32 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    33 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    34 0x82139687faae8a29851902783e02e699de0e0846      1
    35 0x86ae37857534a842ac56a4eaeaaf83d02f46d1f8      1
    36 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    37 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
    38 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    39 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    40 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    41 0xa1b669414d40e68c11652b1cd82381f2a6495b89      1
    42 0xa71000e72d38e6a84d7190f59fd3dfc73931c0e8      1
    43 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    44 0xa978eadb605761725d11d3b3a4045cf5859e2d3c      1
    45 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    46 0xb05b7ea3714a95857e45629def2b9c3577690208      1
    47 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    48 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    49 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    50 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    51 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    52 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    53 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    54 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    55 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    56 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    57 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    58 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    59 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
    60 0xe25b24cebed3055236e369570a437a99e1d32602      1
    61 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    62 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    63 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
    64 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    65 0xef15bd3f923245b24d64d6fbf57f036fe650c23a      1
    66 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    67 0xf1f476f144df01480297dca47efca565e8b0c9f1      1
    68 0xf84408070b4de16d6bb0ab2fd8b19d37e3fd1422      1

## Allow Memes Random 69 Phase 1

``` r
c(allow_memes69_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_memes69_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x019d370ada720786cda2c4f78f0f4657af2ff92e      1
     2 0x06cc21aeea76cebb12da254b6e982516662bbca4      1
     3 0x08eed2c77011faa1da376c5fd14fc33825053554      1
     4 0x0ddfa905aaddddc533bff695316979c0bb2d9eb8      1
     5 0x123be94bca3e5ae858039112d14c1c740a3989f9      1
     6 0x18ae9ef0431ee4835f3f075fc2980df02492d09b      1
     7 0x1a8e059d53bf0f50983c4e03e4931e8535b1aa1b      1
     8 0x1d4752ec4aaaf60d0d6817fed1d2784fe2080218      1
     9 0x1e5fe56aa8f66909fc7f2239749897e171380b65      1
    10 0x1ef710af2d02018cfc96521f83e54a516a6ba765      1
    11 0x2249f5016b1b7a306c5e98765bf94fd0a70d8b45      1
    12 0x244fe02fbcf4db4ad96063b161f00e444fc54011      1
    13 0x2759c9fd93e494c366e90eab777c7838de6da822      1
    14 0x2937769116151d3609bfcd7b5efd23dee4f4d913      1
    15 0x299d84d29283471778d922bff9baefe933988de5      1
    16 0x2f1f1f7008b57c7421f39726a0d223c5b17e35dc      1
    17 0x330a7cb2418a2856cf8ec47c2606900fd1a57bed      1
    18 0x380886e656ce40bd145a9fb85ac7e3dc51d5ee8b      1
    19 0x395f2502a415bca4a2f84d73bb8929182a5dd252      1
    20 0x3da0bf57b7a57fcccb32c4a54396a2cbe2f43ba3      1
    21 0x3de927d17cf02bf8d6baa344195acf80632932f0      1
    22 0x4571ca8ce79492fe7e2ba3023add71860ca363d3      1
    23 0x47d4f20ae83bcd350105f199f900e6e6104dab6a      1
    24 0x4b038c047f79e0cfd3762b9e8b4cb017709315fa      1
    25 0x4dd8f136fe8a8202cd6395ad34ef5ae49c26e78b      1
    26 0x53cbd34ca41a2373dc329c63dfca349d90139e27      1
    27 0x576ed9f68a4201e2f2597edc0b98523cc0ac5fae      1
    28 0x58d55449e4cc7719970a27bd1be569cd6834483f      1
    29 0x5b29d2d57bc47a9f55d3889352df04559305bb74      1
    30 0x5d181a27b5a2f444ddf4b89d65c88214d7413ada      1
    31 0x5d18c78f374286d1fa6b1880545bfad714c29273      1
    32 0x5e3d5ece73ce454b2d5c648753bc7562875e6e7a      1
    33 0x6abc0b05b554212a6bcdc10f6bff26017d5adbb0      1
    34 0x6b67623ff56c10d9dcfc2152425f90285fc74ddd      1
    35 0x6fe74660d9ccad62f5ce15657e95453f1c54b6fb      1
    36 0x721540d874053b14f3c5e421abbe7bcce008456b      1
    37 0x735d06ad1f537001dfceda4745541dca048ee088      1
    38 0x787ce8184512a78bb3d4dcb02e473eef4e0faa40      1
    39 0x7c00c9f0e7aed440c0c730a9bd9ee4f49de20d5c      1
    40 0x7d9e921c8334486732918fb3a034df44538eb57e      1
    41 0x7df8936a09c25e03a995d5c4ac1fe79be422745a      1
    42 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    43 0x81ad1a19187f0f3444c0f8bef98ea49c1b9fbc03      1
    44 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
    45 0x8888888888e9997e64793849389a8faf5e8e547c      1
    46 0x8e4c9e487ad507588b1b0e91160b752b1a6c9f9e      1
    47 0x9893117455cf1e8ac3e58e41c5885ad5adb1c9ff      1
    48 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    49 0xa23f27fe27b76c8df9b6d2e1c599e1ac80b44bef      1
    50 0xafef3bb7fc3cd8ae083672b65f79e6cefd4ac619      1
    51 0xb1f83385711225cf381f1611ae09d74e8a8d0f0c      1
    52 0xb33fb83c645ac2a12b138636fe59604312092484      1
    53 0xb789221616c7be137f33d545758f5510591d725e      1
    54 0xbc149c9eca07013ecd81958a5b0746d190682af4      1
    55 0xc439d5db5e5945ece1cc649fb72c036419cee1b2      1
    56 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    57 0xc97a112ca84a12cbdb7f47384a38e19c8622a6ac      1
    58 0xccb0f36ffe6b2a1e54c25870b8e1269290e75282      1
    59 0xdcbf46cbb479ad18861b488acb13af096ab88368      1
    60 0xde8328c4f5ceeb2c2b3e836b3354413161c81285      1
    61 0xdf25c3ce7d37858922b1894abe131e6d65c0b4a0      1
    62 0xe1b289da74a6a179b36db7def1026172cc3baa31      1
    63 0xe513e29ff7ede7e01eb96e2d3fbe1b1189386d06      1
    64 0xe540a8d94bd379c4d9c0478c8fe9f7fe7f0b06e1      1
    65 0xe83105d4e5b144e388d7d35c65c44e0da8c8016f      1
    66 0xee2b31fcf6c51ed01ae06cce05293c30afa5eb87      1
    67 0xf2ff5c8fbebc6049bbcf7dcd857171e54f8824cf      1
    68 0xfcc3a7c8e7029f38e0109088c2a90ffa1518bde9      1
    69 0xfe49a85e98941f1a115acd4beb98521023a25802      1

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

## Allow Artist Phase 2

``` r
c(allow_deekay_phase2) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 300 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0081ee36458fbd4dcecb31bbd25386522b1ef55d      1
      2 0x00a1b46319be9163b8ae30dbe506235608a563dc      1
      3 0x00de941478bce28995318726a7a727935105d5dd      1
      4 0x044c1d3882dd865ec1e136119ccba890e90892a6      1
      5 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      6 0x0513984b11b9ea9919cbf54e6ebdccc5f5dc38c4      1
      7 0x062a9c00bb4c9ddbfb128f2692917fbfdb64fb78      1
      8 0x0743882fa09a0c257b18d53dc1b101d3f32a04e5      1
      9 0x083d1316a2a86f24831f702b13d950e75327619c      1
     10 0x0a8172b6ba1287ba9e04b0cd560f69ed1c30943c      1
     11 0x0b4990b3ac5d1d00e32711d421543975ad7a9906      1
     12 0x0b7b36fd11da4f49dea97a90999201bcaf0120ec      1
     13 0x0bbe8e378891c70a693be79bc91461e621518f32      1
     14 0x0cc5428702c220cc4a68dfb6c83c6930f7270e4a      1
     15 0x0e1a2155eb0ce4a58c2d242be0efea0b09e18d26      1
     16 0x0f0ff0830e9ca533e521766439384a67b7a46337      1
     17 0x1043718731a245ae28b94413518ca9f48e8bf209      1
     18 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     19 0x131415a008df1869a279f781f2ddc6cee795de27      1
     20 0x138c1dc31a0c02a1f8c06159a502b3be690bbd42      1
     21 0x18302a9b14d7340f770fb85ca623f5a9a2269484      1
     22 0x18ac3c77ea0bdb50a9aa1ae3f3bbfd78e7afbe76      1
     23 0x18efe35763af417083dbab782b546503b5f684ae      1
     24 0x1986521f6caf7e3dbeab041e0578c62da5dd11af      1
     25 0x1a7d7e6abea492e7b9a3cae667f468c72e884671      1
     26 0x1aae1bc09e1785c8cb00650db0c6015bd73b0c7e      1
     27 0x1ae24fc2f4f1668f57fd15a98d6868b291046958      1
     28 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     29 0x1da6ab38e41e187348d773fed15bf547b69b7602      1
     30 0x1dc301854bfecb4ba03eb417579585c2b8ea12c1      1
     31 0x1fd8df37b360d7366db649703014e637ee214f4a      1
     32 0x20be12a62c45311c639b56308c5101e4f5353c4f      1
     33 0x2112134feff010321a1692f3bda6c9373552306f      1
     34 0x2132dfe9c2053fe3b6ed8b5966c3f25002c15deb      1
     35 0x2139f84156d1f9cae3bc7d67c856402049d99860      1
     36 0x215d69f03907b99f31cf44b2d1fa5e97d085d1e0      1
     37 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     38 0x223f6b0e03d1e78e8750122053b508e50426ecfc      1
     39 0x23099e3c2b7b8736ca8c428f01c7d2a6b851ed14      1
     40 0x24a082b65f5c6327691d3c3215a52f9a06dd7fd2      1
     41 0x25a19272971f6e681f9b1ea682cc3a70f6d10022      1
     42 0x25d50ce5c3c2962b1eb22963070432c8ace816b0      1
     43 0x265a25670a194df9ff63c848827455da74042087      1
     44 0x270e171dc5a7a0f19cc4c0bc5ad7a0eeb5b8147d      1
     45 0x27109b29155b48865e7bd1b24742d11ca2b5d1d3      1
     46 0x273ff74b03f188715330e9b4d59942334c17e88b      1
     47 0x287b1574aff39811c0b38fa24aafa7f0dfc1dbd1      1
     48 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     49 0x28d8e6dc0da65e2c342d6ba5123fdf9e5ddd94ad      1
     50 0x296e0c21db4061ebf971e55d5db85011e7ff9797      1
     51 0x298009639ccf901007afcd2afc5c366a1241e3c3      1
     52 0x29b79ad1730dc87b5923d47554436501b5d05b84      1
     53 0x2a193336b79d9462bb36215210d44e9d60878c65      1
     54 0x2b1b3274132d14d3ba4469a9ef7a5152d13a506e      1
     55 0x2b2b887e31133a6a56a973c8d0436bc9ba09fc0b      1
     56 0x2b91f332616084654098d4c13ceb9e82c8b73848      1
     57 0x2bb79da2016f6f2e3bc149621d344355a74eebcd      1
     58 0x2c18b21a23f03bfc0b667744389c4f79542b31a5      1
     59 0x2d3e27eabea0ecc21aaf37043f9b1ea29e453aba      1
     60 0x2d5e03d9064a55b96d34290a176656398df91379      1
     61 0x2e73e34c50607b8fdff70323fa3279f41a522957      1
     62 0x2fcc020f72e5d2edd2a24d04f3dc90d7fdfbd1dd      1
     63 0x30a15c08dc3e1ee8fe3e32f638b7ade6688f078e      1
     64 0x313bb17313280635add2bb58e0b172dc7fd2fc51      1
     65 0x31f5afe73e35de36ec0b99ab683883af4dfc8dce      1
     66 0x32542f2122bd78b3282993e9cfb4fbac01997bec      1
     67 0x32d29de590ec186ec5b28710e7659d5fb18419c0      1
     68 0x33b76f30630b88c33b5eb773c5db812972a1ec7f      1
     69 0x344a88c406fb9145b02f23bada4291b277b2296e      1
     70 0x348c991f2879bf58db6ddae337ab1181849b9d02      1
     71 0x34d31c441d418dc041a37907a6f32b68c0bf503d      1
     72 0x35c17dd9df9156ee74726e29f598dca529efac8d      1
     73 0x3673a15cf5566d422c15592a04944d91cd75409b      1
     74 0x36a1eccfeb099c0476a24df1ea86c8007bd7acbe      1
     75 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     76 0x373a8504a1e0230e08ce12de0623526403988dc2      1
     77 0x39067aeb2a223685bc606bc94545e53015468eef      1
     78 0x392b2feb8f01be3dc3fc72584a8185feb3e5f658      1
     79 0x3ac61447a75149ba67639100ecc92c76fb20941c      1
     80 0x3ad08954690e344b980bfebda171641be10878d0      1
     81 0x3bdf83a809bdb1a5aca5af1bdf72898d56947096      1
     82 0x3cb3cfe0c56215220d40d93353e0261e14ce9bcf      1
     83 0x3d1300afc4191e3fb88fa58faaccc7b3456db0da      1
     84 0x3db985313e41d676a65748c49ee07d17efbb45ca      1
     85 0x3de21e9cb7079500bdee959d5ec3ee8bda4edc95      1
     86 0x3f8ded07ff5c7a785f68970675262abdcc536429      1
     87 0x40f465f1ba4c2aba91c0c896cb92bbe4c7e545df      1
     88 0x42317577b5e29b8944c79755cd04cf876132d4cb      1
     89 0x45492c6bc6ab97e4320e6f844c560be62737a303      1
     90 0x46c7de8f81d756e0de13dff258ea17929fb20851      1
     91 0x46eb92137aa15771eae8ef8f472fc0a278363814      1
     92 0x471ccd726115991cbac31c8f724dd988fe05a3ac      1
     93 0x4799c8f127e627fd9a93ea0c09672f54e5d23429      1
     94 0x482b54622726e7a08500e6608a52809fcd00734a      1
     95 0x4845810f76acfa2c05cf9e31705cada2d6cc4131      1
     96 0x4961afb750673cf201a184291e9c5becbdc2f287      1
     97 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     98 0x4b6918e49e4d64170e79a971ae9ed2e7680e6970      1
     99 0x4ba81197da575acbbeef49e229de3bd2381c93c8      1
    100 0x4c8743c2bf58028e2c5cd7a1d1d7f8a9d0beebca      1
    101 0x4d3c1d8a0bdd27cf3c7d8f0eb27fd8842a6fc69f      1
    102 0x4d9b5696a8afed6b65f92b1b86ccf90c7561ae56      1
    103 0x4e7e21681735141da27f6c92066dfdc9f0d7152d      1
    104 0x4e8d918118e00f049712bb8da2b42088909eeee7      1
    105 0x5168895228e49c785d3d77a4621f076403da79a0      1
    106 0x52c6fbf9345c6c608a323847c3e3f2d3aa2451d0      1
    107 0x5316f1a2594388c989273c353865bc43111a9bd4      1
    108 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    109 0x533b9cdce87facf35c4066f48537660bf2bf16ad      1
    110 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    111 0x535950cfd696ff3e168324c423d3527304955621      1
    112 0x544ec526b8a4bb1f6e465f3c8d135d32c7a41db7      1
    113 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
    114 0x547263cb2ddea3e6f83a05c06ef0ddfcbef52930      1
    115 0x54f35e5464222ddece6e7230ac26fd51fa82f0f2      1
    116 0x56577f982e2066fec566108f0a63f3d09555c6ca      1
    117 0x565d4ed31234bc0562d30442ac79c266c93b9356      1
    118 0x56b834142933fe02f9a108a0d44aabf1157cf401      1
    119 0x57050cb9f0c74ba94216907806fc030a277f33e5      1
    120 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    121 0x57798ea2de5e864959a3f00a12f9224147edf7c8      1
    122 0x5b5433e9f7f5e5f6febef055e3d22d6779b15822      1
    123 0x5be9cc30208914e8e7b2612f47694245111c606a      1
    124 0x5c70af3ddc7e815a40f2a09d22483e6e24eddf8e      1
    125 0x5d88205569a0f01d1f607ecc964179c4afad4bac      1
    126 0x5e0e408d38ebd85b54caaebd9e01a4985b7a9099      1
    127 0x5f3efc431c1ed10d66146c18ab4c1246e676dec4      1
    128 0x5f4ab143df63665940a7c49406e0fde3f0f4558a      1
    129 0x60c4ae0ee854a20ea7796a9678090767679b30fc      1
    130 0x60d42d798395e51d7efdc2b590ead9fd6953d018      1
    131 0x61fd0d043d519f5a2bd05785000f30db96809429      1
    132 0x624943747ba3fc17734dfb0dc3fd9e5a436a16b3      1
    133 0x630430b225e1997afe7b8c7b99274b965b2d7015      1
    134 0x64072ae2095677ab7e8df63e1fbae89b6366d6b4      1
    135 0x655d51a80418f68168b565b48096ef10db695bc8      1
    136 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    137 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    138 0x69cb511fca7c6d3534b6dfa3bb4ac5446b63ef53      1
    139 0x6a3099dfeda083b31bef29adfdabc8d32dc96112      1
    140 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    141 0x6de871e6e9d6271c701f0dc031e3a4cd35681264      1
    142 0x6e08a7b853722ed42b43f85b626e242e7256842f      1
    143 0x6e6cba632af82d5acb547415f62ce4e1e9e1046a      1
    144 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    145 0x6f860367a8fc033d7f6f793859f2fa98cb93833e      1
    146 0x70f275bb99e26c8275dab9ac843535f8e130bc87      1
    147 0x720a4fab08cb746fc90e88d1924a98104c0822cf      1
    148 0x725a70b1847d1bf3fbb7dcaf43880a9343f308be      1
    149 0x738a1f6d79e592f726efaa3b8bef81797f408119      1
    150 0x74550e8a0dc9e16ff5d6b251a23c7d9cde912ba3      1
    151 0x74e2560995ba0b3e27539cea74d40b784f54689c      1
    152 0x79c5146bd916944b4f4aee4c2447644be2b78e0f      1
    153 0x7a85da43cd10b4c86a233e9a6d9a508e00dcf5a3      1
    154 0x7ac0ccc92148214123e1cdb884c3a8cedab5852f      1
    155 0x7bb09046c5835b3e7ee83c3fddb51b0ac51bae7c      1
    156 0x7bb84017b5bf09e2a821f59b3d5e1afc6879e1af      1
    157 0x7d77bec48a3a58a5e024d3ec3d0339f856afb0f0      1
    158 0x7d981acb5fa854de28504c87e72b4e91c2035364      1
    159 0x7e7216319b016c758ed6f792eff389291aa8040f      1
    160 0x7ed9cf5dbf2f68bd33ea2c95ef2b4634976ba7f8      1
    161 0x7f80ada1163b2f7eb8904f71b52059df69f250db      1
    162 0x80b30a661ca2e02878dc3161cfd67a37696f483c      1
    163 0x8222f50e4e01f519da7da1cbcdb3355efb8f9f0f      1
    164 0x82f912aaaf7551de526d5d67eec250aac0f862d5      1
    165 0x832518c074c50116b2f28bd4a00174e4eeb09797      1
    166 0x851109ac40639696ccab08d5af0a2b2a56803221      1
    167 0x85bc77fe4e423142dd32a2b3b097caaca4a03172      1
    168 0x85f199546c4f5acf90cd9ba4a333ae09f61af4f5      1
    169 0x87713711714df33bd21f0f4aeebbc05cdf876ced      1
    170 0x877966054785c393ef68b2c4bc45d5676915c7e2      1
    171 0x882bf217841b57d7406618004611fc45ca9afd2e      1
    172 0x883aa26b9a36c5f4d277378b1ee585585c2f23aa      1
    173 0x88d8fabbb70d3854b7be95a0237d957cbdea24c5      1
    174 0x898753a7555e911557f0b5cefca87e07cd3cc5c7      1
    175 0x8aef4c22edb65a0c3fb766283f781b80a8baadb6      1
    176 0x8b7d031fb0d8aab5a985583c6c56c4fffab27ee7      1
    177 0x91407e2da300e0a8257d7f6669f60ae3e44185b2      1
    178 0x91bde5e5546a78624d80fe311dc2cd43f045a25a      1
    179 0x9330d4d458c9c4942832d685f24bd319ac8c89d3      1
    180 0x93d849a31acea594a116b760bb6dd71bde536970      1
    181 0x9424116b9d61d04b678c5e5eddf8499f88ed9ade      1
    182 0x945e5da00ff55feda8c4ad9b8cda225d014e219a      1
    183 0x94b08f4daf6b0b1501fe4297280ec99412f66030      1
    184 0x95e9c4ef3c60038c9626e4116fc2ae52156b3692      1
    185 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
    186 0x97460ff37eebc62e9e1d9237e9f3501766fbc2bc      1
    187 0x97c0e8fe684fc1c5e050037c2bd5d48e62edcb5e      1
    188 0x97f48785bc7735e4513c0e2f2a62578e5ace8431      1
    189 0x98bcfb12ac9f2fa7cb0ec582b3ac04f7ac548a25      1
    190 0x9c232158b5e0f9a23bf9a399cd9197c7f131d13b      1
    191 0x9d2579cf50fea00aa7136b5f3d1feae49074c7bc      1
    192 0x9d2a41b4fc8dec34bf6bfed31e58e101bef939f7      1
    193 0x9ef01e6681e2be12f26e30504fc6250542de2c84      1
    194 0x9fc10fa9a7a47eac3bd17e50f2b8534c88a127f8      1
    195 0x9fe176816f90145f8aed9ffdedee158ed8423ba2      1
    196 0xa14c5189b6eed2657365232f23265e0d24601c10      1
    197 0xa18e9f2f9df9acb358579662be21a5ef3e9e3b1c      1
    198 0xa1b321dfb73c1bf9aff183c412f53efa07a7583a      1
    199 0xa266e8b5c812c6a9d63b8862599f2963d19a797e      1
    200 0xa2a6feb8fb3720a625e122e6d3102638c1d36b29      1
    201 0xa5a0b7c3dd5dddbfbd51e56b9170bb6d1253788b      1
    202 0xa7a7bdd730d69ac2d3215703faf03e1a5c600e8b      1
    203 0xa88218ce58baf8074333ec4845d79383e9417d88      1
    204 0xab6ca2017548a170699890214bfd66583a0c1754      1
    205 0xac8bc7f417790c13402fb10822c743f1266bba8d      1
    206 0xad1fe7a34aebe62c3ffba1f863107f44eeef6b5d      1
    207 0xad37805e0a724e2c334acf49d255a0a35b76fe7c      1
    208 0xad442616b04cac445ecf7873989f8a22c276b78b      1
    209 0xae08bc16f9afb623efe894147dc36ed0eeb5cdb4      1
    210 0xafbf455574c05b0d9874e11775492990bf8abd01      1
    211 0xafffa762da1e32146197992d11a6f170f9bdced8      1
    212 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
    213 0xb254430db394b9f1fe742e7f27dcb162a3aa48f3      1
    214 0xb30f83204890634740dc0238590c879a8d66cfc3      1
    215 0xb395d307b06b9a2f6b665065cdaf870ea4c372cc      1
    216 0xb3c6e32f46ebc84f3e9b39bbd3d96c359129cb5c      1
    217 0xb3c95418ffab63a4a6643e159047283ce500a6fc      1
    218 0xb4b83999eb13d4a887c0da1794e5ff62fabf7737      1
    219 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    220 0xb7c2f269beb66d75a118f9aa96a2b3181e5895a4      1
    221 0xbbd43b1b7028a60012f6a92f5c795055b8ec2fff      1
    222 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    223 0xbcb52bdeb935776c98b44428641cbad6c529aa73      1
    224 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    225 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    226 0xbe03ab0c13af2fd997b437425f0061c046f18f1a      1
    227 0xbfd06c5485b837a620df5e93ceb7b61b17cc2e04      1
    228 0xc002066ff909b98b61bbf842301cc04cb7a300eb      1
    229 0xc02f533a819c4d3149544dd1a55cf7cc87a8d30b      1
    230 0xc142995f05436c91bba172b7d17522a301323236      1
    231 0xc1855da46c382600e1571484d0880d4967e85aee      1
    232 0xc1eca986c20103f3a180d797f5be64702bae7b22      1
    233 0xc217bd12bf5fb14a5e10450eebf34890092d3cb5      1
    234 0xc2a4d3ffc69391f67cd06c62813b9d8117019797      1
    235 0xc2b35534e47cdf3d787e01630129631d8270abfb      1
    236 0xc2ec556af69fc6d7d708f1d111d5860583982d5b      1
    237 0xc4a5b26040c95186e37fe8e1b262026d4f550c86      1
    238 0xc93445359baaa7b0a384d4b47cf5d8056b5933f4      1
    239 0xc9d13bdcf27b24569d02d28411a9f20852fa8a0a      1
    240 0xca177d4b3e6d4c2778912b47e6aae49b7c429819      1
    241 0xca95cb22706e10c24b72210c818366d4023a82ee      1
    242 0xcb53d670bf107e459d52d08d66a89b597ea26c80      1
    243 0xcb93531eb1e872cb98bf8118c310d06475a19e60      1
    244 0xcbbd4a84b34c97af74dffa07927590f7ec3be972      1
    245 0xcd3c0b92dd15b1166cc51d3b969fed57ad2baab6      1
    246 0xcd6034eb0f8f5c8b0ea954ea64f5b1f10f4d9bde      1
    247 0xcd8f9e6a934f56361f111f0c0441e790104581bc      1
    248 0xce1199cea54c3f253389c04b9cc13df920842534      1
    249 0xcfac7a1765f45404be1b384739802f2ddbb80954      1
    250 0xd02cf646520827d74d4ff0435f96220819772b4b      1
    251 0xd1f2c10b65a8a19bd1264487cf005c1f6e68b1fb      1
    252 0xd2e85e0d26c7e15fc6a5d8f86618471bd3f1caa2      1
    253 0xd34d42d1335ab3ce12c8dc85ebe21cf1fcdefed8      1
    254 0xd4e55c0f693317b728acb84820a81a330da0cccb      1
    255 0xd5ec0e1673ba50aa0b5c437d2cbbadc99ba92fd8      1
    256 0xd61bee56435dc7eeca895bae90fc8b9c7fe709eb      1
    257 0xd889eba3db46702d2e4cfe6d94229db2ba188494      1
    258 0xd926c3dc7feca623ab94680082c6882c9783cdd7      1
    259 0xda87387debb614b2d76e7d2a713d0affa22ee077      1
    260 0xdac6a6bf61f10d57d7156348a5400ed0b9aa1fb3      1
    261 0xdb0bfe41e295af126f91a7e2e16e31945b792fdb      1
    262 0xdd995b6d40fa69a7d624f3aabcbbe1e7fcb0ce78      1
    263 0xde05d2725469465b330d2242729869f45139f5ad      1
    264 0xde3e1da19ea0c90fcf70b5f1a8d2fd7fce7151e4      1
    265 0xde8b953eafd05c63d12547bce980117267f38215      1
    266 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    267 0xdffc9d6970c910d35818e7ce4964bb5ae546792b      1
    268 0xe055631ad65d54a8b42c6dc21d9556d308aea9a9      1
    269 0xe1385ea3cd4aeb508b2b8094f822960d0c968505      1
    270 0xe395257d64bce9e1795cc59dd2a625930f15d28b      1
    271 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    272 0xe50b97b7578454925b65c150e00935ab2866710e      1
    273 0xe6ff1989f68b6fd95b3b9f966d32c9e7d96e6255      1
    274 0xe900669436005e7c2f0467483a093d756ce58384      1
    275 0xe9c4e2f7bbfe363129c2a18d8dabdd5c9b649d3e      1
    276 0xeb43deccb22674abde57bd12e88ff7011630cba7      1
    277 0xeb7921cda81164f7e711ac0bec1a9243fd10a815      1
    278 0xec375abd130a6aa8fc37f4a0ea9c9ab3716019ec      1
    279 0xee097bd75c6299dc12fd91e31ec10b797d572747      1
    280 0xee153be6cd5b58f3f72817bc168b7fc4c5ba6a82      1
    281 0xf02a629c0a74f2fa653f83770f3de2e113f8a602      1
    282 0xf045e55802a8f0aead20d6f537bff4fd9fc354b2      1
    283 0xf04a957fd3f9424804df1bd7333f5653a71f1391      1
    284 0xf1c2843c4b2b40553263813e3a6014b305598ecd      1
    285 0xf1f20ddcc34f4781f0260db41578371df9997f83      1
    286 0xf3c717413ea5cf5eb29b00e644df1a582ae877db      1
    287 0xf424f275b7d3310ead34fd43fdff4be087e6a4be      1
    288 0xf4d1f6d4a2dfc9a70c119ce0c69d4f5fea073067      1
    289 0xf53efc8469285437acb24a650463a5afb50dd800      1
    290 0xf89c94f43b36719046b55e2ae60bacbfc0db1c6a      1
    291 0xf8c75c5e9ec6875c57c0dbc30b59934b37908c4e      1
    292 0xf98539a9c388dcdc153bcefb6a7cfc427133e0ff      1
    293 0xfa621bbc0e434c400aa45d631b9bb97be11e44c3      1
    294 0xfb666c44fc6fbf119f710d2197a4256c3c832643      1
    295 0xfb8ba0706b03aee4274e8b60bc2de5a6a177ffa4      1
    296 0xfe2e3e999bc785bb053c85688da6daa4a19eb0f4      1
    297 0xfea72aef6da0e3c4d6001ceacbe04faa4613d663      1
    298 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1
    299 0xffb93dc622c18291bb8efa112764d8539f8c338c      1
    300 0xffcad6bba49e6a63489c078c3a4a9298ee21cfab      1

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
