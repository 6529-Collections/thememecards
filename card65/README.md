
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16589569.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:770         Length:770         Min.   :1   Length:770        
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:770        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16589969 # https://etherscan.io/block/16589969
block_hash <- "0x61e40b2ca4ddd7314872c5b411b8ad7ad07e2a816ac543978b30b53deaeda14f"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4767 

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
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=43,address_max=1)
airdrop_fvckrender    <- pick(snapshot, contracts=c("Fvckrender","Foundation","SuperRare","SuperRare2","NG","NG2","NG3","NG4","NG5","TimePieces"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_fvckrender      <- pick(snapshot, contracts=c("ProjectGhostEditions","BROKENEditions","BLOOMEDEditions","TOUCHEDEditions","FEELEditions","STILLEditions","MINDSTONEEditions","EGOSTONEEditions","DetachedEditions","FOCUSEditions","SPIRITSTONEEditions","SOULSTONEEditions","PRESSURESTONEEditions","PROTECTEditions","DISTANCEEditions","THOUGHTSEditions","DESTRUCTEditions","BreatheEditions","REPRESSEDEditions","UNANIMITYEditions","UNITYAGAINST","ECTOSYMBIOSISEditions","ENDOSYMBIOTICEditions","CYANOBIONTSEditions","COMPLETIONEditions","BALANCEEditions"), address_remove=address_remove, address_subtract=airdrop_fvckrender,address_max=1)
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
    1 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    2 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    3 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    4 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 43 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0936d312ba234451fa7a90f68c7ede0f4fa962c6      1
     2 0x13eee41d67b8d99e11174161d72cf8ccd194458c      1
     3 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     4 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     5 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     6 0x23602ca06e977c86339ffddad74966e824ab691e      1
     7 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     8 0x3876be5be4998adecbfbbad26604a762467e7f42      1
     9 0x388160a99390392278afdba240046b8b5e73f77b      1
    10 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    11 0x405020c797a64f155c9966c88e5c677b2dbca5ab      1
    12 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    13 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    14 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    15 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    16 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    17 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    18 0x64ca4373a93e25822dcfdca0306e899a29e92b3c      1
    19 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    20 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    21 0x6f566a6672f615c2975f6c48224c46153e12ffcf      1
    22 0x7525e71f51bda1fbc326000714d2fc68ed5aed6b      1
    23 0x7ed69ff055d746d451f675635e9bf773accc6a97      1
    24 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    25 0x81974c1a09e53c5129c6e4f74e547fda0adf4d2d      1
    26 0x82139687faae8a29851902783e02e699de0e0846      1
    27 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    28 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    29 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    30 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    31 0xadebdeab678647a457743ea3af98f8b804e45c24      1
    32 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    33 0xb692704d590e224c67fa96830c4b2526fccaf3a3      1
    34 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    35 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    36 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    37 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    38 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    39 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    40 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    41 0xd3364ea1c742cc2d396ac65e1723b8d608a939bb      1
    42 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    43 0xf1f476f144df01480297dca47efca565e8b0c9f1      1

## Airdrop Artist

``` r
c(airdrop_fvckrender) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 43 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00dcdd43bd25f78ae91c12f3c295dc11d3065e00      1
     2 0x03e97a7fe5325a1fe2315a9ba00db9a0eef09928      1
     3 0x0c54a765c77a88d9cda0bd7ad9b10422848d92ef      1
     4 0x0ef024d299cb56805f2437cd00b8a361a7b06d54      1
     5 0x1d1c9dad9a24b1e9324605153906d584520b8e93      1
     6 0x1dcdda173ef0405e7ff3bb2433b58ce1aa92eccc      1
     7 0x21301d901db04724597d1b6012ac49878157580d      1
     8 0x2585b94bd758107e6d1698a0786e84efabc36882      1
     9 0x33914463964f994f093bfbaef4db18110dad60d7      1
    10 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    11 0x46d235c84bd217d19ad0eeaf644012dda307910c      1
    12 0x4ef33f32c39796ae02c3ec21656ace134bab73e9      1
    13 0x54ace8d119c4bd7baf11f750c690e9667a80801a      1
    14 0x550e970e31a45b06df01a00b1c89a478d4d5e00a      1
    15 0x580a96bc816c2324bdff5eb2a7e159ae7ee63022      1
    16 0x5b93ff82faaf241c15997ea3975419dddd8362c5      1
    17 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
    18 0x720a4fab08cb746fc90e88d1924a98104c0822cf      1
    19 0x75d3bd82c08c7933f3d2b7b79c4e4208e229a0d6      1
    20 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    21 0x8e63e4376dde62cb82f5a91c4bce4156457dedb1      1
    22 0x94496ccf856a0a3d4c0fc42a676e0c89ca8b4b50      1
    23 0x94531d647c78bddc26915e74c2ee8e6aa04e18b3      1
    24 0x96d2b5bfa179ce0979fef08ca7ecec497b050b75      1
    25 0x989a5d65720e62d80299747e5a95024ff07732ff      1
    26 0x99d3aa3010b1d90d0c2f4c12a07ab6755b07bba5      1
    27 0x9bb524bccce50c6a3606cecd80cfaa38d7d4fc26      1
    28 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    29 0xa0df120104cc8796af1377b931b30279b2a01ae8      1
    30 0xab6ca2017548a170699890214bfd66583a0c1754      1
    31 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    32 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    33 0xbea73d090461d4d7d06c30c12612b5b5219c69e1      1
    34 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    35 0xcca3fe904b6a635fa3c4872e3ab2e60acf8b3c00      1
    36 0xd1e8a92f44bdc83bc620c56a7913fd97de5abe10      1
    37 0xd7cf63d3a136de2fada021f4779e9200522b8f33      1
    38 0xe34308ce308066b1419cf18992fd068b8d5b7500      1
    39 0xe4550aed6a6237ee253e022c94455e417a4c9375      1
    40 0xe50f359b5bc6dac1d5eb19e1b2fde2cf322d6c02      1
    41 0xe5fccbbc11e4a36a77e9d8e47a9b28ec44e2e2ff      1
    42 0xeee5eb24e7a0ea53b75a1b9ad72e7d20562f4283      1
    43 0xf251b5d633b3ef46307653bcf04c8209cde7d8be      1

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
     7 0x1566ae673ae80725bcce901b486c336e6acef465      1
     8 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     9 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    10 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    11 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    12 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    13 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    14 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    15 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    16 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    17 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    18 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    19 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    20 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    21 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    22 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    23 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    24 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    25 0x59068075a799594db03c0255eed68e8e121155c8      1
    26 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    29 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    30 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    31 0x69e68074f1aada957edd39c5eae0069973343f30      1
    32 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    33 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    34 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    35 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    36 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    37 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    38 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    39 0x82139687faae8a29851902783e02e699de0e0846      1
    40 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    41 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    42 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    43 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    44 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    45 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    46 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    47 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    48 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    49 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    50 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    51 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    52 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
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
    70 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    71 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
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

## Allow Artist

``` r
c(allow_fvckrender) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 382 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0055cd5f017027d10adf4f13332181e6d8d886bb      1
      2 0x005fd30b00f66d0d016e3512d03464a1d237c154      1
      3 0x007dbed1b4a125c45df88f3ffa350ff70c94dd9f      1
      4 0x027cae2ed1a23350a751452e907b4120330f9762      1
      5 0x03c402888ae76df943e863e5a9c534ad88f09669      1
      6 0x0403542a67116b628fe61beb3acb9262935af0a0      1
      7 0x055f86a0aaf702d7324076ae9c5b9aa203204ccd      1
      8 0x06a7718758a787b93b1794e93d8506a27a67eaf2      1
      9 0x08cc394e9a690e7ca60d57072876cecd3cf4ed32      1
     10 0x0ac2185374664768ac7c44f9674a3c82ab31ce67      1
     11 0x0ae4fc3dbc0fcce25f099cc1cf5c646e4e9570f0      1
     12 0x0e1ddf0fb2219fab7a2f79ee39b7ea93b1708066      1
     13 0x0e50da617197baad40804c95c25a8b0172d25d5f      1
     14 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     15 0x0f1125c1835089bfc915d4c549b027169a43d5a6      1
     16 0x0f3ed7a3519dc3b3f4a80d0922c300875a527a60      1
     17 0x0f4c06751b61ce8ad54c480715dd9f27b80f2a9e      1
     18 0x108289ecb4830e7de08dc8f9e58526ddeccd2d32      1
     19 0x10dd6302c3ea6dc387fbf23fb1fb972a2b80b656      1
     20 0x1244d96845d94b3d1a550110a5c97ef018f4d24e      1
     21 0x13e8810a25171327c55a2903ff00704f27c0c8a4      1
     22 0x13f5dd0023aeeec1cd32f5892973065512f30139      1
     23 0x157be16daa4498b555654e1705eaec7dee9970c4      1
     24 0x177d902f15c55b4cd40b294836b846f40694b4d9      1
     25 0x180f71c2bbcbe786d8b3a3b07c9378e467c4b5c1      1
     26 0x19925d32ea737de44562b6b6f6daef9b257d5eb2      1
     27 0x199d68f140c3dec500f80b0198ca7ef55d5e587f      1
     28 0x1ad4fb4852d257987fd43613febe787f5535af47      1
     29 0x1bc26c75c728b711228677b2ad8219ed24f05c7a      1
     30 0x1bcf4ba23b16d851402a950ea138295f97445a83      1
     31 0x1d4752ec4aaaf60d0d6817fed1d2784fe2080218      1
     32 0x1da6ab38e41e187348d773fed15bf547b69b7602      1
     33 0x1e3259caf9679c86c2b0b808432c64f33359baad      1
     34 0x1e3610c561c32edc58ee6da2a22463dc81001340      1
     35 0x1e506e6af1dcf5633e105ad36fcfe0ac83dce013      1
     36 0x2082c5a3c08a4d5ade0ce779e24b463daa241646      1
     37 0x20ffedc61d2f70f70d643096f6b4abc209726d93      1
     38 0x2289c8fed5be4b1c4fcfd7a74ad2afcdd277312f      1
     39 0x22a1789dd5ee19151356a05cd715d73081d833f8      1
     40 0x22a993fe21369ff202ad16b802d5612f221390b0      1
     41 0x2307482c97ffa776ab7a391a707889e15d62dd98      1
     42 0x2404a65cc0ee26a4efc82185e2176d4ac029c35c      1
     43 0x249e16a08396618bfd45b02aa4eea28ad8397873      1
     44 0x259ae15e3b07e051ddef0b76d06f136e0bc24aad      1
     45 0x27fe0ff8c2ebb9e7893bd0515def9e08f993a7b9      1
     46 0x285f7404221d57a756f3b690e636d6f51ea534e1      1
     47 0x2871fd6c37dbbcaad37b420175bf7c5f8251b238      1
     48 0x28c7400877f6d012b79a6b85297204a73d388335      1
     49 0x28d8e6dc0da65e2c342d6ba5123fdf9e5ddd94ad      1
     50 0x291b7521ff79b6a96f5b88b9347a4fca113a1fc2      1
     51 0x29b54625d1ac801d995aa328881592548395518b      1
     52 0x2ab77da61e4a60841a7e66f0e9595a6104ac4cad      1
     53 0x2b511e25e858641654261fea1654b2ab4915ac2f      1
     54 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
     55 0x2cb96a47ad05d41b61d5a103c6ca68ec38c268c0      1
     56 0x307c13a6360994d4f0a94a3e5c767d59d7d2c0c2      1
     57 0x314787d7ac1efc6eb28fb3f1a5aba95e136bcb2b      1
     58 0x33389d09b2e267918e645f56e8ea41a1a2ad97c3      1
     59 0x34978faf3a9f469da7248d1365ddf69ac099588c      1
     60 0x3504c444b87a6cd15f4fe92ba04bfb3f91d6fbcb      1
     61 0x350db95720043fafc4bbe07fa07ede717facf8e5      1
     62 0x352216a5b428fbee3dc882a2c268a1669fb8b2a5      1
     63 0x363e89408093719f67b7a674b74006989442116a      1
     64 0x373b75e699aac3523ddf7a7f31f6b29b25b69f6c      1
     65 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     66 0x3837bed4055a7e0d9b1f180f038786e2e6055d8b      1
     67 0x388c474ef97367c1da8a9b0526569a0a4d463f97      1
     68 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     69 0x39a7fb978b1f43219f096d7eb0836fd08cd74e15      1
     70 0x39b9ac2c5ce2a5ccad6c7b338ba5d2a96f61d4d4      1
     71 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
     72 0x3c0fbce46ecc02f24e3916b368366300a1848ce4      1
     73 0x3c33267b63dfb408edf7ef453c1c5a4b0d269492      1
     74 0x3cdee27bc42f4efa1cc2803942392b1ad777e5dd      1
     75 0x3db0b224099e56488d93f4e887a6bad88c39cfa4      1
     76 0x3e4cbc326d457fab95fd052122c4642e57fb6ce4      1
     77 0x40d775827365ae4d54cbc08a1a1c4f586b2c1d0a      1
     78 0x413c3531f3d5a9248d031b4b5cb2de7b1ba8b7f4      1
     79 0x4176ea7e8cf0e006a02bc9f50689394d02b33134      1
     80 0x42d3b1e30f39191f6da2701e31cfc82574ea14d5      1
     81 0x42d5ec165226b750174bab221cfdf5d347d5bd54      1
     82 0x42ea056920e02c2e046596d20075316a18abadc0      1
     83 0x44340f7dc53bf90363e503350bbedf69e2d7870c      1
     84 0x45821aa4d9c538595d8f11022929a6d7dd1bc6c2      1
     85 0x45de56ecf71d7b76b304dde350d34e9abc7368ca      1
     86 0x45f2e08b35a959a87833c57c4bacbf58f95eefb7      1
     87 0x468d80e1a1433c18256d00331efdc9cb7d193e9d      1
     88 0x46ae5b30edfde7a6f83285f8ae98a2e97c9445dd      1
     89 0x46b06185c9039ab046dbed25cf261b8410912b74      1
     90 0x4786390c6d8b889139510a6a3f851186f3e67be5      1
     91 0x49ac01958bceb1cdff62c3e9f9f76d17fb2294b4      1
     92 0x49f9c2be1aa71afac6f2f2ce5b884d8cda10e26d      1
     93 0x4a39ae58b605102913ac19b7c071da75b55b2674      1
     94 0x4a48b46971002c8dc66bf904405b8c4b3c919d57      1
     95 0x4a6eacd00e3a586550bb3b52731d51ce7f53f490      1
     96 0x4c1e900cb9083329fc930b9eb8fddc1905f197b5      1
     97 0x4ca6b786b406e9cd072acdacad96c707691fe795      1
     98 0x4cbc3966088d27859423c42cf2c00ff40ac0261d      1
     99 0x4d218086b6d5336a492dcaa31c0ba235b0ec4082      1
    100 0x4eb67ff5dfffe72a6388b2fa4916a796e3bc4b1d      1
    101 0x4f3f4a69670fd49ff6f425687feb3cd0197b0098      1
    102 0x50b88f3f1ea8948731c6af4c0ebfdef1beccd6fa      1
    103 0x50dd57f50a17d57304e7a4f262da30beb31c2e87      1
    104 0x50e33cfe14b13ca02d9ba855c7deeae92ff541fa      1
    105 0x5170ccf3f96c73aaa1f560912e91049ca77aade6      1
    106 0x518b870520476e08ee523a285e9255b443883885      1
    107 0x51e957a31b37b7d213922146e8cfd095c3013f71      1
    108 0x5328cd117d42fbeb105794b5cd994df420065462      1
    109 0x53c1b80ec35cbca46bb2fafd37e42c3b026f8c38      1
    110 0x53e2cace204e83783ebb59a48e874651c2102ca1      1
    111 0x54151a569e348600b155b8b5a7884e14df50e5e9      1
    112 0x54429ffb6389759a04b00879abf86a6ddbcd5a47      1
    113 0x547a0cac52630a93fa7e4c2946ee256dbf471839      1
    114 0x54f35e5464222ddece6e7230ac26fd51fa82f0f2      1
    115 0x5519800061bb5136b31d90d166f39769d1a937f3      1
    116 0x568cebf022cfe147e79040046c136be4810391b2      1
    117 0x57115f7d04c16f3983802b7c5a2d5089a188d76a      1
    118 0x5712ec69b1dedf934303ce530ae2b4f1d3ca4c61      1
    119 0x577ff1df8c33f95c6180bcd7b56251a9d1f3422c      1
    120 0x57a4c7c267afdf07bc8c9f8faa46a43ad5ccb688      1
    121 0x57ae265eab27498113b6d0bd3e63ad9ce36512b0      1
    122 0x57b42117029b9e806e0e0d1edaa3e68a8007d032      1
    123 0x57b7acbe2367b3192f826ca679910a9c059c431e      1
    124 0x58791b7d2cfc8310f7d2032b99b3e9dffaae4f17      1
    125 0x591075793897954b28efda7a3683312c1e0a7376      1
    126 0x5940284fd7971d8109e7ff8598973cd295acd6c6      1
    127 0x5a418d8bc0c074a4a8fa88d1322dc51cc1cb9d29      1
    128 0x5b4bb3c2bb6e2ec707cc39a86dd398c0a9f69add      1
    129 0x5b67a5c825f2eb9a6a0abde5f4e3dd46b722d431      1
    130 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    131 0x5c544509cc551b7d4f7ca44eb26e4e896b6a3533      1
    132 0x5d080b5d2718bd65678c9582519eb876c3042511      1
    133 0x5d25ba9f6696740bbc96e16a005245f29b9da7fe      1
    134 0x5d7dcb9f59d4e1cf96463a72e866966149df1552      1
    135 0x5fbe0ae423f1767028f150aa92545267507588ef      1
    136 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    137 0x60cc0349d1151af21fd25f608429c63c6b1a9448      1
    138 0x632e28fffc28c54700747207ff4a4472a78f2a42      1
    139 0x64402d5d43fc3faec310c7063e4a64b35e1f29ed      1
    140 0x6476842e131a467632fcf6165db205793df0fc4f      1
    141 0x650ee363324d54b93d1b155e507039f3df31ca15      1
    142 0x65657e65292c3dfd4c67bcec2d22fc44de87702e      1
    143 0x6728ecda881529b46eeb630baa30dd33dc76b070      1
    144 0x679d5162bad71990abca0f18095948c12a2756b0      1
    145 0x67ba5775a60b13254bf65ea67295dd47543bc5e1      1
    146 0x67fe4daf0242df7f03d504e84d4195a15b017d29      1
    147 0x68d814e8bcc971df740fa8b5610c290284012781      1
    148 0x692d1fce318f98865203a256b1024e070295c6f4      1
    149 0x6a654dc02112807f3db4705be69b3a6773dd2745      1
    150 0x6c2d65145b14978a95d14c8ddc5415d9fc447910      1
    151 0x6c3d6d6038b40854b811b4dc8310c04aa68310c9      1
    152 0x6cc4774cf4d4c738e3310f1b210c6ffe23d93999      1
    153 0x6eff21c2bfdf83a33aa690fe3db17fc672244907      1
    154 0x6f4152b9eb3b626baea2f549c387c5bc31d857cb      1
    155 0x71370779681d53660ad77b2d91661f8bf4d3cceb      1
    156 0x71784fb96ba10ffe0a43f5cdce018ebe9de7fd0a      1
    157 0x71bde65c9ac549a12ca85410a4a2f0e669f109e3      1
    158 0x72f1fccc677dc68b97aca7e0ce3b44ffb79bf968      1
    159 0x7341158387c33247d7afff2e06d1ae8f5d114c85      1
    160 0x75f826a21b1d410b2b6523202de6238057c6e046      1
    161 0x768e0d151be7d60af09bcd74983bf3e1e6ee5de5      1
    162 0x76ac7d9f489b8f3126eba43c4e2f29828b835a3a      1
    163 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    164 0x77424437e320fc70ab04d983e259ca6e6e205c86      1
    165 0x78c64ff0d6f3a3419688bb2624231a43e842365e      1
    166 0x794f0e25c5910cb3f8a82c9fb9cb44bc069674f0      1
    167 0x795128b6bf286c30c4334e776b03058cc70564b5      1
    168 0x7971e007a4e4d4dc1f8380f5d91d3f52b5e53461      1
    169 0x7a44d904e5372f64db51436fcdf4024a8144c383      1
    170 0x7a7939e041ae6c38920b4b21383a06676d49b2b4      1
    171 0x7b394bade7178f569d0cd7895c7d679fa54c552b      1
    172 0x7b3e4d6d044ec5d286e9ebd1f6984853229143c8      1
    173 0x7c1958ba95ab3170f6069dadf4de304b0c00000c      1
    174 0x7c3565d658a71526afd6f0e268faff2fe401623a      1
    175 0x7c46ce5624c44a7489e357776ba2a9f7b868e22b      1
    176 0x7cdd6e556c8c3d2e64f9a3ce6eed1cf76147de3b      1
    177 0x7d5f4e7568068aa7251f4ff87b3a1ef3d1ef3952      1
    178 0x7d81a77cff9862ab62009748c24e903bc1317f72      1
    179 0x7e804cd0da5df489ee9b27ec6aab8d09d9ae8f32      1
    180 0x7eca4f3d197da34c5761fc707326ef29935bc4d3      1
    181 0x7ef0729e3999a5a4645ab78a80cf72d3631a0f05      1
    182 0x7f6b206b33b1ddfab68e3c3d5f310229b5e78152      1
    183 0x7fcfc832ea3ea6aa66fffe80747c67b475326f45      1
    184 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
    185 0x8240aee96840bb6c884402bec4682831d81616f8      1
    186 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    187 0x8481be8cf9d472ee513aaa850702ee37fe27c063      1
    188 0x84a3200d4d3129d84433c4789db1e3d8ddddfb70      1
    189 0x84a91de94155a7001bf92685d04a12e653d94bdb      1
    190 0x8528effbe7cdcfc9b6f9bc915c67007a09df42d0      1
    191 0x852a1ea52bcd4384e3df995746c687e32e38831e      1
    192 0x8548b2b606c91da29964460e68677b7a4cc21396      1
    193 0x856d1b7cdb66e29938cdddf11293904802ce451b      1
    194 0x86d48f73010ca7c65ec027ea566cf67b0db6404e      1
    195 0x86fcc02d9d194ffa836473cd6554330feb11fb13      1
    196 0x86fe9b4a3055994fb38175ef7bf3ecf88d0608d2      1
    197 0x87885aaeeded51c7e3858a782644f5d89759f245      1
    198 0x89d3f76c7745babde1f66364f8b80b6169cbec0c      1
    199 0x8a867ff9e8123b9d44f233a21d64d3f751bdc8a6      1
    200 0x8abc6546a2895c27d2165776931c1276c258e903      1
    201 0x8afe2f0b6f3f0bad814e4d37658f0eb5bb7bf8db      1
    202 0x8b9ed9fa64c74d49cc57d038b16a85004938bdd0      1
    203 0x8c050d434a8c64f516d00a1c2baa5c1f929ca064      1
    204 0x8c8a36f5b618c42fcdfc1a74222af201557dbed4      1
    205 0x8cc426d9c78143e0655be3c934a4dde3c6c51941      1
    206 0x8d5b98af63fe6b3b1220bf7fe24643681c2d3bbe      1
    207 0x8d99fbf1fe6088bddddcb7ef2782318303761519      1
    208 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
    209 0x8e4e2cb1f2538878c60923bbf2964a2e89cf8a52      1
    210 0x91073ecdbd064b9bddc2fe6806a97679d5fa2220      1
    211 0x9127c9221b22ea3789c90383284c72dcd7d9b9fb      1
    212 0x912e2183aee79754c4a633496b8f739c2ae281d5      1
    213 0x91360cbcfa38864930288333d0d04455f90ba1d3      1
    214 0x9150df052c9ce5a68fb3a0028a657a274ab43258      1
    215 0x915d22b27a3f8fad1add9e93886d1d7515542579      1
    216 0x9191f7e24faff57e2e61ee7928bc91a8055ea119      1
    217 0x94042981fbe827237acf0d49bbb51fa8c1c56aa1      1
    218 0x95d4ecab6e08041b76dc488d6bec5c92f833b9a8      1
    219 0x962cdf563724d6d0ed6e8518c4c1da334f004b18      1
    220 0x965d294c687814bc00264b69fbed5c091ca0f580      1
    221 0x96b68ad7925ba82f59a7eceb616fc8701f0ecf59      1
    222 0x97527b1e9e410aa8cef7e226052cb0e8f8f495df      1
    223 0x9769334fc882775f4951865aa473481880669d47      1
    224 0x977d3dbf93174f517a52736e1e556b79300ce3cc      1
    225 0x97ab88faa19480346719ad6521f7ddb2cedd9e31      1
    226 0x97de65d1757ce193f77e5aa0b9c2437a5ebabe45      1
    227 0x988070d1060fd2c826ccbc9b1446296a96fa75ad      1
    228 0x9983d09b9e3741c0cd1f11ccae41fca1745efd2c      1
    229 0x9b423d446eb59e7398c1c968dd1c87ad78faf9d0      1
    230 0x9b4b7282e4838273c79cca63c6c03a2df5ee4286      1
    231 0x9b6d720354b24a47ed44d159a77accbb059dcf9e      1
    232 0x9b8d12fb2045d6f5d021e7e1f45e51296df6f060      1
    233 0x9c0031fe523e9f080b51bb18920c7be46d173046      1
    234 0x9cb79c20939d6ac3143910926754fbc67840be8a      1
    235 0x9ce9cbfd3ded8b0597ba6d750557f21bd2713c6f      1
    236 0x9d2dacbd15f1d2f14ed36c4b6e374b2d496d93ff      1
    237 0x9dabb5e4a4936633586d5886ac0c6eb224600b90      1
    238 0x9e0e57de9cac0e9c489c080a0c07ff6e42ae12d1      1
    239 0x9e4e4bec255a59d2b07ce3bbec0afe5df00b4b45      1
    240 0x9e53563dbfea80b39bf683b7d52df449a2b247e7      1
    241 0x9ea0c4bb61096ade0638e52bf0241dea5c2424ad      1
    242 0x9ee5e3ff06425cf972e77c195f70ecb18ac23d7f      1
    243 0xa046df70bd1e5988d1e4fc9e5448cbbf9d518298      1
    244 0xa1c23a4d232dc6268038f273299d463787bde8c3      1
    245 0xa2197ed617b8b1427606f4b1c6c00155d8340e22      1
    246 0xa22414e4af5767448624017c7e0151d22490412b      1
    247 0xa45ef043c595e5da4a2bbb78ae47b88d24d90019      1
    248 0xa4656c50e0290627fbc37f87ec5f14a352716dbc      1
    249 0xa52578c6ada18248d95805083ed148957573e4eb      1
    250 0xa5b36a3732937375bc579fa38159347da9938441      1
    251 0xa68dce940c5bb9c601b9293f01aaf2d2db98e325      1
    252 0xa7c5bf2b18f45bd9bf2bb7e97d4b34be20bbd4e3      1
    253 0xa80921ae974f20cd8f74bcfe6601226435ad705d      1
    254 0xa81134646bf68807e16e80dc9778a314ed1a9747      1
    255 0xa83fa9c834bb5abe25f9c56eb5845b3203df94f2      1
    256 0xa99bac549060716a6f45c16b145a614b78f7b3fc      1
    257 0xa9cfc982a66d0134fee2539cfd633559fb5cf9a0      1
    258 0xaa2f23dfc73783ea17993f3ce7f1b8d510ca76db      1
    259 0xaae8b0adf77b41cc955709f779822b2da2b02408      1
    260 0xab1de2f850e4a77cc4fdd7f258d1bf8f157c0582      1
    261 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    262 0xab2edc59b2b62773d089e79d6bafb08563227915      1
    263 0xac2b5067a9bf31429d6819cf5b995642c398abd0      1
    264 0xadb630d5795b384a5ceae977543714b249aee808      1
    265 0xaef63b54eacbdceff0b63609623d038195008d88      1
    266 0xafc9d2e4f7e7be7d1c5090f02a196ca9a144f02e      1
    267 0xb0ad0552c0c674fb3f98fedc80254908d01f553d      1
    268 0xb0b93e6400271836f3a291f3d3d2b098e4ee4fea      1
    269 0xb0c6596d32b90d390077f7af7dcc97ffcbd5bde0      1
    270 0xb0e984d4bf1830dcb6c8c4e0f81d0869d2f037b1      1
    271 0xb15ebf21a217af1385df577932a14b09bdf8c434      1
    272 0xb21f128567241ee99bd445aa3c1fa26f62cc375d      1
    273 0xb2d2ecc7d94cfb8e70f60aeb97bf7f4c4cb8ef28      1
    274 0xb35b73d7c93ddb12efff96735fb0c06b0b72655a      1
    275 0xb3a3af59e21493e8e794b1770a1fd226f88e3248      1
    276 0xb3b56657f60e48ac7bdab91d7d028cbdd55d006b      1
    277 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    278 0xb3c4bf884bd58e2673326030eb6755de4573ebbb      1
    279 0xb4f2b27ed1193f9550fb5528b29a9a2a17a9907c      1
    280 0xb662ffa9f1f873c12d60e105117ac9c081e14d87      1
    281 0xb69c7ccac0a0207066db77a8f59830f921a759ab      1
    282 0xb6c47a3a5c157d6ca400b22f32a65089334a1aab      1
    283 0xb75ae17b60c05ef3c2e86f42241f9eb0cd39249b      1
    284 0xb855567a17c266c1d82a52bb093ef5b6a66deb01      1
    285 0xb8964cb4ed7e0f521c72b68903b1a3e3e7f248e0      1
    286 0xb92774ab2f23d1bcb69f9e41155c9a3295de7901      1
    287 0xbbc6d8744076640b0af984aa0d42896eb777b7a5      1
    288 0xbdfbf2a55de91fee5d0eb829fdfd337430862bff      1
    289 0xbe58c00a35b9995438a298c44673faaebc2f53b3      1
    290 0xc0be11efafb9eef7069a4b910c9c39cf59617e2f      1
    291 0xc14480bd7bb6d36377c75286b80452654f5b7a79      1
    292 0xc2978441f46a76c60e0cd59e986498b75a40572d      1
    293 0xc2f753ac8f813da903dba5380c02913003b5716e      1
    294 0xc3b6cb4b0206ad1e20dad322b4270f5c0fed6f08      1
    295 0xc48116f3c3b16c125611ea6fbd033c7eb186b4a7      1
    296 0xc58f1b1deb0ed06ec754de897b25f44d8289cf64      1
    297 0xc5c9cfe3e5771b3e5a57a62c2141c58f2a3749c8      1
    298 0xc6ab0fbe71b65e3a8b0c5fda3904154d4d15bea0      1
    299 0xc72aed14386158960d0e93fecb83642e68482e4b      1
    300 0xc78c838e66e93ab9bccaa72404e01adfcca423a7      1
    301 0xc917ee63ea36765d63737d8baac564dc57c33b69      1
    302 0xc9b5caead28d9fb4fbca386aa90b8deb647e5a12      1
    303 0xc9bb325af49cf65511360984b54fd0fe02ba8905      1
    304 0xc9df79fe514b75cc9f372cd32c8ee6ebda699bb4      1
    305 0xca05faeadae30f5b358292223debf16f092f4c16      1
    306 0xca11d10ceb098f597a0cab28117fc3465991a63c      1
    307 0xcade1e68a994c5b1459ccd19150128ffef09ea3c      1
    308 0xcafcaa99551d0122add5d9b50235b0b0b05a5e83      1
    309 0xcc47fcbfdd6e558fb39cf5ef4d92e59890476b86      1
    310 0xcc6f3815128b7b4655b1487fd521a79af45fa1af      1
    311 0xccd8550fddedec66075f689464fde8a38ebe257f      1
    312 0xccffdd9f9f94599458cda148c6e815e2384c44d6      1
    313 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    314 0xceb8240efd5164bbdcb21cf06107f9d5baf53db2      1
    315 0xcf3b555729822b93ab8f05c9124b10243cd3748f      1
    316 0xcff70633d1088623963d682753b6d3c6366146e6      1
    317 0xd119f015140388488ffebef36bc5e4f014ac61a1      1
    318 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    319 0xd154ae5da672d6d8d7b66a73a9b1e1457ef1c898      1
    320 0xd1cc5a0b833399c4f006cc9ec082aaf836d47bbb      1
    321 0xd27fc8aee201ec6a35f3ae32c457d04c44965158      1
    322 0xd36d0ace6b18f13760376751af10741336131ce8      1
    323 0xd566e6273043f7d60e616a701ebb0ad6816f1957      1
    324 0xd5d2582ec8c75046d14bdc1acf96f55e6e8e680a      1
    325 0xd67360166baf50bd81bf7972ae5a4bc105e79f2a      1
    326 0xd72987036646b91cd833cb14a35bee5fbb69be8a      1
    327 0xd841d89ff8775966750eae25547757751460e1eb      1
    328 0xd99208a6b2c353b88db78f04642b47f9fa993337      1
    329 0xda022d23ca13b209a3699fbfe08d12a1f28fe35e      1
    330 0xda59a5ec18a3bc48f1346a1a439aee253405f2a1      1
    331 0xdd0c246165c8b4a90bc5a446da9ee0908a45966f      1
    332 0xde8b8fe5d03179f3cf756d44b76f587f55d44b50      1
    333 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    334 0xdf9749659a797f991693f16a9ace4b5347f77e5e      1
    335 0xdffb2d60e5ecb233d448239988d3a6fb6470a8d6      1
    336 0xe22e9db25c68f96072fa29a485f195ac09355af6      1
    337 0xe2ebb099022ff5d6cb0ba1f09fc168a7937345d5      1
    338 0xe3c019b4c3d32a78003847fad9a78e1254b4fa6c      1
    339 0xe543c3f99beef6236371d7b104b30ed4aec181db      1
    340 0xe59d87e5298ac2fb24c2fc55d256cb8fcb696238      1
    341 0xe5d48962c14bf3c71d48f37e630709b7dc97456e      1
    342 0xe674529bd1748703ed885d0298714c9dce1a53e8      1
    343 0xe6c6e985b8624c2e7d4c27c58f9cd82ee1751f9e      1
    344 0xe6cb0c85b06616b16c939b6abed3a44b93e01e49      1
    345 0xe6e0b49bdc9ee4323dffb09efa19bb81170220e8      1
    346 0xe780932ee1df53a0cd01c3678d21b4bd2ae93962      1
    347 0xe90aadbf8e1d1830e7e9509a5b158d94cf84c36b      1
    348 0xe91cbc483a8fda6bc377ad8b8c717f386a93d349      1
    349 0xe98903c3a84e7208806f0da97af42014ef1c8a92      1
    350 0xea05f13196bd437b92c682a86620f58b0f673cec      1
    351 0xea17505d3b8d88583239b3237b2931bf15e5e403      1
    352 0xea5f87d9f02aa5296dd1807a2af76654f5c16e76      1
    353 0xeca588e75c8eb0d2322f52c2c90bd525c5a5d93d      1
    354 0xecc199ec31efd2648c714a4baa50745e73ca7371      1
    355 0xece76cf8eb3241fcdcd99cde9d360b70f65a08b4      1
    356 0xee0046b3b5ab5f4495b13496652bd83779b64b5e      1
    357 0xeebb13590fd5d8508c859a2d6af157080e4c28b5      1
    358 0xef2487a8c8d5b268bce7ae6af56cecce48bacdff      1
    359 0xf084f1f98f7d0917dbe5eec0098b99e16a37c79e      1
    360 0xf089cec067a9b52c229dfbd7e94a124cd72943bf      1
    361 0xf0b84462afc5cfce715aa3bfaf921a8e97467b92      1
    362 0xf2c7e2f00e72018769e419bb6511d34d59fb4d61      1
    363 0xf4b877a1d7d0ca8f7c66ffe770891932a025c4d0      1
    364 0xf5731ba57f5648562d159e912cbc2e921c8cd5d5      1
    365 0xf5c0ef01c6182dc5f20b5ab92f7bdba13facb50a      1
    366 0xf61030d320e71256a43ec22839db345d80ac84b3      1
    367 0xf61b873a21d767ff0506e8b2630c4dde7216035d      1
    368 0xf698cb85014702653c6c17d55cfa380618e8f098      1
    369 0xf74ea8fdfd03b7fc714babf62336ef53db0cff41      1
    370 0xf777af5cfa7a0f242b3e0cf7fff4823c15dde70f      1
    371 0xf7b060806ace46cea4df0e688f871c9664383a8d      1
    372 0xf81c1d67b9c1f80eb30c610e2a78c2a1b1fb013c      1
    373 0xf91e3017c894e9a240972fcb7a771c355b7d2b16      1
    374 0xfa17ca22c988844c89a9ca6e4f84e154997eec36      1
    375 0xfb4d91e65958ae2265edcd64bb50650f31d56591      1
    376 0xfd0e359a4b52f17740c181bd85408cba8cf88ec8      1
    377 0xfd7966bedeea8b6a0c97f97824d5649b80fe39a2      1
    378 0xfdcd06edd66582102ea8e8c2cb1fa271cfec8dfc      1
    379 0xfe2a623f84ccb41bd7ba9a702d30d75873c4b633      1
    380 0xfe917312344b4ed3283c5e142868cd3eaa782933      1
    381 0xfede771a9e62837727674dacd74acf12dd1a5a3e      1
    382 0xffb7520564b1192a75fb4e2f3f8ff25b0f85a5f4      1

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
