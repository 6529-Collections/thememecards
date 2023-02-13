
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16611169.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:881         Length:881         Min.   : 1.00   Length:881        
     Class :character   Class :character   1st Qu.: 1.00   Class :character  
     Mode  :character   Mode  :character   Median : 1.00   Mode  :character  
                                           Mean   : 1.07                     
                                           3rd Qu.: 1.00                     
                                           Max.   :19.00                     
         name          
     Length:881        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16611569 # https://etherscan.io/block/16611569
block_hash <- "0xf305b076c915a76481649ed30d6d7abbb05d0b0473d2392140ffbd250367dcfa"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4582 

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
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=42,address_max=1)
airdrop_darkmarkart    <- pick(snapshot, contracts=c("SuperRare","SuperRare2","DARKROOMNAKED","Foundation","DARKROOM","Dune","MONSTERS","DarkMarkArt11"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_darkmarkart      <- pick(snapshot, contracts=c("ROCKNROLLAEditions","WANTEDEditions","NobleEditions","NinfaEditions"), address_remove=address_remove, address_subtract=airdrop_darkmarkart,address_max=1)
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
    1 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    2 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    3 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    4 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
     2 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
     3 0x195f2cd867c1c02ae6e61a14ec53f5f01b440edc      1
     4 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     5 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     6 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     7 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     8 0x2d0ddb67b7d551afa7c8fa4d31f86da9cc947450      1
     9 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    10 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    11 0x3df7131fbd4f6b27be8317c69e41817498e1bc0c      1
    12 0x4ad5bf7218b6695630208560bb62a38016ed64b3      1
    13 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    14 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    15 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
    16 0x5c3097e7fd3b97d9fdeec6d378884c892ff0545f      1
    17 0x6b2aa5fc4508ec023a8e355959eaccef5e3a250b      1
    18 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    19 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
    20 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    21 0x8120cd4fd4d867dae6dc1c2e9c504224ed045f34      1
    22 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    23 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    24 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    25 0x88a6f6ac171e24be54d95dfa7ceff80ff185b346      1
    26 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    27 0x97a45305513133c62b2e3a1f2181dfb5ffdbb450      1
    28 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    29 0x9c7471c6d03cddab9ef6e1db6ed93fb8f9328072      1
    30 0x9f3c44a7331ca9da2d3432bcb0df91186a27e3d7      1
    31 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    32 0xac772daaa5079e005eec3e53314d6d1e9149dc87      1
    33 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    34 0xc29ded4c6ede7482068197c3e3205d850ffedb0f      1
    35 0xd1dd467f1630be10d67205d6c816429f8ee49124      1
    36 0xe2b34bbf669096a794397376fb0587e98eb81016      1
    37 0xebe4c00073f1061ad27859478adcb4f5bc8ca67c      1
    38 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    39 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    40 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    41 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    42 0xf81489f074a9f70c294164e07692559269f3defc      1

## Airdrop Artist

``` r
c(airdrop_darkmarkart) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 42 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0a5b7c226ee4c3510a6f567a5e081971a0f79a90      1
     2 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     3 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     4 0x22098012d15b4ff9a2b1da8d658dae8e5d0d3fbb      1
     5 0x273061aa9b9ddddce5b7c2d1eb7237611d558d4f      1
     6 0x2a74bd40df54a0065aeed3cd764ac37325938b8f      1
     7 0x2db0ac6cb953573153c2740502f60e248e2aefb9      1
     8 0x308bff328370fc4a9683ab9fcd79a21be6dc5ee4      1
     9 0x3c27ebd2263a55876896b69c3593b6eefe9136a7      1
    10 0x40b35cf63c0d2ca561affb06e91d0db818dde14a      1
    11 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    12 0x46b2bd5c888e6d57a839c559fd6076f2c15a8cb1      1
    13 0x4739fda4950e4ef5ebc97f48eaf06b1bb80044de      1
    14 0x51d9b3b8dd64fe7dd835d3f21e35183841810fc5      1
    15 0x5bea445ee2fd3a9efa65c1c9ead66e0141fb9f21      1
    16 0x5ee1d9f8ef343ed9bfca83b7213c9b384fafc4c7      1
    17 0x6b6ae848f555f70944bc99d736fe29fcaecf8b23      1
    18 0x6daafa799c4d527f718aa83a81bf6e0abdaa3d61      1
    19 0x7771b1d9967e528152f5db90fdedd8f6c64c6fc4      1
    20 0x7c86fb711078794c75c88c0a945121690365dc84      1
    21 0x907854ac2768a1a923f15a1345872c149b774e4d      1
    22 0x93a6d60ee0a7059d9f508f0af995581278d90e41      1
    23 0x973231cf471f768f0939042e0591da1b15f4799f      1
    24 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    25 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    26 0xb5e7594ec5c93498123571dbac1e7c1699afb768      1
    27 0xc25d23d642b05b9546d85574b53f8b62b6b11a7b      1
    28 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    29 0xcd733fea07b1b68a63b6ffa84ce0d12a94f0bc22      1
    30 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    31 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    32 0xdf067d1c62b6a94e45ced3f01ebc94ae304cb045      1
    33 0xe538bae1a0fe4048e92544b3c67e5e0f340830d2      1
    34 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    35 0xe9ac836ab6e18637ecd10c5fc3b6f786cd40d3af      1
    36 0xeb10f919d15740e4d1a695ff2ff5a239850b351b      1
    37 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    38 0xeeadc5701b5fd25fbcc7fbf04b048cdb1fff1c65      1
    39 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    40 0xf17c382e51d9acd6eeee4df02076227c81256058      1
    41 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    42 0xfe5573c66273313034f7ff6050c54b5402553716      1

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
    17 0x400e904b5071bc41d0c69aeaa5c5de34bf83cae4      1
    18 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    19 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    20 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    21 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    22 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    23 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    30 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    33 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    34 0x69e68074f1aada957edd39c5eae0069973343f30      1
    35 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    36 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    37 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    38 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    39 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    40 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    41 0x82139687faae8a29851902783e02e699de0e0846      1
    42 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    43 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    44 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    45 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    46 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    47 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    48 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    49 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    50 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    51 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    52 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    53 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    54 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    55 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    56 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    57 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    58 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    59 0xbf814810b44978de273191fd612aa47f7b69d564      1
    60 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    61 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    62 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    63 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    64 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    65 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1
    66 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    67 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    68 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    69 0xee958e45f3464d712b8830deb5875c8ac105f698      1
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

## Allow Artist

``` r
c(allow_darkmarkart) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 260 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0057ff99a06f82cd876c4f7f1718bd9a4f2e74b6      1
      2 0x04490fb053a8ff110bea35f22d955c0092aae5f8      1
      3 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
      4 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
      5 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
      6 0x06ef754ca26ea63c508805e4b840c3ff5ebe60e2      1
      7 0x088bb97ec41e98254bf209e19616a003140322a6      1
      8 0x09e3ab684e211596e63fc839be72909b33d1c852      1
      9 0x0ace76b681084e0f7fd9fe4eab763275d43bec8f      1
     10 0x0d53aba24d961c60a12bd7bbd0c5bc28df00a600      1
     11 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     12 0x110886b13e4a932396c0e9d53bf887770ca16784      1
     13 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     14 0x132df84e9ea2f4cca42976b4ce1a8e814e5cb809      1
     15 0x15619273a8826b4bafa17499c3c49aa5c7e78d96      1
     16 0x17114243b4213c2f0ecc8e8a8496d912441e7bc2      1
     17 0x180c08fe3e6917e64b74f3e9e3386f5f5d51e399      1
     18 0x19487393409ef4096ce5cb287f17d52b96e37c8a      1
     19 0x19e7a132bd4b42f580f546197f42e19c42cdfe6c      1
     20 0x1adef8b7253c43caaf50500913d422ae688367bd      1
     21 0x1e121993b4a8bc79d18a4c409db84c100fff25f5      1
     22 0x1f6278bd02a577f057f5e17d2af6cf008d508bbf      1
     23 0x1f6f623b9348b3ca47bf7c25a975959c3d5668cf      1
     24 0x1fd8df37b360d7366db649703014e637ee214f4a      1
     25 0x1ffc29a768e26ab393ea93e4284773410a84b660      1
     26 0x218a211431d5592316717bb9ac07d36f18d3c8ef      1
     27 0x230f4981634d3c96ee9066f17652dc8b9049015f      1
     28 0x23602ca06e977c86339ffddad74966e824ab691e      1
     29 0x23905e87eb85041bc2dfa1fcdc3ef4247d6bc411      1
     30 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     31 0x24e838b0181c7af61518c27063e1dc9359cd2f0f      1
     32 0x24fbadccd6684106e24065694ac87b0e98819235      1
     33 0x25079298fb99886347b9af7fec321799775c6bce      1
     34 0x261b89e16244fa9a1288e75d06544992a7d63768      1
     35 0x265a25670a194df9ff63c848827455da74042087      1
     36 0x26f87915216d0d7375c9febd8a63bb90d1b503c5      1
     37 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
     38 0x29b63a037d8bd0341a45585b8b8fe1bc9e1aa4f5      1
     39 0x2a850e470ec8c38712c389d8aefe40c13f22d013      1
     40 0x2ac81c02fbfa8fc432d572f2894ea61554d11dd0      1
     41 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     42 0x2dce07502002ecc96d17f948ed52f978edd362d2      1
     43 0x2f54455f1e05982590a376bd070dfedb6d3230cc      1
     44 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     45 0x3098c04b75c7fecba9c7a80fc24c097995e95681      1
     46 0x31109265a787ef9fbf997242e36e9d78e8a61999      1
     47 0x311f2b05f8c9dacf4443da9951443664368e726b      1
     48 0x31222b4a8e0a2592d8ee19507186237faaef4467      1
     49 0x3168a9334c9106bcc947b4ac0818eb1613b91e64      1
     50 0x32ef146df0aaf8f6cee7a9d67c5ca56ca0efb48f      1
     51 0x342bd953d03c0baa824124a40e33010a215e51f5      1
     52 0x347d9c6ea6accda235bcee056c649d891ff08fd0      1
     53 0x348d43bd8daeb9f13611d5f0838225e6a9d89bd0      1
     54 0x3564ce014967c81e25f6349124673cf60b7ac572      1
     55 0x35c92a2447de0576399aa34b74d2cb26af950a69      1
     56 0x35fe2a980a124242db3b45421b4e27be05962b91      1
     57 0x367ff64da0668d86e7ac5d43dd9459fd4ce381d6      1
     58 0x36be14c0d731d70333e4f906a0d33ef65182fd0d      1
     59 0x37145f730aa36ee38230c49830238beb3f4b3d32      1
     60 0x373d419979862c6845e74cee5ae1e075801b6abc      1
     61 0x3bc1b52d90fcd503031ca39f718474f3b42b2200      1
     62 0x3d81a716c76c30b6e2f272461bf9c844aee7469f      1
     63 0x3dbc93aecd50e485ecd2dd6dbe73d1ec19a4d839      1
     64 0x3e0d97f01c186e66c28b462ddab96c261a85b129      1
     65 0x3fb160a681806acc91aed14c284c09a96ebc9dfc      1
     66 0x40c3a9fbc5cbcc3910d191dac938c8edec0e768f      1
     67 0x419beee486a63971332cee7170c2f675d92ac5d3      1
     68 0x41bd1e75ec5b56cedfa18a223de19a0a0768f1f5      1
     69 0x423232b60d97d4a989a47ee5fefd47c8dc4b0e1d      1
     70 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
     71 0x43b31a62ada9bd2f1c617a021dc9dd0f81cb8681      1
     72 0x43bc75335f986c5ebb0641e69122f50f2a2794f3      1
     73 0x449d948b4618178c4bea679f58ea6cd805da0331      1
     74 0x45492c6bc6ab97e4320e6f844c560be62737a303      1
     75 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
     76 0x46c7de8f81d756e0de13dff258ea17929fb20851      1
     77 0x46eb92137aa15771eae8ef8f472fc0a278363814      1
     78 0x46f9ad43e6415eed9dd5a694e1ebc40a75cd600c      1
     79 0x478454599df852e11524083605457875e50ceea8      1
     80 0x47f406f4db7d469afc6ee9dd82504c60136f57ef      1
     81 0x49df36a77f819190a41368bc8aca8a54fb6d760d      1
     82 0x4d910a111da1a949ab715db9ea81ed65dd7af6f5      1
     83 0x4da45f63dc3c9c93fef481f10cd7cf65192fde2b      1
     84 0x4de687d92445c81307e66c4f5640a72c093d8762      1
     85 0x4f22a92b274cf65e436b29667ed292059a49b982      1
     86 0x4fb98d1499a24e9c794401e9fb96e9483a43d934      1
     87 0x50ce2d90b63aa4861d4fade0d498fb957c7c9b1d      1
     88 0x524b7c9b4ca33ba72445dfd2d6404c81d8d1f2e3      1
     89 0x52f75e44f0bf4d2373dacb5b0d06019a5ccaa8a0      1
     90 0x534b8531e362da97d808ba2ab3959fa2597bef11      1
     91 0x5467aa7f330356e7628a7a16d4bc1c5891081c4e      1
     92 0x54f1ed06887ac617c8a2e3ee0129d3cb2a8d6c7c      1
     93 0x56bdc5fe7f9752e7f5a381e394acfe72a724462b      1
     94 0x5a483c063b6b2b4f0baafbfe799a3504dd477dae      1
     95 0x5b6f36473aea942c3dbcede8fb2a9c645fe75fd1      1
     96 0x5b97f95e23d5eddf630b756eea62876c7a7eeccf      1
     97 0x5be8f739c8ea94d99b44ab0b1421889c8b99b2e1      1
     98 0x5f4ab143df63665940a7c49406e0fde3f0f4558a      1
     99 0x5f58527f99df57ec7692267daeb5058b7e826ac1      1
    100 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    101 0x6140f00e4ff3936702e68744f2b5978885464cbb      1
    102 0x64177b68b4ae0cca9b13a7bc598e09e12d8af02b      1
    103 0x650a8005cc8133a5fcdf031c29a9687e81a82145      1
    104 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    105 0x66ff8f288c413e61c8ee807088ce301c03076e6e      1
    106 0x6811eaa638b04aedafeaddfca36c50ea303b1866      1
    107 0x688d97023329d17027dde76ba4d42ef11e8ee46a      1
    108 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    109 0x6a3de9a0cb759f634a6a6f66c926d71b055482c4      1
    110 0x6af7812fc932da2d34738a33f295de54011fd16b      1
    111 0x6dadad611b4cec834902281d46c279b951c95db3      1
    112 0x6dd20703b80b0b97f87d44f377ea76b692504a13      1
    113 0x6de871e6e9d6271c701f0dc031e3a4cd35681264      1
    114 0x6e3bb5e59c667c9d78f70fa220df2adf6e953f3f      1
    115 0x7165a04c41c9e5e67d4850eab1dc6ede84d117f0      1
    116 0x7181e11cf2ac6faa3da297ff064ebd8bc4dcd065      1
    117 0x72a62dfdda6868570e709b01c4315d8c99332e05      1
    118 0x753e5a4127668456ac9cd3ac30c6fa53534c87ea      1
    119 0x753f8dab2ce1e6ec33e2967bcdd4974f60025319      1
    120 0x7564314c19620a83b2c87e6d2febf0c3f846b1db      1
    121 0x75e1ee1dc2f501c1b23332e92c19f21866960ecd      1
    122 0x773d715200ab5c07f39db9772e3c83c48534a585      1
    123 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    124 0x7a1d99c12c22fd8b64e62b1fbfc4677140f0cf8f      1
    125 0x7a6597adf4ba4e405bf5bbe0f5976dc933b089ae      1
    126 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    127 0x7b640407513bc16167ef3450fd6339803982e976      1
    128 0x7c59163ca87cbc6f670a6898bb68ba0867ac9ac1      1
    129 0x7cad02c1e53decf5d1102a4b9bee5de79c6d2e18      1
    130 0x7d989ce97cff246e23b2c8e9d431e922c3e85dea      1
    131 0x7eabfe610657218bb4fd5620ed699b14de305412      1
    132 0x7f0c77594ac37984120d4d0758b0aa69e40cbbf0      1
    133 0x7fd29e547dc2d2ec3773457295a98893a0db2e05      1
    134 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    135 0x811c1ab500966bae6ef3451c2c1820d89c3a9f0a      1
    136 0x825c6943f9c16eec210d65f061ed109ee06c8639      1
    137 0x82d15097aab37824a20014b80855b9447f542e1d      1
    138 0x82f23de5a474fc904041c357576afe53c30dd250      1
    139 0x84ba38696f4f2dbcb88ec2c5f82d24a074d2e6fe      1
    140 0x86d5c85abf48484b1032e92425f89578d67744bc      1
    141 0x87da297a832706b5115ee1feb47b7fdb7c476261      1
    142 0x8abe2749b67ad7debb7442c46ea4a0e003d4852a      1
    143 0x8c8c9610d0efc6563d15526e2ee08d8a7aa89f77      1
    144 0x8c8f1be5dbdfa432bbfb33d6a13779e889d8ccf9      1
    145 0x8d049a762fba4d99f9daf434e0c42164f1a2dc50      1
    146 0x8d2c9c6c5029496fd8321762b20d2529ef027c26      1
    147 0x8d805ce50248bbc1424121c5905850353334946f      1
    148 0x8e2336d04480fc7eba123118536e967c23f458c5      1
    149 0x8efceb29863fb0b62c73c6c9c23f10f24243ebfa      1
    150 0x8fc54e3d572012099f3f48369657d58f653e92e4      1
    151 0x926cc3f9a51d8aa0f3254193943249b9d400a77c      1
    152 0x927c2f09ddc5aaca143164e8cc0c774ebc71b0ac      1
    153 0x93ff71b125c12cff9365ae5c6383fdb4a9446f10      1
    154 0x95999c47c3e32a337ef108d657675c2757a606ed      1
    155 0x96fd61202a698ee3eac21e247a6b209ea5ffeb91      1
    156 0x98a853c794f8ea6dcac3c6b48c1730580ebde7b7      1
    157 0x98bdfe7174ed431da22604aaf87d66fe2bd8621e      1
    158 0x9a667dcebfab972af744ac02e6d477945fc24da2      1
    159 0x9ad1089943b6fe90d930426f1d8db095321aa666      1
    160 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    161 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    162 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
    163 0x9d45c8492f94687ec1317ee81e2b8647af5a1759      1
    164 0x9f128b52128027dd6ea9892f741519979d36fa34      1
    165 0xa0e3f0da4112c8e91d56bbbd72cbf4b890a13e0f      1
    166 0xa1ce3d8e2c7607cc04fbcfd282ab2bf046ce8e98      1
    167 0xa266e8b5c812c6a9d63b8862599f2963d19a797e      1
    168 0xa4de5931b992d26651b29b351be8303f7d1875f1      1
    169 0xa72ac4dc61f794d76c408d68d3995d7f75982a3b      1
    170 0xa7536d6e455469987fd8d4216a8900c3d7a9228c      1
    171 0xa78b54a5486ce07838c1511c5ad5f72df2ac833a      1
    172 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    173 0xac2eb6591b402c1e2ec0c741ef34c4625803c045      1
    174 0xac77a336d596bb2d034b2cdca8018e54f7ab14e0      1
    175 0xada00080816c264ece458eee56802c2521c2469e      1
    176 0xae9934cdb0d403b2247378bf53b597faf7655bb0      1
    177 0xaee4c04c8ab29fbc397ea89dff2a81558e5772cf      1
    178 0xb006e407c055350055c197cc00aa38301d916d0c      1
    179 0xb3c3b3038bd2231b7b61a510102b459e694690a4      1
    180 0xb4ac76d41a3f9fadbfe6b7909fb3b25390d7a1e8      1
    181 0xb5b988b04fe7d6cde81c26a0c69d3e3d58f3f704      1
    182 0xb6cf777e3696a502107417265c92d1b075636a10      1
    183 0xb78656e6f62a3e573163c038c6d5be9c10801a84      1
    184 0xb9e8e3135b87a55e25febc411d2450ea63476bc6      1
    185 0xba88e875447dbe1dab4ec13135f159aa6faa38b3      1
    186 0xbadcfb8e4e3c6a766c63121a3e32caf2f4f0938d      1
    187 0xbb3be72a5f745a5a055dafa72b13a6e04fc5859e      1
    188 0xbb5ad1bf3a3e2d6014377aeca3787ccc083ca0ed      1
    189 0xbc0398a920ae62e01f486f9fbee90ffeb46ad7a6      1
    190 0xbdde100544020fc3dd072bf4ce9a62845e6c9884      1
    191 0xc01b3274cee6dba296add4002972f579c8999d0a      1
    192 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    193 0xc4a72063c03e77893b2f0181ffd22b34cab170fd      1
    194 0xc5633bc0bcc3897117841f434a8f5f95a724a7b8      1
    195 0xc5666b5c1f1077ae5c1a41d63efe8290abb9e0f0      1
    196 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    197 0xc81b0b82f9519a329b122a24f31597e872967abf      1
    198 0xc9646794d03c53bc9a3b4ad60188a0b1d962e8ff      1
    199 0xc97958ff5370c56b7e0e6fb58437f00702714d49      1
    200 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    201 0xcbe5b92259fa3b33420fd2565a845ee53aa60a0e      1
    202 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    203 0xcd33e2aa93223ecc6cc816d836dce0f3e635732f      1
    204 0xcd486efddbca4d4ac9504f4272a7148376b36e94      1
    205 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    206 0xceae52c73db66e280f8d5be3db89dc69463c7d72      1
    207 0xcee53c7318ff49571ee1d889e40f3774c7c98b80      1
    208 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    209 0xcf8fd84b9074fc741e8b797009284f1361016f6c      1
    210 0xd0d289dae2bd66ddc6dbd32f1537426bf71141a0      1
    211 0xd2ee5e1381ce222dba7a33edf7eb5d2f31bb1db0      1
    212 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    213 0xd353a1a0051175b4ddacc8e6e6ee5f3976b907e8      1
    214 0xd478df699a99eb1921c7993b1aba4194cdb490e4      1
    215 0xd566f68779f1f671a6cb9b1b866af278a41653de      1
    216 0xd57721b29f2a17ab6a0635210ce05dbbecf5cbf4      1
    217 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    218 0xd6deab61aa40c18508a4454e6cb81ce33560a206      1
    219 0xd86057c793bdfb77bb11dcabfe054ee4fd86ebdd      1
    220 0xd9fa295d29f022d34028182329b051cbceb92579      1
    221 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    222 0xdca51787f58c18fb8f613a27b84b108324ac4c52      1
    223 0xdcaff8e23cfa324d1398219264204d485e9fbd00      1
    224 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    225 0xe01c438e65fada889634d6d863674f17e0669b20      1
    226 0xe0b93aaa75212ef2839738e6fc139c36bdfef33e      1
    227 0xe1976fd4def8ef7a89ed3149239fd982daf6f892      1
    228 0xe19a76c6659e34f099441e84bffa638ad6a3ab25      1
    229 0xe1ce4240b0b8e0c0dea9e34f7e1565c56151dbb6      1
    230 0xe1d15b137cfec76e4dc92378eb7f12e1567b62da      1
    231 0xe2f00c430b36d8a6b389f909340d0f2dff93b876      1
    232 0xe434f59930e9cd2dfd2235ea86c9bfdb44f5bc0f      1
    233 0xe4d27ee31b170819153b1296b2f4bd228fad4366      1
    234 0xe974e75da812e3764dcc746c8fa82e25fb9f0627      1
    235 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    236 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    237 0xecd2663a2c86e08c3cf1e79952a898365b9d894f      1
    238 0xed6b89e5d3c40cec90024cc8b74b308e3b751987      1
    239 0xed6ff3a2c8a13f2223f6a7b3985e54e1f8dc064b      1
    240 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    241 0xf1199b160fa5c2c9f5e38c9c92d8cc3046c7f9aa      1
    242 0xf18d48b4c0f0fb2de32d6a4bb138c0d6b0d865fe      1
    243 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    244 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    245 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    246 0xf433bdbf45ae8396805f5882c9f395b246e62af8      1
    247 0xf53af966dab1afaaac1171dd941148384d38417d      1
    248 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    249 0xf6dfa4a1213f1d14f898f2eb840b1405984fe014      1
    250 0xf74b25d3f6616e51b2d8be6c8cec54b045229655      1
    251 0xf7764568b698e5954892c00ed7d9a390b105c5f7      1
    252 0xf837203e4fa9c139e41ff3241c434411930c1081      1
    253 0xfb3df26810b5f41172731ee406c2ad577d5c8a5a      1
    254 0xfba4f418956c83e27b1b09be2a773ff37d145510      1
    255 0xfc2ee91d24fd57ab5d382c92818ccbcfeb854b56      1
    256 0xfcc3a7c8e7029f38e0109088c2a90ffa1518bde9      1
    257 0xfdd9a3e4c07756c1dc31ba938fc062d45eab1668      1
    258 0xfded90a3b1348425577688866f798f94d77a0d02      1
    259 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    260 0xff93e7aa23280d83115aa513b16de7493edd0196      1

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
