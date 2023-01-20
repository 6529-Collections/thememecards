
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16439369.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance          contract        
     Length:33374       Length:33374       Min.   :  1.000   Length:33374      
     Class :character   Class :character   1st Qu.:  1.000   Class :character  
     Mode  :character   Mode  :character   Median :  1.000   Mode  :character  
                                           Mean   :  1.289                     
                                           3rd Qu.:  1.000                     
                                           Max.   :652.000                     
         name          
     Length:33374      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16439969 # https://etherscan.io/block/16439969
block_hash <- "0x75bf4f8fbd3a1dac88403d27ec003e56a8eeb1d7ed957b67a638f67389b816cc"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4820 

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
airdrop_strauss     <- pick(snapshot, contracts=c("SuperRare","TransientGallery","Foundation","MakersPlace"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_strauss       <- pick(snapshot, contracts=c("MakersPlaceEditions","BenStraussEditions","HouseofStrauss"), address_remove=address_remove, address_subtract=airdrop_strauss,address_max=1)
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
     1 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     2 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
     3 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
     4 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     5 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
     6 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
     7 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
     8 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
     9 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    10 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1

## Airdrop Artist

``` r
c(airdrop_strauss) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 18 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1b672e08c3cc6486b1c01d1114d5ced3c5682882      1
     2 0x1f8dec5061b0d9bf17e5828f249142b39dab84b4      1
     3 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     4 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     5 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     6 0x513a36ad32552a8939697d12a342f8ecb124bd61      1
     7 0x7333a52c1355fab787de23f28354faff5e6a9688      1
     8 0x75c5b5498181da0ff9d3618372c4fde670b1d809      1
     9 0x7b2ad6af37be6a77c95556d9453e4877b6a8437d      1
    10 0x7e5b399e254665590266ac6a9e2a1e3336576cc0      1
    11 0x88668b63218bcfd31692e14f635d195f9f45c030      1
    12 0x8a0a569b2937ecc360a686386e34df83cd2348a3      1
    13 0x910ca5528ff6de3e72c7979ee2abc1a4400d77e4      1
    14 0xa925717c2b022a65849690052be9beaa4c8f0d80      1
    15 0xdcaa90d9f3b75cda80764326f6594b58d0585d21      1
    16 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    17 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    18 0xf2582f0a1573439a5a45e3cf1cd6e40a4457ef7b      1

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
    24 0x53562e8ecee54356dc89ad263a444528b34d6c80      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    27 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    28 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    29 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    30 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    31 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    32 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    33 0x69e68074f1aada957edd39c5eae0069973343f30      1
    34 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    35 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    36 0x71677455ef1479a96596cb7fb894d16dbe6e792a      1
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
    47 0x8bd9c4b5ea8f6f14c9b14d830ddb67f3720d77f6      1
    48 0x8fc2326375174bfe3baccc98cb9906921dc9599c      1
    49 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    50 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    51 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    52 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    53 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    54 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    55 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    56 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    57 0xaee4bdcf9d164d9adbbcbfd846623fbe133a6018      1
    58 0xb6b4a02dca517564eb98790ff67d42b5b37a3d4e      1
    59 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    60 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    61 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
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
c(allow_strauss) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 128 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02d1a96a3f39c2e4965e57caf70b3ac5083033f9      1
      2 0x08e0a3e752d1338c0f02b986b9504f1009a962ca      1
      3 0x09b14b1c8a92bca0cc592a80dc311bf1a3fde9f5      1
      4 0x0bcbb835fe0c47431a105ad5c65adfb433ffcb04      1
      5 0x0c4a911ed6af1032beec7c19225c91a3160e39e3      1
      6 0x1151507dcc7a043a63fd51bc2364ce30eeb9291b      1
      7 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
      8 0x1451d4f51f5db86ab2480640ed2c69b304570931      1
      9 0x15265c9989a97f84c01cdba9da4461ae518083fc      1
     10 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     11 0x17e72a77a84c2705e77c686a3f756ce9d3637c58      1
     12 0x18acaf5a29b70a659cca4768460bd04f7fd79403      1
     13 0x1bc14f2908955dbdf748b094a2032194807f1ccc      1
     14 0x1da98aa4faefb6ec93cc1ba6adcfb59c8af51152      1
     15 0x1de9d1ce8cba16c98ad50d0f902e6feacbb35df9      1
     16 0x1e927ca8376d51c163fff2038080484ece23212b      1
     17 0x26013b787aac632a92483f669e2de85103ad2536      1
     18 0x27458760bf383bd2fa9957ae8aa95e9fd5236654      1
     19 0x28062cfef2cb09f263787fe5cbd37ac72534d67c      1
     20 0x2842d3267092eb2e24957688671fbecc67ea7fe0      1
     21 0x296bdab9a39638179dd72102b1b592582f4b9a0d      1
     22 0x29cd9553e268ca520d73437a8045ab9604a149e5      1
     23 0x2a26fb1a180a2ce5c6c2d06dc4d430c37e7ecf39      1
     24 0x2a76f7df64889a1f20f5b6aa87ebffa9a38ab925      1
     25 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     26 0x2f87cebd3bd2339c43096eeeb9f934e0e14aca25      1
     27 0x3089d5dccf383570ca6d2f11059753cf7618131a      1
     28 0x30943b6e8799908dbabcb68308472cc8d4017cbf      1
     29 0x33ea86954055c5fd3e286c08865a9242ec3056f1      1
     30 0x34a94a458a536d3f62dc3167838f2fec2adface3      1
     31 0x359c8e5967245d22e2c6dc83b5c083c39b133fda      1
     32 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     33 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     34 0x468cbad61a63ef6d753ac395d99ebacddcf437ad      1
     35 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
     36 0x49f9fb56043978fec51b200c84c64dc3959a3a68      1
     37 0x4ce19c5084e045d029bfb4857c75b1778eeaeb98      1
     38 0x50734b1ddbb0011f40e14df926df04d0dd9e7aef      1
     39 0x53f34fd3a0105fd0a3e5c37494e0ad1dc10e438b      1
     40 0x55157ce961289be90889ebcd7a0494b438a1ff6d      1
     41 0x561b88119ce226e789b209f56de747079e02eb3d      1
     42 0x59c4c94c7e818fcfc3449eda4e6db998079f423f      1
     43 0x5bd2b53cb6b51b4bda9a23ef46b76a501ad12e63      1
     44 0x5ec3563e1ce1147154ed86d18ebb02b3257ad5a6      1
     45 0x608f0a54b97604a528b48c7164f6990a03e241ca      1
     46 0x6373c0181f6dc83f6705466570cb10c06826246a      1
     47 0x64072ae2095677ab7e8df63e1fbae89b6366d6b4      1
     48 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     49 0x6ef8fd2ffd39df9e73eea701e17db9cf51ffbb7e      1
     50 0x72fae93d08a060a7f0a8919708c0db74ca46cbb6      1
     51 0x7737ca9d2407f14fba41547c01dcff2793605892      1
     52 0x776c40dc5de135fd3d9870aa0c8e58b8171888ed      1
     53 0x78938dcce6e6f3e3ffb9d6dd8ad05e5cba457409      1
     54 0x79d899379844d35a1a1f5d51d3185dd821f44dc1      1
     55 0x7cc696eeb48dc6acee89447168302a90c116bc34      1
     56 0x7d9e512b563f48145aa999e8176e3796e0918e4b      1
     57 0x80a7747adadd60de888da9d7e9054e8d33817d02      1
     58 0x80c7dc92c0b1397aaf8277b8fbcee229c402d702      1
     59 0x81e04dbe70f405c0f4355511163ab821a39cbfea      1
     60 0x8231ff66a7bbdc14ad89782a51c939bbc4c92784      1
     61 0x82d0efe3e244d97d763ea17b66dcaeaed13ba679      1
     62 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
     63 0x8a47daaffa94c930690dd9f6848c8ca1ca91186d      1
     64 0x8a80e1299ee31908fc8f19b5bc409a2e46a0b93b      1
     65 0x8bcd39e6bd0f48b0bce0d1e2430596f47f7de18e      1
     66 0x8e53900319f136f55ccc0c14c9d681943de4cca0      1
     67 0x8f77e5212ad133ac1ba5f63816719202234fcfcc      1
     68 0x905d13367f3bb940072a133c81563f1b1a6779ad      1
     69 0x9152cdea66c12e79eeb95313a09cc4f7ee180f3e      1
     70 0x9481d8bc76fb1b7d2f95733a6ec0b40d0cb1fffc      1
     71 0x952c6b14405566fa50434d23af7075b9e0833676      1
     72 0x95317e0fb4bd69f83d9fb21972f6ed44833d03c2      1
     73 0x96d4f030eeeca1a57540b7c2019fc7682d6e7af7      1
     74 0x971625a19fc373e8628a6bcb0ef17ca943d2fe17      1
     75 0x97df501354ba30868c93ffb113d9ce675c7e5929      1
     76 0x9827929b310ca4e0b525ccf00333c90a16720da8      1
     77 0x9a1a94e850fa0348dc64c396dfaf8209289a94f0      1
     78 0x9e121698d68cf4c2b34202ccee11ea62769a8bc7      1
     79 0x9e910c0d8eb905eaee0317e9d6b7004862fd31b5      1
     80 0x9e9aef6a9684c659f813ef8177444257027adec4      1
     81 0xa6ac0455fa821d3aa70fc5dd761bedd8533f7252      1
     82 0xa6f22ed1871071b675ba351ced448a9173530575      1
     83 0xa6f95ffa23ca53d33390f3ad6d1da06f8a456ccf      1
     84 0xa823a6ce1f881863cf1616525ff7cb6f17458a29      1
     85 0xaccb9e02e2ecf1c59960588ac48da6540a875469      1
     86 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
     87 0xb2f091d7aafd7fc00e1de194fd2aa24aa2dfe35d      1
     88 0xb4b732fd4d12149d5ac06c70032c657b976ccd66      1
     89 0xb52343dbb87db123289ebbd6d8277ce9f1b71ca1      1
     90 0xb6688b0904110b65991f88db8ec5d585229899f5      1
     91 0xb691120339b4f17bbdfa591e5f9f6278c8a15f46      1
     92 0xb6cf777e3696a502107417265c92d1b075636a10      1
     93 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
     94 0xb99cfe2b85b141a6a6fb342c4c4372ca32f2990f      1
     95 0xbb9b12111115cc026883822c236fb7c4a3bd9f84      1
     96 0xbc1e0021f9d22582b34b584b8516dff417e7ab71      1
     97 0xbdd8c23c8e8a39e2cad6159c3a9483f4c106c616      1
     98 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
     99 0xc10fc4f8566f2e89cbfb682e50552c193aa1244d      1
    100 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    101 0xc1b4008432d3e12c3cff0f5ad515709a93d0d140      1
    102 0xc1d040b7ebcc0a571d9e2d54e10bd04c505e7d37      1
    103 0xc3b9038827b7d11048f2ced1bcec0fcf169c2c14      1
    104 0xc53a1682403bf4e00b8c6d8995953209f3c8fe6d      1
    105 0xc55c1c84771a75f5f641bb382adcb7c83e57cfe7      1
    106 0xc6de15da3b913764a695819b9835c9ea3f80df29      1
    107 0xc7db4c95e3050c73db2da4041a03c24df14fd024      1
    108 0xcc392cd51b8cd01d050291f5e01d31714f16dd49      1
    109 0xcc7413acce8b2e4613c48b859af804bb69fafb5b      1
    110 0xcca11a8edb05d64a654092e9551f9122d70ea80e      1
    111 0xcf04c33bfea64cf5dd6ea97e2d5620f825b03b49      1
    112 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    113 0xd9b5021ba375be74b637fd9e0b8c666aebf2bd07      1
    114 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    115 0xdbe1ef8fd76f12b9f82c06aa7b34f652d5fe2c41      1
    116 0xdebb3a9fee141b026f6bec12e9e87e927b892e83      1
    117 0xdf753557f4c742fae88436df1057f76f45b809bd      1
    118 0xe195104ab3eed3e6fbfb16f78ba7e07b3d2f3114      1
    119 0xe70e823e77d87ee06f573587555fccb6c337e9f5      1
    120 0xe7b3f8a344861409f1bd27a25d759e70bc24cb22      1
    121 0xe83505dfd92a901bf36561facd268584f249f2b7      1
    122 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    123 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    124 0xf0f3517c807afd36e85e0467d56416de60b17bb1      1
    125 0xf2a5d9c71ddc45ca98476d39f3dfc2f16dccab68      1
    126 0xf54d3e434a74865540224ee6219f60ba35de228f      1
    127 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    128 0xfd22004806a6846ea67ad883356be810f0428793      1

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
