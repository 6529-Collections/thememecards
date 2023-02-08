
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16575369.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:888         Length:888         Min.   : 1.000   Length:888        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.075                     
                                           3rd Qu.: 1.000                     
                                           Max.   :10.000                     
         name          
     Length:888        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16575969 # https://etherscan.io/block/16575969
block_hash <- "0xcd4f1c65da463387ce0d4e2d870527c3553b43705961dc34b3a99afb025960d2"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4503 

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

airdrop_gradient  <- pick(snapshot, contracts=c("gradient"), address_pick=3,address_max=1)
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=31,address_max=1)
airdrop_killian    <- pick(snapshot, contracts=c("SRMintedMemories","SRFromAboveTheClouds","Foundation","SnowDays"), address_remove=address_remove,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"),address_subtract=airdrop_gradient, address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_killian      <- pick(snapshot, contracts=c("KillianMooreEditions","WAGMIEditions","TrickOrTreatEditions","MakersPlaceEditions"), address_remove=address_remove, address_subtract=airdrop_killian,address_max=1)
```

## Airdrop Gradient

``` r
c(airdrop_gradient) %>%
tally() %T>%
readr::write_csv(file="airdrop_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 3 × 2
      address                                    amount
      <chr>                                       <int>
    1 0x1566ae673ae80725bcce901b486c336e6acef465      1
    2 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    3 0xd3e401814d1faa8ca0419ecccbfee93ac7b15b31      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x076097cf460e3b5fc3a94c43d9a598164bb29168      1
     2 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
     3 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     4 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
     5 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
     6 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
     7 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
     8 0x526ef837f56eb025908ba91811a6ed0a3b32c560      1
     9 0x53f1b3d8990472d1adcd7b76348e5508db17cea0      1
    10 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    11 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    12 0x8774be790cb9e12d5edaf2eb8a3f6c89410a497d      1
    13 0x90fa40f24362c24de09b1cb86df9f0b1f1b15a90      1
    14 0x96e861634b4c0e0a9e7c6a65dec549cc2a8a0e56      1
    15 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    16 0xa2b55ffb7ada20a70a1e6e79073f2f4d6623d72c      1
    17 0xae20574304b6264d3129dbfe5df72eead8c5b208      1
    18 0xb0767b217fb1530b064bb1f835c57c047c08ae72      1
    19 0xb26c452e8ac20ae7445fcf46b6478d3984f5d8e9      1
    20 0xbc2ab46887142d546a976d56eb9a3d9da147ee65      1
    21 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    22 0xc03e57b6ace9dd62c84a095e11e494e3c8fd4d42      1
    23 0xc40d07925d1ac5500e9dfc4e0a05eb24a75d195e      1
    24 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
    25 0xd537a2b2b97d104f2d9c7a84377fc11573629085      1
    26 0xe3d9a03a6422b56e7097ff41a86045683f0aa150      1
    27 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    28 0xf4df143e90c2095e173f861a22f1e6a2503d06cb      1
    29 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    30 0xfd849d8cf365fe98bc60025ff7fab45cef0f557b      1
    31 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Airdrop Artist

``` r
c(airdrop_killian) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 31 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     2 0x0aaaac358f29ca7290f78651ae9b08285b5e16d4      1
     3 0x0fb3fa40850c1e472b6eced8fb10781763d42193      1
     4 0x1273b62a0e80ed80ffd46c119555eca9787fa37f      1
     5 0x2ae31abf71ce95a10d3a9eec7bd68745bed56160      1
     6 0x36d77e6773d7cc278d8be2b18a1b416a8d3de6d8      1
     7 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     8 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     9 0x3c8a67ff9d751c3cce50c9acf617959396daacd3      1
    10 0x42689518d72a0fdbc34cfe30aa36d89ed8beb5bb      1
    11 0x42b46951ae492fde820016ad6432c1360a227dd1      1
    12 0x4583e00b4d17ce9e969f94baa2a6336f923d68de      1
    13 0x60a363530ec7db4fe74f5ebe62c337fdca8efe0f      1
    14 0x774dd298809867f9e03cd2ae036a5c769d1c74e4      1
    15 0x7bf8bf27e3f300fece90eb785e4f09aba827edde      1
    16 0x8497277c9339170a0420e86da8352e0c084624cd      1
    17 0x8ee94e820e7e898f47ac934a755d7382d088edd4      1
    18 0x9971bfed7e71b338cc46383adc84625c0688f471      1
    19 0xa03212293bc3bb7e6722d185a89312476aa5fb1d      1
    20 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    21 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    22 0xc9a194bb6a3855b14dc4699cabe7b2881c3f6552      1
    23 0xc9df79fe514b75cc9f372cd32c8ee6ebda699bb4      1
    24 0xcb06bede7cb0a4b333581b6bdcd05f7cc737b9cc      1
    25 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    26 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    27 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    28 0xe8bee2f6bf252ed9efbbd483ca4093ace7350c92      1
    29 0xee0d982d26d7bf4f52a62a06d22eb7c00576cbb6      1
    30 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    31 0xf4296d0591541f6d25e241a0dac3d7021d8b821a      1

## Allow Gradient

``` r
c(allow_gradient) %>%
tally() %T>%
readr::write_csv(file="allow_gradient.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 76 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
     2 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     3 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     4 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     5 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     6 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     7 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     8 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
     9 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    10 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    11 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    12 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
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
    24 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    25 0x59068075a799594db03c0255eed68e8e121155c8      1
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
    36 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    37 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    38 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    39 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    40 0x82139687faae8a29851902783e02e699de0e0846      1
    41 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    42 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    43 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
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
    61 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    62 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    63 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    64 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    65 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    66 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    67 0xe359ab04cec41ac8c62bc5016c10c749c7de5480      1
    68 0xe3b41ae8785e4107cc69f988042ff4a66a367fac      1
    69 0xea39c551834d07ee2ee87f1ceff843c308e089af      1
    70 0xee958e45f3464d712b8830deb5875c8ac105f698      1
    71 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1
    72 0xf15a6b54e68884d27e1bebb1624d70c227b7d04b      1
    73 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    74 0xf8aace471723dba6514f6c6d43651611a0c34529      1
    75 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    76 0xfd22004806a6846ea67ad883356be810f0428793      1

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
c(allow_killian) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 410 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x002a3be63b3b312da5d58eb02d2b48869b46ec82      1
      2 0x00326c6125bb6c50f85960067ad0e1e544388242      1
      3 0x01876c26c13b9916853b97033f89f4ab0b5001ca      1
      4 0x01fe13639b3c0b9127412b6f8210e4753ac1da37      1
      5 0x025fdd585f03ce846740fe5542469f1de425e439      1
      6 0x02ba9ddaa2054fe67197d14cea577a0aa7729b7b      1
      7 0x035da52e7aa085f7e71764c0c8a1ce6690e3dfef      1
      8 0x03e88df6fd001f77be2afb240bca53c1437a45d3      1
      9 0x057931155d0c6355b1bd108b0fbfe9bc77d59ef0      1
     10 0x058c11bdca759f48fcf9619e28c9a11ac0fe60fd      1
     11 0x072f212e54e9b97d1f2aca157a96a0448b5460ab      1
     12 0x073610a8ceb671e2947ec7ca59e10a85594d3881      1
     13 0x077bfc14dd6725f260e1abfd5c942ee13a27091b      1
     14 0x08109966299bef8198b56a3ce6f3bb6058147e95      1
     15 0x096e9348f2279ab7f6dd529e325deda9270e01f5      1
     16 0x09e3ab684e211596e63fc839be72909b33d1c852      1
     17 0x0b08f955f76aa909b95a2ad6b44e116821300e87      1
     18 0x0cdb22cf3891dcbde7fb2a09ebad42f9702d1a86      1
     19 0x0db597f7ecbf7cb8047402e1f7100818ad15d37d      1
     20 0x0e786c79a6c30a20bee286cbef84c63a8a20170e      1
     21 0x0ea9f5d0c97b430d5426465e2832a504bd6dd9f9      1
     22 0x100da4222cfbe6e8deb8cdb8c15899986cb188d9      1
     23 0x113eda13cc8f3597e2e42be6f13f2c0ea4d086f5      1
     24 0x117dde66c2de2979ba4619d8d494fd20ffe96c2e      1
     25 0x11a17b76a33869f29777d4ad9b861e5f83a8e692      1
     26 0x11a22b262e505d355f975e1e48a365b5d4811ae0      1
     27 0x11a80a5192bd09f312f0e728f068283f5e6446cb      1
     28 0x120e4809a78cfc042bcb090b8daf6b40963626a4      1
     29 0x1238d545fe712ac792213604b0ab169257706417      1
     30 0x1451d4f51f5db86ab2480640ed2c69b304570931      1
     31 0x147683a5bdf3d06b363c250699e37ea44b2ae686      1
     32 0x15a33852846b4762d71a62928aa7c64d8aac8544      1
     33 0x16764df1641d2218f7b38f90b80f6c1098b43098      1
     34 0x16c992e55b4f0d8ebf5973daec2ceb882c86fd17      1
     35 0x170a561433c2c1f17fcbeecb854f5d532fbe859d      1
     36 0x1725070316139c7f097095df61b9e0f3e0edb9e2      1
     37 0x18111df217f0f54eaad4704399fa8e7a2dd94e05      1
     38 0x18a8fb79dc55390087ae3582c400772245dee783      1
     39 0x1952687ef8cbd7dda3483c5b8919766dd29a7baa      1
     40 0x19e428e313b390412abc914a4576fa5b90b68c7b      1
     41 0x1a4b7409e020140443a02ef26858a7e53ac7d652      1
     42 0x1ad0e7a4318328c1dd21a9b6e5bf18a5e64b727a      1
     43 0x1b129a517c3169bd549db4a8dc84da77ea463978      1
     44 0x1b1e8ea518f5f23253c4543c2f38e668aad74c07      1
     45 0x1bdb6188a6d6b624ead2a3906a982a8b0bffb954      1
     46 0x1d10b0166a761f269adf864ebdaca9be445ca637      1
     47 0x1d960cddbec881631568bce2e18b9b751b01221b      1
     48 0x1e3b6cd74d67421f01c588ca0a22809dc34a4c13      1
     49 0x1f6278bd02a577f057f5e17d2af6cf008d508bbf      1
     50 0x20717e574ee9c3dc26ab5e5e69af6b9657130321      1
     51 0x20e41495121362c5fcbd6299a16fa9d32cde65ec      1
     52 0x213529556f2a1add57efd4a73f795ed7d72369ee      1
     53 0x213b5a572d81b9bdc71aa51c16fa746218991a83      1
     54 0x21e56d921c7c15f1a0ad2b3983fab569edc5ba34      1
     55 0x221fce6b6dac61520c1c283825e29bb556979111      1
     56 0x24e838b0181c7af61518c27063e1dc9359cd2f0f      1
     57 0x251be313e35d620a36b148e1e301e7082e0d11c9      1
     58 0x2574625852176e84678ae8515181c69df2f58cd6      1
     59 0x2b02d605ba4e59e667b3093206cd704271316eac      1
     60 0x2b36c077836635c606aa09d57f5e7013121ed929      1
     61 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     62 0x2d17a7381c3a7af82e3bcf02173df197ae3fea4f      1
     63 0x2d1a6268ddef23e3cf079812e83755968dd93da5      1
     64 0x2def8c95901a01d4f8428083db4ce8b7d5f743ae      1
     65 0x2e039299209de1419280127d6823a3db1e7e1ee6      1
     66 0x2e70f8a381efd5142474e8ce61fa3d05d3ce0576      1
     67 0x2e954a7fa420a2dd87c5b5a0746d9f1a31d0b3c2      1
     68 0x2f0607aa5d12743aef4e8c642b5d94374588558c      1
     69 0x2fe28b62063509d527f7e07aa9ecdc33429eee75      1
     70 0x3052e1fd42db76738d0710971ef23db55130e3b0      1
     71 0x3055d970653a2d1785a989d2699ea50c0c96f159      1
     72 0x306325d7b11b35173f65f5d80370ce9a07a5097a      1
     73 0x309162f149e1d9bbfd7dce73ea54c70c68fd41f7      1
     74 0x30d6630d31171745017c8ab7cdcaae42bcc4d748      1
     75 0x30e69c65230d4a93602d0859af78dfc56306cb49      1
     76 0x319af5203839970ccb04d2eda95c427941c498ae      1
     77 0x32130144209b9e08aa81f23a0a5abaa7e43ee6e8      1
     78 0x33858fc5acf9a41d204226ed27655ebec8820305      1
     79 0x33a65a078fc4ede4557a2e45bf66aa1c8efc0863      1
     80 0x346950eb1260b162371faa8c813a7d6b0ad3a5c0      1
     81 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     82 0x350c603508524952244042ab5563a4da4dc4488d      1
     83 0x3513fda59c1932232553831ca4d3de32f731b62c      1
     84 0x351f52e0381eee7e93b34d5633d1c31486da3352      1
     85 0x3670e969cc5c917f25447e441a092a524283ba78      1
     86 0x36799ff7bf99602870b91572e2a897b06c91c87b      1
     87 0x37145f730aa36ee38230c49830238beb3f4b3d32      1
     88 0x385dc7d2ba88318c3a22de8cbe7b7b0ff54df842      1
     89 0x3881f0735d9c8aaa779fce6f72938c52e424f852      1
     90 0x38e3d4a5bf7dea9280e389864d4bf9834cc2f266      1
     91 0x3a68b863c4e05564cac4a161e73b22d0770f66fd      1
     92 0x3aa66d4597f98130c7a319024591762d772fdb33      1
     93 0x3ac38d3bb7bdc2ade019b2d71a34ef37d9e097b3      1
     94 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     95 0x3c42528d268b409872bdc3cea1c315cb7206c10e      1
     96 0x3c46252311214d391fd24dbdb80a46a3e30cce60      1
     97 0x3c7a0fb79e04c3c7fdf66452a3ad073998d49104      1
     98 0x3d1df78367d956c4fafc766cfecb9fb2a7fc479c      1
     99 0x3d5078da5ae6c6608f987f8cb00ea2fe31956490      1
    100 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
    101 0x4163f0f8b1caae2492b8bf3ee451cee59d56ec7b      1
    102 0x4257564c6a110a896168a42efee4c5ae3dfd26f0      1
    103 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
    104 0x426a260be49cc952a322ca8a88881000e98453c9      1
    105 0x4283007220a0d6ca3e08b5bbb5e910eda7f28855      1
    106 0x42e62e421bedf2469826879ec1a0574d7d3cca26      1
    107 0x447f043bd961278787fd79318864e09d5fbf8682      1
    108 0x44c2a80f8b924858db469a5b35128908e55c531f      1
    109 0x44ed32b27aebf445c209dab3ea7e3a9d50f37b2a      1
    110 0x4527c353ab252ae0d6f5968b17d1fad9d0f5153b      1
    111 0x45f1e7eee9dc2dba7d85f2ec146632dc8db8ce85      1
    112 0x460aa467a8e9cec334141521b662a4d7ba264082      1
    113 0x4687f6b19b5ceea0cd50eca6711127b23602deac      1
    114 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    115 0x49d6358f762edb536f0418955cc89e887dd182e1      1
    116 0x4b8fb8ab0f79e26d6fce247c6f30a214d506f22f      1
    117 0x4bfd702aa9dd526f5a65851771b90d78ee97abbe      1
    118 0x4c79303747d47d01112d91c20cb9685c44112896      1
    119 0x4cc2b3075c3cc735958a50ce1b3dd00173e97b04      1
    120 0x4cf8c22a7ed4d8e5750d2b815fcaf8f2a0f14441      1
    121 0x4d2420a4d983fdeb453daf3c8ec4306d219d1834      1
    122 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
    123 0x4f22a92b274cf65e436b29667ed292059a49b982      1
    124 0x4fd3bf4b285bcd30848327052abda467a16f9f48      1
    125 0x50734b1ddbb0011f40e14df926df04d0dd9e7aef      1
    126 0x50a77ec4b8aeb8a86e8c8fc73f276a40875b3368      1
    127 0x51097fdf7b9ba09e578c4421b99b427097b055a1      1
    128 0x511077d1505d3e5ae2a5aa4d7188f316a1228cad      1
    129 0x5147c1a9e594041ccdba5d044b6274231dc6ebf5      1
    130 0x5168895228e49c785d3d77a4621f076403da79a0      1
    131 0x51cb87167c04abfa132a67a037b50b5dc4d978e8      1
    132 0x528eeb05a93a22a9cb1f9e507af991e264b9cf3b      1
    133 0x52e14e8dfc87e8875ce5e9a94019f497b82b3e01      1
    134 0x5327486c3a44775f75188ff174560f9c3cdba570      1
    135 0x533ddab5caf25f197e1e2836506fb069288a6008      1
    136 0x53cf7ad54512259264ada2e8a629d7b5c05fcbc9      1
    137 0x54f798ca0bd83f1d9827a9f07a4c4e1b8d5b1bd7      1
    138 0x552a1fe13b45146b4bf295cd5396eb980e88e133      1
    139 0x5588e1c09dc7cc618bfa3e40fed3aef7824b20ce      1
    140 0x55b97e94b29cd9bf491fe63011cc23dae6392628      1
    141 0x56b367a6303b3d626bfbabc625b8e0b5bd9452f8      1
    142 0x570ff649bb5ae8e032a4f9456aa1ad74cc3f3e8f      1
    143 0x596dc4bb87c9d7dc4e227906b2fd6651ae2e6fd9      1
    144 0x598ce158e94a87ebef2c6865d37b63dac4312c06      1
    145 0x5a88de603547f7ebb549f6412a7bc6af8cb40757      1
    146 0x5ab6353df494e6f8282f1eeec99998d5a7ca9bea      1
    147 0x5ae9a091976cedf12a2ae140063f776642789a39      1
    148 0x5b0301950d850fcff4fc6a2c0b19382ca3e9646d      1
    149 0x5b5fc02d41eaafa7ecde3c02c3e5c59110a77d99      1
    150 0x5ba38bde843c3a2815ec00a47e73c0bc612e6495      1
    151 0x5bbfbc4e659e40fd281b3863f138289ccba74c52      1
    152 0x5be8f739c8ea94d99b44ab0b1421889c8b99b2e1      1
    153 0x5c4fe960950ba0e09a72869c3d51fe70f07580e0      1
    154 0x5d70a49e217dbdc1a5049542d70c0fafba5e6bf2      1
    155 0x5de8a18ad5b663470b9b395ebec66593e6b63f83      1
    156 0x5e3161efeceab0d3f7f3fe46f0e3b045d0c9ec74      1
    157 0x6031d4e67f4e098f6663a3a6277bedbe529521af      1
    158 0x607f80309a94aec9bbb5a2f6da2c1329bd68e33e      1
    159 0x6134daed8c4d10b2cd35b95e4330d240b2371a7d      1
    160 0x61709f96c5701cbeb398a11162fe70f7b9dd92a8      1
    161 0x61cb1b5ed74848963f8a02d82011aab512be5efc      1
    162 0x62f0dd4e7d33b3c11ffbfdd31fa82d4aaed38ca8      1
    163 0x633225e814e8fce8cde11c2b0b3d1d1aaf655108      1
    164 0x634ae4b57e6246f0318257743e5255648f9473a6      1
    165 0x63c8e1155e2be1e10041ff56625805abcf1fbf9b      1
    166 0x64f7e4e59a5614411a1a1675de29874a923bb3ee      1
    167 0x65d6f46c346cf45f3160b9a59d4c758bb2b02eb4      1
    168 0x66472ee4287a5abf1b5f448ac08399f69aab9ee6      1
    169 0x66950fc2a0750c858b8b66040212666918d5f828      1
    170 0x669735809ff59029991d52435b274b1b18ade6a8      1
    171 0x68b3778679e6c88a19f612dbec9bacc3a4e52d05      1
    172 0x693c7524aa242e52bb71461e7b1185ec6a6ad182      1
    173 0x695099cb725915c2df63a1d88e1ad1268d8712ef      1
    174 0x6954ee7ede83ca555663704fe7e2b1528ff2c619      1
    175 0x69b9f2bd11f68ac293093c1fcdeef0d141828e99      1
    176 0x6a3bec532a9d04ddd22657014df023a702b7f1f1      1
    177 0x6a828d32fe744cefef5ed21d4ff93a4edd9f0d03      1
    178 0x6ab30e00a8e0a01dace2be30c15e43c1c6a369a3      1
    179 0x6d10ab9b038122124de213a2ba8c9e6234ff3d4c      1
    180 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    181 0x6e957cfd52aa70acb1eceaf6ef7bcbb5dbc02f04      1
    182 0x6ecae358e99dfdd1abe900bebe5f775431c12324      1
    183 0x6f15ddf0abd573986875ed740c1a94f5fef65205      1
    184 0x6ff2ed25cda2f6e5fa1fc0da5839ec554f6590fa      1
    185 0x7056d3ff77c3ac9811bba12783ba3c51a40e6664      1
    186 0x7132f37e94e8752ed37f3239597e3d927ccc2d83      1
    187 0x714e76aaf332b4c8c102bb8b3b127e9b9becd4d0      1
    188 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
    189 0x7375c6a14373f89a07b7e6deeb1d7fdddfecf6bc      1
    190 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    191 0x74db22f8c148d56a2adb7da85e667526cae322c6      1
    192 0x7586834e655ee2de6357b2c8423b76efc5fbcc6b      1
    193 0x767fdfaac0d01878d2cec9148fe8585c22b1a1e2      1
    194 0x7695205c2cda7aa76afbc6fae5093306b931377c      1
    195 0x77039399801f462b4ed13444a266b16355c471bf      1
    196 0x7761ca398e836534bed3b8798f96a3cf39d3bd2e      1
    197 0x777bef8d44c5efe02f3c0a705ec3bff613c82a9c      1
    198 0x77e8001a83e5175e36b0d80c11fe8b5a65ab5ca3      1
    199 0x7842c0b2773ad16d0e0f00e24815c04f700a7d33      1
    200 0x7891596b9c9e6fe809a81ae56e39a5e710259bb5      1
    201 0x7921ee1e0e4d7c470a26af77afe6c0584fa33348      1
    202 0x7983f91136900a15a9cb19088e98bc6f28dc8d53      1
    203 0x7a0208a0b55fcbc31915e8ba7495e5b1f71609e7      1
    204 0x7ba3d1c4f46516fb975fb012f7db04381188e907      1
    205 0x7bb41013dae1a32a5f043c5aadcc4da2a2469210      1
    206 0x7c22cd949433587be928afec5ead4f9ef15012a5      1
    207 0x7c76bc0e71ece83b02c599e95ee8eb91fc389b55      1
    208 0x7cdfa7dfc1c6759b5b893a2abe41d010f9dd79a7      1
    209 0x7d1deb3aa5dffd6b01e8f360a0f1fbfdedf3b0a7      1
    210 0x7d5300e22db1b2850da1beed10a894893731c1a7      1
    211 0x7e8ea603481ad8e570efbbe43653df49ed052842      1
    212 0x7eb63b648d9bdfb49ddb49f64ee099201f53c9e3      1
    213 0x7f6d34a4fdb21729ac15aa0ae01b5b47ff2f5f31      1
    214 0x8105e062d6b94c951e93739aee33018bd6a6d1c7      1
    215 0x812010bbe6652497d33c7a1fd986f629769ea9cc      1
    216 0x822e60d79ecfd2d4525cb6192b6e9732d8b94bb6      1
    217 0x82e3dc297418c4741d40b7ec4923c77fd124a36c      1
    218 0x82f23de5a474fc904041c357576afe53c30dd250      1
    219 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    220 0x8348735e5192371ab899118d5cf9fb258d7ce5c0      1
    221 0x83a091d12823848c6650ebd3517c35a77694345a      1
    222 0x83ad673b6a84ae7df6cf720c8b538d28af43154d      1
    223 0x83bff380d2c59f88f3132542fb23b40afcf361d7      1
    224 0x841412043d4e6b727bae0ea5ddb5cc73dcb699f2      1
    225 0x84da8d78ed9bd5d23225f96b5b115d1576d497a6      1
    226 0x86c53524ce998d2d2bb86fd6e187e08a28704638      1
    227 0x8a47daaffa94c930690dd9f6848c8ca1ca91186d      1
    228 0x8b7ae9db845ff3eae47fe1070224f62e12b2aea9      1
    229 0x8c005e7e9d802dfb1e48074af7d5797303076ce3      1
    230 0x8c4a2b57063ae4b742b4cd6e41946ba36168865d      1
    231 0x8d6cfe2ff08d8b6766eaef33990b78f8990b4520      1
    232 0x8dfd856af8b868bdc73b4eddbf34511310402c03      1
    233 0x8f47315db9fcff1f8075545b19ceabd861cbc7e8      1
    234 0x906d3c28dc17e47c71cc7ca712c8df068df396fd      1
    235 0x9152cdea66c12e79eeb95313a09cc4f7ee180f3e      1
    236 0x921917ef7aff36300a25ca257f3b208e6eeac728      1
    237 0x922631898b7956fd3c601f5b1f069ed16efc8256      1
    238 0x924baf1e2bbc58e9cfe333d4c7074886a6af1afe      1
    239 0x933d7d515b6cdc7763aff030bfc61aff143d6637      1
    240 0x93e6832b6d190d1d7373408833231b1dc3bff79f      1
    241 0x943590a42c27d08e3744202c4ae5ed55c2de240d      1
    242 0x95131b9190f129bbe2c268804bf39c43e3711ac2      1
    243 0x953a8dc0e0e2e8fdf8607e5c3c55dd53b2f4fba6      1
    244 0x966a2359de1bf205f083fe40a8ec2f21b911a433      1
    245 0x96baf6118bde48ca1447a1d0528684f80a3864a3      1
    246 0x980ddf397e5d8a2b3e83751b8d18f8203f89b8f1      1
    247 0x988b5863306649f7191948dfa1e45a873792cc7d      1
    248 0x997dae1a37c4a85296b4499de8d263430a48696f      1
    249 0x9a22c60447083c99a238054b3c722ef1f2c6f23f      1
    250 0x9a49d9f653364171044a755b4d28d49a3ef79c66      1
    251 0x9ab5a9572db323c70a94a1b043759b3490f6f51d      1
    252 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    253 0x9b25142253717e120cb77868b8a805d56ab62369      1
    254 0x9bf12a0280dd6db2d7af5864a67a9e0cede3bdee      1
    255 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    256 0x9c335e4d8cacda86647ff4497789019c7a762a62      1
    257 0x9c9da7dc226927a831f5f37759eae40d41703228      1
    258 0x9decd2a74340a4f22d2dbb73583c85c898447589      1
    259 0x9e910c0d8eb905eaee0317e9d6b7004862fd31b5      1
    260 0x9f477d97a21389542e0a20879a3899730843dccd      1
    261 0x9f558ff9237242fd67b7aa42c162705f6c00db93      1
    262 0x9ff77d193c091ae350b8ce26d50d46e392631292      1
    263 0xa05cd277ba50ec72fc41917437a6c09e39b99bbb      1
    264 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    265 0xa219c09cba809860ebc4dafbce9b8071eeb8a55e      1
    266 0xa3785d2f231808d54aa207a84aee81c85dcbfb21      1
    267 0xa3861a789d05597bc046e8e6d5e10ecde947569f      1
    268 0xa42c95ab281f8a9480fac75379275fce286dd226      1
    269 0xa5cd39ccca574b54085e7c10e53e23883ba50dca      1
    270 0xa7ed45b73ce44f089cba2c11a7340ba11ea299f7      1
    271 0xa7faf09d160d777c98beef579087266f6da167c9      1
    272 0xa80b7bdf45e35bd9ac49f0cc38c72600e4b498b2      1
    273 0xa837c1e2b87aab4d28fee399c6593b66422fd5d2      1
    274 0xa881608403825b94b6c952aa1e190145f0265db0      1
    275 0xa8f1b7bee4bb60311511144ecd6dab7b04ef2735      1
    276 0xaad0ac2ef7ec521d0990ca83586239dd5e915688      1
    277 0xab0ac99d34b23dbfd106f539bd8a265275f28c87      1
    278 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    279 0xab7473da0af4658ae2879cb641d82e7652721486      1
    280 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    281 0xabbb8aa2e9200d67e4097796fcc39353fa0b4864      1
    282 0xabf04838ffcf99e991b7e0df92d9dc7ebd40113b      1
    283 0xac104d56e4f8e0462f8c001ec7dde4c68deb596f      1
    284 0xac9206a124dae4cfe276cb8d117179fb64306582      1
    285 0xada00080816c264ece458eee56802c2521c2469e      1
    286 0xb1eb435fec01c94098a692a9930af6dc12f24903      1
    287 0xb26376ac84799261b61f20452a6c42b8f54fd143      1
    288 0xb27e7350c38f40cdf2c01e3912e4fce9d3237b75      1
    289 0xb33aac6ea683d3abc6fca6019f97418211b18890      1
    290 0xb347dd642e9b20be829abd986a3c16d3e5cf1ffc      1
    291 0xb3cc0cd218fb5c7193ddc88d53926cfc335dbd22      1
    292 0xb423f2a7786d8fdd70aada0d13a11ed573a84c57      1
    293 0xb5a4a9677de36140b4330a3c90500a091723da4b      1
    294 0xb5bbffd7d19e0ed0e4e29f86fb6c70a8379666d4      1
    295 0xb5f31eb9e70f88c5997a243dfb59b7097bcd84be      1
    296 0xb6095324712ae350bd3bab367b151c4ab3ef6a9b      1
    297 0xb6c4ce4eae85066f8fc45093d12444c8b89a6aa9      1
    298 0xb6fef67b00d644fbb5e4368aed9f85ec2d134fd5      1
    299 0xb79f550b3586e3c75f1cc57dd81db0ec10bf37b1      1
    300 0xb7bc86cb0183af5853274ae4e20d36de387c4a64      1
    301 0xb7bf6684445e554a0b16e14988fa0e8641a0484f      1
    302 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    303 0xb7fafa52585882ff727db81c1275103e18a81b6a      1
    304 0xb97c5725c29f96daaf8952b051564acf7f9fb626      1
    305 0xb9efbf93bec0ece78399c718c36849f802b108cc      1
    306 0xbc1e0021f9d22582b34b584b8516dff417e7ab71      1
    307 0xbc2f3873cd6650474d7153f3ecc0a4ac23968fff      1
    308 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    309 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    310 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    311 0xbf13215f5267441581e0811c09dcd5786370ec19      1
    312 0xbf44c636a4a4bbe257a87067b25c4c875170e04d      1
    313 0xbf9d1f65301b6a3b7c8a15f54e70f528a33115f9      1
    314 0xbfbe3d5dac1e55dff6b03602e87594579838f99f      1
    315 0xbfd90a5fa214a5e868bca65fd37b2acd7643ec85      1
    316 0xc0fb55eb408a4e12773139ae152da413d288f3c2      1
    317 0xc167513801a009972f655ce8f57c50b0b4e70489      1
    318 0xc18d4c2fee42accfe7bfdce98bfacbabf76242e9      1
    319 0xc2ba77069899a2409440cf3fe9fd246e1f7b75fa      1
    320 0xc2bc916f748ba52e2765f633ab5d7016e0954e54      1
    321 0xc3cdbc995698254ae6739db0d8e248aafa8d6116      1
    322 0xc457fee0564bf3eb972114762f62ab45c0e3590b      1
    323 0xc4d0af96a1c580fbc9e967776a1666c27ab933e4      1
    324 0xc56c5a7e9ba7b239d130ff3845b504adc62e8bfc      1
    325 0xc585491efbce5ed346c0b1ef067978f21c35c357      1
    326 0xc5a2406ce879c01956068c07a6515b2b3d102f34      1
    327 0xc5af7c22ce278cc5ffc008a59612d4db8bd14c7c      1
    328 0xc5ce6fea2d97622c00ce3fbf89ad4e90ca50cbed      1
    329 0xc5fe72c8cb2ad6a65338d52c4ee8b3e94027f17f      1
    330 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    331 0xc740ea19d0abd440665e28f1783f2401c669ca26      1
    332 0xc7cb755ae92e3e5089ad575bd462ea6bb9fa4642      1
    333 0xc7db4c95e3050c73db2da4041a03c24df14fd024      1
    334 0xc830eeb7e2351850d006463c3e8d3a3ce29ac6ad      1
    335 0xc948ae3d8856782b87a269c256608066d8023c37      1
    336 0xc979db456e3a5e38f32232f52142386b815eeeb0      1
    337 0xc99360f97475dcd02cbaa1940e0e6a2cb67186c8      1
    338 0xca888337597ac67c0333de023f304fa79a622cfa      1
    339 0xcb1803ff8bec838bc7331031c7918bf21759395b      1
    340 0xcb6ace115e764226ddd116fba710650b8b452a1b      1
    341 0xcc6c1d21e8474b3578e69eb036c712ab08ffdfbb      1
    342 0xcc92de3bb8f65595f1095da88aa1a4ae30492603      1
    343 0xcd90c980b25eaabace2a760b364ea5cccb95e2b4      1
    344 0xcddcbd9c2665a71cd4cf5d1fac0740bf7643f260      1
    345 0xce6e27e298bd6024e91be5028762d57d857bb5c5      1
    346 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    347 0xd0c946acb2ffdfe637a9ee901dd9d3d853697b8c      1
    348 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
    349 0xd1e1e5dbc165c6550f935e6f08ed77ca97e351be      1
    350 0xd385605623db1be4974838829703b8e29124bf37      1
    351 0xd3b6c89eccc59183a394854a43fabee2539dbb56      1
    352 0xd458f62925b3e135680626a0f520098972f93fe9      1
    353 0xd506a5b95d0a0c5f861b46127ffa644e4b050c81      1
    354 0xd59120f2a160c9862301c1f3bb5f171279f95512      1
    355 0xd63c4496f130de5006fb622215d763766c9d48ca      1
    356 0xd66bf38e7c704f0cd7910773f5c8cc3098b6828d      1
    357 0xd6d437af5b02bfb8d6063e28be4b94204555abc6      1
    358 0xd6eb449e39a260dd2c17d08147482e43ed770e53      1
    359 0xd8150e48739848f54eecf59b4d43b14ee901f562      1
    360 0xd88da1af87e28d3209b8f55aaf7318ab76003503      1
    361 0xd919845cdd095eb3094c0cf18aed1153c48c747d      1
    362 0xd9ff960de89db597c73d1244a76b9ba5a594d5f8      1
    363 0xdad1d029d7ab8142beb1b882c9cad7c02da6a165      1
    364 0xdbe41dbeabb006df451db3a03736bb802a843733      1
    365 0xdc6f0111ed880d10df01ed39198a5b08ee747ad9      1
    366 0xded79cfc77cf05b1fc8a9424b2ff342747df36d8      1
    367 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    368 0xdf8b134fb7743acd805ecdee11335dd0cca921fc      1
    369 0xe09a14d233e63df313d5d8172821f3dcf321b516      1
    370 0xe0a55ecfd3ef034c61a0a42ebf40e26fd82ee523      1
    371 0xe126731c09e9388e0375243b91a5a9a3fcc64b8c      1
    372 0xe26322f69c04754ba4724edfbc1d4087a8e64256      1
    373 0xe28bc349f666a4281bbfed1e485e8dfad90bb3d2      1
    374 0xe2a9fd5b1a3efd2426c8950d8d37e80614d52571      1
    375 0xe3e06e9ec56ac1e7f3bced968b920d46ae1b460a      1
    376 0xe4ae8ed5500fd43026af8f07542a17f03d876924      1
    377 0xe54a4330f62ea371ea054c9ebfecea67c829186b      1
    378 0xe627311229899b45ba356ecfe02bc137d62abb84      1
    379 0xe6ba7ce1bbd7b811d891c2d4c09d7295bb96752c      1
    380 0xe735fd3f8b99b6129cd90ea8460a98206bd04c18      1
    381 0xe76930b77dbd775f5d9827fd46a19c631a54fef0      1
    382 0xe96ead0ad625122677684448155ba4ae2700814d      1
    383 0xe9d41a1b8613926cb1851c5855d27a1ea32111d0      1
    384 0xe9d53246209e4cd87dbc5d1fee71f44bff89a611      1
    385 0xea13fc9b05806a76e478b82f6527d4f7eb21321d      1
    386 0xea733d49e86a6c0684e8ff8dfae701ce1ac9bb9f      1
    387 0xebafd172eb84d85136e212a98b57ad9120ee526e      1
    388 0xec6d643c765d439da9001cff8d0b71d8b9827cac      1
    389 0xecaebf8f5a28a6ef322bb5428c11270ed057c497      1
    390 0xef3fc754512df1493c5b5e168c14b1ffa9a3edef      1
    391 0xef7bae67054640aa0eaa6f1c55d961296534d8c3      1
    392 0xf07caceaaf2836d5a03c0b53172f6b7eda9b517c      1
    393 0xf3442dd014446b60d19cdcce0313160c3e515d41      1
    394 0xf433771be52fe3b915b5c3840ffe9a55425766e3      1
    395 0xf6eabcf9cf4548e579f53fac29b4172ad9d4904e      1
    396 0xf7008f7d415f04e74bb2460deb800608aa991c1c      1
    397 0xf75b621ecf446991530d73f9b34be14dba96851b      1
    398 0xf765d3e35f82f330f6e3b8046020a17fed58e210      1
    399 0xf7c66bcd6ea3607174da0bc8458bd1508027e539      1
    400 0xf8986ce1fa0a656ee8d7e306c9182db06c6d6449      1
    401 0xf94e09ec9947fc1b9f8974e168c16adb6d3c6a56      1
    402 0xf9e13d8fc6c8f74449a9cee1088822b817526588      1
    403 0xf9e8eaceac12cb5c294705b40a47b7fea9c30775      1
    404 0xfad7819967a6ef122a123f92416616a9a56eb5f0      1
    405 0xfb5b6f0a1073a5ac19672bc8925feed20d4ed5bc      1
    406 0xfb7f7ed923ff00a7bbe14b09b8180e2dfdd5de46      1
    407 0xfde25d8e5e1e3b03f4d0b1d5c793e3916fd85bc4      1
    408 0xfe2b8dc957459ac2a08fef57576bcb8a89845d8b      1
    409 0xfed473cd045563542414e31af846a43bf58fc59a      1
    410 0xff41da21ff3c36ed08b2640d499b6943b881ca35      1

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
