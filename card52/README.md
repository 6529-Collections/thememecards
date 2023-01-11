
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "16375069.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:32629       Length:32629       Min.   :1.000   Length:32629      
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.001                     
                                           3rd Qu.:1.000                     
                                           Max.   :4.000                     
         name          
     Length:32629      
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16375369 # https://etherscan.io/block/16375369
block_hash <- "0x81a9ffed35930cc90eb9c350014ef299c28db217130fb48654603aa625c3bdde"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4643 

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
airdrop_memes     <- pick(snapshot, contracts=c("memes"),address_remove=address_remove, address_pick=69,address_max=1)
airdrop_grant      <- pick(snapshot, contracts=c("SuperRare","LifeInJapanV2","GrantYun2","Foundation","GrantRivenYun11s","EarlyWorks","FoundationsofStudy","EnRoute","Rarible"), address_remove=address_remove,address_pick=69,address_max=1)

allow_gradient    <- pick(snapshot, contracts=c("gradient"), address_max=1)
allow_raw         <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles     <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
allow_grant        <- pick(snapshot, contracts=c("SuperRare","LifeInJapanV2","GrantYun2","Foundation","GrantRivenYun11s","EarlyWorks","FoundationsofStudy","EnRoute","Rarible","CozyHomesEdition","Noble","LifeInJapanEditions1","LifeInJapanEditions2"), address_remove=address_remove, address_subtract=airdrop_grant,address_max=1)
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
    1 0x04df8d02f912d34fef12a1b0488ee56fd6f7416c      1
    2 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    3 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    4 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    5 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    6 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    7 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    8 0xa32f90b21d11561d31ff604745907acc77fb67e3      1
    9 0xef5ab90a44b68d4f5e3f6be6af4bedb12cd2c66e      1

## Airdrop Memes

``` r
c(airdrop_memes) %>%
tally() %T>%
readr::write_csv(file="airdrop_memes.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06bda076a7eee0b8641431a1eab08cc1c69d76d3      1
     2 0x0a689d7687f20ad37918291fcb0160452fcd4af0      1
     3 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     4 0x0ed39b4c8ca3f0faf74c780e8ec128314ee7f514      1
     5 0x19487393409ef4096ce5cb287f17d52b96e37c8a      1
     6 0x1dac73f3d00052d7e815b41e7b1b5cb241aa4d70      1
     7 0x30eaf1e4db6e830e112988b87a300c03c75cc566      1
     8 0x370ebb6e69b5023fc68755bb6a1435d084dfe07a      1
     9 0x388892c25255c89f73ce9e22515f4b996f1768b7      1
    10 0x4184f399b416f5d2ae4fcd9a842253a9896e73cc      1
    11 0x4808d5a2b6423abc0060465fdfd338c58e758690      1
    12 0x4a6d345c287e3c315eafd0e6a4274a74c6cb9af3      1
    13 0x4ee538b3a9c12644a9d3ae20a67437e8d18b91c2      1
    14 0x5181aaefa633db3539382f8a19fc62e2fb11975f      1
    15 0x52abe6a75eaaed6545b615ec4e0d08e689e84cc5      1
    16 0x61b50a74376a9cd1dabf20bc922017441f9f8267      1
    17 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    18 0x68cb9d174377427182592a078f6a81d0f0d5df0d      1
    19 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
    20 0x723d5453fc08769cb454b5b95db106bf396c73b3      1
    21 0x72703b554a7089f93ff1fc6cc6c0e623900a7b80      1
    22 0x762da606029d3120735aa1eec15464e265db7a3c      1
    23 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    24 0x8044b38345eacd88908c8421e7b5848e1b54b884      1
    25 0x807ea4c5d7945dfea05d358473fee6042e92cf37      1
    26 0x8081a75141dbc89f70d31eece08ff12cfe105e43      1
    27 0x83399f231a2b810166bd3066a7e506787dbeb6b4      1
    28 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    29 0x8606cd6877c93431ce8cc61dfd1042385f107440      1
    30 0x8741e2c8220e8d5e9e698045be1af5c43124c65d      1
    31 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    32 0x92fd2b261020ccb259069e602f08e1cf8782b684      1
    33 0x93f7260304320b5734acfca524ed52f86f9b43a3      1
    34 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    35 0x979ded3b9685b12eb3f17ee6824928979f849c26      1
    36 0x97d2b1b4a249ec77d56fd1576546996d14f7db1a      1
    37 0x9c73c0b9d332e2c28527033e8c800b17dc946b33      1
    38 0x9f2dee49af076fddc50ffcdb9d7b5b69ba22f754      1
    39 0xa36f3c6219485d4c779e58f32e4f6f6ac8db33e8      1
    40 0xa5ed8da239f400141427800da33b602a039f2254      1
    41 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    42 0xb1736ebbdb2f0bccbe844db124524457dd8284de      1
    43 0xb1db41aa2484e3f5f5a510e07003c29fd1b0f115      1
    44 0xb209e4bd577cecb120fcd1797ee399ddd6533ac5      1
    45 0xb60049ca9fcda3764adc647d70221cf3bbeca9ed      1
    46 0xba9e27317dcd38a312e5d9111af834e17237f7ed      1
    47 0xbb0af8fb13473b81c2b6fe1503a0f70207231717      1
    48 0xc26012491b9dfb2e6f2cb0305673e212721d5950      1
    49 0xc2d9eb979f4679adc0a4328ac5caa096a03f18c3      1
    50 0xd095197b164fbc29f08623c8d57e3f9484283593      1
    51 0xd2a744047bb754040911ae14cb5bfe289812e9a9      1
    52 0xd2f132930ea3c58a389c6ad3a91ef104f95fe697      1
    53 0xd4af6c2027eec82b6ea16700a1ce0c532873aa82      1
    54 0xd8c40a8f23c062837410b348e257d48ed1cf8482      1
    55 0xda599dedef2d8c00c01cd047bbbedc7399ad5908      1
    56 0xdc0d28f3d09b292d41b836db991b157d641ad64f      1
    57 0xdd66f694bde45827c0de232c19159f4a1683b659      1
    58 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    59 0xe05e90015f6577ea1b1eb682ff8e1db1b7b6bcce      1
    60 0xe262b5a7fa6d23be9007eeb93833d2e51d89834c      1
    61 0xe9efb8bf023567849a00a87a62d3e4a4ee0e3342      1
    62 0xeaa511b1aa3f49d63af30c85377fe0df3a9a4c38      1
    63 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    64 0xf2626eadce87913c29e63b95e39552e1bbe26b44      1
    65 0xf3359e5b89f7804c8c9283781a23133bbd979c9d      1
    66 0xf78f9f879e0c3ca488fb83cb276b0a9bdc42e103      1
    67 0xf8027e0f03c95782ef9be1826828bee931c1ab83      1
    68 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    69 0xfd113ce2c7d6fee4a6fa9a282aabfc32eca5509c      1

## Airdrop Artist

``` r
c(airdrop_grant) %>%
tally() %T>%
readr::write_csv(file="airdrop_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00668bd79ede077b99bbe1c4db59418bc333d4cf      1
     2 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     3 0x04b5a01e011352624245a5150d7f36e754ef8b62      1
     4 0x052564eb0fd8b340803df55def89c25c432f43f4      1
     5 0x0b24d43508544bde9238308337e66ef09316c411      1
     6 0x0ebf7828cd69cc18dd81a4ef1115b201b3086f19      1
     7 0x0f01735fb8fa1a88100e5a060272f44c2496987f      1
     8 0x0f0eae91990140c560d4156db4f00c854dc8f09e      1
     9 0x139a0975ea36cec4c59447002a875749ac9c460f      1
    10 0x1854a7a0b4f9466dbeeb3279e247d4d0b0a06e3b      1
    11 0x18e804e61c44fd194753ce89b32a87acd8435a8d      1
    12 0x1adef8b7253c43caaf50500913d422ae688367bd      1
    13 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
    14 0x24cef6ea31dd26f18ec1455c804f02d8d53b552f      1
    15 0x255eefd8307b3878be1e620fbd6a0ffa193b1cc5      1
    16 0x25f1a3ce7d9dea98d51c18b95758d417a9a641af      1
    17 0x268221eff4de4d9ca8943f9e143c076dd2c1dd54      1
    18 0x26ba73676957e78921c4ccf74801e5cf5c0c8410      1
    19 0x284fdd26d51b2fba3db0eb36f40617cb11a6bccf      1
    20 0x2ac3b47e7bc9d42822c1db3e6948c1a47051e805      1
    21 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
    22 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    23 0x4155ab641b6c36be6822704e82b2b3b5d7e55778      1
    24 0x450a6049e00c72c8205424a1f3d752ad06382fb9      1
    25 0x4e39795874922b61cf3a6adb3a85d52fa736eceb      1
    26 0x4fffe1f377999580d589617e160714215fd99650      1
    27 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    28 0x53314ed80d217744eaa3cfa13fea6692d08627ac      1
    29 0x5a77b2d05afb67eacd8f9c7e98692dd1e2883cb3      1
    30 0x62247532f72dce05d7bca9f1a6d778c91a00054c      1
    31 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    32 0x696ef7ffa70b25666e394a095b0d46b2c17a2662      1
    33 0x69a949d2330bfac2fdbee90ccfa9923343326b19      1
    34 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
    35 0x6d7c44773c52d396f43c2d511b81aa168e9a7a42      1
    36 0x76e9037854329cdb14ee78b07337bd79114f6016      1
    37 0x7efbdd57601b9d5e98fb325f32626ff90aabc110      1
    38 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
    39 0x9273eb21bc9854d0e06432b5b8c7634ba824ae49      1
    40 0x93c79c3ee2964efb7235b9f445f50e6ff9003ac1      1
    41 0x96e1199931a9e201a4d209262c8c379e2c0e4852      1
    42 0x9769334fc882775f4951865aa473481880669d47      1
    43 0x9c163eb3589b0dcf366fc23c581307618b68fb06      1
    44 0xa0c1bca2da31d84180114965d2f80e37b63030ec      1
    45 0xae2c29ae99da10b81ae3350603a12052952f16ce      1
    46 0xb6f79b687c2f61fc69fef6fad96aee61228d81e1      1
    47 0xb9cd01bf2680bedd1bc41b66d27840d06bf4beff      1
    48 0xbd88526a40bd28acb6761a238d7751d3f1bb5fb8      1
    49 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    50 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    51 0xbfe6ab779517544df703159ea41e7d8a71db6ef0      1
    52 0xc449f005667bef849261b35accf931a4bace48fb      1
    53 0xc611376086b5152a78070d89290d0a21e8d17c17      1
    54 0xc6605255a3be604a74e2224ab515718ee01e7fba      1
    55 0xc880331a7b55bb2f443880e77d2322fd671d0b48      1
    56 0xc98a513970f320b11ab408c12eef176292e227bf      1
    57 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    58 0xd010e20dd739c107e592a350d17c981073605a8b      1
    59 0xd11d1c63911a42a2435256c013e4b88313dee2e3      1
    60 0xd4f94745ccf56c97687d6ba7c6713af7ca850ea2      1
    61 0xd83e662b83880081741d0e810c3fe8630fa6cb4d      1
    62 0xd863c0f47bdeb7f113ea85b3cb3d95c667f17ab4      1
    63 0xe35e2e4624987575dfd263b124a3999e180b8b89      1
    64 0xe8d8edbf4d3dda68ce89b2988e1c31b105e3150f      1
    65 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    66 0xf14ff6c6c32172dfbe09e1ab2dcda4173fe9ad23      1
    67 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    68 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    69 0xf5cbb183d0d883ce8b4a1964e4ff85b4d5ad4746      1

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
c(allow_grant) %>%
tally() %T>%
readr::write_csv(file="allow_artist.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 288 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x007abdd190137f614701a6216fd703329afbcdf1      1
      2 0x00dcdd43bd25f78ae91c12f3c295dc11d3065e00      1
      3 0x00ffef3eda48057e3e1f585d7b33f19224db90f2      1
      4 0x011f919a4eb08408de83f4ba74de8e89391f2484      1
      5 0x01df3717ea028db51c6080db588cd04940c493f6      1
      6 0x024a88896b6a85bdacb2de0f23e7fdfca7091f5c      1
      7 0x027cae2ed1a23350a751452e907b4120330f9762      1
      8 0x03d1a11f5e98a02ecd31962dc787000451e0c1d2      1
      9 0x04c95c6f8ea7ff7c3f7a2f148fae75650c5a875e      1
     10 0x0504aced77f859f8b0be73f864dd2f0c1019d41a      1
     11 0x05576913eea5d79b83f28f0cb0d12be54fdae8dc      1
     12 0x05feef6f10fe8890b82c9bf5065d58fb9b9cb284      1
     13 0x069d5abcb2e396ab255afe7907721b6a077d4002      1
     14 0x06a6712572b532188d499eb5ffe8c4f08e379666      1
     15 0x06e13bd0a3cba08e61028f2326b9ea2ca2539900      1
     16 0x073d67c28d909a18555a4a860df3f309ba5472c7      1
     17 0x08679fb863e0b509310104657800bb05375d5fdc      1
     18 0x09e3ab684e211596e63fc839be72909b33d1c852      1
     19 0x0b2e5c3b145988b03d1638f8baea33e35e638afc      1
     20 0x0b8202f0e0f992a6b8fca17628349583de1f7c78      1
     21 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     22 0x0ed39b4c8ca3f0faf74c780e8ec128314ee7f514      1
     23 0x0edc79adf90a6f6960932488ae70e8c22db9c208      1
     24 0x0f18c529b51851079b348bcffd1a45c9426fa65d      1
     25 0x0f3c76ade30adf61b64d01154911308491784dfe      1
     26 0x108794375c0d1ef0bf3d52eb3cd1f5c051628569      1
     27 0x11360f0c5552443b33720a44408aba01a809905e      1
     28 0x116723a14c7ee8ac3647d37bb5e9f193c29b3489      1
     29 0x1282eb73cbfa72279b31938758bc2bdc7955e2f1      1
     30 0x12d4e572cf6b44196c6327ff0eb879ca547f5c34      1
     31 0x14368bfb69824aa8645a36e7ff38584ccfe0fbb6      1
     32 0x177e6254970a4e4f340fc1a130d701d91bdeaf95      1
     33 0x18302a9b14d7340f770fb85ca623f5a9a2269484      1
     34 0x18960fcf439306a0c27644842ef82540a405fa55      1
     35 0x1ae5f1c9939a5fc8928fc544c4cbb667fb505daf      1
     36 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     37 0x1b7690aebc8f986a780a19d74c5a9c293edb0d44      1
     38 0x1c52ee3ef2d07d7b37da542137585cb5ec4973c3      1
     39 0x1d3dd4cc14c9bc93bc8744559be4ace760f7a1d3      1
     40 0x1f2a9ca494a1f29a8dd736ff8f589b1f2e9f3a07      1
     41 0x1f623ee324ca178941545f01bfccfea38cda2072      1
     42 0x201554cc488237d7dda249ab4c9937b5f792cad5      1
     43 0x205a46012bc3c1a2583fdd426679dff276fb257e      1
     44 0x2082c5a3c08a4d5ade0ce779e24b463daa241646      1
     45 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
     46 0x23602ca06e977c86339ffddad74966e824ab691e      1
     47 0x25cbbcb4b10f6e4fa70f6116fa4eb86bc80d564b      1
     48 0x261cd1771b6ff5dedd5c1e6a39a250c062a26836      1
     49 0x26a601ac8a07e1c8b27bd2a5f05d6ec89fe7ce4a      1
     50 0x26b7dbcf563bcb1505ab60c6948c2c73f20ead91      1
     51 0x26dbcf7bb0e7826e087c691228b5f337143fbe16      1
     52 0x27e832f46148e089d1529d1ba5087dbf1ccc29f2      1
     53 0x290e4508f112e79baddd4e204483bc37be3091b2      1
     54 0x2919e3ee9975ed82c7836829e5be471e3933585a      1
     55 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     56 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
     57 0x2e4b5a288311b5fb0b25462c7942d9355ed8145c      1
     58 0x2e8c4204cde4c78917d0313a75eff65d0262ad46      1
     59 0x2f8cb25737f469a3479dbf3cedf428a3d9900d39      1
     60 0x330bce303d27df0eb1b856da3464278db1db1ac5      1
     61 0x33bf10b2b4a57bc20d955c00b2f735897124785c      1
     62 0x35fe2a980a124242db3b45421b4e27be05962b91      1
     63 0x3600aa29edb4f1c864014ca5d5f233e8616fa9f0      1
     64 0x368dc50bbb5928fc72847af3bf10ba5e4a9b322f      1
     65 0x37ba996dd7eb738af38af7839a17ad19cad5f155      1
     66 0x3859f6cc6cd83c0fdf2dbad2a19e39c41405e22a      1
     67 0x39117cedc1769fd480ff696a61e8534893805865      1
     68 0x3a96ce3a88e90bd6b480fa99952ddb3e2394215d      1
     69 0x3ae72034f05af89cc7249d05887a2c70d0c22b41      1
     70 0x3b0025b5cca0de395a561636acc602fe421d4ec8      1
     71 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
     72 0x3cd000b1dfa4482dbed44b60b3f1e43ed34c56d6      1
     73 0x3deed956b999b83361b85bff31d388c35125411d      1
     74 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     75 0x3ef81dc5f1e838e8eccfa3a176d2518eac3fef00      1
     76 0x3f358b4e54385ba4fc8dd9ea718d360db5c7fda5      1
     77 0x3fdbeedcbfd67cbc00fc169fcf557f77ea4ad4ed      1
     78 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     79 0x41955ab7d12f9f6c03de972b91d9b895d9c2eaf8      1
     80 0x426f6d19d35d7044488fba52eb2aa0fdbabc7c4b      1
     81 0x43b3a12cdc49003c9537d0ab92800a97c0a8959e      1
     82 0x446de57a85d7b8cec2ba273293b55605be27eeea      1
     83 0x44c35f9e90fb412e8689bbf80ff5d67bf578859c      1
     84 0x46b0a58d416d518171c05fea44d9acc6779728b0      1
     85 0x46ec81ab57a48caeab71d6981183e617190ad87f      1
     86 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     87 0x4862a36794d271bf7d5c5da41d30e00684b0bad6      1
     88 0x48e2f33ca34bb58b31abe1e38623bb63b5320024      1
     89 0x4ad742e05edb6bdfede3e94d3f4bb884de2d4836      1
     90 0x4add6d10680210419c83d4663689ba3657294b73      1
     91 0x4b4d75b8b0fc7528ea2614b2ca83555824c07867      1
     92 0x4cb4762efa90a9e5f17513b9a213035e63acf9be      1
     93 0x4d31c6c4a3a27fd78965e5876447d8630942131f      1
     94 0x4dd2a681d52f783ca9e317bfb9c6b1f52acdc619      1
     95 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
     96 0x51664b043cecd86527a93c95d163888bec348e63      1
     97 0x53cf7ad54512259264ada2e8a629d7b5c05fcbc9      1
     98 0x53f22f78918879270f0f1ac9d3d6f0adbca70284      1
     99 0x55205fbabda8036697dd087135440dd92df3ec36      1
    100 0x55c8ddb3f486d9f434da6bc24bc756d8d81b2a7b      1
    101 0x560825199379eaecce24f5d73ebe842ff6cd957d      1
    102 0x5959002cb524181d5526714c2804c3775212d823      1
    103 0x5a39e2d5a81fe9e630ee574df446b78f742f5d9e      1
    104 0x5a483c063b6b2b4f0baafbfe799a3504dd477dae      1
    105 0x5b8af9043d1a8e4acbaa5c34b7dbce31849a82bd      1
    106 0x5c1761e297f8bbbe8800c9cbeeb9d6f46639c8cc      1
    107 0x5cd6c95fc5ae0380ef3d1587cc0f747f5ad9716c      1
    108 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
    109 0x5e2ce569a84686a7c396e464e019159f231a8ede      1
    110 0x5e6684c5e5a5b1ee161dc2ab4f3bde5a8a7cb2bc      1
    111 0x5e9135d9564400dd49d8b23abec20e0f317c4c22      1
    112 0x5fa123dcf576c5d247514f49b18d7110c43ae408      1
    113 0x60256530d074465406df460b6f38424ab5df6bed      1
    114 0x6170014e08317ad94ce9f02a1723ffbec6876786      1
    115 0x62c5ee7bcc0e336bc58fc2946bb9671ab2878f7c      1
    116 0x64acc74b69faaaa90a5bcd5c46b4de3baf0c51b8      1
    117 0x66c1cb7d63d6dc5e049ce7667d8973dc35dfb427      1
    118 0x688976844782ca545968d368060744900c5f49b7      1
    119 0x68c4aa868424f57e8e8580555cf9c921e0870a0f      1
    120 0x6afdf83501af209d2455e49ed9179c209852a701      1
    121 0x6b2d81c560c6ee19d648e7bfd697a473547c8ac7      1
    122 0x6b92cead2602decf90877945845c80d76f90b850      1
    123 0x6d8455d43dce9fbe8597499d447755d7ad934b6e      1
    124 0x6dd20703b80b0b97f87d44f377ea76b692504a13      1
    125 0x6ecb6038d923606b3540c4c6ac267c73ab7fa3c5      1
    126 0x6fc9c61a79f995275c962a7429cfa4177686807b      1
    127 0x70eb722513e77168183df2333081855e6eda5311      1
    128 0x71a63543986eb654d915554b3c0b5da52c0b29af      1
    129 0x72b2f940f561198702b4addaf38efb27bd294185      1
    130 0x75d41cdb138c4125f89bef198fd3d865c5e42808      1
    131 0x77d67811527acdf2fc5c9748cfba0ee9013c2891      1
    132 0x78086ad810f8f99a0b6c92a9a6c8857d3c665622      1
    133 0x782adafbf47a604f146af4a059908e946eae539f      1
    134 0x7970d2d7f9acc00447e81ca89a91cd51325b9a80      1
    135 0x7b59793ad075e4ce1e35181054759c080b8d965d      1
    136 0x7c86fb711078794c75c88c0a945121690365dc84      1
    137 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    138 0x7cf90106bafcf7716563f4cf92430809853162cf      1
    139 0x7d70b5b638a2af9816202880535c8fe8f1fb9772      1
    140 0x7dab31a034ef37229a53d270dd973e4f559665b3      1
    141 0x7e7216319b016c758ed6f792eff389291aa8040f      1
    142 0x7f6a2f68e3f16b363ab65e07594e9a352851e759      1
    143 0x7fdab8c244e0b775edeeb9ebb8356b59068c6873      1
    144 0x8386262bc928b5c7a0d19496c4921adffada05f2      1
    145 0x84aeccde4c9f217e83d3fa28c31d34378b903f91      1
    146 0x863141ce1c010ab00f1e0bc6bb3b61335dbdf893      1
    147 0x879f36dcec0780ed7c0f863c9951cfe881f10144      1
    148 0x8a2c9f1ecdab5d32811f7daf495e89b4058fb233      1
    149 0x8a6490d9e24fe8871ae40e73d387f474c329760d      1
    150 0x8c8a36f5b618c42fcdfc1a74222af201557dbed4      1
    151 0x8dd129965c6c770a3b34d9ff5034cd73df5eff8b      1
    152 0x8e166ebb2dc4686640bc70e47fdea1d2615cc443      1
    153 0x8e1fdf5f1ba3f86d77b4dc5b8125169ede8eeabd      1
    154 0x8e62278262472195f6f91802de3a076057ce172d      1
    155 0x8fe8686b254e6685d38eabdc88235d74bf0cbead      1
    156 0x91b26fffffb325e13f1ef592b0933696098044af      1
    157 0x92730185034d49ce10f0b0670f2ee3857415f178      1
    158 0x952e2d065e60b0fbc9f130917f227621204ed48f      1
    159 0x9631d389a1fa14996d93d3bd1948a4e844eecad7      1
    160 0x96667e68a477ac7220e7e2d54d552607abd7f758      1
    161 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    162 0x971995d5961a3fda21d5cce39a9c6615e138cb02      1
    163 0x973ddec1d9d7240a97e4e8781591e58eba225595      1
    164 0x990fe2e6face45a1e3dc72a422e2b7eecfee8449      1
    165 0x9a7dc5307940fa170f9093ca548bda0edb602767      1
    166 0x9af52329437360323d4794f6f75fad4fe3b3599a      1
    167 0x9b07be1cdda3a58f352b7247919077bdd62c4dc8      1
    168 0x9c66cb22926de77756d719b69dbfd73f794d4e90      1
    169 0x9d2fd63c49509e82c7abe56d864deabf41ff4a1e      1
    170 0x9e4e4bec255a59d2b07ce3bbec0afe5df00b4b45      1
    171 0x9e52bf0f8973324721f4d4e71e913f172074b6d9      1
    172 0x9e9c0431eafe5addf548bacfea974ccb550dad45      1
    173 0xa01701fb730631b6cf0f2e1f6efddd13241d11a1      1
    174 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
    175 0xa1168968174d3a4f2b6666051800115d86fdc235      1
    176 0xa38b7a980df40439f1968abf67673fafdd2ac3b1      1
    177 0xa39573625512970f0f438e0fe3a660ba6c4fe857      1
    178 0xa3c523365e5604ad806c1c599c90aa9ebf7a3277      1
    179 0xa3e49fea82d17326636cece9c8c090edf013dccb      1
    180 0xa43d0698b2afbe80c4080b12f1dc4648dfd0fea7      1
    181 0xa490a0346808dda91aea6698cb19e4697d9fc5cc      1
    182 0xa523da93344dd163c32a8cd9a31459ead1d86b0a      1
    183 0xa65a864ca19535e4c06000768c6cfa9f6ebaeb12      1
    184 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    185 0xa7e3647294ae2724bf125339f2c053b387396300      1
    186 0xa90dc6d356b622b9783f5020722e6f92b663438f      1
    187 0xac0564bafdc13aad3423576b496f97d886756883      1
    188 0xaca470227af72b3db690804d628af2c8e97abd57      1
    189 0xaed39ca3cc9ebabe568ec02f419edad009558300      1
    190 0xaf37772cd8a8faefeebc41c30ea915404469ed2f      1
    191 0xaf4c93d576664fcf60fcc56df130a42773926126      1
    192 0xb006e407c055350055c197cc00aa38301d916d0c      1
    193 0xb03d8ce66f7107cff05e84578f57455a15055eb4      1
    194 0xb0bdd53b627d7e61cfc5c13ef110e47e210fac6f      1
    195 0xb20898ad9ae01fa34fb6746de24dbd599c886e22      1
    196 0xb244f4b768208ca90f7cd92c347ee017b4ceb44f      1
    197 0xb38ff496097518a833669a16d8cc3be6bc479ec8      1
    198 0xb94e6215c9c39d8486a363ae67cb86c6c1ee5ac7      1
    199 0xb9c416eb0d03efdce046e4f4bba26c1c3544f4c1      1
    200 0xba0b371278e144e1974f3e883ce71a62273592ae      1
    201 0xba7cfbd459dfa75ddbb9901c661804d06fd4dbac      1
    202 0xbbc37f68e9876d64b2c55016081528ae0a85d8b2      1
    203 0xbd088edb1aeced8d069a52f3bf253beb899437f2      1
    204 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    205 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    206 0xbed872177df6a565b7190873f9540d8ca224f607      1
    207 0xbfe856e4968b0a91b61342c8fdce7e0376ba1894      1
    208 0xc04038c0d0b6ee1034cbde6239f71ff10b81bed6      1
    209 0xc1caa71b27f6548f30a5ee8ca07b11a3d1362d99      1
    210 0xc27eb79f94462a2b4620a270c588ea79461cede0      1
    211 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
    212 0xc40df87e16339f21fbb2e59fb38bf2a957a16ffd      1
    213 0xc481aeec5a219ca98643e71f0b967f40bf211b38      1
    214 0xc4f370c6d3164a56971692362a9e488c0992a29d      1
    215 0xc54ce8aea86260166c243b8ce7f565ff16e16852      1
    216 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    217 0xc7b7e8b3f164ae2a2202ab6bfb7477665ffd8043      1
    218 0xc876b346d50c4199458ee0e3754883f62fe3a5b0      1
    219 0xc98d3abee55c645bc4fce58b168408c04962d41e      1
    220 0xca7e077ab56ba580f7dbe6e50cb2468827bcc40e      1
    221 0xcb69c5838afd73317abe43ea23574ddf7a6e51b7      1
    222 0xccbf8bcde2c0867cb9c05b30a0fd5aee3c2a7513      1
    223 0xccd021ac40448e74641c34e71aaf617afa951ba3      1
    224 0xcce5ad053b8a02c2ecaa8c00412ef9915339542f      1
    225 0xccf1d05f1b3f20303e3d9ada6cd8f723e36b6802      1
    226 0xce435038720f5db23cc8627f825382dfe85a7e06      1
    227 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    228 0xceb78abfad166c11e350156fd671b385265a6c87      1
    229 0xcec2baca25afaf9bbecfd9a800ca2c71576cbf9f      1
    230 0xcf5c11d684a94b5a9b71ef627411b61756bd1f0b      1
    231 0xd2839e6d35efae479bcc932fe2779f97a24177af      1
    232 0xd2a515f2dc07bc23ff47f089047e5fc39ace329a      1
    233 0xd2b12f1a75b2313980d7c410002e6a48cf389b8d      1
    234 0xd4012143f24c2a02a89110241942b9628c1fe636      1
    235 0xd4091d661a44648d61bd3bb51e129d0d60892056      1
    236 0xd4100a2e1aacdeb18bcda92a280124a7111a12b6      1
    237 0xd566f68779f1f671a6cb9b1b866af278a41653de      1
    238 0xd61d1fc8fc0a47872bd9fc773a6453f402401600      1
    239 0xd6dd6961d3224958fcd306b76a991ab974ec1ebc      1
    240 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    241 0xd7d1c6484e4b8b3c09c7b9ca8722aa52b0046c12      1
    242 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    243 0xd8c54dc1a07429a8e0a677f3810ae25a7ce0ae90      1
    244 0xda4745f0254044e4b702c9301737c2a0b8c69a6a      1
    245 0xdac47e0ea5dd4565dfde65080a18e42190600846      1
    246 0xdb46c7bae3aed8bd740c1275c3717548954fc34b      1
    247 0xdc295e925e84e155e860bca1d36a68b88a604642      1
    248 0xdca523dd6e3180498c7d1b285d0f1d6be30e04a4      1
    249 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
    250 0xdf38a8b16f9bb88083c1f1edad41c5323d60f8a8      1
    251 0xe052113bd7d7700d623414a0a4585bcae754e9d5      1
    252 0xe0f2567fe0b74c1fd3650ebbceba8110cc844752      1
    253 0xe0f8c0fcdf09a3b9ad474f128ed9d4269edddcea      1
    254 0xe2f2abb2199a93ab3ffa034567ca43eae6d4f111      1
    255 0xe3dbc73e5719226c13cd46d782bd333d2dac5196      1
    256 0xe487ed5ec80cca6e8a44f6d6f3f712088df8ad4f      1
    257 0xe4f3c69d95049250c154bd6332911f08b6c13d02      1
    258 0xe53ebe8ded621a3e5a1789bbd2605378f8591c87      1
    259 0xe5d9211066084eaba8910b93159f275acb77f855      1
    260 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    261 0xea080de78b140031ce75f6cd7dd7e2cab854e240      1
    262 0xea15534dcd3a12c78345357d0d496f7fa85a5b71      1
    263 0xebfb3b263f11bb1a9d6b14cade9a5e6c8ea4e07c      1
    264 0xec9c3f8bba1d2380429bab40917345fb6685ad27      1
    265 0xed46a567cdd65358a938276cde48258590506375      1
    266 0xed6ff3a2c8a13f2223f6a7b3985e54e1f8dc064b      1
    267 0xed7003a84b1defc3f97c8e2125a210ffdf4095c1      1
    268 0xed8982578836f7d498856b3f543f128bb8621f2a      1
    269 0xee27b8e5760f979d67a6e98b2b403a09d4c04fd7      1
    270 0xeeb1e9aa02a8a542f05e9b8f99b7c379e9507c09      1
    271 0xf10045a32920082f51b4343ed2dc4bb1944355d8      1
    272 0xf2439241881964006369c0e2377d45f3740f48a0      1
    273 0xf2c5f1fd977dbd6de9d04bc4e62dff722d4bb1a1      1
    274 0xf33f6c5302fee8dcaf6363f6862071092b3d56b0      1
    275 0xf38f67d8b16752b18f7358ef216a35423e45f806      1
    276 0xf444a6b13c63999639df53c1db10bd59e3048b3e      1
    277 0xf450a188a00a82ca95fdff613a34aafb81a7c5d0      1
    278 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    279 0xf8938559870a560cab98a03ebe850e5bae2eac14      1
    280 0xfa5b0a20884fb6dce6bf1df3238935433f346a2d      1
    281 0xfb4d91e65958ae2265edcd64bb50650f31d56591      1
    282 0xfc0344564a36d3f572beca9cdaf67ca123bd4b45      1
    283 0xfcf080d18e260a01246fd5c6e8a893a7def4d369      1
    284 0xfdb6dae2791921aeb1aaadfd0ce766225029e3e5      1
    285 0xfef1697cfa1a6c1211020681405013fee40fe2f2      1
    286 0xfefc2850b25fe1aa3e9aa154df61c6b9bc7aa374      1
    287 0xff61206ff5d58e060815eeddaab5219855d7ecde      1
    288 0xfff423212eb0b97788d43a0c38b7d5762ba3c6e6      1

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
