
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:8600        Length:8600        Min.   :1   Length:8600       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:8600       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17483969 # https://etherscan.io/block/17483969
block_hash <- "0x5718c9275e9602528ebeda83d1a4810ec2ce89f26dadbdbdb445e068b895097c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4718 

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

allow_artist_phase1       <- pick(snapshot, contracts=c("SuperRare","SilentTravelers","Foundation","NikolinaPetolas","MakersPlace","KnownOrigin","NikolinaPetolasEditions","NikolinaPetolasArt"), address_remove=address_remove,address_max=1)

allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=75,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_subtract=allow_memesRandom1_phase1,address_pick=100,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1),address_pick=90,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1),address_pick=90,address_max=1)
allow_memesRandom5_phase1 <- pick(snapshot, contracts=c("memes5"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1, allow_memesRandom4_phase1),address_pick=90,address_max=1)
allow_memesRandom6_phase1 <- pick(snapshot, contracts=c("memes6"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1, allow_memesRandom4_phase1, allow_memesRandom5_phase1),address_pick=69,address_max=1)
allow_gradient_phase1     <- pick(snapshot, contracts=c("gradient"), address_max=1)


allow_raw                 <- pick(snapshot, contracts=c("raw"), address_remove=address_remove,address_max=1)
allow_singles             <- pick(snapshot, contracts=c("65291/1s"), address_remove=address_remove,address_max=1)
```

## Allow Artist Phase 1

``` r
c(allow_artist_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_artist_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 40 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x017ffb7c9e5aff47d9569265d7c78b2494099782      1
     2 0x0e3c735708f660b9d29b1f05679c13f72e14c0ea      1
     3 0x1d249860acf2c1bde1f2a8642200edc719969230      1
     4 0x1f08cbfca1f63a23bb6255a76e1ddeeb8d24d0fa      1
     5 0x22bd04906e808196b19f7d10463fe463ed5f88cb      1
     6 0x235e14f5cde92a098d9b38a3f3ac07e7058cd40c      1
     7 0x2918ceda641716fd6bda3ccf691c3b54d4c87b6e      1
     8 0x2debdf4427ccbcfdbc7f29d63964499a0ec184f6      1
     9 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
    10 0x368aedb06bb81adc81bdbf9368e5c72134394789      1
    11 0x43ac08ac562b78cc6ec17cc51ac4aa4d03b826ad      1
    12 0x4a992f5f5caf1a059e82696fbb9a3ac28a9ad602      1
    13 0x53f5f9ccb500e49f2a4701c3d402d1cac34c8684      1
    14 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    15 0x5a6f6e912ba4887ed537bb67e2696066354a07ff      1
    16 0x5c68f2b527598eeb251f64374a46839eb68fee1e      1
    17 0x5c85b6e4364189af994edd34af49e7586808006f      1
    18 0x5d55fb429167208ce248870f5ed47bd48820960b      1
    19 0x65b5ea1e9df2f92f4fe3bdb6f2cc8550c608a534      1
    20 0x66805d8b82664acab4cbe0c0498889dde9af7841      1
    21 0x73fec1c7e6bb02154ab578dd0f4a0f42c1ca7f3a      1
    22 0x762da606029d3120735aa1eec15464e265db7a3c      1
    23 0x79d8708b071adc332ce260ad263d26b89d2d34a3      1
    24 0x7d544a853dbcd39a53315e7002f4951a6d2f080d      1
    25 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    26 0x8888888888e9997e64793849389a8faf5e8e547c      1
    27 0x96e32dd09ff875fac038d41181cfbb2224a4573a      1
    28 0x9b98683433aca2542552fe68bad16228dc483e31      1
    29 0xa7dcc417c63f24f9073b667a5d7149bd38463d0f      1
    30 0xb048197de0e342a2c8c40f1e89b03448e8168677      1
    31 0xb7be3631d8de49e12dedaa26bf8a05daf54297a6      1
    32 0xba1d258082e8758f06adb9314c31d3934186bef2      1
    33 0xc0b9c3c9890ff4b7b5264d36c911781890a9b32f      1
    34 0xc185ffb12406b8bd994c7805ed0339ce9f2529ec      1
    35 0xc2e7baeee8bd51a69b2ddb3e34c507203fc9d338      1
    36 0xc6af121c3fd4121f0bbf78fddc95900ee9cac46e      1
    37 0xcc71ef1e9a21e71603060800cc107c1e8a7cb99a      1
    38 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    39 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    40 0xf91d0854d75012bf33c19f4ca48afa1c3c63ff69      1

## Allow Random1 Memes Phase 1

``` r
c(allow_memesRandom1_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random1memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0064f02799ea7748a9b51b5e78bcd274d9e7d0a1      1
     2 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     3 0x128ad42b82c752c5b4c7f679231d8956c98038eb      1
     4 0x13928eb9a86c8278a45b6ff2935c7730b58ac675      1
     5 0x147f466fef6bd04617fc0c8037ea01c73bc25a3f      1
     6 0x14abeea8db07c46b37a79bfa856d4c2da6d3e6df      1
     7 0x185952b3bb31da0ae18354bbb90ae40adc840c33      1
     8 0x19f1a4a606a4741df1a8b6a45c33a29f43c4553a      1
     9 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
    10 0x2177da04f9479496c2292d6344d306aa49beb34a      1
    11 0x22e8efe40ddb7f13b17b4c10f768967fc7a9f875      1
    12 0x2801dc73a6dcefb364b959606e0c61234105fd5a      1
    13 0x29c2188c6c318ab5cae4ae4b11a49edf2ec9ab0e      1
    14 0x2a0a412e0a0d436cca7ddba177c4dd9f29801062      1
    15 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    16 0x2ec4a2bcd4f33c7c9aafab7cfa865ec15508bf62      1
    17 0x367dc97068ab54ba1dfbfc0fad12fbcb7b3a0d09      1
    18 0x3876be5be4998adecbfbbad26604a762467e7f42      1
    19 0x388160a99390392278afdba240046b8b5e73f77b      1
    20 0x417c269b82387ab605bdfbd312d91baf03dc8516      1
    21 0x420ecd4ec65c0fea87d8dc5d16c2476c42466b65      1
    22 0x4220132c9df1ab7bd2913f0fd03297c90e7cc6fe      1
    23 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    24 0x47d539d4dd9b6f21ccabc5c96bbbf7071290938e      1
    25 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    26 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    27 0x4c48d00997c6d23d6b47a9ce8caa6027d0068e42      1
    28 0x51a2d3572577b6c47186c82165ab8a61e046dc83      1
    29 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    30 0x557c60995797fa7b47be105227a2e46148d85750      1
    31 0x5663d7d0751db1a80f01d6974e02036b6d44d56a      1
    32 0x5de26d392ea46ffc17131042e2584fe6ba46206f      1
    33 0x5f656037e30a003862cf878db24ab5f537177fd9      1
    34 0x6d1db4a7e83dae0eee7e95d421722d46d2a7e94b      1
    35 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    36 0x774a34da2ee2e2d242180819f1ee88783215f7b9      1
    37 0x7ca00a09e3b431d48c30e3e6cceaaeaf6385cc81      1
    38 0x80a1c9fdc26199a69d190ebc8ad287ef48758977      1
    39 0x8623a32af48544b055fb6ae64f33eb43edf091ff      1
    40 0x8854b06ba346a703a3c043e2e7b8822db3ca6b3a      1
    41 0x8b4d0402f7b2f063f255214b7095b5911a257a30      1
    42 0x9233389b9afa0a6e3bb403f9d211c23a3b290d69      1
    43 0x933522fbb4c067652730bbc8e25d7820f4d72a60      1
    44 0x9b742d1e98e5bb2f4d50f9fbbc047daf288ffc8b      1
    45 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    46 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    47 0x9f6ae0370d74f0e591c64cec4a8ae0d627817014      1
    48 0xa1b669414d40e68c11652b1cd82381f2a6495b89      1
    49 0xa711818b11bdd5797042ade80e3a59687558a4e1      1
    50 0xa90aa01ec98f390cb86cd1846f7c0fd342a07955      1
    51 0xab3ba2d668215acd23f7da923683a5ed88bad625      1
    52 0xb00b18b3bf6ecc43e6235ee69424d4a220437a4d      1
    53 0xb08f95dbc639621dbaf48a472ae8fce0f6f56a6e      1
    54 0xb31a4974499daad3255206daee7d1f3595fa259e      1
    55 0xb42ab92f5af0f162fffefa2b1e12702ce3fc9e17      1
    56 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    57 0xba12fda058a14eb03c14613601c3a30d6f955196      1
    58 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    59 0xbe9998830c38910ef83e85eb33c90dd301d5516e      1
    60 0xc1966d218b7492dd9cd672a5a237cef4d82004e5      1
    61 0xc2e8ed8cc0be70f91fc9aa903d5f4538719d7dec      1
    62 0xc5a2c3a524be0792519655d4093b44b988db4f68      1
    63 0xc78cd2e1e8ad4a288eddafb139c9d0891ad01ae7      1
    64 0xc7bb15c11595c877302ddfb330a4082d92f5bcd7      1
    65 0xc8cb180415b83f3f9a61691d246fe39a74c6a41e      1
    66 0xcaf3365d474690a1ac6643d3d6ef44cb0c6deec4      1
    67 0xce8ad80ce1a979381d209ac230d02adafb9fa897      1
    68 0xd193a7d7b231c204b76b9d638768ea602de515f6      1
    69 0xddfd836f7c9e42055b1f6ceb005fee4c7882f161      1
    70 0xde112b77a1a72c726d8c914c23dceaa53273c351      1
    71 0xe25b24cebed3055236e369570a437a99e1d32602      1
    72 0xe96ba1a10f72b58934b9ac0e3fb471d2ba65b757      1
    73 0xea0ed16746257eb2bc11de2fefd63cdeece23a98      1
    74 0xee2c055f7706b9dfcd98cd5a23d5629d6316c0bd      1
    75 0xfa977a0125c16a658a59ca1e59c7c48d58142226      1

## Allow Random2 Memes Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random2memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x02b294ed96e1175b8c71071a303e8a7d2396b017      1
      2 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      3 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
      4 0x11924c505ecab6a2434bdfe94bc0ff1ca081facb      1
      5 0x16f3d833bb91aebb5066884501242d8b3c3b5e61      1
      6 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
      7 0x1c172d05b75178fc669d74407243cc932030f139      1
      8 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
      9 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     10 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
     11 0x22fbaa2dd20b848084545515fb04b9c846942779      1
     12 0x23ae72f8336aca747ef02d596403de56cca489fb      1
     13 0x28e6c1352950238be088ef2a741f3c8b91b9ffad      1
     14 0x2917e947fb1925abe184d4062520a11bcf2a5b36      1
     15 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
     16 0x2a462fb59a5b0885b60ad31c5fe473d5ba0d79fd      1
     17 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
     18 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
     19 0x31bedb3962ab3a1d2a2e070853aa5c4acdb734f4      1
     20 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
     21 0x32ffe815277ff53dd2a73557664e229899e6501e      1
     22 0x343e85ac3009ac01538d4251900ce1a2b7d7ffec      1
     23 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
     24 0x3d3af253b037d3b22c4c810673c5d14de16d1af3      1
     25 0x3fa5a25f48ba1b736761706801be4f639ca4853e      1
     26 0x45360f55024132b3110166e1b327170daa2cc299      1
     27 0x47dc3a7aec5c0e1f4b2c71303a4b1eaa0bee3e4c      1
     28 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
     29 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
     30 0x52690f90740621f89f58521433e9b0921d626708      1
     31 0x527bb834cc5c8ff730c673880e51372282b06e14      1
     32 0x542c5648d46462207fba8a4bd7d890b4154b722f      1
     33 0x57bd982d577660ab22d0a65d2c0a32e482112348      1
     34 0x5eefc3ae27be1483ad6b864b45ed72fc39e6bb6c      1
     35 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
     36 0x6140f00e4ff3936702e68744f2b5978885464cbb      1
     37 0x614b89f072ea263a9387460963142e73548fbaf1      1
     38 0x6232d7a6085d0ab8f885292078eeb723064a376b      1
     39 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
     40 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
     41 0x6ce52498047b8279ccc7c25b41c95cd482525e54      1
     42 0x6fdd8c800cbb81790cd48a20f90ce7e725f89c27      1
     43 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
     44 0x78e37b881d078d4b2f90de0422dadfd4da50ed4f      1
     45 0x7a231b6d33f1da74e9fb593bc785b4914a852867      1
     46 0x7ae3b0627dac32d3ee16c204ef791e37067b8460      1
     47 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
     48 0x7dbba7f0551aef5894fd6ee331b95dbb4349f5d4      1
     49 0x7dd14501c25c221ffe213d7d56d225f4fe411038      1
     50 0x7e737324cd6702e1b93c9240072ec9cc036dce21      1
     51 0x8043812aea5a07dd6523b5d83abd5e606422944f      1
     52 0x843708d85621273f3bbc643b348da3a60d5b0334      1
     53 0x8586c32dbe1c986bffb8f1ed6c3e8c6489f4fa3c      1
     54 0x8668681d2017c2322d7ba37070a8ea223ef3729c      1
     55 0x8874174a2366668d54fea6343f71709389563c8a      1
     56 0x8a063961e57a68a8a1f68930d758e1bde05fc6b3      1
     57 0x8b4567fa8c4715c27a682215a031033a8b544206      1
     58 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
     59 0x954d65e5310607e12f93f6156de92cd37ffcae8e      1
     60 0x957143815f0e1d1b1a31b2bfcded5b416ee675ed      1
     61 0x95999c47c3e32a337ef108d657675c2757a606ed      1
     62 0x97ece7185467c78293f3b796bde3704421d4fa69      1
     63 0x986d1bfc94671b5633d90d0540820bd3813e3a50      1
     64 0x9997e55f76e34f14d7bd6b268a2e16f896389ee8      1
     65 0x9f4fe19fed4a008a270840fe95660f25df36c734      1
     66 0xa0f1de4882a5cd1851989d9a1e9bbe9b4604e9a9      1
     67 0xa32c38646299818ccedc6401818c2e1639c39c08      1
     68 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
     69 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
     70 0xa59422ec0634a0263bcdfce5e29dd2df3803447e      1
     71 0xadebdeab678647a457743ea3af98f8b804e45c24      1
     72 0xb335326c7f2cd2e4eb09ce3d1745c92f66497b7e      1
     73 0xb4627672ee52660a9e453ec541834e04583f3602      1
     74 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
     75 0xbd3a5d12af9fd82fb927fd26216e4dc2722d2337      1
     76 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
     77 0xc04208f289d3842ac168f2c373b3164e1d872650      1
     78 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
     79 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
     80 0xce990032a3ed14e197421270a9bec9e276bf2f31      1
     81 0xd1afbe4da2a5adc6faf30fcbea9e4eea0ba8c70a      1
     82 0xd2b9f9d1e6734f241b2a6eb64ef7f6e63af0a97d      1
     83 0xd36590461162795ee33099b2076a0d4e017ae17c      1
     84 0xd3b37c6567e2702aa727cff4ce5939b4a07a2fde      1
     85 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
     86 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
     87 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
     88 0xdc78107155918e230246439e4159fea4c477eae9      1
     89 0xdcefc9ff1e47e458e56959154c1cdcf56003d30b      1
     90 0xe26027e219998c0acfbd00b74795dc850aee244a      1
     91 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
     92 0xe418a9a5e49dde0d13e1ef51d4bfb7fcc51c28df      1
     93 0xeadd2d51e75a8b42302adc2aff0157a4bf626adb      1
     94 0xee3aa3fa5b780bc73da73a82d47fa2d8cdc1a647      1
     95 0xee91d62eb5aaea933efbfd0790613af5db305006      1
     96 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
     97 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
     98 0xf8be957f65e67fb0342992a51c30290d5758f880      1
     99 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    100 0xffe6832850476eb6d5ff184d747ed84f1b686aa9      1

## Allow Random3 Memes Phase 1

``` r
c(allow_memesRandom3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random3memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x017347cb75ac8725608e593ea35d83f9b2b3cfb8      1
     2 0x036aeaa55ba6cf87984a5f0a467b21c63b9bf9d1      1
     3 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
     4 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
     5 0x080ffeaf914180e18f69092d66de11925434b540      1
     6 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     7 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     8 0x0fb4e7dff29c22029975373d1bbf5873ffc3c61a      1
     9 0x118d6f5d61a9d881d452510f29c703abe9d80cdc      1
    10 0x156db724902f03f3986f6adfdc38f7696e4d5111      1
    11 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
    12 0x18eb4aece6cff56f3bd4ccd844491d55b6ab21d2      1
    13 0x1acb5e5c169aed4b9f963bdf786a8377bff678a1      1
    14 0x1cb89e486db5774ba084f683796286848df489d0      1
    15 0x1e0486ee85dd758078d75c674f3d28efc4c899fc      1
    16 0x1e31c76b78ff00e3cb64d41d5844626d991ab9e8      1
    17 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    18 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
    19 0x2da903666829f302b0501f76144339213259c260      1
    20 0x301e2d2a98c5873ac27fd9eae85f0153959100fa      1
    21 0x36ff6a01782501553486a4efe6ea6e07f8f3ae28      1
    22 0x3a0c596a40bbbb558d86b0f692884aadf1dbf20d      1
    23 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    24 0x47d278101ee9335c3a3baa14fe184f757229a7b8      1
    25 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
    26 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
    27 0x541db1ed2628f2f4897417d06181af6a179e90d0      1
    28 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    29 0x5c9e2a6fec34b510996a8e2a3d1e2c47a382a8b9      1
    30 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
    31 0x5e0737b90f1db90eb4f423dec067fd6c06a958d0      1
    32 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    33 0x67a90218cc5fc1adde45ad27a257f268023b2ecb      1
    34 0x6bae54650c0d17f4aee76839a7d319dc5060d6b0      1
    35 0x74bfa6fc87e23d64451f5bb084b6fbf79f46e495      1
    36 0x75cf6e0dc5294bef8cb5a6fcd23dc9badbfc443e      1
    37 0x791967551708fe99eb2cd8dd580ef98c61e67ac3      1
    38 0x7decf7a31168778f311c57b9a948abaa7321001e      1
    39 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    40 0x8037cfedb47d493a391dad76c4f60b8927cb8108      1
    41 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    42 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    43 0x84df1fb4801e72cf6958a994dc0b690f9e7cb23b      1
    44 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    45 0x86ff945e6dc2470317ca082a01de7d24188671ac      1
    46 0x8854ad314fdbcc275eb0d5f4fd6dd86ab7981cce      1
    47 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    48 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    49 0x9045ede9bc11bc1148a0743ea518e3f36f62bb23      1
    50 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    51 0x92c75284b0d863165a8b33e212f02ffeecb2853e      1
    52 0x953b0f8afc70e0fca9e2e43b0a4914be002c4e94      1
    53 0x956f7db2fdb52de9453ae184d51d1883b8cd9d68      1
    54 0x9f35af4727fb91114563d8a8f779a792cb557f3f      1
    55 0xa40117a1394fc5cfb3576fe12632381b0b79c0c0      1
    56 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    57 0xa7cafd18dd8bc1e23203058d66f89f0f0ee539d9      1
    58 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    59 0xb5497427ac120c734a76be1b85a82156447c7e78      1
    60 0xb67a577a1855dc3a43df1d94b05a8f5d29570f89      1
    61 0xb6c189179b204d14fcc0b608bc97243b389b0030      1
    62 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
    63 0xbc1eb4359ab755af079f6ef77e3faac465e53eda      1
    64 0xbe2f803bfbcffbcd77aae4e9104406abfeda497a      1
    65 0xc0ced4439c1f0871f16a46e13fbe41fbf61ba265      1
    66 0xc33164f7ecc76869fafd44363cd094a22e0c296f      1
    67 0xc3c1744bccfe4691e332c873f9cb99b53214e03c      1
    68 0xc58326c7020f26345f4568cc09daddf019a8e6d4      1
    69 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    70 0xc97a5623578a832354988e7e40869f5207193d53      1
    71 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    72 0xcea266acb92d5603dc53b5d6bc6e568dcde0d311      1
    73 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    74 0xd1380a4e089950ee3a23d818e24ccbbef003a432      1
    75 0xd2ce17b0566df31f8020700fbda6521d28d98c22      1
    76 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    77 0xd9e2ad004ac82915d7472448cef2b182547487bd      1
    78 0xda23ab0b2636c3f9806a868278d73aef2439ab60      1
    79 0xdbefee517025559e7898d3a48f18221c32d3fcf5      1
    80 0xdcab53e3c8a2c37cc5e2509c2db0caa6c04f3ce0      1
    81 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    82 0xe831977b52714501b52bada9034021a7cac79709      1
    83 0xe96eb4507a1d162bbb99301fe592d310e9489e40      1
    84 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    85 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    86 0xefb3da5189a6169a61176d1357579e135a1d1187      1
    87 0xf161ff39e19f605b2115afaeccbb3a112bbe4004      1
    88 0xfe8312a959e031c7d4cbe3f9cdd3ec8726d0d80e      1
    89 0xff08bbef8f2b6986018fe3bb62850aa7db843b40      1
    90 0xff1a6681aee53d032e8f5fb3d27ccf29d493e922      1

## Allow Random4 Memes Phase 1

``` r
c(allow_memesRandom4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random4memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01282b55c6be0b0d724d2a9324feb3c08229f2ca      1
     2 0x036302a75c0f7dafe15f83c8a1faec282a74a03b      1
     3 0x09e6267d0cb691fe647da5029d28ab0fd62d9325      1
     4 0x0aadeef83545196ccb2ce70fabf8be1afa3c9b87      1
     5 0x1003b47b2980ae3cf433165f81296ef747e4f484      1
     6 0x107d1198854fbbcb2e1014c3ffab166b2595586a      1
     7 0x11a880910eb629a109b0e586167796f7438fd4e4      1
     8 0x13e073537f9d820eb7b8bcd41edbf7f9df85024d      1
     9 0x1566ae673ae80725bcce901b486c336e6acef465      1
    10 0x157234e891ac0d70fffc76ff8e399c92ec264a5e      1
    11 0x17b2d1911262b9cda80aa5354a1c13ee9027ace1      1
    12 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
    13 0x20aa168e6c793646f60737399c8466dd643d4281      1
    14 0x21804e35a54aa4820e6cd409d70926a63dba3e45      1
    15 0x24d6ff72eccab41724d488a03e8ea721ec3177a3      1
    16 0x286cd2ff7ad1337baa783c345080e5af9bba0b6e      1
    17 0x2cd00bdf1605046cef0f50420831bf3b7d675882      1
    18 0x2f44fb58135ae5d3793803c73d471c7cde4bb774      1
    19 0x325d12fe8bce0f969f879679275adccfe8ba0605      1
    20 0x35a7378f37918969f7a733d46519de9bbdc6fb04      1
    21 0x3e77fe1e72f93e891691831e684997e0de75b35f      1
    22 0x47e054a8fcf3e27a4bed979c33ab65a5883fe437      1
    23 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
    24 0x4c3d85e7fc0c91215eb6a4186592a41d975d2a4f      1
    25 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    26 0x4e8849962c43d9d7540f64b24bf76704926926ea      1
    27 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
    28 0x52d232170f00607b555d97b943094b4ba866f0f0      1
    29 0x58058169d69f9c24a53ca88947cd2f9fc4e5561e      1
    30 0x592e480e0066a51eb981b532e275d6576e5730fd      1
    31 0x5b3c2035f87ecd710d363a8c9c982f53259c6edd      1
    32 0x5b8660a36d3af77830a35355658bde886dad95b2      1
    33 0x5df6dd99718f32ae362b41e78b7bbf66ebccc179      1
    34 0x6072b510aa765dd2f31975c9aa65dde39fd1a282      1
    35 0x6767b1e546dcb7499652a1fc4bd6f1e36992623b      1
    36 0x6a5ad95a3b0d6d4739de4370f51c8670a4d53700      1
    37 0x722e2e4c15be1fdded3c86f4100bc32b181827f5      1
    38 0x72f315c115408afd9240c5be0946c8ebf7261fb1      1
    39 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
    40 0x762da606029d3120735aa1eec15464e265db7a3c      1
    41 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    42 0x78130d139ee6c5c1b99a49ac70271fe696dc2b3b      1
    43 0x78143238df750e9d9f1082e12ed56f2bfa332d65      1
    44 0x7862e990963405616afd1e08cd369433a87adb3a      1
    45 0x7cd9ff84abf2b23e825154aca2a62fafe185bd5e      1
    46 0x7ddae9afcdf981729b18ad3ac0fcef50d9662c24      1
    47 0x81590c16b9d1495d133cf881acf5cc0b56730b74      1
    48 0x8212642c68a030bab8e4e8d43952c7c4c6cf2903      1
    49 0x8a730bcc5572d3cb7f7f45568161fb28b3242d15      1
    50 0x8c4a5c29a1e94a0e88f42ab172501a34b0d2c38f      1
    51 0x8e22df65a6cebf6709e1ca7f44c544a1c2fb5bad      1
    52 0x95d41776812c6ca222dd5fdea56c065b55ff7655      1
    53 0x96de627be6262ad2e19553824aad1af6ba1ebe9b      1
    54 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    55 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    56 0x9f64e6a575f50d7c8c5aea4be962f86ee2c5ca31      1
    57 0xa0a43e56c6df95cf01d089d1284987468f8ddcff      1
    58 0xa17bfb3a816996ac3a1e26dddcc7850663548c16      1
    59 0xa5b992a3374ba87a44917cf9c2e20c296d52f7e9      1
    60 0xa75dc095ed4ad69b088c3eb8ba2f93f1aa942b6f      1
    61 0xaa300c7fc927345162a00b83fca3ebbc5828b5eb      1
    62 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    63 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    64 0xb6fc3c8f4e5233f2ee2ea1ab84a4d10c61f6d215      1
    65 0xb7187511a0a0f5a63a538b7bc0abdd1a01979848      1
    66 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    67 0xb7ef5fb53dc42839270a94ca569536a7c7e7edde      1
    68 0xb8dfd425f4d6227036d5342cc4ac2b90826e1b05      1
    69 0xbe7a5ccea9f7279f714d4c9b4c5436dd38fb4fe1      1
    70 0xc659fd78a395d70ec69ff034b4378a712e7edb64      1
    71 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
    72 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    73 0xd413f436a036b9773d7adccaac10242e27b9da74      1
    74 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    75 0xe29bfd5761a18b5c3ac4a4271eb7fadd4c7fb942      1
    76 0xe4d3cbd20bd9ab79a70a1612853154cb80b02961      1
    77 0xe51748456ea9759c19d232a32f96ba3c1c110776      1
    78 0xe781fe7a6f65ee6b3efe66ed8f7f5c1e9f01cc55      1
    79 0xe7a8a504a82f0a829656261a54d0443f85e19604      1
    80 0xe9ced9e75a01ee2eeddb9eb1922089309091247d      1
    81 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
    82 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    83 0xf54d81b2955832474a87a8dada4a042a05b83180      1
    84 0xf599a05b4ea846a5afa1db274b508bb1ba1ddd93      1
    85 0xf5a93410e7e32bbf28a8eaafbd7f241cf0b290fb      1
    86 0xfc075a5fc4ecaeb1ce66995e054bacd7c422f2ca      1
    87 0xfdb325752f9793ae7fa4aecc641cf5386846f566      1
    88 0xfe73f0b55ee4b411d7d1e4d5d5d4f8834064e2b5      1
    89 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1
    90 0xff8991b1fa6b84e141bde964d442ceae0348040e      1

## Allow Random5 Memes Phase 1

``` r
c(allow_memesRandom5_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random5memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 90 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x01e92264abaccd5e77be8a9a0ed5f1115fa36163      1
     2 0x05e07f4bf676bacfd8d211895cb080e5711c0a24      1
     3 0x061ae2b316ba7808763be9032b23c0a35a64a2be      1
     4 0x08eed2c77011faa1da376c5fd14fc33825053554      1
     5 0x09881c99a4c81dc07ff5ac9c8f8ceb46087cdb5c      1
     6 0x0daac372398974373b29cb61c31deb11afa7ce23      1
     7 0x134309c4cf57bfa43ef66bf20bd0eeccdeb2d80c      1
     8 0x1536b993645ed5c0b731011f8534554646a9b588      1
     9 0x1df4b6d97eb84c007f52cbfd942c987a72e94ce6      1
    10 0x1e39ebad76f75a514ebe5432f4efa2e8e50bc52c      1
    11 0x208b4a8ef875a5e4645e23f27343f47fd887d9c5      1
    12 0x24c238108f5a770663c4ab4b004ad5dc6f2d64d8      1
    13 0x26a6434385cd63a88450ea06e2b2256979400b29      1
    14 0x297baa7d8b38330349250799af489e35ac1ef9c1      1
    15 0x2d4bbe602c6e868316953f155ee3adefc70d4113      1
    16 0x2dba88cb3b435f99a3e58b6e0fe450e8f1a3f20f      1
    17 0x302522150ef22faafa6bf6530b330b5a4bc38369      1
    18 0x310ffe8defda3b8fd626ef05ef0b34cf0363e90d      1
    19 0x39b0b2bb9c5dc3c63a715c16f9115d456a212780      1
    20 0x3cc8837cd778d12021a4e7baabab0e06d1d1fed2      1
    21 0x3e02aa6b648394769d9094838cdc100962d33e72      1
    22 0x41dd1c6338e5b3cdf9dc69e20dad9834ae36a6d3      1
    23 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
    24 0x45d2bafe56c85433e0b9f9b50dd124ea3041f223      1
    25 0x4874211da929dd87e28a4267c4b96a5d0ccf48a7      1
    26 0x49ff66d90c67ca99feaa5e8e473396d5c25aa71a      1
    27 0x4b46f1e241838a910945a3fac242ddf6f5d7c041      1
    28 0x4c327ee3ae154220513cddd0292d166bdb414426      1
    29 0x55157ce961289be90889ebcd7a0494b438a1ff6d      1
    30 0x56d90629aa29830e361e113e53a979f95afe93c1      1
    31 0x5a5f9d7e7f487e71d08df5e0e103da291820a529      1
    32 0x5aae5802046733fa533ae09bf644ce359d83fcc7      1
    33 0x5dd47005f8a429b228a5d30b5345a1b64fa78c0c      1
    34 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    35 0x6a3d9412a705f5531b029ef4a7b8040e1eb84ad3      1
    36 0x6c2ef68148f96f0ff50499743b8acff699ddaa72      1
    37 0x6cb575052aa5c146de209ab813c3f6ab82424bcb      1
    38 0x6e7cacc6f2b49dfb980663bf2bb014046ac45320      1
    39 0x70e7a6621f4cb3c3e073d0539899f49fc88424c0      1
    40 0x711402390e3d26fd137ebfe72ad3909c6e30926e      1
    41 0x78887448976b93443125cd9ec40a9485684c759b      1
    42 0x78be57664220d2c0b6b583acff9f9d339647a15f      1
    43 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    44 0x7ca83da48606cbfbbf12af0294dfb7433c0393ea      1
    45 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    46 0x7efe40097e4322087d7e262b3798175261513e5f      1
    47 0x8181dd699e486493027e4e21bf6d0c7b7c94055e      1
    48 0x81ad1a19187f0f3444c0f8bef98ea49c1b9fbc03      1
    49 0x88ceca090d7d810f0d3bde0cdfb1cf1f2301bc17      1
    50 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
    51 0x8f9c8d7f8d44fd2486bcf0605efa5fd0c397a658      1
    52 0x8fb17d9e2f2735fa5e5095440f418e4a05ee22d8      1
    53 0x9ab69183b05f5d9ca7e7d88c2081b281f259101c      1
    54 0x9baf2d175fc6d5c7f1490069c2db6cb2d9525363      1
    55 0x9eea866c15ad425aef0e1a0179843a8f98341436      1
    56 0x9f9ca0285dcc35cc6831652e79c029fd0ed4bc75      1
    57 0xa14964479ebf9cd336011ad80652b08cd83dfe3a      1
    58 0xa2e05ee0e72e0226a79d68d7eb7f8425281022b0      1
    59 0xa579f766406d0e18d8c7ff81d6e2db1dc3063943      1
    60 0xac79d2ca0d415dad95090b5b9f5cb5704cfac071      1
    61 0xb406ebb1a43cdc9c6eb5b392c712329c0b84d546      1
    62 0xb7086b0ef4be397bcf06e14e82a34bdab3f6b014      1
    63 0xb7bd84b71fbe6f2ade449508b2b78cae45a18dc0      1
    64 0xb8a69fd9b077b1588cc10d807efc4618df22b99c      1
    65 0xba0b5b765d3d638eb560c7e08fe17f360d8e5005      1
    66 0xbd46f6fa9045e203dfc0cd31d19294dc2011cce2      1
    67 0xc07f2785adfdd4ffe9288a1ab77ed5e147ade0bf      1
    68 0xc23e873f9c9b968ccdff0e413710d865ef18b860      1
    69 0xc455347ff5c5bf06008477864edbeb1bf225cdfd      1
    70 0xd191441efd409fb3ddf1992b0af2f9955d447c97      1
    71 0xd573becb6a6b0a0d43065d468d07787ca65daf8a      1
    72 0xd5755a4276a53ee7ca2703c6e1967af59cbc9feb      1
    73 0xd6ce702632f94069e138d30836194b121f047e79      1
    74 0xd76d392d17379c22ca5e49d167bad1dcaf7eba0d      1
    75 0xd8e93ccf41f079627f40573152bea5178041e1be      1
    76 0xd96a41af259755aebbd7d13afef763dfe8197e04      1
    77 0xdaac4d315441be67b136f163060865ff0493674c      1
    78 0xe1391499f4c04cb574f8acd7e33a25cc4c819bfa      1
    79 0xe25c73435702fed11e9c5584ce6efe7cbff71739      1
    80 0xe373d8e1cdafd885029faefa4b0d66813d353353      1
    81 0xe38e8198a4e87c5f0f6bf96d9d3cd3053010a5f7      1
    82 0xe48ab528f2b51fa68e22d57069cffafcd4aa2b6c      1
    83 0xe8ff464b954d12db575ad0e5e5a7dc9c041ee6d6      1
    84 0xeeb2abcf0ba8f51fbf6cbaf357d4193c105381c0      1
    85 0xf266bd362edec2656ff30d797206d1ca608178d1      1
    86 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
    87 0xf353b57ffe0506a44950805395e5412f42181dd0      1
    88 0xfbbc953b46ae6ae37392999707f996a868d40f20      1
    89 0xfc58ab0c3d7810b05af4b347653dab8187e22c1b      1
    90 0xfe504884abd05cb7fe828afb689c06e2a5b4fe64      1

## Allow Random6 Memes Phase 1

``` r
c(allow_memesRandom6_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random6memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 69 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00172a79a73b9dfd5871a92a0365280186f7eff4      1
     2 0x0341e935efa5150b84003869c9212ea68aeb3b5a      1
     3 0x07e92b07f74a3d021534e2f0382a9054411b8b2e      1
     4 0x09276485fb976693e79ed9cf8defa435deb3be1b      1
     5 0x0a605f26a5b6c37b27fddb155ccb495fd4f128c0      1
     6 0x0e7a408a775af29310970de51b59501e21eee87a      1
     7 0x10eea07f522633557a4eb00bff16c2f466d49983      1
     8 0x15619273a8826b4bafa17499c3c49aa5c7e78d96      1
     9 0x15e3bf5f0158afd884a85defea6ffa9056ef871f      1
    10 0x19748446a67b690ef1dd13ee61a615e9028bc6e0      1
    11 0x1c9365a7bef98384401d513bbfa3416ebfe99578      1
    12 0x1de387d8d4d7000910761d7030f58568d207b56d      1
    13 0x213594d974427e8c1bf4f4f557b7b6f4987c50c5      1
    14 0x22111e321c58fc8e9fa009d3263364d37dc6e685      1
    15 0x2256e6d1c611c74005b435d0f34cee9b6034a1dd      1
    16 0x26fca9a99e95be90447a25a1da852d870417aac9      1
    17 0x2847c232662d53404529938e7c9357e34a5051d2      1
    18 0x30c1f8d8b7d34acd0827e4fdc7b2d98c32287ba1      1
    19 0x31b23128eb1063faf10ddb10e773aea0a50db236      1
    20 0x34a1091227dbf7abcd563792ff28086a454b11f4      1
    21 0x3a89cd7d1cf3eb657f70954f0011548675c7132e      1
    22 0x3ce53518159c99c7f47dcbc0426fe375f824b945      1
    23 0x3ef055a7b8d9132b6638cd108e65ecd44d597abd      1
    24 0x47450b6044689a96cb603308d8da10f0ae501aa7      1
    25 0x48488c36c8643f6eeb13f6bcd5be8612918c6590      1
    26 0x5263d3df74cddafafa867d404a94dcb4e3c50661      1
    27 0x5322d25a83fedb9055fbdfaf3e485821438871dd      1
    28 0x578dedd91786c6b7989c87539729b6f4d17d910e      1
    29 0x5b60608b5a9930114a3587a97bc93b5091927380      1
    30 0x5f883431d7467e42b1df641ae7b57144b01bbbfa      1
    31 0x60356707746ea5a24f78cbf6172090fb4ae4d53a      1
    32 0x669ecdde4a56480c48bc5a7f243cd94072bd5f94      1
    33 0x67f9960a1ce533c90ebbdfcac29c2d0ec322e030      1
    34 0x6c5f56ec2cbbe1c9a8539407b121834d2c8f8dfe      1
    35 0x6cd6604bef8786ba11d5b04777916d3ddfa313fd      1
    36 0x6d5f1fc2799d285f26951ad0d2bffd6a85cb2b6f      1
    37 0x6e15fed85e0f5918a4a8d087f25eb0c3b706e524      1
    38 0x7132f37e94e8752ed37f3239597e3d927ccc2d83      1
    39 0x782de3f99f9c73c125a5e6b494373a3c68a2a914      1
    40 0x7c8f072015d4e29c24088fe55e62381406bd71ec      1
    41 0x7fdc3797c6c4e0683e40964d526d2eec89686cc8      1
    42 0x810b7d663f688b1d146c149a9d0718547b103a65      1
    43 0x8cb382bf9c743e74937645e2f5a2459b65812fdb      1
    44 0x8d9d8d19e396dda0250eb9b0eb8dd631e612190f      1
    45 0x992e69dd674045be990f023a540e63c6f9105e5c      1
    46 0x9a5d5f6d17b9d9093f1107dd6da934c292a601cc      1
    47 0x9bd91abdf39a167712b9f6c24d1c9805d5fb9242      1
    48 0x9ccfd6d678d1b467fceb11cd115e6c7afa323460      1
    49 0x9eab4b2feed9483838d2e9b7c1ad407a18624df4      1
    50 0x9ec11f051e8cf2b3a4ae9c8d93e8729cc3b966b4      1
    51 0xa5f09c6f40aa9b8f7ad70d64c42e20df1ad1f0f4      1
    52 0xaae33d661bb2604158748ea861280231600cfe61      1
    53 0xab51ad23d222fd0afb4e29f3244402af9aa3c420      1
    54 0xad0f4b5bd7682aa7b87c94c4dc7671f37fe00c78      1
    55 0xaf8fa5568931769e99ba860e78baee7f9522e76c      1
    56 0xb23ea43e4811be9efe93dbe8f6b0191883342ce3      1
    57 0xb66be9ff7f3be2a847406e312b86bf7a32e94e3a      1
    58 0xba2b941c148f6ef7dfd1f2838e095396c6cb7ac5      1
    59 0xbe2127ebbd7c2a3e6473e213749f212b4f97c69b      1
    60 0xc6165f99edc566743a0528b1d8bfc6a038e8e4fc      1
    61 0xc915d266472685f1ef2f974c5f01b4915f0df46e      1
    62 0xcc0dc0a1d51bb8a8a593fd9a4a2eea1e7b688c39      1
    63 0xd42545bc62521805c2093a30df6bb5f71f030590      1
    64 0xdd4dd1a8fd014861a7a705f31f0dbb0528043515      1
    65 0xde178a961723eb49654d20cb8482e54d46c58901      1
    66 0xe1c7580accbccc3490345ed5c6833ec170200599      1
    67 0xf35acaf05e8cacff748a4fd2127786eabf60f24f      1
    68 0xf4cd60a92a7d20997d8dd3ed30eb7b340f05f135      1
    69 0xfb89213242d1043e560b4d0232a75b4f524e74d1      1

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
     1 0x0a98f97e89743406a8611e1b4219a073b60ffed3      1
     2 0x0b3e3a27d85996e20b2cdd0e7f1e1208816d05b8      1
     3 0x0ca4dc095608c6b153019f734106581c85f2dcc3      1
     4 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
     5 0x134cb213549c9f114176adb9e4a78d6a272a3672      1
     6 0x1566ae673ae80725bcce901b486c336e6acef465      1
     7 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
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
    26 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    27 0x59068075a799594db03c0255eed68e8e121155c8      1
    28 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    29 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    30 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    31 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    32 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    33 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    34 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    35 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    36 0x69e68074f1aada957edd39c5eae0069973343f30      1
    37 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    38 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    39 0x729fcbb6e1289c88ce5113ec1c83a48a8e3c9f2f      1
    40 0x73bcb8c5e30bf85806aade7fc36f16c6b80fd3b9      1
    41 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    42 0x78a576d3751e8cc8f30c54c367fe8e5a2cd71228      1
    43 0x7e5ab36876a267560e7191cedbe99ee7bc04bc30      1
    44 0x82139687faae8a29851902783e02e699de0e0846      1
    45 0x85603a042455d2f36f3ec52c1c756344aa70c5ef      1
    46 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    47 0x896b94f4f27f12369698c302e2049cae86936bbb      1
    48 0x8ba68cfe71550efc8988d81d040473709b7f9218      1
    49 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    50 0xa56c04347abee42f663eff9bc2d0147b97c8f782      1
    51 0xa743c8c57c425b84cb2ed18c6b9ae3ad21629cb5      1
    52 0xab2056903a7b62bac46f45a3d7a70ac799ca88cb      1
    53 0xae0d16586e5d60d334624c115216a52b9b1a0335      1
    54 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    55 0xb6cf25f5cf8a1e1727d988facdd47f1dfc492caf      1
    56 0xb735af7ae1a77d1ec764c862c2c09bdbf2b34b27      1
    57 0xba4575ea27041d99e6614ec02318f1e23a623fe2      1
    58 0xbbdd72fcce73c2626719be00259ddffef0d5673d      1
    59 0xbf270918afe2ad16093ddce904fc358ad337cefa      1
    60 0xbf814810b44978de273191fd612aa47f7b69d564      1
    61 0xc02e6b0d0c1a5d8cd26beeba0fe8d76c5d2f19b9      1
    62 0xc2419841dcb9a0f8906d06463ae24e00e1470846      1
    63 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    64 0xcaa1c396e70384db58dd33be74b26fb119e98c3a      1
    65 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    66 0xd007058e9b58e74c33c6bf6fbcd38baab813cbb6      1
    67 0xd1f6e5592361257aef68b96df42aef064080c5cc      1
    68 0xdde27b3ba448e15357183060ae006bb54ebb3d86      1
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
