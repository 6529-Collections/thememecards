
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "memesphase1.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:1657        Length:1657        Min.   :1   Length:1657       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:1657       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 16389869 # https://etherscan.io/block/16389869
block_hash <- "0x07f10c446ff56debe21eb8f4ba97ab9135f0e716f73346411115638cf0410b20"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4488 

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

allow_memes1       <- pick(snapshot, contracts=c("memes1"), address_pick=100)
allow_memes2       <- pick(snapshot, contracts=c("memes2"), address_pick=50)
```

## Allow Memes1

``` r
c(allow_memes1) %>%
tally() %T>%
readr::write_csv(file="allow_memes1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 100 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      2 0x02955d69d33f1d811a8431b237841813f5a07338      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      5 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
      6 0x08f8070f1a17791dcfda507d2e1a47bd78b6cdc6      1
      7 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
      8 0x0cb1b32f096302c93488f072ebd3c500d2db850a      1
      9 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     10 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     11 0x0e0e34094e42f47fe25da3cc1b441faf15f8551a      1
     12 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     13 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     14 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     15 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     16 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     17 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     18 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
     19 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     20 0x231595e3673a10e846803194f4982e1cf3389161      1
     21 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     22 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     23 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     24 0x24fbadccd6684106e24065694ac87b0e98819235      1
     25 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     26 0x28d2e96e6a94a37bdecb82579a4e01e873bc1d09      1
     27 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     28 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     29 0x31231036ed51376a70b3824d7da1a4e036def5a4      1
     30 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     31 0x3334c740aee8c9e6a0c6f16ac057b0e9c6a73479      1
     32 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     33 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     34 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     35 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     36 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     37 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     38 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     39 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     40 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     41 0x493485c7b822d077f14bd6484a5c40f2adc91c96      1
     42 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     43 0x52f0d5805045a14579bbdcd719e0086c5ef3574d      1
     44 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
     45 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     46 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
     47 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
     48 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
     49 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     50 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     51 0x660105ea6435e8d4c887a3c830b8812746eada30      1
     52 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     53 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
     54 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     55 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     56 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
     57 0x71784687d4c74338bf284bea22956c74fbe6d631      1
     58 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
     59 0x74c2e5ebf5b7d9d1f43425c77b4237533dcf66b5      1
     60 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     61 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     62 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
     63 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
     64 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     65 0x8f3a658a9736530e89c242ef572d7198cfd540ea      1
     66 0x8f4b933491e1319799229ed9d36b179bb859d705      1
     67 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
     68 0x94345607093b249b5100e8d1e499928dc163dfdc      1
     69 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
     70 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
     71 0xa222204acf1be4077d34102fab38a759060b77c2      1
     72 0xa4df50f02a778bf281ea0db761900d354449eb17      1
     73 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
     74 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
     75 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
     76 0xb12897740478eec7b86b9ebf14245cdacbba4f2f      1
     77 0xb40969f60bfa246a09593099dace2ddd543254a3      1
     78 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
     79 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
     80 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
     81 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
     82 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
     83 0xbe52b15ccabce3b33e4e631ef285399ad9de46d8      1
     84 0xc1b0307ff325527511310c7d6fd3248188742f86      1
     85 0xc8ed349529bab23ac6c726693623cefa2e31ed98      1
     86 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
     87 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
     88 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
     89 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
     90 0xd9390b46a1749dee5325a490490491db9a826d1f      1
     91 0xda4c79ccfdcbfa9128b1e328f77e5f9c6ad72a44      1
     92 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
     93 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
     94 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
     95 0xea9f3a983d965e582c34eb852c18babac52050d8      1
     96 0xeafc83bd210706fc3930a32dcfd8920e953c3622      1
     97 0xf78f9f879e0c3ca488fb83cb276b0a9bdc42e103      1
     98 0xfb541dc23e3b4359275d37e9e8ea3dc54cae5ff8      1
     99 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    100 0xfeb41f7dc10c98fb5a7995fd9c5de7c83e02dde7      1

## Allow Memes2

``` r
c(allow_memes2) %>%
tally() %T>%
readr::write_csv(file="allow_memes2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 50 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
     2 0x074396d67d04c48fdbef50af95df216185b63cd5      1
     3 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     4 0x11c1238607556c6b23592aed09e469028702e9f8      1
     5 0x123cd3fbcae960bbb0e7f68593b453186065c6d6      1
     6 0x14b072f1954dd88131271d597a30e9899b57eb0f      1
     7 0x1a051968e421023d61a0afac659b6916784f251f      1
     8 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     9 0x1f2dd54ce97965c6aee5d860118c326d68a21f63      1
    10 0x22ed7a936d3dea108004e3229f3bd3d84c7225db      1
    11 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
    12 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
    13 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
    14 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
    15 0x42e82d04bb7b7159e717195385e28fd7ccacca44      1
    16 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
    17 0x452f438aad8b675232c1fd7ff8e940d72d8a9f45      1
    18 0x4839f0a75ede31d26613c7cdccba89be774e9856      1
    19 0x4cdb9c3499f31ccb63da5374877ee2111440f648      1
    20 0x59aa4d2140ec130a76cd69cb0974cec4dbd110a3      1
    21 0x63a612c0b8dfc559318e39daae6d0c7d09212965      1
    22 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
    23 0x6c359ab465ec8f893fb6c4243810f4d8933d71b5      1
    24 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
    25 0x715fd9d95c5302e4f7febd8476e3f9c6db4c918a      1
    26 0x7e840de1d4987a189eda6440b9282edd452403d1      1
    27 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
    28 0x8396a631492c60696ba4119c54e0fa616b2ffbb2      1
    29 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
    30 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
    31 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
    32 0x87e5a5c79757d6e1d9c0586b3dcc7663e5095701      1
    33 0x8c0a11eb047c1097c821c159b8e0c2c5f37f81bf      1
    34 0x8dc287825d71f758bab052608ba8a4f156f84176      1
    35 0x92ff64df0d05cbfde1913cff17a531f068ea0672      1
    36 0x9613a1f63bb8c3cfe6eb36cfd7b731e5fd07d322      1
    37 0xa5ff2bbc712fc114c29f3900ebb81b7713fe131f      1
    38 0xb6ea44d485a6e1468830689bdde59a2a1810de83      1
    39 0xc322e8ec33e9b0a34c7cd185c616087d9842ad50      1
    40 0xc7c32e908ceca0c308e6c6d58050d233cd278661      1
    41 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    42 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    43 0xd683678594e7b41281b2736f1beb13ebc5b06ec7      1
    44 0xda015d7b426755d7c28a8069f1c94321b5e70143      1
    45 0xe74207386ac0dc2091da54f39b617ee0720efb69      1
    46 0xed6e663b8d2192c515ff70ee0d6291e44db83be9      1
    47 0xf1133ca2ff1b4e5df7121566c0520199f8937e78      1
    48 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    49 0xf6f85d9b96a43c87fd29e2facbf644df6bb029b0      1
    50 0xfe53dccb4db09e660b2dc5ec48eaff8bc18124c8      1

## Allow MemesPhase1

``` r
c(allow_memes1,allow_memes2) %>%
tally() %T>%
readr::write_csv(file="allow_memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 150 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x013cc394844aa993d73ca9a08d89dd6d046f3bcc      1
      2 0x02955d69d33f1d811a8431b237841813f5a07338      1
      3 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
      4 0x0387876a09a45581109eecab2ff4ac049e76ba37      1
      5 0x05a13dcf55ea6d532f15284f39e02811fc183a8a      1
      6 0x074396d67d04c48fdbef50af95df216185b63cd5      1
      7 0x078cc4caa131eb9d6d3e31847946d5f37fd3f58b      1
      8 0x08f8070f1a17791dcfda507d2e1a47bd78b6cdc6      1
      9 0x0c76717074a973f3f6e3d422166c999483f6c7fd      1
     10 0x0cb1b32f096302c93488f072ebd3c500d2db850a      1
     11 0x0d5668339c3c17aa7f06ba7fe2667536cf15d748      1
     12 0x0d6b4f304b91a3b67b1db34776ef7e95effbc729      1
     13 0x0db7b27b7a3ea8035d3f66f3ba7532221ef06abe      1
     14 0x0e0e34094e42f47fe25da3cc1b441faf15f8551a      1
     15 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     16 0x0fb7f9eb8e9bee4dd70dc334b94dee2621a32fb3      1
     17 0x11c1238607556c6b23592aed09e469028702e9f8      1
     18 0x123cd3fbcae960bbb0e7f68593b453186065c6d6      1
     19 0x12c3d9510bba6fa40c55d4f30e1be25f5f953e22      1
     20 0x137f41ca27f570d4c50e56d5c20ae2807225f09c      1
     21 0x14b072f1954dd88131271d597a30e9899b57eb0f      1
     22 0x16ecafb3b5d8e15d07bf8d3ff3a3f9ab16cda860      1
     23 0x18412b9f84df461bdc0f2e0709df507d5bdb6363      1
     24 0x1a051968e421023d61a0afac659b6916784f251f      1
     25 0x1c5762f9899b46c19d4c9b8fbdd38d35856227f3      1
     26 0x1ee5106b2233169b84dad2acdbf498b29c3c7d15      1
     27 0x1f2dd54ce97965c6aee5d860118c326d68a21f63      1
     28 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
     29 0x22ed7a936d3dea108004e3229f3bd3d84c7225db      1
     30 0x231595e3673a10e846803194f4982e1cf3389161      1
     31 0x2380ca49ed8e933c97905977763693e5cf8770f4      1
     32 0x23fed2634fdc629c1a82eb6ed3391efad2a58670      1
     33 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     34 0x24fbadccd6684106e24065694ac87b0e98819235      1
     35 0x28cf5d9d465dfaf5c616958ef8b23dbee567e2b7      1
     36 0x28d2e96e6a94a37bdecb82579a4e01e873bc1d09      1
     37 0x2be9f18c46df11f05b581131af4d339c20c7254e      1
     38 0x2e299b7faa9b906117181a4550d828b1300b48d1      1
     39 0x31231036ed51376a70b3824d7da1a4e036def5a4      1
     40 0x317c32ca0309bb8a1f9d8a3e2836544f233c841e      1
     41 0x3334c740aee8c9e6a0c6f16ac057b0e9c6a73479      1
     42 0x3368b4733a4552b38b7d81c215d88cac4c78d8e5      1
     43 0x34e03545e5527f54243ce5a4f88ebb202efb0ae3      1
     44 0x3543493a76ce5ca362dd6eeef000547de52b6875      1
     45 0x37769c5bdd7a43b0b52958ca51bdace1556c96bf      1
     46 0x378bcce7235d53bbc3774bff8559191f06e6818e      1
     47 0x389458f93e387fc568ca4568c231a64ffd0456d2      1
     48 0x3927e502c865a2c873a735775e224930eadfd2e3      1
     49 0x3927f1b51afd205311bdddeb5646c4c6a309b2b2      1
     50 0x3e3721d26d5b8612bcd6504696b82401b9951ba6      1
     51 0x3e998c50b1eaf1577416e2bf31a76d09a48bc3fb      1
     52 0x42e82d04bb7b7159e717195385e28fd7ccacca44      1
     53 0x4313ca73355b0b098a78d406b42a0d86296bf0f4      1
     54 0x44f2afb689aea37ab17feb2420f17f9a3e851307      1
     55 0x452f438aad8b675232c1fd7ff8e940d72d8a9f45      1
     56 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
     57 0x4839f0a75ede31d26613c7cdccba89be774e9856      1
     58 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
     59 0x493485c7b822d077f14bd6484a5c40f2adc91c96      1
     60 0x4cdb9c3499f31ccb63da5374877ee2111440f648      1
     61 0x4d6aa3da789ea162a5978193bd584d3067227835      1
     62 0x52f0d5805045a14579bbdcd719e0086c5ef3574d      1
     63 0x53edcefe31da6a0051df17ad80e19ff93c490b17      1
     64 0x54669ca58bf3a59caea8ae5135db491e4738f65a      1
     65 0x56543717d994d09d5862ab9c6f99bce964ae664a      1
     66 0x56a061f4ef706e1f6dcdbcf7e10e4340c1d99bbd      1
     67 0x571b8db726c2a29d237f88c9efeb896290bf395b      1
     68 0x59aa4d2140ec130a76cd69cb0974cec4dbd110a3      1
     69 0x5b8ed97b1e85ea4e5e96c3a6e00b0835b087fce5      1
     70 0x63a612c0b8dfc559318e39daae6d0c7d09212965      1
     71 0x6430c11231ec6723e5bbe6ab258f184f5af7c836      1
     72 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
     73 0x660105ea6435e8d4c887a3c830b8812746eada30      1
     74 0x6b539c723716628725aee9e7e26796cfe1f38fcc      1
     75 0x6c359ab465ec8f893fb6c4243810f4d8933d71b5      1
     76 0x6d8dec3fd68d94a7189a98346ea52b4d32e00012      1
     77 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
     78 0x6e88c16794cb212bdcfd6b6f8901af2f83237d3b      1
     79 0x6edd8fa5550e29d47641f969d2ead3decaa29ba2      1
     80 0x70814fb113d5b2b22902b568b67ebef657d9d66d      1
     81 0x715fd9d95c5302e4f7febd8476e3f9c6db4c918a      1
     82 0x71784687d4c74338bf284bea22956c74fbe6d631      1
     83 0x71aa6c4e87225fcae2df49f977c7fc0d1e8d2112      1
     84 0x74c2e5ebf5b7d9d1f43425c77b4237533dcf66b5      1
     85 0x7546c60ae8d65dc6dd7a0f61c169818059ef49db      1
     86 0x765718a8e4a5562be34b122af259ce5a71372e12      1
     87 0x7921dcfa9f14dc980d7e8f946b7bd223624c3db9      1
     88 0x7e840de1d4987a189eda6440b9282edd452403d1      1
     89 0x808a023b72260170c95d831f589a1ae0dca1e43e      1
     90 0x8396a631492c60696ba4119c54e0fa616b2ffbb2      1
     91 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
     92 0x867db38c83f4287a71ffd0cdfba6ffae93150ff7      1
     93 0x873dfcbbc61ef7ebadb652adb98df9f69399b70b      1
     94 0x87e5a5c79757d6e1d9c0586b3dcc7663e5095701      1
     95 0x8a0c391ed12dfbb23d4a926a7c7353dcfcca8a47      1
     96 0x8c0a11eb047c1097c821c159b8e0c2c5f37f81bf      1
     97 0x8dc287825d71f758bab052608ba8a4f156f84176      1
     98 0x8dcd5fa47d7ef22db41b2b66504923ccca5065a3      1
     99 0x8f3a658a9736530e89c242ef572d7198cfd540ea      1
    100 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    101 0x9224bbb4e0fbe2f2f8fab55debc41eb21fdfb804      1
    102 0x92ff64df0d05cbfde1913cff17a531f068ea0672      1
    103 0x94345607093b249b5100e8d1e499928dc163dfdc      1
    104 0x9613a1f63bb8c3cfe6eb36cfd7b731e5fd07d322      1
    105 0x993220d8cc0e48e7b88c96ec7f37cc3b6e6009ce      1
    106 0xa1fbc0a9ff967ce51b8ea952b35ff7005cd4cc63      1
    107 0xa222204acf1be4077d34102fab38a759060b77c2      1
    108 0xa4df50f02a778bf281ea0db761900d354449eb17      1
    109 0xa5ff2bbc712fc114c29f3900ebb81b7713fe131f      1
    110 0xa817b9bd1ece360e4a1692894a5ad1d40b889f20      1
    111 0xa9237c4eeebc879208e59c867cb6d7cbffc1df30      1
    112 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    113 0xb12897740478eec7b86b9ebf14245cdacbba4f2f      1
    114 0xb40969f60bfa246a09593099dace2ddd543254a3      1
    115 0xb5976803fe7b4fa10834e98433f38ba208c53c0c      1
    116 0xb6ea44d485a6e1468830689bdde59a2a1810de83      1
    117 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    118 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    119 0xbaea3cf94abd0d6e0f029ef5b0e54e9424a72985      1
    120 0xbd751cac1fb27d930a6a0394d97c77908d94ad5d      1
    121 0xbe52b15ccabce3b33e4e631ef285399ad9de46d8      1
    122 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    123 0xc322e8ec33e9b0a34c7cd185c616087d9842ad50      1
    124 0xc7c32e908ceca0c308e6c6d58050d233cd278661      1
    125 0xc839de0fed241607d50aa4107ae582443b906e4c      1
    126 0xc8ed349529bab23ac6c726693623cefa2e31ed98      1
    127 0xcd094518f2d30fdd8c7911636bd8b52bd5303066      1
    128 0xceae4ca7d082856d140f3672780029d6e90c3dcd      1
    129 0xcf803a49f6e37b92a4040e397b976939dc5d9841      1
    130 0xcfd648eb66b736351f48dbd5a1752708371c10f4      1
    131 0xd38a87d7b690323ef6883e887614502abcf9b1eb      1
    132 0xd683678594e7b41281b2736f1beb13ebc5b06ec7      1
    133 0xd9390b46a1749dee5325a490490491db9a826d1f      1
    134 0xda015d7b426755d7c28a8069f1c94321b5e70143      1
    135 0xda4c79ccfdcbfa9128b1e328f77e5f9c6ad72a44      1
    136 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    137 0xe2b76c9f15a1e5841a91b404ab4be1c3e5d19551      1
    138 0xe5fd5ec4695b93d7e43f5d96657a535e0ff499bd      1
    139 0xe74207386ac0dc2091da54f39b617ee0720efb69      1
    140 0xea9f3a983d965e582c34eb852c18babac52050d8      1
    141 0xeafc83bd210706fc3930a32dcfd8920e953c3622      1
    142 0xed6e663b8d2192c515ff70ee0d6291e44db83be9      1
    143 0xf1133ca2ff1b4e5df7121566c0520199f8937e78      1
    144 0xf3290df945e484a4ff846fb5baeb6d1147a7aa69      1
    145 0xf6f85d9b96a43c87fd29e2facbf644df6bb029b0      1
    146 0xf78f9f879e0c3ca488fb83cb276b0a9bdc42e103      1
    147 0xfb541dc23e3b4359275d37e9e8ea3dc54cae5ff8      1
    148 0xfc0ac71b8ca9b219b3c74625755f62202d19ad39      1
    149 0xfe53dccb4db09e660b2dc5ec48eaff8bc18124c8      1
    150 0xfeb41f7dc10c98fb5a7995fd9c5de7c83e02dde7      1

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
