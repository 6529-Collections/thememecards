
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance    contract        
     Length:7872        Length:7872        Min.   :1   Length:7872       
     Class :character   Class :character   1st Qu.:1   Class :character  
     Mode  :character   Mode  :character   Median :1   Mode  :character  
                                           Mean   :1                     
                                           3rd Qu.:1                     
                                           Max.   :1                     
         name          
     Length:7872       
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 17519469 # https://etherscan.io/block/17519469
block_hash <- "0x49523b4d10819f72c639a3d352147d66da82abdda245aba97ba69bceee3504a5"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4688 

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

allow_artist_phase1       <- pick(snapshot, contracts=c("SuperRare","Solana"), address_remove=address_remove,address_max=1)

allow_memesRandom1_phase1 <- pick(snapshot, contracts=c("memes1"), address_remove=address_remove,address_pick=75,address_max=1)
allow_memesRandom2_phase1 <- pick(snapshot, contracts=c("memes2"), address_remove=address_remove,address_subtract=allow_memesRandom1_phase1,address_pick=75,address_max=1)
allow_memesRandom3_phase1 <- pick(snapshot, contracts=c("memes3"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1),address_pick=75,address_max=1)
allow_memesRandom4_phase1 <- pick(snapshot, contracts=c("memes4"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1),address_pick=75,address_max=1)
allow_memesRandom5_phase1 <- pick(snapshot, contracts=c("memes5"), address_remove=address_remove,address_subtract = c(allow_memesRandom1_phase1, allow_memesRandom2_phase1, allow_memesRandom3_phase1, allow_memesRandom4_phase1),address_pick=75,address_max=1)
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

    # A tibble: 21 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x143b53a2a83f5a8b8a323f64429f46062197d78a      1
     2 0x1e0a980dc878e8af6ed79ad189a7334d2cd737ef      1
     3 0x3852471d266d9e2222ca9fdd922bafc904dc49e5      1
     4 0x3b7f246d8340de4fe0495bfd243fbea798503c7f      1
     5 0x55205fbabda8036697dd087135440dd92df3ec36      1
     6 0x5d7b71cda3ce450b5db872776c3524e574ee0b7a      1
     7 0x639a7110633f0109e811f9065ee6a35c56436d79      1
     8 0x71999edf2e6af09716e026295c3a350904358c75      1
     9 0x77d25628e427bc4596c1a9e0de6f562bf57a1768      1
    10 0x88885b8ec139eaa41377cb3677ca8bcb49850301      1
    11 0x89ebafb42708850ca66f70bea69efdedf7c8347d      1
    12 0x90f5dc8364db3a6083f493d9d4814b9ba9b47237      1
    13 0xac6b7cacc01a811fc7577b8f9e40755cb572956e      1
    14 0xb67b3d7b48c457e32385c7bafe033b08fa8f334a      1
    15 0xc00c841353deb87f25b41ab792e3757e27cb1dd4      1
    16 0xcb8343b134d73afbd9fb94efb71b4276e80d02ac      1
    17 0xce90a7949bb78892f159f428d0dc23a8e3584d75      1
    18 0xdf43c410cf1bfb8bf4876e5f366bde6576babb38      1
    19 0xe0b4bfaea4ce41d50449e7e3f8d3a53ffe8005c2      1
    20 0xe8270b17f341871e814a157142f0fedccb7a9a92      1
    21 0xf7830c1e0df63386a20058ac3a811beffdb88abf      1

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
     1 0x03f4cb9e297ea659f30e09341ee7155a7d136398      1
     2 0x04294157cfba9ff0892f48f8345ea3539995f449      1
     3 0x0a87cbf5eec20d8d9880ad21f0260522ff5a675a      1
     4 0x0acabdaceab63c08bc5aea63d7e07da7c2e42959      1
     5 0x0c481cd7d09bdd222351d52622d272367cb3d159      1
     6 0x0f9ab846449ca068f9714818a6ed0da5d819e9e4      1
     7 0x111818a51c4177e8980566beea68fe334be7b76a      1
     8 0x18595f049a9c5c2019efda113009b0ec1c89ceaa      1
     9 0x1a7f174dcd4211d41c7c4d7dbb3a0e4db5780c67      1
    10 0x1dcfdcb24456a7dba5342479cff0f9ff30145fb8      1
    11 0x1ecded4519a9688ef4bfb5f15811c52f048ba1a6      1
    12 0x20ebf9ae7c66da763fc1ce86062ce883389c8e23      1
    13 0x21b9c7556376efdbf9ee316a4ede0c8257fb7533      1
    14 0x22418183c90741bab6606c0e0d240ae6c3a148f0      1
    15 0x2be96c05fb7e81b020c42086b69d21bbf427b53a      1
    16 0x2ee5fc834903186949c75e0dc10ec245397bf1fa      1
    17 0x32a0d6b013cf8ecad1e37e99532570411d398d05      1
    18 0x35d7dd9230a1f079c907b53e7c5822b34e197a1d      1
    19 0x3bc161e3a5604165f3165ed8aaaf0d490e559324      1
    20 0x4b6c1d196e06446eb0534326bbfb02cc3d073a2b      1
    21 0x4c26c796abcf3513807efa54d54c34405b84a36a      1
    22 0x4c941a23ec94580d3b5442aa8d5851583fd8bcce      1
    23 0x506452ab0dacf04db0ab1544b6be4019651ada90      1
    24 0x527bb834cc5c8ff730c673880e51372282b06e14      1
    25 0x614a62584658d555cc28b3aabdb691e85e002c8b      1
    26 0x62ba9de7d81b96d697fc2f26b5eb647f184f9b2e      1
    27 0x67c6083d540696c6aa1cf31101d78780e0d6bb47      1
    28 0x692d6cf19e0c082185e20ff5ea6bb267e7aeb278      1
    29 0x69c8c2923005d26eaeea9500d7602eff8c81c848      1
    30 0x6e5632d334b861dfe4312c7ba34e175b60be0b5a      1
    31 0x70c8db61d09271f4c90950ba8c6cbaef918f12f2      1
    32 0x7c741ed01ee259feba4b7f9cac114f48bcafacf3      1
    33 0x7c84aab9f3eb0897191d95afb2aeb56a63973532      1
    34 0x843708d85621273f3bbc643b348da3a60d5b0334      1
    35 0x87e91205adab9e2b0be9f7562af3f5b42fad0838      1
    36 0x8856dcf8d1f9be083a25251b0149ad74f971b806      1
    37 0x8ffa8d00db5df56a20c30b737c5efdaabe140df4      1
    38 0x97ece7185467c78293f3b796bde3704421d4fa69      1
    39 0x9ad53944b52e271f1114028b918202cfaaec0a3d      1
    40 0x9bd69d51526fc9db09bb831f5069a48cbe4d3421      1
    41 0x9e1f7b007511036b5cbe5df6d39550bdd2e8bc99      1
    42 0x9e640badecb7c628c6188b74488823e879f42a1a      1
    43 0xa04f4a4b7306cb72f30828834db01699362a4989      1
    44 0xa1b669414d40e68c11652b1cd82381f2a6495b89      1
    45 0xa4b61e227361a9cd9e62ca10946c27748a382cab      1
    46 0xa5214719feb1d07b66292a5d4d0a2712bd048435      1
    47 0xa6d4758ef49d2ae8e65e1a02216cb9744aee6b23      1
    48 0xaa1d3f5d45333e40467e989d472effac4da00da9      1
    49 0xaec3f7e735505c2b787b05d67f4adaefe3dfd678      1
    50 0xafb4fccf58bfc06ebc8ccd387b68140cca05f4c5      1
    51 0xb196931ec22517b0510705eb56d5652fe73877f0      1
    52 0xb37c5603fb0c6c2e2b62d0525affed9e1e5a6a19      1
    53 0xb4627672ee52660a9e453ec541834e04583f3602      1
    54 0xb8937891f08af9854a5ae7a5ec0cbaf4e68acd4c      1
    55 0xc1b0307ff325527511310c7d6fd3248188742f86      1
    56 0xc6411ff69f1ec6bfb4b082f47db61dbedcab250d      1
    57 0xc6cf5a020bcfc65c6f8181c04cbb5ef5050fe28e      1
    58 0xcabd5f77ca9d48f4ef9793f20de42ad39ce93979      1
    59 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    60 0xd5ec003289265705727b622f1700fe814e54ca67      1
    61 0xd7e32b8a326ffd9e784a1ee1eea37684a7512171      1
    62 0xd8f3d5e9a199a35429870fca8befd159bfe48747      1
    63 0xdbf55298b349984a50fa98c7c6965994e30fc3f4      1
    64 0xdebd22d7f63648dfb69bb6e2a4c88264d578c0a4      1
    65 0xdf36b093cbc06522b68105c3d9141db188e980b6      1
    66 0xe2d22dc1c2f7c58f86606e405325c69f5210a6a7      1
    67 0xe8a05c7f4e9c1aa060cf941dbb50381f342d7d43      1
    68 0xee05f658e18eb04d250f829b1920c2fbf6907e27      1
    69 0xee91d62eb5aaea933efbfd0790613af5db305006      1
    70 0xf1e1c701b49b1dc2405a9e8ef40f9f88802b80fa      1
    71 0xf4141aef39803327497f6b81a21bb3f2acfa2436      1
    72 0xf624e9324f9b330cc0289775d7b91e945e881134      1
    73 0xf6fb4e6cdd7ed50dabd90d10b1ddb60a85720737      1
    74 0xfed52d251e31178ff8bf4a0d611721c544f74fc0      1
    75 0xff4f0dbc9ee7583c0c61c7333a5daf8d0ab4aeb3      1

## Allow Random2 Memes Phase 1

``` r
c(allow_memesRandom2_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random2memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x03024d05b1aea10b64ff3af1fed6a3df9adeebb7      1
     2 0x03c5e3ff9514031f910c9bc9ebbc7839e02b23d8      1
     3 0x09b2f0988b6d24336b348c84c2764d2b4f85142f      1
     4 0x0a7669015fef82f5136c9a2e44e1ecbd2a5aec19      1
     5 0x0cb5b9eeed8d51ae57e70e46b2a3596b94ece6c7      1
     6 0x0da4438a9cc8d41fe9c0c6efd302a173ed967da7      1
     7 0x0e3e001a00c07e2cb3219d1ec0c47cc532d87804      1
     8 0x1033caf4e55579e8aa1cc59c3c302d7d924f9f89      1
     9 0x129c6695dfe7a906bd8fda202d26dfff601f83a4      1
    10 0x1af369bc1069dd286ff59cd69553550c07e0dd05      1
    11 0x206b0526f4866e7580780015cbacbb4a8dbb8c80      1
    12 0x231595e3673a10e846803194f4982e1cf3389161      1
    13 0x24fbadccd6684106e24065694ac87b0e98819235      1
    14 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    15 0x29cd8d94a4e564a6d6576b467ea0bb9d51c9f05e      1
    16 0x2ca75a892b59bc7cf863ba261e49ab40a4844ee3      1
    17 0x2cfdaf06e39e5d9d3c9afe20362b725b41f68725      1
    18 0x2d924c0bc8033e7866b90ca2a76cbf4b5714b11b      1
    19 0x2da903666829f302b0501f76144339213259c260      1
    20 0x2fb24b1021e51fa23db91a9b995000181fda0963      1
    21 0x30f2a414945ba487f6a9ca909d0cc0919c6a1812      1
    22 0x331b4a1242ad6a794ef127bd61fe57d7bdfdbb80      1
    23 0x3a10fd1387555cd75db8142393fbaba98efe28d4      1
    24 0x410a9f44948576c60f94b165f63eb79ce769a04c      1
    25 0x44b4d430e7c56e4ca04c5799e561063ccc1f1df2      1
    26 0x45360f55024132b3110166e1b327170daa2cc299      1
    27 0x455ce1afc1403b728789b4a8c5aa512600b668d8      1
    28 0x4581c619ae0556b774f71adab6831a86da1aef17      1
    29 0x487869b7557f0fdecbc5141da1d7c48c15c5b8eb      1
    30 0x50d086175cc14fa5ae008591d72a93cf22b48e32      1
    31 0x511c85042524a41bbf30e7a8533a84b8c5e55f43      1
    32 0x53b2da71db2e266882a465d9aeeaae3e8beeb9ee      1
    33 0x571493b6bb0b99dda41e2da1774b8c9b5bc302af      1
    34 0x5bd832dc1a5ac61f687b87c4199b844819a4d8ed      1
    35 0x5d89737e854c860d25e106c498c6dca0b516ed7a      1
    36 0x5feadf8ef647da7f07584b3d33fcd565b79359a4      1
    37 0x62af590649b7abe80f8c6dffbb92e0875b65e37c      1
    38 0x6e1ac9b3f4499a73385dd8d2daed6449406d49f4      1
    39 0x75005a05bc163b85991b9c9facbcf3155372422e      1
    40 0x7f6ca49d1e50671a586a76bb082dd6b3f73fef17      1
    41 0x88ccdfe9dd047b4cec4c1102d2d803e2d8bf683e      1
    42 0x90af376346ca97d4e9d1e510e82543ef99b56a28      1
    43 0x9197f339cca98b2bc14e98235ec1a59cb2090d77      1
    44 0x98172111480cc81621fd8b12bed2fb5095be11a5      1
    45 0x982d3c5223f6b5794fccb3208eb164d042cf2526      1
    46 0x9feba45425c51b03890604d6ad51f4ee7c9d4866      1
    47 0xa21e1e6c69dfed02b6a69c4c89ea46a1e6e31b86      1
    48 0xa39d385628bd00438f8a9ba4050a25a0210f84eb      1
    49 0xa94ba6f81ede0d81a5252e71916b5c5567ad6a01      1
    50 0xae72c6a6fad9fa9d82d089e1ebf73b3043855425      1
    51 0xb7546d1b04151e1e96d38275af66eaf984fdad2a      1
    52 0xb976163a6f251c208bcf3196b9c4ddf927b63e72      1
    53 0xb97624935dd3fb84e5492e8d01c6fcdce8060cbc      1
    54 0xbb59498b19783e5d81f72ad07acdac619b6808e2      1
    55 0xbfb7969e75ee4e58620f74135d167f5bdd60800a      1
    56 0xc13d5024c2ee14c5f80847afd09275f8b550a135      1
    57 0xc4bfc1dc25827f39ad4a66bfc968456c7852a9a2      1
    58 0xca339bd4739227f71b646f8b23ca098c86f6c3a5      1
    59 0xd3069c9e486a8b77add55ae3ceb3a4b138fb76c7      1
    60 0xd361363a6401870ff8a23b7c77c004aecb8c8b84      1
    61 0xd7342ea20a5afbf24352b5ca61e09844167914cb      1
    62 0xd7d16a716f6dfcef1e9553bf12136463f6414f1a      1
    63 0xd7d941ff2890bda98f40a5dda0593d239a603f09      1
    64 0xe23e0bd6e320c87367b0e4b797b42dc9b4fe7ca0      1
    65 0xe28470253f0c9c7afbae7f61795d6b1ca4644b2f      1
    66 0xe776de32c9dc8889e2dcf1cf0606010a51d1abb2      1
    67 0xeb8661e8548dc074b9ff411080fdd9a377e6ed1e      1
    68 0xebaedb897fc974f35bf36f075d60cd75e46d2d4c      1
    69 0xec47cbbd9e05f8c4d587eb604f41740b0b2f33e4      1
    70 0xed1e25178d7a6438c356d3459fc3efa9458ad52b      1
    71 0xf36796fc4f8fee589ff959264c9e99ca37a1b659      1
    72 0xf54913955c6035a12465e24ba2f162df2a026d9a      1
    73 0xf6109779752d1fdb8b6063a158bed1b02000b700      1
    74 0xf681efdcba35c69dc0fe56b7fdb56324f5abd385      1
    75 0xfcdf3c2b2b6ba00e6db59c70c424b27970ff3974      1

## Allow Random3 Memes Phase 1

``` r
c(allow_memesRandom3_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random3memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0150b0ec3c7b72879fbfdbdd79c18a8368b194e0      1
     2 0x0256c28028cac3e504f1b6c20bf7b3f4631e0d97      1
     3 0x03ed8e6d50bff0b8c49675f0bba94087d5e579ac      1
     4 0x17726f58ba7350a33d08d1bbad623e155fa2daa0      1
     5 0x1da0f2a58e65d98a8d04ba3da56cff9f5ace5615      1
     6 0x1e0486ee85dd758078d75c674f3d28efc4c899fc      1
     7 0x1e6ef3e23dbf38428e752a7293b698b189c7317f      1
     8 0x240d478106bbfb613b923be58c9269c7d90a41b1      1
     9 0x25acc72796bdf4579755708fdbc8409622d224f7      1
    10 0x269c2d0badff4e365ac2a0da8c8e3f186b6adaba      1
    11 0x271c6f480aeb26672b915a7b33159ce8f1e97fd3      1
    12 0x32606c4aa95ea78d7c5b1e0e347e3e9ae451434e      1
    13 0x343193017fd217d19fd983e31db701385c8504f8      1
    14 0x35cbedc3412d3c93dec4afa60ddf38c2a9c38865      1
    15 0x39cc9c86e67baf2129b80fe3414c397492ea8026      1
    16 0x3a30fa661820bf261b39f41a63916cad97b20e60      1
    17 0x3b748a60dfa1d58eac080a5ef24b11a082edb6d2      1
    18 0x41e9e61a699a85daf12dadebbb7e6e517bee6b54      1
    19 0x4525b5402a6d9eefc656e828b9391aa9dff2446c      1
    20 0x46e6aa05e0867d5f0feb749e81e005f5567ab317      1
    21 0x4bfb94bccfd860e1f9d85c10a8949a722676fc4a      1
    22 0x4d6aa3da789ea162a5978193bd584d3067227835      1
    23 0x4e3cc03eb2be6122f2316b84d1c7edb900e90dba      1
    24 0x4e61548d94c8649ebfc2f5f54d2272fcbc687bf2      1
    25 0x50002a9b8e9938d509de84dc3eb3aabfbec1451e      1
    26 0x57dec6115e561f3de9fb7429cf14de429713b3d6      1
    27 0x58f606e6a973e57165bb07057a75f047f42455a7      1
    28 0x5b1022d859a46f42fcf24bcd8f5ab616e52b4800      1
    29 0x5c99e810eb0d87f17b0e467de52b3b2b4b2a2dca      1
    30 0x6215e708a7b61f5ed49642f692f28ba09b807725      1
    31 0x632734882ed0127fbdf2666478df42aa916bdc84      1
    32 0x638fba3bbbe8828394d2b13f92fba75bf32c8efb      1
    33 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
    34 0x6b4b2cbb69b9ca07328a40e7ffff3870f627c8fc      1
    35 0x76ea151c88fd91902f7bf3f829db65dc9ba5d45b      1
    36 0x77acac99ac831a1574b9db4d15299e98e195e6ae      1
    37 0x78f0269f5b1ca914c8c58993f41181f89e212b27      1
    38 0x7ad225751ed67118e9f54f87e08f64e43ffbef0a      1
    39 0x7bc7481751d7fcb76d08f596b03ba1e42378ea27      1
    40 0x7c5504688eb4f81ed558550066dc218ffee1f6c8      1
    41 0x7cdbb8e309ce7c701ed200be123f16c6f7c9e525      1
    42 0x832834fc5b0b35ffd4cf697c4ec08815ad8d6a52      1
    43 0x83ca0f19abd240c3b04bd55a2046c308d042e196      1
    44 0x85914d049aa004d037cae65bad275ed4147f172e      1
    45 0x8b17e8b3f8315e73986f0c8370cea4f3a974a532      1
    46 0x8f4b933491e1319799229ed9d36b179bb859d705      1
    47 0x93c838add3ac72b42f65417408f900a7155e1bb8      1
    48 0x9a659894e5d115846767db0e1685744c452e7a6e      1
    49 0x9d52fd3f58557f3b5fcd2b10314bf566cabca60a      1
    50 0xa73769aed346319287410811639ac3bec8464d55      1
    51 0xab81be93f28664daf0c1638b483e4f9d2539446e      1
    52 0xac6e4334a82b8434b0122bc3c9b42d0a5f190d4a      1
    53 0xb7abe0f0a1c31a88fdcdef71033cf7ae7d12f2d3      1
    54 0xb8efa945cf7f35854afb80e9ac05f5c1dc172fb3      1
    55 0xbdc765d81db7e0ca47fe6bf57e987966a2c204ac      1
    56 0xbeddf3e43015f5221f5d1ac0fd0d2ab3352d2f7b      1
    57 0xc0bcbc0a91cd42e4808faf190e0ad6e9a6c027cc      1
    58 0xc6bd194286802c5975cf681201e41f21a4e256aa      1
    59 0xca8299a5f070faf59e87f6c4df6e7627d565e006      1
    60 0xceb775d4b09c2fba2fe51efcab6e39a7da1528c3      1
    61 0xcece625176cead11c8d2dd6149fda2496b0c192d      1
    62 0xd1042637cdd71f99f3dc5877030e67f3789e1c19      1
    63 0xd4100a2e1aacdeb18bcda92a280124a7111a12b6      1
    64 0xd89c641cf9cca44a3435895d24494d1bb7d70ee9      1
    65 0xd8d97db2a8caec33d58420aaf637cb41058cb58d      1
    66 0xdbc7a4afadbff03d1ac183c9f664a95775c6d52e      1
    67 0xdc36237208adb46959e77a0052843ce5446afab4      1
    68 0xe01a97fc4607a388a41626d5e039fdecbfc6acd9      1
    69 0xe39bd9e3af2054dd193b0d0217cfbd460f104902      1
    70 0xe4000d4f3f0e59ca00b803f54784fe0654a192f4      1
    71 0xe64777dcdca11fd8bde96ed05e7560ae789504b6      1
    72 0xe74ca58ef724b94fac7f2e09e2e48536a5c1ad03      1
    73 0xf4aa005cdd64b2c80e6cbfc30c427f80b5d1f0b4      1
    74 0xfcbca52b23db28470eb97680d18e988d8b60b608      1
    75 0xfec4465b6aeac9c90e552ae7766bdc0ee0d8dbc9      1

## Allow Random4 Memes Phase 1

``` r
c(allow_memesRandom4_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random4memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0ee7d116e0c89aaefb3c3653cdb71e384766ad11      1
     2 0x15a415c7589efaf71345006d33bfa07a9a42c8c8      1
     3 0x1c97ba3997c34ba30b6ce2e9cfc3eba0b544d752      1
     4 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     5 0x20357eb5dc98da197c332d9fdd2a38ef58c91fb3      1
     6 0x21d79c2be4ab3de2802a60da66f86d497d06102b      1
     7 0x277ba29263bb09658bbc973ce6c8f63019901e5d      1
     8 0x29722d00967e6131dcd1615b4a56bb3fc00c06be      1
     9 0x36a0d4dca6d768333f916368b73bc689438e18a1      1
    10 0x383a063d64a5103f775720a7bfb8ecc42fc73199      1
    11 0x3a90face21a9c77ffa0521c734c5e409f6e5b7f3      1
    12 0x435e598e5fd4477fbd0379e2f56afd73f272574d      1
    13 0x445816ca9bb0146940b802d49ce146787c73b5ca      1
    14 0x478bb542f7658d635abba67edb987806dff5b83d      1
    15 0x47e64ae528b2d1320bae6282044d240ff67e703e      1
    16 0x499dbd98f447b660dda53dcbb9376a5165071ba2      1
    17 0x53887f0dee06c6459bc928f9f39beccac3947325      1
    18 0x5b93ff82faaf241c15997ea3975419dddd8362c5      1
    19 0x5ebbde694f3e768c3604d75a932c682e83217cdf      1
    20 0x621fa2a2dff1012ce2989e583c407d060ef59754      1
    21 0x68cbec6e76a8cc835ae75cce5feff1f36cbaf764      1
    22 0x6b3ab283947afb581a461770d7c52d47b2624788      1
    23 0x6c5d86dfafd74031ea5ea1c7315c8010304df411      1
    24 0x6cb575052aa5c146de209ab813c3f6ab82424bcb      1
    25 0x75d7d7972a62b00ff7ef071741c929f59d185ee6      1
    26 0x79dc911ba6f8424a4b6158ea0b24168a8e0e0fe4      1
    27 0x81845ec7df4fa5776998053457b8c4fb1e60cf84      1
    28 0x83ff31faa1fbe034b1bf17ad33759ed85da65e2f      1
    29 0x886e16654fecf66c056960811ecc50fe43701494      1
    30 0x8880b69c95c0183bd3d4f30bc4272e457552c3d2      1
    31 0x8e611b4f46a353aae8fa55af10b5cec24f5f1db8      1
    32 0x92ff64df0d05cbfde1913cff17a531f068ea0672      1
    33 0x931e8194d361571cc8476b15f8ce2f6e72d593f5      1
    34 0x953cc221d2d43ba9b7eb9bdf17617aea39cea774      1
    35 0x968e708d939feb55d914e233cf1097f4adbcd5e4      1
    36 0x9969db4034a136650cdb07955cdf5635499a4012      1
    37 0x99c7be4c85c4e9d26b7cab28a4e6dbfc8bc19178      1
    38 0x9aa91eeed8e34d7ed2145d651f76fae3e15371d3      1
    39 0x9f49288c4658660c82dd98019f600a3d35969fd0      1
    40 0x9fdaf0bd765561fbd609ea28ea67a39054cb28bb      1
    41 0xacb1568482f9af61568d5fd05a54bfe8b7e60ee3      1
    42 0xae220d647426e368ac321efec83a9d696bf84e7a      1
    43 0xb356ab415aafa4b688a6c457e61a4a8f7d5096f0      1
    44 0xb38bd68a6f0ba599808d65c7a9cf2a428105b680      1
    45 0xb8c3e2e2a3465b74b7a57f8921a1c3f6e69302e2      1
    46 0xba98ef31cf6fa06cfa5489f73e2ec94bb1319b9c      1
    47 0xbd1c32d2c3b3f600736d244c56806ddb5a190b8a      1
    48 0xc3956f9e14e62bfe00672ac5a4b5d11f84d8b5b2      1
    49 0xcbc7e8a39a0ec84d6b0e8e0dd98655f348ecd44f      1
    50 0xcc97dc4b6488ca9731c98e1bd5656599b08bac91      1
    51 0xd38e1fb60cd50cf9ae747ecf646b4ff1a310ba55      1
    52 0xd66a2ac715fd7628bbcbf49ac3dd4a0f3a5b847f      1
    53 0xd6701a92ee4e8df99a7ff1a453abba8da84e0c98      1
    54 0xd6d30a3572b40d4137a968d4e0ca41b682aa477f      1
    55 0xda75f63b9cb1cf7668bb0ee34fa718bc1cb5bbc1      1
    56 0xdb22ca143f6396ad289c79cdfa5cc47f65884162      1
    57 0xddc9520acb5d1923412674a4ce07bb2e32ff0ac7      1
    58 0xdf51e97fbcf724f00218494864338e7aedb3dfb6      1
    59 0xe1ff19610020d72930aee1b9c047e35b7fd0080e      1
    60 0xe46105bbda606babb797b343b21f930668e83c1d      1
    61 0xe496a4f9472a4935ebf4ff78a18ae21e2728ffaf      1
    62 0xe678646d480c34f7fe4781276365d04e83c9b597      1
    63 0xe7bfc67952b0a48f4ce3db309ab1adda322763dc      1
    64 0xe8170bc2483571d06aadf73ed5f90e0719c6afa4      1
    65 0xe8c4156a19951deff3203f116fc63274da746baa      1
    66 0xea88730cafec7a18685712b1ce200301c7c0b754      1
    67 0xed3c3bffcf147850406d0e58ce4f4ebd2b5cd96c      1
    68 0xf262c25f75e93ddf7c1fcb4bb0d65a707ad6d023      1
    69 0xf33654f85ba6b567f8841c8c954635b27e14c49d      1
    70 0xf3a662e6a44999322a14525c2877dc48fa8b2084      1
    71 0xf3ab1c5047cbc4cb0a61c9cada9b2929e18d11d1      1
    72 0xf5653e48e24e04cc1b14131ab31b776ae8718b34      1
    73 0xf68e4d63c8ea83083d1cb9858210cf2b03d8266b      1
    74 0xfc4672341c78f1ebc0c23fdbcbb03b61c6f2d50f      1
    75 0xfe949ccf2315faec4b47b3e2bb516c01c06c0798      1

## Allow Random5 Memes Phase 1

``` r
c(allow_memesRandom5_phase1) %>%
tally() %T>%
readr::write_csv(file="allow_random5memes_phase1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 75 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x02eff1f92249f482a4cb6e8f25f6b01cb7381f5c      1
     2 0x087e14b18e87367a7cc58f04989d749b8a0e71a1      1
     3 0x0e0b904e2940a31576326d2f335564b3c85a8afe      1
     4 0x11fa4f109a1601bd1afdb509b3bdb1e23fd6a675      1
     5 0x13075a80df4a80e45e58ef871900f0e0ef2ca5cc      1
     6 0x136a4201199b1e75ecdf9abec5e41eb84183406b      1
     7 0x140de2c2585f66e922b43ebca1387dad90de1adb      1
     8 0x1684c382352fa8992e2d12264b08cbfd8e06b7b1      1
     9 0x16bd1bfc9117aff9269cf0f824c9622330b98a81      1
    10 0x1b1405f6dc51121a63c71e9d7282c221d84dccfe      1
    11 0x1e6c02d4f25dff92b8355abbac1b9522e0a7e8e9      1
    12 0x21b5a27ca8d5681a4dc9e9c29615716bdff7cb7d      1
    13 0x282173335ae1f328b34eb9acadc052e59e70a89f      1
    14 0x2b26da567c2a8c220daf91be8e37a429d33aef0b      1
    15 0x353bc35becfed0c65bf6d4466bdf6b381cc673a9      1
    16 0x373d419979862c6845e74cee5ae1e075801b6abc      1
    17 0x50f27cdb650879a41fb07038bf2b818845c20e17      1
    18 0x513a8a2577056d82b9e9d46c96625a4d4d05687e      1
    19 0x573369473b02d0b4fc9f3556184b67705512d26d      1
    20 0x58ac0439d856e25729d24beaef4f50effa00d834      1
    21 0x593620ba826321a0d063ffa5d1b991a021088300      1
    22 0x59c1591ecd3689e911a49ebd406f9fb157171cef      1
    23 0x5b58aeebafe359fe5b19ae3b41048ab859f4bc87      1
    24 0x5b956d9ae01870d463cf0ee364db00eb995c684c      1
    25 0x5fbf4dc4291b542496cc61f52448c0ea374767c0      1
    26 0x60a363530ec7db4fe74f5ebe62c337fdca8efe0f      1
    27 0x65806cfaa5fd763d379c92992d4eda2260c39d84      1
    28 0x6c1aa1ebfc2d028b609c6c6b3153e8c94333a3aa      1
    29 0x723d5453fc08769cb454b5b95db106bf396c73b3      1
    30 0x729fa41c7413def56234fa29a72991b49b9a542e      1
    31 0x739c6cd894603ddb5e974f597d49bf5ae72727fb      1
    32 0x7859f3da4b5fa22b8e0176925ef997c97f18c400      1
    33 0x794f0e25c5910cb3f8a82c9fb9cb44bc069674f0      1
    34 0x79fdbef5ce6d2302db090cb75de318d2c3edccee      1
    35 0x7e4c920953db2e9ee431f7ebff0bffaeb1d870c8      1
    36 0x81c2eee2902eda6db28a448d8a814f221718ba2d      1
    37 0x838a1c2f0cae94072332c346533acdc6c590971f      1
    38 0x8881fe665adaf782b9333308fb2c31d6402bf32a      1
    39 0x8a7c07154908a1701f61ce2ee1b81fe4eee4216a      1
    40 0x90c11d589dfd2e39b2813fc04a1be80ffd9b7dca      1
    41 0x996666de859b274cdc925eb890ebb39aadbf3317      1
    42 0x9e174789337b6650fdbb77883f77fd99c2af2f10      1
    43 0x9eab4b2feed9483838d2e9b7c1ad407a18624df4      1
    44 0xa2d1ebf3b7d49cbc5b171f4466b7a90f75bd597d      1
    45 0xa454daff2609603b169e6290fe69667705ed11fb      1
    46 0xa76b79f978cd069d77d68cd3f82845fd7da80bca      1
    47 0xa954b7233974944a681957de8385b46b855ffd41      1
    48 0xaf31916637a06c22157c99734cc94e03ab97a101      1
    49 0xb3bf133950d87f84508fc64735ed40a4f2797aa6      1
    50 0xb4d502361a3c6f823eeb9a99af09e110382206ee      1
    51 0xb7fafa52585882ff727db81c1275103e18a81b6a      1
    52 0xb92c8c16ede4649b6ea6015beaf2f34973dbbd44      1
    53 0xbb315ba16c951cdccbab7be11b935387d82d98cc      1
    54 0xbcc94f4c922736925e0e9c15391657888e85f435      1
    55 0xbe58a1b85d11e2083bb7c5766284ed7848448d1d      1
    56 0xc25d23d642b05b9546d85574b53f8b62b6b11a7b      1
    57 0xc355bb2414889837c85c314e6ff5a4b6fdc7554a      1
    58 0xc55d765bbd35e495ea519f0a170dedd30363e114      1
    59 0xc89590b304b88d0e7cbadf6c6e7b1cbee1845847      1
    60 0xcf7e084d9068634b94d18cbe979743a591f84e3c      1
    61 0xcfb20378008dffe381c935c16efcc120a41a48ec      1
    62 0xd8c247ee44dfc6898fe4e224979f64e367d2c9ea      1
    63 0xd9db485876cbb4c909ff87bd3b63ef133b3b1cc2      1
    64 0xda4ad44954c137c98eb210612b0bc0f172df17c5      1
    65 0xdb368b61ac7c285c81bb557fb0c46ea7a764fac3      1
    66 0xdcbf46cbb479ad18861b488acb13af096ab88368      1
    67 0xde64640c32cc89ee35e1657e02c4b0100e838fcf      1
    68 0xdeab3f7094a77199ae670316976654c17fea35d6      1
    69 0xe22a7a0afbac019c46e0a599e8984e23cac57620      1
    70 0xe529c7121fb6aa33273c96bcf38d4ddc66c7ce66      1
    71 0xe6015cde54b2e9b8ca82c27a29a16d4113879c87      1
    72 0xe91d95ba259c6d8001a06deaf02f811baa3e2dfa      1
    73 0xedfaed62db466ceb580a7804affcc65184a4bc46      1
    74 0xef6f856947f7defcbdd1720f6ec578fc115f8ee8      1
    75 0xfa086fd3456f8eea481fa602359e0454fee9d6b3      1

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
     5 0x1566ae673ae80725bcce901b486c336e6acef465      1
     6 0x16b92abc4e0e0d074e927c4bb46a5c3ee54ffff0      1
     7 0x17eebf2b0c31f6af94abdb551d1dd509ba6e4f0a      1
     8 0x1a9aee029a1b8c69c4404890c85715cc726746c7      1
     9 0x1aa1c4fdb3b3de2b005a363281d0419e10d1b173      1
    10 0x22fbaa2dd20b848084545515fb04b9c846942779      1
    11 0x248458947c120ca057ec028b3fe7e4b3f26fdb3d      1
    12 0x27b1abafb2cb065cfaf41b4b7ee95d27192151b2      1
    13 0x2924196a2ec71ea4ae1b1357381eccdcee6c18f2      1
    14 0x2947cd7346b3a372b357256b8955ebd4b5735987      1
    15 0x2f2242b447a54cb110b8a7991cf8a27054ee6921      1
    16 0x3511ae23ee25e2b97dce883c7d496ff5d18f1dfa      1
    17 0x3cb63b82d778105e43f064ed739b0655f1f0fb87      1
    18 0x3d0a1d20ac580ca8fa35888c92dd0859887f3398      1
    19 0x40d2f4399d23f9afb82d0a6b73055f13208614f9      1
    20 0x43b0bd27d5016e969193412933387b0dd4cf3e0a      1
    21 0x477ea7a022e51b6eb0dcb6d802fb5f0cfc3b4a81      1
    22 0x48464efe55fbcae8ae0c992b306afcf21d4910cf      1
    23 0x520e8d6c5b2dbbe62bf8a40653a314d879b19b86      1
    24 0x53006f95def268f88dc1b8216654ab56f3afd052      1
    25 0x575f6540c16a72696c14a17fa64f049992d661ab      1
    26 0x59068075a799594db03c0255eed68e8e121155c8      1
    27 0x5fdb5fdb61bc2975f3be446c5ae9c5df490f55d2      1
    28 0x60d2bc84a71af61b82d1b4168c9103ff6c4898eb      1
    29 0x615502edd5cf2d59244dd0dd53d3c8ec035e172b      1
    30 0x61d9d9cc8c3203dab7100ea79ced77587201c990      1
    31 0x64f7de90dc79d775703bbec66a1591c7a26a22f0      1
    32 0x665654f2d2a151be2d0f8e3697e6ce780f732af2      1
    33 0x670a840848eba22eb2a090fd65e1ff60c3ba9e5b      1
    34 0x699990a8e7ada9e92c932d6e8fb365024fc74b43      1
    35 0x69e68074f1aada957edd39c5eae0069973343f30      1
    36 0x6f0735bf1e6c69030d6990cdd580345b370eb50a      1
    37 0x6f113b0c8c7266d7514e4986e3d6aaf013b6754f      1
    38 0x6f435a9421653b27f048957b8ba2092d65a711f9      1
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
    1 0x2c8875f34ceb219f61b7453b2c5f100ec2f6ed33      1
    2 0x89ef05c30c821c49b67d06e630686c5c3232baab      1
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
