
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:285         Length:285         Min.   : 1.000   Length:285        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.354                     
                                           3rd Qu.: 1.000                     
                                           Max.   :12.000                     
         name          
     Length:285        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19346969 # https://etherscan.io/block/19346969
block_hash <- "0x656499afba12679067ff504e90cdcb47ff910bd5aa26cc29be0fc6bc57d5749c"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4812 

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
  "0x000000000000000000000000000000000000dead",
  "0xA19193B6Bd97798695097e71EAb6d310F99f1955",
  "0x0000000000000000000000000000000000000000"
)

hodlers_remove <- c(
  ""
)

allow_artist1    <- pick(snapshot, contracts=c("VHS","Amixofstories","REEXPERIMENT","PSX00","VTPunks","FAWKEK","Foundation"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("PEPEINDANGEREditions","KnownOriginEditions","NIGHTSKYEditions","WEBCULTUR3Editions","ALPHACULTUREEditions","CFCARTRIDGEEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("VHS","Amixofstories","REEXPERIMENT","PSX00","VTPunks","FAWKEK","Foundation","PEPEINDANGEREditions","KnownOriginEditions","NIGHTSKYEditions","WEBCULTUR3Editions","ALPHACULTUREEditions","CFCARTRIDGEEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 22 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x06163155dfe5bf26ae88556ea1658677000a2c79      1
     2 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
     3 0x0ce390f18af702cca546297845a4a51d102123cf      1
     4 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     5 0x1f35f4a620add904154ec9f179597a2334b3f86b      1
     6 0x2cbe14b7f60fbe6a323cba7db56f2d916c137f3c      1
     7 0x3dcff084bd2d9c14e45fa28b61775420dc712080      1
     8 0x8261b63b07d1ad92aeb7cdd916bb9a25236ceb65      1
     9 0x8412f98582158cc5bda82e9ade35543f72c71fb5      1
    10 0x8888888888e9997e64793849389a8faf5e8e547c      1
    11 0x97d6de0f644eb9bcf25a21ddeb87c33e0f117ead      1
    12 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
    13 0x9a2cc6033ef739c4fcaccd64f44f0381d9f9c0d3      1
    14 0x9e56625509c2f60af937f23b7b532600390e8c8b      1
    15 0xb82db479b41c4b8211c2b3abd80ceedb9c6e5a33      1
    16 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
    17 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
    18 0xc2d11c0ac4ad5aa6667536d7a605f3948d97ee2a      1
    19 0xc4cf16c4a92c8a147dd21d54d5ea05c511007b10      1
    20 0xcfc2729173d1ef8d25050c76b84ec5af1fe7bd44      1
    21 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    22 0xf6d45c42445391b350be3cad097d85a0fe438174      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 92 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x0052d2e4af4548e6cf19e817b944cedb23ae09df      1
     2 0x08ff3b1838bc9a21c87c0c324d8e3fbe52dbe428      1
     3 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
     4 0x0b71fcb7035b12f062b306ba78971bb66e0d0697      1
     5 0x0ce1e7cafe72b4d48de78a2593d9f251345ae740      1
     6 0x0e8caf9eca5e45df0e6f50f58a5bf664db1740c1      1
     7 0x14ff95dbb15ef06c135d048fb417eec6ef080914      1
     8 0x18561020ee51f3cff0d788d16a821b96c1d3b600      1
     9 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
    10 0x1d9d562e3bdf3d0d59723b93fe65335fa41996e0      1
    11 0x20e7d25c324e143ff897e75d4d68cb28a1f6d9a5      1
    12 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
    13 0x236ddecf6ab1269b74f3a497e0dfa7bc0cf16903      1
    14 0x24b2a186e776b0352ebf6f789574eeed024448c8      1
    15 0x3201fc1293f5de5b3176d168ca5ed8644431638b      1
    16 0x35dfa1dbb4b8e82e8a924c53e3649112e45a05f4      1
    17 0x3ef7b6b9179efba366563b07836829d52290010d      1
    18 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
    19 0x43176751abfbd7a39549748aab57641c26df7129      1
    20 0x44cd071ad1509ca4a0e03202ff8dcd265956e434      1
    21 0x4d8f12386561667cfcc600ce2cc161a0d337da8b      1
    22 0x5000f2b7f12f28b55d27bc8ca8346adc8dc2fc2c      1
    23 0x50c889cf22b145fa7d5d7af22998be677cc5fe47      1
    24 0x50cacfa17a23543548ba17ac708484de450808f5      1
    25 0x51b916f41668a4a5df20d30222457ce1f489abce      1
    26 0x578b076f33c021ca8ec8873be00c734559a99057      1
    27 0x597c8150c0d481dc7e7b27cde295358e2fbd0f56      1
    28 0x5e639c1e6a732584517872983ddc6e9caa6df9b1      1
    29 0x61f6f6680d2dcc792822748167e8478fc7b21b5f      1
    30 0x65e1b81a7c0a7cda7ccb3255d931a1d9bd3ecfa2      1
    31 0x695a45b7b6f74165faec3684c25f77f3e4e0928c      1
    32 0x6a0fd52d5e3a1fa9ff8deff3158245477ed1c2ec      1
    33 0x6ae4d80a9762d6f380d6136a7179afe41378f251      1
    34 0x6e1c743c89d614a62f15b0b1b83119fceffe490c      1
    35 0x775a26d0632b8c8d471f4d5a2ed69e07eda1c1dc      1
    36 0x793a68e64214a1c4a0406a214874b60781320596      1
    37 0x7b03127473868f01bb59247e2ea6d811346c61f3      1
    38 0x7b3397db071de728855b45c33a46ab52eb22915c      1
    39 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
    40 0x89795eeb2117b5a65784ebc979c0767934a7dd93      1
    41 0x8abc6546a2895c27d2165776931c1276c258e903      1
    42 0x8f3cc342fac4d0d6f58366e0a1792c65b70c91af      1
    43 0x8f7ab1f43b1bff61cdfa470f397080da0bbee5d5      1
    44 0x9a8b253c304b897af8147143c5d0db4635e8c9c6      1
    45 0x9dbe76805de4a78d39aabf8179fbb4c0405d7b1e      1
    46 0x9fe366d6efe4ae241656915915f2f050c5ae33be      1
    47 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
    48 0xa582360da4ae1fbb912c975fcce1a0c0e2af91e8      1
    49 0xa971d9c4c302553512b89c5777873bf16c9f1d0d      1
    50 0xa9b0226c0d8428102397f19a4e9eae2bece455e1      1
    51 0xab4ce2a08cbea809692218dd7841f681b80069a9      1
    52 0xaea015a0e5edb747e4f11af56555f70f42e1a418      1
    53 0xafdb99def38a49d24a5189d341fc96f4729f27d6      1
    54 0xb26189d36c04c6b49126987c0624c691038c7ace      1
    55 0xb2d9763a3a152de426deca2ab8ed85b638b7bcc2      1
    56 0xb573d55bb681b091ca01ef0e78d519ed26238c38      1
    57 0xba9544045a1ce93e0afaff1e547aac7d05627ee8      1
    58 0xbabd6ad755471fbc26cd15b63e7cd20860feeb81      1
    59 0xbb9f4a4572281670de1a8513b8195c161aadcf62      1
    60 0xbea73d090461d4d7d06c30c12612b5b5219c69e1      1
    61 0xbf03dba3df5c671d807bf1fd888ea5abce122f97      1
    62 0xc006f81f7468985121775afa5e950b92a1d84486      1
    63 0xc4210bfd73206c3208ae1f0854ee146ed8f1271b      1
    64 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
    65 0xc5ad848fa95a259e774750cfb025950892ca5c1b      1
    66 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
    67 0xc9f64274f5bc7f97f02129406eb6191d31548663      1
    68 0xca0ffa49c58a571f54b7bff66e8102b35d69fcf5      1
    69 0xcc9f512c8a0d4727d95e55297d5a7cecbc65e837      1
    70 0xcd89a9e13ebdc0443ba0a5085d4c4536c70c6b4d      1
    71 0xd13c2691e0715efc6070f48242bf4317c84884f1      1
    72 0xd2f1bd28307e0184a112605c4063b2c6da3f0203      1
    73 0xd496b0eb47a5fec00a30d11506edad2d118ec167      1
    74 0xd6d201fc928e31159800e991d13d53aa8a6d3497      1
    75 0xd7ce4706f31606981dc35255c8ce27934f9a7624      1
    76 0xd8625f9178a259b14f37edd79d11100dd5bb1c2f      1
    77 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
    78 0xdc2185280f518a561e4c04c478baa6cf29965495      1
    79 0xe0a749772f7512983759a8a7dee2f5a39d9ad14c      1
    80 0xe2768806620471d87771da5195a6494800dc700e      1
    81 0xe4e2a5f91626c4afe595a50f02207448cb36a768      1
    82 0xe576c6cba76f48b1f9ac68ab1c0627225e1dc5ff      1
    83 0xe720298945353c065287eeef34eff82e5d7bcdac      1
    84 0xeaeb42b770e662e653f0de9f4c6f1677102517b7      1
    85 0xec1ef8b368549575cd834a514ef1e387b52d0fc9      1
    86 0xef16d178f36f6cd0c83d3a7dc3719c41c409150a      1
    87 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    88 0xf1e9259db3999a1b28fca4c81f2b1cac5e5e3836      1
    89 0xf739b48e89ed4b7c2398cc4e2dcd54479b7bea50      1
    90 0xf7770beba307ff3dd921d1ce8faf6dbc061a71aa      1
    91 0xf9fb89a00c4e38d6839ae93dee090d35c388edea      1
    92 0xff86870a3c7e39d0e9f3710cef08862dc9928878      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 114 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0052d2e4af4548e6cf19e817b944cedb23ae09df      1
      2 0x06163155dfe5bf26ae88556ea1658677000a2c79      1
      3 0x07587c046d4d4bd97c2d64edbfab1c1fe28a10e5      1
      4 0x08ff3b1838bc9a21c87c0c324d8e3fbe52dbe428      1
      5 0x0a1c6f1638adbe3873c7e8dad6c0e3b08285ba07      1
      6 0x0b71fcb7035b12f062b306ba78971bb66e0d0697      1
      7 0x0ce1e7cafe72b4d48de78a2593d9f251345ae740      1
      8 0x0ce390f18af702cca546297845a4a51d102123cf      1
      9 0x0e8caf9eca5e45df0e6f50f58a5bf664db1740c1      1
     10 0x10844a040d1c3eebb76286e9916f421aa3f27514      1
     11 0x14ff95dbb15ef06c135d048fb417eec6ef080914      1
     12 0x18561020ee51f3cff0d788d16a821b96c1d3b600      1
     13 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     14 0x1d9d562e3bdf3d0d59723b93fe65335fa41996e0      1
     15 0x1f35f4a620add904154ec9f179597a2334b3f86b      1
     16 0x20e7d25c324e143ff897e75d4d68cb28a1f6d9a5      1
     17 0x235f32bb3920bf99e9fa813a37fcebadaf9cdf39      1
     18 0x236ddecf6ab1269b74f3a497e0dfa7bc0cf16903      1
     19 0x24b2a186e776b0352ebf6f789574eeed024448c8      1
     20 0x2cbe14b7f60fbe6a323cba7db56f2d916c137f3c      1
     21 0x3201fc1293f5de5b3176d168ca5ed8644431638b      1
     22 0x35dfa1dbb4b8e82e8a924c53e3649112e45a05f4      1
     23 0x3dcff084bd2d9c14e45fa28b61775420dc712080      1
     24 0x3ef7b6b9179efba366563b07836829d52290010d      1
     25 0x41b7d5bd3bb528f1358aaca73cbdf2cd7b34d966      1
     26 0x43176751abfbd7a39549748aab57641c26df7129      1
     27 0x44cd071ad1509ca4a0e03202ff8dcd265956e434      1
     28 0x4d8f12386561667cfcc600ce2cc161a0d337da8b      1
     29 0x5000f2b7f12f28b55d27bc8ca8346adc8dc2fc2c      1
     30 0x50c889cf22b145fa7d5d7af22998be677cc5fe47      1
     31 0x50cacfa17a23543548ba17ac708484de450808f5      1
     32 0x51b916f41668a4a5df20d30222457ce1f489abce      1
     33 0x578b076f33c021ca8ec8873be00c734559a99057      1
     34 0x597c8150c0d481dc7e7b27cde295358e2fbd0f56      1
     35 0x5e639c1e6a732584517872983ddc6e9caa6df9b1      1
     36 0x61f6f6680d2dcc792822748167e8478fc7b21b5f      1
     37 0x65e1b81a7c0a7cda7ccb3255d931a1d9bd3ecfa2      1
     38 0x695a45b7b6f74165faec3684c25f77f3e4e0928c      1
     39 0x6a0fd52d5e3a1fa9ff8deff3158245477ed1c2ec      1
     40 0x6ae4d80a9762d6f380d6136a7179afe41378f251      1
     41 0x6e1c743c89d614a62f15b0b1b83119fceffe490c      1
     42 0x775a26d0632b8c8d471f4d5a2ed69e07eda1c1dc      1
     43 0x793a68e64214a1c4a0406a214874b60781320596      1
     44 0x7b03127473868f01bb59247e2ea6d811346c61f3      1
     45 0x7b3397db071de728855b45c33a46ab52eb22915c      1
     46 0x8261b63b07d1ad92aeb7cdd916bb9a25236ceb65      1
     47 0x8264e9e0f4cbcbbbb3f8ecaec0a625b590ae790e      1
     48 0x8412f98582158cc5bda82e9ade35543f72c71fb5      1
     49 0x8888888888e9997e64793849389a8faf5e8e547c      1
     50 0x89795eeb2117b5a65784ebc979c0767934a7dd93      1
     51 0x8abc6546a2895c27d2165776931c1276c258e903      1
     52 0x8f3cc342fac4d0d6f58366e0a1792c65b70c91af      1
     53 0x8f7ab1f43b1bff61cdfa470f397080da0bbee5d5      1
     54 0x97d6de0f644eb9bcf25a21ddeb87c33e0f117ead      1
     55 0x99e73a69536f7365fd4f2f2e5391f5daa484b99b      1
     56 0x9a2cc6033ef739c4fcaccd64f44f0381d9f9c0d3      1
     57 0x9a8b253c304b897af8147143c5d0db4635e8c9c6      1
     58 0x9dbe76805de4a78d39aabf8179fbb4c0405d7b1e      1
     59 0x9e56625509c2f60af937f23b7b532600390e8c8b      1
     60 0x9fe366d6efe4ae241656915915f2f050c5ae33be      1
     61 0xa43711f5dcc3559a1e07ec124fd167e26c9bad43      1
     62 0xa582360da4ae1fbb912c975fcce1a0c0e2af91e8      1
     63 0xa971d9c4c302553512b89c5777873bf16c9f1d0d      1
     64 0xa9b0226c0d8428102397f19a4e9eae2bece455e1      1
     65 0xab4ce2a08cbea809692218dd7841f681b80069a9      1
     66 0xaea015a0e5edb747e4f11af56555f70f42e1a418      1
     67 0xafdb99def38a49d24a5189d341fc96f4729f27d6      1
     68 0xb26189d36c04c6b49126987c0624c691038c7ace      1
     69 0xb2d9763a3a152de426deca2ab8ed85b638b7bcc2      1
     70 0xb573d55bb681b091ca01ef0e78d519ed26238c38      1
     71 0xb82db479b41c4b8211c2b3abd80ceedb9c6e5a33      1
     72 0xba9544045a1ce93e0afaff1e547aac7d05627ee8      1
     73 0xbabd6ad755471fbc26cd15b63e7cd20860feeb81      1
     74 0xbb9f4a4572281670de1a8513b8195c161aadcf62      1
     75 0xbdb0dd845e95d2e24b77d9bef54d4df82baf8335      1
     76 0xbe8fe12b9eb1ca2a593e6c070c71c294b6fe9f00      1
     77 0xbea73d090461d4d7d06c30c12612b5b5219c69e1      1
     78 0xbf03dba3df5c671d807bf1fd888ea5abce122f97      1
     79 0xc006f81f7468985121775afa5e950b92a1d84486      1
     80 0xc2d11c0ac4ad5aa6667536d7a605f3948d97ee2a      1
     81 0xc4210bfd73206c3208ae1f0854ee146ed8f1271b      1
     82 0xc47c0dccfbb4d1c20d53569a288738f22e32275b      1
     83 0xc4cf16c4a92c8a147dd21d54d5ea05c511007b10      1
     84 0xc5ad848fa95a259e774750cfb025950892ca5c1b      1
     85 0xc68c7771ec6a6e5d67d62aa9c6f22df69865e401      1
     86 0xc9f64274f5bc7f97f02129406eb6191d31548663      1
     87 0xca0ffa49c58a571f54b7bff66e8102b35d69fcf5      1
     88 0xcc9f512c8a0d4727d95e55297d5a7cecbc65e837      1
     89 0xcd89a9e13ebdc0443ba0a5085d4c4536c70c6b4d      1
     90 0xcfc2729173d1ef8d25050c76b84ec5af1fe7bd44      1
     91 0xd13c2691e0715efc6070f48242bf4317c84884f1      1
     92 0xd2f1bd28307e0184a112605c4063b2c6da3f0203      1
     93 0xd496b0eb47a5fec00a30d11506edad2d118ec167      1
     94 0xd6d201fc928e31159800e991d13d53aa8a6d3497      1
     95 0xd7ce4706f31606981dc35255c8ce27934f9a7624      1
     96 0xd8625f9178a259b14f37edd79d11100dd5bb1c2f      1
     97 0xda3863583c9fcd8a5d3506d91aaf33157124d1d1      1
     98 0xdc2185280f518a561e4c04c478baa6cf29965495      1
     99 0xe0a749772f7512983759a8a7dee2f5a39d9ad14c      1
    100 0xe2768806620471d87771da5195a6494800dc700e      1
    101 0xe4e2a5f91626c4afe595a50f02207448cb36a768      1
    102 0xe576c6cba76f48b1f9ac68ab1c0627225e1dc5ff      1
    103 0xe720298945353c065287eeef34eff82e5d7bcdac      1
    104 0xeaeb42b770e662e653f0de9f4c6f1677102517b7      1
    105 0xeb43b5597e3bde0b0c03ee6731ba7c0247e1581e      1
    106 0xec1ef8b368549575cd834a514ef1e387b52d0fc9      1
    107 0xef16d178f36f6cd0c83d3a7dc3719c41c409150a      1
    108 0xf19ab0e65c902ffedcb95a76a93ca7bf789239ed      1
    109 0xf1e9259db3999a1b28fca4c81f2b1cac5e5e3836      1
    110 0xf6d45c42445391b350be3cad097d85a0fe438174      1
    111 0xf739b48e89ed4b7c2398cc4e2dcd54479b7bea50      1
    112 0xf7770beba307ff3dd921d1ce8faf6dbc061a71aa      1
    113 0xf9fb89a00c4e38d6839ae93dee090d35c388edea      1
    114 0xff86870a3c7e39d0e9f3710cef08862dc9928878      1

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
