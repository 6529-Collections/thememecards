
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:166         Length:166         Min.   :1.000   Length:166        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.054                     
                                           3rd Qu.:1.000                     
                                           Max.   :8.000                     
         name          
     Length:166        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 18546969 # https://etherscan.io/block/18546969
block_hash <- "0xc52e73558909a731bc9f3250790f68fd79ecaa39671018c37abde2737296fe91"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4492 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","SleepParalysis","Foundation","MonochromeDreams","HotelRooms","Iam","SashaKatz","AppleGarden","KnownOrigin"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("SashaKatzOEEdition","RustlesEditions","MonochromeDreamNo9Editions","KnownOriginEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","SleepParalysis","Foundation","MonochromeDreams","HotelRooms","Iam","SashaKatz","AppleGarden","KnownOrigin","SashaKatzOEEdition","RustlesEditions","MonochromeDreamNo9Editions","KnownOriginEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 28 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
     2 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     3 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     4 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     5 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     6 0x46a3299b465dfb25dbd0a30052c8576b7d85a9dd      1
     7 0x56c5b4b4c72251abbf25a301191bcea49367ac4e      1
     8 0x576a655161b5502dcf40602be1f3519a89b71658      1
     9 0x5fbe0ae423f1767028f150aa92545267507588ef      1
    10 0x62247532f72dce05d7bca9f1a6d778c91a00054c      1
    11 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
    12 0x6974606889a3b1ff76a2347441324b13a4bbb7e6      1
    13 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
    14 0x7ce438bf068c8f47f0f46cb7891fc7fd0956f117      1
    15 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
    16 0x90e7474ad0bf0507313ccb9efe7e2921905da398      1
    17 0x9f7064dfe6ca5856b3afd7a3b3974686a35bdab5      1
    18 0xa4ad045d62a493f0ed883b413866448afb13087c      1
    19 0xa5a8fb5d3132510381a3206d232f375205f809e6      1
    20 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    21 0xab6ca2017548a170699890214bfd66583a0c1754      1
    22 0xaf35be196994a1ea334f01acb85b5278e0e88ca5      1
    23 0xb048197de0e342a2c8c40f1e89b03448e8168677      1
    24 0xc3b8fdbb0e0d7c4027084b0996f0129ab1a29dc2      1
    25 0xca536f1a439eadb69643b850a198939bae77ee4e      1
    26 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
    27 0xdbc522931928f8ff7784c84e5fcec4ffdcd6e9eb      1
    28 0xef768ef781637a7995a2b57ff77825e8abcf682d      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 88 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00a34fa254e644ef15031a56e47a345cb7320e91      1
     2 0x00e2c64ca4df1b5da6837e4fc6529f3eca88eee7      1
     3 0x07b564ade1b8212eb5e46e2d82ea1addf6f7273d      1
     4 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
     5 0x11731586c573bc263de405603793ef48f974830f      1
     6 0x133fab28caec665ade2a67d1b360ea1eb18b812c      1
     7 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
     8 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     9 0x220cbc6f22efd7fc0a6d9222c09287699251315e      1
    10 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
    11 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
    12 0x2c09bd77b17ad609f97802cc8b741a4ed22581e8      1
    13 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
    14 0x31b9aa7881a0225a3d189a4f52a8a53f09853d7c      1
    15 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
    16 0x3945a8c8b6d39e47a14ebd8dbc699382af284107      1
    17 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
    18 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
    19 0x436b9a5e9a71bd90867714da1617deb70f012126      1
    20 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
    21 0x4f41c1e394062ce9979de26d5ac0a5e72484bce5      1
    22 0x5090752b49b42eff5a3744c2914222fb02b5a925      1
    23 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
    24 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
    25 0x5c0ec6cd4a23fb4d2946fd51c6ce57f36d0902b1      1
    26 0x5c90e93e5cdb4fa7d60087ed694e8afdf59719c3      1
    27 0x5cfddc9d77622559b6baf0935a6ef0b521e24602      1
    28 0x6006a70daaa8d9971485ebae7e3a998f6566c0f6      1
    29 0x657c1f8421ae93e88fb3171f019779e87b0263ca      1
    30 0x668248df4595e09aa253b31478312748078f7a20      1
    31 0x6b8ad90480d73dd77cd719ca637c0f0289b6665e      1
    32 0x6dce0c9dc00fb34b5e1e932e1d640884f5f1782a      1
    33 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
    34 0x6ec01696717d9915564df652b8049a3cf872035c      1
    35 0x757a21fbd39cccaff681e1930e273b9add0008db      1
    36 0x75aadbb7c36db057e5ac64183899f118de76df35      1
    37 0x762da606029d3120735aa1eec15464e265db7a3c      1
    38 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
    39 0x7b5658e5212377020b953d232875fc59ee6edbef      1
    40 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
    41 0x7d08c340fcfe051c462dcfda8b9ed47642b96b10      1
    42 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
    43 0x82a9bfd5901ae6946355e228c9ad619b8090121b      1
    44 0x843dd16b4da6044d69199518d0e7d01bbd31b5d6      1
    45 0x85bcb8a0809c5dd5b03939b62f692d5ca5e3c0fe      1
    46 0x898952b4d071ead9d3bdcc5e430b17efc4996737      1
    47 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
    48 0x8ba5bf16a4af0802d21de2b39f732ffa24e45f81      1
    49 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
    50 0x8dd331de57c2b1305cd3c2792bbc5383136f3b52      1
    51 0x968f609474c2095011baa4ef6602a080f3fe0aeb      1
    52 0x9b08042e20dc4e883c41e89813be918d6729099a      1
    53 0x9c4e9a0b3fddefbc8991b9bab1b05cdde9c67a71      1
    54 0x9e00b0b219d5c2612a61fc66b9d864b375f6d90d      1
    55 0xa4867d6ae55974270005e543791c51cb0b2a07de      1
    56 0xa698cade532bf7491f0f887284f8e10e2de97198      1
    57 0xa98c907bc3464b6920f5490ebd8464edb4393b84      1
    58 0xab94f597b94f45680b07997c394535d7ebc4a297      1
    59 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
    60 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    61 0xb9d4951ba3d38c6781a2ac422ffbc8b5a4b7a757      1
    62 0xbb3444a06e9928dda9a739cdab3e0c5cf6890099      1
    63 0xbf1f16335d32ca883b5938d19357366002b5feb5      1
    64 0xbf96525c11664742e12f3a7d66869f593b8956bf      1
    65 0xc1aeb6d1786d182ffa088f7dd66af31735f9afe4      1
    66 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
    67 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
    68 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
    69 0xcae1bfdf15b343b84d915a8823796d9578d7746a      1
    70 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
    71 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
    72 0xd726f639291edae53f6765c36d4bc5e716aabc21      1
    73 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    74 0xdc0753f6b3cc6034317b73c10413c19958d36ed1      1
    75 0xe19e13974d4eb5fa9a1d1e5e976d84c9964337cc      1
    76 0xe288a00df4b697606078876788e4d64633cd2e01      1
    77 0xe9110c571f6a4f846ab119329c054fda34912042      1
    78 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    79 0xeb6973c998e62c42a07316eb4fbc7b5008d6cf17      1
    80 0xedd8a26afcc8f2711b3bd20e5c9ac590a1894093      1
    81 0xeede4f6114a9f166a52de679e21144656b3d8b0d      1
    82 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    83 0xf084f1f98f7d0917dbe5eec0098b99e16a37c79e      1
    84 0xf65a82a76874659444b704e75875921636d493c6      1
    85 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    86 0xf91d0854d75012bf33c19f4ca48afa1c3c63ff69      1
    87 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    88 0xfd6866214582e58ae146f0c2603ffe7cd5fda611      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 116 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00a34fa254e644ef15031a56e47a345cb7320e91      1
      2 0x00e2c64ca4df1b5da6837e4fc6529f3eca88eee7      1
      3 0x07b564ade1b8212eb5e46e2d82ea1addf6f7273d      1
      4 0x092cd1a6d222a167f5d0767e6444c8b45c92cc72      1
      5 0x11731586c573bc263de405603793ef48f974830f      1
      6 0x133fab28caec665ade2a67d1b360ea1eb18b812c      1
      7 0x1bb7b4f7d98a9d204186b36d3852e28f3286f577      1
      8 0x1de4d49959eafa01ab3706aa20707a3f1dbe2424      1
      9 0x1e8a2bcb241d3ad860c95fe200c2cc6aed1fa5c8      1
     10 0x1f92fc15bcbeb4dd24eefbead8d7aee409f891dc      1
     11 0x220cbc6f22efd7fc0a6d9222c09287699251315e      1
     12 0x2334d91317950c19a6c21c69d69f5ab8a8f275fa      1
     13 0x264fb9ed5553017beaa709e6410364acd8f0ded0      1
     14 0x2c09bd77b17ad609f97802cc8b741a4ed22581e8      1
     15 0x2ce64f21dcebb47b08375e62d75c098ead3c1cb4      1
     16 0x31b9aa7881a0225a3d189a4f52a8a53f09853d7c      1
     17 0x31c72d0d03adc76456d3c14a4fe19f8aa8307c16      1
     18 0x3612b2e93b49f6c797066ca8c38b7f522b32c7cb      1
     19 0x36a5bc205df1ed65c86301022cfc343a6ce546ff      1
     20 0x3877b4c39476e3af04cb1ff8003db09f39f068f9      1
     21 0x3945a8c8b6d39e47a14ebd8dbc699382af284107      1
     22 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     23 0x3cd378c9b1cb5f147ebf1b2c2564118946ae4ba1      1
     24 0x436b9a5e9a71bd90867714da1617deb70f012126      1
     25 0x46a3299b465dfb25dbd0a30052c8576b7d85a9dd      1
     26 0x4de5ae339b406a8cd5037e9138a62e86aa5352ef      1
     27 0x4f41c1e394062ce9979de26d5ac0a5e72484bce5      1
     28 0x5090752b49b42eff5a3744c2914222fb02b5a925      1
     29 0x51ec8bb228af8aed84d935e0cbed0fdb3fc39114      1
     30 0x523a06ca89600691701f6afa4dc4e8db0d60ed41      1
     31 0x56c5b4b4c72251abbf25a301191bcea49367ac4e      1
     32 0x576a655161b5502dcf40602be1f3519a89b71658      1
     33 0x5c0ec6cd4a23fb4d2946fd51c6ce57f36d0902b1      1
     34 0x5c90e93e5cdb4fa7d60087ed694e8afdf59719c3      1
     35 0x5cfddc9d77622559b6baf0935a6ef0b521e24602      1
     36 0x5fbe0ae423f1767028f150aa92545267507588ef      1
     37 0x6006a70daaa8d9971485ebae7e3a998f6566c0f6      1
     38 0x62247532f72dce05d7bca9f1a6d778c91a00054c      1
     39 0x657c1f8421ae93e88fb3171f019779e87b0263ca      1
     40 0x668248df4595e09aa253b31478312748078f7a20      1
     41 0x694e64d4ad77e0c234b7b1c55ac40302ad86ce3f      1
     42 0x6974606889a3b1ff76a2347441324b13a4bbb7e6      1
     43 0x6b8ad90480d73dd77cd719ca637c0f0289b6665e      1
     44 0x6dce0c9dc00fb34b5e1e932e1d640884f5f1782a      1
     45 0x6ebf583ead402a32f310dc16536067e45e20c9a6      1
     46 0x6ec01696717d9915564df652b8049a3cf872035c      1
     47 0x751f655ff4fbd7dd77f12e0f9ed5d0075cba1b5f      1
     48 0x757a21fbd39cccaff681e1930e273b9add0008db      1
     49 0x75aadbb7c36db057e5ac64183899f118de76df35      1
     50 0x762da606029d3120735aa1eec15464e265db7a3c      1
     51 0x78a9c6ed2174584a109957580164e12c04ec3ec5      1
     52 0x7b5658e5212377020b953d232875fc59ee6edbef      1
     53 0x7ce30498e564f081ca65a226f44b1751f93a0f82      1
     54 0x7ce438bf068c8f47f0f46cb7891fc7fd0956f117      1
     55 0x7d08c340fcfe051c462dcfda8b9ed47642b96b10      1
     56 0x7fc53dfff734d7cb9f533154344e4d569a34f8e8      1
     57 0x82a9bfd5901ae6946355e228c9ad619b8090121b      1
     58 0x843dd16b4da6044d69199518d0e7d01bbd31b5d6      1
     59 0x84590d8b7c4f89f0dbd186e12b1d2eaa92446e41      1
     60 0x85bcb8a0809c5dd5b03939b62f692d5ca5e3c0fe      1
     61 0x898952b4d071ead9d3bdcc5e430b17efc4996737      1
     62 0x8a90f7679506b79a9175bcf6cb755ecba8e905f0      1
     63 0x8ba5bf16a4af0802d21de2b39f732ffa24e45f81      1
     64 0x8d7f598347e1d526e02e51e663ba837393068e6e      1
     65 0x8dd331de57c2b1305cd3c2792bbc5383136f3b52      1
     66 0x90e7474ad0bf0507313ccb9efe7e2921905da398      1
     67 0x968f609474c2095011baa4ef6602a080f3fe0aeb      1
     68 0x9b08042e20dc4e883c41e89813be918d6729099a      1
     69 0x9c4e9a0b3fddefbc8991b9bab1b05cdde9c67a71      1
     70 0x9e00b0b219d5c2612a61fc66b9d864b375f6d90d      1
     71 0x9f7064dfe6ca5856b3afd7a3b3974686a35bdab5      1
     72 0xa4867d6ae55974270005e543791c51cb0b2a07de      1
     73 0xa4ad045d62a493f0ed883b413866448afb13087c      1
     74 0xa5a8fb5d3132510381a3206d232f375205f809e6      1
     75 0xa698cade532bf7491f0f887284f8e10e2de97198      1
     76 0xa98c907bc3464b6920f5490ebd8464edb4393b84      1
     77 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     78 0xab6ca2017548a170699890214bfd66583a0c1754      1
     79 0xab94f597b94f45680b07997c394535d7ebc4a297      1
     80 0xaf35be196994a1ea334f01acb85b5278e0e88ca5      1
     81 0xb048197de0e342a2c8c40f1e89b03448e8168677      1
     82 0xb154b590d9fbb17c7b6166f16f389f0121186d3a      1
     83 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
     84 0xb9d4951ba3d38c6781a2ac422ffbc8b5a4b7a757      1
     85 0xbb3444a06e9928dda9a739cdab3e0c5cf6890099      1
     86 0xbf1f16335d32ca883b5938d19357366002b5feb5      1
     87 0xbf96525c11664742e12f3a7d66869f593b8956bf      1
     88 0xc1aeb6d1786d182ffa088f7dd66af31735f9afe4      1
     89 0xc3b8fdbb0e0d7c4027084b0996f0129ab1a29dc2      1
     90 0xc79556a222367ecb3c2291d5c6d2b3e58e25fbf1      1
     91 0xc7b85a9ccd23894fe34bb5462c4e902742faf22a      1
     92 0xc841fabb79c0b39bd0af850dcd5281022445eb51      1
     93 0xca536f1a439eadb69643b850a198939bae77ee4e      1
     94 0xcae1bfdf15b343b84d915a8823796d9578d7746a      1
     95 0xcd8b72710595c69019c109aef5b1b92eea7f995f      1
     96 0xd0b643edaa2be4aba13db18cb3e3d608cf567f61      1
     97 0xd726f639291edae53f6765c36d4bc5e716aabc21      1
     98 0xd81ce8e89dd987c8ab630858c8f1e9df14788c35      1
     99 0xd88c17aad286da44d7216f487b9a65ce1f7d4581      1
    100 0xdbc522931928f8ff7784c84e5fcec4ffdcd6e9eb      1
    101 0xdc0753f6b3cc6034317b73c10413c19958d36ed1      1
    102 0xe19e13974d4eb5fa9a1d1e5e976d84c9964337cc      1
    103 0xe288a00df4b697606078876788e4d64633cd2e01      1
    104 0xe9110c571f6a4f846ab119329c054fda34912042      1
    105 0xeafff95282ba3053ff49bfb77fb37ef30754eb62      1
    106 0xeb6973c998e62c42a07316eb4fbc7b5008d6cf17      1
    107 0xedd8a26afcc8f2711b3bd20e5c9ac590a1894093      1
    108 0xeede4f6114a9f166a52de679e21144656b3d8b0d      1
    109 0xef49522a18a0488a0be5568df36fb2c4f828b99a      1
    110 0xef768ef781637a7995a2b57ff77825e8abcf682d      1
    111 0xf084f1f98f7d0917dbe5eec0098b99e16a37c79e      1
    112 0xf65a82a76874659444b704e75875921636d493c6      1
    113 0xf770e42fd25d745e9f25ab57cca3e96f7fe62d14      1
    114 0xf91d0854d75012bf33c19f4ca48afa1c3c63ff69      1
    115 0xfc295aeb2be32ef90104cb3e2807896e85647b73      1
    116 0xfd6866214582e58ae146f0c2603ffe7cd5fda611      1

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