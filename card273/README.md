
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance         contract        
     Length:348         Length:348         Min.   : 1.000   Length:348        
     Class :character   Class :character   1st Qu.: 1.000   Class :character  
     Mode  :character   Mode  :character   Median : 1.000   Mode  :character  
                                           Mean   : 1.374                     
                                           3rd Qu.: 1.000                     
                                           Max.   :25.000                     
         name          
     Length:348        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 20646969 # https://etherscan.io/block/20646969
block_hash <- "0xc98f55ad4b575058ce0576d11bee700682fbff79c5cc8abcb84315a71d3c235d"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4752 

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

allow_artist1    <- pick(snapshot, contracts=c("TouchtheSun","Hope","InwardHarmony","Red","LonelyintheHotel","Thebalcony","Trappedinsideme","Morningvibes","Asummerdayinthecornfield","Awalkintheforest","BlackWhiteSophie","11Collection"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("EnigmaticBeautyEditions","AnastasiaEditions","LemonJuiceEditions","44BDayEditions","BurningEditions","GmCoffeeEditions","DianaEdition","RestroomStoriesEditions","BellyEditions","BurningEdition"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("TouchtheSun","Hope","InwardHarmony","Red","LonelyintheHotel","Thebalcony","Trappedinsideme","Morningvibes","Asummerdayinthecornfield","Awalkintheforest","BlackWhiteSophie","11Collection","EnigmaticBeautyEditions","AnastasiaEditions","LemonJuiceEditions","44BDayEditions","BurningEditions","GmCoffeeEditions","DianaEdition","RestroomStoriesEditions","BellyEditions","BurningEdition"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 29 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x09c52c99e701304332b5998227f07d2648e8a72c      1
     2 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     3 0x254886570d082ff98ca755fbea2dd3fd9e53f7c1      1
     4 0x3190666db6eb4ce5762afdccfa65868d639091c0      1
     5 0x37dd390265ea2438ff20d77d01b055d8e7219900      1
     6 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     7 0x3df63758bbcf697f39022a2dd619765c0fb8949d      1
     8 0x401069e3a228427e879c52e44a47d4c518ac049d      1
     9 0x506e2bdfc4b923e5589a8da6657036477825c069      1
    10 0x62607d87b5793859fb80ac96c743e8e1df4e4905      1
    11 0x6c4c3ad5aef4384d4440f5315434f55a2d6c79e1      1
    12 0x750986abc383a87500581c18834e2569a7087e85      1
    13 0x758ce9505c827824e65d65128c29387256c6e670      1
    14 0x7cad02c1e53decf5d1102a4b9bee5de79c6d2e18      1
    15 0x84e811cfc5aad0c8419a0374d0ef42a73da44cad      1
    16 0x9f36f3e6aae72011e97d84c52229595e0f913b78      1
    17 0xa48b3248d6cffd6bf858a7e6253f4f781d5e59ec      1
    18 0xae5ae13a008b662336ff3674d920819102ae4256      1
    19 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
    20 0xbd605703f546c3abf29b637acb96395488d0eb9d      1
    21 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    22 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    23 0xd1c6d8127312ba8715db4a666e7cab1af898abfe      1
    24 0xd2e0d62b6558efa8d77fa9762ad46832f89f0f5d      1
    25 0xda6f9c53acd07513a618d7f73d681b8a0061c8df      1
    26 0xe7f56761fad98d731e39e13f66c1a7d9201d7137      1
    27 0xec5e8e92bda7411ceced2c2b8828c4527e870c2d      1
    28 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    29 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1

## Allow Artist 2 - Editions

``` r
c(allow_artist2) %>%
tally() %T>%
readr::write_csv(file="allow_artist2.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 103 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      2 0x0208dab46d3df9dbb1fc331e85b5d890fd0bb350      1
      3 0x027f56ad81459af9fbf01db41fbbf03e5dd5bc30      1
      4 0x0363da9a2e829d8c9df85dc8f4b7a573cdbd9fe5      1
      5 0x0428955998e37d62990936e0773d24114f852954      1
      6 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
      7 0x08aed5ebc620f0e9bebb3411b7a2986ec67873d8      1
      8 0x08d8604a1c24fd74b85366aeafb01186d49708af      1
      9 0x0ac3503b2aa1db06b883d7600b10e5dd9c026b4a      1
     10 0x13258d611a6c9e6c511cb5594ff9bca38b9bfaa0      1
     11 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     12 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     13 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     14 0x19b27469edba628c4c3e3221d3713d387b8589bc      1
     15 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     16 0x1f2d05dde95819cdf87f146e9b2f47c7a7be2cc8      1
     17 0x20fcda3d9023866063ed8e4e13e84eb145cd5bcb      1
     18 0x21bc1b661287cafefbf8d98bf8e3d2853ecd2d8e      1
     19 0x221003ab6d85544a40c7a02dcc3327ab788d5f8a      1
     20 0x22f057b5189d796e9b56159774a701b563280b2c      1
     21 0x26b9d49ce42a8e5df9cb545b7ef290544e595e65      1
     22 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     23 0x2f0607aa5d12743aef4e8c642b5d94374588558c      1
     24 0x2f77555f5565b18bcfc5ee0ab406c24717db1152      1
     25 0x30d826b6338cc6bdabd36a10fd85de81974a2474      1
     26 0x32a4bfa6540758444a31090a5d8ee51a40f48a8d      1
     27 0x3690399f6d700e855afa70a3f8fbf848b67c961a      1
     28 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     29 0x39256c222e2a16db63f21da9d8266fc6f95f45b9      1
     30 0x3c01640dc2d37bd4fc79c0f1f99f087306862e51      1
     31 0x3e0a0cff1b0e9b43f9d878a263d65a208602c619      1
     32 0x4132799aa77f69c014b4851c1389a7429a79eb62      1
     33 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     34 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     35 0x4fc31591586075be5de33f223bfff060283babd4      1
     36 0x50538f9c1582bc1e7c4b5498b9a730506d8efde2      1
     37 0x59068075a799594db03c0255eed68e8e121155c8      1
     38 0x59d6779eca6c91ed7679e261b54299b5155eadf0      1
     39 0x5badab9b26c294b6c129dafd6909e87cb58de77b      1
     40 0x5c0c0a331a74018f8c42fd031dbacce5033d29c5      1
     41 0x5ece27c8123007a4273797ccb301529150f0ae1b      1
     42 0x60c4ae0ee854a20ea7796a9678090767679b30fc      1
     43 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
     44 0x6529595ccb74e48e421d55f54f080c1d2531f6dd      1
     45 0x656f6595a0c8a5c4e5d6b193228aacfd7cf55c47      1
     46 0x6a57b2dad1f417ca489758a083d456096e2b344b      1
     47 0x6a61e489417c914f0ef581bd99e523e4d878b392      1
     48 0x7059f6af7dab26739612c0330ad65ce411b87127      1
     49 0x733880dd47701e11577120670bafc9401d7fb658      1
     50 0x74769ae22342b5b2e45802fb97d7c0832e659bd8      1
     51 0x780a0487f34ae167865e095a140cb31a96c44f4d      1
     52 0x7843862d114e579d29d60124690ab6e11b22e4a6      1
     53 0x7e33f022f4cfab98dd93acc69442f0fd720cced0      1
     54 0x7faed47c53f3f3a3c388e72ebf79a1d85308c969      1
     55 0x8028bfbbb68ac004e0704161cd080409a8cf2950      1
     56 0x833eab3f58cf58323b8e133cf69503698c3a21f1      1
     57 0x8889ebb11295f456541901f50bcb5f382047caac      1
     58 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
     59 0x8c3996617922c3ca288958c233141d2672b6c4cf      1
     60 0x8ca8cd973e220f2fb63f59acca3fb6b6f5383831      1
     61 0x92058906c65bc9cf1b77f2f76de0687854cae050      1
     62 0x9716d338ab778c34c01c1863385ad6c5ec9ba5f8      1
     63 0x98cf0978e7144ce0ce8e911449d02b9c56231667      1
     64 0x9addd91874b08f6856bf6b4a2263665d2bc0d933      1
     65 0xa219c09cba809860ebc4dafbce9b8071eeb8a55e      1
     66 0xa46664ba7222221475146c6710c812741a6c8bf5      1
     67 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
     68 0xa6a322b0afedd465e8997ee39340a36c4997cb77      1
     69 0xa92e794e2b196e612a59b9b5c5051e1888fd7b24      1
     70 0xabd4d2b15ed7c40a2a37a5c4eb4d204ba6f208dd      1
     71 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
     72 0xaff7277fb299c33ea635981369dd07af664155f3      1
     73 0xb1879d9ec2381cc343b10a09d1d752ba275f9c33      1
     74 0xb77b1fd000e21d7e831a17e5c146e0ffe5ece440      1
     75 0xb8f3ac467a137c4da77f7841084825c122993746      1
     76 0xb998874bcb70f1075d2cac03b560245ae2917989      1
     77 0xba3fd2cbdbf15afa98f9c42562e953f5f5cd60b8      1
     78 0xba65896b8112e837fc6fa9f179116fc9b2bf8d81      1
     79 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
     80 0xbd9a54c0cc23e75029116558d5bfc8e234743b0b      1
     81 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
     82 0xbf68017293574ab4204268db4ffca9991d2e2144      1
     83 0xbfd7dde89d2244b6b35ad95c2e3c0eadf38e24ac      1
     84 0xc216523931c66522164d9c168cfc875f316e1f51      1
     85 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
     86 0xca9bb916074c6a51281a45cb9b4123a41f79cbac      1
     87 0xcb044928d73a945d31198a926e0474a00302c569      1
     88 0xcb9f87919b6818ccf3e8350e0ee2e13200dc8fe8      1
     89 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
     90 0xcc7a7d700b789224132e9434c21d145e8367a94e      1
     91 0xccb21daeda83f0ff59ace970cbc990bc29fe6dc7      1
     92 0xcd289df46f8bd595f3a886197c1b8c260ca26eaa      1
     93 0xcf6ef5a557cfc45d5208e68d226209e6f5acddf4      1
     94 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
     95 0xd72166111abc841ca922755e730244ea8ade27a2      1
     96 0xdf8465e364c5ba32bdb44d83b302bd163622a263      1
     97 0xe72b967bf1d62a6e12dc0d17a8250a682005e03d      1
     98 0xe87f08dcb2e4883c26eba5c4c0465568055a2b01      1
     99 0xe8a0cc457405c250a3142db639b3e058fd431510      1
    100 0xf03f75a7f90af699523a420a93d296a18f2db862      1
    101 0xf2cea9c6492348c19faa54a85fd77965d47fc0ff      1
    102 0xf37d25310681d5826a8cd451638251cb061f7027      1
    103 0xfe72756afb7638e080516ab3549bca9c08e70547      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 132 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x0187c9a182736ba18b44ee8134ee438374cf87dc      1
      2 0x0208dab46d3df9dbb1fc331e85b5d890fd0bb350      1
      3 0x027f56ad81459af9fbf01db41fbbf03e5dd5bc30      1
      4 0x0363da9a2e829d8c9df85dc8f4b7a573cdbd9fe5      1
      5 0x0428955998e37d62990936e0773d24114f852954      1
      6 0x0616ba63e300db20c760a3a5edbab3f2cc32cba4      1
      7 0x08aed5ebc620f0e9bebb3411b7a2986ec67873d8      1
      8 0x08d8604a1c24fd74b85366aeafb01186d49708af      1
      9 0x09c52c99e701304332b5998227f07d2648e8a72c      1
     10 0x0ac3503b2aa1db06b883d7600b10e5dd9c026b4a      1
     11 0x13258d611a6c9e6c511cb5594ff9bca38b9bfaa0      1
     12 0x1423d7ba74817a2617fa3e69b609a54e4daf6f79      1
     13 0x14977b0dbe7e155f9907effecbb70c9b7a05e737      1
     14 0x196ab3e637219ea9d2efdec0523006af0c1fc8f1      1
     15 0x19b27469edba628c4c3e3221d3713d387b8589bc      1
     16 0x1b6265a40839a331ace2d81bc82b6104703c0426      1
     17 0x1e299dc52eb9fdab6a6849f9731a948d8d72e474      1
     18 0x1f2d05dde95819cdf87f146e9b2f47c7a7be2cc8      1
     19 0x20fcda3d9023866063ed8e4e13e84eb145cd5bcb      1
     20 0x21bc1b661287cafefbf8d98bf8e3d2853ecd2d8e      1
     21 0x221003ab6d85544a40c7a02dcc3327ab788d5f8a      1
     22 0x22f057b5189d796e9b56159774a701b563280b2c      1
     23 0x254886570d082ff98ca755fbea2dd3fd9e53f7c1      1
     24 0x26b9d49ce42a8e5df9cb545b7ef290544e595e65      1
     25 0x2c419c8b207b10b39673482d83caa3e11f3604c5      1
     26 0x2f0607aa5d12743aef4e8c642b5d94374588558c      1
     27 0x2f77555f5565b18bcfc5ee0ab406c24717db1152      1
     28 0x30d826b6338cc6bdabd36a10fd85de81974a2474      1
     29 0x3190666db6eb4ce5762afdccfa65868d639091c0      1
     30 0x32a4bfa6540758444a31090a5d8ee51a40f48a8d      1
     31 0x3690399f6d700e855afa70a3f8fbf848b67c961a      1
     32 0x371c4566e886ddff10a888a9e5f26a2bac13cfa8      1
     33 0x37dd390265ea2438ff20d77d01b055d8e7219900      1
     34 0x39256c222e2a16db63f21da9d8266fc6f95f45b9      1
     35 0x3b0aa499cc6acde1d4a7433da6968d7bb8bd8509      1
     36 0x3c01640dc2d37bd4fc79c0f1f99f087306862e51      1
     37 0x3df63758bbcf697f39022a2dd619765c0fb8949d      1
     38 0x3e0a0cff1b0e9b43f9d878a263d65a208602c619      1
     39 0x401069e3a228427e879c52e44a47d4c518ac049d      1
     40 0x4132799aa77f69c014b4851c1389a7429a79eb62      1
     41 0x4269aadfd043b58cba893bfe6029c9895c25cb61      1
     42 0x4dc583548a3407a5c5f6468ccc964523cb14e9d1      1
     43 0x4fc31591586075be5de33f223bfff060283babd4      1
     44 0x50538f9c1582bc1e7c4b5498b9a730506d8efde2      1
     45 0x506e2bdfc4b923e5589a8da6657036477825c069      1
     46 0x59068075a799594db03c0255eed68e8e121155c8      1
     47 0x59d6779eca6c91ed7679e261b54299b5155eadf0      1
     48 0x5badab9b26c294b6c129dafd6909e87cb58de77b      1
     49 0x5c0c0a331a74018f8c42fd031dbacce5033d29c5      1
     50 0x5ece27c8123007a4273797ccb301529150f0ae1b      1
     51 0x60c4ae0ee854a20ea7796a9678090767679b30fc      1
     52 0x62607d87b5793859fb80ac96c743e8e1df4e4905      1
     53 0x63f11d2060d147e75c2bfb83dbebe155b4d06b66      1
     54 0x6529595ccb74e48e421d55f54f080c1d2531f6dd      1
     55 0x656f6595a0c8a5c4e5d6b193228aacfd7cf55c47      1
     56 0x6a57b2dad1f417ca489758a083d456096e2b344b      1
     57 0x6a61e489417c914f0ef581bd99e523e4d878b392      1
     58 0x6c4c3ad5aef4384d4440f5315434f55a2d6c79e1      1
     59 0x7059f6af7dab26739612c0330ad65ce411b87127      1
     60 0x733880dd47701e11577120670bafc9401d7fb658      1
     61 0x74769ae22342b5b2e45802fb97d7c0832e659bd8      1
     62 0x750986abc383a87500581c18834e2569a7087e85      1
     63 0x758ce9505c827824e65d65128c29387256c6e670      1
     64 0x780a0487f34ae167865e095a140cb31a96c44f4d      1
     65 0x7843862d114e579d29d60124690ab6e11b22e4a6      1
     66 0x7cad02c1e53decf5d1102a4b9bee5de79c6d2e18      1
     67 0x7e33f022f4cfab98dd93acc69442f0fd720cced0      1
     68 0x7faed47c53f3f3a3c388e72ebf79a1d85308c969      1
     69 0x8028bfbbb68ac004e0704161cd080409a8cf2950      1
     70 0x833eab3f58cf58323b8e133cf69503698c3a21f1      1
     71 0x84e811cfc5aad0c8419a0374d0ef42a73da44cad      1
     72 0x8889ebb11295f456541901f50bcb5f382047caac      1
     73 0x89eb11795fb79ab0f565342a906ed8491022cabe      1
     74 0x8c3996617922c3ca288958c233141d2672b6c4cf      1
     75 0x8ca8cd973e220f2fb63f59acca3fb6b6f5383831      1
     76 0x92058906c65bc9cf1b77f2f76de0687854cae050      1
     77 0x9716d338ab778c34c01c1863385ad6c5ec9ba5f8      1
     78 0x98cf0978e7144ce0ce8e911449d02b9c56231667      1
     79 0x9addd91874b08f6856bf6b4a2263665d2bc0d933      1
     80 0x9f36f3e6aae72011e97d84c52229595e0f913b78      1
     81 0xa219c09cba809860ebc4dafbce9b8071eeb8a55e      1
     82 0xa46664ba7222221475146c6710c812741a6c8bf5      1
     83 0xa48b3248d6cffd6bf858a7e6253f4f781d5e59ec      1
     84 0xa5dce742f3775059d9406342d2ecbb0ff8e3a3c2      1
     85 0xa6a322b0afedd465e8997ee39340a36c4997cb77      1
     86 0xa92e794e2b196e612a59b9b5c5051e1888fd7b24      1
     87 0xabd4d2b15ed7c40a2a37a5c4eb4d204ba6f208dd      1
     88 0xae5ae13a008b662336ff3674d920819102ae4256      1
     89 0xafc3b4a393ab5bfb5d7a905141ce3a7351f55ae3      1
     90 0xaff7277fb299c33ea635981369dd07af664155f3      1
     91 0xb1879d9ec2381cc343b10a09d1d752ba275f9c33      1
     92 0xb394f1aa30ba874df3ae6097bf4c2474b2d8f3ac      1
     93 0xb77b1fd000e21d7e831a17e5c146e0ffe5ece440      1
     94 0xb8f3ac467a137c4da77f7841084825c122993746      1
     95 0xb998874bcb70f1075d2cac03b560245ae2917989      1
     96 0xba3fd2cbdbf15afa98f9c42562e953f5f5cd60b8      1
     97 0xba65896b8112e837fc6fa9f179116fc9b2bf8d81      1
     98 0xbc56f43098e07fa18dcc4d5064eda629f2c67a22      1
     99 0xbd605703f546c3abf29b637acb96395488d0eb9d      1
    100 0xbd9a54c0cc23e75029116558d5bfc8e234743b0b      1
    101 0xbdc4a5c0ff7275736cad102c7408555fb5d6c495      1
    102 0xbf68017293574ab4204268db4ffca9991d2e2144      1
    103 0xbfd7dde89d2244b6b35ad95c2e3c0eadf38e24ac      1
    104 0xc216523931c66522164d9c168cfc875f316e1f51      1
    105 0xc6400a5584db71e41b0e5dfbdc769b54b91256cd      1
    106 0xc72f40a397453051349f73cf2e2a04fac06e37a3      1
    107 0xca9bb916074c6a51281a45cb9b4123a41f79cbac      1
    108 0xcb044928d73a945d31198a926e0474a00302c569      1
    109 0xcb9f87919b6818ccf3e8350e0ee2e13200dc8fe8      1
    110 0xcc48857242141cc7f4f8555f8a3d5bed03556c19      1
    111 0xcc7a7d700b789224132e9434c21d145e8367a94e      1
    112 0xccb21daeda83f0ff59ace970cbc990bc29fe6dc7      1
    113 0xcd289df46f8bd595f3a886197c1b8c260ca26eaa      1
    114 0xcd30b6de9dadb0a1598eb12a4ccf34b98c98c1df      1
    115 0xcf6ef5a557cfc45d5208e68d226209e6f5acddf4      1
    116 0xd1c6d8127312ba8715db4a666e7cab1af898abfe      1
    117 0xd2e0d62b6558efa8d77fa9762ad46832f89f0f5d      1
    118 0xd40b63bf04a44e43fbfe5784bcf22acaab34a180      1
    119 0xd72166111abc841ca922755e730244ea8ade27a2      1
    120 0xda6f9c53acd07513a618d7f73d681b8a0061c8df      1
    121 0xdf8465e364c5ba32bdb44d83b302bd163622a263      1
    122 0xe72b967bf1d62a6e12dc0d17a8250a682005e03d      1
    123 0xe7f56761fad98d731e39e13f66c1a7d9201d7137      1
    124 0xe87f08dcb2e4883c26eba5c4c0465568055a2b01      1
    125 0xe8a0cc457405c250a3142db639b3e058fd431510      1
    126 0xec5e8e92bda7411ceced2c2b8828c4527e870c2d      1
    127 0xf03f75a7f90af699523a420a93d296a18f2db862      1
    128 0xf2cea9c6492348c19faa54a85fd77965d47fc0ff      1
    129 0xf37d25310681d5826a8cd451638251cb061f7027      1
    130 0xf76a3c1965b15fa7b109bc7ce18d64246dae6d72      1
    131 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    132 0xfe72756afb7638e080516ab3549bca9c08e70547      1

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
