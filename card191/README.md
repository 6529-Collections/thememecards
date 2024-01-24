
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Data

``` r
fn <- "snapshot.csv"
snapshot <- readr::read_csv(fn, col_types=c("ccdcc"))
snapshot$balance <- as.integer(snapshot$balance)
summary(snapshot)
```

       address            token_id            balance        contract        
     Length:168         Length:168         Min.   :1.000   Length:168        
     Class :character   Class :character   1st Qu.:1.000   Class :character  
     Mode  :character   Mode  :character   Median :1.000   Mode  :character  
                                           Mean   :1.071                     
                                           3rd Qu.:1.000                     
                                           Max.   :3.000                     
         name          
     Length:168        
     Class :character  
     Mode  :character  
                       
                       
                       

## Seed

``` r
block <- 19046969 # https://etherscan.io/block/19046969
block_hash <- "0x88fe5b7289d867d06a97b3d1d8b1b4410c59ceec296e48225cd31429c8d05a3f"
# Convert block hash to ASCII
# Take the sum over integer vector to set seed
seed <- sum(utf8ToInt(block_hash))
cat("Seed:", seed, "\n")
```

    Seed: 4624 

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

allow_artist1    <- pick(snapshot, contracts=c("SuperRare","ChroniclesofVoxels","Realms","Foundation","ChroniclesofVoxelArt"), address_remove=address_remove,address_max=1)


allow_artist2    <- pick(snapshot, contracts=c("TheSmellofaFreshBreadEditions","ChroniclesofClayEditions","HasanGoktepeEditions"), address_remove=address_remove,address_subtract=allow_artist1,address_max=1)


allow_artist_all <- pick(snapshot, contracts=c("SuperRare","ChroniclesofVoxels","Realms","Foundation","ChroniclesofVoxelArt","TheSmellofaFreshBreadEditions","ChroniclesofClayEditions","HasanGoktepeEditions"), address_remove=address_remove,address_max=1)
```

## Allow Artist 1 - 1/1s

``` r
c(allow_artist1) %>%
tally() %T>%
readr::write_csv(file="allow_artist1.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 21 × 2
       address                                    amount
       <chr>                                       <int>
     1 0x00ff192363430a35abbf968c535b64147e88abdb      1
     2 0x016be919b52f0e2358efa57ef67a3297132b9a31      1
     3 0x05fc3a360944ba03abb2bbcce275f89c96cb1385      1
     4 0x0f4c54b80384282175f20a536cfb7c9c3c4843da      1
     5 0x15174623b8e114eb2f31851205075fc6f81be6d0      1
     6 0x1a60dfb071b039c6e33dcb3220891c83da72c1be      1
     7 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     8 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     9 0x2b776b6418a7ae859c5e630afa3fb59e82b49fa8      1
    10 0x50806f1e52f4c482cc96d03145abf731074fa33f      1
    11 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
    12 0x744320b3c5764a60b9c9a9957c46b1fcb9e64362      1
    13 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
    14 0x766977e1e61a75914cfebabc7554d558185b22ea      1
    15 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
    16 0x8ccaf951c46899aa11e96435261c271c3e5ba963      1
    17 0x9eea822800921cd4425d86f0e797749b5220ce0f      1
    18 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
    19 0xbbc30ddda94ac46496736215129b6b6f8c02bbf4      1
    20 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    21 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1

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
      1 0x034795009565c95e8700460ff72cbba2bf9e4798      1
      2 0x06f5b6243499a6f19c93fd44a92c80b7dc2cd3ee      1
      3 0x075f344771dbbba0260db5640f6150657b2b3c46      1
      4 0x0a828953f4a30f097e6217bcfcb8fc4f7c9aa110      1
      5 0x0ba175b34c80dba65bbc6cda2828eacb25338ee4      1
      6 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
      7 0x106be3b6818c7c774df5ce0a801ff9e05d65b2e8      1
      8 0x11f6ff77722501a04a31688dfd0e324a7edaa477      1
      9 0x134ca4fee2a9b53007e1cdc811c8c06fa05cf4dd      1
     10 0x16d7d99b64f2d967aebd9b6629cdd194d273cadd      1
     11 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     12 0x17dda0d374bf4246fbe320be4688be0d945f6812      1
     13 0x1adcf07389b1f6605c44a7683c50a5243829a92c      1
     14 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     15 0x1cc51e5ce9fd2f8ed687afe3ada52aab833fe908      1
     16 0x25ba562c31aaca514319f615ed56fa587a9c7ea4      1
     17 0x2f1282e26b5b7944d0151b0d069981884ce8413e      1
     18 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     19 0x30edec1c25218f5a748cccc54c562d7879e47caa      1
     20 0x32273f5f668e0d94ed7db7eb9908efc3a17e7483      1
     21 0x33b86b391d8f2acf27bce05241cb83c0a2bc77f4      1
     22 0x37b0e438a14a4f6d7d54d52c7c3017921c25eb4f      1
     23 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     24 0x3cb91a674fd627b66eeafcdf0cbcf963cede73ad      1
     25 0x3fda4facaa542d9966f043ea5f370adf06681dcf      1
     26 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
     27 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     28 0x42cf75ebf6a80daae8be438d145ab321c1b54885      1
     29 0x46ad4eb64b48c0fda7f13866792a72a67e968e23      1
     30 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     31 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     32 0x47fe90158eb5b3a40369be6cc2e911a81cd25626      1
     33 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     34 0x504cbe8565ef608f9544e59158d7d5d86ece18ea      1
     35 0x50b14457fc25d32f3c5f330af97ec8ea1f9ec573      1
     36 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     37 0x51adfb9ec934ea23731480f99c64a0ae3b6cd6c0      1
     38 0x52147b125db7595bf949ce28b2330365844dc9a6      1
     39 0x530a558d0512d536d24799be7141f36dd9ace4ad      1
     40 0x55b0fb395acb8091e37186d659efc7ccc8b5af06      1
     41 0x55f2d064c6851ae635ae15dde2dfdee88a1d34fa      1
     42 0x5a4d38aedfcda3e5b6a74d45ef1aea9f82bcd359      1
     43 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     44 0x5d97de0d40603e70545259754a6b62c6e0f790ad      1
     45 0x5e391bb4541181dd07949b769ce193898e812ebc      1
     46 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
     47 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     48 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     49 0x6b3fc213d1ebe98e53cc823503c8c16bc68a5489      1
     50 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     51 0x6bb8acab4849aedc8dab242f8ae3e4604d151081      1
     52 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     53 0x727f25672f4f2815831ed496c87b33faeb639238      1
     54 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     55 0x782a3d02dd042a7b3e9cb098619a959670775af0      1
     56 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     57 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     58 0x7dbb8e1bcd3e5f40614fc090d18059d38ee0524f      1
     59 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
     60 0x89dbd46209c8a6dd8bb73c2e186d526114e74d9e      1
     61 0x89e5d091b22892a510700f56438ca23e424a4260      1
     62 0x90025f5b7ece60f78166a7002ef0c4fdfe2b95d3      1
     63 0x904a819e486d4af2653eec8e424599335e2f190e      1
     64 0x92730185034d49ce10f0b0670f2ee3857415f178      1
     65 0x92f20a01a7fd20a4af299e91dc7c1b9448a477f1      1
     66 0x95999c47c3e32a337ef108d657675c2757a606ed      1
     67 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     68 0x97145d1d5e6fd6cc9285f4b2584f6674bb17e183      1
     69 0x9952b5d1342f27c5e9eae528550fa661c06a9ecf      1
     70 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
     71 0xa530f7739413e787c205233658185edc1e68c25e      1
     72 0xa7650e468993e41f32e506e9d74be8998937ed6d      1
     73 0xa949b1c18234c45632f460a27cfad08e5ab6ce26      1
     74 0xad3e8565fea8d59d48a874cd9d87eee4559ffc39      1
     75 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
     76 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
     77 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     78 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
     79 0xb84580a14e42c74efa1213ab2fb9f98da33452ee      1
     80 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
     81 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
     82 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
     83 0xc23631250d7e41caf2f65bb03cf556ab4588c52e      1
     84 0xc491cc1c68df1b3f3d1eb3af1604e2787891fef9      1
     85 0xc9805e721d71b011ee7f6f88c16191bf92094234      1
     86 0xcdbdfcef716aa9806a09d6c58abe3a1f69866ac2      1
     87 0xd4d2b38106e40f2693b617314ba5f406985864eb      1
     88 0xdaa1131400bb96fad912bdf5d329d43f55c029bb      1
     89 0xdb2ba0eabbfe920d4e501dfd92d6667f512e5870      1
     90 0xdbc437b6c4ede1773b60311dbe486c911cabbfea      1
     91 0xdc819a832bfa55c933238a474954f3646da275ed      1
     92 0xdf4f13ca69072752827ffd528cd736d0537b9191      1
     93 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
     94 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
     95 0xe3e1cff6ef4cc878a6bdd2a735e941e69d497839      1
     96 0xe97c4939f1215a95eaf37516f2407494ee843359      1
     97 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
     98 0xf3e4d991a20043b6bd025058cf4d96fd7501070b      1
     99 0xfafdf20f66cdf9dce074b4a9001e0ad17ca6e338      1
    100 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    101 0xfcca53d1425a72c4f9666c2749e0a3a8deb5fadf      1
    102 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    103 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1

## Allow Artist All - Combined

``` r
c(allow_artist_all) %>%
tally() %T>%
readr::write_csv(file="allow_artist_all.csv", col_names=FALSE) %>%
print(n=Inf)
```

    # A tibble: 124 × 2
        address                                    amount
        <chr>                                       <int>
      1 0x00ff192363430a35abbf968c535b64147e88abdb      1
      2 0x016be919b52f0e2358efa57ef67a3297132b9a31      1
      3 0x034795009565c95e8700460ff72cbba2bf9e4798      1
      4 0x05fc3a360944ba03abb2bbcce275f89c96cb1385      1
      5 0x06f5b6243499a6f19c93fd44a92c80b7dc2cd3ee      1
      6 0x075f344771dbbba0260db5640f6150657b2b3c46      1
      7 0x0a828953f4a30f097e6217bcfcb8fc4f7c9aa110      1
      8 0x0ba175b34c80dba65bbc6cda2828eacb25338ee4      1
      9 0x0bb75bef057da63a0ae4b25fe9adafd35cd92b87      1
     10 0x0f4c54b80384282175f20a536cfb7c9c3c4843da      1
     11 0x106be3b6818c7c774df5ce0a801ff9e05d65b2e8      1
     12 0x11f6ff77722501a04a31688dfd0e324a7edaa477      1
     13 0x134ca4fee2a9b53007e1cdc811c8c06fa05cf4dd      1
     14 0x15174623b8e114eb2f31851205075fc6f81be6d0      1
     15 0x16d7d99b64f2d967aebd9b6629cdd194d273cadd      1
     16 0x174fb6da52a957b0620d0902ace4fea33ba84d35      1
     17 0x17dda0d374bf4246fbe320be4688be0d945f6812      1
     18 0x1a60dfb071b039c6e33dcb3220891c83da72c1be      1
     19 0x1adcf07389b1f6605c44a7683c50a5243829a92c      1
     20 0x1cb9998ab32e6d172f301a10ed8e0272d96509b4      1
     21 0x1cc51e5ce9fd2f8ed687afe3ada52aab833fe908      1
     22 0x1d4b9b250b1bd41daa35d94bf9204ec1b0494ee3      1
     23 0x25ba562c31aaca514319f615ed56fa587a9c7ea4      1
     24 0x2aaba4b94a805f0fea696dc11de324c2320ff4e1      1
     25 0x2b776b6418a7ae859c5e630afa3fb59e82b49fa8      1
     26 0x2f1282e26b5b7944d0151b0d069981884ce8413e      1
     27 0x2fb5bf565ca66786bf2c2b3f7f47b5e0c650768c      1
     28 0x30edec1c25218f5a748cccc54c562d7879e47caa      1
     29 0x32273f5f668e0d94ed7db7eb9908efc3a17e7483      1
     30 0x33b86b391d8f2acf27bce05241cb83c0a2bc77f4      1
     31 0x37b0e438a14a4f6d7d54d52c7c3017921c25eb4f      1
     32 0x399d887ced702fa13bd5002f77e34d08d803e6c1      1
     33 0x3cb91a674fd627b66eeafcdf0cbcf963cede73ad      1
     34 0x3fda4facaa542d9966f043ea5f370adf06681dcf      1
     35 0x40d6cc4ad15707844b320b1d3815e0f0cf09ff30      1
     36 0x41254fe73d3b49f0fe3cc7743b4d40ef26bb338e      1
     37 0x42cf75ebf6a80daae8be438d145ab321c1b54885      1
     38 0x46ad4eb64b48c0fda7f13866792a72a67e968e23      1
     39 0x47c553e889f7bfa75cf3775473180a04e9effcfa      1
     40 0x47e255bf1ae9040151774d307c9e01d12da71d57      1
     41 0x47fe90158eb5b3a40369be6cc2e911a81cd25626      1
     42 0x48238faa9bd6d56fb44a32dac6185f04eabf2117      1
     43 0x504cbe8565ef608f9544e59158d7d5d86ece18ea      1
     44 0x50806f1e52f4c482cc96d03145abf731074fa33f      1
     45 0x50b14457fc25d32f3c5f330af97ec8ea1f9ec573      1
     46 0x511531656cb4bea59d62f3b1f3bb8b1d40eebef9      1
     47 0x51adfb9ec934ea23731480f99c64a0ae3b6cd6c0      1
     48 0x52147b125db7595bf949ce28b2330365844dc9a6      1
     49 0x530a558d0512d536d24799be7141f36dd9ace4ad      1
     50 0x55b0fb395acb8091e37186d659efc7ccc8b5af06      1
     51 0x55f2d064c6851ae635ae15dde2dfdee88a1d34fa      1
     52 0x5a4d38aedfcda3e5b6a74d45ef1aea9f82bcd359      1
     53 0x5b00f3d1644ff3106248b2c50505a67abf31efff      1
     54 0x5d97de0d40603e70545259754a6b62c6e0f790ad      1
     55 0x5e391bb4541181dd07949b769ce193898e812ebc      1
     56 0x65ba4f92d7dfa813ddbd849d9faf38a723dd9b12      1
     57 0x6605c7c3d96235beeb96650c025a96729dfc1f00      1
     58 0x69cb3b1de24e08f1cfc2994171b6c6930498f750      1
     59 0x6b3fc213d1ebe98e53cc823503c8c16bc68a5489      1
     60 0x6b4b60cd239b5b7947c98bb2b9bbd01db5bdf44c      1
     61 0x6bb8acab4849aedc8dab242f8ae3e4604d151081      1
     62 0x6e63a4caeccb4f341ee9c9175c9cc554bdb6d10b      1
     63 0x727f25672f4f2815831ed496c87b33faeb639238      1
     64 0x72ab6547ee820299d843f38147d017ca817b4a6f      1
     65 0x7304689aac83c3b236332b0c233878f1819ca89d      1
     66 0x744320b3c5764a60b9c9a9957c46b1fcb9e64362      1
     67 0x75a56cd64e10c4f07a6d2cb9027580b7b9a728db      1
     68 0x766977e1e61a75914cfebabc7554d558185b22ea      1
     69 0x782a3d02dd042a7b3e9cb098619a959670775af0      1
     70 0x793e48857f3ccdfe5cf3c504b6dfc7e8dab5b0e1      1
     71 0x7a9c55a0c4d0e88f18615bbdedad5a1dccee8527      1
     72 0x7dbb8e1bcd3e5f40614fc090d18059d38ee0524f      1
     73 0x81b55fbe66c5ffbb8468328e924af96a84438f14      1
     74 0x8754e1ed406a72f8deca5b4c9654ad2db698ba83      1
     75 0x89dbd46209c8a6dd8bb73c2e186d526114e74d9e      1
     76 0x89e5d091b22892a510700f56438ca23e424a4260      1
     77 0x8ccaf951c46899aa11e96435261c271c3e5ba963      1
     78 0x90025f5b7ece60f78166a7002ef0c4fdfe2b95d3      1
     79 0x904a819e486d4af2653eec8e424599335e2f190e      1
     80 0x92730185034d49ce10f0b0670f2ee3857415f178      1
     81 0x92f20a01a7fd20a4af299e91dc7c1b9448a477f1      1
     82 0x95999c47c3e32a337ef108d657675c2757a606ed      1
     83 0x970bb85b997af2369b4be78e703e552d4d0d7158      1
     84 0x97145d1d5e6fd6cc9285f4b2584f6674bb17e183      1
     85 0x9952b5d1342f27c5e9eae528550fa661c06a9ecf      1
     86 0x9eea822800921cd4425d86f0e797749b5220ce0f      1
     87 0xa199f8ffecafa4f3d500d3ab1d1c8e0b49c9dfd0      1
     88 0xa530f7739413e787c205233658185edc1e68c25e      1
     89 0xa7650e468993e41f32e506e9d74be8998937ed6d      1
     90 0xa949b1c18234c45632f460a27cfad08e5ab6ce26      1
     91 0xab4273e7137f9531fcb47d558d9bab0e726e6937      1
     92 0xad3e8565fea8d59d48a874cd9d87eee4559ffc39      1
     93 0xb05e9e74a9a9bbc241218c491ad038b8d40f4345      1
     94 0xb29b1936c11b77cec6061a9daf765ee2666c8f77      1
     95 0xb6e32692f2210ca8c27c8a374c0f9b11369d7e91      1
     96 0xb700fea0e530f280b69373e951f2b060e92a06b2      1
     97 0xb84580a14e42c74efa1213ab2fb9f98da33452ee      1
     98 0xb890f264b1a7591cdf9e21b3b640a741cf677ae9      1
     99 0xb98d10d9f6d07ba283bfd21b2dfec050f9ae282a      1
    100 0xbbc30ddda94ac46496736215129b6b6f8c02bbf4      1
    101 0xbe65d45635a400d895f0a2682c4fd3122b10fddd      1
    102 0xc23631250d7e41caf2f65bb03cf556ab4588c52e      1
    103 0xc491cc1c68df1b3f3d1eb3af1604e2787891fef9      1
    104 0xc9805e721d71b011ee7f6f88c16191bf92094234      1
    105 0xcdbdfcef716aa9806a09d6c58abe3a1f69866ac2      1
    106 0xd4d2b38106e40f2693b617314ba5f406985864eb      1
    107 0xdaa1131400bb96fad912bdf5d329d43f55c029bb      1
    108 0xdb2ba0eabbfe920d4e501dfd92d6667f512e5870      1
    109 0xdbc437b6c4ede1773b60311dbe486c911cabbfea      1
    110 0xdc819a832bfa55c933238a474954f3646da275ed      1
    111 0xdf4f13ca69072752827ffd528cd736d0537b9191      1
    112 0xdf95a9ca4f6a7dfe2576ca99d96417a62deb2e0a      1
    113 0xe06b24ebd56726a44e8c7f888dd185e9175d169d      1
    114 0xe3e1cff6ef4cc878a6bdd2a735e941e69d497839      1
    115 0xe97c4939f1215a95eaf37516f2407494ee843359      1
    116 0xedda20e0cc11fae46747868fcea07e60c9b25379      1
    117 0xf3e4d991a20043b6bd025058cf4d96fd7501070b      1
    118 0xf4cdfea7fdf85872141e4f8eb0e953427de7626f      1
    119 0xfafdf20f66cdf9dce074b4a9001e0ad17ca6e338      1
    120 0xfbc76261fd55cf91b81a97dbcc2b3f6118f2b935      1
    121 0xfcca53d1425a72c4f9666c2749e0a3a8deb5fadf      1
    122 0xfe0dad687399f50ae473d8028576e1451a53b518      1
    123 0xfea037b0b5edcc90c626e2e1d5d792b949888892      1
    124 0xff7efba11a8888d6974997d4a8940bf83cd7a580      1

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
